list.of.packages <- c(
  "shiny",
  "plotly",
  "conflicted",
  "duckplyr",
  "tidyr",
  "scales",
  "DT"
  )
# Comparar output para instalar paquetes
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(plotly)
library(conflicted)
library(duckplyr)
library(tidyr)
library(scales)
library(DT)
conflict_prefer("filter", "dplyr", quiet = TRUE)

# # Initialize connection with explicit configuration
# con <- duckdb::duckdb(dbdir = ":memory:", config = list(
#   threads = 2,              # Limit resource usage
#   memory_limit = "1GB"      # Prevent memory overconsumption
# ))

# # Lazy-load parquet data without immediate materialization
# df_duck <- read_parquet_duckdb(
#   path = "./data/data.parquet",
#   connection = con
# )

# Read data using duckplyr's optimized parquet reader
df_duck <- read_parquet_duckdb(
  path = "./data/data.parquet",
  prudence = "lavish" # Materialize data for testing
)

# Optimized country list calculation
country_list <- df_duck %>%
  filter(is_collab == FALSE) %>%
  distinct(country, iso2c) %>%
  filter(!is.na(country) & !is.na(iso2c) & country != "" & iso2c != "") %>% # Ensure valid entries
  arrange(country) %>%
  collect() # Collect list for UI and lookups


# --- UI ---
ui <- fluidPage(
  titlePanel("International Collaboration Trends"), # Add title
  sidebarLayout( # Use layout for better organization
    sidebarPanel(
      width = 3, # Adjust width as needed
      selectInput("country", "Select Country:",
        selected = "China",
        # Use full names for display, return full name for easier ISO lookup below
        choices = country_list$country
      ),
      sliderInput("year_range", "Select Year Range:",
        min = 1996,
        max = 2022,
        value = c(1996, 2022),
        sep = ""
      )
    ),
    mainPanel(
      width = 9, # Adjust width as needed
      plotlyOutput("collab_plot", height = "600px"), # Adjusted height
      hr(), # Add a horizontal rule
      DT::DTOutput("summary_table") # Add DT output to UI
    )
  )
)


server <- function(input, output, session) {
  # Create a reactive for selected ISO code to avoid repeated lookups
  selected_iso <- reactive({
    req(input$country)
    # Look up the ISO code for the selected country name
    iso <- country_list$iso2c[country_list$country == input$country]
    validate(need(length(iso) > 0 && !is.na(iso), "Invalid country selection"))
    iso
  })

  # Reactive query optimized with DuckDB materialization points
  filtered_data <- reactive({
    req(input$year_range, selected_iso())
    iso <- selected_iso()
    validate(need(iso, "Country not found"))

    withProgress(message = "Processing data...", {
      # Initial processing with explicit materialization
      # Start query - apply filters early
      base_query <- df_duck |>
        filter(is_collab == TRUE) |>
        filter(between(year, input$year_range[1], input$year_range[2])) |>
        # Filter potentially invalid iso2c strings early (optional but safer)
        filter(!is.na(iso2c) & iso2c != "" & grepl("-", iso2c)) |>
        select(iso2c, year, percentage)

      # Process pairs: split, unnest, join, filter, aggregate
      inner_part <- base_query |>
        mutate(countries = strsplit(iso2c, "-"), row_id = row_number()) |>
        # Consider filtering for pairs only: filter(sapply(countries, length) == 2)
        unnest_longer(countries) |>
        # Rename for clarity before self-join
        rename(country_code = countries)

      # Self-join using the native pipe
      result_query <- inner_part |>
        # Self-join to find pairs from the same original row
        inner_join(
          # Select only necessary columns for the join partner
          select(inner_part, row_id, partner_code = country_code),
          by = "row_id"
        ) |>
        # We have pairs (country_code, partner_code) for each row_id
        # Ensure they are different codes from the same original row
        filter(country_code != partner_code) |>
        # Keep only pairs involving the selected country 'iso'
        filter(country_code == iso | partner_code == iso) |>
        # Identify the partner (the one that is NOT 'iso')
        mutate(partner = ifelse(country_code == iso, partner_code, country_code)) |>
        # Group by original row, year, and the identified partner to sum percentages correctly
        group_by(row_id, year, partner) |>
        # Take the first percentage found for the pair in that year
        summarize(percentage = first(percentage), .groups = "drop") |>
        # Now aggregate percentage across all rows for the same year/partner combo
        group_by(year, partner) |>
        summarize(
          total_percentage = sum(percentage, na.rm = TRUE),
          .groups = "drop"
        )

      return(result_query) # Return the LAZY query object
    })
  }) |> bindCache(input$country, input$year_range)

  # Optimized data collection
  plot_data <- reactive({
    filtered_data() |>
      collect() |>
      left_join(
        country_list |> select(iso2c, country),
        by = c("partner" = "iso2c")
      ) |>
      mutate(country = factor(country)) # Optimize for plotting
  }) |> bindCache(filtered_data())



  # Rest of the server code remains the same...
  # Generate the plot
  output$collab_plot <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No collaborations found for selected country"))

    # This line has a problem - input$country is already the country name, not the ISO code
    # selected_country_name <- country_list$country[country_list$iso2c == input$country]

    # Change to:
    selected_country_name <- input$country

    p <- ggplot(df, aes(x = year, y = total_percentage, color = country)) +
      geom_line(linewidth = 0.2, linetype = "dashed") +
      geom_point(
        aes(
          size = total_percentage,
          text = paste0(
            "<b>", country, "</b><br>",
            "<b>Year:</b> ", year, "<br>",
            "<b>Collaboration with ", selected_country_name, ":</b> ",
            scales::percent(total_percentage / 100, accuracy = 0.01)
          )
        )
      ) +
      scale_color_viridis_d(
        option = "turbo",
        name = "Partner Country",
        direction = -1
        # guide = guide_legend(
        #   override.aes = list(size = 3),
        #   ncol = if(partner_count > 20) 3 else 2
        # )
      ) +
      # scale_radius(range = c(0.5, 6), name = "") +
      labs(
        title = paste("Collaboration Trends for", selected_country_name),
        x = "Year",
        y = "Collaboration Percentage (%)",
        color = "Partner Country"
      ) +
      theme_minimal(base_size = 14) +
      scale_y_continuous(labels = percent_format(scale = 1)) +
      # scale_x_continuous(
      #     breaks = scales::pretty_breaks(n = 10)
      #   ) +
      theme(legend.position = "right")

    ggplotly(p, tooltip = "text") |>
      plotly::layout(
        hoverlabel = list(bgcolor = "white"),
        legend = list(
          title = list(text = "Partner Country"),
          font = list(size = 9),
          itemsizing = "constant"
        )
      )
  })

  # Summary table
  output$summary_table <- DT::renderDT(
    {
      df <- plot_data() |>
        group_by(country) |>
        summarize(
          avg_percentage = mean(total_percentage, na.rm = TRUE),
          max_percentage = max(total_percentage, na.rm = TRUE),
          years_present = n_distinct(year)
        ) |>
        arrange(desc(avg_percentage))

      df$avg_percentage <- scales::percent(df$avg_percentage / 100, accuracy = 0.01)
      df$max_percentage <- scales::percent(df$max_percentage / 100, accuracy = 0.01)

      df |> rename(
        "Partner Country" = country,
        "Average %" = avg_percentage,
        "Maximum %" = max_percentage,
        "Years Present" = years_present
      )
    },
    options = list(pageLength = 5)
  )

  # Proper connection management
  # onStop(function() {
  #   duckdb::duckdb_shutdown(con)
  # })
}
shinyApp(ui, server)
