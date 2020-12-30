source("analysis.R")


##############################
# ui elements
##############################

page_one <- tabPanel(
  "Summary",
  titlePanel("Summary"),
  br(),
  splitLayout(
    p("For this assignment, we have been asked to analyze the",
      a("Data on CO2 and Greenhouse Emissions",
        href =
          "https://github.com/owid/co2-data/"),
      "dataset by",
      a("Our World In Data",
        href =
          "https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions"),
      paste("which was first released on August 7, 2020. This dataset contains",
            "metrics about CO2 and greenhouse gas emissions, such as CO2 output",
            "per capita and cumulative CO2 production, over time. The variables",
            "that my visualization focuses on are"),
      code("year"), ", ", code("co2"), ", ", code("cement_co2"), ", ",
      code("coal_co2"), ", ", code("flaring_co2"), ", ", code("gas_co2"),
      ", and ", code("oil_co2"),
      paste(", which are defined to measure the annual production CO2 emissions",
            "and CO2 emissions for cement, coal, flaring, gas, and oil",
            "production emissions respectively. I chose to focus on these",
            "variables because I am interested in examining how these",
            "different sources of CO2 contribute to the overall carbon",
            "emissions, and how their contributions have fluctuated over",
            "the years.")),
    tags$ol(
      tags$li(paste("There are", num_features, "features in the dataset and",
                    num_elem, "elements total.")),
      tags$li(paste("In total", num_countries, "countries are represented with",
                    "records for each country from", range_years[1], "to",
                    range_years[2], "spanning", num_years, "years.")),
      tags$li(paste("The range of values for annual CO2 production emissions are",
                    "from", range_co2[1], "to", range_co2[2],
                    "(in million tons).")),
      tags$li(paste("There are five production sources in the dataset: cement,",
                    "coal, flaring, gas, and oil. The range of values for CO2,",
                    "produced by cement are from", range_cement_co2[1], "to",
                    range_cement_co2[2], "for coal are from", range_coal_co2[1],
                    "to", range_coal_co2[2], "for flaring are from",
                    range_flaring_co2[1], "to", range_flaring_co2[2], "for gas",
                    "are from", range_gas_co2[1], "to", range_gas_co2[2], ", and",
                    "for oil are from", range_oil_co2[1], "to", range_oil_co2[2],
                    "(in million tons).")),
      tags$li(paste("The scope of this analysis is limited to data within the",
                    "United States between the years 1990-2014 in order to have",
                    "the highest validity of data. Within this scope,",
                    prop_valid_data, "percent of the data is non-missing",
                    "(not N/A).")),
    ),
    
  cellArgs = list(style = 'white-space: normal;')))

plot_panel <- tabPanel(
  "Plot",
  br(),
  plotlyOutput("chart"),
  hr(),
  p(paste("This is a graph that depicts the CO2 emissions per year from",
          "various sources in the United States in 1990-2014. I created",
          "the graph this way so that viewers can compare the CO2 sources",
          "they would like to and specify a certain year range to focus",
          "their viewing on. This is important to think about because we",
          "can analyze which sources of CO2 are the most significant",
          "contributors to global warming in the United States and give",
          "them higher priority compared to other sources that may not be",
          "contributing as much to the total emissions over time. From the",
          "graph, we can observe that oil and coal production have made",
          "the most contribution in the United States to CO2 emissions,",
          "followed by gas, cement, and flaring. Looking at the overall",
          "trend, there was a slight decrease in CO2 emissions in the year",
          "2009, but on the aggregate, CO2 emissions in the United States",
          "have been more or less steady through the years. This is",
          "troubling as the global temperature continues to rise, and our",
          "inaction towards fossil fuel production will only exacerbate",
          "this concern. With oil and coal producing the most CO2 in our",
          "country, there is an urgent need to examine alternatives in",
          "favor of renewable, sustainable, and zero-emissions energy",
          "sources.")))

page_two <- tabPanel(
  "Interactive Graph",
  titlePanel("Interactive Graph"),
  br(),
  sidebarLayout(
    sidebarPanel(
      helpText("Select data to display."),
      checkboxGroupInput("checkboxSources",
                         label = "CO2 Source",
                         choices = c("Total", "Cement",
                                     "Coal", "Flaring",
                                     "Gas", "Oil"),
                         selected = "Total"),
      sliderInput(
        inputId = "yearSlider",
        label = "Year Range",
        min = 1990,
        max = 2014,
        value = c(1990, 2014),
        sep = ""
      )
    ),

    mainPanel(
      tabsetPanel(
        plot_panel, 
        tabPanel("Raw Table", tableOutput("table"))
      )
      ,
      br(),

    ))
  )

##############################
# server
##############################

ui <- navbarPage(
  "Assignment 4: Carbon Dioxide",
  page_one,
  page_two
)

server <- function(input, output) {
  filteredData <- reactive({
    data_to_render[data_to_render$source %in% input$checkboxSources, ]
  })

  output$table <- renderTable(data_in_use)
  output$chart <- renderPlotly({
    plot_ly(
      data = filteredData(),
      x = ~year,
      y = ~co2_output,
      split = ~source,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = "%{y} million tons"
    ) %>%
      layout(title = "Annual CO2 Output in the United States (1990-2014)",
             xaxis = list(title = "Year",
                          range = input$yearSlider,
                          fixedrange = TRUE),
             yaxis = list(title = "Annual CO2 output (million tons)",
                          range = c(0, 7000)),
             showlegend = TRUE,
             hovermode = "x unified"
      )
  })
}

# Execute server
shinyApp(ui, server)
