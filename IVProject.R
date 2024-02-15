install.packages("lubridate")
install.packages("plotly")
install.packages("shinyjs")
install.packages("shinythemes")
install.packages("shinydashboardPlus")
install.packages("shinyWidgets")
install.packages("shinyDarkmode")

install.packages("remotes")

remotes::install_github("deepanshu88/shinyDarkmode")



library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(shinydashboardPlus)
library(shinyWidgets)

df <- read.csv("london_merged.csv",header = TRUE,sep = ",")

str(df)

summary(df)

is.null(df)

duplicate_cols <- names(df)[duplicated(names(df))]

if (length(duplicate_cols) > 0) {
  print("Duplicate columns found:")
  print(duplicate_cols)
} else {
  print("No duplicate columns found.")
}
#################################################
library(dplyr)
new_cols <- list(
  "timestamp" = "time",
  "cnt" = "count",
  "t1" = "real_temp_C",
  "t2" = "feels_like_temp_C",
  "hum" = "humidity",
  "wind_speed" = "wind_speed_kph",
  "is_holiday" = "holiday",
  "is_weekend" = "weekend",
  "season" = "season",
  "weathe_code" = "weather"
)

for (col in names(new_cols)) {
  if (col %in% names(df)) {
    names(df)[names(df) == col] <- new_cols[[col]]
  }
}

####################################################3

df$time <- as.Date(df$time)
df$day_of_week <- weekdays(df$time)
df$month <- month(df$time)
df$season <- as.factor(df$season)
df$weather <- as.factor(df$weather)
df$holiday <- as.factor(df$holiday)
df$weekend <- as.factor(df$weekend)


######################################################################
library(shiny)
library(shinydashboard)
library(ggplot2)

library(shinythemes)
library(shinyDarkmode)
library(shinydashboardPlus)


ui <- fluidPage(
  titlePanel("Interactive Data Visualizations"),
  use_darkmode(),
  sidebarLayout(
    sidebarPanel(
      helpText("click on the Button in the right down corner to Switch to Dark Mode"),
      helpText("Select the visualization you want to appear"),
      checkboxGroupInput("visualization_checkbox",
                         "Select Visualizations",
                         choices = c("Histogram", "Line Plot", "Scatter Plot", "Boxplot", "Bar Chart", "Density Plot", "Area Chart","Pie Chart"),
                         selected = c("Histogram")),
      uiOutput("visualization_info"),
      conditionalPanel(
        condition = "input.visualization_checkbox.indexOf('Histogram') !== -1",
        selectInput("hist_category", "Select Category", choices = colnames(df), selected = "count")
      ),
      conditionalPanel(
        condition = "input.visualization_checkbox.indexOf('Line Plot') !== -1",
        selectInput("line_category_x", "X-axis Category", choices = colnames(df), selected = "time"),
        selectInput("line_category_y", "Y-axis Category", choices = colnames(df), selected = "count"),
        helpText("Choose start and end date to display the visualization you choose in that range"),
        dateRangeInput("date_range", "Date Range", start = min(df$time), end = max(df$time))
      ),
      conditionalPanel(
        condition = "input.visualization_checkbox.indexOf('Scatter Plot') !== -1",
        selectInput("scatter_category_x", "X-axis Category", choices = colnames(df), selected = "real_temp_C"),
        selectInput("scatter_category_y", "Y-axis Category", choices = colnames(df), selected = "feels_like_temp_C")
      ),
      conditionalPanel(
        condition = "input.visualization_checkbox.indexOf('Boxplot') !== -1",
        selectInput("boxplot_category", "Category", choices = colnames(df), selected = "season")
      ),
      conditionalPanel(
        condition = "input.visualization_checkbox.indexOf('Bar Chart') !== -1",
        selectInput("bar_x_category", "X-axis Category", choices = colnames(df), selected = "holiday"),
        selectInput("bar_fill_category", "Fill Category", choices = colnames(df), selected = "weekend")
      ),
      conditionalPanel(
        condition = "input.visualization_checkbox.indexOf('Density Plot') !== -1",
        selectInput("density_category", "Category", choices = colnames(df), selected = "humidity")
      ),
      conditionalPanel(
        condition = "input.visualization_checkbox.indexOf('Area Chart') !== -1",
        selectInput("area_x", "X-axis Category", choices = colnames(df), selected = "wind_speed_kph"),
        selectInput("area_y", "Y-axis Category", choices = colnames(df), selected = "count"),
        checkboxGroupInput("weather_categories", "Select Weather Categories",
                           choices = c("Clear or mostly clear with possible haze, fog, or patches of fog" = 1,
                                       "Scattered clouds or few clouds" = 2,
                                       "Broken clouds" = 3,
                                       "Cloudy" = 4),
                           selected = c())
      ),
      selectInput("unit_switch", "Select Unit", choices = c("Absolute Values", "Percentage Change"), selected = "Absolute Values"),
      sliderInput("temp_filter", "Filter by Real Temperature", min = min(df$real_temp_C), max = max(df$real_temp_C), value = c(min(df$real_temp_C), max(df$real_temp_C)), step = 1),
      radioButtons("histogram_mode", "Select Data Mode:",
                   choices = c("Daily", "Cumulative"),
                   selected = "Daily"),
      selectInput("display_type", "Display Type:",
                  choices = c("Percentage", "Total Count"),
                  selected = "Percentage")
     
    ),
    mainPanel(
      useShinyjs(),
      uiOutput("plots")
    )
  )
)

server <- function(input, output) {
  darkmode(label = "⏳")
  
  output$visualization_info <- renderUI({
    if (length(input$visualization_checkbox) > 0) {
      #lapply apply function le list aw vector 
      info <- lapply(input$visualization_checkbox, function(vis) {
        description <- switch(vis,
                              "Histogram" = "A graphical representation of the distribution of numerical data.",
                              "Line Plot" = "Shows the trend of a numeric variable over time or another continuous variable.",
                              "Scatter Plot" = "Displays the relationship between two numeric variables through points.",
                              "Boxplot" = "Summarizes the distribution of a continuous variable across different categories.",
                              "Bar Chart" = "Compares different categories by showing their values as bars.",
                              "Density Plot" = "Illustrates the distribution of a single numeric variable.",
                              "Area Chart" = "Visualizes changes in values over time or categories with filled areas."
        )
        HTML(paste("<b>", vis, ":</b> ", description))
      })
      fluidRow(info)
    }
  })
  
  filtered_df <- reactive({
    filtered <- df
    if (input$unit_switch == "Percentage Change") {
      filtered$real_temp_C <- ((filtered$real_temp_C - lag(filtered$real_temp_C)) / lag(filtered$real_temp_C)) * 100
    }
    # Filter operations excluding weather and season
    filtered <- filter(filtered, real_temp_C >= input$temp_filter[1] & real_temp_C <= input$temp_filter[2])
    return(filtered)
  })
  
  renderHistogram <- function() {
    filtered_data <- filtered_df()
    
    if (input$histogram_mode == "Cumulative") {
      filtered_data <- filtered_data %>%
        mutate(cumulative_count = cumsum(count))
      
      p <- ggplot(filtered_data, aes(x = time, y = cumulative_count)) +
        geom_line(color = "blue") +
        labs(title = "Cumulative Count Over Time", x = "Time", y = "Cumulative Count")
    } else {
      p <- ggplot(filtered_data, aes_string(x = input$hist_category)) +
        geom_histogram(binwidth = 1, color = "black", fill = "skyblue", alpha = 0.7) +
        labs(title = paste("Histogram of", input$hist_category), x = input$hist_category, y = "Frequency")
    }
    
    ggplotly(p)
  }
  
  
  renderLinePlot <- function() {
    filtered_data <- filtered_df()
    if (!is.null(input$date_range)) {
      filtered_data <- filtered_data[filtered_data$time >= input$date_range[1] & filtered_data$time <= input$date_range[2], ]
    }
    
    p <- ggplot(filtered_data, aes_string(x = input$line_category_x, y = input$line_category_y)) +
      geom_line(color = "blue") +
      labs(title = "Count Over Time", x = input$line_category_x, y = input$line_category_y) +
      ylim(0,300)
    
    ggplotly(p, tooltip = c(input$line_category_x, input$line_category_y))
  }
  
  
  
  renderScatterPlot <- function() {
    p <- ggplot(filtered_df(), aes_string(x = input$scatter_category_x, y = input$scatter_category_y)) +
      geom_point(color = "red") +
      labs(
        title = paste(input$scatter_category_x, "vs", input$scatter_category_y),
        x = input$scatter_category_x,
        y = input$scatter_category_y
      )
    ggplotly(p, tooltip = c(input$scatter_category_x, input$scatter_category_y))
  }
  
  
  renderBoxplot <- function() {
    p <- ggplot(filtered_df(), aes_string(x = input$boxplot_category, y = "count")) +
      geom_boxplot(fill = "green", color = "black") +
      labs(title = "Count by Season", x = input$boxplot_category, y = "Count")
    ggplotly(p, tooltip = input$boxplot_category)
  }
  
  renderBarChart <- function() {
    p <- ggplot(filtered_df(), aes_string(x = input$bar_x_category, fill = input$bar_fill_category)) +
      geom_bar(position = "dodge", color = "black") +
      labs(title = "Holiday vs Weekend Counts", x = input$bar_x_category, y = "Count") +
      scale_fill_discrete(name = input$bar_fill_category)
    ggplotly(p, tooltip = c(input$bar_x_category, input$bar_fill_category))
  }
  
  renderDensityPlot <- function() {
    p <- ggplot(filtered_df(), aes_string(x = input$density_category)) +
      geom_density(fill = "purple", alpha = 0.6) +
      labs(
        title = paste("Density Plot of", input$density_category),
        x = input$density_category,
        y = "Density"
      )
    ggplotly(p, tooltip = input$density_category)
  }
  
  
  renderAreaChart <- function() {
    selected_weather <- filtered_df()$weather %in% input$weather_categories
    p <- ggplot(filtered_df()[selected_weather, ],
                aes_string(x = input$area_x, y = input$area_y, fill = "weather")) +
      geom_area(position = "stack") +
      labs(title = "Count Variation by Wind Speed and Weather",
           x = input$area_x,
           y = input$area_y) +
      scale_fill_discrete(name = "weather")
    ggplotly(p, tooltip = c(input$area_x, input$area_y, "weather"))
  }
  

  renderPieChart <- function() {
    p <- ggplot(df, aes(x = factor(1), fill = day_of_week)) +
      geom_bar(width = 1) +
      coord_polar("y") +
      labs(title = "Distribution by Day of Week", x = NULL, y = NULL)
    #ggplotly(p)
  }
  
  
  
  
  
  output$plots <- renderUI({
    plot_output_list <- lapply(input$visualization_checkbox, function(visualization) {
      switch(visualization,
             "Histogram" = renderHistogram(),
             "Line Plot" = renderLinePlot(),
             "Scatter Plot" = renderScatterPlot(),
             "Boxplot" = renderBoxplot(),
             "Bar Chart" = renderBarChart(),
             "Density Plot" = renderDensityPlot(),
             "Area Chart" = renderAreaChart(),
             "Pie Chart" = renderPieChart()
      )
    })
    fluidRow(plot_output_list)
  })
}
shinyApp(ui = ui, server = server) 

