library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)


load("data/Flights_Data_Clean.RData", verbose = TRUE)


# Define UI for the app
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Flight Delays Analysis Dashboard"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Flight Delays", tabName = "flight_delays", icon = icon("plane")),
      menuItem("Airline Frequency", tabName = "airline_frequency", icon = icon("list-ul")),
      menuItem("Airline Delays", tabName = "airline_delays", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Page 1 content: Flight Delays
      tabItem(tabName = "flight_delays",
              fluidRow(
                column(12,
                       tags$div(
                         h4("Flight delays"),
                         p("The graph below displays the relationship between departure times and departure delays for flights based on the selected month and day."),
                         p("Adjust the month and day using the inputs on the left to view different data points."),
                         hr()
                       )
                )
              ),
              fluidRow(
                column(12,
                       selectInput("month", "Select Month",
                                   choices = month.name,
                                   selected = month.name[1])),
                column(12,
                       uiOutput("day_selector")),
                column(12,
                       align = "center",
                       plotOutput("flight_plot", width = "80%", height = "400px"))
              )
      ),
      # Page 2 content: Airline Frequency
      tabItem(tabName = "airline_frequency",
              fluidRow(
                column(12,
                       tags$div(
                         h4("Famous Flights"),
                         p("The graph below displays the frequencies of flights for different selected airlines."),
                         p("Check the box for the airlines for which you want to have the comparison."),
                         hr()
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       align = "center",
                       plotOutput("airline_plot", width = "80%", height = "400px"))
              ),
              fluidRow(
                column(12,
                       checkboxGroupInput("airlines", "Select Airlines",
                                          choices = unique(data$name),
                                          selected = unique(data$name)[1:4]),
                       checkboxInput("select_all", "Select All", FALSE) )
              )
      ),
      
      tabItem(tabName = "airline_delays",
              fluidRow(
                column(12,
                       tags$div(
                         h4("Airline mean delays"),
                         p("The graph below displays the mean of delays of flights over the months for different selected airlines."),
                         p("Check the box for the airlines for which you want to have the comparison."),
                         hr()
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       align = "center",
                       plotOutput("airline_mean_delay_plot", width = "80%", height = "400px"))
              ),
              fluidRow(
                column(12,
                       checkboxGroupInput("airlines_2", "Select Airlines",
                                          choices = unique(data$name),
                                          selected = unique(data$name)[1]))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Render day selection based on selected month
  output$day_selector <- renderUI({
    max_days <- ifelse(input$month %in% c("January", "March", "May", "July", "August", "October", "December"), 31,
                       ifelse(input$month %in% c("April", "June", "September", "November"), 30, 28)) # Account for February in a non-leap year
    
    sliderInput("day", "Select Day", min = 1, max = max_days, value = 1)
  })
  
  output$flight_plot <- renderPlot({
    # Map selected month name to its numeric representation
    month_num <- match(input$month, month.name)
    
    # Filter data based on selected month (using numeric representation) and day
    filtered_data <- subset(data, month == month_num & day == input$day)
    
    # Extract month name and day number
    month_name <- input$month
    day_number <- input$day
    
    # Create ggplot for departure times vs departure delays
    ggplot(filtered_data, aes(x = dep_time, y = dep_delay)) +
      geom_point(color = "#CC79A7", alpha = 0.7, size = 2) +
      labs(x = "Departure Time",
           y = "Departure Delay (minutes)",
           title = paste(month_name, day_number)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$airline_plot <- renderPlot({
    # Filter data based on selected airlines
    selected_airlines_data <- subset(data, name %in% input$airlines)
    
    # Create frequency table for selected airlines
    frequency_table <- table(selected_airlines_data$name)
    frequency_df <- as.data.frame(frequency_table)
    frequency_df <- frequency_df[order(frequency_df$Freq, decreasing = TRUE), ]
    custom_colors <- rainbow(length(input$airlines))
    
    # Create ggplot for frequency of flights by airline
    ggplot(frequency_df, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      labs(title = "Frequency of Flights by Airline",
           x = "Airline",
           y = "Frequency") +
      theme_minimal() +
      coord_flip() +
      scale_fill_manual(values = custom_colors)
  })
  
  observeEvent(input$select_all, {
    if(input$select_all) {
      updateCheckboxGroupInput(session, "airlines", selected = unique(data$name))
    } else {
      updateCheckboxGroupInput(session, "airlines", selected = NULL)
    }
  })
  
  
  
  
  
  
  
  output$airline_mean_delay_plot <- renderPlot({
    # Filter data based on selected airlines
    selected_airlines_data <- subset(data, name %in% input$airlines_2)
    
    summary_data <- selected_airlines_data %>%
      group_by(month, name) %>%
      summarise(mean_delay = mean(dep_delay), .groups = 'drop')
    
    month_names <- month.abb
    
    ggplot(summary_data, aes(x = factor(month, levels = 1:12, labels = month_names), y = mean_delay, color = name)) +
      geom_line(aes(group = name), linewidth = 1, alpha = 0.9) +
      labs(title = "Flight Delay Analysis by Month and Airline",
           x = "Month",
           y = "Mean Delay",
           color = "Airline") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)





