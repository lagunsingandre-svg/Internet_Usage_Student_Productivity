
library(shiny)
library(ggplot2)

# Load the dataset
students <- read.csv("internet_usage_productivity.csv")

# User Interface
ui <- fluidPage(
  
  titlePanel("Internet Usage and Student Productivity"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      selectInput(
        "year",
        "Select Year Level:",
        choices = c("All", unique(students$Year_Level)),
        selected = "All"
      ),
      
      selectInput(
        "purpose",
        "Select Internet Purpose:",
        choices = c(
          "All",
          unique(students$Primary_Internet_Purpose)
        ),
        selected = "All"
      )
    ),
    
    mainPanel(
      
      h3("Student Productivity Dashboard"),
      
      fluidRow(
        column(4, wellPanel(
          h4("Total Students"),
          textOutput("total_students")
        )),
        
        column(4, wellPanel(
          h4("Average Internet Usage"),
          textOutput("average_usage")
        )),
        
        column(4, wellPanel(
          h4("Average Productivity"),
          textOutput("average_productivity")
        ))
      ),
      
      h4("Daily Internet Usage"),
      plotOutput("usage_plot"),
      
      h4("Study Hours and Productivity"),
      plotOutput("study_plot"),
      
      h4("Productivity Rating Distribution"),
      plotOutput("productivity_plot"),
      
      h4("Student Data Table"),
      tableOutput("student_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data <- students
    
    if (input$year != "All") {
      data <- data[data$Year_Level == input$year, ]
    }
    
    if (input$purpose != "All") {
      data <- data[
        data$Primary_Internet_Purpose == input$purpose, ]
    }
    
    data
  })
  
  output$total_students <- renderText({
    nrow(filtered_data())
  })
  
  output$average_usage <- renderText({
    paste(
      round(mean(
        filtered_data()$Daily_Internet_Usage_Hours,
        na.rm = TRUE
      ), 2), "hours"
    )
  })
  
  output$average_productivity <- renderText({
    round(mean(
      filtered_data()$Productivity_Rating,
      na.rm = TRUE
    ), 2)
  })
  
  output$usage_plot <- renderPlot({
    ggplot(
      filtered_data(),
      aes(x = Daily_Internet_Usage_Hours)
    ) +
      geom_histogram(
        bins = 10,
        fill = "steelblue",
        color = "white"
      ) +
      labs(
        x = "Daily Internet Usage (Hours)",
        y = "Number of Students",
        title = "Distribution of Daily Internet Usage"
      ) +
      theme_minimal()
  })
  
  output$study_plot <- renderPlot({
    ggplot(
      filtered_data(),
      aes(
        x = Study_Hours_Per_Day,
        y = Productivity_Rating
      )
    ) +
      geom_point(
        color = "darkgreen",
        size = 3
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE
      ) +
      labs(
        x = "Study Hours Per Day",
        y = "Productivity Rating",
        title = "Study Hours vs Productivity"
      ) +
      theme_minimal()
  })
  
  output$productivity_plot <- renderPlot({
    ggplot(
      filtered_data(),
      aes(x = factor(Productivity_Rating))
    ) +
      geom_bar(
        fill = "orange",
        color = "white"
      ) +
      labs(
        x = "Productivity Rating",
        y = "Number of Students",
        title = "Productivity Rating Distribution"
      ) +
      theme_minimal()
  })
  
  output$student_table <- renderTable({
    filtered_data()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
