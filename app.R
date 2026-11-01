
library(shiny)
library(ggplot2)
library(dplyr)

# Load dataset
student_data <- read.csv("time_management.csv")

ui <- fluidPage(
  titlePanel("Time Management and Student Academic Performance"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filter Students"),
      
      selectInput(
        "gender",
        "Select Gender:",
        choices = c("All", unique(student_data$Gender))
      ),
      
      sliderInput(
        "study_hours",
        "Daily Study Hours:",
        min = min(student_data$Daily_Study_Hours),
        max = max(student_data$Daily_Study_Hours),
        value = range(student_data$Daily_Study_Hours)
      ),
      
      selectInput(
        "time_rating",
        "Time Management Rating:",
        choices = c("All", sort(unique(student_data$Time_Management_Rating)))
      ),
      
      actionButton("reset", "Reset Filters")
    ),
    
    mainPanel(
      h3("Student Summary"),
      
      fluidRow(
        column(3, wellPanel(
          h4("Total Students"),
          textOutput("total_students")
        )),
        column(3, wellPanel(
          h4("Average Study Hours"),
          textOutput("average_study")
        )),
        column(3, wellPanel(
          h4("Average Performance"),
          textOutput("average_performance")
        )),
        column(3, wellPanel(
          h4("Average Time Rating"),
          textOutput("average_rating")
        ))
      ),
      
      h3("Data Visualizations"),
      
      plotOutput("study_plot"),
      plotOutput("rating_plot"),
      plotOutput("attendance_plot"),
      
      h3("Student Data Table"),
      tableOutput("student_table")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- student_data
    
    if (input$gender != "All") {
      data <- data %>% filter(Gender == input$gender)
    }
    
    data <- data %>%
      filter(
        Daily_Study_Hours >= input$study_hours[1],
        Daily_Study_Hours <= input$study_hours[2]
      )
    
    if (input$time_rating != "All") {
      data <- data %>%
        filter(Time_Management_Rating == as.numeric(input$time_rating))
    }
    
    data
  })
  
  observeEvent(input$reset, {
    updateSelectInput(
      session, "gender",
      selected = "All"
    )
    
    updateSliderInput(
      session, "study_hours",
      value = range(student_data$Daily_Study_Hours)
    )
    
    updateSelectInput(
      session, "time_rating",
      selected = "All"
    )
  })
  
  output$total_students <- renderText({
    nrow(filtered_data())
  })
  
  output$average_study <- renderText({
    round(mean(filtered_data()$Daily_Study_Hours), 2)
  })
  
  output$average_performance <- renderText({
    round(mean(filtered_data()$Academic_Performance), 2)
  })
  
  output$average_rating <- renderText({
    round(mean(filtered_data()$Time_Management_Rating), 2)
  })
  
  output$study_plot <- renderPlot({
    ggplot(filtered_data(),
           aes(x = Daily_Study_Hours,
               y = Academic_Performance)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Study Hours vs. Academic Performance",
        x = "Daily Study Hours",
        y = "Academic Performance"
      ) +
      theme_minimal()
  })
  
  output$rating_plot <- renderPlot({
    rating_data <- filtered_data() %>%
      group_by(Time_Management_Rating) %>%
      summarise(
        Average_Performance = mean(Academic_Performance),
        .groups = "drop"
      )
    
    ggplot(rating_data,
           aes(x = factor(Time_Management_Rating),
               y = Average_Performance)) +
      geom_col() +
      labs(
        title = "Average Performance by Time Management Rating",
        x = "Time Management Rating",
        y = "Average Performance"
      ) +
      theme_minimal()
  })
  
  output$attendance_plot <- renderPlot({
    attendance_data <- filtered_data() %>%
      group_by(Class_Attendance_Rate) %>%
      summarise(
        Average_Performance = mean(Academic_Performance),
        .groups = "drop"
      )
    
    ggplot(attendance_data,
           aes(x = Class_Attendance_Rate,
               y = Average_Performance)) +
      geom_col() +
      labs(
        title = "Average Performance by Attendance Rate",
        x = "Class Attendance Rate (%)",
        y = "Average Performance"
      ) +
      theme_minimal()
  })
  
  output$student_table <- renderTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)
