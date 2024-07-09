library(shiny)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(plotly)

source("functions.R")

# UI
ui <- fluidPage(
  titlePanel("Adverse Events Reports Analysis"),
  
  tabsetPanel(
    id = "tabs",
    
    # About Tab
    tabPanel("About",
             fluidRow(
               column(6,
                      h2("About This App"),
                      tags$p("Purpose: This app provides comprehensive analysis and exploration of adverse events reports retrieved from the FDA's openFDA API."),
                      tags$p("About the data: It allows users to query data from two endpoints: 'animalandveterinary' and 'drug', and analyze the data through interactive plots, contigency tables, numerical summaries, and downloading data subsets."),
                      tags$p("About the data source: For more detailed information about the data source, please visit ",
                             tags$a("openFDA Website", href = "https://open.fda.gov"),
                             "."),
                      tags$p("Purpose of Each Tab:"),
                      tags$ul(
                        tags$li("About: Provides an introduction to the app and its purpose."),
                        tags$li("Data Download: Allow the user to query data from the API, view and subset the returned data, and save it as a file."),
                        tags$li("Data Exploration: Enables exploration of data through interactive plots, contingency tables, and numerical summaries.")
                      )
               ),
               column(6,
                      h2("openFDA Logo"),
                      img(src = "https://open.fda.gov/img/l_openFDA.png")
               )
             )
    ),
    
    # Data Download Tab
    tabPanel("Data Download",
             sidebarLayout(
               sidebarPanel(
                 h2("Data Download"),
                 selectInput("endpoint_download", "Endpoint:", 
                             choices = c("animalandveterinary", "drug"),
                             selected = "animalandveterinary"),
                 selectInput("type_of_data_download", "Select Type of Data:",
                             choices = c("Reports over time", "Who reports", "Types of report", "Outcomes")),
                 dateInput("start_date_download", "Start Date:", value = "2020-01-01"),
                 dateInput("end_date_download", "End Date:", value = "2022-12-31"),
                 actionButton("query_button", "Query API"),
                 conditionalPanel(
                   condition = "input.type_of_data_download == 'Reports over time'",
                   radioButtons("summary_download", "Summary Type:",
                                choices = c("Yearly Summary", "Monthly Summary"))
                 ),
                 conditionalPanel(
                   condition = "input.type_of_data_download == 'Reports over time' && input.summary_download == 'Monthly Summary'",
                   selectInput("input_year_download", "Select Year:", choices = NULL)
                 )
               ),
               mainPanel(
                 tableOutput("download_data_table"),
                 downloadButton("download_subset", "Download Subset Data")
               )
             )
    ),
    
    # Data Exploration Tab
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 h2("Data Exploration"),
                 selectInput("endpoint_explore", "Endpoint:",
                             choices = c("animalandveterinary", "drug"),
                             selected = "animalandveterinary"),
                 selectInput("type_of_data_explore", "Select Type of Data:",
                             choices = c("Reports over time", "Who reports", "Types of report", "Outcomes")),
                 dateInput("start_date_explore", "Start Date:", value = "2020-01-01"),
                 dateInput("end_date_explore", "End Date:", value = "2022-12-31"),
                 conditionalPanel(
                   condition = "input.type_of_data_explore == 'Reports over time'",
                   radioButtons("summary", "Summary Type:",
                                choices = c("Yearly Summary", "Monthly Summary"))
                 ),
                 conditionalPanel(
                   condition = "input.type_of_data_explore == 'Reports over time' & input.summary == 'Monthly Summary'",
                   selectInput("input_year", "Select Year:", choices = NULL)
                 ),
                 conditionalPanel(
                   condition = "input.type_of_data_explore == 'Who reports'",
                   radioButtons("plot_or_contingency", "Select View:",
                                choices = c("Plot", "Contingency Table"),
                                selected = "Plot")
                 ),
                 conditionalPanel(
                   condition = "input.endpoint_explore == 'animalandveterinary' & (input.type_of_data_explore == 'Types of report' | input.type_of_data_explore == 'Outcomes')",
                   radioButtons("plot_or_summary", "Select View:",
                                choices = c("Plot", "Numerical Summary"),
                                selected = "Plot")
                 ),
                 actionButton("explore_button", "Explore Data")
               ),
               mainPanel(
                 plotlyOutput("exploration_plot"),
                 uiOutput("exploration_table"),
                 uiOutput("exploration_contingency")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  query_data <- reactive({
    req(input$query_button)
    
    endpoint <- input$endpoint_download
    type_of_data <- input$type_of_data_download
    start_date <- input$start_date_download
    end_date <- input$end_date_download
    summary <- input$summary_download
    input_year <- input$input_year_download
    
    
    if (endpoint == "animalandveterinary"){
      
      if (type_of_data == "Reports over time" & summary == "Yearly Summary") {
        data <- fun_plot1_1(endpoint, start_date, end_date)$data
      } else if (type_of_data == "Reports over time" & summary == "Monthly Summary"){
        data <- fun_plot1_2(endpoint, start_date, end_date, input_year)$data
      } else if (type_of_data == "Who reports"){
        data <- fun_plot2(endpoint, start_date, end_date)$data
      } else if (type_of_data == "Types of report"){
        data <- fun_plot3(endpoint, start_date, end_date)$data
      } else if (type_of_data == "Outcomes"){
        data <-fun_plot4(endpoint, start_date, end_date)$data
      }
      
      return(data)
    } else if (endpoint == "drug"){
      
      if (type_of_data == "Reports over time" & summary == "Yearly Summary") {
        data <- fun_plot1_1(endpoint, start_date, end_date)$data
      } else if (type_of_data == "Reports over time" & summary == "Monthly Summary"){
        data <- fun_plot1_2(endpoint, start_date, end_date, input_year)$data
      } else if (type_of_data == "Who reports"){
        data <- fun_plot2(endpoint, start_date, end_date)$data
      } else {
        data <- paste("Error: Data is only available for 'Reports over time' and 'Who reports' tabs in 'drug' endpoint.")
      }
      
      return(data)
    }
  })
  
  output$download_data_table <- renderTable({
    query_data()
  })
  
  # Subset data and download as CSV
  output$download_subset <- downloadHandler(
    filename = function() {
      paste("subset_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(query_data(), file, row.names = FALSE)
    }
  )
  
  # Render plot based on user selection in Data Exploration tab
  output$exploration_plot <- renderPlotly({
    req(input$explore_button)
    
    if (input$endpoint_explore == "animalandveterinary"){
      
      if (input$type_of_data_explore == "Reports over time" & input$summary == "Yearly Summary") {
        plot <- fun_plot1_1(input$endpoint_explore, input$start_date_explore, input$end_date_explore)
      } else if (input$type_of_data_explore == "Reports over time" & input$summary == "Monthly Summary"){
        plot <- fun_plot1_2(input$endpoint_explore, input$start_date_explore, input$end_date_explore, input$input_year)
      } else if (input$type_of_data_explore == "Who reports"){
        plot <- fun_plot2(input$endpoint_explore, input$start_date_explore, input$end_date_explore)$plot
      } else if (input$type_of_data_explore == "Types of report" & input$plot_or_summary == "Plot"){
        plot <- fun_plot3(input$endpoint_explore, input$start_date_explore, input$end_date_explore) 
      } else if (input$type_of_data_explore == "Outcomes" & input$plot_or_summary == "Plot"){
        plot <-fun_plot4(input$endpoint_explore, input$start_date_explore, input$end_date_explore)
      } else {
        plot <- NULL
      }
      
      return(plot)
    } else if (input$endpoint_explore == "drug"){
      
      if (input$type_of_data_explore == "Reports over time" & input$summary == "Yearly Summary") {
        plot <- fun_plot1_1(input$endpoint_explore, input$start_date_explore, input$end_date_explore)
      } else if (input$type_of_data_explore == "Reports over time" & input$summary == "Monthly Summary"){
        plot <- fun_plot1_2(input$endpoint_explore, input$start_date_explore, input$end_date_explore, input$input_year)
      } else if (input$type_of_data_explore == "Who reports"){
        plot <- fun_plot2(input$endpoint_explore, input$start_date_explore, input$end_date_explore)$plot
      } else if (input$type_of_data_explore == "Types of report"){
        validate(need(FALSE, "Error: Plot is only available for 'Reports over time' and 'Who reports' tabs in 'drug' endpoint."))
        return(NULL)
      } else if (input$type_of_data_explore == "Outcomes"){
        validate(need(FALSE, "Error: Plot is only available for 'Reports over time' and 'Who reports' tabs in 'drug' endpoint."))
        return(NULL)
      } else {
        plot <- NULL
      }
      return(plot)
    }
  })
  
  query_table <- reactive({
    req(input$explore_button)
    
    if (input$endpoint_explore == "animalandveterinary" & 
        (input$type_of_data_explore == "Types of report" || input$type_of_data_explore == "Outcomes") & 
        input$plot_or_summary == "Numerical Summary") {
      
      start_date <- input$start_date_explore
      end_date <- input$end_date_explore
      
      if (input$type_of_data_explore == "Types of report") {
        table <- fun_summary1(start_date, end_date)
        title <- paste0("Summary statistics for percentage of types of adverse events from ", start_date, " to ", end_date, " queried from openFDA <animalandveterinary> endpoint.")
        return(list(title = title, table = table))
      } else if (input$type_of_data_explore == "Outcomes") {
        table <- fun_summary2(start_date, end_date)
        title <- paste0("Summary statistics for percentage by medical status outcome from ", start_date, " to ", end_date, " queried from openFDA <animalandveterinary> endpoint.")
        return(list(title = title, table = table))
      }} else {
        return(NULL)
      }
  })
  
  output$exploration_table <- renderUI({
    table_data <- query_table()
    
    if (!is.null(table_data)) {
      title <- table_data$title
      table <- table_data$table
      # Construct the UI elements for title and table
      title_ui <- fluidRow(
        column(12, h3(title))
      )
      table_ui <- fluidRow(
        column(12, tableOutput("exploration_table_output"))
      )
      # Wrap the table in renderTable
      output$exploration_table_output <- renderTable({
        table
      })
      # Return UI elements
      return(fluidPage(
        title_ui,
        table_ui
      ))
    } else {
      return(NULL)
    }
  })
  
  query_contingency <- reactive({
    req(input$explore_button)
    
    if (input$endpoint_explore == "animalandveterinary" & input$type_of_data_explore == "Who reports" & input$plot_or_contingency == "Contingency Table") {
      
      start_date <- input$start_date_explore
      end_date <- input$end_date_explore
      
      contingency <- fun_contingency_table1(start_date, end_date)
      return(contingency)
    } else if (input$endpoint_explore == "drug" & input$type_of_data_explore == "Who reports" & input$plot_or_contingency == "Contingency Table"){
      
      start_date <- input$start_date_explore
      end_date <- input$end_date_explore
      
      contingency <- fun_contingency_table2(start_date, end_date)
      return(contingency)
    } else {
      return(NULL)
    }
  })
  
  output$exploration_contingency <- renderUI({
    contingency_data <- query_contingency()
    
    if (!is.null(contingency_data)) {
      contingency_ui <- fluidRow(
        column(12, h3("Contingency Table")),
        column(12, tableOutput("exploration_contingency_output"))
      )
      
      output$exploration_contingency_output <- renderTable({
        contingency_data
      })
      
      return(contingency_ui)
    } else {
      return(NULL)
    }
  })
  
  observe({
    req(input$type_of_data_download, input$summary_download)
    
    if (input$type_of_data_download == "Reports over time" && input$summary_download == "Monthly Summary") {
      years <- year(ymd(input$start_date_download)):year(ymd(input$end_date_download))
      updateSelectInput(session, "input_year_download", choices = years)
    } else {
      updateSelectInput(session, "input_year_download", choices = NULL)
    }
  })
  
  # Update year choices based on data available
  observe({
    req(input$type_of_data_explore, input$summary)
    
    if (input$type_of_data_explore == "Reports over time" && input$summary == "Monthly Summary") {
      years <- year(ymd(input$start_date_explore)):year(ymd(input$end_date_explore))
      updateSelectInput(session, "input_year", choices = years)
    } else {
      updateSelectInput(session, "input_year", choices = NULL)
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)