# functions.R

#A general query function
endpoint_API_query <- function(endpoint, query, start_date, end_date){
  
  if (!endpoint %in% c("animalandveterinary", "drug")){
    message <- paste0("ERROR: Please choose endpoint 'animalandveterinary' or 'drug'.")
    stop(message)
  }
  #construct URL
  base_URL <- "https://api.fda.gov/"
  event <- "/event.json"
  count <- "?count="
  receive_date <- if (endpoint == "drug") "receivedate" else "original_receive_date"
  full_URL <- paste0(base_URL, endpoint, event, count, receive_date)
  #read in data
  output_API <- fromJSON(full_URL)
  receivedate_data <- as_tibble(output_API$results)  
  
  earliest_date <- min(ymd(receivedate_data$time))
  latest_date <- max(ymd(receivedate_data$time))
  #find date boundaries from the database
  if (ymd(start_date) < earliest_date | ymd(end_date) > latest_date){
    
    message <- paste0("ERROR: Please pass a date between ", earliest_date, " and ", latest_date, " in the format of YYYYMMDD", ".")
    stop(message)
    
  } else {
    #extract different types of data
    if (endpoint == "animalandveterinary"){
      
      if (!query %in% c(1,2,3,4,5,6)){
        message <- paste0("ERROR: Please choose query from 1 to 6.")
        stop(message)
      } else if (query == 1){
        query_date_range <- paste0(":[", start_date, "+TO+", end_date, "]&count=", receive_date)
      } else if (query == 2){
        query_code <- "primary_reporter.exact"
        query_date_range <- paste0(":[", start_date, "+TO+", end_date, "]&count=", query_code)
      } else if (query == 3){
        query_code <- "type_of_information.exact"
        query_date_range <- paste0(":[", start_date, "+TO+", end_date, "]&count=", query_code)
      } else if (query == 4){
        query_code <- "animal.breed.breed_component.exact"
        query_date_range <- paste0(":[", start_date, "+TO+", end_date, "]&count=", query_code)
      } else if (query == 5){
        query_code <- "reaction.veddra_term_name.exact"
        query_date_range <- paste0(":[", start_date, "+TO+", end_date, "]&count=", query_code)
      } else if (query == 6){
        query_code <- "outcome.medical_status.exact"
        query_date_range <- paste0(":[", start_date, "+TO+", end_date, "]&count=", query_code)
      }
    } else if (endpoint == "drug"){
      
      if(!query %in% c(1,2)){
        message <- paste0("ERROR: Please choose query from 1 to 2.")
        stop(message)
      } else if (query == 1){
        query_date_range <- paste0(":[", start_date, "+TO+", end_date, "]&count=", receive_date)
      } else if (query == 2){
        query_code <- "primarysource.qualification"
        query_date_range <- paste0(":[", start_date, "+TO+", end_date, "]&count=", query_code)
      }
    }
    #constrict URL according to inputs
    search <- "?search="
    full_URL <- paste0(base_URL, endpoint, event, search, receive_date, query_date_range)
    
    output_API <- fromJSON(full_URL)
    query_data <- as_tibble(output_API$results)
    
    if (query == 1){
      
      query_data <- query_data |>
        mutate(receivedate_ymd = ymd(query_data$time)) |>
        select(receivedate_ymd, count) |>
        filter(receivedate_ymd >= ymd(start_date) & receivedate_ymd <= ymd(end_date))
    } 
    
    query_name <- paste0("query_", endpoint, "_", query, "_", start_date, "_to_", end_date)
    return(setNames(list(earliest_date, latest_date, query_data),
                    c("earliest_date", "latest_date", query_name)))
    
  }
}  

#fun_plot1_1 function
fun_plot1_1 <- function(endpoint, start_date, end_date){
  #query data from the general query function
  reports_over_time_animal  <- endpoint_API_query(endpoint,1,start_date,end_date)[[3]] 
  #calculate summary counts by each year
  reports_over_time_animal_year  <- reports_over_time_animal |>
    group_by(year = lubridate::year(receivedate_ymd)) |>
    summarise(count = sum(count)) |>
    mutate(year = as.character(year))
  
  reports_over_time_animal_month <- reports_over_time_animal |>
    group_by(year = year(receivedate_ymd), month = month(receivedate_ymd, label = TRUE)) |> 
    summarise(count = sum(count)) 
  
  reports_over_time_animal_plot <- reports_over_time_animal_month |>
    mutate(year = as.character(year))
  #create a bar plot using the data above
  plot1_1 <- ggplot(reports_over_time_animal_year, aes(year, count)) +
    geom_bar(stat = "identity", fill = "skyblue", width = 0.5) + 
    labs(x = "Year", y = "Counts") +
    ggtitle(paste0("Counts of Received Adverse events Reports from ", start_date, " to ", end_date, ".")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(plot1_1)
}

#fun_plot1_2 function
fun_plot1_2 <- function(endpoint, start_date, end_date, input_year){
  
  reports_over_time_animal  <- endpoint_API_query(endpoint,1,start_date,end_date)[[3]] 
  #calculate summary counts by each month
  reports_over_time_animal_year  <- reports_over_time_animal |>
    group_by(year = lubridate::year(receivedate_ymd)) |>
    summarise(count = sum(count)) |>
    mutate(year = as.character(year))
  
  reports_over_time_animal_month <- reports_over_time_animal |>
    group_by(year = year(receivedate_ymd), month = month(receivedate_ymd, label = TRUE)) |> 
    summarise(count = sum(count))
  
  reports_over_time_animal_stat <- reports_over_time_animal_month |>
    filter(year == input_year) |>
    mutate(year = as.character(year))
  #create a bar plot using the data above
  plot1_2 <- ggplot(reports_over_time_animal_stat, aes(month, count)) +
    geom_bar(stat = "identity", position = "dodge", fill = "skyblue") + 
    labs(x = "Month", y = "Counts") +
    ggtitle(paste0("Counts of Received Adverse events Reports ", "in ", input_year, ".")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plot1_2
  
}

#fun_plot2 function
fun_plot2 <- function(endpoint, start_date, end_date){
  if (endpoint == "animalandveterinary"){
    #query data from the general query function and calculate percentages by each category
    who_reports <- endpoint_API_query("animalandveterinary",2,start_date,end_date)[[3]] |>
      rename(primarysource = term) |>
      mutate(percentage = count / sum(count) * 100) |>
      select(primarysource, percentage, count)
    
  } else if (endpoint == "drug"){
    #query data from another endpoint
    who_reports <- endpoint_API_query("drug",2,start_date,end_date)[[3]] |>
      mutate(primarysource = as.character(term)) |>
      mutate(primarysource = case_when(
        term == 1 ~ "Physician",
        term == 2 ~ "Pharmacist",
        term == 3 ~ "Other health professional",
        term == 4 ~ "Lawyer",
        term == 5 ~ "Consumer or non-health",
        TRUE ~ as.character(primarysource)
      )) |>
      select(primarysource, count) |>
      mutate(percentage = count / sum(count) * 100) |>
      select(primarysource, percentage, count)
  }
  #create an interactive plot using plotly::plot_ly to see percentages and counts from each category
  plot2 <- plotly::plot_ly(who_reports, labels = ~primarysource, values = ~count, type = 'pie',
                           textinfo = 'label+percent+value',
                           hoverinfo = 'text',
                           text = ~paste(primarysource, "<br>", "Count: ", count, "<br>", "Percentage: ", round(percentage, 2), "%"),
                           
                           textfont = list(size = 8)) |>
    plotly::layout(title = list(text = paste0("Primary Sources of Adverse Events Report from ", start_date, " to ", end_date),
                                font = list(size = 12)))
  
  # Display the plot.
  return(list(data = who_reports, plot = plot2))
}

#fun_plot3 function
fun_plot3 <- function(endpoint, start_date, end_date){
  
  start_year <- substr(start_date, 1, 4)
  end_year <- substr(end_date, 1, 4)
  #cycle through the input years and calculate percentages for each category
  for(i in start_year:end_year){
    
    start_date2 <- as.numeric(paste0(i, "0101"))
    end_date2 <- as.numeric(paste0(i, "1231"))
    #query data
    type_of_report_animal <- endpoint_API_query(endpoint,3,start_date2,end_date2)
    
    type_of_report_animal <- type_of_report_animal[[3]] |>
      rename(reaction = term) |>
      mutate(percentage = count / sum(count) * 100) |>
      select(reaction, percentage, count)
    
    assign(paste0("type_of_report_animal_", i), type_of_report_animal)
    
  }
  
  data_frames <- mget(paste0("type_of_report_animal_", start_year:end_year))
  type_of_report_animal_year_range <- bind_rows(data_frames, .id = "source_year") 
  #create a boxplot
  plot3 <- ggplot(type_of_report_animal_year_range, 
                  aes(reaction,
                      percentage,
                      color=reaction)) +
    geom_boxplot() + 
    # jitter the points to add a little more info to the boxplot.
    geom_jitter() + 
    # add labels to the axes.
    scale_x_discrete(name = "Reaction", labels = LETTERS[1:11]) + 
    scale_y_continuous("Percentage%") +
    scale_color_discrete("Reaction") +
    # add a title
    ggtitle(paste0("Percentage of Types of Adverse Events from ", start_date, " to ", end_date)) +
    # remove the legend because it isn't needed.
    theme(legend.position = "right",
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          plot.title = element_text(size = 11),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) 
  
  plot3
  
}

#fun_plot4 function
fun_plot4 <- function(endpoint, start_date, end_date){
  
  start_year <- substr(start_date, 1, 4)
  end_year <- substr(end_date, 1, 4)
  #cycle through the input years and calculate percentages for each category
  for(i in start_year:end_year){
    
    start_date2 <- as.numeric(paste0(i, "0101"))
    end_date2 <- as.numeric(paste0(i, "1231"))
    outcomes_animal <- endpoint_API_query(endpoint,6,start_date2,end_date2)
    
    outcomes_animal <- outcomes_animal[[3]] |>
      rename(medical.status.outcome = term) |>
      mutate(percentage = count / sum(count) * 100) |>
      select(medical.status.outcome, percentage, count)
    
    assign(paste0("outcomes_animal_", i), outcomes_animal)
    
  }
  
  data_frames <- mget(paste0("outcomes_animal_", start_year:end_year))
  outcomes_animal_year_range <- bind_rows(data_frames, .id = "source_year")
  #create a dotplot
  plot4 <- ggplot(outcomes_animal_year_range, 
                  aes(medical.status.outcome,
                      percentage,
                      color=medical.status.outcome
                  )) +
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1.5) + 
    # jitter the points to add a little more info to the boxplot.
    geom_jitter() + 
    scale_x_discrete(name = "Medical Status Outcome", labels = LETTERS[1:6]) + 
    scale_y_continuous("Percentage%") +
    scale_color_discrete("Outcomes") +
    ggtitle(paste0("Percentage of Medical Status Outcomes after Treatment from ", start_date, " to ", end_date)) + 
    # remove the legend because it isn't needed.
    theme(legend.position = "right",
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 8),
          plot.title = element_text(size = 11),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 8)) 
  
  plot4
  
}

#fun_summary1 function
fun_summary1 <- function(start_date, end_date){
  
  start_year <- substr(start_date, 1, 4)
  end_year <- substr(end_date, 1, 4)
  #cycle through all years from input and calculate relative percentages from each year 
  for(i in start_year:end_year){
    
    start_date2 <- as.numeric(paste0(i, "0101"))
    end_date2 <- as.numeric(paste0(i, "1231"))
    
    type_of_report_animal <- endpoint_API_query("animalandveterinary",3,start_date2,end_date2)
    
    type_of_report_animal <- type_of_report_animal[[3]] |>
      rename(reaction = term) |>
      mutate(percentage = count / sum(count) * 100) |>
      select(reaction, percentage, count)
    
    assign(paste0("type_of_report_animal_", i), type_of_report_animal)
    
  }
  #combine data from all years together
  data_frames <- mget(paste0("type_of_report_animal_", start_year:end_year))
  
  type_of_report_animal_year_range <- bind_rows(data_frames, .id = "source_year") 
  #calculate numerical summaries for percentages for all years
  type_of_report_animal_year_range_stat <- type_of_report_animal_year_range |>
    group_by(reaction) |>
    summarize(
      across(percentage, .fns = list(
        "mean" = mean,
        "median" = median,
        "var" = var,
        "sd" = sd,
        "IQR" = IQR
      ), .names = "{.fn}_{.col}"))
  
  title <- paste0("Summary statistics for percentage of types of adverse events from ", start_date, " to ", end_date, " queried from openFDA <animalandveterinary> endpoint." )
  
  print_tibble_with_title <- function(tbl, title) {
    cat(title, "\n")
    print(tbl)
  }
  
  print_tibble_with_title(type_of_report_animal_year_range_stat, title)
  
}

#fun_summary2 function
fun_summary2 <- function(start_date, end_date){
  
  start_year <- substr(start_date, 1, 4)
  end_year <- substr(end_date, 1, 4)
  #cycle through all years from input and calculate relative percentages from each year 
  for(i in start_year:end_year){
    
    start_date2 <- as.numeric(paste0(i, "0101"))
    end_date2 <- as.numeric(paste0(i, "1231"))
    outcomes_animal <- endpoint_API_query("animalandveterinary",6,start_date2,end_date2)
    
    outcomes_animal <- outcomes_animal[[3]] |>
      rename(medical.status.outcome = term) |>
      mutate(percentage = count / sum(count) * 100) |>
      select(medical.status.outcome, percentage, count)
    
    assign(paste0("outcomes_animal_", i), outcomes_animal)
    
  }
  #combine data from all years together
  data_frames <- mget(paste0("outcomes_animal_", start_year:end_year))
  
  outcomes_animal_year_range <- bind_rows(data_frames, .id = "source_year")
  #calculate numerical summaries for percentages for all years
  outcomes_animal_year_range_stat <- outcomes_animal_year_range |>
    group_by(medical.status.outcome) |>
    summarize(
      across(percentage, .fns = list(
        "mean" = mean,
        "median" = median,
        "var" = var,
        "sd" = sd,
        "IQR" = IQR
      ), .names = "{.fn}_{.col}"))
  
  title <- paste0("Summary statistics for percentage by medical status outcome from ", start_date, " to ", end_date, " queried from openFDA <animalandveterinary> endpoint." )
  
  print_tibble_with_title <- function(tbl, title) {
    cat(title, "\n")
    print(tbl)
  }
  
  print_tibble_with_title(outcomes_animal_year_range_stat, title)
  
}

#fun_contingency_table1 function
fun_contingency_table1 <- function(start_date, end_date){
  #data query
  who_reports_animal <- endpoint_API_query("animalandveterinary",2,start_date,end_date)
  
  who_reports_animal <- who_reports_animal[[3]] |>
    rename(primarysource = term)
  #create a function to include a title
  print_tibble_with_title <- function(tbl, title) {
    cat(title, "\n")
    print(tbl)
  }
  #print a contigency table with a title
  print_tibble_with_title(who_reports_animal, paste0("<Who reports> Contingency Table: Primary Sources of Adverse Events Report from ", start_date, " to ", end_date, " queried from openFDA <animalandveterinary> endpoint."))
  
}

#fun_contingency_table2 function
fun_contingency_table2 <- function(start_date,end_date){
  #data query
  who_reports_drug <- endpoint_API_query("drug",2,start_date,end_date)
  #define variable term
  who_reports_drug <- who_reports_drug[[3]] |>
    mutate(primarysource = as.character(term)) |>
    mutate(primarysource = case_when(
      term == 1 ~ "Physician",
      term == 2 ~ "Pharmacist",
      term == 3 ~ "Other health professional",
      term == 4 ~ "Lawyer",
      term == 5 ~ "Consumer or non-health",
      TRUE ~ as.character(primarysource)
    )) |>
    select(primarysource, count)
  
  print_tibble_with_title <- function(tbl, title) {
    cat(title, "\n")
    print(tbl)
  }
  
  print_tibble_with_title(who_reports_drug, paste0("<Who reports> Contingency Table: Primary Sources of Adverse Events Report from ", start_date, " to ", end_date, " queried from openFDA <drug> endpoint."))
  
}