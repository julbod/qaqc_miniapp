# Define the variable at the top
variables <- c("Snow_Depth", "SWE", "Air_Temp", "PC_Raw_Pipe", "RH", "BP", "PP_Tipper")

# Define formatHoverText function outside the renderPlotly block
formatHoverText <- function(variable, flags_column, flags) {
  prefix <- if (variable == "SWE") "SWE_flags: " else if (variable == "Snow_Depth") "Snow_Depth_flags: " else if (variable == "Air_Temp") "Air_Temp_flags: " else if (variable == "PC_Raw_Pipe") "PC_Raw_Pipe_flags: " else if (variable == "RH") "RH_flags: " else if (variable == "BP") "BP_flags: " else if (variable == "PP_Tipper") "PP_Tipper_flags: " else ""
  return(paste(prefix, ifelse(length(flags) > 0, paste(flags, collapse = ", "), "NA")))
}

output$header2 <- renderUI({
  req(input$custom_site)
  str1 <- paste0("<h2>", station_meta[[input$custom_site]][1], " (", station_meta[[input$custom_site]][2], " m)", "</h2>")
  HTML(paste(str1))
})

# pull data from mysql db based on user station and year input
custom_data_query <- reactive({
  req(input$custom_site)
  req(input$custom_year)

  # Connect to the first database
  conn1 <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn1))
  query1 <- paste0("SELECT DateTime, WatYr, ", paste(variables, collapse = ", "), " FROM clean_", input$custom_site, " WHERE WatYr = ", input$custom_year, ";")
  data1 <- dbGetQuery(conn1, query1)

  # Connect to the second database
  conn2 <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn2))
  query2 <- paste0("SELECT DateTime, WatYr, Snow_Depth, Snow_Depth_flags, SWE, SWE_flags, Air_Temp, Air_Temp_flags, PC_Raw_Pipe, PC_Raw_Pipe_flags, RH, RH_flags, BP, BP_flags, PP_Tipper, PP_Tipper_flags FROM qaqc_", input$custom_site, " WHERE WatYr = ", input$custom_year, ";")
  data2 <- dbGetQuery(conn2, query2)

  # Create a new column "Database" to differentiate between the two databases
  data1$Database <- "Clean_sql"
  data2$Database <- "QAQC_sql"

  # Combine data from both databases
  data1 <- as.data.frame(lapply(data1, function(x) if(is.character(x)) x else x))
  data2 <- as.data.frame(lapply(data2, function(x) if(is.character(x)) x else x))

  # fix in case all values in qaqc are NULL
  data2$SWE <- as.numeric(ifelse(is.na(data2$SWE), data2$SWE, data2$SWE))
  data2$Snow_Depth <- as.numeric(ifelse(is.na(data2$Snow_Depth), data2$Snow_Depth, data2$Snow_Depth))
  data2$Air_Temp <- as.numeric(ifelse(is.na(data2$Air_Temp), data2$Air_Temp, data2$Air_Temp))
  data2$PC_Raw_Pipe <- as.numeric(ifelse(is.na(data2$PC_Raw_Pipe), data2$PC_Raw_Pipe, data2$PC_Raw_Pipe))
  data2$RH <- as.numeric(ifelse(is.na(data2$RH), data2$RH, data2$RH))
  data2$BP <- as.numeric(ifelse(is.na(data2$BP), data2$BP, data2$BP))
  data2$PP_Tipper <- as.numeric(ifelse(is.na(data2$PP_Tipper), data2$PP_Tipper, data2$PP_Tipper))

data <- bind_rows(data1, data2)
})

# reactive element to create year list based on available years for chosen station
observe({
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  start_years <- station_meta[[input$custom_site]][3]
  min_year <- unname(unlist(lapply(start_years, max)))
  max_year <- year(Sys.Date()) - ifelse(month(Sys.Date()) < 10, 1, 0) # up to the previous water year
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "custom_year", "Select Water Year:", year_range, selected = max_year)
})

# get available variables for selected station
output$varSelection <- renderUI({
  radioButtons(inputId = "custom_var", 
               label = "Select one or two variables:", 
               choices = variables,
               inline = FALSE,
               selected = variables[1])  # Set a default selection, e.g., the first variable
})


# ensure only two variables are selected
observe({
  if(length(input$custom_var) > 2){
    updateCheckboxGroupInput(session, "custom_var", selected = tail(input$custom_var, 2))
  }
})

output$slider <- renderUI({
  req(custom_data_query())
  sliderInput(inputId = "sliderTimeRange", label = "",
              min = min(custom_data_query()$DateTime),
              max = max(custom_data_query()$DateTime),
              value = c(min(custom_data_query()$DateTime),
                        max(custom_data_query()$DateTime)),
              step = 3600,
              width = '85%',
              height )
})

#filter preset data query
customDataFilter <-  reactive({
  req(input$sliderTimeRange)
  df <- custom_data_query()
  df %>%  filter(DateTime >= input$sliderTimeRange[1] & DateTime <= input$sliderTimeRange[2])
})

# final data set
finalData <- reactive({
  req(customDataFilter())
  req(input$custom_var)

  df <- customDataFilter()

  # Convert all columns to string, preserving non-numeric values with commas
  df <- as.data.frame(lapply(df, function(x) {
    if (is.character(x)) {
      convert_to_string <- function(value) {
        return(value)
      }
      return(convert_to_string(x))
    } else {
      return(x)
    }
  }))

  return(df)
})

# plot for custom graphs page
output$plot1 <- renderPlotly({
  req(input$custom_site, input$custom_year, input$custom_var, finalData())

  df <- finalData() %>%
    select(DateTime, !!!input$custom_var, SWE_flags, Snow_Depth_flags, Air_Temp_flags, PC_Raw_Pipe_flags, RH_flags, BP_flags, PP_Tipper_flags)

  # Check if all values in QAQC data are NULL or NaN for each variable
  all_null_or_nan <- sapply(df[grep("_flags$", colnames(df))], function(x) all(is.na(x)))

  # Add a new column "Database" to indicate the source
  df$Database <- rep(c("Clean_sql", "QAQC_sql"), each = nrow(df) / 2)

  # Extract the flags values from QAQC_sql database
  qaqc_flags <- df[df$Database == "QAQC_sql", c("DateTime", paste(input$custom_var, "flags", sep = "_"))]

  # Merge the extracted flags with the original dataframe based on DateTime
  df <- left_join(df, qaqc_flags, by = "DateTime")

  # Use the merged flags for Clean_sql database
  df[[paste(input$custom_var, "flags", sep = "_")]] <- ifelse(df$Database == "Clean_sql", df[[paste(input$custom_var, "flags.y", sep = "_")]], df[[paste(input$custom_var, "flags.x", sep = "_")]])

  # Create hover text for each variable
  df$hover_text <- mapply(formatHoverText, input$custom_var, paste(input$custom_var, "_flags", sep = "_"), df[[paste(input$custom_var, "flags", sep = "_")]])

  # Check if all values in QAQC data are NULL or NaN for each variable
  all_null_or_nan <- sapply(df[grep("_flags$", colnames(df))], function(x) all(is.na(x)))

# plot data
  plots <- lapply(input$custom_var, function(variable) {
    # Check if the variable column exists in the dataframe before plotting
    if (variable %in% colnames(df)) {
      # Check if all values in QAQC data are NULL or NaN for the current variable
      if (all_null_or_nan[paste(variable, "flags", sep = "_")]) {
        p <- plot_ly() %>%
          layout(
            showlegend = TRUE,
            title = variable,
            annotations = list(
              list(
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                text = "No QAQC data exists at this station for this variable or water year",
                showarrow = FALSE,
                font = list(size = 16, color = "red")  # Set font color to red
              )
            )
          )
      } else {
        p <- plot_ly(data = df, x = ~DateTime, y = df[[variable]], color = ~Database, type = "scatter", mode = "lines",
                     text = ~hover_text) %>%
          layout(
            showlegend = TRUE,
            title = variable
          )
      }

      p
    } else {
      print(paste("Warning: Variable", variable, "not found in the dataframe. Skipping..."))
      print(str(df))  # Print the structure of the dataframe for debugging purposes
      NULL  # Return NULL if the variable is not found
    } 
  })

  subplot(plots)
})

#### render partner logo ui ####
output$partnerLogoUI_custom <- renderUI({
  req(input$custom_site)
  cur_stn <- input$custom_site
  station_meta[[cur_stn]]['logos']
})

# create warning for down stations
observe({
  req(preset_data_query())
  req(input$custom_site)
  if(input$smenu == "cstm_graph"){
    if(input$custom_site %in% down_stations){
      showModal(modalDialog(
        title = "Warning:",
        paste("This station is currently offline."),
        easyClose = T
      ))
    }
  }
}
)
