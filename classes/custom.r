#### custom graphs ####

output$header2 <- renderUI({
  req(input$custom_site)
  str1 <- paste0("<h2>", station_meta[[input$custom_site]][1], " (", station_meta[[input$custom_site]][2], " m)", "</h2>")
  if(input$custom_site %in% list_stn_tipping_bucket_errs){
    HTML(paste(str1, p('The tipping bucket is currently malfunctioning at this station please refer to total precipitation (precip pipe) instead.', style = "color:red")))
  }
  else{HTML(paste(str1))}
})



# pull data from mysql db based on user station and year input
custom_data_query <- reactive({
  req(input$custom_site)
  req(input$custom_year)

  # Connect to the first database
  conn1 <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn1))
  query1 <- paste0("SELECT DateTime, WatYr, Snow_Depth FROM clean_", input$custom_site, " WHERE WatYr = ", input$custom_year, ";")
  data1 <- dbGetQuery(conn1, query1)

  # Connect to the second database
  conn2 <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn2))
  query2 <- paste0("SELECT DateTime, WatYr, Snow_Depth FROM qaqc_", input$custom_site, " WHERE WatYr = ", input$custom_year, ";")
  data2 <- dbGetQuery(conn2, query2)

  # Create a new column "Database" to differentiate between the two databases
  data1$Database <- "Clean_sql"
  data2$Database <- "QAQC_sql"

  # Combine data from both databases
  data <- bind_rows(data1, data2)
})

# reactive element to create year list based on available years for chosen station
observe({
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  start_years <- station_meta[[input$custom_site]][3]
  min_year <- unname(unlist(lapply(start_years, max)))
  max_year <- year(Sys.Date()) - ifelse(month(Sys.Date()) < 10, 1, 0) # up to previous water year
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "custom_year", "Select Water Year:", year_range, selected = max_year)
})

# get available variables for selected station
output$varSelection <- renderUI({
  radioButtons(inputId = "custom_var", label = "Select one or two variables:", choices = c("Snow_Depth"), inline = FALSE, selected = "Snow_Depth")
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

  return(df)
})


# plot for custom graphs page
output$plot1 <- renderPlotly({
  req(input$custom_site)
  req(input$custom_year)
  req(input$custom_var)
  req(finalData())

  df <- finalData() %>%
    select(DateTime, Snow_Depth)  # Only select relevant columns

  # Add a new column "Database" to indicate the source
  df$Database <- rep(c("Clean_sql", "QAQC_sql"), each = nrow(df) / 2)

  if ("Snow_Depth" %in% input$custom_var) {
    plot_ly(data = df, x = ~DateTime, y = ~Snow_Depth, color = ~Database, type = "scatter", mode = "lines") %>%
      layout(
        #xaxis = list(title = "DateTime"),
        #yaxis = list(title = "Snow Depth"),
        showlegend = TRUE
      )
  }
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