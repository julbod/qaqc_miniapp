#### Annual Comparisons ####

output$header3 <- renderUI({
  req(input$annual_site)
  str1 <- paste0("<h2>", station_meta[[input$annual_site]][1], " (", station_meta[[input$annual_site]][2], " m)", "</h2>")
  HTML(paste(str1))
})

# reactive element to create year list based on available years for chosen station
observe({
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  start_years_ann <- station_meta[[input$annual_site]][3]
  min_year <- unname(unlist(lapply(start_years_ann, max)))
  max_year <- wtr_yr(Sys.Date(), 10)
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "compare_year", "Select Water Years to Compare: ", year_range, selected = c(max_year, (max_year-1)))
})

# get available variables for selected station
output$varSelection_ann <- renderUI({
  # get colnames from reactive dataset
  stnVars <- unname(unlist(station_meta[[input$annual_site]][6]))

  var_subset <- Filter(function(x) any(stnVars %in% x), varsDict)

  radioButtons(inputId = "compare_var", label = "Select one variable: ", choices = var_subset, inline = FALSE, selected = "SWE")
})

annual_data_query <- reactive({

  req(input$annual_site)
  req(input$compare_var)

  withProgress(message = 'Requesting Data... ', value = 1, {
    conn_qaqc <- do.call(DBI::dbConnect, args)
    on.exit(DBI::dbDisconnect(conn_qaqc))
    query_qaqc <- paste0("SELECT DateTime, WatYr,", input$compare_var, " FROM qaqc_", input$annual_site,";")
    data_qaqc <- dbGetQuery(conn_qaqc, query_qaqc) %>%
      mutate(
        plotTime = if_else(month(DateTime) < 10,
                           weatherdash::set_yr(DateTime, 1901),
                           weatherdash::set_yr(DateTime, 1900))) %>%
      filter(WatYr > 0,
             WatYr %in% input$compare_year)

    conn_clean <- do.call(DBI::dbConnect, args)  # Assuming args_clean is the connection details for the "clean_" database
    on.exit(DBI::dbDisconnect(conn_clean))
    query_clean <- paste0("SELECT DateTime, WatYr,", input$compare_var, " FROM clean_", input$annual_site, ";")
    data_clean <- dbGetQuery(conn_clean, query_clean) %>%
      mutate(
        plotTime = if_else(month(DateTime) < 10,
                           weatherdash::set_yr(DateTime, 1901),
                           weatherdash::set_yr(DateTime, 1900))) %>%
      filter(WatYr == 2024)  # Filter only the missing WatYr (2024)

    # Combine data from qaqc and clean databases
    combined_data <- bind_rows(data_qaqc, data_clean)

    # Filter only relevant years
    result_data <- combined_data %>%
      filter(WatYr %in% input$compare_year)
  })

})

# plot all years
output$plot2 <- renderPlotly({
  req(annual_data_query())
  req(input$annual_site)
  req(input$compare_var)
  req(input$compare_year)

  df <- annual_data_query()

  varNames <- names(Filter(function(x) unlist(x) %in% input$compare_var, varsDict))

  plot_ly(df,
          x = ~plotTime, # plot without year
          y = ~df[,3],
          text = ~DateTime, # bring in datetime with year for hover text
          color = as.factor(df$WatYr),
          colors = cbs_pal,
          type = "scatter",
          mode = "lines",
          hovertemplate = paste('<b>%{text}</b><br>%{yaxis.title.text}: %{y}<extra></extra>')

  ) %>%

    layout(
      xaxis = c(generalAxLayout,
                list(title = "",
                     type = 'date',
                     tickformat = "%b %d" # Mon Day
                )),
      yaxis = c(generalAxLayout, list(title=paste0("<b>",varNames[1],"</b>"))),
      margin = list(r = 50, l = 50),
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "#f5f5f5",
      hovermode = 'x'
    )
})


#### render partner logo ui ####
output$partnerLogoUI_annCompare <- renderUI({
  req(input$annual_site)
  cur_stn <- input$annual_site
  station_meta[[cur_stn]]['logos']
})
