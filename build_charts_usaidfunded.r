# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build the charts to display annual and provisional monthly or quarterly
# notifications using the echarts4r package (based on ECharts)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Build an Echarts line graph of annual new and relapse cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$annual_usaid_plot <- renderEcharts4r({
  # Make sure there are data to plot
  req(pdata()$c_newinc_year)
  req(pdata()$c_newinc_prov)
  
  # Store the selected country name to make it easier to display in the chart
  selected_country <- country_list_json()$countries %>%
    filter(iso2 == input$iso2)
  
  c_newinc_annual <- pdata()$c_newinc_year %>%
    # Change the year column to text so that chart is similar to the provisional one
    mutate(year = as.character(year))
  
  # Find out if there is a complete year's worth of provisional notifications
  # to add to the published annual notifications
  if (nrow(publication_year_notifications() == 1)) {
    
    # Add the latest year of provisional notifications to the annual time series
    c_newinc_annual <- rbind(c_newinc_annual, publication_year_notifications())
    
  }
  
  
  # Create the chart
  c_newinc_annual %>%
    filter(year >= 2022) %>%
    
    e_charts(x = year) %>%
    
    e_line(serie = c_newinc,
           symbolSize = 12) %>%
    
    e_title(text = selected_country[, "country"])   %>%
    
    e_legend(FALSE) %>%
    
    # Adjust x and y axis properties
    e_y_axis(axisLine = list(show = FALSE),
             axisTick = list(show = FALSE)) %>%
    
    e_x_axis(name = "Year",
             nameLocation = 'middle',
             nameTextStyle = list(fontSize  = 14,
                                  padding = 14)) %>%
    
    # Make sure large numbers are not truncated in exes labels
    e_grid(containLabel = TRUE) %>%
    
    e_tooltip(formatter = JS("
                                  function(params){
                                    return(
                                    params.value[0] + '<br />Number notified: ' + params.value[1])
                                  }
                                "))
  
})



# Build an Echarts line graph of monthly or quarterly provisional new and relapse cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$prov_usaid_plot <- renderEcharts4r({
  
  # Make sure there are data to plot
  req(pdata()$c_newinc_prov)
  
  # Store selected country name in variable
  # to make it easier to display in the chart
  selected_country <- country_list_json()$countries %>%
    filter(iso2 == input$iso2)
  
  # Put the provisional data into a variable to make subsequent code easier to read
  data_to_plot <- pdata()$c_newinc_prov
  
  # Find out whether we have monthly or quarterly data
  frequency <- as.numeric(min(data_to_plot$report_frequency))
  # period_prefix <- ifelse(frequency == 71, "q_", "m_")
  period_name  <- "Month"
  
  # Check if some years have a different frequency
  # if (min(data_to_plot$report_frequency) != max(data_to_plot$report_frequency)) {
  
  # Convert any years with quarterly data to monthly equivalent
  data_to_plot <- data_to_plot %>%
    mutate(m_01 = ifelse(report_frequency == 71, round(q_1/3), m_01),
           m_02 = ifelse(report_frequency == 71, round(q_1/3), m_02),
           m_03 = ifelse(report_frequency == 71, round(q_1/3), m_03),
           
           m_04 = ifelse(report_frequency == 71, round(q_2/3), m_04),
           m_05 = ifelse(report_frequency == 71, round(q_2/3), m_05),
           m_06 = ifelse(report_frequency == 71, round(q_2/3), m_06),
           
           m_07 = ifelse(report_frequency == 71, round(q_3/3), m_07),
           m_08 = ifelse(report_frequency == 71, round(q_3/3), m_08),
           m_09 = ifelse(report_frequency == 71, round(q_3/3), m_09),
           
           m_10 = ifelse(report_frequency == 71, round(q_4/3), m_10),
           m_11 = ifelse(report_frequency == 71, round(q_4/3), m_11),
           m_12 = ifelse(report_frequency == 71, round(q_4/3), m_12),
           
           text = ifelse(report_frequency == 71, paste("(quarterly averaged as monthly)"), "")
    )
  
  # }
  
  # Get the total notification for 2019 (final pre-pandemic year) and calculate the average monthly or quarterly
  pretrump <- pdata()$c_newinc_year %>%
    filter(year == max(year) & year < 2025) %>%
    select(c_newinc)
  
  prepandemic_year_avge <- pretrump/12 %>% as.numeric()
  
  current_yearmonth <- Sys.Date()
  
  pretrump_year <- pdata()$c_newinc_year %>%
    filter(year == max(year) & year < 2025) %>%
    select(year) %>%
    as.numeric()
  
  data_to_plot <- data_to_plot %>%
    filter(year >= pretrump_year)
  
  # Build the chart
  # Refer to https://echarts.apache.org/en/option.html for additional properties
  # noting that instead of using Javascript dot notation to set properties such as yAxis.axisTick.show = true
  # have to use an R list such as axisTick = list(show = FALSE)
  
  prov_chart <-  data_to_plot %>%
    select(!starts_with("q_")) %>%
    # Flip to long format
    pivot_longer(cols = starts_with("m_"),
                 names_to = "period",
                 # Add "0?" to the names_prefix regex to remove any leading zeros
                 names_prefix = paste0("m_", "0?"),
                 values_to = "c_newinc") %>%
    mutate(date = as.Date(paste(year, period, "01", sep="-"), format="%Y-%m-%d")) %>%
    filter(date < current_yearmonth-60) %>%
    
    mutate(date = format(date, "%b %Y")) %>%
    
    # select(year, period, c_newinc) %>%
    
    # Grouping by year makes Echarts show each year as a separate line (named data series)
    # group_by(year) %>%
    
    # Build the chart object
    e_charts(x = date) %>%
    
    e_line(serie = c_newinc,
           symbolSize = 8) %>%
    
    e_title(text = selected_country[, "country"])   %>%
    
    # params.seriesName is the year because the dataframe is grouped by year
    e_tooltip(formatter = JS(paste0("
                                  function(params){
                                    return(
                                     params.value[0] + 
                                     '<br />Number notified*: ' + params.value[1] )
                                  }
                                "))) %>%
    
    e_legend(F) %>%
    
    # Adjust x axis properties
    e_x_axis(name = period_name,
             nameLocation = 'middle',
             nameTextStyle = list(fontSize  = 14,
                                  padding = 14),
             axisLabel = list(rotate = 0)
    ) %>%
    
    # Make sure large numbers are not truncated in exes labels
    e_grid(containLabel = TRUE) %>%
    
    # Add a horizontal line to show pre-pandemic year's average notification by quarter/month
    e_mark_line(data = list(yAxis = prepandemic_year_avge$c_newinc),
                title = paste0(pretrump_year,"\nAverage"),
                
                # suppress mouseover tooltip:
                silent = TRUE, lineStyle = list(color = '#DAA520'))
  
  # Before defining the y-axis properties
  # Calculate the maximum value of all the points and the pre-pandemic year average
  max_value <- data_to_plot %>%
    select(starts_with("m_")) %>%
    max(na.rm = TRUE) %>%
    max(c(prepandemic_year_avge$c_newinc), na.rm = TRUE)
  # 
  if (max_value == prepandemic_year_avge) {
    
    # The automatic y-axis scale will not show the pre-pandemic year average, so
    # extend the scale manually
    
    # Want to calculate the next next convenient highest multiple of 10
    # one order of magnitude below that of the previous year's average
    # e.g for 115 we want 120 (next multiple of 10), for 1568 we want 1600 (next multiple of 100), etc.
    
    order_of_magnitude <- 10^floor(log10(max_value) - 1)
    
    max_value <- ceiling(max_value/order_of_magnitude) * order_of_magnitude
    
    prov_chart <- prov_chart %>%
      
      e_y_axis(axisLine = list(show = FALSE),
               axisTick = list(show = FALSE),
               # over-ride the scales generated automatically because the
               # previous year average doesn't appear if it is higher than any of the
               # monthly or quarterly data points
               min = 0,
               max = max_value)
    
  } else {
    
    # The maximum value comes from the data points so let eCharts figure out the scale
    prov_chart <- prov_chart %>%
      
      e_y_axis(axisLine = list(show = FALSE),
               axisTick = list(show = FALSE))
    
  }
  
  
})  
# Put headers and footers in text output components rather than trying to fit them all in the
# chart titles/subtitles

output$annual_heading2 <- renderText({
  # Make sure there are data to plot
  req(pdata()$dcyear_published)
  
  paste0("Number of people with new or relapse episodes of TB notified per year, ",
         # timeseries starts 5 years before the most recent year of publication
         pdata()$dcyear_published - 5,
         " - ",
         # See if we have provisional data for the whole of the latest publication year
         ifelse(nrow(publication_year_notifications()) == 1,
                paste0(pdata()$dcyear_published,"*"),
                pdata()$dcyear_published - 1)
  )
})


output$provisional_heading2 <- renderText({
  # Make sure there are data to plot
  req(pdata()$c_newinc_prov)
  
  # Find out whether we have monthly or quarterly data
  frequency <- as.numeric(min(pdata()$c_newinc_prov$report_frequency))
  period_name  <- ifelse(frequency == 71, "quarter", "month")
  
  paste0("Provisional* number of people with new or relapse episodes of TB notified per ",
         period_name)
})


output$page_footer4 <- renderText({
  # Make sure there are data to plot
  req(pdata()$c_newinc_prov)
  
  # Is reporting comprehensive or partial?
  data_to_plot <- pdata()$c_newinc_prov
  
  report_coverage <- as.numeric(max(data_to_plot$report_coverage))
  
  paste0("* ",
         # Customise the footnote for China
         ifelse(input$iso2 == "CN",
                " Data for China are for reported pulmonary TB cases published
                     in the monthly communicable disease surveillance reports of the China
                     Centres for Disease Control, adjusted by a factor of 0.7 to account for
                     the historical relationship between reported and notified cases. ",
                paste0(" Data are provisional as reported to WHO by ",
                       format(Sys.time(),
                              format = "%Y-%m-%d %H:%M",
                              tz = "GMT"),
                       " UTC and subject to change.")
         ),
         ifelse(report_coverage == 35,
                " Does not include data from all reporting units.",
                ""),
         " Monthly/quarterly totals for a given year may differ from the final and
              official annual total subsequently reported to WHO. \n",
         "For countries that reported the providinal number of TB notifications on a quarterly basis, the data are averaged as a monthly basis."
  )
})
