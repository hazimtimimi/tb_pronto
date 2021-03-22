# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display provisional monthly or quarterly TB notifications for
# a country using JSON data retrieved from the WHO global tuberculosis database.
# Using the Echarts for R package instead of ggplot2.
# Hazim Timimi, February - March 2021
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.0"

library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(echarts4r)
library(htmlwidgets)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


ui <- fluidPage(
    title = "Provisional number of new and relapse TB cases",

    # add CSS to colour headings and to prevent printing of the country selector dropdown
    tags$style(HTML("
    #page_header {padding-top: 10px;}
    #provisional_heading {
        color: #4b8f36;
        padding-left: 5px;}
    #annual_heading {
        color: #1790cf;
        padding-left: 5px;}
    @media print {
        #entities, #page_header, #metadata {display: none;}
                    }")),

    fluidRow(tags$div(style = "padding-left: 20px;",
                      textOutput(outputId = "page_header"),
                      uiOutput(outputId = "entities"))
             ),

    fluidRow(

        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput(outputId = "annual_heading", container = h3),
               echarts4rOutput("annual_plot")
               ),
        column(width = 6,
               tags$div(style = "padding-left: 20px;"),
               textOutput(outputId = "provisional_heading", container = h3),
               echarts4rOutput("prov_plot")
        )
    ),

    fluidRow(tags$div(style = "padding-left: 20px; padding-right: 20px;",
                      textOutput(outputId = "page_footer"))
    ),

    fluidRow(tags$div(id = "metadata",
                      style = "padding: 20px; font-style: italic; font-size: 80%;",

                      # Add app version number and links to GTB and Github
                      HTML(paste0(app_version,
                                  ", Source code on <a href='https://github.com/hazimtimimi/tb_pronto/' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))

    ))

)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Backend server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {

    json_url <- "https://extranet.who.int/tme/generateJSON.asp"


    # Get the latest list of counries with provisional data to use in country dropdown
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    country_list_json <- reactive({

        url <- paste0(json_url, "?ds=c_newinc_countries")

        json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))

        return(json)
    })


    # Build the select country control
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$entities <- renderUI({

        already_selected <- input$iso2

        # Create a namedlist for selectInput
        country_list <- country_list_json()$countries %>%
            select(iso2, country) %>%
            arrange(country)

        country_list <- setNames(country_list[,"iso2"], country_list[,"country"])

        selectInput(inputId = "iso2",
                    label = "",
                    choices = country_list,
                    # next line needed to avoid losing the selected country when the language is changed
                    selected = already_selected,
                    selectize = FALSE,
                    width = "380px")
    })


    # Get the data as a JSON file for the chosen country
    # - - - - - - - - - - - - - - - - - - - - - - - - - -

    pdata <- reactive({

        url <- paste0(json_url, "?ds=c_newinc&iso2=", input$iso2)

       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
       return(json)
    })

    # Build an Echarts line graph of monthly or quarterly provisional new and relapse cases
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$prov_plot <- renderEcharts4r({
        # Make sure there are data to plot
        req(pdata()$c_newinc)
        req(pdata()$c_newinc_year)

        # Find out whether we have monthly or quarterly data
        frequency <- as.numeric(pdata()$c_newinc$report_frequency)
        period_prefix <- ifelse(frequency == 71, "q_", "m_")
        period_name  <- ifelse(frequency == 71, "Quarter", "Month")

        # Store data collection year and selected country name in variables
        # to make it easier to display in the chart
        dcyear <- country_list_json()$dcyear
        selected_country <- country_list_json()$countries %>%
            filter(iso2 == input$iso2)

        # Get the total notification for the previous year and calculate the average monthly or quarterly
        previous_year <- pdata()$c_newinc_year %>%
            filter(year == dcyear - 1) %>%
            select(c_newinc)

        previous_year_avge <- ifelse(frequency == 71, previous_year/4, previous_year/12) %>% as.numeric()

        # Build the chart
        # Refer to https://echarts.apache.org/en/option.html for additional properties
        # noting that instead of using Javascript dot notation to set properties such as yAxis.axisTick.show = true
        # have to use an R list such as axisTick = list(show = FALSE)

        prov_chart <-  pdata()$c_newinc %>%

            # Flip to long format
            pivot_longer(cols = starts_with(period_prefix),
                         names_to = "period",
                         # Add "0?" to the names_prefix regex to remove any leading zeros
                         names_prefix = paste0(period_prefix, "0?"),
                         values_to = "c_newinc") %>%

            # Build the chart object
            e_charts(x = period) %>%

            e_line(serie = c_newinc,
                   symbolSize = 12) %>%

            e_title(text = selected_country[, "country"])   %>%

            e_tooltip(formatter = JS(paste0("
                                  function(params){
                                    return(
                                    '", period_name, " ' + params.value[0] + ', ",
                                    dcyear,
                                    "' + '<br />Number of cases: ' + params.value[1])
                                  }
                                "))) %>%

            e_legend(FALSE) %>%

            e_theme("green") %>%

            # Adjust x axis properties
            e_x_axis(name = period_name,
                     nameLocation = 'middle',
                     nameTextStyle = list(fontSize  = 14,
                                          padding = 14) ) %>%

            # Make sure large numbers are not truncated in exes labels
            e_grid(containLabel = TRUE) %>%

            # Add a horizontal line to show previous year's average notification by quarter/month
            e_mark_line(data = list(yAxis = previous_year_avge),
                        title = paste0(dcyear - 1, "\nAverage"),

                        # suppress mouseover tooltip:
                        silent = TRUE)

        # Before defining the y-axis properties
        # Calculate the maximum value of all the points and the previous year average
        max_value <- pdata()$c_newinc %>%
            select(starts_with(period_prefix)) %>%
            max() %>%
            max(c(previous_year_avge))

        if (max_value == previous_year_avge) {

            # The automatic y-axis scale will not show the previous year average, so
            # extend the scale manually

            # Want to calculate the next next convenent highest multiple of 10
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

    # Build an Echarts line graph of monthly or quarterly provisional new and relapse cases
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$annual_plot <- renderEcharts4r({
        # Make sure there are data to plot
        req(pdata()$c_newinc_year)
        req(pdata()$c_newinc)

        # Store the selected country name to make it easier to display in the chart
        selected_country <- country_list_json()$countries %>%
            filter(iso2 == input$iso2)

        # Calculate the total number of cases notified in dcyear
        c_newinc_latest <- pdata()$c_newinc %>%
            select(-starts_with("report_"), -starts_with("previous_year")) %>%
            rowSums(na.rm = TRUE)

        # Append the total for dcyear to the annual timeseries
        c_newinc_annual <- rbind(pdata()$c_newinc_year,
                                 c(country_list_json()$dcyear, c_newinc_latest ) ) %>%

            # Change the year column to text so that chart is similar to the provisional one
            mutate(year = as.character(year))

        # Create the chart
        c_newinc_annual %>%

            e_charts(x = year) %>%

            e_line(serie = c_newinc,
                   symbolSize = 12) %>%

            e_title(text = selected_country[, "country"])   %>%

            e_legend(FALSE) %>%

            e_theme("blue") %>%

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
                                    params.value[0] + '<br />Number of cases: ' + params.value[1])
                                  }
                                "))


    })

    # Put headers and footers in text output components rather than trying to fit them all in the
    # chart titles/subtitles

    output$page_header <- renderText({
        paste0("Select from high TB burden countries and other regional priority countries that reported",
        " at least one case in the final reporting period of ",
              country_list_json()$dcyear,
        ":")
    })

    output$annual_heading <- renderText({
        paste0("Number of new and relapse TB cases per year, 2016 - ",
               country_list_json()$dcyear,
               "*")
    })


    output$provisional_heading <- renderText({
        # Make sure there are data to plot
        req(pdata()$c_newinc)

        # Find out whether we have monthly or quarterly data
        frequency <- as.numeric(pdata()$c_newinc$report_frequency)
        period_name  <- ifelse(frequency == 71, "quarter", "month")

        paste0("Provisional number of new and relapse TB cases per ",
               period_name,
               ", ",
               country_list_json()$dcyear,
               "*")
    })

    output$page_footer <- renderText({
        # Make sure there are data to plot
        req(pdata()$c_newinc)

        # Is reporting comprehensive or partial?
        coverage <- as.numeric(pdata()$c_newinc$report_coverage)

        paste0("* ",
              country_list_json()$dcyear,
               # Customise the footnote for China
              ifelse(input$iso2 == "CN",
                     " data for China are for reported pulmonary TB cases published
                     in the monthly communicable disease surveillance reports of the China
                     Centres for Disease Control, adjusted by a factor of 0.7 to account for
                     the historical relationship between reported and notified cases. ",
                     paste0(" data are provisional as reported to WHO by ",
                             format(Sys.time(),
                                     format = "%Y-%m-%d %H:%M",
                                     tz = "GMT"),
                             " UTC and subject to change.")
                             ),
              ifelse(coverage == 35,
                     " Does not include data from all reporting units.",
                     "")
             )
    })

}

# Run the application
shinyApp(ui = ui, server = server)
