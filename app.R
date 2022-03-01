# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display provisional monthly or quarterly TB notifications for
# a country using JSON data retrieved from the WHO global tuberculosis database.
# Using the Echarts for R package instead of ggplot2.
# Hazim Timimi, Takuya Yamanaka Feb 2022
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.3"

library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(echarts4r)
library(htmlwidgets)
library(ggplot2)
library(ggtext)
library(shinythemes)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <-
  navbarPage(
    "Provisional tuberculosis (TB) notifications",
  tabPanel(
    "Single country",
  #--------------------- by country ---------------------#
  fluidPage(theme = shinytheme("sandstone"),
  title = "Provisional number of people notified with new or relapse episodes of TB",

  # add CSS to colour headings and to prevent printing of the country selector dropdown
  tags$style(HTML("
    #page_header {
        padding-top: 10px;
        padding-left: 20px;}
    #annual_heading {
        color: #1790cf;
        padding-left: 5px;}
    #provisional_heading {
        color: #4b8f36;
        padding-left: 5px;}
    @media print {
        #entities, #page_header, #metadata {display: none;}
    }")),

  fluidRow(tags$div(id = "page_header",
                    HTML("Select from high tuberculosis (TB) burden countries and other regional priority countries
                           that reported provisional notifications to the World Health Organization (WHO)<br />"),
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

  fluidRow(tags$div(id = "metadata2",
                    style = "padding: 20px; font-style: italic; font-size: 80%;",

                    # Add app version number and links to GTB and Github
                    HTML(paste0(app_version,
                                ", Source code on <a href='https://github.com/hazimtimimi/tb_pronto/' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
  ))
  )
  ),

tabPanel(
    "Groups of countries",
    # --------------------- 30 HBCs ---------------------#
    fluidPage(
      tags$style(HTML("
    #hbc_heading {
        color: #1790cf;
        padding-left: 20px;}
    @media print {
        #hbc_heading {display: none;}
    }")),

      fluidRow(
        tags$div(style = "padding-left: 20px;"),
        downloadButton('download_plot', 'Download')
      ),

      br(),

      radioButtons("indicator", HTML("Select an indicator:"),width=700,
                   choiceNames = list(
                     tags$span(style = "font-size: 90%;", "Number of people with new or relapse episodes of TB notified per year, 2016-2020"),
                     tags$span(style = "font-size: 90%;", "Provisional* number of people with new or relapse episodes of TB notified per month or quarter since January 2020")
                   ),
                   choiceValues = list('annual', 'provisional'),
                   selected = "annual"),

      radioButtons("country_set", HTML("Select a group of countries:"),width=700,
                   choiceNames = list(
                     tags$span(style = "font-size: 90%;", "30 TB high burden countries"),
                     tags$span(style = "font-size: 90%;", "30 countries with the biggest contributions to the global shortfall in TB notifications in 2020 vs 2019"),
                     tags$span(style = "font-size: 90%;", "3 global TB watchlist countries")
                   ),
                   choiceValues = list('30hbc', '30mpc','3gwc'),
                   selected = "30hbc"),


      fluidRow(
      tags$div(
        tags$div(style = "padding-left: 20px;"),
        textOutput(outputId = "hbc_heading", container = h3)
        )),

      fluidRow(
          column(width = 12,
               tags$div(style = "padding-left: 20px;"),
               uiOutput("multiple_plot.ui")
               # plotOutput(paste0("multiple_plot"), height = "auto")
               )),

    fluidRow(tags$div(style = "padding-left: 20px; padding-right: 20px;",
                      textOutput(outputId = "page_footer2"))
    ),
    fluidRow(tags$div(style = "padding-left: 20px; padding-right: 20px;",
                      textOutput(outputId = "page_footer3"))
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
    )#fluidpage close
) # tabpanel close
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Back end server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {

  json_url <- "https://extranet.who.int/tme/generateJSON.asp"

  # Get the latest list of countries with provisional data to use in country dropdown
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

    # Create a named list for selectInput
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


  # Find out if there is a complete year's worth of provisional notifications
  # to add to the published annual notifications
  # notifications for the latest published year will be NA if not all periods are filled
  publication_year_notifications <- reactive({

    # Make sure there are data to use
    req(pdata()$c_newinc_prov)
    req(pdata()$dcyear_published)

    pdata()$c_newinc_prov %>%

      filter(year == pdata()$dcyear_published) %>%
      mutate(c_newinc = ifelse(report_frequency == 71,
                               q_1 + q_2 + q_3 + q_4,
                               m_01 + m_02 + m_03 + m_04 + m_05 + m_06 +
                                 m_07 + m_08 + m_09 + m_10 + m_11 + m_12),
             year = paste0(year, "*")) %>%
      select(year, c_newinc) %>%
      filter(!is.na(c_newinc))

  })

  # Create the charts
  source("build_charts_onecountry.r", local = TRUE)

  # Put headers and footers in text output components rather than trying to fit them all in the
  # chart titles/subtitles

  output$annual_heading <- renderText({
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


  output$provisional_heading <- renderText({
    # Make sure there are data to plot
    req(pdata()$c_newinc_prov)

    # Find out whether we have monthly or quarterly data
    frequency <- as.numeric(min(pdata()$c_newinc_prov$report_frequency))
    period_name  <- ifelse(frequency == 71, "quarter", "month")

    paste0("Provisional* number of people with new or relapse episodes of TB notified per ",
           period_name)
  })


  output$page_footer <- renderText({
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
              official annual total subsequently reported to WHO."
    )
  })


  #------------- multiple countries -----------------#
  # Create the charts
  source("build_charts_multicountry.r", local = TRUE)

}

# Run the application
shinyApp(ui = ui, server = server)
