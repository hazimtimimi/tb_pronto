# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display provisional monthly or quarterly TB notifications for
# a country using JSON data retrieved from the WHO global tuberculosis database.
# Using the Echarts for R package instead of ggplot2.
# Hazim Timimi, Takuya Yamanaka Apr 2024, updated May 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 3.0"

library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(echarts4r)
library(htmlwidgets)
library(ggplot2)
library(ggtext)
library(shinythemes)
library(lubridate)
library(gtbreport)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# load WIDP files
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
source(here("import/load_gtb.R"))
snapshot_date <- latest_snapshot_date()
report_year <- 2025 # update once the GTBR of the year released
current_year <- year(Sys.Date()) 

tb <- load_gtb("tb") |>
  arrange(iso3)
provisional <- load_gtb("provisional")

# define full sequence
years <- 2021:current_year

months <- sprintf("%02d", 1:12)
month_seq <- as.vector(outer(years, months, paste0))   # "202101" ... "202312"

quarters <- paste0("Q", 1:4)
quarter_seq <- as.vector(outer(years, quarters, paste0))  # "2021Q1" ... "2023Q4"

full_periods <- c(month_seq, quarter_seq)

provisional <- provisional |>
  complete(
    iso3, year = full_periods
  ) |>
  group_by(iso3) |>
  fill(country, g_whoregion, .direction = "downup") |>
  ungroup()

# calc c_newinc
tb <- tb |>
  mutate(
    c_newinc = if_else(
      year >= 2025,
      
      {
        tmp <- pick(matches("new|rec|unk"))
        
        ifelse(
          rowSums(!is.na(tmp)) == 0,
          NA_real_,
          rowSums(tmp, na.rm = TRUE)
        )
      },
      
      {
        tmp <- pick(any_of(c(
          "new_labconf",
          "new_clindx",
          "ret_rel_labconf",
          "ret_rel_clindx",
          "new_ep",
          "ret_rel_ep"
        )))
        
        ifelse(
          rowSums(!is.na(tmp)) == 0,
          NA_real_,
          rowSums(tmp, na.rm = TRUE)
        )
      }
    )
  ) |>
  select(country, iso3, year, c_newinc) |>
  filter(year < report_year)

country_list_widp <- provisional |>
  select(iso3, country) |>
  distinct() |>
  filter(iso3 != "ABW") |>
  arrange(country)

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
                                ", Source code on <a href='https://github.com/yamanakatakuya/tb_pronto/' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
  ))
  )
  ),

tabPanel(
    "Groups of countries",
    # --------------------- MPanel plots for multiple countries ---------------------#
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
                     tags$span(style = "font-size: 90%;", "Number of people with new or relapse episodes of TB notified per year, most recent 5 years for which annual data have been reported to WHO"),
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
                                  ", Source code on <a href='https://github.com/yamanakatakuya/tb_pronto/' target='_blank'>Github</a>.
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

  # Get the latest list of countries with provisional data to use in country dropdown
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  country_list_json <- reactive({
    country_list_widp
  })


  # Build the select country control
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  output$entities <- renderUI({

    already_selected <- input$iso3

    # Create a named list for selectInput
    country_list <- country_list_widp #%>%
    country_list <- setNames(country_list$iso3, country_list$country)

    selectInput(inputId = "iso3",
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
    req(input$iso3)
    
    c_newinc_prov <- provisional |>
      filter(iso3 == input$iso3) |>
      select(year, newinc_prov) |>
      mutate(
        year_month = year,
        year = str_sub(year_month, 1, 4),
        
        period = case_when(
          str_detect(year_month, "Q") ~
            paste0("q_", str_sub(year_month, 6, 6)),
          
          TRUE ~
            paste0("m_", str_sub(year_month, 5, 6))
        )
      ) |>
      select(year, period, newinc_prov) |>
      pivot_wider(
        names_from = period,
        values_from = newinc_prov
      )  |>
      mutate(
        report_frequency = if_any(
          any_of(c("q_1", "q_2", "q_3", "q_4")),
          ~ !is.na(.)
        ) |>
          ifelse(71, 70),
        report_coverage = 35
      ) |>
      arrange(year)
    
    c_newinc_year <- tb |>
      filter(iso3 == input$iso3) |>
      select(year, c_newinc) 
    
    pdata <- list()
    pdata$c_newinc_prov <- c_newinc_prov
    pdata$c_newinc_year <- c_newinc_year
    pdata$dcyear_published <- report_year
    pdata

  })


  # Find out if there is a complete year's worth of provisional notifications
  # to add to the published annual notifications
  # notifications for the latest published year will be NA if not all periods are filled
  publication_year_notifications <- reactive({

    # Make sure there are data to use
    req(pdata()$c_newinc_prov)
    req(pdata()$dcyear_published)

    pdata()$c_newinc_prov %>%

      filter(year == pdata()$dcyear_published) |>
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

  #------------- multiple countries -----------------#
  # Create the charts
  source("build_charts_multicountry.r", local = TRUE)

}

# Run the application
shinyApp(ui = ui, server = server)
