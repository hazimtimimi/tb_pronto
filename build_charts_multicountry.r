# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build the dataframes for groups of countries to display annual and provisional monthly or quarterly
# notifications using ggplot
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ----------------  plot title  ---------------------#

output$hbc_heading <- renderText({
  if(input$indicator == "provisional") {
    paste0("Provisional* number of people with new or relapse episodes of TB notified per month or quarter since January 2020")

  } else {
    paste0("Number of people with new or relapse episodes of TB notified per year, 2016-2020")

  }
})


# Generate a dataframe for provisional monthly data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

i <- "PH"
# define sets of countries
pdata_country <- reactive({

  if(input$country_set == "30hbc") {

    y<-c("AGO","BGD","BRA","CAF","CHN","PRK","COG","COD","ETH","GAB","IND",
         "IDN","KEN","LSO","LBR","MNG","MOZ","MMR","NAM","NGA","PAK",
         "PNG","PHL","SLE","ZAF","THA","UGA","TZA","VNM","ZMB")
  return(y)

} else {

    if(input$country_set == "30mpc") {

      y<-c('AGO','BGD','BRA','CHN','IDN','IND','KEN','MMR','PAK','PER','PHL',
           'RUS','UGA','UKR','VNM','ZAF','ETH','MEX','PNG','THA','NPL','MYS',
           'ROU','ZWE','KAZ','AZE','KHM','COL','LSO','KGZ','UGA')


  } else {

    y<-c("KHM","RUS","ZWE")
    return(y)

  }
}
})

# count number of countries for adjusting size of output figure
pdata_count <- reactive({
  y <- pdata_country()
  count <- as.data.frame(table(y))
  count <- sum(count$Freq)
  count <- round(count/3,0)
})

# define url to load json data
url <- reactive({
  json_url <- "https://extranet.who.int/tme/generateJSON.asp"

  if(input$country_set == "30hbc") {

    url <- paste0(json_url, "?ds=c_newinc_group&group_code=g_hb_tb")

  } else {

  if(input$country_set == "30mpc") {

    url <- paste0(json_url, "?ds=c_newinc_group&group_code=g_shortfall_2020")

  } else {

    url <- paste0(json_url,"?ds=c_newinc_group&group_code=g_watchlist")

  }
  }
})


# Generate a dataframe for provisional monthly dataset
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

pdata_prov <- reactive({
  req(url())
  json <- fromJSON(readLines(url(), warn = FALSE, encoding = 'UTF-8'))
  pr <- json[[2]]
  json[[1]] %>% inner_join(pr,by=c("iso2")) -> pr

  # #load dataset
  # pr <- read.csv("https://extranet.who.int/tme/generateCSV.asp?ds=provisional_notifications", header = T)
  #
  # y <- pdata_country()
  # pr <- filter(pr, iso3 %in% y)

  })


# Generate a dataframe for annual data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

pdata_annual <- reactive({
  req(url())
  json <- fromJSON(readLines(url(), warn = FALSE, encoding = 'UTF-8'))
  cn <- json[[3]]
  json[[1]] %>% inner_join(cn,by=c("iso2")) -> cn

})


# Generate a dataframe for average in prepandemic year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pdata_prepandemic <- reactive({

  cn <- pdata_annual()
  pr <- pdata_prov()

  pr2 <- pr %>% select(iso2,report_frequency) %>%
    add_row(iso2="CG",report_frequency=70)
  pr2 %>% inner_join(cn,by=c('iso2')) -> prepandemic_year

  prepandemic_year_avge <- prepandemic_year %>%
    filter(year == 2019) %>%
    mutate(prepandemic_year_avge=as.numeric(ifelse(report_frequency == 71, c_newinc/4, c_newinc/12))) %>%
    unique()

})



# Plot figure for HBCs: reactive > download
#----------------------------------------------------------
multi_plot <- reactive({
  ## figure for provisional monthly data
  if(input$indicator == "provisional") {
    req(pdata_prov())
    req(pdata_prepandemic())
    # Put the provisional data into a variable to make subsequent code easier to read
    data_to_plot <- pdata_prov()

    # Find out whether we have monthly or quarterly data
    data_to_plot$frequency <- as.numeric(data_to_plot$report_frequency)
    data_to_plot$period_prefix <- ifelse(data_to_plot$frequency == 71, "q_", "m_")
    data_to_plot$period_name  <- ifelse(data_to_plot$frequency == 71, "Quarter", "Month")

    # df only with quarterly data
    df1 <- data_to_plot %>%
      subset(!is.na(q_1)) %>%
      # Flip to long format
      pivot_longer(cols = starts_with("q"),
                   names_to = "period",
                   # Add "0?" to the names_prefix regex to remove any leading zeros
                   names_prefix = "q_",
                   values_to = "c_newinc") %>%
      select(country, year, period,period_name, c_newinc) %>%
      # Grouping by year makes Echarts show each year as a separate line (named data series)
      group_by(year) %>%
      mutate(text=NA)

    # df only with monthly data
    df2 <- data_to_plot %>%
      subset(is.na(q_1)) %>%
      # Flip to long format
      pivot_longer(cols = starts_with("m"),
                   names_to = "period",
                   # Add "0?" to the names_prefix regex to remove any leading zeros
                   names_prefix = paste0("m_", "0?"),
                   values_to = "c_newinc") %>%
      select(country, year, period,period_name, c_newinc) %>%
      add_row(country="Congo",year=2021,period="1",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="2",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="3",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="4",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="5",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="6",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="7",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="8",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="9",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="10",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="11",period_name="Month") %>%
      add_row(country="Congo",year=2021,period="12",period_name="Month") %>%

      # Grouping by year makes Echarts show each year as a separate line (named data series)
      group_by(year) %>%
      mutate(text=ifelse(country=="Congo","No Data Available**",NA))

    # df to be merged
    df <- rbind(df1,df2)
    df <- df %>%
      mutate(year=factor(year)) %>%
      mutate(period=ifelse(period_name=="Quarter",paste0("Q",period),period)) %>%
      mutate(period=factor(period, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","Q1","Q2","Q3","Q4")))
    #
    # load prepandemic data
    df_prepandemic <- pdata_prepandemic()

    df_prepandemic <- df_prepandemic %>%
      select(country,prepandemic_year_avge)

    df <- df %>%
      inner_join(df_prepandemic, by=c('country'))

    p <- df %>%
      # Build the chart object
      ggplot(aes(x = period, y = c_newinc, group=year, color=year)) +
      geom_line( size = 1.2
      ) +
      scale_color_manual(values=c("dodgerblue3", "limegreen","deeppink2")) +
      geom_point(size = 5) +
      geom_point(size = 3, color = "white") +
      geom_segment(data = df, aes(x = 0, y = prepandemic_year_avge, xend = Inf, yend = prepandemic_year_avge),
                   arrow = arrow(length = unit(0.03, "npc"), type="closed"),linetype = "dashed",color = "goldenrod3",size=0.9) +
      geom_text(data = df, aes(label=paste0("2019\naverage"),x=Inf,y=prepandemic_year_avge*1.1),hjust=1.2,col="grey50")+
      geom_text(data = df, aes(label=text,x=4,y=prepandemic_year_avge*0.6),hjust=-0.1,col="grey50",size=7)+
      # geom_hline(data = df, aes(yintercept = prepandemic_year_avge), linetype = "dashed", color = "limegreen", size = 1) +
      facet_wrap(~ country,strip.position="top",ncol=3, scales="free",drop = FALSE)+
      xlab("Month/Quarter") +
      ylab(NULL)+ scale_y_continuous(labels = function(x) format(x, big.mark = " ",
                                                                 scientific = FALSE),
                                     limits = function(x){c(0, max(0.1, x))})

    p <- p +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold"),
            strip.placement = "outside",strip.background = element_blank(),
            panel.grid.major.x  = element_blank(),
            panel.grid.major.y  = element_line(colour = "grey80"),
            panel.grid.minor.y  = element_line(colour = "grey80"),
            axis.line.x = element_line(colour = "black", size = 1),
            text = element_text(size = 20),
            panel.background = element_rect(fill = "white"),
            legend.position = "top",
            legend.title=element_blank(),
            panel.spacing = unit(3, "lines"),
            legend.key = element_rect(colour = NA, fill = NA)

      )

    p


  } else {
    ## figure for annual data
    req(pdata_annual())
    df <- pdata_annual()

    p <- df %>%
      subset(!is.na(c_newinc)) %>%
      ggplot(aes(x = year, y = c_newinc, group=1)) +
      geom_line(color = "dodgerblue3", size = 1
      ) +
      geom_point(size = 5, color = "dodgerblue3") +
      geom_point(size = 3, color = "white") +
      facet_wrap(~ country,strip.position="top",ncol=3, scales="free")+
      xlab("Year") +
      ylab(NULL)+ scale_y_continuous(labels = function(x) format(x, big.mark = " ",
                                                                 scientific = FALSE),
                                     limits = function(x){c(0, max(0.1, x))})

    p <- p +
      theme(strip.text.y.left = element_text(hjust=1,vjust = 1,angle=0,face="bold"),
            strip.placement = "outside",strip.background = element_blank(),
            panel.grid.major.x  = element_blank(),
            panel.grid.major.y  = element_line(colour = "grey80"),
            panel.grid.minor.y  = element_line(colour = "grey80"),
            axis.line.x = element_line(colour = "black", size = 1),
            text = element_text(size = 20),
            panel.background = element_rect(fill = "white"),
            panel.spacing = unit(3, "lines")
      )

    p

  }
})

# Plot figure for multiple countries: reactive -> renderPlot
# ----------------------------------------------------------
output$multiple_plot <- renderPlot({
  multi_plot()
})

output$multiple_plot.ui <- renderUI({
  plotOutput("multiple_plot", height = pdata_count()*400)
})


# Plot figure for multiple countries: reactive -> downloadHandler
#----------------------------------------------------------
multi_plot_pdf <- reactive({
  p <- multi_plot()

    if(input$indicator == "provisional") {

      p <- p + facet_wrap(~ country,strip.position="top",ncol=5, scales="free")+
        ggtitle("Provisional* number of people with new or relapse episodes of TB notified per month or quarter since January 2020") +
        labs(caption =   paste0("* "," Data are provisional as reported to WHO by ",
                                format(Sys.time(),
                                       format = "%Y-%m-%d %H:%M",
                                       tz = "GMT"),
                                " UTC and subject to change.",
                                " Monthly/quarterly totals for a given year may differ from the final and official annual total subsequently reported to WHO.")) +
        theme(
          plot.title = element_text(color="limegreen", size=25, face="bold"),
          plot.caption = element_text(hjust = 0),
          panel.spacing = unit(5, "lines"),
          plot.margin = unit(c(1,1,1,1), "cm"))

    } else {

      p <- p + facet_wrap(~ country,strip.position="top",ncol=5, scales="free")+
        ggtitle("Number of people with new or relapse episodes of TB notified per year, 2016-2020") +
        theme(
          plot.title = element_text(color="dodgerblue3", size=25, face="bold"),
          panel.spacing = unit(5, "lines"),
          plot.margin = unit(c(1,1,1,1), "cm"))

    }

})

output$download_plot <- downloadHandler(

  filename = function() {
    paste0("multi_plot_",Sys.Date(),".pdf") },

  content = function(file) {
    if(input$country_set == "3gwc") {
      ggsave(file, plot = multi_plot_pdf(), device = "pdf",width=25,height=pdata_count()*6, title="TB notifications")

    } else {
      ggsave(file, plot = multi_plot_pdf(), device = "pdf",width=45,height=pdata_count()*4.5, title="TB notifications")

      }

    }
)

output$page_footer2 <- renderText({
  # Make sure there are data to plot
  paste0("* "," Data are provisional as reported to WHO by ",
         format(Sys.time(),
                format = "%Y-%m-%d %H:%M",
                tz = "GMT"),
         " UTC and subject to change.",
         " Monthly/quarterly totals for a given year may differ from the final and
              official annual total subsequently reported to WHO.")

})

output$page_footer3 <- renderText({
  if(input$indicator == "provisional" & input$country_set == "30hbc") {

  paste0("** No data available for Congo for provisional number of people with new or relapse episodes of TB notified per month or quarter.")
  } else {
    paste0("")

  }

})
