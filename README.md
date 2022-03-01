# Provisional notifcation of tuberculosis cases by month or quarter
Show as timeseries charts the provisional numbers of new and relapse cases reported to WHO by high TB burden and other regional priority countries. 

## Components and data source

This is an app built using [Shiny](https://shiny.rstudio.com/) and hosted at [shinyapps.io](https://worldhealthorg.shinyapps.io/tb_pronto/). It uses the [echarts4r package](https://echarts4r.john-coene.com/) to show charts for single countries with mouseover pop-ups of data published by the World Health Organization's [Global Tuberculosis Programme](https://www.who.int/teams/global-tuberculosis-programme/data). It uses the [ggplot2 package](https://cran.r-project.org/web/packages/ggplot2/index.html) to show charts for multiple countries. 

The app pulls data directly from the global TB database. Unfortunately there is no standard api with which to interogate the database, so I built a script and some queries to return JSON files specifically for this app.

The script can return three types of JSON file:

1. https://extranet.who.int/tme/generateJSON.asp?ds=c_newinc_countries. This returns a list of all countries which have provisionally reported at least one case in the final reporting period of the most recent year. Only high TB burden countries and regional priority countries have been asked by WHO to report provisional TB notification data.

2. https://extranet.who.int/tme/generateJSON.asp?ds=c_newinc&iso2=XX (where XX is a country [ISO2 code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)). This returns all the data that appear in the charts for the chosen country.

3. https://extranet.who.int/tme/generateJSON.asp?ds=c_newinc_group&group_code=XXX (where XXX is a code to identify a group of countries). This returns all the data that appear in the charts for the chosen group of countries. There are three group codes available:

    a. _g_hb_tb_ for the 30 high TB burden countries
    b. _g_shortfall_2020_ for the 30 countries with the biggest contributions to the global shortfall in TB notifications in 2020 vs 2019
    c. _g_watchlist_ for 3 global TB watchlist countries

See Annex 3 of the [2021 Global Tuberculosis Report](https://www.who.int/publications/i/item/9789240037021) for the definition and composition of the 30 high TB burden countries and the global watchlist.

## Data updates

WHO collects TB data annually from all countries and areas and publishes them in the  [Global Tuberculosis Report](https://www.who.int/teams/global-tuberculosis-programme/data), usually in October of each year. In 2020 WHO also started to collect provisional monthly or quarterly TB notifications from high TB burden and other regional priority countries in between annual data collection rounds.



