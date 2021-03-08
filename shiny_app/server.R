# Load all the required libraries 
packages.used <- c("httr", "jsonlite", "dplyr", "ggplot2","RColorBrewer",
                   "leaflet", "shinyWidgets","plotly","shinythemes","geojsonio")

# check packages that need to be installed.
packages.needed <- setdiff(packages.used, 
                           intersect(installed.packages()[,1], 
                                     packages.used))
# install additional packages
if(length(packages.needed) > 0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(geojsonio)

server = function(input, output, session) {
  
  # reactive
  formatted_date = reactive({
    format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(formatted_date()),"%d %B %Y")
  })
  
  reactive_db = reactive({
    df_countries %>% filter(dates == formatted_date())
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(Alpha.3.code %in% worldcountry$adm0_a3)
    worldcountry_subset = worldcountry[worldcountry$adm0_a3 %in% large_countries$Alpha.3.code, ]
    large_countries = large_countries[match(worldcountry_subset$adm0_a3, large_countries$Alpha.3.code),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$adm0_a3 %in% reactive_db_large()$Alpha.3.code, ]
  })
  
  reactive_db_global = reactive({
    global_combined %>% filter(dates == formatted_date())
  })
  
  country_reactive_db = reactive({
    df_countries %>% filter(country %in% input$selected_country)
  })
  
  # Texts
  output$reactive_case_count <- renderText({
    paste0(prettyNum(reactive_db_global()$confirmed_cases, big.mark=","), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(reactive_db_global()$death_cases, big.mark=","), " deaths")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), confirmed_cases!=0)), " countries/regions affected")
  })
  
  output$date <- renderText({
    format(current_date, "%b %d")
  })
  
  output$total_case <- renderText({
    prettyNum(reactive_db_global()$confirmed_cases, big.mark=",")
  })
  
  output$death_rate <- renderText({
    paste(round(as.numeric(Global_deaths)/as.numeric(Global_confirmed)*100, 1), "%", sep="")
  })
  
  output$incidence_rate <- renderText({
    paste(round(as.numeric(Global_confirmed)/as.numeric(Global_population)*100, 1), "%", sep="")
  })
  
  # maps
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearShapes() %>%

      addCircleMarkers(data = reactive_db(), lat = ~ Latitude..average., lng = ~ Longitude..average., weight = 1,
                       radius = ~(confirmed_cases)^(1/5.5),
                       fillOpacity = 0.1, color = covid_col, group = "COVID-19 (accumulated)",
                       label = sprintf("<strong>%s </strong><br/>Confirmed cases: %%1.f <br/>Deaths: %%1.f ", reactive_db()$country, reactive_db()$confirmed_cases, reactive_db()$death_cases) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                       textsize = "15px", direction = "auto")
    ) %>%

      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$confirmed_cases))
  })
  
  # plots
  output$cumulative_plot <- renderPlot({
    cumulative_plot(global_combined, formatted_date())
  })
  
  output$country_cases_plot_ly <- renderPlotly({
    country_cases_plot(country_reactive_db())
  })
  
  output$country_death_plot_ly <- renderPlotly({
    country_death_plot(country_reactive_db())
  })
  
}