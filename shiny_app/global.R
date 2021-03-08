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

# Use API to get Covid-19 data --- cases
res_cases = GET("https://covid-api.mmediagroup.fr/v1/cases")
data = fromJSON(rawToChar(res_cases$content))
data <- data[c(1:178,192,179:191,193)]
country_names<-names(data)

# Use API to get Covid-19 data --- historical confirmed data
res_confirmed_his = GET("https://covid-api.mmediagroup.fr/v1/history?status=confirmed")
data_confirmed_his = fromJSON(rawToChar(res_confirmed_his$content))

# Use API to get Covid-19 data --- historical death data
res_death_his = GET("https://covid-api.mmediagroup.fr/v1/history?status=deaths")
data_death_his = fromJSON(rawToChar(res_death_his$content))

# Use API to get Covid-19 data --- historical recovered data
res_recovered_his = GET("https://covid-api.mmediagroup.fr/v1/history?status=recovered")
data_recovered_his = fromJSON(rawToChar(res_recovered_his$content))

# Get Global data
Global_confirmed_his<-as.data.frame(t(t((data_confirmed_his$Global$All$dates))))
colnames(Global_confirmed_his) <- c('confirmed_cases')
Global_confirmed_his <- tibble::rownames_to_column(Global_confirmed_his, "dates")

Global_death_his<-as.data.frame(t(t((data_death_his$Global$All$dates))))
colnames(Global_death_his) <- c('death_cases')
Global_death_his <- tibble::rownames_to_column(Global_death_his, "dates")

Global_recovered_his<-as.data.frame(t(t((data_recovered_his$Global$All$dates))))
colnames(Global_recovered_his) <- c('recovered_cases')
Global_recovered_his <- tibble::rownames_to_column(Global_recovered_his, "dates")

global_combined <- Global_confirmed_his %>% 
  left_join(Global_death_his, by = 'dates') %>%
  left_join(Global_recovered_his, by = 'dates')
global_combined['country'] <- 'Global'
global_combined = global_combined[order(global_combined$dates),]

global_combined$confirmed_cases = as.numeric(global_combined$confirmed_cases)
global_combined$death_cases = as.numeric(global_combined$death_cases)
# global_combined$recovered_cases = as.numeric(global_combined$recovered_cases)
global_combined$dates = as.Date(global_combined$dates)

Global_population <- data$Global$All$population
Global_confirmed <- data$Global$All$confirmed
Global_recovered <- data$Global$All$recovered
Global_deaths <- data$Global$All$deaths

# Get two other public data downloaded online, prepare for the map
countries = read.csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("all.geojson", what = "sp")

# Convert Other data from json into dataframe
df <- data.frame(matrix(ncol = 9, nrow = length(country_names)))
colnames(df) <- c("country_names","confirmed","recovered","deaths","population","life_expectancy","sq_km_area","continent","abbreviation")
total_his_data <- data.frame(matrix(ncol = 5))
colnames(total_his_data) <- c('dates','confirmed_cases','death_cases', 'recovered_cases','country')

for (i in 1:length(country_names))
{
  df[i,'country_names'] = country_names[i]
  df[i,'confirmed'] = data[[i]]$All$confirmed
  df[i,'recovered'] = data[[i]]$All$recovered
  df[i,'deaths'] = data[[i]]$All$deaths
  df[i,'population'] = ifelse(!is.null(data[[i]]$All$population), data[[i]]$All$population, 0)
  df[i,'life_expectancy'] = ifelse(!is.null(data[[i]]$All$life_expectancy), data[[i]]$All$life_expectancy, 0)
  df[i,'sq_km_area'] = ifelse(!is.null(data[[i]]$All$sq_km_area), data[[i]]$All$sq_km_area, 0)
  df[i,'continent'] = ifelse(!is.null(data[[i]]$All$continent), data[[i]]$All$continent, 0)
  df[i,'abbreviation'] = ifelse(!is.null(data[[i]]$All$abbreviation), data[[i]]$All$abbreviation, 0)
  
  temp_confirmed <- as.data.frame(t(t((data_confirmed_his[[i]]$All$dates))))
  colnames(temp_confirmed) <- c('confirmed_cases')
  temp_confirmed <- tibble::rownames_to_column(temp_confirmed, "dates")
  
  temp_deaths <- as.data.frame(t(t((data_death_his[[i]]$All$dates))))
  colnames(temp_deaths) <- c('death_cases')
  temp_deaths <- tibble::rownames_to_column(temp_deaths, "dates")
  
  temp_recovered <- as.data.frame(t(t((data_recovered_his[[i]]$All$dates))))
  colnames(temp_recovered) <- c('recovered_cases')
  temp_recovered <- tibble::rownames_to_column(temp_recovered, "dates")
  
  temp_combined <- temp_confirmed %>% 
    left_join(temp_deaths, by = 'dates') %>%
    left_join(temp_recovered, by = 'dates')
  temp_combined['country'] <- country_names[i]
  
  total_his_data <- rbind(temp_combined,
                          total_his_data)
}

total_data <- total_his_data %>% left_join(df,by = c('country' = 'country_names'))
total_data = total_data[order(total_data$dates),]

# Manipulate some data to be consistant to the geojson for map
total_data$country[total_data$country=="Cabo Verde"] = "Cape Verde"
total_data$country[total_data$country=="Congo (Brazzaville)"] = "Congo"
total_data$country[total_data$country=="Congo (Kinshasa)"] = "Congo, the Democratic Republic of the"
total_data$country[total_data$country=="Cote d'Ivoire"] = "Côte d'Ivoire"
total_data$country[total_data$country=="Czechia"] = "Czech Republic"
total_data$country[total_data$country=="Eswatini"] = "Swaziland"
total_data$country[total_data$country=="Holy See"] = "Holy See (Vatican City State)"
total_data$country[total_data$country=="Iran"] = "Iran, Islamic Republic of"
total_data$country[total_data$country=="Korea, South"] = "South Korea"
total_data$country[total_data$country=="Kosovo"] = "Kosovo"
total_data$country[total_data$country=="Laos"] = "Lao People's Democratic Republic"
total_data$country[total_data$country=="Micronesia"] = "Micronesia, Federated States of"
total_data$country[total_data$country=="Moldova"] = "Moldova, Republic of"
total_data$country[total_data$country=="North Macedonia"] = "Macedonia, the former Yugoslav Republic of"
total_data$country[total_data$country=="Syria"] = "Syrian Arab Republic"
total_data$country[total_data$country=="Taiwan*"] = "Taiwan, Province of China"
total_data$country[total_data$country=="Tanzania"] = "Tanzania, United Republic of"
total_data$country[total_data$country=="US"] = "United States"
total_data <- total_data[total_data$country!="Global"&total_data$country!="Diamond Princess"&total_data$country!="MS Zaandam"&total_data$country!="West Bank and Gaza"&total_data$country!="Kosovo",]
total_data <- total_data[!is.na(total_data$country),]

# check if the two dataset are consistent now
if (all(total_data$country %in% countries$Country)==FALSE) { print("Error: inconsistent country names")}

# combine dataset with country alpha 3 code
df_countries <- total_data %>%
  inner_join(countries[c('Country','Alpha.3.code','Latitude..average.', 'Longitude..average.')], by =  c("country" = "Country"),copy = TRUE)
df_countries$Alpha.3.code<-gsub(" ","",as.character(df_countries$Alpha.3.code))

# correct one error in the geojson data about South Sudan's alpha 3 code
worldcountry$adm0_a3[worldcountry$name_long == df_countries$country[!(df_countries$Alpha.3.code %in% worldcountry$adm0_a3)][1]] <- "SSD"
if (all(df_countries$Alpha.3.code %in% worldcountry$adm0_a3)==FALSE) { print("Error: inconsistent alpha 3 code")}

# formatting data
df_countries$confirmed_cases = as.numeric(df_countries$confirmed_cases)
df_countries$death_cases = as.numeric(df_countries$death_cases)
# df_countries$recovered_cases = as.numeric(df_countries$recovered_cases)
df_countries$dates = as.Date(df_countries$dates)
cv_min_date = as.Date(min(df_countries$dates),"%Y-%m-%d")
current_date = as.Date(max(df_countries$dates),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

###### data preprocessing done


###### Plots preparation starts

# assign colours to ensure consistency between plots 
covid_col = "#cc4c02"
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(df_countries$country)), as.character(unique(df_countries$continent)), as.character(unique(df_countries$state)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names

# plot preprocessed
plot_map <- worldcountry[worldcountry$adm0_a3 %in% df_countries$Alpha.3.code, ]
bins = c(0,100000,500000,1000000,5000000,10000000,Inf)
cv_pal <- colorBin("Oranges", domain = df$confirmed, bins = bins)

# world map
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("COVID-19 (accumulated)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  # hideGroup(c("COVID-19 (cumulative)")) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~df$confirmed,
            title = "<small>confimed cases</small>") 

# Plots functions
cumulative_plot = function(plot_data, plot_date) {
  plot_df = subset(plot_data, dates<=plot_date)
  g1 = ggplot(plot_df, aes(x = dates, y = confirmed_cases)) + geom_line(color=covid_col) + geom_point(size = 1, alpha = 0.8, color=covid_col) +
    ylab("Cumulative cases") +  xlab("Date") + theme_bw() +
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10),
          plot.margin = margin(5, 12, 5, 5))
  g1
}

country_cases_plot = function(plot_data) {
  g = ggplot(plot_data, aes(x = dates, y = confirmed_cases, colour = country, group = 1,
                            text = paste0(format(dates, "%d %B %Y"), "\n", country, ": ",confirmed_cases)
  )) +
    xlim(c(cv_min_date,(current_date+1))) + xlab("Date")+ 
    geom_point(size = 0.3, alpha = 0.8) +
    ylab("Cumulative") + theme_bw() +
    scale_colour_manual(values=country_cols) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

country_death_plot = function(plot_data) {
  g = ggplot(plot_data, aes(x = dates, y = death_cases, colour = country, group = 1,
                            text = paste0(format(dates, "%d %B %Y"), "\n", country, ": ",confirmed_cases)
  )) +
    xlim(c(cv_min_date,(current_date+1))) + xlab("Date")+ 
    geom_point(size = 0.3, alpha = 0.8) +
    ylab("Cumulative") + theme_bw() +
    scale_colour_manual(values=country_cols) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}