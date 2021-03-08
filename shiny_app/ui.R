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

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 Worldwide</a>'), id="nav",
             windowTitle = "COVID-19 Worldwide",
             
             tabPanel("COVID-19 mapper",
                      icon = icon("map-marked-alt"),
                      div(class="outer",
                          tags$head(includeCSS("www/styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 100, left = 100, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        h4(textOutput("reactive_death_count"), align = "right"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderTextInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        choices = format(unique(df_countries$dates), "%d %b %y"),
                                                        selected = format(current_date, "%d %b %y"),
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 3000, loop = FALSE))
                                        
                          )
                      )
             ),
             
             tabPanel("Analysis Plots",icon = icon("chart-line"),
                      
                      div(class = "container-fluid",
                          fluidRow(column(width=2,offset = 0.8,
                                          align="center",
                                          div(class="container",
                                              h3(br()),
                                              h3("Updated on",
                                                 br(),br(),
                                                 textOutput("date"))),
                                          br(),
                                          h3(textOutput("total_case"),
                                             style = "font-family: Impact;
                                                  font-size:40px ;
                                                  line-height:50%;"),
                                          h3("Cases",style = "line-height: 60%"),
                                          p("_________",
                                            style = "line-height: 0;
                                        font-size=10px;"),
                                          br(),
                                          column(width=1,align="right",
                                                 h4(textOutput("incidence_rate"),
                                                    style = "font-family: Impact;
                                                  font-size:30px ;
                                                  line-height:50%;"),
                                                 h4("Incidence Rate",style = "line-height: 100%"),
                                                 p("_________",
                                                   style = "line-height: 0;
                                                font-size=10px;")),
                                          column(width=1,align="center",
                                                 h4(textOutput("death_rate"),
                                                    style = "font-family: Impact;
                                                  font-size:30px ;
                                                  line-height:50%;"),
                                                 h4("Death Rate",style = "line-height: 100%"),
                                                 p("_________",
                                                   style = "line-height: 0;
                                        font-size=10px;"),offset=4),
                                          br(),br(),br(),br(),br(),br(),
                                          fluidRow(p("The Dashboard above showing the most up-to-date information around the world about the Covid-19.")))
                                   ,
                                   
                                   column(width = 10, offset = 0.8,
                                          wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 100px;",
                                                    width = "100%",
                                                    column(6,
                                                           span(tags$i(h6("Please select the countries that you care about and would like to compare among. Hover the mouse on the plotted line, you are able to see more detailed information of the time and case numbers.")), style="color:#045a8d"),
                                                    ),
                                                    column(6,offset = 0.5,
                                                           pickerInput("selected_country", "Country:",
                                                                       choices = as.character(df_countries[df_countries$dates == current_date,][order(-df_countries[df_countries$dates == current_date,]$confirmed_cases),]$country),
                                                                       options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                                                       selected = as.character(df_countries[df_countries$dates == current_date,][order(-df_countries[df_countries$dates == current_date,]$confirmed_cases),]$country)[1:10],
                                                                       multiple = TRUE) )
                                          ),
                                          mainPanel(width = 12,
                                                    tabsetPanel(
                                                      tabPanel("Confirmed Cases", plotlyOutput("country_cases_plot_ly")),
                                                      tabPanel("Death Cases", plotlyOutput("country_death_plot_ly"))
                                                    )
                                          )
                                   )
                          )
                          
                      )
                      
             )
             ,
             tabPanel("About",icon = icon("book-open"),
                      fluidPage(
                        
                        mainPanel( width=12,
                                   h1(strong("What you'll find here"), align = "center"),
                                   HTML("<hr width=400>"),
                                   p("The Coronavirus Disease 2019 (COVID-19) brings not only pain and death, but also panic to the public. 
                                     It is common that people are worried about the Covid-19. This app is built for the public to stay informed 
                                     with the most up-to-date worldwide disease spreading conditions. The information might help you with a better 
                                     decision to travel plans. Comparing different countries, you will be able to gain an overview of how the 
                                     disease is controlled in different countries, which might help you generate your political ideas. The 
                                     animation moving date by date could give you a dynamic view of how fast the disease is spreading.",
                                     align="center",
                                     style="margin-left:80px;
                                              margin-right:80px"),
                                   h4(strong("HOPE EVERYONE STAY HEALTHY!"),align="center"),
                                   h4(strong("HOPE THINGS WILL GET BACK TO NORMAL SOON!"),align="center")),
                        br(),
                        mainPanel( width=12,
                                   h1(strong("About Dataset"),align="center"),
                                   HTML("<hr width=300>"),
                                   p("The worldwide Covid-19 dataset I used is from ",
                                     a("Covid19 public API",href="https://github.com/M-Media-Group/Covid-19-API"),
                                     ". With the help of ",
                                     a("Worldwide Geojson Data",href="https://github.com/AshKyd/geojson-regions"),
                                     ", I am able to plot the map in the app. Since plotting the dataset on map requires geometric features, I used ",
                                     a("Country mapping coordinates",href="https://gist.github.com/tadast/8827699"),
                                     "for help.",align="center",
                                     style="margin-left:80px;
                                              margin-right:80px"),
                                   p("Credict goes to the contributors of these public dataset",align="center",
                                     style="margin-left:80px;
                                              margin-right:80px"),
                                   p("The Covid19 public dataset contains historical data of the confirmed, death, recovered case numbers of the Coronavirus Disease 2019 (COVID-19) from all over the world. 
                                   Data are updated daily. Our app is consistant to the dataset.",align="center",
                                     style="margin-left:80px;
                                              margin-right:80px"),
                                   ),
                        mainPanel( width=12,
                                   h1(strong("Contributor"),align="center"),
                                   HTML("<hr width=200>"),
                                   p("Xiya Ma, fourth year student at UC Davis, majored in statistics and accounting.",align="center",
                                     style="margin-left:80px;
                                              margin-right:80px"),
                                   p("To find more details for the app, please see ",
                                     a("the GitHub repository of the Shiny App",href="https://github.com/xiyma2021/xiyama-sta141bfinalproject"),align = "center",
                                     style="margin-left:80px;
                                              margin-right:80px"),
                                   p("If you have any questions, please feel free to reach out to me. Thank you!",align = "center",
                                     style="margin-left:80px;
                                              margin-right:80px"),
                                   )
                                     
                                   
                          
                        )
                      ))
  )

