#
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(rworldmap)
library(ggplot2)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(hrbrthemes)


covid_data <- read.csv("owid-covid-data.csv")
death_rate_data <- read.csv("df.csv")
covid_data2 <- read.csv("owid-covid-data2.csv")


####################################################
country_data <- covid_data %>%
    filter(location!="World") %>%
    select(ISO3 = iso_code, location, total_cases, total_deaths) %>%
    group_by(ISO3) %>%
    summarize(country = unique(location),
              total_cases = max(total_cases, na.rm=TRUE),
              total_deaths = max(total_deaths, na.rm=TRUE)) 

country_data <- country_data %>%
    arrange(desc(-total_cases)) %>%
    mutate(rank = 1:nrow(country_data))

country_death_data <- death_rate_data %>%
    select(ISO3 = iso_code,location, death_rate)

country_death_data <- country_death_data %>%
    arrange(desc(-death_rate)) %>%
    mutate(rank = 1:nrow(country_death_data))
################################################

###################COVID World map##################
mapped_data <- joinCountryData2Map(country_data, joinCode = "ISO3", 
                                   nameJoinColumn = "ISO3")

mapped_death_data <- joinCountryData2Map(country_death_data, joinCode = "ISO3", 
                                         nameJoinColumn = "ISO3")

limits <- range(country_data$rank)
legend.limits <- as.integer(seq(min(limits), max(limits), len = 8) )

#####################################################

############
## COVID map By Total Cases##
#set map Resolutions
worldmap <- getMap(resolution = "coarse")

# load data
world <- ne_countries(scale = "medium", returnclass = "sf")

# RECODE NAMES TO MATCH WORLDmap
as.factor(country_data$country) %>% levels()

country_data$country <- recode(country_data$country
                               ,'United States' = 'USA'
                               ,'United Kingdom' = 'UK')

as.factor(country_death_data$location) %>% levels()

country_death_data$location <- recode(country_death_data$location
                                      ,'United States' = 'USA'
                                      ,'United Kingdom' = 'UK')
# GET WORLD map
map.world <- map_data("world")

# join the 'country_data'  
map.world_joined <- left_join(map.world, country_data, by = c('region' = 'country'))
map_death.world_joined <- left_join(map.world, country_death_data, by = c('region' = 'location'))

############################################################

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19 Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("COVID-19 Cases Worldwide", tabName = "chart1"),
            menuItem("COVID-19 Fatality Rate ", tabName = "chart2"),
            menuItem("COVID-19 Monthly Analysis", tabName = "chart3")
            
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            # Second tab content
            ######################################Kritiha#####################################
            tabItem(
                tabName = "chart1", 
                h2("COVID-19 Cases Worldwide"),
                tags$head(tags$style(HTML('.small-box .icon-large {top: 5px;}'))),
                fluidRow(
                    tags$style(".small-box.bg-yellow { background-color: #FFFF00 !important; color: #000000 !important; }"),
                    (column (width = 12,
                             fluidRow(
                                 valueBoxOutput("vbox1", width = 4),
                                 valueBoxOutput("vbox2", width = 4),
                                 valueBoxOutput("vbox3", width = 4)))),
                    
                    sidebarLayout(
                        sidebarPanel(
                            selectInput(
                                "var",
                                label = "Choose number of cases",
                                choices = c(
                                    "-",
                                    "10,000,000+ cases", 
                                    "1,000,000 - 9,999,999",
                                    "100,000 - 999,999", 
                                    "10,000 - 99,999",
                                    "1,000 - 9,999", 
                                    "100 - 999", 
                                    "1-99", 
                                    "0"),
                                selected = "10,000,000+ cases")
                        ),#end of sidebarPanel
                        # Show a plot of the generated distribution
                        mainPanel(
                        
                            plotOutput(outputId = "map", width = "100%")
                        ) #end of mainPanel
                    ) #end of sidebarLayout
                )#end of fluidRow
            ), #end of tabItem chart1,
            
            ########################################Firas##########################################
            tabItem(
                tabName = "chart2", 
                h2("COVID-19 Map on Fatality Rate"),
                fluidRow(
                    sidebarLayout(
                        sidebarPanel(
                            selectInput(
                                "death_rate",
                                label = "Choose Fatality Rate",
                                choices = c(
                                    "-",
                                    "5%++", 
                                    "3-4.9%",
                                    "2-2.9%",
                                    "--1.9%"
                                ),
                                selected = "--1.9%")
                        ),#end of sidebarPanel
                        # Show a plot of the generated distribution
                        mainPanel(
                            plotOutput(outputId = "map_death", width = "100%")
                        ) #end of mainPanel
                    ) #end of sidebarLayout
                )#end of fluidRow
            ),
            
            #################################fatin & Illani's ##########################################
            tabItem(
                tabName = "chart3", 
                
                h2("COVID-19 Cases Monthly Analysis"),
                h4("January 2020 - December 2020"),
                
                
                fluidRow(
                    
                    
                    sidebarLayout(
                        
                        sidebarPanel(
                            
                            selectizeInput("stateInput", "Choose Location",
                                           choices = unique(covid_data2$location),  
                                           selected="Malaysia", multiple =FALSE)
                        ),  
                        
                        mainPanel(
                            # outputs
                            tabsetPanel(
                                tabPanel("Covid 19 Cases", plotOutput("overplot"),options(scipen=999)),
                                tabPanel("Covid 19 Death Cases", plotOutput("deathplot")),
                                tabPanel("Covid 19 Tests Taken", plotOutput("testplot"))
                            )
                        )#end of mainpanel  
                    ) #end of sidebarLayout
                )#end of fluidRow
            )#end of tabitem
            ################################################################################# 
        )
        
    )
)





server <- function(input, output) {
    
    output$map <- renderPlot({
        if(input$var != "-"){
            selected_min <- switch(
                input$var, 
                "10,000,000+ cases" = 10000000,
                "1,000,000 - 9,999,999" = 1000000,
                "100,000 - 999,999" = 100000,
                "10,000 - 99,999" = 10000,
                "1,000 - 9,999" = 1000,
                "100 - 999" = 100,
                "1-99" = 10,
                "0" = 0
            )
            selected_max <- switch(
                input$var, 
                "10,000,000+ cases" = 19999999,
                "1,000,000 - 9,999,999" = 9999999,
                "100,000 - 999,999" = 999999,
                "10,000 - 99,999" = 99999,
                "1,000 - 9,999" = 9999,
                "100 - 999" = 999,
                "1-99" = 99,
                "0" = 0
            )
            # Add flag variable to indicate which countries to highlight
            map.world_joined <- map.world_joined %>% 
                mutate(fill_flg = ifelse((total_cases %in% selected_min:selected_max),T,F))
            # #=======
            # # PLOT map
            # #=======
            p<-  ggplot() +
                geom_polygon(
                    data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
                scale_fill_manual(values = c("#CCCCCC","#e60000")) +
                labs(
                    title = paste("Countries with ",input$var," COVID cases"),subtitle = " "
                ) +
                theme(
                    text = element_text(family = "Gill Sans", color = "#FFFFFF")
                    ,panel.background = element_rect(fill = "#444444")
                    ,plot.background = element_rect(fill = "#444444")
                    ,panel.grid = element_blank()
                    ,plot.title = element_text(size = 30)
                    ,plot.subtitle = element_text(size = 10)
                    ,axis.text = element_blank()
                    ,axis.title = element_blank()
                    ,axis.ticks = element_blank()
                    ,legend.position = "none"
                )
            print(p)  
            #remove flag column for resetting
            map.world_joined <- map.world_joined %>% select(-fill_flg)
        }
    }, height=500, units="px")

    ##### Map death rate
    output$map_death <- renderPlot({
        if(input$death_rate != "-"){
            selected_death_min <- switch(
                input$death_rate,
                "5%++" = 5, 
                "3-4.9%" = 3,
                "2-2.9%" = 2,
                "--1.9%" = 0
            )
            selected_death_max <- switch(
                input$death_rate,
                "5%++" = 100, 
                "3-4.9%" = 4.9,
                "2-2.9%" = 2.9,
                "--1.9%" = 1.9,
            )
            # Add flag variable to indicate which countries to highlight
            map_death.world_joined <- map_death.world_joined %>% 
                mutate(fill_flg = ifelse((selected_death_min<death_rate & selected_death_max > death_rate),T,F))
            # #=======
            # # PLOT map
            # #=======
            p<-  ggplot() +
                geom_polygon(
                    data = map_death.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
                scale_fill_manual(values = c("#CCCCCC","#e60000")) +
                labs(
                    title = paste("Countries with ",input$death_rate," COVID cases"),subtitle = " "
                ) +
                theme(
                    text = element_text(family = "Gill Sans", color = "#FFFFFF")
                    ,panel.background = element_rect(fill = "#444444")
                    ,plot.background = element_rect(fill = "#444444")
                    ,panel.grid = element_blank()
                    ,plot.title = element_text(size = 30)
                    ,plot.subtitle = element_text(size = 10)
                    ,axis.text = element_blank()
                    ,axis.title = element_blank()
                    ,axis.ticks = element_blank()
                    ,legend.position = "none"
                )
            print(p)  
            #remove flag column for resetting
            map_death.world_joined <- map_death.world_joined %>% select(-fill_flg)
        }
    }, height=500, units="px")
    
    
    ############Fatin & Illani's###################
    
    
    d <- reactive({
        filtered <-
            covid_data2 %>%
            filter(location == input$stateInput)    
        
    }) 
    
    output$overplot <- renderPlot({
        
        ggplot(d(),aes(x= month, y=total_cases, color=location)) +
            ggtitle("COVID-19 Cases") +
            geom_line( color="orange", size=3, alpha=0.9, linetype=1) + 
            theme_ipsum() +
            xlab("Month") +
            ylab("Number of cases")
    })
    
    output$deathplot <- renderPlot({
        
        ggplot(d(),aes(x= month, y=total_deaths, color=location)) +
            ggtitle("COVID-19 Deaths Cases") +
            geom_line( color="red", size=3, alpha=0.9, linetype=1) + 
            theme_ipsum() +
            xlab("Month") +
            ylab("Number of cases")
    })
    
    output$testplot <- renderPlot({
        ggplot(d(), aes(x=month, y=new_tests)) +
            ggtitle("Total Tests Cases") +
            geom_bar(stat="identity", fill="blue") +
            xlab("Month")+
            ylab("Total Tests Taken")
    })
    
    aggregated <- reactive({
        filtered <- 
            covid_data2 %>%
            group_by_(input$stateSelect) %>%
            sum(covid_data2$new_cases)
    })
    
    output$vbox1 <- renderValueBox({
        a <- "95 Million"
        valueBox(h4("Total Cases"),h4(a),icon = icon("fas fa-hospital-user"), color= "orange")
    })
    
    output$vbox2 <- renderValueBox({
        e <- "2 Million"
        valueBox(h4("Total Deaths"),h4(e),icon = icon("far fa-user"), color ="red")
    })
    
    output$vbox3 <- renderValueBox({
        f <- "500 Million"
        valueBox(h4("Total Tests Taken"),h4(f), icon = icon("fas fa-vial"),color = "blue")
    })
    
}


# Run the application
shinyApp(ui, server)

