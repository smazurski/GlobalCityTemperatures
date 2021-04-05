library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(plotly)
library(caret)
library(rpart)
library(rpart.plot)
library(DMwR)
library(hrbrthemes)
library(patchwork)
library(scales)
library(viridis)
library(shiny)
library(shinythemes)
library(leaflet)


#global_temps_orig <- read_csv("C:/Users/steve/Documents/Global_Temps/Global_Temps.csv")

#global_temps <- gather(global_temps_orig,"Month","Temp", -Continent, -Country, -City, -Year)

#avg_global_temps_city <- global_temps %>%
#    group_by(Continent, Country, City) %>%
#    summarise(AvgTemp=mean(Year))


#choices <- print(avg_global_temps_city$City)


temps_min_max_avg <- read_csv("C:/USers/steve/Documents/Global_Temps/city_temps.csv")

temps_min_max_avg_map <- temps_min_max_avg %>%
  group_by(City) %>%
  summarise(Average=mean(Avg), Lat=mean(Lat), Long=mean(Long), Population=mean(Population))


temps_min_max_avg_map <- temps_min_max_avg_map %>%
  filter(Average>25)

temp_choices <- print(temps_min_max_avg$City)



# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("Global City Temperatures",

    theme = shinytheme("flatly"),
    # Sidebar with a slider input for number of bins 
        # Show a plot of the generated distribution
    
        tabPanel("Compare",
                 titlePanel("Global Metropolitan Temperatures"),
                 h4("Select Up to 5 Cities to Compare Seasonal Tempature Changes"),
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput('city', 'Select Cities', choices = temp_choices, multiple = TRUE)) ,
                     mainPanel(
                         br(),
                         br(),
                         plotlyOutput("distPlot")))),
        tabPanel("Range", 
                 h4("View Seasonal Tempature Ranges"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput('city_temp', 'Select City', choices = temp_choices, multiple = FALSE)) ,
                     mainPanel(
                         br(),
                         br(),
                         plotlyOutput("distPlot2")))),
        tabPanel("Map",
                 mainPanel(
                   leafletOutput("distPlot3")))))

# Define server logic required to draw a histogram
server <- function(input, output) {

        
        output$distPlot <- renderPlotly({
            
            
            if (length(input$city)==5)
            {
            
            city1 <- unique(input$city[1])
            city2 <- unique(input$city[2])
            city3 <- unique(input$city[3])
            city4 <- unique(input$city[4])
            city5 <- unique(input$city[5])
            
            City1_Temps <- temps_min_max_avg %>%
                filter(City==city1)
            
            City2_Temps <- temps_min_max_avg %>%
                filter(City==city2)
            
            City3_Temps <- temps_min_max_avg %>%
                filter(City==city3)
            
            City4_Temps <- temps_min_max_avg %>%
                filter(City==city4)
            
            City5_Temps <- temps_min_max_avg %>%
                filter(City==city5)
            
            
#            City1_Temps$Month <- factor(City1_Temps$Month, levels = City1_Temps[["Month"]])
            
            City1_Temps$city2 <- City2_Temps$Avg
            City1_Temps$city3 <- City3_Temps$Avg
            City1_Temps$city4 <- City4_Temps$Avg
            City1_Temps$city5 <- City5_Temps$Avg
            
            
            plot_ly(City1_Temps, x = ~Month, y = ~Avg, name = city1, type = 'scatter', mode = 'lines',
                    line = list(color = 'rgb(0, 1, 1)', width = 4)) %>% 
                add_trace(y = ~city2, name = city2, line = list(color = 'rgb(22, 96, 167)', width = 4)) %>% 
                add_trace(y = ~city3, name = city3, line = list(color = 'rgb(0, .4, .2)', width = 4)) %>% 
                add_trace(y = ~city4, name = city4, line = list(color = 'rgb(.4, .2, .6)', width = 4)) %>% 
                add_trace(y = ~city5, name = city5, line = list(color = 'rgb(.6, 0, 0)', width = 4))  
            
            
            } else if (length(input$city)==4)
            
            {
                
                
                city1 <- unique(input$city[1])
                city2 <- unique(input$city[2])
                city3 <- unique(input$city[3])
                city4 <- unique(input$city[4])
                
                
                City1_Temps <- temps_min_max_avg %>%
                    filter(City==city1)
                
                City2_Temps <- temps_min_max_avg %>%
                    filter(City==city2)
                
                City3_Temps <- temps_min_max_avg %>%
                    filter(City==city3)
                
                City4_Temps <- temps_min_max_avg %>%
                    filter(City==city4)
                
                
#                City1_Temps$Month <- factor(City1_Temps$Month, levels = City1_Temps[["Month"]])
                
                City1_Temps$city2 <- City2_Temps$Avg
                City1_Temps$city3 <- City3_Temps$Avg
                City1_Temps$city4 <- City4_Temps$Avg
                
                plot_ly(City1_Temps, x = ~Month, y = ~Avg, name = city1, type = 'scatter', mode = 'lines',
                        line = list(color = 'rgb(0, 1, 1)', width = 4)) %>% 
                    add_trace(y = ~city2, name = city2, line = list(color = 'rgb(22, 96, 167)', width = 4)) %>% 
                    add_trace(y = ~city3, name = city3, line = list(color = 'rgb(0, .4, .2)', width = 4)) %>% 
                    add_trace(y = ~city4, name = city4, line = list(color = 'rgb(.4, .2, .6)', width = 4))
                
            }
            
            
            else if (length(input$city)==3)
            {
                
                city1 <- unique(input$city[1])
                city2 <- unique(input$city[2])
                city3 <- unique(input$city[3])
                
                City1_Temps <- temps_min_max_avg %>%
                    filter(City==city1)
                
                City2_Temps <- temps_min_max_avg %>%
                    filter(City==city2)
                
                City3_Temps <- temps_min_max_avg %>%
                    filter(City==city3)
                
#                City1_Temps$Month <- factor(City1_Temps$Month, levels = City1_Temps[["Month"]])
                
                City1_Temps$city2 <- City2_Temps$Avg
                City1_Temps$city3 <- City3_Temps$Avg
                
                plot_ly(City1_Temps, x = ~Month, y = ~Avg, name = city1, type = 'scatter', mode = 'lines',
                        line = list(color = 'rgb(0, 1, 1)', width = 4)) %>% 
                    add_trace(y = ~city2, name = city2, line = list(color = 'rgb(22, 96, 167)', width = 4)) %>% 
                    add_trace(y = ~city3, name = city3, line = list(color = 'rgb(0, .4, .2)', width = 4))
                
                
            }
            
            
            
            else if (length(input$city)==2)
            {
                
                city1 <- unique(input$city[1])
                city2 <- unique(input$city[2])
                
                City1_Temps <- temps_min_max_avg %>%
                    filter(City==city1)
                
                City2_Temps <- temps_min_max_avg %>%
                    filter(City==city2)
                
#                City1_Temps$Month <- factor(City1_Temps$Month, levels = City1_Temps[["Month"]])
                
                City1_Temps$city2 <- City2_Temps$Avg
                
                plot_ly(City1_Temps, x = ~Month, y = ~Avg, name = city1, type = 'scatter', mode = 'lines',
                        line = list(color = 'rgb(0, 1, 1)', width = 4)) %>% 
                    add_trace(y = ~city2, name = city2, line = list(color = 'rgb(22, 96, 167)', width = 4))
                
            }
            

            
            else if (length(input$city)==1)
            {
                
                city1 <- unique(input$city[1])
                
                City1_Temps <- temps_min_max_avg %>%
                    filter(City==city1)
                
#                City1_Temps$Month <- factor(City1_Temps$Month, levels = City1_Temps[["Month"]])
                
                
                plot_ly(City1_Temps, x = ~Month, y = ~Avg, name = city1, type = 'scatter', mode = 'lines',
                        line = list(color = 'rgb(0, 1, 1)', width = 4))
                
            }
            
            
            
                        
        })
        
        
        
        output$distPlot2 <- renderPlotly({
            
            range_temps <- temps_min_max_avg %>%
                filter(City==input$city_temp)
            
            
            plot_ly(range_temps, x = ~Month, y = ~HighAvg, type = 'scatter', mode = 'lines',
                    line = list(color = 'transparent'),
                    showlegend = FALSE, name = 'Average High') %>%
                add_trace(y = ~LowAvg, type = 'scatter', mode = 'lines',
                          fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                          showlegend = FALSE, name = 'Average Low') %>%
                add_trace(x = ~Month, y = ~Avg, type = 'scatter', mode = 'lines',
                          line = list(color='rgb(0,100,80)'),
                          name = 'Average') %>%
                layout(title = paste("Average, High and Low Temperatures in",input$city_temp),
                       paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                       xaxis = list(title = "Months",
                                    gridcolor = 'rgb(255,255,255)',
                                    showgrid = TRUE,
                                    showline = FALSE,
                                    showticklabels = TRUE,
                                    tickcolor = 'rgb(127,127,127)',
                                    ticks = 'outside',
                                    zeroline = FALSE),
                       yaxis = list(title = "Temperature (degrees F)",
                                    gridcolor = 'rgb(255,255,255)',
                                    showgrid = TRUE,
                                    showline = FALSE,
                                    showticklabels = TRUE,
                                    tickcolor = 'rgb(127,127,127)',
                                    ticks = 'outside',
                                    zeroline = FALSE)) %>%
                layout(shapes = list(hline(60), hline(80)))
            
            
            
            
        })
        
        
        output$distPlot3 <- renderLeaflet({
          
          pal <- colorNumeric(
            palette = "viridis",
            domain = temps_min_max_avg_map$Average)
          
          leaflet(temps_min_max_avg_map) %>% 
            addTiles() %>% 
            addCircleMarkers(lat = ~temps_min_max_avg_map$Lat, lng = ~temps_min_max_avg_map$Long, label = paste(temps_min_max_avg_map$City, "Avg Temp:", round(temps_min_max_avg_map$Average,2)), color = ~pal(temps_min_max_avg_map$Average), fillColor = ~pal(temps_min_max_avg_map$Average)) %>%
            addProviderTiles("Esri.WorldGrayCanvas")
          
        })
        
        
        
        
        }
    
  

        
    
    #plot_ly(City1_Temps, x = ~Month, y = ~Temp, name = 'Chicago', type = 'scatter', mode = 'lines',
    #        line = list(color = 'rgb(0, 1, 1)', width = 4)) %>% 
    #    add_trace(y = ~Atlanta, name = 'Atlanta', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>%
    #    add_trace(y = ~Jacksonville, name = 'Jacksonville', line = list(color = 'rgb(0, .4, .2)', width = 4)) %>%
    #    add_trace(y = ~SD, name = 'San Diego', line = list(color = 'rgb(.4, .2, .6)', width = 4)) %>%
    #    add_trace(y = ~Phoenix, name = 'Phoenix', line = list(color = 'rgb(.6, 0, 0)', width = 4))
    
    
    

# Run the application 
shinyApp(ui = ui, server = server)
