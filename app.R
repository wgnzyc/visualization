packageList = c('plotly', 'ggvis', 'shiny', 'ggmap', 'ggrepel', 'dplyr', 'plyr', 'devtools', 'jsonlite', 'networkD3', 'googleVis')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

library(plotly)
library(ggvis)
library(shiny)
library(ggmap)
library(ggrepel)
library(dplyr)
library(plyr)
library(devtools)
install_github('ramnathv/rCharts', force = TRUE)
library(rCharts)
library(jsonlite)
library(networkD3)
library(googleVis)

flight_2008 <- read.csv(unz("new_2008.csv.zip", "new_2008.csv"))
drops <- c("X","CancellationCode")
flight_2008 <- flight_2008[ , !(names(flight_2008) %in% drops)]
airport <- read.csv("airports.csv")
plot1_x <- c("Month","DayofMonth","DayOfWeek")

########### delay df
flight_2008$UniqueCarrier <- as.character(flight_2008$UniqueCarrier)
flight_2008[flight_2008$UniqueCarrier=='WN',]$UniqueCarrier='South West'
flight_2008[flight_2008$UniqueCarrier=='9E',]$UniqueCarrier='Endeavor Air'
flight_2008[flight_2008$UniqueCarrier=='EV',]$UniqueCarrier='EVA Air'
flight_2008[flight_2008$UniqueCarrier=='YV',]$UniqueCarrier='Mesa Airlines'
flight_2008[flight_2008$UniqueCarrier=='OO',]$UniqueCarrier='SkyWest Airlines'
flight_2008[flight_2008$UniqueCarrier=='US',]$UniqueCarrier='US Airways'
flight_2008[flight_2008$UniqueCarrier=='XE',]$UniqueCarrier='ExpressJet Airlines'
flight_2008[flight_2008$UniqueCarrier=='OH',]$UniqueCarrier='Comair'
flight_2008[flight_2008$UniqueCarrier=='MQ',]$UniqueCarrier='American Eagle'
flight_2008[flight_2008$UniqueCarrier=='DL',]$UniqueCarrier='Delta Air Lines'
flight_2008[flight_2008$UniqueCarrier=='NW',]$UniqueCarrier='Northwest Airlines'
flight_2008[flight_2008$UniqueCarrier=='UA',]$UniqueCarrier='United Airlines'
flight_2008[flight_2008$UniqueCarrier=='AA',]$UniqueCarrier='American Airlines'
flight_2008[flight_2008$UniqueCarrier=='F9',]$UniqueCarrier='Frontier'
flight_2008[flight_2008$UniqueCarrier=='CO',]$UniqueCarrier='Continental Airlines'
flight_2008[flight_2008$UniqueCarrier=='B6',]$UniqueCarrier='JetBlue Airways'
flight_2008[flight_2008$UniqueCarrier=='AS',]$UniqueCarrier='Alaska Airlines'
flight_2008[flight_2008$UniqueCarrier=='FL',]$UniqueCarrier='AirTran Airways'
flight_2008[flight_2008$UniqueCarrier=='HA',]$UniqueCarrier='Hawaiian Airlines'
flight_2008[flight_2008$UniqueCarrier=='AQ',]$UniqueCarrier='Aloha'

delayDF <- data.frame("month" = flight_2008$Month, "airline" = flight_2008$UniqueCarrier, "arrdelay" = flight_2008$ArrDelay, "depdelay" = flight_2008$DepDelay)
delayDF <- delayDF %>% group_by(month, airline) %>% summarise_each(funs(mean), arrdelay, depdelay)
colnames(delayDF) <- c('Month', 'Airline', 'Average_Arrival_Delay', 'Average_Departure_Delay')
delayDF <- aggregate(. ~ Month+Airline, data = delayDF, FUN = mean)
delayDF$nflight <- 0

colnames(airport)[1] <- 'Origin'
origin_join <- merge(x = flight_2008, y = airport[,c(1,3,6:7)], by = "Origin", all.x = TRUE)
colnames(airport)[1] <- 'Dest'
colnames(origin_join)[29:31] <- c("origin_city", "origin_lat", "origin_long")
dest_join <- merge(x = origin_join, y = airport[,c(1,3,6:7)], by = "Dest", all.x = TRUE)
colnames(dest_join)[32:34] <- c("destination_city", "dest_lat", "dest_long")

for (airline in unique(dest_join$UniqueCarrier)[unique(dest_join$UniqueCarrier)!="Aloha"]) {
  for (month in unique(dest_join$Month)) {
    delayDF[delayDF$Month==month & delayDF$Airline==airline,]$nflight <- dim(dest_join[dest_join$UniqueCarrier==airline & dest_join$Month==month,])[1]
  }
}
############

############ candel df
cancelDF <- data.frame("month"=flight_2008$Month, "airline"=flight_2008$UniqueCarrier, 'CarrierDelay'=flight_2008$CarrierDelay, 'WeatherDelay'=flight_2008$WeatherDelay, 'NASDelay'=flight_2008$NASDelay, 'SecurityDelay'=flight_2008$SecurityDelay, 'LateAircraftDelay'=flight_2008$LateAircraftDelay)
cancelDF <- cancelDF %>% group_by(airline, month) %>% summarise_each(funs(mean), CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)
############


original_places <- levels(dest_join$Origin)

ui <- fluidPage(
  headerPanel('Flight data'),
  sidebarPanel(
    conditionalPanel(condition = "input.conditionedPanels == 1", 
                     selectInput("month", "X-Value", choices = plot1_x)),
    conditionalPanel(condition = "input.conditionedPanels == 2",
                     #selectInput("origin","Origin",choices = original_places),
                     textInput("origin", "Airport Code (eg SFO)", value = ""),
                     selectInput("map_airline", "Select Airline", choices = unique(dest_join$UniqueCarrier)),
                     selectInput("month_2","departure month",choices = list("January"=1, "February"=2, "March"=3, "April"=4, 
                                                                            "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, 
                                                                            "October"=10, "November"=11, "December"=12)),
                     selectInput("dayofweek","day of week",choices = list("Monday"=1, "Tuesday"=2, "Wednesday"=3, "Thursday"=4, 
                                                                          "Friday"=5, "Saturday"=6, "Sunday"=7))),
    conditionalPanel(condition = "input.conditionedPanels ==3",
                     selectInput(inputId = "Month", label = "Select Month", 
                                 choices = list("January"=1, "February"=2, "March"=3, "April"=4, 
                                                "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, 
                                                "October"=10, "November"=11, "December"=12), selected = 1)),
    conditionalPanel(condition = "input.conditionedPanels == 4",
                     # selectInput(inputId = "cancelMonth", label = "Select Month", 
                     #             choices = list("January"=1, "February"=2, "March"=3, "April"=4, 
                     #                            "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, 
                     #                            "October"=10, "November"=11, "December"=12), selected = 1)
                     selectInput(inputId = "cancelAirline", label = "Select Airline", choices = unique(cancelDF$airline))
                     ),
    conditionalPanel(condition = "input.conditionedPanels == 5",
                     numericInput("num1","month",1,1,12,1),
                     numericInput("num2","Day of Month",1,1,31,1)
  )),
  mainPanel(
    # uiOutput("ggvis_ui"),
    # ggvisOutput("ggvis"),
    tabsetPanel(id = 'conditionedPanels',
                tabPanel("line plot", plotlyOutput("hist1"),value = 1),
                tabPanel("map plot", plotOutput("hist2",hover = hoverOpts(id ="plot_hover")),value = 2),
                tabPanel("bubble plot", plotlyOutput("delayPlot"),value = 3),
                tabPanel("bar chart",plotlyOutput("cancelPlot"),value = 4),
                tabPanel("flow/carrier",sankeyNetworkOutput("hist5"),value = 5)
    )
  )
)

server <- function(input, output) {
  output$hist1 <- renderPlotly({
    ipt <- reactive(input$month)
    hist1_data <- dest_join[c(ipt(),"UniqueCarrier","FlightNum")]
    hist1_ct <- as.data.frame(table(hist1_data[,c(as.character(ipt()),"UniqueCarrier")]))
    hist1_ct[,1] <- as.numeric(hist1_ct[,1])
    colnames(hist1_ct) <- c("Time","UniqueCarrier","Freq")
    plot_ly(data = hist1_ct,x = ~Time,y = ~Freq,type = 'scatter',mode = 'lines',color = ~UniqueCarrier)%>%
      layout(showlegend = FALSE,
             xaxis = list(title = as.name(ipt()),range = c(min(hist1_ct[,1]),max(hist1_ct[,1]))),
             yaxis = list(title = "number of flights"))
  })
  
  output$hist2 <- renderPlot({
    worldmap <- borders("usa", colour="#efede1", fill="#efede1")
    ggplot(data=dest_join[dest_join$Origin==input$origin& dest_join$Month==input$month_2&dest_join$UniqueCarrier==input$map_airline&dest_join$DayOfWeek==input$dayofweek,]) + worldmap +
      geom_curve(aes(x = origin_long, y = origin_lat, xend = dest_long, yend = dest_lat), col = "#b29e7d", size = 0.5, curvature = .2) +
      geom_point(aes(x = origin_long, y = origin_lat), col = "#970027")+
      geom_text(aes(x = origin_long, y = origin_lat,label = origin_city),hjust=1.5, vjust=0.5,size = 5)+
      geom_point(aes(x = dest_long, y = dest_lat), col = "#970027") +
      geom_text(aes(x = dest_long, y = dest_lat,label = destination_city),hjust=-0.1, vjust=-0.1,size = 5)+
      #geom_text_repel(data=dest_join[dest_join$Origin=="SFO"& dest_join$Month==5&dest_join$DayOfWeek==1,], aes(x = origin_long, y = origin_lat, label = Origin), col = "black", size = 2, segment.color = NA) +
      #geom_text_repel(data=dest_join[dest_join$Origin=="SFO"& dest_join$Month==5&dest_join$DayOfWeek==1,], aes(x = dest_long, y = dest_lat, label = Dest), col = "black", size = 2, segment.color = NA) +
      theme(panel.background = element_rect(fill="white"),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
      )
  })
  
  output$delayPlot <- renderPlotly({
    delayMonth = reactive(input$Month)
    p <- plot_ly(delayDF[delayDF$Month==delayMonth(),], x = ~Average_Arrival_Delay, y = ~Average_Departure_Delay, text = ~paste('Airline:', Airline, '<br>Number of Flights:', nflight), 
                 size=~nflight, type = 'scatter', mode = 'markers', color = ~Airline, colors = 'Paired',
                 marker = list(symbol = 'circle', sizemode = 'diameter', opacity = 0.7,
                               line = list(width = 2, color = '#FFFFFF'))) %>%
      layout(title = 'Average Arrival Delay vs Average Departure Delay',
             xaxis = list(title = 'Average Arrival Delay',
                          gridcolor = 'rgb(255, 255, 255)',
                          type = 'log',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
             yaxis = list(title = 'Average Departure Dalay',
                          gridcolor = 'rgb(255, 255, 255)',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
             showlegend = FALSE, paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
    p
  })
  
  output$cancelPlot <- renderPlotly({
    #cancelMonth <- reactive(input$cancelMonth)
    cancelairline <- reactive(input$cancelAirline)
    
    p <- plot_ly(cancelDF[cancelDF$airline==cancelairline(),], x=~month, y=~CarrierDelay, type = 'bar', name='Carrier Delay', marker = list(color = 'black')) %>%
      add_trace(y = ~WeatherDelay, name = 'Weather Delay', marker = list(color = 'red')) %>%
      add_trace(y = ~NASDelay, name = 'NAS Delay', marker = list(color = 'green')) %>%
      add_trace(y = ~LateAircraftDelay, name = 'Late Aircraft Delay', marker = list(color = 'blue')) %>%
      layout(title = 'Different Types of Delays in Minutes Each Month',
             xaxis = list(
               title = "",
               tickfont = list(
                 size = 20)),
             yaxis = list(
               title = 'Average Delay in Minutes',
               titlefont = list(
                 size = 20),
               tickfont = list(
                 size = 14)),
             legend = list(x = 0, y = 1),
             barmode = 'group', bargap = 0.3)
    
    p
  })
  
  output$hist5 <- renderSankeyNetwork({
    ####### source/target #######
    may <- dest_join[dest_join$Month == input$num1 & dest_join$DayofMonth ==input$num2,]
    #may <- dest_join[dest_join$Month == 5 &dest_join$DayofMonth ==1,]
    may_count <- plyr::count(may, c('Origin','UniqueCarrier'))
    origin_count <- plyr::count(may,vars = c("Origin"))
    selected_origin <- origin_count[origin_count$freq>50,]$Origin
    may_count <- may_count[may_count$Origin%in%selected_origin,]
    colnames(may_count) <- c('source','target','value')
    unique_select <- unique(data.frame(source=may_count$source,
                                       target=may_count$target))
    
    unique_source <- as.data.frame(unique(unique_select$source))
    unique_source$sid <- c(0:(nrow(unique_source)-1))
    colnames(unique_source) <- c('source','sid')
    
    unique_target <- as.data.frame(unique(unique_select$target))
    unique_target$tid <- c(nrow(unique_source):(nrow(unique_source)+(nrow(unique_target)-1)))
    colnames(unique_target) <- c('target','tid')
    
    uni_ss <- merge(unique_select,unique_source,by = 'source')
    uni_sst <- merge(uni_ss,unique_target,by = c('target'))
    
    final_uni <- merge(may_count,uni_sst,by = c('target','source'))
    colnames(final_uni) <- c("targetName","sourceName","value","source","target")
    final_uni = final_uni[,c("sourceName","targetName","source","target","value")]
    
    node <- as.data.frame(unique_source$source)
    colnames(node) <- c('node')
    node2 <- as.data.frame(unique_target$target)
    colnames(node2) <- c('node')
    nodes <- rbind(node,node2)
    
    x <- list(link = final_uni[,c('source','target','value')], 
              node = nodes)
    toJSON(x, pretty = TRUE, auto_unbox = TRUE)
    
    
    options(gvis.plot.tag = 'chart')
    sankeyNetwork(Links = x$link, Nodes = x$node, Source = "source",
                  Target = "target", Value = "value", NodeID = "node",
                  fontSize = 12, nodeWidth = 30)
  })
}

shinyApp(ui = ui, server = server)
