#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggmap)
library(gridExtra)
library(maps)      
library(ggplot2)
map <- map_data("state")
register_google(key = Sys.getenv("GoogleAKey"))
cdcdata = read.csv(file = "cdc.csv")
allkwrdscoordinates_state = read.csv("state_frequency_all.csv")
flukwrdcoordinates_state=read.csv("state_frequency_flu.csv")
Influenzakwrdcoordinates_state = read.csv("state_frequency_influenza.csv")
# Define UI for application that draws a histogram
ui <- fluidPage( pageWithSidebar(
   
   # Application title
   titlePanel("Twitter data maps comparison with CDC maps for Flu"),
   
   
      sidebarPanel(
        selectInput("flukeyword","Please select the map you want", choices = c("Map with #Flu And #Influenza Tweets","Map with only #Flu Tweets","Map with only #Influenza Tweets"),multiple = FALSE)
         
      ),
      
     
      mainPanel(
         plotOutput("distPlot"),
         verbatimTextOutput("caption")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  Allkeywordmapanalysis ="As we  compare the cdc heat map to the heatmap created by us with tweets having 'influenza','flu' and other keywords we can note a few points.
    ->Few states have high level of tweets such as California, Texas, Ohio, Pennsylvania, New york, Georgia and Massachusetts. Same can be observed in cdc data.
    -> Other states that seem to be having high number of cases in cdc map dont reflect the same in our map.It might be due to the fact that people didnt tweet about their condition or we might have not come accross them.
    ->The states reflecting low number of cases in cdc namely Idaho, , Utah and North Dakota reflect the same with our map.
    ->The data we have collected over twitter seems to reflect the similar trends that are shown by cdc data. Though cdc data extensive and showing large number os cases where our data isn't but there is no place where the reverse trend is observed so it will be safe to assume twitter can be used to determine trends uptill a certain point.
    ->We didn't come across data from few states so we cant comment on their trends."
   FluMapAnalysis="As we draw a comparison between cdc heat map and the heatmap created by us with tweets having 'flu' keyword we can note a few points.
    ->Few states have high level of tweets such as  Ohio, Pennsylvania and Georgia. Same can be observed in cdc data.
    -> Other states that seem to be having high number of cases in cdc map dont reflect the same in our map.It might be due to the fact that people didnt tweet about their condition or we might have not come accross them.
    ->Utah reflects low number of cases in cdc is reflecting the same with our map.
    ->The data we have collected over twitter seems to reflect the similar trends that are shown by cdc data. Though cdc data extensive and showing large number os cases where our data isn't but there is no place where the reverse trend is observed so it will be safe to assume twitter can be used to determine trends uptill a certain point.
    ->We didn't come across data from few states so we cant comment on their trends."
   InfluenzaMapAnalysis ="As we draw a comparison between cdc heat map and the heatmap created by us with tweets having 'influenza' keyword we can note a few points.
    ->Few states have high level of tweets such as  Ohio and Georgia. Same can be observed in cdc data.
    -> Other states that seem to be having high number of cases in cdc map dont reflect the same in our map.It might be due to the fact that people didnt tweet about their condition or we might have not come accross them.
    ->Utah reflects low number of cases in cdc is reflecting the same with our map.
    ->The data we have collected over twitter seems to reflect the similar trends that are shown by cdc data. Though cdc data extensive and showing large number os cases where our data isn't but there is no place where the reverse trend is observed so it will be safe to assume twitter can be used to determine trends uptill a certain point.
    ->We didn't come across data from few states so we cant comment on their trends."
   output$distPlot <- renderPlot({
     keywordtype<-input$flukeyword
     if(keywordtype=="Map with #Flu And #Influenza Tweets"){
      
       
       theme_opts = list(theme(panel.grid.minor = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.background = element_blank(),
                               plot.background = element_blank(),
                               panel.border = element_blank(),
                               axis.line = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               legend.text = element_text(),
                               legend.position="right"))
       
       
       tweet_plot <- ggplot(data = allkwrdscoordinates_state) + ggtitle("Tweets with all keyword") +
         geom_polygon(data = map,aes(x=long, y = lat, group = group), fill = NA, color="black", size=0.25) +
         geom_map(data = allkwrdscoordinates_state, color = "black", map = map, aes(map_id = region, fill = frequency)) +
         coord_map()+
         theme_opts+
         scale_fill_continuous(low= "green", high="red",  space = "Lab",na.value="white",guide="colorbar")
       
       
      
       
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 10"]= c(10)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 9"] = c(9)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 8"] = c(8)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 7"] = c(7)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 5"] = c(6)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 6"] = c(5)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 4"] = c(4)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 3"] = c(3)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 2"] = c(2)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 1"] = c(1)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 0"] = c(0)
       
       Level = as.numeric(cdcdata$ILI_ACTIVITY_LEVEL)
       
       theme_opts1 = list(theme(panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.background = element_blank(),
                                plot.background = element_blank(),
                                panel.border = element_blank(),
                                axis.line = element_blank(),
                                axis.text.x = element_blank(),
                                axis.text.y = element_blank(),
                                axis.ticks = element_blank(),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                #legend.key.size = unit(.2,"cm"),
                                legend.text = element_text(),
                                legend.position="right"))
       flu_plot <- ggplot(data = cdcdata) + ggtitle("Heat Map with CDC Data") +
         geom_polygon(data = map,aes(x=long, y = lat, group = group), fill = NA, color="black", size=0.25) +
         geom_map(data = cdcdata, color = "black", map = map, aes(map_id = STATENAME, fill = Level)) +
         coord_map()+
         theme_opts1 + scale_fill_continuous(low= "green", high="red",  space = "Lab",na.value="white",guide="colorbar")
       
       
       ### plotting side-by-side ###########
       
       grid.arrange(flu_plot, tweet_plot, ncol = 2)
       
     }
     else if (keywordtype=="Map with only #Flu Tweets"){
       
       
       theme_opts = list(theme(panel.grid.minor = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.background = element_blank(),
                               plot.background = element_blank(),
                               panel.border = element_blank(),
                               axis.line = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               legend.text = element_text(),
                               legend.position="right"))
       
   
       tweet_plot <- ggplot(data = flukwrdcoordinates_state) + ggtitle("Tweets with keyword '#FLU'") +
         geom_polygon(data = map,aes(x=long, y = lat, group = group), fill = NA, color="black", size=0.25) +
         geom_map(data = flukwrdcoordinates_state, color = "black", map = map, aes(map_id = region, fill = frequency)) +
         coord_map()+
         theme_opts+
         scale_fill_continuous(low= "green", high="red",  space = "Lab",na.value="white",guide="colorbar")
       
       
       
       
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 10"]= c(10)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 9"] = c(9)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 8"] = c(8)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 7"] = c(7)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 5"] = c(6)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 6"] = c(5)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 4"] = c(4)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 3"] = c(3)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 2"] = c(2)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 1"] = c(1)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 0"] = c(0)
       
       Level = as.numeric(cdcdata$ILI_ACTIVITY_LEVEL)
       
       theme_opts1 = list(theme(panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.background = element_blank(),
                                plot.background = element_blank(),
                                panel.border = element_blank(),
                                axis.line = element_blank(),
                                axis.text.x = element_blank(),
                                axis.text.y = element_blank(),
                                axis.ticks = element_blank(),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                #legend.key.size = unit(.2,"cm"),
                                legend.text = element_text(),
                                legend.position="right"))
       flu_plot <- ggplot(data = cdcdata) + ggtitle("Heat Map with CDC Data") +
         geom_polygon(data = map,aes(x=long, y = lat, group = group), fill = NA, color="black", size=0.25) +
         geom_map(data = cdcdata, color = "black", map = map, aes(map_id = STATENAME, fill = Level)) +
         coord_map()+
         theme_opts1 + scale_fill_continuous(low= "green", high="red",  space = "Lab",na.value="white",guide="colorbar")
       
       
       ### plotting side-by-side ###########
       
       grid.arrange(flu_plot, tweet_plot, ncol = 2)
     }
     else {
  
       theme_opts = list(theme(panel.grid.minor = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.background = element_blank(),
                               plot.background = element_blank(),
                               panel.border = element_blank(),
                               axis.line = element_blank(),
                               axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               legend.text = element_text(),
                               legend.position="right"))
       
      
       tweet_plot <- ggplot(data = Influenzakwrdcoordinates_state) + ggtitle("Tweets with keyword '#Influenza'") +
         geom_polygon(data = map,aes(x=long, y = lat, group = group), fill = NA, color="black", size=0.25) +
         geom_map(data = Influenzakwrdcoordinates_state, color = "black", map = map, aes(map_id = region, fill = frequency)) +
         coord_map()+
         theme_opts+
         scale_fill_continuous(low= "green", high="red",  space = "Lab",na.value="white",guide="colorbar")
       
       
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 10"]= c(10)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 9"] = c(9)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 8"] = c(8)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 7"] = c(7)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 5"] = c(6)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 6"] = c(5)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 4"] = c(4)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 3"] = c(3)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 2"] = c(2)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 1"] = c(1)
       cdcdata$ILI_ACTIVITY_LEVEL[cdcdata$ACTIVITY.LEVEL == "Level 0"] = c(0)
       
       Level = as.numeric(cdcdata$ILI_ACTIVITY_LEVEL)
       
       theme_opts1 = list(theme(panel.grid.minor = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.background = element_blank(),
                                plot.background = element_blank(),
                                panel.border = element_blank(),
                                axis.line = element_blank(),
                                axis.text.x = element_blank(),
                                axis.text.y = element_blank(),
                                axis.ticks = element_blank(),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                #legend.key.size = unit(.2,"cm"),
                                legend.text = element_text(),
                                legend.position="right"))
       flu_plot <- ggplot(data = cdcdata) + ggtitle("Heat Map with CDC Data") +
         geom_polygon(data = map,aes(x=long, y = lat, group = group), fill = NA, color="black", size=0.25) +
         geom_map(data = cdcdata, color = "black", map = map, aes(map_id = STATENAME, fill = Level)) +
         coord_map()+
         theme_opts1 + scale_fill_continuous(low= "green", high="red",  space = "Lab",na.value="white",guide="colorbar")
       
       
       ### plotting side-by-side ###########
       
       grid.arrange(flu_plot, tweet_plot, ncol = 2)
       
     }
          })
   output$caption<-renderText({
     keywordtype<-input$flukeyword
     if(keywordtype=="Map with #Flu And #Influenza Tweets"){
       Allkeywordmapanalysis
     }
     else if (keywordtype=="Map with only #Flu Tweets"){
       FluMapAnalysis
     }
     else {
       InfluenzaMapAnalysis
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

