#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
label_fill <- function(orig, .offset=0, mod, .fill=""){
  ii <- as.logical(
    ## offset==0 keeps first
    (1:length(orig)-1+.offset) %% mod
  )
  orig[ii] <- .fill
  orig
}

library("ggplot2")
library(dplyr)
library(tidyr)
library(ggmap)
us_map <- map_data("state")
stateData_full <- read.csv("StateDataforMap_2018-19week7.csv", header=T)
stateData = data.frame(region=tolower(stateData_full$STATENAME), level=stateData_full$ACTIVITY.LEVEL, stringsAsFactors=F)
stateData[,2] <- substring(stateData[,2],6,8)
stateData$level <- (as.numeric(stateData$level)+11)
stateData$level <- (as.character(stateData$level))

ns1<-read.csv("ns.csv")
ns1data1<-data.frame(x=format(ns1$YEARWEEK,scientific=FALSE),y=ns1$COUNT,fill_var=ns1$TYPE)
ns1TOTAL_A <- subset(ns1, ns1$COUNT > 0)
ns1my_breaks <- format(ns1$YEARWEEK,scientific=FALSE)
ps1<-read.csv("ps1.csv")

ps1my_breaks <- format(ps1$YEARWEEK,scientific=FALSE)
ns1PERCENT_POSITIVE<- subset(ns1, ns1$PERCENT.POSITIVE > 0)
pd1<-read.csv("pediatricdeath.csv")
pd1df <- data.frame(x=pd1$WEEK.NUMBER,y=pd1$COUNT,fill_var=pd1$WEEK)
pd1total16<-paste("\n Number of Deaths \nReported =",sum(pd1$NO_OF_DEATHS[1:98])/2)
pd1total17<-paste("\n Number of Deaths \nReported =",sum(pd1$NO_OF_DEATHS[99:202])/2)
pd1total18<-paste("\n Number of Deaths \nReported =",sum(pd1$NO_OF_DEATHS[203:310])/2)
pd1total19<-paste("\n Number of Deaths \nReported =",sum(pd1$NO_OF_DEATHS[311:416])/2)
pd1my_breaks <- format(pd1$WEEK.NUMBER,scientific=FALSE)
ns2<-read.csv("ns_part5_1.csv")
ns2data1<-data.frame(x=format(ns2$YEARWEEK,scientific=FALSE),y=ns2$COUNT,fill_var=ns2$TYPE)
ns2TOTAL_A <- subset(ns2, ns2$COUNT > 0)
ns2my_breaks <- format(ns2$YEARWEEK,scientific=FALSE)

ns2PERCENT_POSITIVE<- subset(ns2, ns2$PERCENT.POSITIVE > 0)
ns3<-read.csv("ns_part6.csv")
ns3data1<-data.frame(x=format(ns3$YEARWEEK,scientific=FALSE),y=ns3$COUNT,fill_var=ns3$TYPE)
ns3TOTAL_A <- subset(ns3, ns3$COUNT > 0)
ns3my_breaks <- format(ns3$YEARWEEK,scientific=FALSE)

ns3PERCENT_POSITIVE<- subset(ns3, ns3$PERCENT.POSITIVE > 0)

ps2<-read.csv("ps2.csv")

ps2my_breaks <- format(ps2$YEARWEEK,scientific=FALSE)
ld1<-read.csv("Ili.csv")
ld1data<-data.frame(x=factor(ld1$WEEK , levels=unique(ld1$WEEK )),stringsAsFactors=FALSE)
ld1datanew<-subset(ld1, ld1$Year2018_19 > 0)
ld1datan<-data.frame(x=factor(ld1datanew$WEEK , levels=unique(ld1datanew$WEEK )),stringsAsFactors=FALSE)
ld1my_breaks <-ld1$WEEK

choices<-c("Influenza national summary","Positive tested","Pediatric deaths","Influenza-like illness","CDC Flu heat map of USA")

# Define UI for application that draws a histogram
ui <- fluidPage(
   pageWithSidebar(
   # Application title
   titlePanel(h3("Flu Data For USA made by Prabhleen Kaur and Arsalan Gundroo")),
   
  sidebarPanel(
   selectInput("graph","Select the graph to view",choices = choices,multiple = FALSE,selected = "Influenza national summary"),
   conditionalPanel(condition = "input.graph=='Influenza national summary'",
                    selectInput("clinicaldata","Select time period and region",choices =c("2018-19","2018 whole year for USA","2018 whole year for New York"),multiple=FALSE,selected = "2018-19")),
   conditionalPanel(condition = "input.graph=='Positive tested'",
                    selectInput("publicdata","Select time period and region",choices =c("2018-19","2018 whole year for USA"),multiple=FALSE,selected="2018-19"))
   
    ),
 
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   ))


# Define server logic required to draw a histogram
server <- function(input, output) {

   output$distPlot <- renderPlot({
     
     graphtype<-input$graph
     if (graphtype=="Influenza national summary"){
       graphtime<-input$clinicaldata
       if(graphtime=="2018-19"){
      p<-ggplot() +ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories,\nNational Summary, 2018-2019 Season")+
       geom_bar(ns1data1,mapping=aes(x=ns1data1$x,y=ns1data1$y,fill=ns1data1$fill_var),stat = "identity")
     q<-p +labs( x="Week", y="Number of positive Specimens")+coord_cartesian(ylim = c(0,10000))+theme(axis.text.x = element_text(angle = 60, hjust = 1))
     a<-q+scale_y_continuous(sec.axis = sec_axis(~ ./400, name = "Percent Positive"))+geom_path(ns1PERCENT_POSITIVE,mapping = aes(x =format(ns1PERCENT_POSITIVE$YEARWEEK,scientific=FALSE), y = ns1PERCENT_POSITIVE$PERCENT.POSITIVE*400,group = 3,color="PP"))+geom_path(ns1TOTAL_A,mapping = aes(x =format(ns1TOTAL_A$YEARWEEK,scientific=FALSE), y = ns1TOTAL_A$PERCENT.A*400,group = 3,color="PPA"),linetype = "dotted")
     mygraph<- a+geom_path(ns1TOTAL_A,mapping = aes(x =format(ns1TOTAL_A$YEARWEEK,scientific=FALSE), y = ns1TOTAL_A$PERCENT.B*400,group = 4 , color="PPB"),linetype = "dotted")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
     mynewgraph <-mygraph+ scale_color_manual(name="",labels = c("Percent Positive", "% Positive Flu A","% Positive Flu B"),values = c("PP"= "black", "PPA"="goldenrod3","PPB"="green4"))+ scale_fill_manual("",labels=c("A","B"),values=c("A"="goldenrod1","B"="dark green"))+theme(legend.key = element_blank(),legend.justification = c(1, 1), legend.position = c(1, 1),legend.spacing.y = unit(-0.3, "cm"),plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))
     mynewgraph + scale_x_discrete(breaks=ns1my_breaks, labels=label_fill(ns1my_breaks,mod=4))   }
     
     else if (graphtime=="2018 whole year for USA"){
         p<-ggplot() +ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories,\nNational Summary, 2018-2019 Season")+
         geom_bar(ns2data1,mapping=aes(x=ns2data1$x,y=ns2data1$y,fill=ns2data1$fill_var),stat = "identity")
       q<-p +labs( x="Week", y="Number of positive Specimens")+coord_cartesian(ylim = c(0,22000))+theme(axis.text.x = element_text(angle = 60, hjust = 1))
       a<-q+scale_y_continuous(sec.axis = sec_axis(~ ./400, name = "Percent Positive"))+geom_path(ns2PERCENT_POSITIVE,mapping = aes(x =format(ns2PERCENT_POSITIVE$YEARWEEK,scientific=FALSE), y = ns2PERCENT_POSITIVE$PERCENT.POSITIVE*400,group = 3,color="PP"))+geom_path(ns2TOTAL_A,mapping = aes(x =format(ns2TOTAL_A$YEARWEEK,scientific=FALSE), y = ns2TOTAL_A$PERCENT.A*400,group = 3,color="PPA"),linetype = "dotted")
       mygraph<- a+geom_path(ns2TOTAL_A,mapping = aes(x =format(ns2TOTAL_A$YEARWEEK,scientific=FALSE), y = ns2TOTAL_A$PERCENT.B*400,group = 4 , color="PPB"),linetype = "dotted")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
       mynewgraph <-mygraph+ scale_color_manual(name="",labels = c("Percent Positive", "% Positive Flu A","% Positive Flu B"),values = c("PP"= "black", "PPA"="goldenrod3","PPB"="green4"))+ scale_fill_manual("",labels=c("A","B"),values=c("A"="goldenrod1","B"="dark green"))+theme(legend.key = element_blank(),legend.justification = c(1, 1), legend.position = c(1, 1),legend.spacing.y = unit(-0.3, "cm"),plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))
       mynewgraph + scale_x_discrete(breaks=ns2my_breaks, labels=label_fill(ns2my_breaks, mod=4)) 
     }
       else {
         p<-ggplot() +ggtitle("Influenza Positive Tests Reported to CDC by U.S. Clinical Laboratories,\nNational Summary, 2018-2019 Season")+
           geom_bar(ns3data1,mapping=aes(x=ns3data1$x,y=ns3data1$y,fill=ns3data1$fill_var),stat = "identity")
         q<-p +labs( x="Week", y="Number of positive Specimens")+coord_cartesian(ylim = c(0,3000))+theme(axis.text.x = element_text(angle = 60, hjust = 1))
         a<-q+scale_y_continuous(sec.axis = sec_axis(~ ./90, name = "Percent Positive"))+geom_path(ns3PERCENT_POSITIVE,mapping = aes(x =format(ns3PERCENT_POSITIVE$YEARWEEK,scientific=FALSE), y = ns3PERCENT_POSITIVE$PERCENT.POSITIVE*90,group = 3,color="PP"))+geom_path(ns3TOTAL_A,mapping = aes(x =format(ns3TOTAL_A$YEARWEEK,scientific=FALSE), y = ns3TOTAL_A$PERCENT.A*90,group = 3,color="PPA"),linetype = "dotted")
         mygraph<- a+geom_path(ns3TOTAL_A,mapping = aes(x =format(ns3TOTAL_A$YEARWEEK,scientific=FALSE), y = ns3TOTAL_A$PERCENT.B*90,group = 4 , color="PPB"),linetype = "dotted")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
         mynewgraph <-mygraph+ scale_color_manual(name="",labels = c("Percent Positive", "% Positive Flu A","% Positive Flu B"),values = c("PP"= "black", "PPA"="goldenrod3","PPB"="green4"))+ scale_fill_manual("",labels=c("A","B"),values=c("A"="goldenrod1","B"="dark green"))+theme(legend.key = element_blank(),legend.justification = c(1, 1), legend.position = c(1, 1),legend.spacing.y = unit(-0.3, "cm"),plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))
         mynewgraph + scale_x_discrete(breaks=ns3my_breaks, labels=label_fill(ns3my_breaks, mod=4)) 
         
       }
     }
     else if (graphtype=="Positive tested"){
       graphtime<-input$publicdata
       if(graphtime=="2018-19"){
       p<-ggplot() +ggtitle("Influenza Positive Tests Reported to CDC by U.S. Public Health Laboratories,\nNational Summary, 2018 ")+ geom_bar(aes(y = ps1$COUNT, x = format(ps1$YEARWEEK,scientific=FALSE), fill = ps1$TYPE),stat="identity",position = position_stack(reverse = FALSE))
       
       geom_bar(aes(y = ps1$COUNT, x = format(ps1$YEARWEEK,scientific=FALSE), fill = ps1$TYPE),stat="identity",position = position_stack(reverse = FALSE))
       x<-p+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
       w<-x+labs( x="Week", y="Number of positive Specimens")+coord_cartesian(ylim = c(0,2500))+theme(axis.text.x = element_text(angle = 60, hjust = 1))
       mynewgraph<-w+ scale_fill_manual("",labels=c("A(H1N1)pdm09","A(H3N2)","A(Subtyping not performed)","B(lineage not performed)","B(Victoria Lineage)","B(Yamagata Lineage)","H3N2v"),values=c("A_Subtyping_not_Performed"="yellow","A_H1N1"="goldenrod1","A_H3"="red","H3N2v"="darkviolet","B"="darkgreen","BVic"="darkolivegreen","BYam"="green3"))+theme(legend.key = element_blank(),legend.justification = c(1, 1), legend.position = c(1, 1),legend.spacing.y = unit(-0.3, "cm"),plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))
       mynewgraph + scale_x_discrete(breaks=ps1my_breaks, labels=label_fill(ps1my_breaks, mod=2)) 
       }
       else {
       
         p<-ggplot() + geom_bar(aes(y = ps2$COUNT, x = format(ps2$YEARWEEK,scientific=FALSE), fill = ps2$TYPE),stat="identity",position = position_stack(reverse = FALSE))
         x<-p+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
         w<-x+labs( x="Week", y="Number of positive Specimens")+coord_cartesian(ylim = c(0,4500))+theme(axis.text.x = element_text(angle = 60, hjust = 1))
         mynewgraph<-w+ scale_fill_manual("",values=c("A (Subtyping not Performed)"="yellow","A (2009 H1N1)"="goldenrod1","A (H3)"="red","H3N2v"="darkviolet","B"="darkgreen","BVic"="darkolivegreen","BYam"="green3"))+theme(legend.key = element_blank(),legend.justification = c(1, 1), legend.position = c(1, 1),legend.spacing.y = unit(-0.3, "cm"),plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))
         mynewgraph + scale_x_discrete(breaks=ps2my_breaks, labels=label_fill(ps2my_breaks, mod=2)) +ggtitle("Influenza Positive Tests Reported to CDC by U.S. Public Health Laboratories,\nNational Summary, 2018 ")
         
         
         
           }
     }
      else if (graphtype=='Pediatric deaths'){
          
        p<-ggplot(pd1df,aes(x=x,y=y,fill=fill_var,group=1)) +ggtitle("Number of Influenza Associated Pediatric Deaths,\nBy Week of Death: 2015-16 session to present")+ geom_bar(color="black",stat="identity",width=1.2,position=position_stack(vjust = 1, reverse = FALSE))
        x<-p+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
        w<-x+labs( x="Week of Deaths", y="Number of deaths")+coord_cartesian(ylim = c(0,30))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        mynewgraph<-w+theme(legend.key = element_blank(),legend.position="bottom",legend.background = element_rect(linetype = 1, size = 0.5, colour = "black"),legend.spacing.y = unit(-0.3, "cm"),plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))
        graph<-mynewgraph + scale_x_discrete(breaks=pd1my_breaks, labels=label_fill(pd1my_breaks, mod=12))+ scale_fill_manual("",label=c("Deaths reported current year","Deaths reported previous year"),values=c("cyan","darkGreen"))
        mygraph<-graph+guides(fill = guide_legend(reverse=T))
        mygraph+annotate("text",x=30, y=20,label='atop(bold("2015-16"))',parse=TRUE)+
          annotate("text",x=30, y=19,label=pd1total16)+
          annotate("text",x=75, y=20,label='atop(bold("2016-17"))',parse=TRUE)+
          annotate("text",x=75, y=19,label=pd1total17)+
          annotate("text",x=120, y=23,label='atop(bold("2017-18"))',parse=TRUE)+
          annotate("text",x=120, y=22,label=pd1total18) +
          annotate("text",x=175, y=20,label='atop(bold("2018-19"))',parse=TRUE) +
          annotate("text",x=175, y=19,label=pd1total19) 

      }
      else if (graphtype=="Influenza-like illness"){
         
        p<-ggplot() +ggtitle("Percentage of visits for Influenza-like illness(ILI) Reported by \n the U.S. Outpatient Influenza-like illness Surveillance Network(ILINet),\nWeekly National Summary,2018-2019 and Selected Previous Seasons ")
        x<-p+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
        w<-x+labs( x="Week", y="% of visits for ILI")+coord_cartesian(ylim = c(0,8))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
        mynewgraph<-w+theme(legend.key = element_blank(),legend.justification=c(1,1),legend.position=c(1,1),legend.background = element_rect(linetype = 1, size = 0.3, colour = "black"),legend.spacing.y = unit(-0.3, "cm"),plot.title = element_text(lineheight=.8,hjust=0.5,size=11))
        graph<-mynewgraph + scale_x_discrete(breaks=ld1my_breaks, labels=label_fill(ld1my_breaks, mod=2))+ scale_y_continuous(breaks=seq(0,8,1))
        grapha<-graph+geom_line(ld1data,mapping = aes(x =ld1data$x, y = ld1$Year2017_18,group=1,color="A"))+geom_line(ld1data,mapping = aes(x =ld1data$x, y = ld1$Year2016_17,group=2,color="B"))+geom_line(ld1data,mapping = aes(x =ld1data$x, y = ld1$Year2015_16,group=1,color="C"))
        graph1<-grapha+geom_line(ld1data,mapping = aes(x =ld1data$x, y = ld1$Year2014_15,group=1,color="D"))+geom_line(ld1data,mapping = aes(x =ld1data$x, y = ld1$Year2011_12,group=1,color="E"))+geom_line(ld1data,mapping = aes(x =ld1data$x, y = ld1$Year2009_10,group=1,color="F"))
        graph2<-graph1+geom_line(ld1data,mapping = aes(x =ld1data$x, y = ld1$National.Baseline,group=1,color="G"),linetype = 2,size=1)+geom_line(ld1datan,mapping = aes(x =ld1datan$x, y = ld1datanew$Year2018_19,group=1,color="H"))+geom_point(aes(x =ld1datan$x, y = ld1datanew$Year2018_19),color="red4",shape=17,size=2)
        graph2+scale_color_manual(name="",labels=c("2017-18","2016-17","2015-16","2014-15","2011-12","2009-10","National Baseline","2018-19"),values=c("A"="deepskyblue1","B"="darkblue","C"="orange","D"="pink","E"="lawngreen","F"="black","G"="black","H"="red")) 
        
      }
     else if (graphtype=="CDC Flu heat map of USA"){
       
       
       ggplot(data=stateData, aes(map_id = region)) + 
         geom_map(aes(fill = level), map = us_map,colour='white')+
         scale_fill_manual(values = c("white","green4", "green3", "green2","green", "greenyellow","yellow", "orange","red1", "red2", "red3"),labels = c("Insufficient Data","Minimal", "Minimal", "Minimal","Low", "Low", "Moderate","Moderate", "High","High", "High"))+
         theme_bw()+
         expand_limits(x = us_map$long, y = us_map$lat)+
         ggtitle("2018-19 Influenza Season Week 7 ending Feb 16, 2019")+
         theme(plot.title = element_text(size = 15, face = "bold"))
       
       
     }
})
}
# Run the application 
shinyApp(ui = ui, server = server)

