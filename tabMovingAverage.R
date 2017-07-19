library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)

output$revenue_hourly = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  ggplot(single, aes(InstoreTraffic, Revenue)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=10),axis.text.y = element_text(size=14),
            axis.title=element_text(size=16))
})

output$transaction_hourly = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  ggplot(single, aes(InstoreTraffic, Transaction)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=10),axis.text.y = element_text(size=14),
          axis.title=element_text(size=16))
})

output$revenue_daily = renderPlot({
  
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  ggplot(daily, aes(InstoreTraffic, Revenue)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=10),axis.text.y = element_text(size=14)
  ,axis.title=element_text(size=16))
  })

output$transaction_daily = renderPlot({
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  ggplot(daily, aes(InstoreTraffic, Transaction)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=10),axis.text.y = element_text(size=14)
          ,axis.title=element_text(size=16))
})


output$Month_of_year = renderPlot({
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  time_variables<-as.POSIXlt(daily$Date)
  daily$MonthofYear<-time_variables$mon+1
  daily$MonthofYear<-as.factor(month.abb[daily$MonthofYear])
  
  levels(daily$MonthofYear)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  a<-daily %>%
    ggplot(aes(MonthofYear,Conversion))+
    geom_point(aes(col=MonthofYear))+
    stat_summary(fun.Conversion=mean, colour="grey",lwd=1.5, geom="line",aes(group = 1))+
    theme_bw()+
    theme(
      axis.title=element_text(size=16),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16)
    )
  
  b<-daily %>%
    ggplot(aes(MonthofYear,InstoreTraffic))+
    geom_point(aes(col=MonthofYear))+
    stat_summary(fun.InstoreTraffic=mean, colour="grey",lwd=1.5, geom="line",aes(group = 1))+
    theme_bw()+
    theme(
      axis.title=element_text(size=16),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16)
    )
  
  c<-daily %>%
    ggplot(aes(MonthofYear,Revenue))+
    geom_point(aes(col=MonthofYear))+
    stat_summary(fun.Revenue=mean, colour="grey",lwd=1.5, geom="line",aes(group = 1))+
    theme_bw()+
    theme(
      axis.title=element_text(size=16),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16)
    )
  
  print(grid.arrange(a,b,c))
  
  })