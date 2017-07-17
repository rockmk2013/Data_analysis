library(readr)
library(dplyr)
library(ggplot2)

output$revenue_hourly = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  ggplot(single, aes(InstoreTraffic, Revenue)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    theme_bw()
})

output$transaction_hourly = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  ggplot(single, aes(InstoreTraffic, Transaction)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    theme_bw()
})

output$revenue_daily = renderPlot({
  
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  ggplot(daily, aes(InstoreTraffic, Revenue)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    theme_bw()
})

output$transaction_daily = renderPlot({
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  
  ggplot(daily, aes(InstoreTraffic, Transaction)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    theme_bw()
})
