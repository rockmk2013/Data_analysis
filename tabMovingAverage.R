output$revenue_hourly = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  single = single[,1:23]
  
  ggplot(single, aes(InstoreTraffic, Revenue)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    scale_x_continuous(breaks = seq(0,round(max(single$InstoreTraffic))*10^(nchar(as.character(max(single$InstoreTraffic)))-1),10^(nchar(as.character(max(single$InstoreTraffic)))-1))/2)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=10),axis.text.y = element_text(size=14),
            axis.title=element_text(size=16))
})

output$transaction_hourly = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  single = single[,1:23]
  
  ggplot(single, aes(InstoreTraffic, Transaction)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    scale_x_continuous(breaks = seq(0,round(max(single$InstoreTraffic))*10^(nchar(as.character(max(single$InstoreTraffic)))-1),10^(nchar(as.character(max(single$InstoreTraffic)))-1))/2)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=10),axis.text.y = element_text(size=14),
          axis.title = element_text(size=16))
})

output$revenue_daily = renderPlot({
  
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  daily = daily[,1:23]
  
  ggplot(daily, aes(InstoreTraffic, Revenue)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    scale_x_continuous(breaks = seq(0,round(max(daily$InstoreTraffic))*10^(nchar(as.character(max(daily$InstoreTraffic)))-1),10^(nchar(as.character(max(daily$InstoreTraffic)))-1)/5))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=8),axis.text.y = element_text(size=14)
  ,axis.title=element_text(size=16))
  })

output$transaction_daily = renderPlot({
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  daily = daily[,1:23]
  
  ggplot(daily, aes(InstoreTraffic, Transaction)) + 
    geom_point(pch=21,fill="#7B7B7B") +
    geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                span=0.3, se=FALSE)+
    scale_x_continuous(breaks = seq(0,round(max(daily$InstoreTraffic))*10^(nchar(as.character(max(daily$InstoreTraffic)))-1),10^(nchar(as.character(max(daily$InstoreTraffic)))-1)/5))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=8),axis.text.y = element_text(size=14)
          ,axis.title=element_text(size=16))
})


output$Month_of_year = renderPlot({
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  daily = daily[,1:23]
  
  time_variables<-as.POSIXlt(daily$Date)
  daily$MonthofYear<-time_variables$mon+1
  daily$MonthofYear<-as.factor(month.abb[daily$MonthofYear])
  
  daily$MonthofYear<-factor(daily$MonthofYear,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  
  a<-daily %>%
    ggplot(aes(MonthofYear,SalesConversion))+
    geom_point(aes(col=MonthofYear))+
    stat_summary(fun.SalesConversion=mean, colour="grey",lwd=1.5, geom="line",aes(group = 1))+
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

month_return <- reactive({
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  daily = daily[,1:23]
  
  time_variables<-as.POSIXlt(daily$Date)
  daily$MonthofYear<-time_variables$mon+1
  daily$MonthofYear<-as.factor(month.abb[daily$MonthofYear])
  
  daily$MonthofYear<-factor(daily$MonthofYear,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  daily$holiday<-as.factor(ifelse(daily$NormalVacation==1|daily$SpecialVacation==1|daily$ConsistentVacation==1,"假日","平日"))
  daily
})

output$Month_of_year_instore = renderDataTable({
  daily = month_return()
  daily %>% group_by(MonthofYear,holiday) %>% summarise(mean(InstoreTraffic))
})
output$Month_of_year_revenue = renderDataTable({
  daily = month_return()
  daily %>% group_by(MonthofYear,holiday) %>% summarise(mean(Revenue))
})
output$Month_of_year_salesconversion = renderDataTable({
  daily = month_return()
  daily %>% group_by(MonthofYear,holiday) %>% summarise(mean(SalesConversion))
})
  
output$downloadData_TAH <- downloadHandler(
  filename = 'TAH_graph.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("TAH_revenue.png","TAH_transaction.png")
    
    single <- input$single
    single <- read.table(single$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    single = single[,1:23]
    
    ggplot(single, aes(InstoreTraffic, Revenue)) + 
      geom_point(pch=21,fill="#7B7B7B") +
      geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                  span=0.3, se=FALSE)+
      scale_x_continuous(breaks = seq(0,round(max(single$InstoreTraffic))*10^(nchar(as.character(max(single$InstoreTraffic)))-1),10^(nchar(as.character(max(single$InstoreTraffic)))-1))/2)+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=10),axis.text.y = element_text(size=14),
            axis.title=element_text(size=16))
    ggsave("TAH_revenue.png",width=7,height=3)
    
    ggplot(single, aes(InstoreTraffic, Transaction)) + 
      geom_point(pch=21,fill="#7B7B7B") +
      geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                  span=0.3, se=FALSE)+
      scale_x_continuous(breaks = seq(0,round(max(single$InstoreTraffic))*10^(nchar(as.character(max(single$InstoreTraffic)))-1),10^(nchar(as.character(max(single$InstoreTraffic)))-1))/2)+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=10),axis.text.y = element_text(size=14),
            axis.title = element_text(size=16))
    ggsave("TAH_transaction.png",width=7,height=3)
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)
output$downloadData_TAD <- downloadHandler(
  filename = 'TAD_graph.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("TAD_revenue.png","TAD_transaction.png")
    
    daily <- input$daily
    daily <- read.table(daily$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    daily = daily[,1:23]
    
    ggplot(daily, aes(InstoreTraffic, Revenue)) + 
      geom_point(pch=21,fill="#7B7B7B") +
      geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                  span=0.3, se=FALSE)+
      scale_x_continuous(breaks = seq(0,round(max(daily$InstoreTraffic))*10^(nchar(as.character(max(daily$InstoreTraffic)))-1),10^(nchar(as.character(max(daily$InstoreTraffic)))-1)/5))+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=8),axis.text.y = element_text(size=14)
            ,axis.title=element_text(size=16))
    ggsave("TAD_revenue.png",width=7,height=3)
    
    ggplot(daily, aes(InstoreTraffic, Transaction)) + 
      geom_point(pch=21,fill="#7B7B7B") +
      geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", 
                  span=0.3, se=FALSE)+
      scale_x_continuous(breaks = seq(0,round(max(daily$InstoreTraffic))*10^(nchar(as.character(max(daily$InstoreTraffic)))-1),10^(nchar(as.character(max(daily$InstoreTraffic)))-1)/5))+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=8),axis.text.y = element_text(size=14)
            ,axis.title=element_text(size=16))
    ggsave("TAD_transaction.png",width=7,height=3)
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)
output$downloadData_TAM <- downloadHandler(
  filename = 'TAM_graph.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("TAM_conversion.png","TAM_instore.png","TAM_revenue.png")
    
    daily <- input$daily
    daily <- read.table(daily$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    daily = daily[,1:23]
    
    time_variables<-as.POSIXlt(daily$Date)
    daily$MonthofYear<-time_variables$mon+1
    daily$MonthofYear<-as.factor(month.abb[daily$MonthofYear])
    
    daily$MonthofYear<-factor(daily$MonthofYear,levels = c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sept","Oct","Nov","Dec"))
    
    a<-daily %>%
      ggplot(aes(MonthofYear,SalesConversion))+
      geom_point(aes(col=MonthofYear))+
      stat_summary(fun.SalesConversion=mean, colour="grey",lwd=1.5, geom="line",aes(group = 1))+
      theme_bw()+
      theme(
        axis.title=element_text(size=16),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16)
      )
    ggsave("TAM_conversion.png",width=7,height=3)
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
    ggsave("TAM_instore.png",width=7,height=3)
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
    
    
    ggsave("TAM_revenue.png",width=7,height=3)
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)