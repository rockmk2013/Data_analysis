#抓取比例函數
catch_after_point = function(data,threshold){
  data = transform_monthofyear(data[,1:23])
  console = data %>% 
    mutate(thresholdtype = if_else(InstoreTraffic<threshold,0,1))  %>% 
    group_by(thresholdtype) %>% 
    summarize(count=n()) %>% 
    mutate(freq = round(count / sum(count),2)) %>%
    filter(thresholdtype==1) %>%
    select_("Count"="count","Freq"="freq")
  return(console)
}
#計算比例
caculate_propotion_hourly <- reactive({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  if(input$goButton_hourly){
    threshold=as.numeric(input$threshold_hourly)
     catchpoint = catch_after_point(single,threshold)
     
     console1 = single %>% 
       mutate(holiday=if_else(SpecialVacation==1|ConsistentVacation==1|NormalVacation==1,1,0))%>% 
       filter(InstoreTraffic > threshold) %>% 
       group_by("是否為假日" = holiday) %>%
       summarize(小時數=n()) %>% 
       mutate(比例 = round(小時數 / sum(小時數),2)) 
     
     console2 = single %>% 
       mutate(holiday=if_else(SpecialVacation==1|ConsistentVacation==1|NormalVacation==1,1,0))%>% 
       filter(InstoreTraffic > threshold) %>% 
       group_by("是否為假日" = holiday,"小時"=Time) %>%
       summarize(小時數=n()) %>% 
       mutate(比例 = round(小時數 / sum(小時數),2)) 
     
     list("catchpoint" = catchpoint,
          "console1" = console1,
          "console2" = console2)
  }
})

output$threshold_hourly_console = renderText({
  if(!is.null(caculate_propotion_hourly())){
    threshold = input$threshold_hourly
    count = caculate_propotion_hourly()[["catchpoint"]][[1]]
    freq = caculate_propotion_hourly()[["catchpoint"]][[2]]
    console = paste("大於",threshold,"人有",count,"小時，佔",round(freq,3))
    console 
  }
})

output$hourly_threshold_table_holiday = renderDataTable({
  caculate_propotion_hourly()[["console1"]]
})
output$hourly_threshold_table_time = renderDataTable({
  caculate_propotion_hourly()[["console2"]]
})

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
#計算比例
caculate_propotion_daily <- reactive({
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  if(input$goButton_daily){
    threshold=as.numeric(input$threshold_daily)
    catchpoint_day = catch_after_point(daily,threshold)
    
    console_day = daily%>% 
      mutate(holiday=if_else(SpecialVacation==1|ConsistentVacation==1|NormalVacation==1,1,0))%>% 
      filter(InstoreTraffic > threshold) %>% 
      group_by("是否為假日" = holiday) %>%
      summarize(天數=n()) %>% 
      mutate(比例 = round(天數 / sum(天數),2)) 
    list("catchpoint_day" = catchpoint_day,
         "console_day" = console_day)
  }
})
output$threshold_daily_console = renderText({
  if(!is.null(caculate_propotion_daily())){
    threshold = input$threshold_daily
    count = caculate_propotion_daily()[["catchpoint_day"]][[1]]
    freq = caculate_propotion_daily()[["catchpoint_day"]][[2]]
    console = paste("大於",threshold,"人有",count,"天，佔",round(freq,3))
    console 
  }
})
output$daily_threshold_table_holiday = renderDataTable({
  caculate_propotion_daily()[["console_day"]]
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
  daily = transform_monthofyear(daily[,1:23])
  
  a<-daily %>%
    filter(SalesConversion>0) %>% 
    ggplot(aes(MonthofYear,Revenue))+
    geom_point(aes(col=MonthofYear))+
    stat_summary(fun.Revenue=mean, colour="grey",lwd=1.5, geom="line",aes(group = 1))+
    theme_bw()+
    theme(
      axis.title=element_text(size=16),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16)
    )
  b<-daily %>%
    filter(SalesConversion>0) %>% 
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
    filter(SalesConversion>0) %>% 
    ggplot(aes(MonthofYear,SalesConversion))+
    geom_point(aes(col=MonthofYear))+
    stat_summary(fun.SalesConversion=mean, colour="grey",lwd=1.5, geom="line",aes(group = 1))+
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
  
  daily = transform_monthofyear(daily[,1:23])
  daily$holiday<-as.factor(ifelse(daily$NormalVacation==1|daily$SpecialVacation==1|daily$ConsistentVacation==1,"假日","平日"))
  daily
})

output$Month_of_year_overview = renderDataTable({
  daily = month_return()
  daily %>% group_by("月份"=MonthofYear) %>% summarise("平均店外人數"=mean(StorefrontTraffic),"平均進店人數"=mean(InstoreTraffic),"平均進店率"=sum(InstoreTraffic)/sum(StorefrontTraffic),"平均提袋率"=sum(Transaction)/sum(InstoreTraffic),"平均客單價"=sum(Revenue)/sum(Transaction),"平均客件數"=sum(Transaction*UPT)/sum(Transaction),"平均交易量"=mean(Transaction),"平均營業額"=mean(Revenue))
  
})
output$Month_of_year_instore = renderDataTable({
  daily = month_return()
  daily %>% group_by("月份"=MonthofYear,"平假日"=holiday) %>% summarise("平均進店人數"=mean(InstoreTraffic))
})
output$Month_of_year_revenue = renderDataTable({
  daily = month_return()
  daily %>% group_by("月份"=MonthofYear,"平假日"=holiday) %>% summarise("平均營業額"=mean(Revenue))
})
output$Month_of_year_salesconversion = renderDataTable({
  daily = month_return()
  daily %>% group_by("月份"=MonthofYear,"平假日"=holiday) %>% filter(SalesConversion>0) %>% summarise("平均進店率"=sum(InstoreTraffic)/sum(StorefrontTraffic))
})

#抓取比例函數
catch_after_point_compare = function(data1,data2,threshold){
  data_compare = rbind(data1[,1:20] ,data2[,1:20])
  data1 = transform_monthofyear(data1[,1:20])
  data2 = transform_monthofyear(data2[,1:20])
  data_compare = transform_monthofyear(data_compare)
  data1type = paste0(data1$MonthofYear[1],"~",data1$MonthofYear[length(data1$MonthofYear)])
  data2type = paste0(data2$MonthofYear[1],"~",data2$MonthofYear[length(data2$MonthofYear)])
  
  console_compare = data_compare %>% 
    mutate(monthtype=if_else(MonthofYear %in% data1$MonthofYear,data1type,data2type),thresholdtype = if_else(InstoreTraffic<threshold,0,1))  %>% 
    group_by(monthtype,thresholdtype) %>% 
    summarize(count=n()) %>% 
    mutate(freq = count / sum(count)) %>% 
    filter(thresholdtype==1) %>% 
    select_("Month"="monthtype","Count"="count","Freq"="freq")
  return(console_compare)
}
#計算比例
output$threshold_compare_console = renderText({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  
  if(is.null(input$compare_data_hourly)){
  }else if(input$goButton_compare){
    compare <- input$compare_data_hourly
    compare <- read.table(compare$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
    
    threshold_compare = input$threshold_compare
    threshold=as.numeric(input$threshold_compare)
    final= catch_after_point_compare(single,compare,threshold)
    
    Month = final[[1]]
    count_compare = final[[2]]
    freq_compare = final[[3]]
    paste(Month,"月份大於",threshold_compare,"人有",count_compare,"小時，佔",round(freq_compare,3),"/")
  }
})
transform_monthofyear = function(data){
  data$Date =  as.Date(data$Date)
  time_variables<-as.POSIXlt(data$Date)
  data$MonthofYear<-time_variables$mon+1
  data$MonthofYear<-as.factor(month.abb[data$MonthofYear])
  data$MonthofYear<-factor(data$MonthofYear,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  return(data)
}

draw_trend_compare = function(data1,data2,variable){
  data_compare = rbind(data1[,1:20] ,data2[,1:20])
  data1 = transform_monthofyear(data1[,1:20])
  data2 = transform_monthofyear(data2[,1:20])
  data_compare = transform_monthofyear(data_compare)
  data1type = paste0(data1$MonthofYear[1],"~",data1$MonthofYear[length(data1$MonthofYear)])
  data2type = paste0(data2$MonthofYear[1],"~",data2$MonthofYear[length(data2$MonthofYear)])

  trend_compare = data_compare %>%
    mutate(區間=if_else(MonthofYear %in% data1$MonthofYear,data1type,data2type))  %>%
    ggplot(aes_string("InstoreTraffic",variable))+
    geom_point(aes(col=區間))+
    geom_smooth(aes(col=區間),size=1.5)+
    theme_bw()
  return(trend_compare)
}

output$revenue_compare = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  if (is.null(input$compare_data_hourly)) {
    
  }else{
    compare <- input$compare_data_hourly
    compare <- read.table(compare$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
    
    draw_trend_compare(single,compare,"Revenue")
  }
})

output$transaction_compare = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  if (is.null(input$compare_data_hourly)) {
    
  }else{
    compare <- input$compare_data_hourly
    compare <- read.table(compare$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
    
    draw_trend_compare(single,compare,"Transaction")
  }
})  

output$acv_compare = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  if (is.null(input$compare_data_hourly)) {
    
  }else{
    compare <- input$compare_data_hourly
    compare <- read.table(compare$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
    
    draw_trend_compare(single,compare,"ACV")
  }
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
output$downloadData_Compare <- downloadHandler(
  filename = 'TACompare_graph.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("revenue_compare.png","transaction_compare.png")
    
    single <- input$single
    single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
    if (is.null(input$compare_data_hourly)) {
      
    }else{
      compare <- input$compare_data_hourly
      compare <- read.table(compare$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
      
      draw_trend_compare(single,compare,"Revenue")
      ggsave("revenue_compare.png",width=10,height=5)
      draw_trend_compare(single,compare,"Transaction")
      ggsave("transaction_compare.png",width=10,height=5)
    }
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)