
draw_hourly <- function(hourly,var,fs){
  #date
  fs <- c(fs,paste0(var,"date.png"))
  hourly%>%
    ggplot(aes_string("Date",var))+geom_point(pch=20,col="steel blue")+geom_smooth(se=F,col="red")+theme_bw()+xlab("")
  ggsave(paste0(var,"date.png"),width=7.2,height=2.8)
  #box
  fs <- c(fs,paste0(var,"box.png"))
  hourly$Weekday<-factor(hourly$Weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  hourly %>%
    ggplot(aes_string("Weekday",var))+
    geom_boxplot(varwidth = T, fill="plum")+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),
      legend.position = 'hide'
    )
  ggsave(paste0(var,"box.png"),width=7.2,height=2.8)
  #time
  fs <- c(fs,paste0(var,"time.png"))
  hourly %>%
    ggplot(aes_string("Time",var))+
    geom_point(col="grey")+
    stat_summary(lwd=1.5, geom="line",aes(group = holiday,col = holiday))+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank()
    )
  ggsave(paste0(var,"time.png"),width=7.2,height=2.8)
  #bubble
  fs <- c(fs,paste0(var,"bubble.png"))
  formula=paste0("mean(",var,")")
  hourly%>%
    group_by(Time,Weekday)%>%
    summarise_(Mean=formula)%>%
    ggplot(aes(Time,Weekday))+geom_point(col="tomato 2",aes(size=Mean))+
    ggtitle(paste("Bubble plot of",var))+theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'hide',
          plot.title = element_text(hjust = 0.5))
  ggsave(paste0(var,"bubble.png"),width=7.2,height=2.8)
  #weather
  fs <- c(fs,paste0(var,"weather.png"))
  hourly%>%
    filter(Weather!="na")%>%
    group_by(Weather)%>%
    summarise_(Mean=formula)%>%
    ggplot(aes(reorder(Weather, -Mean),Mean))+geom_bar(stat = "identity",aes(fill=Weather))+ggtitle(paste(var,"by weather"))+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank())
  ggsave(paste0(var,"weather.png"),width=7.2,height=2.8)
  return(fs)
}
draw_daily <- function(daily,var,fs){
  #partition
  fs <- c(fs,paste0(var,"partition.png"))
  daily%>%
    ggplot(aes_string("Date",var))+geom_bar(stat="identity",col="black",aes(fill= holiday))+
    theme_bw()+theme(axis.title.x = element_blank(),
                     legend.title = element_blank())
  ggsave(paste0(var,"partition.png"),width=7.2,height=2.8)
  #weekday
  fs <- c(fs,paste0(var,"weekday.png"))
  daily$Weekday<-factor(daily$Weekday,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  daily %>%
    ggplot(aes_string("Weekday",var))+
    geom_point(aes(col=Weekday))+
    stat_summary( colour="#99d8c9",lwd=1, geom="line",aes(group = 1))+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),
      legend.position = 'hide'
    )
  ggsave(paste0(var,"weekday.png"),width=7.2,height=2.8)
  #month
  fs <- c(fs,paste0(var,"month.png"))
  time_variables<-as.POSIXlt(daily$Date)
  daily$MonthofYear<-time_variables$mon+1
  daily$MonthofYear<-as.factor(month.abb[daily$MonthofYear])
  daily$MonthofYear<-factor(daily$MonthofYear,levels = c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sept","Oct","Nov","Dec"))
  daily %>%
    ggplot(aes_string("MonthofYear",var))+
    geom_point(aes(col=MonthofYear))+
    stat_summary( colour="#99d8c9",lwd=1, geom="line",aes(group = 1))+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),
      legend.position = 'hide'
    )
  ggsave(paste0(var,"month.png"),width=7.2,height=2.8)
  return(fs)
}

data_output <- function(hourly,daily,fs){
  fs <- c( "Time_dataoutput.csv","Holiday_dataoutput.csv","Weekday_dataoutput.csv")
  
  selection <- c("holiday","Time","InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV")
  selectionday <- c("holiday","InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV","PotentialShopper","Avg.ShopperDwell","RepeatCustomer")
  selectionweek <- c("Weekday","InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV","PotentialShopper","Avg.ShopperDwell","RepeatCustomer")
  
  hour = hourly %>% group_by(holiday,Time) %>% dplyr::select(one_of(selection)) %>% summarise_all(funs(mean))
  hoursum  = hourly %>% group_by(holiday) %>% dplyr::select(one_of(selectionday)) %>% summarise_all(funs(mean))
  week = hourly %>% group_by(Weekday) %>% dplyr::select(one_of(selectionweek)) %>% summarise_all(funs(mean))

  write.table(hour,file = "time_dataoutput.csv",fileEncoding  = "big5",row.names = FALSE,sep=",")
  write.table(hoursum,file = "holiday_dataoutput.csv",fileEncoding  = "big5",row.names = FALSE,sep=",")
  write.table(week,file = "weekday_dataoutput.csv",fileEncoding  = "big5",row.names = FALSE,sep=",")
  return(fs)
}

draw_all <- function(hourly,daily,fs){
  
  hour_data =c("InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV")
  day_data  =c("InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV","PotentialShopper","Avg.ShopperDwell","RepeatCustomer")
  
  for(i in hour_data){

    fs = draw_hourly(hourly,i,fs)
  }
  for(i in day_data){
    fs = draw_daily(daily,i,fs)
  }
  return(fs)
}

#main function
main <- function(){
  #單店
  single <- input$single
  #建立資料夾名稱
  filename = paste0(head(strsplit(single$name,split=".",fixed=T)[[1]],1),"(allgraph)")
  #讀取
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  single = single[,1:23]
  #多店
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  daily = daily[,1:23]
  #DataProcessing
  daily$Date<-as.Date(daily$Date,"%Y/%m/%d")
  single$Date<-as.Date(single$Date,"%Y/%m/%d")
  daily$holiday<-as.factor(ifelse(daily$NormalVacation==1|daily$SpecialVacation==1|daily$ConsistentVacation==1,"假日","平日"))
  single$holiday<-as.factor(ifelse(single$NormalVacation==1|single$SpecialVacation==1|single$ConsistentVacation==1,"假日","平日"))
  single$Time = as.factor(single$Time)
  #資料夾
  setwd("c:/Users/asus/Documents/graph")
  dir.create(filename)
  setwd(paste0("c:/Users/asus/Documents/graph/",filename))
  
  # write.table(single,file = "hourly.csv",fileEncoding  = "big5",row.names = FALSE,sep=",")
  # write.table(daily,file = "daily.csv",fileEncoding  = "big5",row.names = FALSE,sep=",")
  fs <- c()
  fs= c(draw_all(single,daily,fs),data_output(single,daily,fs))
  return(fs)
}

output$DrawAlert <- renderText({
  if(is.null(input$single)){
    paste0("請輸入資料!") 
  }else{
    main()
    paste("準備完成，請按鈕下載圖片!")
  }
})
filename_return <- reactive({
  #單店
  single <- input$single
  #建立資料夾名稱
  filename = paste0(head(strsplit(single$name,split=".",fixed=T)[[1]],1),"(allgraph)")
  return(filename)
})

output$downloadGraph_all <- downloadHandler(
  filename = 'download_graph_all.zip',
  content = function(fname) {
    # tmpdir <- tempdir()
    # setwd(tempdir())
    # print (tempdir())
    # 
    # fs = main()
    # 
    # print (fs)
    setwd("c:/Users/asus/Documents/graph")
    print(filename_return())
    zip(zipfile=fname, files=filename_return())
    do.call(unlink, list(filename_return(),recursive = T))
  }
  
)