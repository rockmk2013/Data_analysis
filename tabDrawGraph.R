
draw_hourly <- function(hourly,var){
  #date
 
  hourly%>%
    ggplot(aes_string("Date",var))+
    geom_point(pch=20,col="steel blue")+
    geom_smooth(se=F,col="red")+
    theme_bw()+
    theme(axis.text = element_text(family = "BL"))+
    xlab("")
  ggsave(paste0(var,"date.png"),width=7.2,height=2.8)
  #box
  
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

  hourly %>%
    ggplot(aes_string("Time",var))+
    geom_point(col="grey")+
    stat_summary(lwd=1.5, geom="line",aes(group = holiday,color=holiday))+
    scale_color_manual(values=c("#00CACA", "#ff7575"))+
    theme_bw()+
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      axis.text = element_text(family = "BL"),
      legend.text = element_text(family="BL")
    )
  ggsave(paste0(var,"time.png"),width=7.2,height=2.8)
  #bubble
  
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
  
}
draw_daily <- function(daily,var){
  #partition
 
  daily%>%
    ggplot(aes_string("Date",var))+
    geom_bar(stat="identity",aes(fill= holiday),col="white")+
    scale_fill_manual(values=c("#00CACA", "#ff7575"))+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.text = element_text(family = "BL"),
          legend.text = element_text(family = "BL"),
          legend.title = element_blank())+
    scale_x_date(date_breaks = "1 month")
  ggsave(paste0(var,"partition.png"),width=8,height=2.8)
  #weekday
  
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
 
  time_variables<-as.POSIXlt(daily$Date)
  daily$MonthofYear<-time_variables$mon+1
  daily$MonthofYear<-as.factor(month.abb[daily$MonthofYear])
  daily$MonthofYear<-factor(daily$MonthofYear,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
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
  
}

data_output <- function(hourly,daily){
  
  selection <- c("holiday","Time","InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV")
  selectionday <- c("holiday","InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV","PotentialShopper","Avg.ShopperDwell","RepeatCustomer")
  selectionweek <- c("Weekday","InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV","PotentialShopper","Avg.ShopperDwell","RepeatCustomer")
  
  hour = hourly %>% group_by(holiday,Time) %>% dplyr::select(one_of(selection)) %>% summarise_all(funs(mean))
  hoursum  = hourly %>% group_by(holiday) %>% dplyr::select(one_of(selectionday)) %>% summarise_all(funs(mean))
  week = hourly %>% group_by(Weekday) %>% dplyr::select(one_of(selectionweek)) %>% summarise_all(funs(mean))

  write.table(hour,file = "time_dataoutput.csv",fileEncoding  = "big5",row.names = FALSE,sep=",")
  write.table(hoursum,file = "holiday_dataoutput.csv",fileEncoding  = "big5",row.names = FALSE,sep=",")
  write.table(week,file = "weekday_dataoutput.csv",fileEncoding  = "big5",row.names = FALSE,sep=",")
 
}

draw_all <- function(hourly,daily){
  
  hour_data =c("InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV")
  day_data  =c("InstoreTraffic","StorefrontTraffic","TrafficConversion","Revenue","Transaction","SalesConversion","ATV","UPT","ACV","PotentialShopper","Avg.ShopperDwell","RepeatCustomer")
  
  for(i in hour_data){

     draw_hourly(hourly,i)
  }
  for(i in day_data){
     draw_daily(daily,i)
  }
  
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
  
  draw_all(single,daily)
  data_output(single,daily)
}

output$DrawAlert <- renderText({
  if(is.null(input$single)){
    paste("請輸入資料!") 
  }else{
    paste("請按鈕下載圖片!跑圖需要一陣子，請喝口水稍待片刻!")
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
  filename = paste0(filename_return(),".zip"),
  content = function(fname) {
    withProgress( value = 20,detail = 'This might take some time..',{
      main()
      setwd("c:/Users/asus/Documents/graph")
      print(filename_return())
      zip(zipfile=fname, files=filename_return())
      
    })
    do.call(unlink, list(filename_return(),recursive = T))
  }
)