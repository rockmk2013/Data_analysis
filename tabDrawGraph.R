library(readr)
library(dplyr)

draw_graph_hour <- function(table,names,x,meanall,meanweekday,meanweekend,week,weekend,holiday){
  
  #周天圖
  plot(table$WeekNumber,x,las=1,pch=20,col="#7B7B7B")
  lines(meanall,col="red",lwd=5)
  
  #泡泡圖
  symbols(table$Time,
          table$WeekNumber,
          circle=x,
          inches = 0.15,
          bg= "purple"
  )
  
  #三種天
  plot(table$Time,x,las=1,col="gray")
  lines(week,col="blue",lwd=5)
  lines(weekend,col="orange",lwd=5)
  lines(holiday,col="red",lwd=5)
  legend("topright",                                # 表示在右上角
         pch = 20,                                   # pch代表點的圖案
         col = c("blue","orange","red"),           # col代表顏色
         bty="n",#消除邊框
         legend = c("weekday", "weekend","holiday") # 顏色所對應的名稱
  )
  
  
}

draw_graph_daily <- function(table,names,x,meanall_day){
  
  #周天BAR圖
  barplot((table$Weekday =="Sunday" |table$Weekday =="Saturday" )*x,col='orange')
  barplot((table$Weekday !="Sunday" &table$Weekday !="Saturday" )*x,col='blue',add=TRUE)
  barplot((table$consistent_vacation ==1 |table$special_vacation == 1 )*x,col='red',add=TRUE)
  
  #周天平均
  plot(table$WeekNumber,x,las=1,pch=20,col="#7B7B7B",ann=FALSE)
  lines(meanall_day,col="red",lwd=5)
  
}

getmeans <- function(dataset,dataset_day){
  #分周間周末
  DATA_week    = dataset[dataset$Weekday !='Sunday' & dataset$Weekday !='Saturday',]
  DATA_weekend = dataset[dataset$Weekday =='Sunday' | dataset$Weekday =='Saturday',]
  
  #算七天平均
  meanper = c()
  meaninstore=c()
  meanstorefront = c()
  meanratio = c()
  
  meanmember=c()
  meanmembertime=c()
  
  meansales=c()
  meanatv=c()
  meanconversion=c()
  meantransaction=c()
  meanupt= c()
  
  meanper_day = c()
  meaninstore_day=c()
  meanstorefront_day=c()
  meanratio_day=c()
  
  meansales_day=c()
  meanatv_day=c()
  meanupt_day=c()
  meanconversion_day=c()
  meantransaction_day=c()
  
  #算周間周末平均
  
  meanper_week = c()
  meanper_weekend = c()
  
  meaninstore_week=c()
  meaninstore_weekend=c()
  
  meanstorefront_week=c()
  meanstorefront_weekend=c()
  
  meanratio_week=c()
  meanratio_weekend=c()
  
  meanratio_1to2_week=c()
  meanratio_1to2_weekend=c()
  
  
  meanfloor_2_week=c()
  meanfloor_2_weekend=c()
  
  
  meansales_week    = c()
  meansales_weekend = c()
  
  meantransaction_week = c()
  meantransaction_weekend = c()
  
  meanatv_week =c ()
  meanatv_weekend = c()
  
  meanupt_week =c ()
  meanupt_weekend = c()
  
  meanconversion_week= c()
  meanconversion_weekend =c()
  
  # meanmember_week= c()
  # meanmember_weekend =c()
  # 
  # meanmembertime_week= c()
  # meanmembertime_weekend =c()
  
  
  for(j in 1:7){
    
    meanper[j]=mean(dataset$per[which(dataset$WeekNumber==j)])
    meaninstore[j]=mean(dataset$InstoreTraffic[which(dataset$WeekNumber==j)])
    meanstorefront[j]=mean(dataset$StorefrontTraffic[which(dataset$WeekNumber==j)])
    meanratio[j]=mean(dataset$WindowsConversion[which(dataset$WeekNumber==j)])
    
    meanmember[j]=mean(dataset$EfficientVisit[which(dataset$WeekNumber==j)])
    meanmembertime[j]=mean(dataset$VisitDuration[which(dataset$WeekNumber==j)])
    
    meansales[j]=mean(dataset$Revenue[which(dataset$WeekNumber==j)])
    meantransaction[j]=mean(dataset$Transaction[which(dataset$WeekNumber==j)])
    meanatv[j]=mean(dataset$ATV[which(dataset$WeekNumber==j)])
    meanconversion[j]=mean(dataset$Conversion[which(dataset$WeekNumber==j)])
    meanupt[j]=mean(dataset$UPT[which(dataset$WeekNumber==j)])
    
    meanper_day[j]= mean(dataset_day$per[which(dataset_day$WeekNumber==j)])
    meaninstore_day[j]=mean(dataset_day$InstoreTraffic[which(dataset_day$WeekNumber==j)])
    meanstorefront_day[j]=mean(dataset_day$StorefrontTraffic[which(dataset_day$WeekNumber==j)])
    meanratio_day[j]=mean(dataset_day$WindowsConversion[which(dataset_day$WeekNumber==j)])
    
    meansales_day[j]=mean(dataset_day$Revenue[which(dataset_day$WeekNumber==j)])
    meantransaction_day[j]=mean(dataset_day$Transaction[which(dataset_day$WeekNumber==j)])
    meanatv_day[j]=mean(dataset_day$ATV[which(dataset_day$WeekNumber==j)])
    meanupt_day[j]=mean(dataset_day$UPT[which(dataset_day$WeekNumber==j)])
    meanconversion_day[j]=mean(dataset_day$Conversion[which(dataset_day$WeekNumber==j)])
    
  }
  for(l in min(dataset[[4]]): max(dataset[[4]])){
    
    meanper_week[l]   =mean(DATA_week$per[which(DATA_week$Time==l)])
    meanper_weekend[l]=mean(DATA_weekend$per[which(DATA_weekend$Time==l)])
    
    meaninstore_week[l]   =mean(DATA_week$InstoreTraffic[which(DATA_week$Time==l)])
    meaninstore_weekend[l]=mean(DATA_weekend$InstoreTraffic[which(DATA_weekend$Time==l)])
    
    meanstorefront_week[l]   =mean(DATA_week$StorefrontTraffic[which(DATA_week$Time==l)])
    meanstorefront_weekend[l]=mean(DATA_weekend$StorefrontTraffic[which(DATA_weekend$Time==l)])
    
    meanratio_week[l]   =mean(DATA_week$WindowsConversion[which(DATA_week$Time==l)])
    meanratio_weekend[l]=mean(DATA_weekend$WindowsConversion[which(DATA_weekend$Time==l)])
    
    meansales_week[l] =mean(DATA_week$Revenue[which(DATA_week$Time==l)])
    meansales_weekend[l]=mean(DATA_weekend$Revenue[which(DATA_weekend$Time==l)])
    
    meantransaction_week[l]   =mean(DATA_week$Transaction[which(DATA_week$Time==l)])
    meantransaction_weekend[l]=mean(DATA_weekend$Transaction[which(DATA_weekend$Time==l)])
    
    meanatv_week[l]   =mean(DATA_week$ATV[which(DATA_week$Time==l)])
    meanatv_weekend[l]=mean(DATA_weekend$ATV[which(DATA_weekend$Time==l)])
    
    meanupt_week[l]   =mean(DATA_week$UPT[which(DATA_week$Time==l)])
    meanupt_weekend[l]=mean(DATA_weekend$UPT[which(DATA_weekend$Time==l)])
    
    meanconversion_week[l]   =mean(DATA_week$Conversion[which(DATA_week$Time==l)])
    meanconversion_weekend[l]=mean(DATA_weekend$Conversion[which(DATA_weekend$Time==l)])
    
  }
  
  #輸出TABLE
  meandata_day = data.frame(c(1:7),meanper_day,meaninstore_day,meanstorefront_day,meanratio_day,meansales_day,meantransaction_day,meanupt_day,meanatv_day,meanconversion_day,meanmember,meanmembertime)
  meandata_all = data.frame(mean(per),mean(InstoreTraffic),mean(StorefrontTraffic),mean(WindowsConversion),mean(Revenue),mean(Transaction),mean(UPT),mean(ATV),mean(Conversion),mean(EfficientVisit),mean(VisitDuration))
  
  
  #---分周間周末假日------
  DATA_week_three    = dataset[dataset$Weekday !='Sunday' & dataset$Weekday !='Saturday' & dataset$consistent_vacation ==0 & dataset$special_vacation ==0 ,]
  DATA_weekend_three = dataset[dataset$Weekday =='Sunday' | dataset$Weekday =='Saturday' ,]-++++-
  DATA_weekend_three = DATA_weekend[DATA_weekend$consistent_vacation ==0 & DATA_weekend$special_vacation ==0 ,]
  DATA_holiday_three = dataset[dataset$consistent_vacation==1 |dataset$special_vacation==1,]
  
  per_holiday = c()
  per_week = c()
  per_weekend =c()
  
  instore_holiday = c()
  instore_week = c()
  instore_weekend =c()
  
  front_holiday = c()
  front_week = c()
  front_weekend =c()
  
  ratio_week   = c()
  ratio_weekend= c()
  ratio_holiday = c()
  
  revenue_holiday = c()
  revenue_week = c()
  revenue_weekend = c()
  
  trans_holiday = c()
  trans_week = c()
  trans_weekend = c()
  
  atv_holiday = c()
  atv_week = c()
  atv_weekend = c()
  
  upt_holiday = c()
  upt_week = c()
  upt_weekend = c()
  
  convert_holiday = c()
  convert_week = c()
  convert_weekend = c()
  
  for(i in min(dataset[[4]]): max(dataset[[4]]) ){
    per_holiday[i] = mean(DATA_holiday_three $per[which(DATA_holiday_three $Time==i)])
    per_week[i]    = mean(DATA_week_three $per[which(DATA_week_three $Time==i)])
    per_weekend[i] = mean(DATA_weekend_three $per[which(DATA_weekend_three $Time==i)])
    
    instore_holiday[i] = mean(DATA_holiday_three $InstoreTraffic[which(DATA_holiday_three $Time==i)])
    instore_week[i]    = mean(DATA_week_three $InstoreTraffic[which(DATA_week_three $Time==i)])
    instore_weekend[i] = mean(DATA_weekend_three $InstoreTraffic[which(DATA_weekend_three $Time==i)])
    
    front_holiday[i] = mean(DATA_holiday_three $StorefrontTraffic[which(DATA_holiday_three $Time==i)])
    front_week[i]    = mean(DATA_week_three $StorefrontTraffic[which(DATA_week_three $Time==i)])
    front_weekend[i] = mean(DATA_weekend_three $StorefrontTraffic[which(DATA_weekend_three $Time==i)])
    
    revenue_holiday[i] = mean(DATA_holiday_three $Revenue[which(DATA_holiday_three $Time==i)])
    revenue_week[i]    = mean(DATA_week_three $Revenue[which(DATA_week_three $Time==i)])
    revenue_weekend[i] = mean(DATA_weekend_three $Revenue[which(DATA_weekend_three $Time==i)])
    
    trans_holiday[i] = mean(DATA_holiday_three $Transaction[which(DATA_holiday_three $Time==i)])
    trans_week[i]    = mean(DATA_week_three $Transaction[which(DATA_week_three $Time==i)])
    trans_weekend[i] = mean(DATA_weekend_three $Transaction[which(DATA_weekend_three $Time==i)])
    
    convert_holiday[i] = mean(DATA_holiday_three $Conversion[which(DATA_holiday_three $Time==i)])
    convert_week[i]    = mean(DATA_week_three $Conversion[which(DATA_week_three $Time==i)])
    convert_weekend[i] = mean(DATA_weekend_three $Conversion[which(DATA_weekend_three $Time==i)])
    
    atv_holiday[i] = mean(DATA_holiday_three $ATV[which(DATA_holiday_three $Time==i)])
    atv_week[i]    = mean(DATA_week_three $ATV[which(DATA_week_three $Time==i)])
    atv_weekend[i] = mean(DATA_weekend_three $ATV[which(DATA_weekend_three $Time==i)])
    
    upt_holiday[i] = mean(DATA_holiday_three $UPT[which(DATA_holiday_three $Time==i)])
    upt_week[i]    = mean(DATA_week_three $UPT[which(DATA_week_three $Time==i)])
    upt_weekend[i] = mean(DATA_weekend_three $UPT[which(DATA_weekend_three $Time==i)])
    
    ratio_holiday[i] = mean(DATA_holiday_three $WindowsConversion[which(DATA_holiday_three $Time==i)])
    ratio_week[i]    = mean(DATA_week_three $WindowsConversion[which(DATA_week_three $Time==i)])
    ratio_weekend[i] = mean(DATA_weekend_three $WindowsConversion[which(DATA_weekend_three $Time==i)])
    
  }
  
  
  
  meanall_hour = list(NA,NA,NA,NA,meaninstore,meanstorefront,meanratio,NA,meansales,meantransaction,meanconversion,meanatv,meanupt,NA,NA,meanper)
  meanall_week = list(NA,NA,NA,NA,meaninstore_week,meanstorefront_week,meanratio_week,NA,meansales_week,meantransaction_week,meanconversion_week,meanatv_week,meanupt_week,NA,NA,meanper_week)
  meanall_weekend = list(NA,NA,NA,NA,meaninstore_weekend,meanstorefront_weekend,meanratio_weekend,NA,meansales_weekend,meantransaction_weekend,meanconversion_weekend,meanatv_weekend,meanupt_weekend,NA,NA,meanper_weekend)
  meanall_day  = list(NA,NA,NA,NA,meaninstore_day,meanstorefront_day,meanratio_day,NA,meansales_day,meantransaction_day,meanconversion_day,meanatv_day,meanupt_day,meanmember,meanmembertime,meanper_day)
  
  meanthree_week    =list(NA,NA,NA,NA,instore_week,front_week,ratio_week,NA,revenue_week,trans_week,convert_week,atv_week,upt_week,NA,NA,per_week)
  meanthree_weekend =list(NA,NA,NA,NA,instore_weekend,front_weekend,ratio_weekend,NA,revenue_weekend,trans_weekend,convert_weekend,atv_weekend,upt_weekend,NA,NA,per_weekend)
  meanthree_holiday =list(NA,NA,NA,NA,instore_holiday,front_holiday,ratio_holiday,NA,revenue_holiday,trans_holiday,convert_holiday,atv_holiday,upt_holiday,NA,NA,per_holiday)
  
  
  all = list(meanall_hour,meanall_week,meanall_weekend,meanall_day,meanthree_week,meanthree_weekend,meanthree_holiday)
  all
}

draw_all <- function(dataset,dataset_day){
  hour =c(5,6,7,9,10,11,12,13,16)
  daily=c(5,6,7,9,10,11,12,13,14,15,16)
  
  for(i in hour){
    draw_graph_hour(dataset,colnames(dataset)[i],dataset[[i]],getmeans(dataset,dataset_day)[[1]][[i]],getmeans(dataset,dataset_day)[[2]][[i]],getmeans(dataset,dataset_day)[[3]][[i]],getmeans(dataset,dataset_day)[[5]][[i]],getmeans(dataset,dataset_day)[[6]][[i]],getmeans(dataset,dataset_day)[[7]][[i]])
  }
  for(i in daily){
    draw_graph_daily(dataset_day,colnames(dataset_day)[i],dataset_day[[i]],getmeans(dataset,dataset_day)[[4]][[i]])
  }
}

draw_all(dataset,dataset_day)

