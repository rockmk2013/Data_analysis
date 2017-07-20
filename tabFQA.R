library(shiny)

fqastoresummary <- function(df, group, selection){
  return(df %>% group_by_(group) %>% dplyr::select(one_of(selection)) %>% summarise_all(funs(mean)))
}
findfqamean <- function(multi){
  # Get mean of columns from multistore data grouped by store number
  group <- "StoreNumber"
  selection <- c("StoreNumber","InstoreTraffic","Revenue")
  storename <- as.character(unique(multi$StoreNumber))
  
  # Seperate multistore data into workingdays and holidays
  multi_workingday <- filter(multi,special_vacation !=1,consistent_vacation !=1,normal_vacation !=1)
  multi_holiday <- filter(multi,special_vacation ==1 | consistent_vacation ==1 | normal_vacation==1) 
  
  # Get mean of columns by weekdays and weekends grouped by store number
  multistore_workingday <- fqastoresummary(multi_workingday, group, selection)
  multistore_holiday <- fqastoresummary(multi_holiday, group, selection)
  multistore_weekdays <- cbind(storename,multistore_workingday[,-1], multistore_holiday[,-1])
  colnames(multistore_weekdays) <- c("storename","mean_instore_workingday", "mean_sales_workingday", "mean_instore_holiday", "mean_sales_holiday")
  
  output <- list (multistore_weekdays = multistore_weekdays)
  return(output)
}


output$multi_workingday_plot = renderPlot({
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multidata <- findfqamean(multi)
  #set data
  multi = data.frame(multidata)
  
  slicedata = list()
  storename = multi[,1]
  for( i in 1:length(storename)){
    slicedata[[i]] = multi %>% filter(multistore_weekdays.storename==storename[i])
  }
  #set limit
  x_min <- min(multi$multistore_weekdays.mean_instore_workingday)
  x_max <- max(multi$multistore_weekdays.mean_instore_holiday)
  
  y_min <- min(multi$multistore_weekdays.mean_sales_workingday)
  y_max <- max(multi$multistore_weekdays.mean_sales_holiday)
  
  #
  colortype = c("red","#80FFFF","brown","green","blue","black","purple","pink","yellow","#4F9D9D","#FF8040")
  for(i in 1:length(storename)){
    if(i==1){
      plot(slicedata[[i]]$multistore_weekdays.mean_instore_workingday,
           slicedata[[i]]$multistore_weekdays.mean_sales_workingday,
           pch=16,
           col=colortype[i],
           xlim=c(0,x_max),
           ylim=c(0,y_max),
           ann = FALSE,
           lwd=8,
           cex=3,
           las=1)
      abline(v=x_max/2,h=y_max/2)
      legend("bottomright",                                # 表示在右上角
             pch = 16,                 # pch代表點的圖案
             bty="n",
             col = colortype,           # col代表顏色
             legend = storename # 顏色所對應的名稱
      )
      
    }else{
      points(slicedata[[i]]$multistore_weekdays.mean_instore_workingday,
             slicedata[[i]]$multistore_weekdays.mean_sales_workingday,
             pch=16,
             col=colortype[i],
             cex=3,
             lwd=8)
      
    }
  }
  
})

output$multi_holiday_plot = renderPlot({
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multidata <- findfqamean(multi)
  #set data
  multi = data.frame(multidata)
  
  slicedata = list()
  storename = multi[,1]
  for( i in 1:length(storename)){
    slicedata[[i]] = multi %>% filter(multistore_weekdays.storename==storename[i])
  }
  #set limit
  x_min <- min(multi$multistore_weekdays.mean_instore_workingday)
  x_max <- max(multi$multistore_weekdays.mean_instore_holiday)
  
  y_min <- min(multi$multistore_weekdays.mean_sales_workingday)
  y_max <- max(multi$multistore_weekdays.mean_sales_holiday)
  
  #
  colortype = c("red","#80FFFF","brown","green","blue","black","purple","pink","yellow","#4F9D9D","#FF8040")
  for(i in 1:length(storename)){
    if(i==1){
      
      plot(slicedata[[i]]$multistore_weekdays.mean_instore_holiday,
           slicedata[[i]]$multistore_weekdays.mean_sales_holiday,
           pch=17,
           col=colortype[i],
           xlim=c(0,x_max),
           ylim=c(0,y_max),
           ann = FALSE,
           lwd=8,
           cex=3,
           las=1)
      abline(v=x_max/2,h=y_max/2)
      legend("bottomright",                                # 表示在右上角
             pch = 17,                 # pch代表點的圖案
             bty="n",
             col = colortype,           # col代表顏色
             legend = storename # 顏色所對應的名稱
      )
      
    }else{
      points(slicedata[[i]]$multistore_weekdays.mean_instore_holiday,
             slicedata[[i]]$multistore_weekdays.mean_sales_holiday,
             pch=17,
             col=colortype[i],
             cex=3,
             lwd=8)
      
    }
  }
  
})

output$multi_slope_plot = renderPlot({
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multidata <- findfqamean(multi)
  #set data
  multi = data.frame(multidata)
  #set graph data
  #slicedata = list()
  storename = multi[,1]
  
  instore_dif = multi$multistore_weekdays.mean_instore_holiday-multi$multistore_weekdays.mean_instore_workingday
  sales_dif = multi$multistore_weekdays.mean_sales_holiday-multi$multistore_weekdays.mean_sales_workingday
  slope = (sales_dif/instore_dif)
  slope = slope/mean(slope)
  slopedata = data.frame(storename,instore_dif,sales_dif,slope)
  
  ggplot(slopedata,aes(x=as.factor(storename),y=slope))+geom_bar(stat="identity",fill="#00BFC4")+
    ylab("成長率")+
    xlab("店名")+
    theme_bw()+
    theme(panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.title=element_text(size=16)
    )
})