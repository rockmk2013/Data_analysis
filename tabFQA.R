library(shiny)
library(dplyr)

fqastoresummary <- function(df, group, selection){
  return(df %>% group_by_(group) %>% dplyr::select(one_of(selection)) %>% summarise_all(funs(mean)))
}
findfqamean <- function(multi){
  # Get mean of columns from multistore data grouped by store number
  group <- "StoreNumber"
  selection <- c("StoreNumber","InstoreTraffic","Revenue")
  storename <- as.character(unique(multi$StoreNumber))
  
  # Seperate multistore data into weekdays and weekends
  multi_weekday <- multi[multi$Weekday !='Sunday' & multi$Weekday !='Saturday',]
  multi_weekend <- multi[multi$Weekday =='Sunday' | multi$Weekday =='Saturday',]
  
  # Get mean of columns by weekdays and weekends grouped by store number
  multistore_weekday <- fqastoresummary(multi_weekday, group, selection)
  multistore_weekend <- fqastoresummary(multi_weekend, group, selection)
  multistore_week <- cbind(storename,multistore_weekday[,-1], multistore_weekend[,-1])
  colnames(multistore_week) <- c("storename","mean_instore_week", "mean_sales_week", "mean_instore_weekend", "mean_sales_weekend")
  
  output <- list (multistore_week = multistore_week)
  return(output)
}


output$multi_week_plot = renderPlot({
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multidata <- findfqamean(multi)
  #set data
  multi = data.frame(multidata)
  
  slicedata = list()
  storename = multi[,1]
  for( i in 1:length(storename)){
    slicedata[[i]] = multi %>% filter(multistore_week.storename==storename[i])
  }
  #set limit
  x_min <- min(multi$multistore_week.mean_instore_week)
  x_max <- max(multi$multistore_week.mean_instore_weekend)
  
  y_min <- min(multi$multistore_week.mean_sales_week)
  y_max <- max(multi$multistore_week.mean_sales_weekend)
  
  #
  colortype = c("red","#80FFFF","brown","green","blue","black","purple","pink","yellow","#4F9D9D","#FF8040")
  for(i in 1:length(storename)){
    if(i==1){
      plot(slicedata[[i]]$multistore_week.mean_instore_week,
           slicedata[[i]]$multistore_week.mean_sales_week,
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
      points(slicedata[[i]]$multistore_week.mean_instore_week,
             slicedata[[i]]$multistore_week.mean_sales_week,
             pch=16,
             col=colortype[i],
             cex=3,
             lwd=8)
      
    }
  }
  
})

output$multi_weekend_plot = renderPlot({
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multidata <- findfqamean(multi)
  #set data
  multi = data.frame(multidata)
  
  #set graph data
  slicedata = list()
  storename = multi[,1]
  for( i in 1:length(storename)){
    slicedata[[i]] = multi %>% filter(multistore_week.storename==storename[i])
  }
  #set limit
  x_min <- min(multi$multistore_week.mean_instore_week)
  x_max <- max(multi$multistore_week.mean_instore_weekend)
  
  y_min <- min(multi$multistore_week.mean_sales_week)
  y_max <- max(multi$multistore_week.mean_sales_weekend)
  
  #
  colortype = c("red","#80FFFF","brown","green","blue","black","purple","pink","yellow","#4F9D9D","#FF8040")
  for(i in 1:length(storename)){
    if(i==1){
      
      plot(slicedata[[i]]$multistore_week.mean_instore_weekend,
           slicedata[[i]]$multistore_week.mean_sales_weekend,
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
      points(slicedata[[i]]$multistore_week.mean_instore_weekend,
             slicedata[[i]]$multistore_week.mean_sales_weekend,
             pch=17,
             col=colortype[i],
             cex=3,
             lwd=8)
      
    }
  }
  
})