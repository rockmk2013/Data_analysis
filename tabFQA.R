library(shiny)
library(dplyr)

fqa_return <- reactive({
  # Read data
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multidata <- findfqamean(multi)
  #set data
  multiconsole = data.frame(multidata)
  #
   fqa<-draw_fqa(multiconsole)
   list("fqa" = fqa)
  
})
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
draw_fqa <- function(multi){
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
      
      plot(slicedata[[i]]$multistore_week.mean_instore_week,
           slicedata[[i]]$multistore_week.mean_sales_week,
           pch=16,
           col=colortype[i],
           xlim=c(0,x_max),
           ylim=c(0,y_max),
           ann = FALSE,
           lwd=8,
           las=1)
      points(slicedata[[i]]$multistore_week.mean_instore_weekend,
             slicedata[[i]]$multistore_week.mean_sales_weekend,
             pch=17,
             col=colortype[i],
             lwd=8)
      abline(v=x_max/2,h=y_max/2)
      legend("bottomright",                                # 表示在右上角
             pch = 16,                 # pch代表點的圖案
             bty="n",
             col = colortype,           # col代表顏色
             legend = storename # 顏色所對應的名稱
      )
      legend("topleft",                                # 表示在右上角
             pch = c(16,17),                 # pch代表點的圖案
             bty="n",
             legend = c("weekday","weekend")
      )
      # lines(slicedata[[i]]$multistore_week.mean_instore_week:slicedata[[i]]$multistore_week.mean_instore_weekend,
      #       slicedata[[i]]$multistore_week.mean_sales_week:slicedata[[i]]$multistore_week.mean_sales_weekend,
      #       col=colortype[i]
      # )
    }else{
      points(slicedata[[i]]$multistore_week.mean_instore_week,
             slicedata[[i]]$multistore_week.mean_sales_week,
             pch=16,
             col=colortype[i],
             lwd=8)
      points(slicedata[[i]]$multistore_week.mean_instore_weekend,
             slicedata[[i]]$multistore_week.mean_sales_weekend,
             pch=17,
             col=colortype[i],
             lwd=8)
      # lines(slicedata[[i]]$multistore_week.mean_instore_week:slicedata[[i]]$multistore_week.mean_instore_weekend,
      #       slicedata[[i]]$multistore_week.mean_sales_week:slicedata[[i]]$multistore_week.mean_sales_weekend,
      #       col=colortype[i]
      # )
    }
  }
  
}


output$multi_weekandweekend_plot = renderPlot({
  
  multi_weekandweekend_plot <- fqa_return()[["fqa"]]
  multi_weekandweekend_plot
  
  })
# 
# 

# 
# legend("downright",                                # 表示在右上角
#        pch = 16,                 # pch代表點的圖案
#        bty="n",
#        col = c("red","orange","brown","green","blue","black","purple"),           # col代表顏色 
#        legend = c("公館", "台中","台南","桃園","高雄","逢甲","新竹") # 顏色所對應的名稱
# )
# legend("topleft",                                # 表示在右上角
#        pch = c(16,17),                 # pch代表點的圖案
#        bty="n",
#        legend = c("weekday","weekend")  
# )
# abline(v=(mean(life8_multistore$ratio_weekend)*8+
#             mean(life8_multistore$ratio_week)*23)/31,
#        h=(mean(life8_multistore$title_conversion_weekend)*8+
#             mean(life8_multistore$title_conversion_week)*23)/31,
#        col="blue")
# 
# plot(life8_multistore$title_conversion_week,life8_multistore$ratio_week,col="orange",pch=20,lwd=5,xlim=c(0.05,0.25),ylim=c(0,0.15),las=1,ann=FALSE)
# points(life8_multistore$title_conversion_weekend,life8_multistore$ratio_weekend,col="blue",pch=20,lwd=5)
# legend(
#   "topleft",                                # 表示在右上角
#   pch = 16,                 # pch代表點的圖案
#   bty="n",
#   col=c("orange","blue"),
#   legend = c("weekday","weekend")  
# ) 

