library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

single_dea_return <- reactive({
  # Read data
  single <- input$single
  single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  singledata <- single_findmean(single)
  single_all_mean <- singledata$singlestore_dea
  single_weekday_mean <- singledata$singlestore_weekday
  single_weekend_mean <- singledata$singlestore_weekend
  all <- single_dea_calculate(single, single_all_mean)
  weekday <- single_dea_calculate(single, single_weekday_mean)
  weekend <- single_dea_calculate(single, single_weekend_mean)
  list("all" = all,
       "weekday" = weekday,
       "weekend" = weekend)
})

single_dea_calculate <- function(single,single_mean){
  # mean_table
  mean_table <- single_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  rownames(mean_table) <- NULL
  time <- unique(single$Time)
  single_mean <- data.frame(apply(single_mean[,-1],2,function(x) x / mean(x)))
  X=matrix(single_mean$mean_instore,ncol=1)
  Y=cbind(single_mean$mean_sales,single_mean$mean_transaction)
  # frontier
  frontier <- dea.plot.frontier(X,Y,txt=time,col="red", RTS="vrs",lwd=3)
  frontier <- frontier + dea.plot.frontier(X,Y,txt=time,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed")
  p <- recordPlot()
  # store_full
  crs <- 1 / eff(dea(X,Y,RTS = "crs",ORIENTATION = "out"))
  vrs <- 1 / eff(dea(X,Y,RTS = "vrs",ORIENTATION = "out"))
  store_full <- data.frame(cbind(time,crs,vrs))
  colnames(store_full) <- c("Time","CRS","VRS")
  rownames(store_full) <- NULL
  # single_cv_plot
  store_full_new <- melt(store_full, id.vars = 'Time')
  store_full_new$value <- as.numeric(store_full_new$value)
  cv_plot <- ggplot(store_full_new, aes(x=as.factor(Time), y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge') + xlab("時間") +
    ylab("效率值")+ scale_y_continuous(breaks=seq(0,1,0.1)) + 
    theme(plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name = "方法")
  
  return(  list("mean_table" = mean_table, 
                "frontier" = p,
                "store_full" = store_full,
                "cv_plot" = cv_plot))
}

storesummary <- function(df, group, selection){
  return(df %>% group_by_(group) %>% dplyr::select(one_of(selection)) %>% summarise_all(funs(mean)))
}

single_findmean <- function(single){
  group <- "Time"
  selection <- c("Time","InstoreTraffic","Revenue","Transaction")
  singlestore_dea <- storesummary(single, group, selection)
  time <- as.character(unique(single$Time))
  singlestore_dea[,1] <- time
  colnames(singlestore_dea) <- c("time", "mean_instore", "mean_sales", "mean_transaction")
  single_weekday <- single[single$Weekday !='Sunday' & single$Weekday !='Saturday',]
  single_weekend <- single[single$Weekday =='Sunday' | single$Weekday =='Saturday',]
  singlestore_weekday <- storesummary(single_weekday, group, selection)
  singlestore_weekend <- storesummary(single_weekend, group, selection)
  singlestore_weekday[,1] <- time
  singlestore_weekend[,1] <- time
  colnames(singlestore_weekday) <- c("time", "mean_instore", "mean_sales", "mean_transaction")
  colnames(singlestore_weekend) <- c("time", "mean_instore", "mean_sales", "mean_transaction")
  output <- list(singlestore_dea = singlestore_dea, singlestore_weekday = singlestore_weekday, singlestore_weekend = singlestore_weekend)
  return(output)
}


output$single_mean = renderDataTable({
  mean_table <- single_dea_return()[["all"]][["mean_table"]]
  print(DT::datatable(mean_table, options = list(searching = FALSE, paging = FALSE)))
})

output$single_cv = renderDataTable({
  store_full <- single_dea_return()[["all"]][["store_full"]]
  print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_cv_plot = renderPlot({
  single_cv_plot <- single_dea_return()[["all"]][["cv_plot"]]
  print(single_cv_plot)
})

output$single_frontier_plot = renderPlot({
  frontier <- single_dea_return()[["all"]][["frontier"]]
  print(frontier)
})

output$single_weekday_mean = renderDataTable({
  store_full <- single_dea_return()[["weekday"]][["mean_table"]]
  print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_weekday_cv = renderDataTable({
  store_full <- single_dea_return()[["weekday"]][["store_full"]]
  print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_weekday_cv_plot = renderPlot({
  single_cv_plot <- single_dea_return()[["weekday"]][["cv_plot"]]
  print(single_cv_plot)
})

output$single_weekday_frontier_plot = renderPlot({
  frontier <- single_dea_return()[["weekday"]][["frontier"]]
  print(frontier)
})

output$single_weekend_mean = renderDataTable({
  store_full <- single_dea_return()[["weekend"]][["mean_table"]]
  print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_weekend_cv = renderDataTable({
  store_full <- single_dea_return()[["weekend"]][["store_full"]]
  print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_weekend_cv_plot = renderPlot({
  single_cv_plot <- single_dea_return()[["weekend"]][["cv_plot"]]
  print(single_cv_plot)
})

output$single_weekend_frontier_plot = renderPlot({
  frontier <- single_dea_return()[["weekend"]][["frontier"]]
  print(frontier)
})
