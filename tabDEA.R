library(shiny)
library(Benchmarking)
library(ggplot2)
library(dplyr)
library(reshape2)

dea_return <- reactive({
  # Read data
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multidata <- findmean(multi)
  multi_all_mean <- multidata$multistore_dea
  multi_weekday_mean <- multidata$multistore_weekday
  multi_weekend_mean <- multidata$multistore_weekend
  all <- dea_calculate(multi, multi_all_mean)
  weekday <- dea_calculate(multi, multi_weekday_mean)
  weekend <- dea_calculate(multi, multi_weekend_mean)
  list("all" = all,
       "weekday" = weekday,
       "weekend" = weekend)
  
})

dea_calculate <- function(multi,multi_mean){
  # mean_table
  mean_table <- multi_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  rownames(mean_table) <- NULL
  store_name <- unique(multi[,1])
  multi_mean <- data.frame(apply(multi_mean[,-1],2,function(x) x / mean(x)))
  X=matrix(multi_mean$mean_instore,ncol=1)
  Y=cbind(multi_mean$mean_sales,multi_mean$mean_transaction)
  # frontier
  frontier <- dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="vrs",lwd=3)
  frontier <- frontier + dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed")
  # store_full
  crs <- 1 / eff(dea(X,Y,RTS = "crs",ORIENTATION = "out"))
  vrs <- 1 / eff(dea(X,Y,RTS = "vrs",ORIENTATION = "out"))
  store_full <- data.frame(cbind(store_name,crs,vrs))
  colnames(store_full) <- c("StoreName","CRS","VRS")
  rownames(store_full) <- NULL
  p <- recordPlot()
  # multi_cv_plot
  store_full_new <- melt(store_full, id.vars = 'StoreName')
  store_full_new$value <- as.numeric(store_full_new$value)
  cv_plot <- ggplot(store_full_new, aes(x=as.factor(StoreName), y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge') + xlab("店名") +
    ylab("效率值")+ scale_y_continuous(breaks=seq(0,1,0.1)) + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.title=element_text(size=16)
          ) +
          scale_fill_discrete(name = "方法")
  
  return(  list("mean_table" = mean_table,
                "frontier" = p,
                "store_full" = store_full,
                "cv_plot" = cv_plot))
  
}

storesummary <- function(df, group, selection){
  return(df %>% group_by_(group) %>% dplyr::select(one_of(selection)) %>% summarise_all(funs(mean)))
}

findmean <- function(multi){
  group <- "StoreNumber"
  selection <- c("StoreNumber","InstoreTraffic","Revenue","Transaction")
  multistore_dea <- storesummary(multi, group, selection)
  storename <- as.character(unique(multi$StoreNumber))
  multistore_dea[,1] <- storename
  colnames(multistore_dea) <- c("store_name", "mean_instore", "mean_sales", "mean_transaction")
  multi_weekday <- multi[multi$Weekday !='Sunday' & multi$Weekday !='Saturday',]
  multi_weekend <- multi[multi$Weekday =='Sunday' | multi$Weekday =='Saturday',]
  multistore_weekday <- storesummary(multi_weekday, group, selection)
  multistore_weekend <- storesummary(multi_weekend, group, selection)
  multistore_weekday[,1] <- storename
  multistore_weekend[,1] <- storename
  colnames(multistore_weekday) <- c("store_name", "mean_instore", "mean_sales", "mean_transaction")
  colnames(multistore_weekend) <- c("store_name", "mean_instore", "mean_sales", "mean_transaction")
  output <- list(multistore_dea = multistore_dea, multistore_weekday = multistore_weekday, multistore_weekend = multistore_weekend)
  return(output)
}


output$multi_mean = renderDataTable({
  mean_table <- dea_return()[["all"]][["mean_table"]]
  #print(DT::datatable(mean_table, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_cv = renderDataTable({
  store_full <- dea_return()[["all"]][["store_full"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_cv_plot = renderPlot({
  multi_cv_plot <- dea_return()[["all"]][["cv_plot"]]
  print(multi_cv_plot)
})

output$multi_frontier_plot = renderPlot({
  frontier <- dea_return()[["all"]][["frontier"]]
  print(frontier)
})

output$multi_weekday_mean = renderDataTable({
  mean_table <- dea_return()[["weekday"]][["mean_table"]]
  #print(DT::datatable(mean_table, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_weekday_cv = renderDataTable({
  store_full <- dea_return()[["weekday"]][["store_full"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_weekday_cv_plot = renderPlot({
  multi_cv_plot <- dea_return()[["weekday"]][["cv_plot"]]
  print(multi_cv_plot)
})

output$multi_weekday_frontier_plot = renderPlot({

  frontier <- dea_return()[["weekday"]][["frontier"]]
  print(frontier)
})

output$multi_weekend_mean = renderDataTable({
  mean_table <- dea_return()[["weekend"]][["mean_table"]]
  #print(DT::datatable(mean_table, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_weekend_cv = renderDataTable({
  store_full <- dea_return()[["weekend"]][["store_full"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_weekend_cv_plot = renderPlot({
  multi_cv_plot <- dea_return()[["weekend"]][["cv_plot"]]
  print(multi_cv_plot)
})

output$multi_weekend_frontier_plot = renderPlot({
  
  frontier <- dea_return()[["weekend"]][["frontier"]]
  print(frontier)
})
