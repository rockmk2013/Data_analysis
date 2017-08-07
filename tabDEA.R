library(shiny)


dea_return <- reactive({
  # Read data
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  # multi$Revenue = as.numeric(gsub(",","",as.character(multi$Revenue)))
  # multi$ATV = as.numeric(gsub(",","",as.character(multi$ATV)))
  
  #刪去商品櫃資料
  multi = multi[,1:23]
  #開始
  multidata <- findmean(multi)
  multi_all_mean <- multidata$multistore_dea
  multi_workingday_mean <- multidata$multistore_workingday
  multi_holiday_mean <- multidata$multistore_holiday
  
  all <- dea_calculate(multi, multi_all_mean)
  workingday <- dea_calculate(multi, multi_workingday_mean)
  holiday <- dea_calculate(multi, multi_holiday_mean)
  list("all" = all,
       "workingday" = workingday,
       "holiday" = holiday)
  
})

dea_calculate <- function(multi,multi_mean){
  # mean_table
  mean_table <- multi_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  rownames(mean_table) <- NULL
  store_name <- mean_table[,1]
  multi_mean <- data.frame(apply(multi_mean[,-1],2,function(x) x / mean(x)))
  X=matrix(multi_mean$MeanInstore,ncol=1)
  Y=cbind(multi_mean$MeanRevenue,multi_mean$MeanTransaction)
  # frontier
  # frontier <- dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="vrs",lwd=3)
  # frontier <- frontier + dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed")
  # 
  # store_full
  crs <- 1 / eff(dea(X,Y,RTS = "crs",ORIENTATION = "out"))
  vrs <- 1 / eff(dea(X,Y,RTS = "vrs",ORIENTATION = "out"))
  store_full <- data.frame(cbind(store_name,crs,vrs))
  colnames(store_full) <- c("StoreName","CRS","VRS")
  rownames(store_full) <- NULL
  #p <- recordPlot()
  # multi_cv_plot
  store_full_new <- melt(store_full, id.vars = 'StoreName')
  store_full_new$value <- as.numeric(store_full_new$value)
  cv_plot <- ggplot(store_full_new, aes(x=as.factor(StoreName), y=value, fill=variable)) +
    geom_bar(stat='identity', position='dodge') + xlab("店名") +
    ylab("效率值")+ scale_y_continuous(breaks=seq(0,1,0.1)) + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=12,family = "BL"),
          axis.text.y = element_text(size=12,family = "BL"),
          axis.title  = element_text(size=16,family = "BL"),
          legend.title = element_text(family = "BL")
    ) +
    scale_fill_discrete(name = "方法")
  return(  list("mean_table" = mean_table,
                #"frontier" = p,
                "store_full" = store_full,
                "cv_plot" = cv_plot))
  
}

storesummary <- function(df, group, selection){
  return(df %>% group_by_(group) %>% dplyr::select(one_of(selection)) %>% summarise_all(funs(mean)))
}

findmean <- function(multi){
  group <- "Store"
  selection <- c("Store","InstoreTraffic","Revenue","Transaction")
  multistore_dea <- storesummary(multi, group, selection)
  
  colnames(multistore_dea) <- c("Storename", "MeanInstore", "MeanRevenue", "MeanTransaction")
  
  multi_workingday <- filter(multi,SpecialVacation !=1,ConsistentVacation !=1, NormalVacation !=1)
  multi_holiday <- filter(multi,SpecialVacation ==1 | ConsistentVacation ==1 | NormalVacation ==1) 
  
  multistore_workingday <- storesummary(multi_workingday, group, selection)
  multistore_holiday <- storesummary(multi_holiday, group, selection)
  
  colnames(multistore_workingday) <- c("Storename", "MeanInstore", "MeanRevenue", "MeanTransaction")
  colnames(multistore_holiday) <-c("Storename", "MeanInstore", "MeanRevenue", "MeanTransaction")
  output <- list(multistore_dea = multistore_dea, multistore_workingday = multistore_workingday, multistore_holiday = multistore_holiday)
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




output$downloadData_all <- downloadHandler(
  filename = 'DEA_graph_all.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("cv_plot.png","frontier.png")
    
    multi_cv_plot <- dea_return()[["all"]][["cv_plot"]]
    ggsave("cv_plot.png",width = 7.2,height = 2.8)
    #Read data
    multi <- input$multi
    multi <- read.table(multi$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    multi = multi[,1:23]
    
    multi$Revenue = as.numeric(gsub(",","",as.character(multi$Revenue)))
    multi$ATV = as.numeric(gsub(",","",as.character(multi$ATV)))
    multidata <- findmean(multi)
    multi_all_mean <- multidata$multistore_dea
    
    # mean_table
    mean_table <- multi_all_mean
    mean_table[,-1] <- round(mean_table[,-1], digits = 2)
    rownames(mean_table) <- NULL
    store_name <- sort(unique(multi$Store))
    
    multi_all_mean <- data.frame(apply(multi_all_mean[,-1],2,function(x) x / mean(x)))
    X=matrix(multi_all_mean$MeanInstore,ncol=1)
    Y=cbind(multi_all_mean$MeanRevenue,multi_all_mean$MeanTransaction)
    # frontier
    png("frontier.png",width = 700,height=300)
    dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="vrs",lwd=3,family = "BL")
    dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
    dev.off()
    
    print (fs)
    
    zip(zipfile=fname, files=fs)
    
  }
  
)
output$downloadData_working <- downloadHandler(
  filename = 'DEA_graph_working.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("cv_plot.png","frontier.png")
    
    multi_cv_plot <- dea_return()[["all"]][["cv_plot"]]
    ggsave("cv_plot.png",width = 7.2,height = 2.8)
    #Read data
    multi <- input$multi
    multi <- read.table(multi$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    multi = multi[,1:23]
    
    multi$Revenue = as.numeric(gsub(",","",as.character(multi$Revenue)))
    multi$ATV = as.numeric(gsub(",","",as.character(multi$ATV)))
    multidata <- findmean(multi)
    multi_all_mean <- multidata$multistore_dea
    
    # mean_table
    mean_table <- multi_all_mean
    mean_table[,-1] <- round(mean_table[,-1], digits = 2)
    rownames(mean_table) <- NULL
    store_name <- sort(unique(multi$Store))
    multi_all_mean <- data.frame(apply(multi_all_mean[,-1],2,function(x) x / mean(x)))
    X=matrix(multi_all_mean$MeanInstore,ncol=1)
    Y=cbind(multi_all_mean$MeanRevenue,multi_all_mean$MeanTransaction)
    # frontier
    png("frontier.png",width = 700,height=300)
    dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="vrs",lwd=3,family = "BL")
    dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
    dev.off()
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)
output$downloadData_weekend <- downloadHandler(
  filename = 'DEA_graph_weekend.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("cv_plot.png","frontier.png")
    
    multi_cv_plot <- dea_return()[["all"]][["cv_plot"]]
    ggsave("cv_plot.png",width = 7.2,height = 2.8)
    #Read data
    multi <- input$multi
    multi <- read.table(multi$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    multi = multi[,1:23]
    
    multi$Revenue = as.numeric(gsub(",","",as.character(multi$Revenue)))
    multi$ATV = as.numeric(gsub(",","",as.character(multi$ATV)))
    multidata <- findmean(multi)
    multi_all_mean <- multidata$multistore_dea
    
    # mean_table
    mean_table <- multi_all_mean
    mean_table[,-1] <- round(mean_table[,-1], digits = 2)
    rownames(mean_table) <- NULL
    store_name <- sort(unique(multi$Store))
    multi_all_mean <- data.frame(apply(multi_all_mean[,-1],2,function(x) x / mean(x)))
    X=matrix(multi_all_mean$MeanInstore,ncol=1)
    Y=cbind(multi_all_mean$MeanRevenue,multi_all_mean$MeanTransaction)
    # frontier
    png("frontier.png",width = 700,height=300)
    dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="vrs",lwd=3,family = "BL")
    dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
    dev.off()
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)
output$multi_frontier_plot = renderPlot({
  # Read data
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  multi = multi[,1:23]
  
  multi$Revenue = as.numeric(gsub(",","",as.character(multi$Revenue)))
  multi$ATV = as.numeric(gsub(",","",as.character(multi$ATV)))
  multidata <- findmean(multi)
  multi_all_mean <- multidata$multistore_dea
  
  # mean_table
  mean_table <- multi_all_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  
  store_name <- sort(unique(multi$Store))
  multi_all_mean <- data.frame(apply(multi_all_mean[,-1],2,function(x) x / mean(x)))
  X=matrix(multi_all_mean$MeanInstore,ncol=1)
  Y=cbind(multi_all_mean$MeanRevenue,multi_all_mean$MeanTransaction)
  # frontier
  dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="vrs",lwd=3)
  dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed")
  
  # 
  # frontier <- dea_return()[["all"]][["frontier"]]
  # print(frontier)
})

output$multi_workingday_mean = renderDataTable({
  mean_table <- dea_return()[["workingday"]][["mean_table"]]
  #print(DT::datatable(mean_table, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_workingday_cv = renderDataTable({
  store_full <- dea_return()[["workingday"]][["store_full"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_workingday_cv_plot = renderPlot({
  multi_cv_plot <- dea_return()[["workingday"]][["cv_plot"]]
  print(multi_cv_plot)
})

output$multi_workingday_frontier_plot = renderPlot({
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  multi = multi[,1:23]
  
  multi$Revenue = as.numeric(gsub(",","",as.character(multi$Revenue)))
  multi$ATV = as.numeric(gsub(",","",as.character(multi$ATV)))
  multidata <- findmean(multi)
  multi_workingday_mean <- multidata$multistore_workingday
  
  # mean_table
  mean_table <- multi_workingday_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  
  store_name <- sort(unique(multi$Store))
  multi_workingday_mean <- data.frame(apply(multi_workingday_mean[,-1],2,function(x) x / mean(x)))
  X=matrix(multi_workingday_mean$MeanInstore,ncol=1)
  Y=cbind(multi_workingday_mean$MeanRevenue,multi_workingday_mean$MeanTransaction)
  # frontier
  dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="vrs",lwd=3,family = "BL")
  dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
  
  # 
  #   frontier <- dea_return()[["weekday"]][["frontier"]]
  #   print(frontier)
})

output$multi_holiday_mean = renderDataTable({
  mean_table <- dea_return()[["holiday"]][["mean_table"]]
  #print(DT::datatable(mean_table, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_holiday_cv = renderDataTable({
  store_full <- dea_return()[["holiday"]][["store_full"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$multi_holiday_cv_plot = renderPlot({
  multi_cv_plot <- dea_return()[["holiday"]][["cv_plot"]]
  print(multi_cv_plot)
})

output$multi_holiday_frontier_plot = renderPlot({
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  multi = multi[,1:23]
  
  multi$Revenue = as.numeric(gsub(",","",as.character(multi$Revenue)))
  multi$ATV = as.numeric(gsub(",","",as.character(multi$ATV)))
  multidata <- findmean(multi)
  multi_holiday_mean <- multidata$multistore_holiday

  # mean_table
  mean_table <- multi_holiday_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  
  store_name <- sort(unique(multi$Store))
  multi_holiday_mean <- data.frame(apply(multi_holiday_mean[,-1],2,function(x) x / mean(x)))
  X=matrix(multi_holiday_mean$MeanInstore,ncol=1)
  Y=cbind(multi_holiday_mean$MeanRevenue,multi_holiday_mean$MeanTransaction)
  # frontier
  dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="vrs",lwd=3,family = "BL")
  dea.plot.frontier(X,Y,txt=store_name,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")

  # frontier <- dea_return()[["weekend"]][["frontier"]]
  # print(frontier)
})
