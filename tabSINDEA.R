library(shiny)


single_dea_return <- reactive({
  # Read data
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  single = single [,1:23]
  
  single$Revenue = as.numeric(gsub(",","",as.character(single$Revenue)))
  single$ATV = as.numeric(gsub(",","",as.character(single$ATV)))
  singledata <- single_findmean(single)
  single_all_mean <- singledata$singlestore_dea
  single_workingday_mean <- singledata$singlestore_workingday
  single_holiday_mean <- singledata$singlestore_holiday
  all <- single_dea_calculate(single, single_all_mean)
  workingday <- single_dea_calculate(single, single_workingday_mean)
  holiday <- single_dea_calculate(single, single_holiday_mean)
  list("all" = all,
       "workingday" = workingday,
       "holiday" = holiday)
})

single_dea_calculate <- function(single,single_mean){
  # mean_table
  mean_table <- single_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  rownames(mean_table) <- NULL
  time <- mean_table[,1]
  single_mean <- data.frame(apply(single_mean[,-1],2,function(x) x / mean(x)))
  X=matrix(single_mean$mean_instore,ncol=1)
  Y=cbind(single_mean$mean_sales,single_mean$mean_transaction)
  # frontier
  # frontier <- dea.plot.frontier(X,Y,txt=time,col="red", RTS="vrs",lwd=3)
  # frontier <- frontier + dea.plot.frontier(X,Y,txt=time,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed")
  # p <- recordPlot()
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
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=12,family = "BL"),
          axis.text.y = element_text(size=12,family = "BL"),
          axis.title.x = element_text(size=16,family = "BL"),
          axis.title.y = element_text(size=16,family = "BL"),
          legend.title = element_text(family = "BL")) + 
    scale_fill_discrete(name = "方法")
  
  return(  list("mean_table" = mean_table, 
                # "frontier" = p,
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
  # time <- as.character(unique(single$Time))
  # singlestore_dea[,1] <- time
  colnames(singlestore_dea) <- c("time", "mean_instore", "mean_sales", "mean_transaction")
  
  single_workingday <- filter(single,SpecialVacation !=1,ConsistentVacation !=1,NormalVacation !=1)
  single_holiday <- filter(single,SpecialVacation !=1,ConsistentVacation !=1,NormalVacation !=1)
  
  singlestore_workingday <- storesummary(single_workingday, group, selection)
  singlestore_holiday <- storesummary(single_holiday, group, selection)
  # singlestore_workingday[,1] <- time
  # singlestore_holiday[,1] <- time
  colnames(singlestore_workingday) <- c("time", "mean_instore", "mean_sales", "mean_transaction")
  colnames(singlestore_holiday) <- c("time", "mean_instore", "mean_sales", "mean_transaction")
  output <- list(singlestore_dea = singlestore_dea, singlestore_workingday = singlestore_workingday, singlestore_holiday = singlestore_holiday)
  return(output)
}


output$single_mean = renderDataTable({
  mean_table <- single_dea_return()[["all"]][["mean_table"]]
  #print(DT::datatable(mean_table, options = list(searching = FALSE, paging = FALSE)))
})

output$single_cv = renderDataTable({
  store_full <- single_dea_return()[["all"]][["store_full"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_cv_plot = renderPlot({
  single_cv_plot <- single_dea_return()[["all"]][["cv_plot"]]
  print(single_cv_plot)
})

output$single_frontier_plot = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  single = single [,1:23]
  
  single$Revenue = as.numeric(gsub(",","",as.character(single$Revenue)))
  single$ATV = as.numeric(gsub(",","",as.character(single$ATV)))
  singledata <- single_findmean(single)
  single_all_mean <- singledata$singlestore_dea
  
  mean_table <-  single_all_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  rownames(mean_table) <- NULL
  time <- unique(single$Time)
  single_all_mean <- data.frame(apply( single_all_mean[,-1],2,function(x) x / mean(x)))
  X=matrix( single_all_mean$mean_instore,ncol=1)
  Y=cbind( single_all_mean$mean_sales, single_all_mean$mean_transaction)
  # frontier
  dea.plot.frontier(X,Y,txt=time,col="red", RTS="vrs",lwd=3,family = "BL")
  dea.plot.frontier(X,Y,txt=time,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
  # 
  # frontier <- single_dea_return()[["all"]][["frontier"]]
  # print(frontier)
})

output$single_workingday_mean = renderDataTable({
  store_full <- single_dea_return()[["workingday"]][["mean_table"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_workingday_cv = renderDataTable({
  store_full <- single_dea_return()[["workingday"]][["store_full"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_workingday_cv_plot = renderPlot({
  single_cv_plot <- single_dea_return()[["workingday"]][["cv_plot"]]
  print(single_cv_plot)
})

output$single_workingday_frontier_plot = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  single = single [,1:23]
  
  single$Revenue = as.numeric(gsub(",","",as.character(single$Revenue)))
  single$ATV = as.numeric(gsub(",","",as.character(single$ATV)))
  singledata <- single_findmean(single)
  single_workingday_mean <- singledata$singlestore_workingday
  
  mean_table <-  single_workingday_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  rownames(mean_table) <- NULL
  time <- unique(single$Time)
  single_workingday_mean <- data.frame(apply( single_workingday_mean[,-1],2,function(x) x / mean(x)))
  X=matrix(single_workingday_mean$mean_instore,ncol=1)
  Y=cbind(single_workingday_mean$mean_sales, single_workingday_mean$mean_transaction)
  
  
  # frontier
  dea.plot.frontier(X,Y,txt=time,col="red", RTS="vrs",lwd=3,family = "BL")
  dea.plot.frontier(X,Y,txt=time,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
  
  # 
  # frontier <- single_dea_return()[["weekday"]][["frontier"]]
  # print(frontier)
})

output$single_holiday_mean = renderDataTable({
  store_full <- single_dea_return()[["holiday"]][["mean_table"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_holiday_cv = renderDataTable({
  store_full <- single_dea_return()[["holiday"]][["store_full"]]
  #print(DT::datatable(store_full, options = list(searching = FALSE, paging = FALSE)))
})

output$single_holiday_cv_plot = renderPlot({
  single_cv_plot <- single_dea_return()[["holiday"]][["cv_plot"]]
  print(single_cv_plot)
})

output$single_holiday_frontier_plot = renderPlot({
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  single = single [,1:23]
  
  singledata <- single_findmean(single)
  single_holiday_mean <- singledata$singlestore_holiday
  
  mean_table <-   single_holiday_mean
  mean_table[,-1] <- round(mean_table[,-1], digits = 2)
  rownames(mean_table) <- NULL
  time <- unique(single$Time)
  single_holiday_mean <- data.frame(apply( single_holiday_mean[,-1],2,function(x) x / mean(x)))
  X=matrix( single_holiday_mean$mean_instore,ncol=1)
  Y=cbind( single_holiday_mean$mean_sales,  single_holiday_mean$mean_transaction)
  
  
  # frontier
  dea.plot.frontier(X,Y,txt=time,col="red", RTS="vrs",lwd=3,family = "BL")
  dea.plot.frontier(X,Y,txt=time,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
  
  # 
  # frontier <- single_dea_return()[["weekend"]][["frontier"]]
  # print(frontier)
})

output$downloadData_sinall <- downloadHandler(
  filename = 'DEA_graph_sinall.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("cv_plot.png","frontier.png")
    
    single_cv_plot <- single_dea_return()[["all"]][["cv_plot"]]
    ggsave("cv_plot.png",width = 7.2,height = 2.8)
    single <- input$single
    single <- read.table(single$datapath,sep = ",",header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    single = single [,1:23]
    
    single$Revenue = as.numeric(gsub(",","",as.character(single$Revenue)))
    single$ATV = as.numeric(gsub(",","",as.character(single$ATV)))
    singledata <- single_findmean(single)
    single_all_mean <- singledata$singlestore_dea
    
    mean_table <-  single_all_mean
    mean_table[,-1] <- round(mean_table[,-1], digits = 2)
    rownames(mean_table) <- NULL
    time <- unique(single$Time)
    single_all_mean <- data.frame(apply( single_all_mean[,-1],2,function(x) x / mean(x)))
    X=matrix( single_all_mean$mean_instore,ncol=1)
    Y=cbind( single_all_mean$mean_sales, single_all_mean$mean_transaction)
    # frontier
    png("frontier.png",width = 700,height=300)
    dea.plot.frontier(X,Y,txt=time,col="red", RTS="vrs",lwd=3,family = "BL")
    dea.plot.frontier(X,Y,txt=time,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
    dev.off()
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)
output$downloadData_sinworking <- downloadHandler(
  filename = 'DEA_graph_sinworking.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("cv_plot.png","frontier.png")
    
    single_cv_plot <- single_dea_return()[["workingday"]][["cv_plot"]]
    ggsave("cv_plot.png",width = 7.2,height = 2.8)
    single <- input$single
    single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    single = single [,1:23]
    
    single$Revenue = as.numeric(gsub(",","",as.character(single$Revenue)))
    single$ATV = as.numeric(gsub(",","",as.character(single$ATV)))
    singledata <- single_findmean(single)
    single_workingday_mean <- singledata$singlestore_workingday
    
    mean_table <-  single_workingday_mean
    mean_table[,-1] <- round(mean_table[,-1], digits = 2)
    rownames(mean_table) <- NULL
    time <- unique(single$Time)
    single_workingday_mean <- data.frame(apply( single_workingday_mean[,-1],2,function(x) x / mean(x)))
    X=matrix(single_workingday_mean$mean_instore,ncol=1)
    Y=cbind(single_workingday_mean$mean_sales, single_workingday_mean$mean_transaction)
    
    
    # frontier
    png("frontier.png",width = 700,height=300)
    dea.plot.frontier(X,Y,txt=time,col="red", RTS="vrs",lwd=3,family = "BL")
    dea.plot.frontier(X,Y,txt=time,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
    dev.off()
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)
output$downloadData_sinweekend <- downloadHandler(
  filename = 'DEA_graph_sinweekend.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("cv_plot.png","frontier.png")
    
    single_cv_plot <- single_dea_return()[["holiday"]][["cv_plot"]]
    ggsave("cv_plot.png",width = 7.2,height = 2.8)
    
    single <- input$single
    single <- read.table(single$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
    #刪去商品櫃資料
    single = single [,1:23]
    
    single$Revenue = as.numeric(gsub(",","",as.character(single$Revenue)))
    single$ATV = as.numeric(gsub(",","",as.character(single$ATV)))
    singledata <- single_findmean(single)
    single_holiday_mean <- singledata$singlestore_holiday
    
    mean_table <-   single_holiday_mean
    mean_table[,-1] <- round(mean_table[,-1], digits = 2)
    rownames(mean_table) <- NULL
    time <- unique(single$Time)
    single_holiday_mean <- data.frame(apply(single_holiday_mean[,-1],2,function(x) x / mean(x)))
    X=matrix(single_holiday_mean$mean_instore,ncol=1)
    Y=cbind(single_holiday_mean$mean_sales,single_holiday_mean$mean_transaction)
    
    # frontier
    png("frontier.png",width = 700,height=300)
    dea.plot.frontier(X,Y,txt=time,col="red", RTS="vrs",lwd=3,family = "BL")
    dea.plot.frontier(X,Y,txt=time,col="red", RTS="crs",lwd=3,add=TRUE,lty="dashed",family = "BL")
    dev.off()
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)