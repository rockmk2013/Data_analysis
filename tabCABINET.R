cabinet_return <- reactive({
  # Read data
  cabinet <- input$single
  cabinet <- read.table(cabinet$datapath,sep = ",",header = TRUE,check.names = FALSE,encoding = "utf-8")
  # Get all cabinets containing the words DwellTime, DwellTraffic or ShopperTouch
  index <- grepl(c("DwellTime|DwellTraffic|ShopperTouch"), colnames(cabinet))
  # Number of cabinets
  ncab <- sum(index)/3
  # Index of first cabinet
  nstart <- ncol(cabinet) - ncol(cabinet[,index]) + 1
  # Get the names of the cabinets
  cabinet_names <- unique(gsub("\\-.*","",colnames(cabinet[,nstart:ncol(cabinet)])))
  cabinet <- cabinet[rowSums(cabinet[,nstart:ncol(cabinet)]) !=sum(index) * -1,]
  # Summarize columns for visualization
  sel_cabinet <- cbind(cabinet[,c("Weekday","Time")],cabinet[,nstart:ncol(cabinet)])
  mean_cabinet <- sel_cabinet %>% group_by(Weekday,Time) %>% summarise_all(funs(mean))
  mean_cabinet$Time = factor(mean_cabinet$Time,ordered=TRUE)
  mean_cabinet$Weekday = factor(mean_cabinet$Weekday,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE)
  mean_cabinet <- data.frame(with(mean_cabinet, mean_cabinet[order(Weekday),]))

  #Three plots for the first cabinet
    tmp_cabinet <- cbind(mean_cabinet[,c("Weekday","Time")],mean_cabinet[,3:5])
    tmp_name <- c("Weekday","Time","ShopperTouch","DwellTime","DwellTraffic")
    colnames(tmp_cabinet) <- tmp_name
    shopper_touch_graph <- ggplot(tmp_cabinet,aes(Time,Weekday)) + 
      geom_point(aes(size=ShopperTouch), colour="green3") + 
      theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + 
      ggtitle(colnames(cabinet)[nstart])
    dwell_time_graph <- ggplot(tmp_cabinet,aes(Time,Weekday)) + 
      geom_point(aes(size=DwellTime), colour="pink2") + 
      theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + 
      ggtitle(colnames(cabinet)[nstart+1])
    dwell_traffic_graph <- ggplot(tmp_cabinet,aes(Time,Weekday)) + 
      geom_point(aes(size=DwellTraffic), colour="dodgerblue") + 
      theme_bw() + 
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + 
      ggtitle(colnames(cabinet)[nstart+2])


  # Mean by weekday and time
  mean_by_weekday <- mean_cabinet %>% group_by(Weekday) %>% dplyr::select(seq(from=3,to=ncol(mean_cabinet),by=1)) %>% summarise_all(funs(mean))
  mean_by_time <- mean_cabinet %>% group_by(Time) %>% dplyr::select(seq(from=3,to=ncol(mean_cabinet),by=1)) %>% summarise_all(funs(mean))
  
  # Determine which metric to use
  hasShopperTouch <- sum((colSums(mean_cabinet[,seq(from=3,to=3+(ncab-1)*3,by=3)]) > 0) & (!is.na(colSums(mean_cabinet[,seq(from=3,to=3+(ncab-1)*3,by=3)]))))
  hasDwellTraffic <- sum((colSums(mean_cabinet[,seq(from=5,to=5+(ncab-1)*3,by=3)]) > 0) & (!is.na(colSums(mean_cabinet[,seq(from=5,to=5+(ncab-1)*3,by=3)]))))
  
  useTouch <- FALSE
  useDwell <- FALSE
  
  if(hasShopperTouch > ncab/2){
    useTouch <- TRUE
  }
  if(hasDwellTraffic > ncab/2){
    useDwell <- TRUE
  }
  
  if(useTouch){
    if(useDwell){
      for(i in 1:ncab){
        mean_by_weekday[,(3*i-1)] <- sqrt(mean_by_weekday[,(3*i-1)] * mean_by_weekday[,(3*i+1)])
        mean_by_time[,(3*i-1)] <- sqrt(mean_by_time[,(3*i-1)] * mean_by_time[,(3*i+1)])
      }
    }
  }else if (useDwell){
    for(i in 1:ncab){
      mean_by_weekday[,(3*i-1)] <- mean_by_weekday[,(3*i+1)]
      mean_by_time[,(3*i-1)] <- mean_by_time[,(3*i+1)]
    }
  }
  
  mean_by_weekday <- mean_by_weekday[,c(1,seq(from=2,to=(3*ncab-1),by=3))]
  colnames(mean_by_weekday) <- c("Weekday",as.character(seq(from=1,to=ncab,by=1)))
  mean_by_weekday <- mean_by_weekday[colSums(!is.na(mean_by_weekday)) > 0]
  colnames(mean_by_weekday) <- c("Weekday",cabinet_names[as.integer(colnames(mean_by_weekday)[-1])])
  mean_by_weekday[,-1] <- 5 * (mean_by_weekday[,-1] - min(mean_by_weekday[,-1])) / (max(mean_by_weekday[,-1]) - min(mean_by_weekday[,-1]))
  mean_by_weekday_long <- melt(mean_by_weekday, id.vars="Weekday", value.name="Score", variable.name="Cabinet")
  # RETURN CABINET WEEKDAY GRAPH
  cabinet_weekday_graph <- ggplot(mean_by_weekday_long, aes(Cabinet, Weekday)) + 
          geom_tile(aes(fill = Score),colour = "white") + 
          geom_text(aes(label = round(Score, 1)),size=3) + 
          scale_fill_gradient(low = "white",high = "red") + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  mean_by_time <- mean_by_time[,c(1,seq(from=2,to=(3*ncab-1),by=3))]
  colnames(mean_by_time) <- c("Time",as.character(seq(from=1,to=ncab,by=1)))
  mean_by_time <- mean_by_time[colSums(!is.na(mean_by_time)) > 0]
  colnames(mean_by_time) <- c("Time",cabinet_names[as.integer(colnames(mean_by_time)[-1])])
  mean_by_time[,-1] <- 5 * (mean_by_time[,-1] - min(mean_by_time[,-1])) / (max(mean_by_time[,-1]) - min(mean_by_time[,-1]))
  mean_by_time_long <- melt(mean_by_time, id.vars="Time", value.name="Score", variable.name="Cabinet")
  # RETURN CABINET TIME GRAPH
  cabinet_time_graph <- ggplot(mean_by_time_long, aes(Cabinet, Time)) + 
          geom_tile(aes(fill = Score),colour = "white") + 
          geom_text(aes(label = round(Score, 1)),size=3) + 
          scale_fill_gradient(low = "white",high = "blue") + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Performance of each cabinet
  #colnames(mean_by_time) <- c("Time",cabinet_names[as.integer(colnames(mean_by_time)[-1])])
  score <- colSums(mean_by_time[,-1])
  score <- data.frame(score)
  score$cabinet <- colnames(mean_by_time)[-1]
  score$z_score <- round((score$score - mean(score$score))/sd(score$score), 2)
  score$score_type <- ifelse(score$z_score < 0, "below", "above")  # above / below avg flag
  score <- score[order(score$z_score), ]  # sort
  score$cabinet <- factor(score$cabinet, levels = score$cabinet)
  score <- score[complete.cases(score), ]
  # RETURN CABINET SCORE GRAPH
  cabinet_score_graph <- ggplot(score, aes(x=cabinet, y=z_score, label=z_score)) + 
    geom_bar(stat='identity', aes(fill=score_type), width=.6)  +
    scale_fill_manual(name="Score", 
                      labels = c("Above Average", "Below Average"), 
                      values = c("above"="#00ba38", "below"="#f8766d")) + 
    coord_flip(expand = TRUE)
  #+ labs(title= "Normalised score for each cabinet") 

  
  list("shopper_touch_graph" = shopper_touch_graph,
       "dwell_time_graph" = dwell_time_graph,
       "dwell_traffic_graph" = dwell_traffic_graph,
       "cabinet_weekday_graph" = cabinet_weekday_graph,
       "cabinet_time_graph" = cabinet_time_graph,
       "cabinet_score_graph" = cabinet_score_graph)
})

draw_single_cabinet <- function(cabinet){
  # Get all cabinets containing the words DwellTime, DwellTraffic or ShopperTouch
  index <- grepl(c("DwellTime|DwellTraffic|ShopperTouch"), colnames(cabinet))
  # Number of cabinets
  ncab <- ncol(cabinet[,index])/3
  # Index of first cabinet
  nstart <- ncol(cabinet) - ncol(cabinet[,index]) + 1
  # Get the names of the cabinets
  cabinet_names <- unique(gsub("\\-.*","",colnames(cabinet[,nstart:ncol(cabinet)])))
  # Summarize columns for visualization
  sel_cabinet <- cbind(cabinet[,c("Weekday","Time")],cabinet[,nstart:ncol(cabinet)])
  mean_cabinet <- sel_cabinet %>% group_by(Weekday,Time) %>% summarise_all(funs(mean))
  mean_cabinet$Time = factor(unique(mean_cabinet$Time),ordered=TRUE)
  mean_cabinet$Weekday = factor(mean_cabinet$Weekday,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE)
  mean_cabinet <- data.frame(with(mean_cabinet, mean_cabinet[order(Weekday),]))

  #Three plots for each cabinet
  for(i in 1:ncab){
    tmp_cabinet <- cbind(mean_cabinet[,c("Weekday","Time")],mean_cabinet[,(3*i):(3*i+2)])
    tmp_name <- c("Weekday","Time","ShopperTouch","DwellTime","DwellTraffic")
    colnames(tmp_cabinet) <- tmp_name
    
    ggplot(tmp_cabinet,aes(Time,Weekday))+geom_point(aes(size=ShopperTouch), colour="green3")+theme_bw()+theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5))+ggtitle(enc2native(colnames(cabinet)[3*i+nstart-3]))
    ggsave(paste0(enc2native(colnames(cabinet)[3*i+nstart-3]),".png"),width=7,height=7)
    ggplot(tmp_cabinet,aes(Time,Weekday))+geom_point(aes(size=DwellTime), colour="pink2")+theme_bw()+theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5))+ggtitle(enc2native(colnames(cabinet)[3*i+nstart-2]))
    ggsave(paste0(enc2native(colnames(cabinet)[3*i+nstart-2]),".png"),width=7,height=7)
    ggplot(tmp_cabinet,aes(Time,Weekday))+geom_point(aes(size=DwellTraffic), colour="dodgerblue")+theme_bw()+theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5))+ggtitle(enc2native(colnames(cabinet)[3*i+nstart-1]))
    ggsave(paste0(enc2native(colnames(cabinet)[3*i+nstart-1]),".png"),width=7,height=7)
  }
  
}

main_cab <- function(){
  cabinet <- input$single
  #建立資料夾名稱
  filename = paste0(head(strsplit(cabinet$name,split=".",fixed=T)[[1]],1),"(cabinet)")
  cabinet <- read.table(cabinet$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  setwd("c:/Users/asus/Documents/graph")
  dir.create(filename)
  setwd(paste0("c:/Users/asus/Documents/graph/",filename))

  draw_single_cabinet(cabinet)
}
main_multicab <- function(){
  cabinet <- input$single
  #建立資料夾名稱
  filename = paste0(head(strsplit(cabinet$name,split=".",fixed=T)[[1]],1),"(multicabinet)")
  cabinet <- read.table(cabinet$datapath,sep = ",",check.names=FALSE,header = TRUE,encoding = "utf-8")
  setwd("c:/Users/asus/Documents/graph")
  dir.create(filename)
  setwd(paste0("c:/Users/asus/Documents/graph/",filename))
  
  # Read data
  cabinet <- input$single
  cabinet <- read.table(cabinet$datapath,sep = ",",header = TRUE,check.names = FALSE,encoding = "utf-8")
  # Get all cabinets containing the words DwellTime, DwellTraffic or ShopperTouch
  index <- grepl(c("DwellTime|DwellTraffic|ShopperTouch"), colnames(cabinet))
  # Number of cabinets
  ncab <- ncol(cabinet[,index])/3
  # Index of first cabinet
  nstart <- ncol(cabinet) - ncol(cabinet[,index]) + 1
  # Get the names of the cabinets
  cabinet_names <- unique(gsub("\\-.*","",colnames(cabinet[,nstart:ncol(cabinet)])))
  # Summarize columns for visualization
  sel_cabinet <- cbind(cabinet[,c("Weekday","Time")],cabinet[,nstart:ncol(cabinet)])
  mean_cabinet <- sel_cabinet %>% group_by(Weekday,Time) %>% summarise_all(funs(mean))
  mean_cabinet$Time = factor(unique(mean_cabinet$Time),ordered=TRUE)
  mean_cabinet$Weekday = factor(mean_cabinet$Weekday,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE)
  mean_cabinet <- data.frame(with(mean_cabinet, mean_cabinet[order(Weekday),]))
  
  # Mean by weekday and time
  mean_by_weekday <- mean_cabinet %>% group_by(Weekday) %>% dplyr::select(seq(from=3,to=ncol(mean_cabinet),by=1)) %>% summarise_all(funs(mean))
  mean_by_time <- mean_cabinet %>% group_by(Time) %>% dplyr::select(seq(from=3,to=ncol(mean_cabinet),by=1)) %>% summarise_all(funs(mean))
  
  # Determine which metric to use
  hasShopperTouch <- sum((colSums(mean_cabinet[,seq(from=3,to=3+(ncab-1)*3,by=3)]) > 0) & (!is.na(colSums(mean_cabinet[,seq(from=3,to=3+(ncab-1)*3,by=3)]))))
  hasDwellTraffic <- sum((colSums(mean_cabinet[,seq(from=5,to=5+(ncab-1)*3,by=3)]) > 0) & (!is.na(colSums(mean_cabinet[,seq(from=5,to=5+(ncab-1)*3,by=3)]))))
  
  useTouch <- FALSE
  useDwell <- FALSE
  
  if(hasShopperTouch > ncab/2){
    useTouch <- TRUE
  }
  if(hasDwellTraffic > ncab/2){
    useDwell <- TRUE
  }
  
  if(useTouch){
    if(useDwell){
      for(i in 1:ncab){
        mean_by_weekday[,(3*i-1)] <- sqrt(mean_by_weekday[,(3*i-1)] * mean_by_weekday[,(3*i+1)])
        mean_by_time[,(3*i-1)] <- sqrt(mean_by_time[,(3*i-1)] * mean_by_time[,(3*i+1)])
      }
    }
  }else if (useDwell){
    for(i in 1:ncab){
      mean_by_weekday[,(3*i-1)] <- mean_by_weekday[,(3*i+1)]
      mean_by_time[,(3*i-1)] <- mean_by_time[,(3*i+1)]
    }
  }
  
  mean_by_weekday <- mean_by_weekday[,c(1,seq(from=2,to=(3*ncab-1),by=3))]
  colnames(mean_by_weekday) <- c("Weekday",as.character(seq(from=1,to=ncab,by=1)))
  mean_by_weekday <- mean_by_weekday[colSums(!is.na(mean_by_weekday)) > 0]
  colnames(mean_by_weekday) <- c("Weekday",cabinet_names[as.integer(colnames(mean_by_weekday)[-1])])
  mean_by_weekday[,-1] <- 5 * (mean_by_weekday[,-1] - min(mean_by_weekday[,-1])) / (max(mean_by_weekday[,-1]) - min(mean_by_weekday[,-1]))
  mean_by_weekday_long <- melt(mean_by_weekday, id.vars="Weekday", value.name="Score", variable.name="Cabinet")
  # RETURN CABINET WEEKDAY GRAPH
  cabinet_weekday_graph <- ggplot(mean_by_weekday_long, aes(Cabinet, Weekday)) + 
    geom_tile(aes(fill = Score),colour = "white") + 
    geom_text(aes(label = round(Score, 1)),size=3) + 
    scale_fill_gradient(low = "white",high = "red") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("cabinet_weekday_graph.png",width=max(ncab/2,8),height=max(ncab/4,4))
  
  mean_by_time <- mean_by_time[,c(1,seq(from=2,to=(3*ncab-1),by=3))]
  colnames(mean_by_time) <- c("Time",as.character(seq(from=1,to=ncab,by=1)))
  mean_by_time <- mean_by_time[colSums(!is.na(mean_by_time)) > 0]
  colnames(mean_by_time) <- c("Time",cabinet_names[as.integer(colnames(mean_by_time)[-1])])
  mean_by_time[,-1] <- 5 * (mean_by_time[,-1] - min(mean_by_time[,-1])) / (max(mean_by_time[,-1]) - min(mean_by_time[,-1]))
  mean_by_time_long <- melt(mean_by_time, id.vars="Time", value.name="Score", variable.name="Cabinet")
  # RETURN CABINET TIME GRAPH
  cabinet_time_graph <- ggplot(mean_by_time_long, aes(Cabinet, Time)) + 
    geom_tile(aes(fill = Score),colour = "white") + 
    geom_text(aes(label = round(Score, 1)),size=3) + 
    scale_fill_gradient(low = "white",high = "#0039d8") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave("cabinet_time_graph.png",width=max(ncab/2,8),height=max(ncab/4,4))
  # Performance of each cabinet
  score <- colSums(mean_by_time[,-1])
  score <- data.frame(score)
  score$cabinet <- colnames(mean_by_time)[-1]
  score$z_score <- round((score$score - mean(score$score))/sd(score$score), 2)
  score$score_type <- ifelse(score$z_score < 0, "below", "above")  # above / below avg flag
  score <- score[order(score$z_score), ]  # sort
  score$cabinet <- factor(score$cabinet, levels = score$cabinet)
  score <- score[complete.cases(score), ]
  # RETURN CABINET SCORE GRAPH
  cabinet_score_graph <- ggplot(score, aes(x=cabinet, y=z_score, label=z_score)) + 
    geom_bar(stat='identity', aes(fill=score_type), width=.6)  +
    scale_fill_manual(name="Score", 
                      labels = c("Above Average", "Below Average"), 
                      values = c("above"="#00ba38", "below"="#f8766d")) + 
    coord_flip(expand = TRUE)
  ggsave("cabinet_score_graph.png",width=max(ncab/2,8),height=max(ncab/4,4))
  
}

output$cabinet_weekday_graph = renderPlot({
  cabinet_weekday_graph <- cabinet_return()[["cabinet_weekday_graph"]]
  print(cabinet_weekday_graph)
})

output$cabinet_time_graph = renderPlot({
  cabinet_time_graph <- cabinet_return()[["cabinet_time_graph"]]
  print(cabinet_time_graph)
})

output$cabinet_score_graph = renderPlot({
  cabinet_score_graph <- cabinet_return()[["cabinet_score_graph"]]
  print(cabinet_score_graph)
})

output$shopper_touch_graph = renderPlot({
  shopper_touch_graph <- cabinet_return()[["shopper_touch_graph"]]
  print(shopper_touch_graph)
})

output$dwell_time_graph = renderPlot({
  dwell_time_graph <- cabinet_return()[["dwell_time_graph"]]
  print(dwell_time_graph)
})

output$dwell_traffic_graph = renderPlot({
  dwell_traffic_graph <- cabinet_return()[["dwell_traffic_graph"]]
  print(dwell_traffic_graph)
})

get_filename <- reactive({
  cabinet <- input$single
  #建立資料夾名稱
  cab_filename = paste0(head(strsplit(cabinet$name,split=".",fixed=T)[[1]],1),"(cabinet)")
  return(cab_filename)
})
get_filename_multi <- reactive({
  cabinet <- input$single
  #建立資料夾名稱
  cab_filename = paste0(head(strsplit(cabinet$name,split=".",fixed=T)[[1]],1),"(multicabinet)")
  return(cab_filename)
})

output$downloadCabinet <- downloadHandler(
  filename = paste0("CabinetGraph.zip"),
  content = function(fname) {
    withProgress( value = 20,detail = 'This might take some time...',{
      main_cab()
      setwd("c:/Users/asus/Documents/graph")
      print(get_filename())
      zip(zipfile=fname, files=get_filename())
      
    })
    do.call(unlink, list(get_filename(),recursive = T))
  }
)
output$downloadMultiCabinet<- downloadHandler(
  filename = paste0("MultiCabinetGraph.zip"),
  content = function(fname) {
    withProgress( value = 20,detail = 'This might take some time...',{
      main_multicab()
      setwd("c:/Users/asus/Documents/graph")
      zip(zipfile=fname, files=get_filename_multi())
    })
    do.call(unlink, list(get_filename_multi(),recursive = T))
  }
)