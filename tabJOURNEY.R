<<<<<<< HEAD
customer_journey <- reactive({
  # Read data
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",header = TRUE,encoding = "utf-8",check.names = FALSE)
  waffle_plot <- draw_waffle(daily)
  mean_list <- calculate_funnel(daily)
  funnel_plot <- draw_funnel(mean_list)
  trend_plot <- draw_trend(daily)
  list("funnel_plot" = funnel_plot,
       "waffle_plot" = waffle_plot,
       "trend_plot"  = trend_plot)
})

calculate_funnel <- function(daily){
  ### Storefront ###
  mean_storefront <- mean(daily$StorefrontTraffic,na.rm = TRUE)
  ### TO BE FIXED !!
  mean_storefront <- mean_storefront / 10
  mean_storefront <- round(mean_storefront,2)
  
  ### Awareness (Instore) ###
  mean_instore <- mean(daily$InstoreTraffic,na.rm = TRUE)
  mean_instore <- round(mean_instore,2)
  
  ###　Interest (Dwell) ###
  index <- grepl(c("DwellTraffic"), colnames(daily))
  dwell <- daily[,index]
  mean_dwell <- mean(apply(dwell,1,mean))
  mean_dwell <- round(mean_dwell,2)
  
  ###  Desire (Fitting) ###
  ### TO BE FIXED !!
  mean_fitting <- 70
  #mean_fitting <- mean(daily$X2Floor.Traffic) / 4
  #mean_fitting <- round(mean_fitting,2)
  
  ### Action (Transaction) ###
  mean_transaction <- mean(daily$Transaction,na.rm = TRUE)
  mean_transaction <- round(mean_transaction,2)
  
  return(list(mean_storefront=mean_storefront,mean_instore=mean_instore,mean_dwell=mean_dwell,mean_fitting=mean_fitting,mean_transaction=mean_transaction))
}

draw_funnel <- function(mean_list){
  mean_storefront <- mean_list[["mean_storefront"]]
  mean_instore <- mean_list[["mean_instore"]]
  mean_dwell <- mean_list[["mean_dwell"]]
  mean_fitting <- mean_list[["mean_fitting"]]
  mean_transaction <- mean_list[["mean_transaction"]]
  
  df.content <- data.frame(content = c('Storefront', 'Instore', 'Dwell', 'Fitting','Purchase'), 
                           Step = c('Base','Awareness','Interest','Desire','Action'),
                           number = c(mean_storefront, mean_instore, mean_dwell, mean_fitting, mean_transaction))
  
  df.all <- df.content %>%
    group_by(Step) %>%
    mutate(totnum = sum(number)) %>%
    ungroup() %>%
    mutate(dum = (max(totnum) - totnum)/2,
           maxx = totnum + dum,
           minx = dum)
  
  df.lines <- df.all %>%
    distinct(Step, maxx, minx)
  
  df.dum <- df.all %>%
    distinct(Step, dum) %>%
    mutate(content = 'dummy',
           number = dum) %>%
    dplyr::select(content, Step, number)
  
  conv <- df.all$totnum[df.all$Step == 'Action']
  
  df.rates <- df.all %>%
    distinct(Step, totnum) %>%
    mutate(prevnum = lag(totnum),
           rate = round(totnum / prevnum, 3)) %>%
    dplyr::select(Step, rate)
  
  df.rates <- na.omit(df.rates)
  
  # creating final data frame
  df.all <- df.all %>%
    dplyr::select(content, Step, number)
  
  df.all <- rbind(df.all, df.dum)
  
  df.all$Step <- factor(df.all$Step, levels = c('Action', 'Desire', 'Interest', 'Awareness', 'Base'))
  df.all <- df.all %>%
    arrange(desc(Step))
  list1 <- df.all %>% distinct(content) %>%
    filter(content != 'dummy')
  df.all$content <- factor(df.all$content, levels = c(as.character(list1$content), 'dummy'))
  
  df.all <- df.all %>%
    arrange(Step, desc(content)) %>%
    group_by(Step) %>%
    mutate(pos = cumsum(number) - 0.5*number) %>%
    ungroup()
  
  #numcolors <- nlevels(df.all$content)
  #colors <- rainbow(numcolors-1, s = 1, v = 1, start = 0, end = max(1, numcolors-2)/(numcolors-1), alpha = 1)
  #cols <- c(colors,"#ffffff")
  cols <- c("#588C73", "#F2E394", "#F2AE72", "#D96459","#8C4646", "#ffffff")
  
  funnel_plot <- 
    ggplot() +
    theme_minimal() +
    coord_flip() +
    scale_fill_manual(values=cols) +
    geom_bar(data=df.all, aes(x=Step, y=number, fill=content), stat="identity", width=1) +
    theme(legend.position='none', axis.ticks=element_blank(), axis.text.x=element_blank(), 
          axis.title.x=element_blank(),axis.title.y = element_blank()) +
    geom_text(data=df.all[df.all$content!='dummy', ],
              aes(x=Step, y=pos, label=paste0(content, ' - ', number)),
              size=6, color='white', fontface="bold") + 
    geom_ribbon(data=df.lines, aes(x=Step, ymax=max(maxx), ymin=maxx, group=1), fill='white') +
    geom_line(data=df.lines, aes(x=Step, y=maxx, group=1), color='darkred', size=2) + 
    geom_ribbon(data=df.lines, aes(x=Step, ymax=minx, ymin=min(minx), group=1), fill='white') +
    geom_line(data=df.lines, aes(x=Step, y=minx, group=1), color='darkred', size=2) + 
    geom_text(data=df.rates, aes(x=Step, y=(df.lines$minx[-1]), label=ifelse(rate > 1, paste0('+', rate*100-100, '%'), paste0('-', 100-rate*100, '%'))), hjust=1.2,
              color='darkblue', fontface="bold", size=5) + 
    #ggtitle("Funnel Analysis / Customer Journey") +
    theme(plot.title = element_text(hjust = 0.5, size=20),legend.position='none', axis.ticks=element_blank(), axis.text = element_text(size=20), axis.text.x=element_blank())

    return(funnel_plot)
}

draw_waffle <- function(daily) {
  ### Waffle chart for series
  # Remove parentheses from column name
  colnames(daily) <- gsub("\\(", "", colnames(daily))
  colnames(daily) <- gsub("\\)", "", colnames(daily))
  # Extract columns containing "DwellTraffic"
  index <- grepl(c("DwellTraffic"), colnames(daily))
  dwell <- daily[,index]
  
  # Extract list of keywords
  # 1. Remove everything after "-"
  # keylist <- gsub("-DwellTraffic","",colnames(dwell))
  # #bug fixed "-" ==> "." 
  # # 2. Remove any numbers
  # keylist <- gsub("\\d", "", keylist)
  # # 3. Split by "/"
  # keylist <- unlist(strsplit(keylist, "/"))
  # # 4. Unique words
  # keywords <- unique(unname(keylist))
  keywords <- c("運動鞋","包款","童鞋","商務皮鞋","休閒鞋","配件","服飾")
  
  series_dwell <- c()
  for(i in 1:length(keywords)){
    index <- grepl(keywords[i], colnames(dwell))
    # Remember to set drop to FALSE!
    series_dwell[i] <- mean(apply(dwell[,index,drop=FALSE],2,mean))
  }
  percentage <- scales::percent(series_dwell/sum(series_dwell))
  names(series_dwell) <- paste(keywords,percentage, sep = " - ")
  my_palette <- rep(brewer.pal(12, "Paired"),10)
  #my_colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99")
  
  waffle_plot <- waffle(series_dwell, rows=12,legend_pos = "bottom", colors = my_palette[1:length(series_dwell)]) + theme(plot.title = element_text(hjust = 0.5))
  return(waffle_plot)
  
}

draw_trend <- function(daily){
  touch = rowSums(daily[,grepl("Touch",colnames(daily))],na.rm = TRUE)
  daily =  daily %>% mutate(touch)
  scale = function(variable){
    return((variable-min(variable))/diff(range(variable)))
  }
  scale_total = cbind(daily[,21:23],sapply(daily[,c(5,6,24,10,128)],scale))
  
  all = scale_total %>% mutate(holiday = if_else(NormalVacation==1|SpecialVacation==1|ConsistentVacation==1,1,0)) %>% group_by(holiday) %>% summarise(mean(StorefrontTraffic),mean(InstoreTraffic),mean(touch),mean(`2Floor Traffic`),mean(Transaction))
  step = c("Base","Awareness","Interact","Desire","Transaction")
  step = factor(step,levels=c("Transaction","Desire","Interact","Awareness","Base"))
  trend_data = cbind(step,transpose(all[,2:6]))
  
  
  colnames(trend_data)[2:3]=c("NormalDay","Holiday")
  library(reshape2)
  new = melt(trend_data,id="step")
  
  trend_plot= ggplot(new,aes(step,value,colour=variable))+
    geom_line(aes(group=variable),lwd=2)+
    geom_point(size=7,shape=21,fill="white",stroke=2)+
    ylim(0,1)+coord_flip()+
    ggtitle("NormalDay/Holiday")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text = element_text(size=14),axis.title = element_text(size=16))
  return(trend_plot)
}

draw_river <- function(daily){
  
  # Storefront and Instore
  mean_storefront <- mean(daily$StorefrontTraffic,na.rm = TRUE)
  mean_instore <- mean(daily$InstoreTraffic,na.rm = TRUE)
  # Enter and exit percentages
  storefront_to_instore <- round(mean_instore/mean_storefront,2)
  storefront_to_exit <- 1 - storefront_to_instore
  
  # Dwell 
  index <- grepl(c("DwellTraffic"), colnames(daily))
  dwell <- daily[,index]
  mean_dwell <- mean(apply(dwell,1,mean))
  # Enter and exit percentages
  instore_to_dwell <- round(mean_dwell/mean_instore,2)
  instore_to_exit <- 1 - instore_to_dwell
  
  # Cabinets
  keywords <- c("運動鞋","包款","童鞋","商務皮鞋","休閒鞋","配件","服飾")
  series_dwell <- c()
  for(i in 1:length(keywords)){
    index <- grepl(keywords[i], colnames(dwell))
    # Remember to set drop to FALSE!
    series_dwell[i] <- mean(apply(dwell[,index,drop=FALSE],2,mean))
  }
  names(series_dwell) <- keywords
  series_dwell <- sort(series_dwell, decreasing = TRUE)
  others <- sum(series_dwell[5:length(series_dwell)])
  names(others) <- "其他"
  series_dwell <- c(series_dwell[1:4],others)
  series_percentage <- series_dwell/sum(series_dwell)
  names(series_percentage) <- names(series_dwell)
  
  # Extract top 4 series and others
  dwell_1_per <- round(series_percentage[1],2)
  dwell_2_per <- round(series_percentage[2],2)
  dwell_3_per <- round(series_percentage[3],2)
  dwell_4_per <- round(series_percentage[4],2)
  dwell_other_per <- round(series_percentage[5],2)
  
  ###  Desire (Fitting) ###
  # FIX !
  dwell_to_fitting <- 0.4
  dwell_to_exit <- 1 - dwell_to_fitting
  
  ### Action (Transaction) ###
  # Fix !
  fitting_to_purchase <- 0.3
  fitting_to_exit <- 1 - fitting_to_purchase
  
  # Fix !
  buy_1_per <- 0.3
  names(buy_1_per) <- "運動鞋"
  buy_2_per <- 0.25
  names(buy_2_per) <- "包款"
  buy_3_per <- 0.2
  names(buy_3_per) <- "休閒鞋"
  buy_4_per <- 0.15
  names(buy_4_per) <- "配件"
  buy_other_per <- 1 - (buy_1_per + buy_2_per + buy_3_per + buy_4_per)
  names(buy_other_per) <- "其他"
  
  nodes <- data.frame(ID = c(LETTERS[1:20]),
                      labels = c( A= "店外", B= "離開", C="進店", D="離開", E="互動", F="離開", G="購買", H="其他", I=names(dwell_4_per), J=names(dwell_3_per),K=names(dwell_2_per),L=names(dwell_1_per),M="其他", N=names(buy_4_per), O=names(buy_3_per), P=names(buy_2_per),Q=names(buy_1_per), R="潛在", S="離開", T="試穿"),
                      x = c(1,9,8,15,15,44,42,22,22,22,22,22,49,49,49,49,49,29,38,36),
                      y = c(4,0,3,1,5,5,9,3.6,4.3,5,5.7,6.4,7.6,8.3,9,9.7,10.4,5,3,7),
                      col =
                        c("#80B1D3","#FFFFB3","#80B1D3","#FFFFB3","#80B1D3","#FFFFB3","#80B1D3",
                          "#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA",
                          "#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA",
                          "#80B1D3","#FFFFB3","#80B1D3"),
                      stringsAsFactors = FALSE
  )
  
  edges <- data.frame( N1= c("A","A","C","C","E","E","E","E","E",
                             "H","I","J","K","L","R","R","T","T","G","G","G","G","G"), 
                       N2= c("B","C","D","E","H","I","J","K","L",
                             "R","R","R","R","R","S","T","F","G","M","N","O","P","Q"), 
                       Value = c(storefront_to_exit,storefront_to_instore,instore_to_exit,instore_to_dwell,
                                 dwell_other_per,dwell_4_per,dwell_3_per,dwell_2_per,dwell_1_per,
                                 dwell_other_per,dwell_4_per,dwell_3_per,dwell_2_per,dwell_1_per,
                                 dwell_to_exit,dwell_to_fitting,fitting_to_exit,fitting_to_purchase,
                                 buy_other_per,buy_1_per,buy_2_per,buy_3_per,buy_4_per))
  r <- makeRiver(nodes,edges)
  plt<-riverplot(r,default_style = list(srt=0,textcex=0.7),
                 node_margin = 0.3, nodewidth = 3, plot_area = 1.1)
  
  X<-NULL
  Y<-NULL
  for(i in 1:nrow(edges)){
    if(i%in%c(1:4,15:18)){
      n1 = as.character(edges[i,"N1"])
      n2 = as.character(edges[i,"N2"])
      x_coord = (plt[,n1]["x"]+plt[,n2]["x"])/2
      y_coord = (plt[,n1]["center"]+plt[,n2]["center"])/2
      X<-append(X,x_coord)
      Y<-append(Y,y_coord)
    }
    else{
      n = ifelse(i<10|i>14,as.character(edges[i,"N2"]),as.character(edges[i,"N1"]))
      n2 = ifelse(i<10|i>14,as.character(edges[i+1,"N2"]),as.character(edges[i+1,"N1"]))
      x_coord = plt[,n]["x"]
      y_coord = ifelse(i%in%c(9,14,23),plt[,n]["top"]+0.035,
                       (plt[,n]["center"]+plt[,n2]["center"])/2)
      X<-append(X,x_coord)
      Y<-append(Y,y_coord)
    }
  }
  X<-unname(X)
  Y<-unname(Y)
  X<-X[-c(5:9)]
  Y<-Y[-c(5:9)]
  percentage<-paste0(edges$Value[-c(5:9)]*100,"%")
  text(X,Y,as.character(percentage),cex=0.7)
}

output$trend_plot = renderPlot({
  trend_plot <- customer_journey()[["trend_plot"]]
  print(trend_plot)
})

output$funnel_plot = renderPlot({
  funnel_plot <- customer_journey()[["funnel_plot"]]
  print(funnel_plot)
})

output$river_plot = renderPlot({
  draw_river(daily)
})

output$waffle_plot = renderPlot({
  waffle_plot <- customer_journey()[["waffle_plot"]]
  print(waffle_plot)
})


=======
customer_journey <- reactive({
  # Read data
  daily <- input$daily
  daily <- read.table(daily$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  waffle_plot <- draw_waffle(daily)
  mean_list <- calculate_funnel(daily)
  funnel_plot <- draw_funnel(mean_list)
  trend_plot <- draw_trend(daily)
  list("funnel_plot" = funnel_plot,
       "waffle_plot" = waffle_plot,
       "trend_plot"  = trend_plot)
})

calculate_funnel <- function(daily){
  ### Storefront ###
  mean_storefront <- mean(daily$StorefrontTraffic,na.rm = TRUE)
  ### TO BE FIXED !!
  mean_storefront <- mean_storefront / 10
  mean_storefront <- round(mean_storefront,2)
  
  ### Awareness (Instore) ###
  mean_instore <- mean(daily$InstoreTraffic,na.rm = TRUE)
  mean_instore <- round(mean_instore,2)
  
  ###　Interest (Dwell) ###
  index <- grepl(c("DwellTraffic"), colnames(daily))
  dwell <- daily[,index]
  mean_dwell <- mean(apply(dwell,1,mean))
  mean_dwell <- round(mean_dwell,2)
  
  ###  Desire (Fitting) ###
  ### TO BE FIXED !!
  mean_fitting <- 70
  #mean_fitting <- mean(daily$X2Floor.Traffic) / 4
  #mean_fitting <- round(mean_fitting,2)
  
  ### Action (Transaction) ###
  mean_transaction <- mean(daily$Transaction,na.rm = TRUE)
  mean_transaction <- round(mean_transaction,2)
  
  return(list(mean_storefront=mean_storefront,mean_instore=mean_instore,mean_dwell=mean_dwell,mean_fitting=mean_fitting,mean_transaction=mean_transaction))
}

draw_funnel <- function(mean_list){
  mean_storefront <- mean_list[["mean_storefront"]]
  mean_instore <- mean_list[["mean_instore"]]
  mean_dwell <- mean_list[["mean_dwell"]]
  mean_fitting <- mean_list[["mean_fitting"]]
  mean_transaction <- mean_list[["mean_transaction"]]
  
  df.content <- data.frame(content = c('Storefront', 'Instore', 'Dwell', 'Fitting','Purchase'), 
                           Step = c('Base','Awareness','Interest','Desire','Action'),
                           number = c(mean_storefront, mean_instore, mean_dwell, mean_fitting, mean_transaction))
  
  df.all <- df.content %>%
    group_by(Step) %>%
    mutate(totnum = sum(number)) %>%
    ungroup() %>%
    mutate(dum = (max(totnum) - totnum)/2,
           maxx = totnum + dum,
           minx = dum)
  
  df.lines <- df.all %>%
    distinct(Step, maxx, minx)
  
  df.dum <- df.all %>%
    distinct(Step, dum) %>%
    mutate(content = 'dummy',
           number = dum) %>%
    dplyr::select(content, Step, number)
  
  conv <- df.all$totnum[df.all$Step == 'Action']
  
  df.rates <- df.all %>%
    distinct(Step, totnum) %>%
    mutate(prevnum = lag(totnum),
           rate = round(totnum / prevnum, 3)) %>%
    dplyr::select(Step, rate)
  
  df.rates <- na.omit(df.rates)
  
  # creating final data frame
  df.all <- df.all %>%
    dplyr::select(content, Step, number)
  
  df.all <- rbind(df.all, df.dum)
  
  df.all$Step <- factor(df.all$Step, levels = c('Action', 'Desire', 'Interest', 'Awareness', 'Base'))
  df.all <- df.all %>%
    arrange(desc(Step))
  list1 <- df.all %>% distinct(content) %>%
    filter(content != 'dummy')
  df.all$content <- factor(df.all$content, levels = c(as.character(list1$content), 'dummy'))
  
  df.all <- df.all %>%
    arrange(Step, desc(content)) %>%
    group_by(Step) %>%
    mutate(pos = cumsum(number) - 0.5*number) %>%
    ungroup()
  
  #numcolors <- nlevels(df.all$content)
  #colors <- rainbow(numcolors-1, s = 1, v = 1, start = 0, end = max(1, numcolors-2)/(numcolors-1), alpha = 1)
  #cols <- c(colors,"#ffffff")
  cols <- c("#588C73", "#F2E394", "#F2AE72", "#D96459","#8C4646", "#ffffff")
  
  funnel_plot <- 
    ggplot() +
    theme_minimal() +
    coord_flip() +
    scale_fill_manual(values=cols) +
    geom_bar(data=df.all, aes(x=Step, y=number, fill=content), stat="identity", width=1) +
    theme(legend.position='none', axis.ticks=element_blank(), axis.text.x=element_blank(), 
          axis.title.x=element_blank(),axis.title.y = element_blank()) +
    geom_text(data=df.all[df.all$content!='dummy', ],
              aes(x=Step, y=pos, label=paste0(content, ' - ', number)),
              size=6, color='white', fontface="bold") + 
    geom_ribbon(data=df.lines, aes(x=Step, ymax=max(maxx), ymin=maxx, group=1), fill='white') +
    geom_line(data=df.lines, aes(x=Step, y=maxx, group=1), color='darkred', size=2) + 
    geom_ribbon(data=df.lines, aes(x=Step, ymax=minx, ymin=min(minx), group=1), fill='white') +
    geom_line(data=df.lines, aes(x=Step, y=minx, group=1), color='darkred', size=2) + 
    geom_text(data=df.rates, aes(x=Step, y=(df.lines$minx[-1]), label=ifelse(rate > 1, paste0('+', rate*100-100, '%'), paste0('-', 100-rate*100, '%'))), hjust=1.2,
              color='darkblue', fontface="bold", size=5) + 
    #ggtitle("Funnel Analysis / Customer Journey") +
    theme(plot.title = element_text(hjust = 0.5, size=20),legend.position='none', axis.ticks=element_blank(), axis.text = element_text(size=20), axis.text.x=element_blank())

    return(funnel_plot)
}

draw_waffle <- function(daily) {
  ### Waffle chart for series
  # Remove parentheses from column name
  colnames(daily) <- gsub("\\(", "", colnames(daily))
  colnames(daily) <- gsub("\\)", "", colnames(daily))
  # Extract columns containing "DwellTraffic"
  index <- grepl(c("DwellTraffic"), colnames(daily))
  dwell <- daily[,index]
  
  # Extract list of keywords
  # 1. Remove everything after "-"
  keylist <- gsub(".DwellTraffic","",colnames(dwell))
  #bug fixed "-" ==> "." 
  # 2. Remove any numbers
  keylist <- gsub("\\d", "", keylist)
  # 3. Split by "/"
  keylist <- unlist(strsplit(keylist, "/"))
  # 4. Unique words
  keywords <- unique(unname(keylist))
  #keywords <- c("運動鞋","包款","配件","拖鞋","服飾商務")
  
  series_dwell <- c()
  for(i in 1:length(keywords)){
    index <- grepl(keywords[i], colnames(dwell))
    # Remember to set drop to FALSE!
    series_dwell[i] <- mean(apply(dwell[,index,drop=FALSE],2,mean))
  }
  percentage <- scales::percent(series_dwell/sum(series_dwell))
  names(series_dwell) <- paste(keywords,percentage, sep = " - ")
  my_palette <- rep(brewer.pal(12, "Paired"),10)
  #my_colors <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99")
  
  ### BUG HERE !!!!!!
  ### error in rep, 可能times吃到NA或是負值?
  waffle_plot <- waffle(series_dwell, rows=12,legend_pos = "bottom", colors = my_palette[1:length(series_dwell)]) + theme(plot.title = element_text(hjust = 0.5))
  return(waffle_plot)
  
}

draw_trend <- function(daily){
  touch = rowSums(daily[,grepl("Touch",colnames(daily))],na.rm = TRUE)
  daily =  daily %>% mutate(touch)
  scale = function(variable){
    return((variable-min(variable))/diff(range(variable)))
  }
  scale_total = cbind(daily[,21:23],sapply(daily[,c(5,6,24,10,128)],scale))
  
  all = scale_total %>% mutate(holiday = if_else(NormalVacation==1|SpecialVacation==1|ConsistentVacation==1,1,0)) %>% group_by(holiday) %>% summarise(mean(StorefrontTraffic),mean(InstoreTraffic),mean(touch),mean(X2Floor.Traffic),mean(Transaction))
  step = c("Base","Awareness","Interact","Desire","Transaction")
  step = factor(step,levels=c("Transaction","Desire","Interact","Awareness","Base"))
  trend_data = cbind(step,transpose(all[,2:6]))
  
  
  colnames(trend_data)[2:3]=c("NormalDay","Holiday")
  library(reshape2)
  new = melt(trend_data,id="step")
  
  trend_plot= ggplot(new,aes(step,value,colour=variable))+
    geom_line(aes(group=variable),lwd=2)+
    geom_point(size=7,shape=21,fill="white",stroke=2)+
    ylim(0,1)+coord_flip()+
    ggtitle("NormalDay/Holiday")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),axis.text = element_text(size=14),axis.title = element_text(size=16))
  return(trend_plot)
}

draw_river <- function(daily){
  nodes <- data.frame(ID = c(LETTERS[1:20]),
                      labels = c( A= "店外", B= "離開", C="進店",
                                  D="離開", E="互動", F="離開", G="購買",
                                  H="其他", I="系列4", J="系列3", K="系列2",L="系列1",
                                  M="其他", N="系列4", O="系列3", P="系列2",Q="系列1",
                                  R="潛在", S="離開", T="試穿"),
                      x = c(1,9,8,15,15,44,42,22,22,22,22,22,49,49,49,49,49,29,38,36),
                      y = c(4,0,3,1,5,5,9,3.6,4.3,5,5.7,6.4,7.6,8.3,9,9.7,10.4,5,3,7),
                      col =
                        c("#80B1D3","#FFFFB3","#80B1D3","#FFFFB3","#80B1D3","#FFFFB3","#80B1D3",
                          "#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA",
                          "#BEBADA","#BEBADA","#BEBADA","#BEBADA","#BEBADA",
                          "#80B1D3","#FFFFB3","#80B1D3"),
                      stringsAsFactors = FALSE
  )
  edges <- data.frame( N1= c("A","A","C","C","E","E","E","E","E",
                             "H","I","J","K","L","R","R","T","T","G","G","G","G","G"), 
                       N2= c("B","C","D","E","H","I","J","K","L",
                             "R","R","R","R","R","S","T","F","G","M","N","O","P","Q"), 
                       Value = c(0.9,0.1,0.5,0.5,
                                 0.2,0.4,0.2,0.1,0.1,
                                 0.2,0.4,0.3,0.2,0.1,
                                 0.7,0.3,0.7,0.3,
                                 0.2,0.2,0.1,0.2,0.3))
  r <- makeRiver(nodes,edges)
  plt<-riverplot(r,default_style = list(srt=0,textcex=0.7),
                 node_margin = 0.3, nodewidth = 3, plot_area = 1.1)
  
  X<-NULL
  Y<-NULL
  for(i in 1:nrow(edges)){
    if(i%in%c(1:4,15:18)){
      n1 = as.character(edges[i,"N1"])
      n2 = as.character(edges[i,"N2"])
      x_coord = (plt[,n1]["x"]+plt[,n2]["x"])/2
      y_coord = (plt[,n1]["center"]+plt[,n2]["center"])/2
      X<-append(X,x_coord)
      Y<-append(Y,y_coord)
    }
    else{
      n = ifelse(i<10|i>14,as.character(edges[i,"N2"]),as.character(edges[i,"N1"]))
      n2 = ifelse(i<10|i>14,as.character(edges[i+1,"N2"]),as.character(edges[i+1,"N1"]))
      x_coord = plt[,n]["x"]
      y_coord = ifelse(i%in%c(9,14,23),plt[,n]["top"]+0.035,
                       (plt[,n]["center"]+plt[,n2]["center"])/2)
      X<-append(X,x_coord)
      Y<-append(Y,y_coord)
    }
  }
  X<-unname(X)
  Y<-unname(Y)
  X<-X[-c(5:9)]
  Y<-Y[-c(5:9)]
  percentage<-paste0(edges$Value[-c(5:9)]*100,"%")
  text(X,Y,as.character(percentage),cex=0.7)
}

output$trend_plot = renderPlot({
  trend_plot <- customer_journey()[["trend_plot"]]
  print(trend_plot)
})

output$funnel_plot = renderPlot({
  funnel_plot <- customer_journey()[["funnel_plot"]]
  print(funnel_plot)
})

output$river_plot = renderPlot({
  draw_river(daily)
})

output$waffle_plot = renderPlot({
  waffle_plot <- customer_journey()[["waffle_plot"]]
  print(waffle_plot)
})


>>>>>>> upstream/master
