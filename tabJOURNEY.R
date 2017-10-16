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


output$trend_plot = renderPlot({
  trend_plot <- customer_journey()[["trend_plot"]]
  print(trend_plot)
})

output$funnel_plot = renderPlot({
  funnel_plot <- customer_journey()[["funnel_plot"]]
  print(funnel_plot)
})

output$waffle_plot = renderPlot({
  waffle_plot <- customer_journey()[["waffle_plot"]]
  print(waffle_plot)
})

