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
  storename = c(1:length(multi[,1]))
  
  #set limit
  x_min <- min(multi$multistore_weekdays.mean_instore_workingday)
  x_max <- max(multi$multistore_weekdays.mean_instore_holiday)
  
  y_min <- min(multi$multistore_weekdays.mean_sales_workingday)
  y_max <- max(multi$multistore_weekdays.mean_sales_holiday)
  
  #
  store_plot <- ggplot(multi,aes(x=multistore_weekdays.mean_instore_workingday, y=multistore_weekdays.mean_sales_workingday)) + 
    geom_point(aes(size = 1.5),alpha=0.7,colour="#00BFC4") + 
    geom_label(aes(label=storename,alpha=0.5),hjust=0.5,vjust=1,colour="#00BFC4") + 
    xlab("Instore") + 
    ylab("Revenue") +
    scale_x_continuous(limits = c(x_min,x_max))+
    scale_y_continuous(limits = c(y_min,y_max))+
    geom_vline(xintercept = x_max/2) + 
    geom_hline(yintercept = y_max/2) +
    theme(panel.background = element_rect(fill = "#F0F0F0"),legend.position="none",axis.title = element_text(size=16),axis.text = element_text(size=16))
  
  return(store_plot)
})

output$multi_holiday_plot = renderPlot({
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multidata <- findfqamean(multi)
  #set data
  multi = data.frame(multidata)
  storename = c(1:length(multi[,1]))
  
  #set limit
  x_min <- min(multi$multistore_weekdays.mean_instore_workingday)
  x_max <- max(multi$multistore_weekdays.mean_instore_holiday)
  
  y_min <- min(multi$multistore_weekdays.mean_sales_workingday)
  y_max <- max(multi$multistore_weekdays.mean_sales_holiday)
  
  #
  
  store_plot <- ggplot(multi,aes(x=multistore_weekdays.mean_instore_holiday, y=multistore_weekdays.mean_sales_holiday, colour="#00BFC4")) + 
    geom_point(aes(size = 1.5),alpha=0.7) + 
    geom_label(aes(label=storename,alpha=0.5),hjust=0.5,vjust=1) + 
    xlab("Instore") + 
    ylab("Revenue") +
    scale_x_continuous(limits = c(x_min,x_max))+
    scale_y_continuous(limits = c(y_min,y_max))+
    geom_vline(xintercept = x_max/2) + 
    geom_hline(yintercept = y_max/2) +
    theme(panel.background = element_rect(fill = "#F0F0F0"),legend.position="none",axis.title = element_text(size=16),axis.text = element_text(size=16))
  
 # colortype = c("red","#80FFFF","brown","green","blue","black","purple","pink","yellow","#4F9D9D","#FF8040")
 
  return(store_plot)
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
    ylab("轉換效率")+
    xlab("店名")+
    theme_bw()+
    theme(panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.title=element_text(size=16,family = "BL")
    )
})


output$downloadData_FQA <- downloadHandler(
  filename = 'FQA_graph_all.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("workingday.png","holiday.png","slope.png")
    
    multi <- input$multi
    multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
    multidata <- findfqamean(multi)
    #set data
    multi = data.frame(multidata)
    storename = c(1:length(multi[,1]))
    
    #set limit
    x_min <- min(multi$multistore_weekdays.mean_instore_workingday)
    x_max <- max(multi$multistore_weekdays.mean_instore_holiday)
    
    y_min <- min(multi$multistore_weekdays.mean_sales_workingday)
    y_max <- max(multi$multistore_weekdays.mean_sales_holiday)
    
     ggplot(multi,aes(x=multistore_weekdays.mean_instore_workingday, y=multistore_weekdays.mean_sales_workingday)) + 
      geom_point(aes(size = 1.5),alpha=0.7,colour="#00BFC4") + 
      geom_label(aes(label=storename,alpha=0.5),hjust=0.5,vjust=1,colour="#00BFC4") + 
      xlab("Instore") + 
      ylab("Revenue") +
      scale_x_continuous(limits = c(x_min,x_max))+
      scale_y_continuous(limits = c(y_min,y_max))+
      geom_vline(xintercept = x_max/2) + 
      geom_hline(yintercept = y_max/2) +
      theme(panel.background = element_rect(fill = "#F0F0F0"),legend.position="none",axis.title = element_text(size=16,family = "BL"),axis.text = element_text(size=16))
    ggsave("workingday.png",width=9,height = 3.8)
     ggplot(multi,aes(x=multistore_weekdays.mean_instore_holiday, y=multistore_weekdays.mean_sales_holiday, colour="#00BFC4")) + 
      geom_point(aes(size = 1.5),alpha=0.7) + 
      geom_label(aes(label=storename,alpha=0.5),hjust=0.5,vjust=1) + 
      xlab("Instore") + 
      ylab("Revenue") +
      scale_x_continuous(limits = c(x_min,x_max))+
      scale_y_continuous(limits = c(y_min,y_max))+
      geom_vline(xintercept = x_max/2) + 
      geom_hline(yintercept = y_max/2) +
      theme(panel.background = element_rect(fill = "#F0F0F0"),legend.position="none",axis.title = element_text(size=16,family = "BL"),axis.text = element_text(size=16))
    ggsave("holiday.png",width=9,height = 3.8)
    instore_dif = multi$multistore_weekdays.mean_instore_holiday-multi$multistore_weekdays.mean_instore_workingday
    sales_dif = multi$multistore_weekdays.mean_sales_holiday-multi$multistore_weekdays.mean_sales_workingday
    slope = (sales_dif/instore_dif)
    slope = slope/mean(slope)
    slopedata = data.frame(storename,instore_dif,sales_dif,slope)
    
    ggplot(slopedata,aes(x=as.factor(storename),y=slope))+geom_bar(stat="identity",fill="#00BFC4")+
      ylab("轉換效率")+
      xlab("店名")+
      theme_bw()+
      theme(panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            axis.title=element_text(size=16,family = "BL")
      )
    ggsave("slope.png",width=9,height = 3.8)
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)
