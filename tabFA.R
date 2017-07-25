library(shiny)

fa_return <- reactive({
  # Read data
  multi <- input$multi
  multi <- read.table(multi$datapath,sep = ",",header = TRUE,encoding = "utf-8")
  multi_mean <- multistoremean(multi)
  storename <- multi_mean[,1]
  multi_mean <- multi_mean[,-1]
  multi_mean <- multi_mean[,colSums(multi_mean) > 0]
  
  multi_mean_table <- data.frame(storename,round(multi_mean,2))
  #corplot
  M <- cor(multi_mean)
  corrplot(M,method="color",type="lower")
  corplot <- recordPlot()
  #multi_scree_plot
  fa.parallel(multi_mean)
  multi_scree_plot <- recordPlot()
  #fa_plot
  fa <- fa(multi_mean,nfactors = 2, residuals = TRUE, scores = "tenBerge", fm = ifelse(det(cor(multi_mean)) < exp(-18),"pa","mle"))
  metrics <- colnames(multi_mean)
  loading <- data.frame(cbind(metrics,xtable(unclass(fa$loadings))))
  rownames(loading) <- NULL
  colnames(loading) <- c("Metrics","Factor 1","Factor 2")
  loading.m <- melt(loading, id="Metrics", measure=c("Factor 1","Factor 2"))
  fa_plot <- ggplot(loading.m, aes(Metrics, abs(value), fill=value)) +
    facet_wrap(~ variable) + geom_bar(stat="identity") + coord_flip() + 
    scale_fill_gradient2(name = "Loading", 
                         high = "blue", mid = "white", low = "red", 
                         midpoint=0, guide="colourbar") +
    ylab("Loading Strength") + 
    theme_bw(base_size=14)
  #fa_plot2
  fa.diagram(fa,Phi=NULL,fe.results=NULL,sort=TRUE,labels=NULL,cut=.5,
             simple=TRUE, errors=FALSE,g=FALSE,digits=1,e.size=.05,rsize=.15,side=2,cex=NULL,marg=c(.5,.5,1,.5),adj=1)
  fa_plot2 <- recordPlot()
  #fa_store
  fa_store <- cbind.data.frame(unique(multi[,1]),fa$scores)
  rownames(fa_store) <- NULL
  colnames(fa_store) <- c("Store Number","Factor 1","Factor 2")
  
  #fa_store_plot
  fa_store_plot <- store_to_plot(fa_store)
  list("multi_mean_table" = multi_mean_table,
       "corplot" = corplot,
       "multi_scree_plot" = multi_scree_plot,
       "fa_plot" = fa_plot,
       "fa_plot2" = fa_plot2,
       "fa_store" = fa_store,
       "fa_store_plot" = fa_store_plot)
})

scorescale <- function(x){sqrt((x-min(x))/(max(x)-min(x)))}

addgroup <- function(fa_store){
  group <- c()
  x_med <- median(fa_store$F1)
  y_med <- median(fa_store$F2)
  for(i in 1:nrow(fa_store)){
    if(fa_store$F1[i] > x_med && fa_store$F2[i] > y_med){
      group[i] <- 1
    }
    else if(fa_store$F1[i] > x_med && fa_store$F2[i] <= y_med){
      group[i] <- 2
    }
    else if(fa_store$F1[i] <= x_med && fa_store$F2[i] > y_med){
      group[i] <- 3
    }
    else if(fa_store$F1[i] <= x_med && fa_store$F2[i] <= y_med){
      group[i] <- 4
    }
  }
  return(group)
}

store_to_plot <- function(fa_store){
  rownames(fa_store) <- NULL
  colnames(fa_store) <- c("Storenum","F1","F2")
  fa_store$F1 <- scorescale(fa_store$F1)
  fa_store$F2 <- scorescale(fa_store$F2)
  fa_store$group <- addgroup(fa_store)
  x_med <- median(fa_store$F1)
  x_min <- min(fa_store$F1)
  x_max <- max(fa_store$F1)
  x_width <- max(x_max - x_med, x_med - x_min)
  y_med <- median(fa_store$F2)
  y_min <- min(fa_store$F2)
  y_max <- max(fa_store$F2)
  y_width <- max(y_max - y_med, y_med - y_min)
  fa_store_plot <- ggplot(fa_store,aes(x=F1, y=F2, colour=factor(group))) + geom_point(aes(size = 1.5)) + geom_label(aes(label=Storenum),hjust=0.5,vjust=1) + xlab("顧客吸引力") + ylab("顧客掌握力")
  fa_store_plot <- fa_store_plot + scale_x_continuous(limits = c(x_med - x_width,x_med + x_width))
  fa_store_plot <- fa_store_plot + scale_y_continuous(limits = c(y_med - y_width,y_med + y_width))
  fa_store_plot <- fa_store_plot + geom_vline(xintercept = median(fa_store$F1)) + geom_hline(yintercept = median(fa_store$F2))
  fa_store_plot <- fa_store_plot + theme(legend.position="none")
  return(fa_store_plot)
}

storesummary <- function(df, group, selection){
  return(df %>% group_by_(group) %>% dplyr::select(one_of(selection)) %>% summarise_all(funs(mean)))
}

multistoremean <- function(multi){
  multi$APV <- multi$Revenue / multi$InstoreTraffic
  group <- "StoreNumber"
  selection <- c("StoreNumber","WindowsConversion","Conversion", "ATV", "UPT","APV","EfficientVisit","VisitDuration","RepeatCustomer")
  multi_mean <- storesummary(multi, group, selection)
  # Set name of stores
  storename <- as.character(unique(multi$StoreNumber))
  multi_mean[,1] <- storename
  return(multi_mean)
}



output$multi_cor_plot = renderPlot({
  multi_cor_plot <- fa_return()[["corplot"]]
  print(multi_cor_plot)
})

output$multi_scree_plot = renderPlot({
  multi_scree_plot <- fa_return()[["multi_scree_plot"]]
  print(multi_scree_plot)
})

output$fa_plot = renderPlot({
  fa_plot <- fa_return()[["fa_plot"]]
  print(fa_plot)
})

output$fa_plot2 = renderPlot({
  fa_plot2 <- fa_return()[["fa_plot2"]]
  print(fa_plot2)
})

output$fa_store = renderDataTable({
  fa_store <- fa_return()[["fa_store"]]
  #print(DT::datatable(fa_store, options = list(searching = FALSE, paging = FALSE)))
})

output$fa_multi_mean = renderDataTable({
  fa_multi_mean <- fa_return()[["multi_mean_table"]]
})

output$fa_store_plot = renderPlot({
  fa_store_plot <- fa_return()[["fa_store_plot"]]
  print(fa_store_plot)
})
