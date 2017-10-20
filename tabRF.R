library(shiny)

rf_return <- reactive({
  # Read data
  single <- input$single
  single <- read.table(single$datapath,sep = ",",check.names = FALSE,header = TRUE,encoding = "utf-8")
  #刪去商品櫃資料
  single = single [,1:23]
  
  # Process the data (factor -> numeric ... etc)
  new_single <- singleprocess(single)
  # Select variables
  rev_vars <- c("Weekday", "Time", "InstoreTraffic","TrafficConversion","Weather","Revenue","Temperature","NormalVacation","SpecialVacation","ConsistentVacation","MonthofYear","WeekofMonth")
  revenue_train <- new_single[,rev_vars]
  instore_vars <- c("Weekday", "Time", "InstoreTraffic","Weather","Temperature","NormalVacation","SpecialVacation","ConsistentVacation","MonthofYear","WeekofMonth")
  instore_train <- new_single[,instore_vars]
  # Train the model
  revenue.rf <- randomForest(Revenue ~ ., data= revenue_train, importance=TRUE,proximity=TRUE,ntree = 200)
  instore.rf <- randomForest(InstoreTraffic ~ ., data= instore_train, importance=TRUE,proximity=TRUE,ntree = 200)
  
  revenue_data <- rf_imp_partial(revenue.rf,revenue_train)
  instore_data <- rf_imp_partial(instore.rf,instore_train)
  
  list("revenue_rf_imp" = revenue_data[["rf_imp"]],
       "revenue_partial" = revenue_data[["partial"]],
       "instore_rf_imp" = instore_data[["rf_imp"]],
       "instore_partial" = instore_data[["partial"]]
  )
})

rf_imp_partial <- function(rf,train){
  imp <- importance(rf)
  impor <- data.frame(round(imp,2))
  # partial
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  par(mfrow=c(2, 2),family = "BL")
  do.call("partialPlot.randomForest", list(x = rf, pred.data = train, x.var = impvar[1]))
  do.call("partialPlot.randomForest", list(x = rf, pred.data = train, x.var = impvar[2]))
  do.call("partialPlot.randomForest", list(x = rf, pred.data = train, x.var = impvar[3]))
  do.call("partialPlot.randomForest", list(x = rf, pred.data = train, x.var = impvar[4]))
  # partialPlot(rf, train,impvar[1])
  # partialPlot(rf, train,impvar[2], main=paste("Partial Dependence on ", impvar[2]),col="red")
  # partialPlot(rf, train,impvar[3], main=paste("Partial Dependence on ", impvar[3]),col="red")
  # partialPlot(rf, train,impvar[4], main=paste("Partial Dependence on ", impvar[4]),col="red")
  partial <- recordPlot()
  # rf_imp
  rf_imp <- ggplot(impor,aes(x=reorder(rownames(impor),X.IncMSE),y=X.IncMSE))+
    geom_bar(stat='identity',fill="#00BFC4") +
    coord_flip()+
    ylab("%MSE Value")+
    xlab(" ")+
    theme( plot.title = element_text(hjust = 0.5,size=12),
           axis.text.x = element_text(size=16),
           axis.text.y = element_text(size=16),
           axis.title  = element_text(size=16,family="BL"))
  return(list("rf_imp" = rf_imp,"partial" = partial))
}


singleprocess <- function(single){
  single$Time<-as.factor(single$Time)
  single$WeekNumber<-as.factor(single$WeekNumber)
  single$NormalVacation <-as.factor(single$NormalVacation)
  single$SpecialVacation <-as.factor(single$SpecialVacation)
  single$ConsistentVacation <-as.factor(single$ConsistentVacation)
  single$Temperature<-as.numeric(as.character(single$Temperature))
  single$Temperature[is.na(single$Temperature)]<-mean(single$Temperature,na.rm = TRUE)
  timevariables<-as.POSIXlt(single$Date)
  single$MonthofYear<-as.factor(timevariables$mon+1)
  single$WeekofMonth<-as.factor(ceiling(timevariables$mday/7))
  # names(single)[names(single) == 'normal_vacation'] <- 'Normal Vacation'
  # names(single)[names(single) == 'special_vacation'] <- 'Special Vacation'
  # names(single)[names(single) == 'consistent_vacation'] <- 'Consistent Vacation'
  return(single)
}

output$revenue_rf_imp = renderPlot({
  revenue_rf_imp <- rf_return()[["revenue_rf_imp"]]
  print(revenue_rf_imp)
})

output$revenue_partial = renderPlot({
  revenue_partial <- rf_return()[["revenue_partial"]]
  print(revenue_partial)
})

output$instore_rf_imp = renderPlot({
  instore_rf_imp <- rf_return()[["instore_rf_imp"]]
  print(instore_rf_imp)
})

output$instore_partial = renderPlot({
  instore_partial <- rf_return()[["instore_partial"]]
  print(instore_partial)
})

output$downloadData_RF_Revenue <- downloadHandler(
  filename = 'RF_graph_revenue.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("revenue_rf_imp.png","revenue_partial.png")
    
    revenue_rf_imp <- rf_return()[["revenue_rf_imp"]]
    ggsave("revenue_rf_imp.png",width = 7.2,height = 2.8)
    
    png("revenue_partial.png",width = 700,height=300)
    print(rf_return()[["revenue_partial"]])
    dev.off()
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)
output$downloadData_RF_Instore <- downloadHandler(
  filename = 'RF_graph_instore.zip',
  content = function(fname) {
    
    tmpdir <- tempdir()
    setwd(tempdir())
    print (tempdir())
    fs <- c("instore_rf_imp.png","instore_partial.png")
    
    instore_rf_imp <- rf_return()[["instore_rf_imp"]]
    ggsave("instore_rf_imp.png",width = 7.2,height = 2.8)
    
    png("instore_partial.png",width = 700,height=300)
    print(rf_return()[["instore_partial"]])
    dev.off()
    
    
    print (fs)
    zip(zipfile=fname, files=fs, flags = "-r9X", extras = "",
        zip = Sys.getenv("R_ZIPCMD", "zip"))
    
  }
)

#------randomforest partial function-------
partialPlot.default <- function(x, ...)
  stop("partial dependence plot not implemented for this class of objects.\n")

partialPlot.randomForest <-
  function (x, pred.data, x.var, which.class, w, plot=TRUE, add=FALSE,
            n.pt = min(length(unique(pred.data[, xname])), 51), rug = TRUE,
            xlab=deparse(substitute(x.var)), ylab="",
            main=paste("Partial Dependence on", deparse(substitute(x.var))),
            ...)
  {
    classRF <- x$type != "regression"
    if (is.null(x$forest))
      stop("The randomForest object must contain the forest.\n")
    x.var <- substitute(x.var)
    xname <- if (is.character(x.var)) x.var else {
      if (is.name(x.var)) deparse(x.var) else {
        eval(x.var)
      }
    }
    xv <- pred.data[, xname]
    n <- nrow(pred.data)
    if (missing(w)) w <- rep(1, n)
    if (classRF) {
      if (missing(which.class)) {
        focus <- 1
      }
      else {
        focus <- charmatch(which.class, colnames(x$votes))
        if (is.na(focus))
          stop(which.class, "is not one of the class labels.")
      }
    }
    if (is.factor(xv) && !is.ordered(xv)) {
      x.pt <- levels(xv)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] > 0,
                                              pr[, focus], .Machine$double.eps)) -
                                     rowMeans(log(ifelse(pr > 0, pr, .Machine$double.eps))),
                                   w, na.rm=TRUE)
        } else y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        
      }
      if (add) {
        points(1:length(x.pt), y.pt, type="h", lwd=2, ...)
      } else {
        if (plot) barplot(y.pt, width=rep(1, length(y.pt)), col="#00BFC4",
                          xlab = xlab, ylab = ylab, main=main,
                          names.arg=x.pt, ...)
      }
    } else {
      if (is.ordered(xv)) xv <- as.numeric(xv)
      x.pt <- seq(min(xv), max(xv), length = n.pt)
      y.pt <- numeric(length(x.pt))
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- rep(x.pt[i], n)
        if (classRF) {
          pr <- predict(x, x.data, type = "prob")
          y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] == 0,
                                              .Machine$double.eps, pr[, focus]))
                                   - rowMeans(log(ifelse(pr == 0, .Machine$double.eps, pr))),
                                   w, na.rm=TRUE)
        } else {
          y.pt[i] <- weighted.mean(predict(x, x.data), w, na.rm=TRUE)
        }
      }
      if (add) {
        lines(x.pt, y.pt, ...)
      } else {
        if (plot) plot(x.pt, y.pt, type = "l", xlab=xlab, ylab=ylab,
                       main = main, ...)
      }
      if (rug && plot) {
        if (n.pt > 10) {
          rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
        } else {
          rug(unique(xv, side = 1))
        }
      }
    }
    invisible(list(x = x.pt, y = y.pt))
  }
