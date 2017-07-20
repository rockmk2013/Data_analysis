require(shiny)
#Sys.setlocale(category = "LC_ALL", locale = "cht")
folder_address = "C://Users//user//Documents//GitHub//Data_analysis"
runApp(host="192.168.1.45",port = 9527,folder_address, launch.browser=TRUE)