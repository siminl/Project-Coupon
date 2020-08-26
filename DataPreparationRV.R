library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)


load("/Users/Simin/Dropbox/project vouchers/Data and Code/deals.rda")

dealsfull <- deals[,c(5,6,7,8,9,13,19,20,21,22,23,24,10,11,1)]
dealsfull <- data.frame(event_id=seq(1:dim(dealsfull)[1]),dealsfull,stringsAsFactors = FALSE)

colnames(dealsfull) <- c("event_id","category","platform","launch_date","end_date",
                         "offering_duration","price","first_day_volume","first_day_sales",
                         "fifth_day_volume","fifth_day_sales","total_volume","total_sales",
                         "deal_link","deal_info","city")

dealsfull <- data.frame(event_id = as.character(dealsfull$event_id), category = as.character(dealsfull$category),
                        platform = as.character(dealsfull$platform),
                        launch_date = as.Date(dealsfull$launch_date),end_date = as.Date(dealsfull$end_date),
                        dealsfull[,c(6:13)],
                        deal_link = as.character(dealsfull$deal_link),
                        deal_info = as.character(dealsfull$deal_info),city = as.character(dealsfull$city),
                        stringsAsFactors=FALSE)

source("LabeltoEng.R")
dealsfull <- LabeltoEng(dealsfull)%>%filter(!category%in%c("Online Shopping, Tickets, cards top up","Others"))

source("DataPrep.R")
# data.start.day <- dealsfull$launch_date[1]
prewindow <- 0
dealsfull <- dataprep(dealsfull, pre_window = prewindow)
dealsfull <- FinerCategory(dealsfull)

