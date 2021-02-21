library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(numDeriv)
library(tictoc)
library(TeachingDemos)
library(profvis)
library(maxLik)
library(ipoptr)
library(DescTools)
library(latex2exp)
library(gridExtra)
library(xtable)
library(foreach)
library(doParallel)
library(snpar)
library(ggthemes)
sl <- dplyr::select
fl <- dplyr::filter
rn <- dplyr::rename
mt <- dplyr::mutate
gb <- dplyr::group_by
sm <- dplyr::summarise


load("/Users/siminli/Documents/GitHub/Project-Coupon/Data/deals.rda")

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

source("Code/LabeltoEng.R")
dealsfull <- LabeltoEng(dealsfull)%>%filter(!category%in%c("Online Shopping, Tickets, cards top up","Others"))

source("Code/DataPrep.R")
# data.start.day <- dealsfull$launch_date[1]
prewindow <- 0
dealsfull <- dataprep(dealsfull, pre_window = prewindow)
source("Code/PlatformSubcategory.R")
dealsfull <- FinerCategory(dealsfull)

# add in number of competitors during holiday -------
source("Code/getCompetitorsRV.R")
hols <- c(seq(22,28),seq(45,45),seq(93,95))
holidays.doy <- c(seq(22-prewindow,28),seq(45-prewindow,45),seq(93-prewindow,95))
#hols <- c(seq(1,3),seq(16,28),seq(39,45),seq(87,95))
dealsfull<- getCompetitors(dealsfull, hols)
# ---------------------------------------------------------
# adjust original prices and the prices by number of people covered in the deal package --------
source("Code/getPerPersonPriceRV.R")
dealsfull <- getPerPersonPrice(dealsfull,c("Casual Dine","Fine Dine","Travel","Outdoor","Indoor","Movie"))
# ---------------------------------------------------------
# Before PCA to get similar prices, we combine some categories with less deals but might have similar
# behavior in terms of market response to holiday and its c/p.
# Here, in particular we combine movies and theatres.

dealsfull[dealsfull$platform_cat%in%c("Movie","Theatre"),"platform_cat"] <- "Movie Theatre"
# ---------------------------------------------------------
# Pricing PCA ------
source("Code/getCategorybyPCARV.R")
optclusters <- c(3,4,3,4,2,2,2,3,3,3,2,3)
dealsfull <- AlgoCat_PCA(dealsfull,optclusters) 

# Preparation time to holiday ---------
#holidays.doy <- c(seq(1,3),seq(22,28),45,seq(93,95))
# robustness check 1: put the purchased early ones in ----
source("Code/getPreparationTimeRV.R")
dealsfull <- getPreparationTime(dealsfull,holidays.doy,prewindow)%>%
  mutate(type = as.numeric(as.factor(detailed_cat_price)))

# fit the sales curve ---
source("FitSalesCurve.R")

source("MLEProcedure.R")
# estimate industry by industry 
# isholiday = 0 nonholiday, otherwise holiday 
MLE_estimation(deal_data,isholiday)




# # get the pre-holiday deals -----
# predays <- 15
# pre.windows <- c(seq(21-predays,21),seq(44-predays,44),seq(92-predays,92))
# for(i in c(2,5,8,11,1,3)){
#   nonhol_data <- dealsfull%>%
#     mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
#     filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
#     #filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
#     mutate(original_price = original_price/sd(original_price))
#   print(dim(nonhol_data))
# }
# 




