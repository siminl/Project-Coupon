# add in competitors, by city

getCompetitors <- function(deal_data,hols){
  
  numcities <- length(unique(deal_data$city))
  cities <- unique(deal_data$city)
  
  deal_data$competitors <- NA
  deal_data$competitors_hol <- NA
  
  for(ci in 1:numcities){
    citydata <- deal_data%>%filter(city==cities[ci])
    competitors <- rep(NA,dim(citydata)[1])
    competitors_hol  <- rep(NA,dim(citydata)[1])
    
    # hols <- c(seq(1,3),seq(22,28),45,seq(93,95))
    for(i in 1:dim(citydata)[1]){
      competitors[i] <- competition_full(citydata,citydata$launch_dow_dm[i],
                                    citydata$end_dow_dm[i],citydata$platform_cat[i])
    }
    
    for(i in 1:dim(citydata)[1]){
      if(citydata$holiday[i]>0){
        coverage = seq(citydata$launch_dow_dm[i],citydata$end_dow_dm[i])
        covered = coverage%in%hols
        lb = min(coverage[covered])
        if(citydata$holiday[i] %in% c(5,9)){
          ub = min(max(coverage[covered]),28)
        }else if(citydata$holiday[i] == 7){
          ub = min(max(coverage[covered]),45)
        }else if(citydata$holiday[i] == 6){
          ub = min(max(coverage[covered]),3)
        }else{
          ub = max(coverage[covered])
        }
        competitors_hol[i] <- competition_full(citydata,lb,ub,citydata$platform_cat[i])
      }else{
        competitors_hol[i] <- 0
      }
    }
    
    #citydata$competitors <- competitors
    #citydata$competitors_hol <- competitors_hol
    
    deal_data[(deal_data$event_id)%in%(citydata$event_id),"competitors"] <- competitors
    deal_data[(deal_data$event_id)%in%(citydata$event_id),"competitors_hol"] <- competitors_hol
    
  }

  return(deal_data)
  
}


### add in competitors by timestamp in the deal_data dataset
### Date: 11/20/2017

getComp_ts_finecat <- function(deal_data){
  
  t1comp <- rep(NA,dim(deal_data)[1])
  t5comp <- rep(NA,dim(deal_data)[1])
  tendcomp <- rep(NA,dim(deal_data)[1])
  
  # numcities <- length(unique(deal_data$city))
  for(i in 1:dim(deal_data)[1]){
    day1 <- deal_data$launch_dow_dm[i]
    day5 <- min(deal_data$launch_dow_dm[i] + 4,deal_data$end_dow_dm[i])
    lastday <- deal_data$end_dow_dm[i]
    
    t1comp[i] <- competition_city(deal_data,day1,day1,deal_data[,"platform_cat_price"][i],deal_data$city[i])
    t5comp[i] <- competition_city(deal_data,day1,day5,deal_data[,"platform_cat_price"][i],deal_data$city[i])
    tendcomp[i] <- competition_city(deal_data,day1,lastday,deal_data[,"platform_cat_price"][i],deal_data$city[i])    
    
    if(i%%100 == 0){
      print(i)
    }
  }
  
  deal_data <- data.frame(deal_data,
                          t1comp = as.data.frame(t1comp),
                          t5comp = as.data.frame(t5comp),
                          tendcomp = as.data.frame(tendcomp))
  
  return(deal_data)
}


# functions to calculate number of competitors -----
competition_full <- function(data,start,end,subcat){
  datatmp<- data%>%filter(platform_cat == as.character(subcat))
  inlife <- (datatmp$launch_dow_dm<=end)* (datatmp$end_dow_dm>=start)
  counts <- sum(inlife)-1
  return(counts)
}

competition_city <- function(data,start,end,subcat,ct){
  datatmp<- data%>%filter(platform_cat_price == as.character(subcat),city == as.character(ct))
  inlife <- (datatmp$launch_dow_dm<=end)* (datatmp$end_dow_dm>=start)
  counts <- sum(inlife)-1
  return(counts)
}

