dataprep <- function(deal_data,pre_window){
  
  holidays_idx_table <- data.frame(holiday_idx = seq(1,4,1),
                                   h_first_day = c(1,22,45,93),
                                   h_last_day = c(3,28,45,95))
  
  numcat <- dim(deal_data%>%distinct(category))[1]
  category_idx <- as.numeric(factor(deal_data$category,labels = seq(1,numcat)))
  deal_data <- cbind(deal_data,category_idx)
  vouchers_idx_table <- deal_data%>%distinct(category,category_idx) # index key table: category
  
  # indicator for the day of the week of the launch date
  nrows <- dim(deal_data)[1]
  
  #data.last.day <- deal_data$launch_date[nrows]
  data.last.day <- max(deal_data$launch_date)
  tmp <- deal_data%>%mutate(dummy = deal_data$launch_date[1])%>%
    mutate(launch_dow_dm = as.numeric(as.Date(deal_data$launch_date)-as.Date(dummy)) +1)%>%
    # mutate(launch_dow = launch_dow_dm%%7-1)%>%mutate(launch_dow=replace(launch_dow, launch_dow==0, 7))%>%
    # mutate(launch_dow=replace(launch_dow, launch_dow==-1, 6))%>%
    mutate(end_dow_dm = as.numeric(as.Date(deal_data$end_date)-as.Date(dummy)) +1)
  # mutate(end_dow = end_dow_dm%%7-1)%>%mutate(end_dow=replace(end_dow, end_dow==0, 7))%>%
  # mutate(end_dow=replace(end_dow, end_dow==-1, 6))
  
  ##### Note!!! there are events covered two holidays!!!
  holiday.tmp <- merge(tmp,holidays_idx_table,all=TRUE)
  holiday.tmp1 <- holiday.tmp%>%mutate(holiday=holiday_idx*(launch_dow_dm<=h_last_day)*(end_dow_dm>=h_first_day-pre_window))%>%
    group_by(event_id)%>%summarise(holiday=sum(holiday,na.rm=TRUE))
  
  # Data Preparation Step1 output: holiday and day of week indicator added 
  deal_data <- merge(tmp,holiday.tmp1,by = "event_id")%>%select(-c(dummy))
  deal_data <- deal_data %>%
    mutate(
      # launch_date = parse_date_time(launch_date, "Ymd HMS"),
      # end_date = parse_date_time(end_date, "Ymd HMS"),
      launch_wkdy = wday(launch_date, label=TRUE, abbr=TRUE),
      end_wkdy = wday(end_date, label=TRUE, abbr=TRUE))
  
  # extract original prices

  pattern <- c("价值[0-9]+.?[0-9]*元","价值元[0-9]+.?[0-9]*元","价值[0-9]+.?[0-9]*-",
               "价值[0-9]+.?[0-9]*/","原价[0-9]+.?[0-9]*元","享原[0-9]+.?[0-9]*元",
               "享受[0-9]+.?[0-9]*元","价值[0-9]+")
  price.pattern <- str_extract(deal_data$deal_info, paste(pattern,collapse="|")) 
  
  # checked: all NA return from this has price 0
  naidx <- which(is.na(price.pattern)==1)
  deal_data[naidx,"price"]
  # replace NA by 0 rmb
  price.pattern[naidx] <- "价值0元"
  #price.pattern.mat <- data.frame(event_id = deal_data$event_id, price.pattern=price.pattern)
  
  original_price <- str_extract(price.pattern,"[0-9]+.?[0-9]*") 
  original_price <- unlist(strsplit(original_price, split='元', fixed=TRUE))
  
  reidxtmp <- gregexpr(paste(c('-','/'),collapse="|"),original_price)
  reidx <- unlist(reidxtmp)
  
  for(i in 1:length(reidx)){
    if(reidx[i]==-1){
      original_price[i] <- as.numeric(original_price[i])
    }else{
      secpart <- substr(original_price[i],reidx[i]+1,nchar(original_price[i]))
      if(secpart!=""){
        original_price[i] <- (as.numeric(substr(original_price[i],1,reidx[i]-1))+
                                as.numeric(substr(original_price[i],reidx[i]+1,nchar(original_price[i]))))/2
      }else{
        original_price[i] <- as.numeric(substr(original_price[i],1,reidx[i]-1))
      }      
    }
  }
  
  # op.tmp <- data.frame(event_id = seq(min(deal_data$event_id),min(deal_data$event_id)+nrows-1),
  #                      original_price = as.numeric(original_price))
  # 
  # deal_data <- merge(deal_data,op.tmp,by="event_id")
  
  deal_data <- data.frame(deal_data, original_price = as.numeric(original_price))
  
  # Data Preparation Step2 output: discount rate
  deal_data <- deal_data%>%mutate(discount = (1-price/original_price)*(original_price!=0))%>%
    mutate(discount=replace(discount,is.nan(discount)==1,0))
  
  # non was thrown, and correct some of errorneously documented offering duration
  deal_data <- deal_data%>%na.omit()%>%mutate(offering_duration = end_dow_dm-launch_dow_dm+1) 
  
  
  # remove price == 0, these are the deals that prices are not correctly extracted 
  paste("No. deals whose original prices are unsuccessfully extracted:", sum((deal_data$original_price)==0))
  deal_data <- deal_data%>%filter(original_price !=0)
  
  # correct 2 discounts, 2 coupons to be used at the same time --> price/2*originalprice
  #deal_data[deal_data$discount>1,"discount"] <- deal_data[deal_data$discount>1,"price"]/(deal_data[deal_data$discount>1,"original_price"]*2)
  paste("No. deals whose discounts>=1:", dim(deal_data%>%filter(discount>=1))[1])
  deal_data <- deal_data%>%filter(discount<1)
  
  return(deal_data)
}
