# data preparation: get preparation time to holiday ----

getPreparationTime <- function(deal_data,holidays.doy,prewindow){
  
  
  holidays_idx_table <- data.frame(holiday_idx = seq(1,3,1),
                                   h_first_day = c(22,45,93),
                                   h_last_day = c(28,45,95))
  
  # finaldate <- max(deal_data$launch_dow_dm) #= 91
  date.table <- seq(1,95,1) # to April 4th, last day of Qingming
  #holidays.doy <- c(seq(1,3),seq(22,28),45,seq(93,95))
  count.tmp <- merge(deal_data%>%select(event_id,category_idx,category,platform_cat,detailed_cat_price,launch_dow_dm,end_dow_dm,price,offering_duration,platform,discount,city),date.table,all =TRUE)%>%
    rename(doy=y)
  count.table <- count.tmp%>%mutate(on.ind = (launch_dow_dm<=doy)*(end_dow_dm>=doy))
  
  daystolaunch <- count.table%>%filter(doy%in%holidays.doy,on.ind==1)
  
  daystolaunch <- merge(daystolaunch,holidays_idx_table)%>%
    mutate(holiday=holiday_idx*(doy<=h_last_day)*(doy>=h_first_day-prewindow))%>%
    filter(holiday!=0)%>%group_by(event_id,platform)%>%
    summarise(holiday=min(holiday),launch_dow_dm = first(launch_dow_dm),h_first_day = min(h_first_day),
              end_dow_dm= first(end_dow_dm),h_last_day = first(h_last_day),
              price=first(price),duration=first(offering_duration),discount= first(discount))%>%
    mutate(holiday.len = h_last_day-h_first_day+1)%>%
    mutate(prep.period = -(launch_dow_dm-h_first_day))
  
  
  daystolaunch.prept <- merge(daystolaunch%>%select(event_id,prep.period,holiday.len,holiday),
                                  deal_data%>%
                                    select(event_id,launch_dow_dm,detailed_cat_price,price,original_price,offering_duration,discount,platform,
                                           competitors_hol, competitors,
                                           first_day_volume,fifth_day_volume,total_volume,city,partysize,weekends),by="event_id",all.y = TRUE)
  #t1comp,t5comp,tendcomp,

  daystolaunch.prept[is.na(daystolaunch.prept$prep.period)==1,"prep.period"] <- 0
  
  return(daystolaunch.prept)
}

