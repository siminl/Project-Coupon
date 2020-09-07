


# tmp <- dealsfull_finecat%>%filter(platform_cat%in%c("Travel","Outdoor"))
#  unique(str_extract(tmp$deal_info, paste(daypattern,collapse="|")) )

getPerPersonPrice <- function(deal_data,catname){
  
  countlist <- c(paste(seq(0,10),"人",sep=""),"一人","双人","三人","四人","五人",
                 "六人","七人","八人","九人","十人")
  
  daylist <- c(paste(c(2,6),"日",sep=""),"3天","6天","二日","六日","三天","六天")
  
  catgp <- deal_data%>%filter(platform_cat %in% catname)
  pattern <- "[0-9|\\p{Han}]{1}人"
  idxtmplist <- str_extract(catgp$deal_info, pattern) 
  extr_pattern <- unique(idxtmplist)
  overlap <- (match(extr_pattern,countlist))
  
  daypattern <- c("[0-9|\\p{Han}]{1}日","[0-9|\\p{Han}]{1}天")
  idxdaylist <- str_extract(catgp$deal_info, paste(daypattern,collapse="|")) 
  extr_daypattern <- unique(idxdaylist)
  overlapday <- (match(extr_daypattern,daylist))
  
  check <- vector()
      for(i in 1:length(overlap)){
        if(is.na(overlap[i])==0){
          idxtmp <- str_extract(catgp$deal_info, extr_pattern[i])
          countidx <- overlap[i]
          if(countidx > 11){
            denom1 <- (countidx-11)
            catgp[is.na(idxtmp)!=1,"partysize"] <- denom1
            #catgp[is.na(idxtmp)!=1,"original_price"] <- catgp[is.na(idxtmp)!=1,"original_price"]/denom1
          }else if(countidx>1&countidx<=11){
            denom1 <- (countidx-1)
            catgp[is.na(idxtmp)!=1,"partysize"] <- denom1
            #catgp[is.na(idxtmp)!=1,"original_price"] <- catgp[is.na(idxtmp)!=1,"original_price"]/denom1
          }else{
            denom1 <- 10
            catgp[is.na(idxtmp)!=1,"partysize"] <- denom1
            #catgp[is.na(idxtmp)!=1,"original_price"] <- catgp[is.na(idxtmp)!=1,"original_price"]/denom1
          }
        }
      

      }
  
  
      for(j in 1:length(overlapday)){
          if(is.na(overlapday[j])!=1){
            idxtmpday <- str_extract(catgp$deal_info, extr_daypattern[j])
            countdayidx <- overlapday[j]
            if(countdayidx %in%c(1,5)){
              denom2 <- 2
              catgp[is.na(idxtmpday)!=1,"partysize"] <- denom2
              #catgp[is.na(idxtmpday)!=1,"original_price"] <- catgp[is.na(idxtmpday)!=1,"original_price"]/denom2
            }else if(countdayidx %in%seq(2,8,2)){
              denom2 <- 6
              catgp[is.na(idxtmpday)!=1,"partysize"] <- denom2
              #catgp[is.na(idxtmpday)!=1,"original_price"] <- catgp[is.na(idxtmpday)!=1,"original_price"]/denom2
            }else{
              denom2 <- 3
              catgp[is.na(idxtmpday)!=1,"partysize"] <- denom2
              #catgp[is.na(idxtmpday)!=1,"original_price"] <- catgp[is.na(idxtmpday)!=1,"original_price"]/denom2
            }
          }
          
      }
  
  # deal_data <- merge(deal_data%>%filter(event_id%in%catgp$event_id)%>%select(-original_price),
  #                    catgp%>%select(event_id,original_price),by="event_id",all.x = TRUE)%>%
  #   rbind(deal_data%>%filter(!event_id%in%catgp$event_id))
  
  deal_data <- merge(deal_data%>%filter(event_id%in%catgp$event_id),
                     catgp%>%select(event_id,partysize),by="event_id",all.x = TRUE)%>%
    rbind(deal_data%>%filter(!event_id%in%catgp$event_id)%>%mutate(partysize =1))%>%
    mutate(partysize = ifelse(is.na(partysize)==1,1,partysize))%>%
    mutate(discprice = (1-discount)*original_price/partysize)
  return(deal_data)
}
