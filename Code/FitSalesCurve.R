# can we do per day fitting? we don't observe the day by day sales, rather just the cumulative sales on the 1st 5th and last day

# t is day until holiday
# addtiive effects in cities and platforms to the demand arrival patterns
# arrivals happens immediately 
# if model as cumulative demand, should use spurious regression
# endogeneity in discount, duration and timing 
# matching analysis  


# discriptive plots of the sales curves
dealsfull%>%filter(type ==1,is.na(holiday)==1)%>%select(first_day_volume,fifth_day_volume,total_volume)%>%
  mutate(rownum = seq(1,dim(dealsfull%>%filter(type ==1,is.na(holiday)==1))[1]))%>%
  mutate(convexity =( (fifth_day_volume - first_day_volume)/first_day_volume - 
           (total_volume - fifth_day_volume)/fifth_day_volume)>0)%>%
  melt(id.vars = c("rownum","convexity"))%>%
  arrange(rownum) -> tmp

ggplot(data = tmp%>%filter(convexity==TRUE,value<2000), aes(x=variable, y = value, group=rownum))+
  geom_line()

# what if we just estimate S(T) or S(d) instead of tracing a curve.

# start with the non-holiday deals 

plotdata <- vector()
sales_estimates <- vector()
sales_estimates_sig <- vector()
for(i in 1:length(unique(dealsfull$type))){
  
  # nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>tmp}%>%
  #   filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
  #   mutate(original_price = original_price/sd(original_price))
  
  nonhol_data <- dealsfull%>%
    mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
    filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
    #filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
    mutate(original_price = original_price/sd(original_price))
  
  idx <- sample(dim(nonhol_data)[1],floor(dim(nonhol_data)[1]*0.9))
  trainset <- nonhol_data[idx,]
  testset <- nonhol_data[-idx,]
  lm_nonhol <- lm(data=trainset%>%mutate(discprice = (1-discount)*original_price/partysize, # = price/partysize
                                            logvol = log(total_volume),
                                            perperson = original_price/partysize,
                                            competitors_ct = (competitors/sd(competitors)),
                                            logcomp = ifelse(competitors!=0,log(competitors),0),
                                            fixlength = ifelse(offering_duration==15,1,
                                                               ifelse(offering_duration==30,2,
                                                                      ifelse(offering_duration==45,3,0)))),
                  logvol~log(offering_duration)+I(log(offering_duration)^2)+
                    discprice+I(discprice^2)+
                    # log(discprice)+I(log(discprice)^2)+
                    I(log(original_price))+I(1/log(original_price))+I(1/(log(original_price)^2))+
                    logcomp+weekends+platform:weekends+
                    platform+city)
  
  sales_estimates <- rbind(sales_estimates,coef(summary(lm_nonhol))[,1])
  sales_estimates_sig <- rbind(sales_estimates_sig,coef(summary(lm_nonhol))[,1]*(coef(summary(lm_nonhol))[,4]<0.1))
  
  adjust_rsq <- 1-((sum(lm_nonhol$residuals**2))/
                     sum((log(trainset$total_volume)-mean(log(trainset$total_volume)))**2))
  # *
  #   (length(lm_nonhol$residuals)-1)/(length(lm_nonhol$residuals)-length(lm_nonhol$coefficients)-1)

  
  testy <- predict(lm_nonhol,testset%>%mutate(discprice = (1-discount)*original_price/partysize,
                                            logvol = log(total_volume),
                                            perperson = original_price/partysize,
                                            competitors_ct = (competitors/sd(competitors)),
                                            logcomp = ifelse(competitors!=0,log(competitors),0),
                                            fixlength = ifelse(offering_duration==15,1,
                                                                ifelse(offering_duration==30,2,
                                                                       ifelse(offering_duration==45,3,0)))))
  
  testerr <- sqrt(sum((exp(testy)-(testset$total_volume))^2))
  plotdata <- rbind(plotdata, data.frame(fitted=(testy),y=log(testset$total_volume),value=i,x=seq(1,length(testy))))
  print(paste(i," ",adjust_rsq, " ",testerr))
  
  
  
}


plotdata <- vector()
sales_estimates_h <- vector()
for(i in 1:length(unique(dealsfull$type))){
  
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0)%>%{.->>tmp}%>%
    filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
    mutate(original_price = original_price/sd(original_price))
  
  idx <- sample(dim(hol_data)[1],floor(dim(hol_data)[1]*0.9))
  trainset <- hol_data[idx,]
  testset <- hol_data[-idx,]
  lm_nonhol <- lm(data=trainset%>%mutate(discprice = (1-discount)*original_price/partysize, # = price/partysize
                                         logvol = log(total_volume),
                                         perperson = original_price/partysize,
                                         competitors_ct = (competitors/sd(competitors)),
                                         logcomp = ifelse(competitors!=0,log(competitors),0),
                                         fixlength = ifelse(offering_duration==15,1,
                                                            ifelse(offering_duration==30,2,
                                                                   ifelse(offering_duration==45,3,0)))),
                  logvol~log(offering_duration)+I(log(offering_duration)^2)+
                    discprice+I(discprice^2)+
                    # log(discprice)+I(log(discprice)^2)+
                    log(prep.period)+I(log(prep.period)^2)+
                    I(log(original_price))+I(1/log(original_price))+I(1/(log(original_price)^2))+
                    logcomp+weekends+platform:weekends+
                    platform+city)
  
  sales_estimates_h <- rbind(sales_estimates_h,coef(summary(lm_nonhol))[,1])
  
  adjust_rsq <- 1-((sum(lm_nonhol$residuals**2))/
                     sum((log(trainset$total_volume)-mean(log(trainset$total_volume)))**2))
  # *
  #   (length(lm_nonhol$residuals)-1)/(length(lm_nonhol$residuals)-length(lm_nonhol$coefficients)-1)
  
  
  testy <- predict(lm_nonhol,testset%>%mutate(discprice = (1-discount)*original_price/partysize,
                                              logvol = log(total_volume),
                                              perperson = original_price/partysize,
                                              competitors_ct = (competitors/sd(competitors)),
                                              logcomp = ifelse(competitors!=0,log(competitors),0),
                                              fixlength = ifelse(offering_duration==15,1,
                                                                 ifelse(offering_duration==30,2,
                                                                        ifelse(offering_duration==45,3,0)))))
  
  testerr <- sqrt(sum((exp(testy)-(testset$total_volume))^2))
  plotdata <- rbind(plotdata, data.frame(fitted=(testy),y=log(testset$total_volume),value=i,x=seq(1,length(testy))))
  print(paste(i," ",adjust_rsq, " ",testerr))
  
  
  
}




ggplot(data=plotdata%>%mutate(diff=(fitted)-(y)),aes(x=x))+
  #geom_point(aes(y=fitted),color="blue")+geom_point(aes(y=y),color="black")+
  geom_point(aes(y=diff))+geom_hline(yintercept = 0,linetype=2)+
  facet_grid(value~.,scales = "free")

# adjust biasedness can help improve fitness?



