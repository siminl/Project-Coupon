# Validating the estimation results

mleout_preh
mleout_h

struct_est_ph <- vector()
for(i in c(2,5)){
  struct_est_ph <- rbind(struct_est_ph,c(mleout_preh[[i]]$par,mleout_preh[[i]]$convergence,i))
}
colnames(struct_est_ph) <- c("cph","phbeta0","phbetalogd","phbetaalphap",#"phbetalogcomp","phbetawks","phgamma",
                          "phconvergence","type")


struct_est_h <- vector()
for(i in c(2,5)){
  struct_est_h <- rbind(struct_est_h,c(mleout_h[[i]]$par,mleout_h[[i]]$convergence,i))
}
colnames(struct_est_h) <- c("chtmp","hthetaalphap","hthetalogT","hthetalogd","hbetalogd","hX1","hX2",
                          "hconvergence","type")

merge(struct_est_ph,struct_est_h,by="type") -> struct_est


merge(struct_est%>%
        merge(dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,5),prep.period>0,offering_duration<=40)%>%
                group_by(type,detailed_cat_price)%>%
                mutate(lqt = quantile(total_volume,c(0.05)), uqt = quantile(total_volume,c(0.95)), sdop = sd(original_price))%>%
                filter(total_volume>lqt, total_volume<uqt)%>%
                mutate(original_price = original_price/sdop)%>%
                select(type,detailed_cat_price,total_volume,discount,original_price,prep.period,offering_duration,holiday.len,sdop)%>%unique(),
              all.y = TRUE,by=c("type"))%>%
        #merge(cbind(type = seq(1,11),sales_estimates_h),by="type",all.x=TRUE)%>%
        mutate(ch = exp(cph)+exp(chtmp))%>%mutate(cph = exp(cph))%>%
        mutate(d1h = total_volume*(exp(-exp(hthetaalphap)*((1-discount)*original_price)+hthetalogT*log(prep.period+1)+((hthetalogd))*log(offering_duration)+hX1)/
                                     (1+exp(-exp(hthetaalphap)*((1-discount)*original_price)+hthetalogT*log(prep.period+1)+((hthetalogd))*log(offering_duration)+hX1))),
               d2h = exp(-exp(hbetalogd)*log(offering_duration)+((hX2))))%>%{.->>valid_est_h}%>%
        group_by(type,detailed_cat_price)%>%
        mutate(meanD = median((d1h+d2h)), meanmch = sdop*ch,demandinhol = d1h/total_volume)%>%
        mutate(
               # nondealdemandhol = total_volume*(exp(hX1)/(1+exp(hX1))),
               # nondealdemandnhol = total_volume-total_volume*(exp(hX1)/(1+exp(hX1))),
               nondealdemandhol = total_volume*(exp(((hthetalogd))*log(offering_duration)+hX1)/
                                                  (1+exp(((hthetalogd))*log(offering_duration)+hX1))),
               nondealdemandnhol = total_volume-total_volume*(exp(((hthetalogd))*log(offering_duration)+hX1)/
                                                                (1+exp(((hthetalogd))*log(offering_duration)+hX1))),
               dealdemandhol = d1h,
               dealdemandnhol = total_volume-d1h)%>%
        mutate(dsidx = (-dealdemandnhol/(offering_duration-holiday.len)+ dealdemandhol/holiday.len)-
                 (-nondealdemandnhol/(offering_duration-holiday.len)+ nondealdemandhol/holiday.len))%>%
        mutate(a = (-dealdemandnhol/(offering_duration-holiday.len)+ dealdemandhol/holiday.len),
               b = (-nondealdemandnhol/(offering_duration-holiday.len)+ nondealdemandhol/holiday.len))%>%
        summarise(mch = mean(meanmch), d1hmed = median(d1h), d2hmed = median(d2h), 
                  demandinholmed = median(d1h/total_volume),
                  dsidxmean = mean(dsidx))%>%
        merge(struct_est_h%>%as.data.frame()%>%select(-c(chtmp,hX1,hX2,hconvergence)),by=c("type")),
      struct_est%>%
        merge(dealsfull%>%filter(is.na(holiday)==1,type%in%c(2,5))%>%
                group_by(type,detailed_cat_price)%>%
                mutate(lqt = quantile(total_volume,c(0.05)), uqt = quantile(total_volume,c(0.95)), sdop = sd(original_price))%>%
                filter(total_volume>lqt, total_volume<uqt)%>%
                mutate(original_price = original_price/sdop)%>%
                select(type,detailed_cat_price,total_volume,discount,original_price,prep.period,offering_duration,holiday.len,city,weekends,competitors,sdop)%>%unique(),
              all.y = TRUE,by=c("type"))%>%
        # merge(cbind(type = seq(1,11),sales_estimates[,seq(13,25)]),by="type",all.x=TRUE)%>%
        # {.->>tmp}%>%
        mutate(ch = exp(cph)+exp(chtmp))%>%mutate(cph = exp(cph))%>%
        # cbind(d2nhbar_est)%>%
        mutate(d2nhbar_est = exp(phbeta0+((phbetaalphap))*(1-discount)*original_price+(-exp(phbetalogd))*log(offering_duration)))%>%
        group_by(type,detailed_cat_price)%>%
        mutate(meanD = median((total_volume+d2nhbar_est)), meanmcph = sdop*cph*meanD/(offering_duration))%>%
        summarise(mcph = mean(meanmcph))%>%
        merge(struct_est_ph%>%as.data.frame()%>%select(-c(cph,phconvergence)),by=c("type"),all.x = TRUE),
      by=c("type","detailed_cat_price"))%>%
  mutate(hthetaalphap = -exp(hthetaalphap), phbetalogd = -exp(phbetalogd)) -> struct_results
      
      





d2nhbar_est <- vector()
for(i in 1:dim(tmp)[1]){
  d2nhbar_est[i] <- D2nhbar(ThetaAll=list(c(0),c(0),c(tmp[i,seq(3,7)],tmp[i,seq(28,40)]),c(0)),
                                  para=tmp[i,c("discount","prep.period","offering_duration")],deal_data = tmp[i,],isholiday=0)
}



for(i in c(2,5,11,10,1)){
  
  nonhol_data <- dealsfull%>%
    mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
    filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    mutate(original_price = original_price/sd(original_price))
  
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    mutate(original_price = original_price/sd(original_price))
  
  mcph[i] <- exp(mleout_preh[[2]]$par[1])*sqrt(var(nonhol_data$original_price))*
    (exp(mleout_preh[[2]]$par[2]+((mleout_preh[[2]]$par[4]))*(1-nonhol_data$discount)*original_price+mleout_preh[[2]]$par[3]*log(nonhol_data$offering_duration)))
  
}



# getcity <- t(matrix(rep(valid_est_h$city,length(sub("city",'',colnames(sales_estimates_h)[grepl("city",colnames(sales_estimates_h))]))),
#                  nrow = dim(valid_est_h)[1],byrow=FALSE) == 
#             matrix(rep(sub("city",'',colnames(sales_estimates_h)[grepl("city",colnames(sales_estimates_h))]),
#                        dim(valid_est_h)[1]),ncol = length(sub("city",'',colnames(sales_estimates_h)[grepl("city",colnames(sales_estimates_h))])),byrow = TRUE))



# type 1
industries_to_check <- c(2,5,11,10,1)
industries_to_check <- c(5)
r <- list()
D2hval <- list()
D1hvalpri <- list()
D2hvalpri <- list()
dsidx <- list()
for(i in industries_to_check){
  nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdopph}%>%
    mutate(original_price = original_price/sd(original_price))
  
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    {.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  if(i == 2){
    hol_data <- hol_data%>%filter(city!="XA")
  }else if(i %in% c(5)){
    hol_data <- hol_data%>%filter(platform!="ww",city!="GZ")
  }
  
  print(i)
  print(paste("mch = ",(exp(ceiling(mleout_h[[i]]$par[1])))*sd(sdop$original_price)))
  print(paste("mcph = ",(exp(floor(mleout_preh[[i]]$par[1])))*sd(sdopph$original_price)))
  if(i %in% c(1)){
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[type,],hol_data))
    
    D1hval[[i]] <- hol_data$total_volume*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- hol_data$total_volume*(((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))    
    
  }else if (i %in%c(2)){
    # D1hval[[i]] <- hol_data$total_volume*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
    #   (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    # 
    # D1hvalpri[[i]] <- hol_data$total_volume*(+mleout_h[[i]]$par[6])/
    #   (1+(mleout_h[[i]]$par[6]))
    
    # r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
    #   (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    # 
    
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    
    D1hval[[i]] <- hol_data$total_volume*(-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- hol_data$total_volume*(((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
        (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))

    
   }else if (i %in% c(5)){
     fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
     
    D1hval[[i]] <- hol_data$total_volume*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    D1hvalpri[[i]] <- hol_data$total_volume*(+mleout_h[[i]]$par[6])/
      (1+(mleout_h[[i]]$par[6]))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
  }else if(i %in% c(11)){
    
    D1hval[[i]] <- hol_data$total_volume*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    D1hvalpri[[i]] <- hol_data$total_volume*(+mleout_h[[i]]$par[6])/
      (1+(mleout_h[[i]]$par[6]))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
  }else if(i %in% c(10)){
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    
    D1hval[[i]] <- hol_data$total_volume*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- hol_data$total_volume*(+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
  }
  
  
  if(i %in% c(1,10)){
    
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- ((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                       -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
  }else if(i %in% c(11)){
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))
    
  }else{
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                       -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
  }
  
  
  
  if(i %in% c(1)){
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                         D2nhval[[i]])
    
    D2hvalpri[[i]] <- (((mleout_h[[i]]$par[7]))+
                      (mleout_preh[[i]]$par[2]+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                         -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration)))
    
  }else if(i %in% c(10)){
    D2hval[[i]] <- ((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                         abs(D2nhval[[i]]))
    
    D2hvalpri[[i]] <- (((mleout_h[[i]]$par[7]))+
                         (mleout_preh[[i]]$par[2]+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                            -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration)))
  }else if(i %in% c(2)){
    
    D2hval[[i]] <- exp(-exp(mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                      (log(D2nhval[[i]])))
    
  }else if(i %in% c(5)){
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7])))
    
  }else{
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7])))
    
    D2hvalpri[[i]] <- (((mleout_h[[i]]$par[7]))+
                         (mleout_preh[[i]]$par[2]+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                            -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration)))
    
    
  }
  

  
  nondealdemandhol <- D1hvalpri[[i]]
  nondealdemandnhol <- hol_data$total_volume - nondealdemandhol
  
  dealdemandhol <- D1hval[[i]]
  dealdemandnhol <- hol_data$total_volume - D1hval[[i]]
  
  dsidx[[i]] <- (-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)-
    (-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len) 
  
  avgdailydemand <- hol_data$total_volume/hol_data$holiday.len
  # print(mean(dsidx[[i]]/avgdailydemand,na.rm = TRUE))
  print(paste("dsidx = ", mean(dsidx[[i]],na.rm = TRUE)/mean(avgdailydemand)))
  #print(sd(sdop$original_price))
  #print(sd(sdop$original_price)*exp(mleout_h[[i]]$par[1]))

  print(paste("D1h = ",mean(D1hval[[i]])))
  print(paste("D2h = ", mean(D2hval[[i]])))
  print(paste("D2nh = ", mean(D2nhval[[i]])))
  #print(mean(r[[i]][r[[i]]>0]))
  print(paste("ratio = ", mean(r[[i]])))
}


mcph <- list()
for(i in industries_to_check){
  nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    {.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  
  if(i %in% c(1)){
    D2nhval[[i]] <- ((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-nonhol_data$discount)*nonhol_data$original_price+
                       -exp(mleout_preh[[i]]$par[3])*log(nonhol_data$offering_duration))
  }else{
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-nonhol_data$discount)*nonhol_data$original_price+
                       -exp(mleout_preh[[i]]$par[3])*log(nonhol_data$offering_duration))
  }
  

  mcph[[i]] <- sd(sdop$original_price)*exp(mleout_preh[[i]]$par[1])
  print(i)

  print(mcph[[i]])
  print(mean(D2nhval[[i]]))
}




testval <- vector()
val <- vector()
for(iter in 1:30){
  
  testval <- c(testval,mleout_h_ms[[iter]][[5]]$value)
  val <- c(val,exp(mleout_h_ms[[iter]][[5]]$par[1]))
  
  
}






