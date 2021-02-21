mleout_preh_1 <- list()
#mleout_h <- list()
predays <- 15
pre.windows <- c(seq(21-predays,21),seq(44-predays,44),seq(92-predays,92))


#mleout_preh_ms <- list()

#txtStart("test.txt")
#2,5,7,11,1,10
#2,5,7,11,1,10,6,8,4,3
for(i in industries_to_est){
  print(i)
  if(i %in%industries_to_est){
    for(iter in 10:NumIter){
      # nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
      #   filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      #   mutate(original_price = original_price/sd(original_price))
      #

      nonhol_data <- dealsfull%>%
        mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
        filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
        filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
        mutate(original_price = original_price/sd(original_price))
      

      disc_alpha <- unique(round(dealsfull_type$discount,digits = 2))
      disc_T <- unique(dealsfull_type$prep.period)
      disc_d <- unique(dealsfull_type$offering_duration)
      #para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)
      para_choice <- unique(dealsfull_type%>%select(discount,prep.period,offering_duration))
      
      print(dim(nonhol_data))
      tryCatch(
        {tic()
          max_loglik(deal_data=nonhol_data,sales_estimates[i,],sales_estimates_h[i,],isholiday=0,Theta_est_nh = 0,siggam=gamma_preh[i,]) -> mleout_preh_1[[iter]]
          toc()}
      )
      
      mleout_preh_ms[[iter]][[i]] <- mleout_preh_1[[iter]]
    }
  }
  
  
}

#txtStop()

# type 2, no sign restrictions on the coefficients
# type 5, sign restriction on logd


#mleout_preh <- list()
mlefnvalue_ls <- list()
for(i in industries_to_est){
  mlefnvalue <- vector()
  for(iter in 1:NumIter){
    mlefnvalue <- c(mlefnvalue,mleout_preh_ms[[iter]][[i]]$value)
  }
  mlefnvalue_ls[[i]] <- mlefnvalue
  mleout_preh[[i]] <- mleout_preh_ms[[which(mlefnvalue==min(mlefnvalue))]][[i]]
}
