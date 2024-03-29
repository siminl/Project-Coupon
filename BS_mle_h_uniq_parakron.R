
#mleout_preh <- list()
#mleout_h <- list()

  
  
  predays <- 15
  pre.windows <- c(seq(21-predays,21),seq(44-predays,44),seq(92-predays,92))
  
  mleout_h_1 <- list()
  #mleout_preh_ms <- list()
  #mleout_h_ms <- list(list())


  
  #txtStart("test.txt")
  #2,5,7,11,1,10
for(i in industries_to_est){
    print(i)
    
    nonhol_data <- dealsfull%>%
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
      #filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      mutate(original_price = original_price/sd(original_price))
    
    # disc_alpha <- unique(round(dealsfull_type$discount,digits = 2))
    # disc_T <- unique(dealsfull_type$prep.period)
    # disc_d <- unique(dealsfull_type$offering_duration)
    # para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)
    # 
    # print(dim(nonhol_data))
    # tryCatch(
    #   {tic()
    #     max_loglik(deal_data=nonhol_data,sales_estimates[i,],sales_estimates_h[i,],isholiday=0,Theta_est_nh = 0) -> mleout_preh[[i]]
    #     toc()}
    # )
    
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    set.seed(100)
    rs <-list()
    for(b in 1:Bsamples){
      rs[[b]] <- sample(dim(hol_data)[1],dim(hol_data)[1],replace = T)
    }

  
    
    foreach(b = 1:Bsamples) %dopar% {
      print(b)
      
      hol_data_bs <- hol_data[rs[[b]],]%>%
        mutate(original_price = original_price/sd(original_price))
      
      
    
    
    if(i == 2){
      hol_data_bs <- hol_data_bs%>%filter(city!="XA")
    }else if(i %in% c(10)){
      hol_data_bs <- hol_data_bs%>%filter(platform!="ww")
    }else if(i %in% c(5)){
      #hol_data <- hol_data%>%filter(platform!="ww",city!="GZ")
    }
    
    disc_alpha <- unique(round(hol_data_bs$discount,digits = 2))
    disc_T <- unique(hol_data_bs$prep.period)
    disc_d <- unique(hol_data_bs$offering_duration)
    para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)%>%
      filter(d>(T))
    print(dim(para_choice))
    
    # para_choice <- unique(hol_data%>%select(discount,prep.period,offering_duration))
    # print(dim(para_choice))
    
    set.seed(100)
    if(i %in%industries_to_est){
      foreach (iter = 1:NumIter)%dopar% { # 4
        print(iter)
        
  
        
        # sigSnh <- log(sqrt(var(nonhol_data$total_volume[nonhol_data$total_volume<quantile(nonhol_data$total_volume,0.7)&
        #                                                   nonhol_data$total_volume>quantile(nonhol_data$total_volume,0.3)])))
        
        sigSnh <- gamma_preh[i,1]
        gamnh <- gamma_preh[i,2]
        Theta_est_nh <- c(mleout_preh[[i]]$par,sigSnh,gamnh)
        
        
        print(dim(hol_data_bs))
        tryCatch(
          {tic()
            max_loglik(deal_data=hol_data_bs,sales_estimates[i,],sales_estimates_h[i,],
                       isholiday=1,Theta_est_nh=Theta_est_nh,siggam=gamma_h[i,],mleout_h_ini = mleout_h[[i]]$par) -> mleout_h_1[[iter]]
            toc()}
        )
        
        
        #mleout_preh_ms[[iter]] <- mleout_preh
        mleout_h_ms[[iter]][[i]] <- mleout_h_1[[iter]]
      }
    }
    mleout_h_ms_bs[[i]][[b]] <- mleout_h_ms[[iter]][[i]]
    
  }
}


#txtStop()

