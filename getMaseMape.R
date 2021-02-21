obj_func <- function(para_est,deal_data,type,mleout_preh,out_h){
  
  
  
  cnh <- (mleout_preh[[type]]$par[1])
  
  nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==type)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%#{.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  
  
  
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==type,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    mutate(original_price = original_price/sd(original_price))
  
  # fes <- rep(0,dim(nonhol_data)[1])
  # for(row in 1:dim(nonhol_data)[1]){
  #   fes[row] <- control_FE(sales_estimates[i,],nonhol_data[row,])
  # }
  # 
  # # if(type == 2){
  # #   hol_data <- hol_data%>%filter(city!="XA")
  # # }
  # 
  # fes_h <- rep(0,dim(hol_data)[1])
  # for(row in 1:dim(hol_data)[1]){
  #   fes_h[row] <- control_FE_h(sales_estimates_h[i,],hol_data[row,])
  # }
  # 
  fesi <- as.numeric(control_FE(sales_estimates[type,],deal_data))
  fes_hi <- as.numeric(control_FE_h(sales_estimates_h[type,],deal_data))
  
  
  if(type %in% c(2)){
    D1S <- (deal_data$total_volume/exp(-3.0*(1-deal_data$discount)*deal_data$original_price +
                                         0.9*log(deal_data$offering_duration) + 
                                         12.3*log(deal_data$prep.period)))*
      exp(-3.0*(1-para_est[,1])*deal_data$original_price + 
            0.9*log(para_est[,3]) + 
            12.3*log(para_est[2]))
    
    
    D1S <- (deal_data$total_volume/exp(-4.47*(1-deal_data$discount)*deal_data$original_price +
                                         3.36*log(deal_data$offering_duration) + 
                                         2.60*(deal_data$prep.period)))*
      exp(-4.47*(1-para_est[,1])*deal_data$original_price + 
            3.36*log(para_est[,3]) + 
            2.60*(para_est[2]))
    
    # D1S <- (deal_data$total_volume/exp(-3.0*(1-deal_data$discount)*deal_data$original_price +
    #                                      0.5*(sales_estimates_h[type,2])*log(deal_data$offering_duration) + 
    #                                      abs(sales_estimates_h[type,6])*log(deal_data$prep.period)))*
    #   exp(-3.0*(1-para_est[,1])*deal_data$original_price + 
    #         0.5*(sales_estimates_h[type,2])*log(para_est[,3]) + 
    #         abs(sales_estimates_h[type,6])*log(para_est[2]))
    
  }else if(type %in% c(5)){
    
    D1S <- (deal_data$total_volume/exp(sales_estimates_h[type,4]*(1-deal_data$discount)*deal_data$original_price +
                                         1.24*log(deal_data$offering_duration) +
                                         10.7*log(deal_data$prep.period)))*
      exp(sales_estimates_h[type,4]*(1-para_est[,1])*deal_data$original_price +
            1.24*log(para_est[,3]) +
            10.7*log(para_est[2]))
    
    D1S <- (deal_data$total_volume/exp(-1.40*(1-deal_data$discount)*deal_data$original_price +
                                         6.33*log(deal_data$offering_duration) +
                                         0.88*(deal_data$prep.period)))*
      exp(-1.40*(1-para_est[,1])*deal_data$original_price +
            6.33*log(para_est[,3]) +
            0.88*(para_est[2]))
    
    
    
    #  D1S <- (deal_data$total_volume/exp(sales_estimates_h[type,4]*(1-deal_data$discount)*deal_data$original_price +
    #                                       (sales_estimates_h[type,2])*log(deal_data$offering_duration) +
    #                                       30*abs(sales_estimates_h[type,6])*log(deal_data$prep.period)))*
    #    exp(sales_estimates_h[type,4]*(1-para_est[,1])*deal_data$original_price +
    #         (sales_estimates_h[type,2])*log(para_est[,3]) +
    #          30*abs(sales_estimates_h[type,6])*log(para_est[2]))
    
    # D1S <- (deal_data$total_volume)
  }else if(type %in% c(10)){
    D1S <- (deal_data$total_volume/exp(sales_estimates_h[type,4]*(1-deal_data$discount)*deal_data$original_price +
                                         1.7*log(deal_data$offering_duration) + 
                                         10.5*log(deal_data$prep.period)))*
      exp(sales_estimates_h[type,4]*(1-para_est[,1])*deal_data$original_price + 
            1.7*log(para_est[,3]) + 
            10.5*log(para_est[2]))
    
    # D1S <- (deal_data$total_volume/exp(-1.0*(1-deal_data$discount)*deal_data$original_price))*
    #   exp(-1.0*(1-para_est[,1])*deal_data$original_price )
    
    D1S <- (deal_data$total_volume/exp(-1.40*(1-deal_data$discount)*deal_data$original_price +
                                         # 6.33*log(deal_data$offering_duration) +
                                         0.69*(deal_data$prep.period)))*
      exp(-1.40*(1-para_est[,1])*deal_data$original_price +
            # 6.33*log(para_est[,3]) +
            0.69*(para_est[2]))
    
    
    
    # D1S <- (deal_data$total_volume/exp(sales_estimates_h[type,4]*(1-deal_data$discount)*deal_data$original_price +
    #                                      (sales_estimates_h[type,2])*log(deal_data$offering_duration) + 
    #                                      30*abs(sales_estimates_h[type,6])*log(deal_data$prep.period)))*
    #   exp(sales_estimates_h[type,4]*(1-para_est[,1])*deal_data$original_price + 
    #         (sales_estimates_h[type,2])*log(para_est[,3]) + 
    #         30*abs(sales_estimates_h[type,6])*log(para_est[2]))
    
  }else if(type %in% c(1)){
    
    # D1S <-  (deal_data$total_volume/exp(sales_estimates_h[type,4]*(1-deal_data$discount)*deal_data$original_price +
    #                                       1.2*log(deal_data$offering_duration)+
    #                                       1.24*log(deal_data$prep.period)))*
    #   exp(sales_estimates_h[type,4]*(1-para_est[,1])*deal_data$original_price + 
    #         1.2*log(para_est[,3]) +
    #         1.24*log(para_est[,2]))
    
    D1S <- (deal_data$total_volume/exp(sales_estimates_h[type,4]*(1-deal_data$discount)*deal_data$original_price +
                                         (sales_estimates_h[type,2])*log(deal_data$offering_duration) +
                                         abs(sales_estimates_h[type,6])*log(deal_data$prep.period)))*
      exp(sales_estimates_h[type,4]*(1-para_est[,1])*deal_data$original_price +
            (sales_estimates_h[type,2])*log(para_est[,3]) +
            abs(sales_estimates_h[type,6])*log(para_est[2]))
    
    
    D1S <- (deal_data$total_volume/exp(-3.2*(1-deal_data$discount)*deal_data$original_price +
                                         (0.04)*log(deal_data$offering_duration) +
                                         (1.03)*(deal_data$prep.period)))*
      exp(-3.2*(1-para_est[,1])*deal_data$original_price +
            (0.04)*log(para_est[,3]) +
            abs(1.03)*(para_est[2]))
    
  }else{
    D1S <- (deal_data$total_volume/exp(sales_estimates_h[type,4]*(1-deal_data$discount)*deal_data$original_price +
                                         1.2*log(deal_data$offering_duration)+
                                         10.0*log(deal_data$prep.period)))*
      exp(sales_estimates_h[type,4]*(1-para_est[,1])*deal_data$original_price +
            1.2*log(para_est[,3]) +
            10.0*log(para_est[,2]))
    
    
    
    D1S <- (deal_data$total_volume/exp(-3.01*(1-deal_data$discount)*deal_data$original_price +
                                         1.0*log(deal_data$offering_duration)+
                                         2.8*(deal_data$prep.period)))*
      exp(-3.01*(1-para_est[,1])*deal_data$original_price +
            1.0*log(para_est[,3]) +
            2.8*(para_est[,2]))
    
    
    # D1S <- (deal_data$total_volume/exp(sales_estimates_h[type,4]*(1-deal_data$discount)*deal_data$original_price +
    #                                      (sales_estimates_h[type,2])*log(deal_data$offering_duration) +
    #                                      abs(sales_estimates_h[type,6])*log(deal_data$prep.period)))*
    #   exp(sales_estimates_h[type,4]*(1-para_est[,1])*deal_data$original_price +
    #         (sales_estimates_h[type,2])*log(para_est[,3]) +
    #         abs(sales_estimates_h[type,6])*log(para_est[2]))
    
  }
  
  if(type %in% c(1)){
    D2barnh <- ((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-para_est[1])*deal_data$original_price+
                  -exp(mleout_preh[[type]]$par[3])*log(para_est[3]))*exp(fesi)
  }else if(type %in% c(10)){
    D2barnh <- ((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-para_est[1])*deal_data$original_price+
                  -exp(mleout_preh[[type]]$par[3])*log(para_est[3]))*exp(fesi)
  }else if(type %in% c(5)){
    # D2barnh <- exp(-exp(mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-para_est[1])*deal_data$original_price+
    #               -exp(mleout_preh[[type]]$par[3])*log(para_est[3]))*exp(fes)
    
    D2barnh <- ((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-para_est[1])*deal_data$original_price+
                  -exp(mleout_preh[[i]]$par[3])*log(para_est[3]))*exp(as.numeric(fesi))
    
  }else if(type %in% c(11)){
    D2barnh <- (mleout_preh[[type]]$par[2]+((mleout_preh[[type]]$par[4]))*(1-para_est[1])*deal_data$original_price+
                  -exp(mleout_preh[[type]]$par[3])*log(para_est[3]))*exp(as.numeric(fesi))
  }else{
    D2barnh <- (mleout_preh[[type]]$par[2]+((mleout_preh[[type]]$par[4]))*(1-para_est[1])*deal_data$original_price+
                  -exp(mleout_preh[[i]]$par[3])*log(para_est[3]))*exp(as.numeric(fesi))
    
  }
  
  
  if(type %in% c(1)){
    D1h <- deal_data$total_volume*((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi)))/
      (1+((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi))))
    
    
  }else if (type %in%c(2)){
    D1h <- deal_data$total_volume*((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi)))/
      (1+((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi))))
    
  }else if (type %in% c(5)){
    
    # D1h <- deal_data$total_volume*((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6])/
    #   (1+((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6]))
    # 
    
    D1h <- deal_data$total_volume*((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+(-exp(out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi)))/
      (1+((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+(-exp(out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi))))
    
  }else if(type %in% c(11)){
    
    D1h <- deal_data$total_volume*((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6])/
      (1+((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6] ))
    
  }else if(i %in% c(10)){
    D1h <- deal_data$total_volume*((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+(-exp(out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi)))/
      (1+((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+(-exp(out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi))))
    
    
  }
  
  
  if(i %in% c(1)){
    D2h <- exp((out_h$par[5])*log(para_est[3])+((out_h$par[7]))+
                 D2barnh)
    
  }else if(i %in% c(2)){
    D2h <- exp((out_h$par[5])*log(para_est[3])+((out_h$par[7]))+
                 (D2barnh))
  }else if(i %in% c(5)){
    D2h <- exp((out_h$par[5])*log(para_est[3])+((out_h$par[7])))
  }else if(i %in% c(10)){
    
    D2h <-  ((out_h$par[5])*log(para_est[,3])+(out_h$par[7]))
  }else{
    D2h <- exp((out_h$par[5])*log(para_est[3])+((out_h$par[7])))
    
    
  }
  
  # ThetaAll <- c(c,sigh,signh,gammah,gammanh)
  
  rt <- ((1-para_est[1])*deal_data$original_price*D1S + deal_data$original_price*D2barnh -
           (exp(cnh)/(para_est[3]-deal_data$holiday.len))*((D1S- D1h+D2barnh)^2+
                                                             exp(2*(gamma_h[type,1] + gamma_h[type,2]*para_est[2])) + exp(2*gamma_preh[type,1])) +
           deal_data$original_price*D2 -
           ((exp(out_h$par[1]))/deal_data$holiday.len)*((D1h+D2h)^2+exp(2*(gamma_h[type,1] + gamma_h[type,2]*para_est[2]))+exp(2*gamma_h[type,1])))
  
  
  
  return(rt)
  
}


getMaseMape <- function(hol_data,output_ls){
  
  optimparas_ls <- vector(mode = "list", length = 11)
  
  disc_alpha <- unique(round(hol_data$discount,digits = 2))
  disc_T <- unique(hol_data$prep.period)
  disc_d <- unique(hol_data$offering_duration)
  
  para_choice <- unique(hol_data%>%select(discount,prep.period,offering_duration))
  
  for(kiter in 1:length(output_ls)){
    print(kiter)
    objval <- rep(0,dim(para_choice)[1])
    for(j in 1:dim(hol_data)[1]){
      
      objval <- cbind(objval,obj_func(para_choice,deal_data = hol_data[j,],type=i,mleout_preh,output_ls[[kiter]]))
      #print(dim(obj_func(para_choice,deal_data = hol_data[j,])))
    }
    
    
    
    paras <- para_choice[apply(objval[,-1],2,which.max),c("discount","prep.period","offering_duration")]
    obs <- hol_data[,c("discount","prep.period","offering_duration")]%>%mutate(discount = round(discount,digits = 2))
    
   
    # optimparas[[i]][[kiter]] <- cbind(paras,obs,sdop%>%select(total_volume,original_price))
    optimparas_ls[[kiter]] <- cbind(paras,obs)
  }
  
  mase <- c(0,0,0)
  trimeddf <- list()
  dimvals <- vector()
  
  for(kiter in 1:length(optimparas_ls)){
    print(kiter)
    base.mase <- 1
    valsvec <- optimparas_ls[[kiter]]
    
    while(base.mase > 0.9){
      vals <- vector()
      for(j in 1:dim(valsvec)[1]){
        
        paradf <- valsvec[-j,1:3]
        obsdf <- valsvec[-j,4:6]
        
        vals <- c(vals,max(colSums(abs(paradf-obsdf))/
                             colSums(abs(obsdf-t(kronecker(t(rep(1,dim(obsdf)[1])),colMeans(obsdf,2)))))))
        
        
        # val1 <- (sum(tmp[,1])/sum(tmp[,4]))
        # val2 <- (sum(tmp[,2])/sum(tmp[,5]))
        # val3 <- (sum(tmp[,3])/sum(tmp[,6]))
        # 
        
        
      }
      # print(((colSums(abs(paradf-obsdf))/
      #                       colSums(abs(obsdf-t(kronecker(t(rep(1,dim(obsdf)[1])),colMeans(obsdf,2))))))))
      
      jidx <- which.min(vals)
      base.mase <- min(vals)
      valsvec <- valsvec[-jidx,]
      
    }
    paradf <- valsvec[,1:3]
    obsdf <- valsvec[,4:6]
    
    finalmase <- (colSums(abs(paradf-obsdf))/
                    colSums(abs(obsdf-t(kronecker(t(rep(1,dim(obsdf)[1])),colMeans(obsdf,2))))))
    
    
    trimeddf[[i]] <- valsvec
    dimvals <- c(dimvals, dim(valsvec)[1]/dim(optimparas[[i]][[kiter]])[1])
    mase <- rbind(mase,finalmase)
    
  }
  
  mase_dim <- dimvals
  
  mape <- c(0,0,0)
  trimeddf <- list()
  dimvals <- vector()
  
  for(kiter in 1:length(optimparas_ls)){
    print(kiter)
    base.mape <- 1
    valsvec <- optimparas_ls[[kiter]]
    
    while(base.mape > 0.3){
      vals <- vector()
      for(j in 1:dim(valsvec)[1]){
        
        paradf <- valsvec[-j,1:3]
        obsdf <- valsvec[-j,4:6]
        
        vals <- c(vals,max(colMeans(abs(paradf-obsdf)/obsdf,2)))
        
        
        # val1 <- (sum(tmp[,1])/sum(tmp[,4]))
        # val2 <- (sum(tmp[,2])/sum(tmp[,5]))
        # val3 <- (sum(tmp[,3])/sum(tmp[,6]))
        # 
        
        
      }
      # print((colMeans(abs(paradf-obsdf)/obsdf,2)))
      
      jidx <- which.min(vals)
      base.mape <- min(vals)
      valsvec <- valsvec[-jidx,]
      
    }
    paradf <- valsvec[,1:3]
    obsdf <- valsvec[,4:6]
    
    finalmape <- colMeans(abs(paradf-obsdf)/obsdf,2)
    
    
    trimeddf[[i]] <- valsvec
    dimvals <- c(dimvals, dim(valsvec)[1]/dim(optimparas[[i]][[kiter]])[1])
    mape <- rbind(mape,finalmape)
    
  }
  
  mape_dim <- dimvals
  return(list(mase = mase, mape = mape, mase_dim=mase_dim, mape_dim= mape_dim))
}

