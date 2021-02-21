
# before using S in the equation, controlling for competition, city, platform, and num of weekends.   
# normalize duration and make prep.period to be the same percentage of the previous original duration

getIV <- function(ThetaAll,para,deal_data,D1total,D2barnh,D1,D2,isholiday,Theta_est_nh,maxS){
  if(isholiday==0){
    
  }else{
    
    # ThetaAll_nh <- list(Theta_est_nh[1],Theta_known[1:28],c(Theta_est_nh[2:6],Theta_known[13:25]),c(Theta_est_nh[8],Theta_est_nh[7]))
    
    # c <- unlist(ThetaAll[1])
    original_price <- deal_data$original_price
    D1bar <- deal_data$total_volume
    hollen <- deal_data$holiday.len
    cnh <- (Theta_est_nh[1])
    
    
    
    # D2barnh <- exp(Theta_est_nh[2]+((Theta_est_nh[4]))*(1-para[,1])*original_price+
    #                  -exp(Theta_est_nh[3])*log(para[,3]))*exp(as.numeric(control_FE(sales_est,deal_data))) 
    # 
    # 
    
    IVmax <- max(max(D1),max(D2),max(D2barnh))
    #IVmax <- max(c(D1,D2,D2barnh))
    
    
    IV_vec <-  ((1-para[,1])*original_price*D1total + original_price*D2barnh -
                  (exp(cnh)/(para[,3]-hollen))*((D1total- D1+D2barnh)^2+
                                                  exp(2*(ThetaAll[2] + ThetaAll[4]*para[,2])) + exp(2*ThetaAll[3])) +
                  original_price*D2 -
                  ((exp(ThetaAll[1]))/hollen)*((D1+D2)^2+exp(2*(ThetaAll[2] + ThetaAll[4]*para[,2]))+exp(2*ThetaAll[2])))/(maxS)
    
    
    IV_vec_exp <- exp(IV_vec)
    IV <- log(sum(IV_vec_exp))
  }
  
  
  #IV[which(IV==-Inf)] <- log(length(IV_vec))+median(IV_vec)
  #print(IV)
  return(list(IV=IV,IVmax=IVmax))
  
}



dealloglik <- function(Theta_est,Theta_known,para,deal_data,isholiday,IV,normalize,Theta_est_nh,sales_est,sales_est_h,maxS){
  
  
  if(isholiday==0){
    
  }else{
    
    
    #ThetaAll <- c(sigSh,sigSnh,gamnh)
    ThetaAll <- c(Theta_est[1],Theta_known[28],Theta_known[45],Theta_known[46],Theta_est_nh[6])
    #ThetaAll_nh <- list(Theta_est_nh[1],Theta_known[1:28],c(Theta_est_nh[2:6],Theta_known[13:25]),c(Theta_est_nh[8],Theta_est_nh[7]))
    
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends
    discprice <- (1-para[,1])*original_price
    
    
    hollen <- deal_data$holiday.len
    cnh <- (Theta_est_nh[1])
    
    # Theta_est <- c(deltac,gamma,thetaalphap, thetaT, thetad, betad, X1, X2)
    
    # D2barnh <- exp(Theta_est_nh[2]+((Theta_est_nh[4]))*(1-para[,1])*original_price+
    #                  -exp(Theta_est_nh[3])*log(para[,3])) # estimated coefficients
    
    # D2barnh <- (Theta_est_nh[2]+((Theta_est_nh[4]))*(1-para[,1])*original_price+
    #               -exp(Theta_est_nh[3])*log(para[,3])) # estimated coefficients
    
    D2barnh <- ((Theta_est_nh[2])+((Theta_est_nh[4]))*(1-para[,1])*original_price+
                  -exp(Theta_est_nh[3])*log(para[,3]))*exp(as.numeric(control_FE(sales_est,deal_data)))  # estimated coefficients
    
    D1bar <- (deal_data$total_volume/exp(sales_est_h[4]*(1-deal_data$discount)*original_price +
                                           1.2*log(deal_data$offering_duration)+
                                           12.4*log(deal_data$prep.period)))*
      exp(sales_est_h[4]*(1-para[,1])*original_price + 
            1.2*log(para[,3]) +
            12.4*log(para[,2]))
    
    # D1bar <- deal_data$total_volume
    
    fes_h <- as.numeric(control_FE_h(sales_est_h,deal_data))
    
    D1barh <- D1bar*(((Theta_est[2])*discprice+((Theta_est[3]))*log(para[,2])+((Theta_est[4]))*log(para[,3])+Theta_est[6]+ exp(fes_h)/(deal_data$total_volume-exp(fes_h)))/
                       (1+((Theta_est[2])*discprice+((Theta_est[3]))*log(para[,2])+((Theta_est[4]))*log(para[,3])+Theta_est[6]+ exp(fes_h)/(deal_data$total_volume-exp(fes_h)))))
    
    D2barh <- exp((Theta_est[5])*log(para[,3])+((Theta_est[7]))+(((D2barnh))))
    
    
    numer <- (discprice*D1bar + original_price*D2barnh -
                (exp(cnh)/(para[,3]-hollen))*((D1bar- D1barh+ D2barnh)^2+
                                                exp(2*(ThetaAll[2] + ThetaAll[4]*para[,2])) + exp(2*ThetaAll[3])) +
                original_price*D2barh -
                ((exp(Theta_est[1]))/hollen)*((D1barh+D2barh)^2+exp(2*(ThetaAll[2] + ThetaAll[4]*para[,2]))+exp(2*ThetaAll[2])))/(maxS)
    
    
    #print(numer)
    rt <- numer - IV
  }
  
  
  return(rt)
}




sample_loglik <- function(Theta_est,Theta_known,deal_data,isholiday,Theta_est_nh,sales_est,sales_est_h){
  
  if(isholiday==0){
    
    
  }else{
    
    # ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
    ThetaAll <- c(Theta_est[1],Theta_known[28],Theta_known[45],Theta_known[46],Theta_est_nh[6])
    #ThetaAll_nh <- list(Theta_est_nh[1],Theta_known[1:28],c(Theta_est_nh[2:6],Theta_known[13:25]),c(Theta_est_nh[8],Theta_est_nh[7]))
    #tic()
    single_loglik <- vector()
    
    maxSi <- max(deal_data$total_volume)
    for(i in 1:dim(deal_data)[1]){
      # print(i)
      total_volume <- deal_data[i,]$total_volume
      D1bar <- (total_volume/exp(sales_est_h[4]*(1-deal_data$discount[i])*deal_data$original_price[i] +
                                   1.2*log(deal_data$offering_duration[i]) +
                                   12.4*log(deal_data$prep.period[i])))*
        exp(sales_est_h[4]*(1-para_choice[,1])*deal_data$original_price[i] + 
              1.2*log(para_choice[,3]) +
              12.4*log(para_choice[,2]))
      
      # D1bar <- total_volume
      
      D2nhbar_t <- ((Theta_est_nh[2])+((Theta_est_nh[4]))*(1-para_choice[,1])*deal_data$original_price[i]+
                      -exp(Theta_est_nh[3])*log(para_choice[,3]))*exp(as.numeric(control_FE(sales_est,deal_data[i,])))
      
      # D2nhbar_t <- (Theta_est_nh[2]+((Theta_est_nh[4]))*(1-para_choice[,1])*deal_data$original_price[i]+
      #                 -exp(Theta_est_nh[3])*log(para_choice[,3]))
      
      fes_h <- as.numeric(control_FE_h(sales_est_h,deal_data[i,]))
      
      D1hbar_t <- D1bar*((Theta_est[2])*((1-para_choice[,1])*deal_data$original_price[i])+((Theta_est[3]))*log(para_choice[,2])+((Theta_est[4]))*log(para_choice[,3])+Theta_est[6] + exp(fes_h)/(total_volume-exp(fes_h)))/
        (1+((Theta_est[2])*((1-para_choice[,1])*deal_data$original_price[i])+((Theta_est[3]))*log(para_choice[,2])+((Theta_est[4]))*log(para_choice[,3])+Theta_est[6] + exp(fes_h)/(total_volume-exp(fes_h))))
      
      D2hbar_t <- exp((Theta_est[5])*log(para_choice[,3])+((Theta_est[7]))+(((D2nhbar_t))))
      IVout <- getIV(ThetaAll,para=para_choice,deal_data[i,],D1total=D1bar,D2barnh = D2nhbar_t,D1=(D1hbar_t),D2=(D2hbar_t),isholiday=1,Theta_est_nh,maxS=maxSi)
      
      para_t <- t(c(deal_data$discount[i],deal_data$prep.period[i],deal_data$offering_duration[i]))
      single_loglik[i] <- dealloglik(Theta_est,Theta_known,para=para_t,deal_data[i,],isholiday=1,IV = IVout$IV, normalize = IVout$IVmax,Theta_est_nh,sales_est,sales_est_h,maxS=maxSi)
    }
    #toc()
    
    counter <<- counter + 1
    if(counter%%500==0){
      print(Theta_est)
    }
    
    
  }
  
  
  
  # if(max(single_loglik)==Inf){
  #   single_loglik[which(single_loglik%in%c(Inf))] <- max(single_loglik[!which(single_loglik%in%c(Inf))])
  # }
  # if(min(single_loglik)==-Inf){
  #   single_loglik[which(single_loglik%in%c(-Inf))] <- min(single_loglik[!which(single_loglik%in%c(-Inf))])
  # }
  # single_loglik[which(single_loglik==-Inf)] <- -100
  # single_loglik[which(single_loglik==Inf)] <- 100    # max(single_loglik[single_loglik!=Inf])
  sample_loglik_neg <- -sum(single_loglik)
  
  # sample_loglik_neg <- -sum(single_loglik[!which(single_loglik%in%c(Inf))])
  
  # if(counter%%30 == 0){
  #   print("Loop")
  # }
  
  
  return(sample_loglik_neg)
}


control_FE <- function(sales_est,deal_data){
  
  weekends <- deal_data$weekends
  
  city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                   nrow = dim(deal_data)[1],byrow=FALSE) == 
              matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                         dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
  
  betapri <- sales_est[c(10,13:25)]
  # betapri[2] <- exp(beta[2])
  # betapri[3] <- -exp(beta[3])
  # betapri[4] <- -exp(beta[4])
  
  
  logD <- (betapri)%*%rbind(weekends,city)%>%as.matrix()
  
  return(logD)
  
}


control_FE_h <- function(sales_est,deal_data){
  
  
  competitors <- deal_data$competitors + 1
  platform <- t(cbind(deal_data%>%select(platform),deal_data%>%select(platform)) == 
                  matrix(rep(sub("platform",'',colnames(sales_estimates)[grepl("platform",colnames(sales_estimates))&
                                                                           !grepl(":",colnames(sales_estimates))]),dim(deal_data)[1]),ncol=2,byrow = TRUE))
  city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                   nrow = dim(deal_data)[1],byrow=FALSE) == 
              matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                         dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
  weekends <- deal_data$weekends
  pfwk <- platform*weekends
  
  
  betapri <- sales_est[c(9,10,11:12,13:25,26:27)]
  logD <- (betapri)%*%rbind(log(competitors),weekends,platform,city,pfwk)%>%as.matrix()
  
  return(logD)
  
}


counter <<- 0
max_loglik <- function(deal_data,sales_estimates,sales_estimates_h,isholiday,Theta_est_nh,siggam){
  
  # Theta_known <- sales_estimates
  # sigs_ini <- c(4,0.01)
  # Theta_est <- c(-2,sales_estimates[c(1,2,6,9,10)],sigs_ini)
  
  if(isholiday == 0){
    
  }else{
    tv <- deal_data$total_volume
    #gamh <- -3.3
    # Theta_known <- c(sales_estimates,
    #                  log(sqrt(var(tv[tv<quantile(tv,0.7)&tv>quantile(tv,0.3)]))),
    #                  sales_estimates_h[13:28],Theta_est_nh[5],gamh)
    
    
    
    gamh <- siggam[2]
    Theta_known <- c(sales_estimates,siggam[1],
                     sales_estimates_h[13:28],Theta_est_nh[5],gamh)
    
    
    #Theta_est <- c(log(median(deal_data$total_volume)),log(median(deal_data$total_volume)/2),-2,0.1)
    Theta_est <- c(Theta_est_nh[1],-log(log(abs(sales_estimates_h[c(4)]))),
                   sign(sales_estimates_h[c(6,2)])*log(abs(sales_estimates_h[c(6,2)])),
                   sign(sales_estimates_h[c(2)])*log(abs(log(abs(sales_estimates_h[c(2)])))),
                   log(mean(tv)),log(mean(tv)))
    Theta_est <- Theta_est + runif(7,-1,1)*abs(Theta_est)
    
    
    ### test on the initial point ###
    inival <- sample_loglik(Theta_est=Theta_est,Theta_known = Theta_known, deal_data= deal_data,isholiday=1,Theta_est_nh=Theta_est_nh,sales_est = sales_estimates,sales_est_h=sales_estimates_h)
    print(inival)
    skipct <- 0
    while(inival %in%c(-Inf,Inf,NaN)&skipct <=20){
      print("skip")
      Theta_est <- c(Theta_est_nh[1],-log(log(abs(sales_estimates_h[c(4)]))),
                     sign(sales_estimates_h[c(6,2)])*log(abs(sales_estimates_h[c(6,2)])),
                     sign(sales_estimates_h[c(2)])*log(abs(log(abs(sales_estimates_h[c(2)])))),
                     log(mean(tv)),log(mean(tv)))
      Theta_est <- Theta_est + runif(7,-1,1)*abs(Theta_est)
      inival <- sample_loglik(Theta_est=Theta_est,Theta_known = Theta_known, deal_data= deal_data,isholiday=1,Theta_est_nh=Theta_est_nh,sales_est = sales_estimates,sales_est_h=sales_estimates_h)
      print(inival)
      skipct <- skipct+1
    }
    print(Theta_est)
    
    tryCatch(optim(
      par = Theta_est, fn=sample_loglik,
      #gr = gr,
      #grad = gr_num,
      #ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1), c(0, 0, -1)), ci = c(0, -1, 0, 0,-50),
      Theta_known = Theta_known, deal_data= deal_data,isholiday=1,Theta_est_nh=Theta_est_nh,sales_est = sales_estimates,sales_est_h = sales_estimates_h,
      # method = "BFGS",
      method = "Nelder-Mead",
      control = list(factr = 1e-3, maxit = 5000)) -> optimTheta)
    
  }
  
  
  
  return(optimTheta)
}


summary(lm(data=hol_data%>%mutate(discprice = (1-discount)*original_price/partysize, # = price/partysize
                                  logvol = log(total_volume),
                                  perperson = original_price/partysize,
                                  competitors_ct = (competitors/sd(competitors)),
                                  logcomp = ifelse(competitors!=0,log(competitors),0),
                                  fixlength = ifelse(offering_duration==15,1,
                                                     ifelse(offering_duration==30,2,
                                                            ifelse(offering_duration==45,3,0)))),
           logvol~-1+log(offering_duration)+#I(log(offering_duration)^2)+
             discprice+#I(discprice^2)+
             log(prep.period) + I(log(prep.period)^2)
           # log(discprice)+I(log(discprice)^2)+
           #I(log(original_price))+I(1/log(original_price))+I(1/(log(original_price)^2))+
           #logcomp+weekends+platform:weekends+
           #platform+city
)
)

