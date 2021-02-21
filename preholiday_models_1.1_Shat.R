

getIV <- function(ThetaAll,para,deal_data,D1,D2,isholiday,Theta_est_nh){
  if(isholiday==0){
    c <- unlist(ThetaAll[1])
    original_price <- deal_data$original_price
    siggam <- unlist(ThetaAll[4])
    IVmax <- max(max(D1),max(D2))
    
    IV_vec <- (((1-para[,1])*original_price*D1 + original_price*D2 - 
                  (exp(c)/para[,3])*((D1+D2)^2+exp(2*siggam[1])))) 
    # IV_vec_max <- (max(abs(IV_vec)[abs(IV_vec)!=max(abs(IV_vec))]))
    #IV_vec_max <- quantile(abs(IV_vec),0.8)
    
    #IV_vec%>%as.data.frame()%>%mutate(expV1 = exp(V1)) ->tmp
    
    
    IV_vec_exp <- exp(IV_vec/D1)
    
    IV <- log(sum(IV_vec_exp))
  }
  
  return(list(IV=IV,IVmax=IVmax,IV_vec_max=IV_vec_max))
  
}


dealloglik <- function(Theta_est,Theta_known,para,deal_data,isholiday,IV,normalize,Theta_est_nh,sales_est){
  
  
  if(isholiday==0){
    
    # ThetaAll <- list(Theta_est[1],Theta_known[-28],c(Theta_est[2:6],Theta_known[13:25]),c(Theta_known[28],Theta_est[7]))
    
    
    ThetaAll <- list(Theta_est[1],0,c(Theta_est[2:4]),Theta_known[1:2])
    
    c <- unlist(ThetaAll[1])
    siggam <- unlist(ThetaAll[4])
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends
    discprice <- (1-para[,1])*original_price
    
    total_volume <- deal_data$total_volume
    # D1bar <- total_volume
    
    D1bar <- (total_volume/exp(sales_est[4]*(1-deal_data$discount)*(deal_data$original_price)+
                                 -1.8*log(deal_data$offering_duration)))*
      exp(sales_est[4]*(1-para[,1])*(deal_data$original_price)+
            -1.8*log(para[,3]))
    # D2bar <- (D2nhbar(ThetaAll,para,deal_data,isholiday=0))
    # (Theta_est[2])
    D2bar <- ((Theta_est[2])+(-exp(Theta_est[4]))*(1-para[,1])*original_price+
                -exp(Theta_est[3])*log(para[,3]))*exp(as.numeric(control_FE(sales_est,deal_data)))
    numer <- (discprice*D1bar + original_price*D2bar -
                (exp(c)/para[,3])*((D1bar+D2bar)^2+exp(2*siggam[1])))/D1bar
    
    #print(numer)
    rt <- (numer - IV)
  }
  return(rt)
}




sample_loglik <- function(Theta_est,Theta_known,deal_data,isholiday,Theta_est_nh,sales_est){
  
  if(isholiday==0){
    
    
    ThetaAll <- list(Theta_est[1],0,c(Theta_est[2:4]),Theta_known[1:2])
    single_loglik <- vector()
    
    for(i in 1:dim(deal_data)[1]){
      #print(i)
      total_volume <- deal_data[i,]$total_volume
      # D1nhbar_t <- rep(total_sales,dim(para_choice)[1])
      D1nhbar_t <- rep(total_volume/exp(sales_est[4]*(1-deal_data[i,]$discount)*(deal_data[i,]$original_price)+
                                          -1.8*log(deal_data[i,]$offering_duration)),dim(para_choice)[1])*
        exp(sales_est[4]*(1-para_choice[,1])*(deal_data[i,]$original_price)+
              -1.8*log(para_choice[,3]))
  
        
      #D2nhbar_t <- D2nhbar(ThetaAll,para=para_choice,deal_data[i,],isholiday)
      D2nhbar_t <- ((Theta_est[2])+(-exp(Theta_est[4]))*(1-para_choice[,1])*deal_data[i,]$original_price+
                      -exp(Theta_est[3])*log(para_choice[,3]))*exp(as.numeric(control_FE(sales_est,deal_data[i,])))
      IVout <- getIV(ThetaAll,para=para_choice,deal_data[i,],D1=(total_volume),D2=(D2nhbar_t),isholiday,Theta_est_nh)
      #This should actually be, IVout <- getIV(ThetaAll,para=para_choice,deal_data[i,],D1=(D1nhbar_t),D2=(D2nhbar_t),isholiday,Theta_est_nh)
      
      
      para_t <- t(c(deal_data$discount[i],deal_data$prep.period[i],deal_data$offering_duration[i]))
      single_loglik[i] <- dealloglik(Theta_est,Theta_known,para=para_t,deal_data[i,],isholiday,IV = IVout$IV, normalize = IVout$IV_vec_max,Theta_est_nh,sales_est)
    }
    
  }
  
  
  
  # if(max(single_loglik)==Inf){
  #   single_loglik[which(single_loglik%in%c(Inf))] <- max(single_loglik[!which(single_loglik%in%c(Inf))])
  # }
  # if(min(single_loglik)==-Inf){
  #   single_loglik[which(single_loglik%in%c(-Inf))] <- min(single_loglik[!which(single_loglik%in%c(-Inf))])
  # }
  # single_loglik[which(single_loglik==-Inf)] <- 0
  # single_loglik[which(single_loglik==Inf)] <- 100
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



counter <<- 0
max_loglik <- function(deal_data,sales_estimates,sales_estimates_h,isholiday,Theta_est_nh,siggam){
  
  
  if(isholiday == 0){
    
    tv <- deal_data$total_volume
    # Theta_est <- c(-1.5,sales_estimates[c(1,2,6,9,10)])
    # Theta_known <- c(sales_estimates[13:25],log(sqrt(var(tv[tv<quantile(tv,0.7)&tv>quantile(tv,0.3)]))),-2.3)
    Theta_est <- c(-5,log(mean(tv)),log(abs(sales_estimates[c(2,4)])))
    # Theta_known <- c(log(sqrt(var(tv[tv<quantile(tv,0.7)&tv>quantile(tv,0.3)]))),-2.3)
    Theta_known <- siggam
 
    Theta_est <- Theta_est + runif(4,-2,2)*abs(Theta_est)
    
    ### test on the initial point ###
    inival <- sample_loglik(Theta_est=Theta_est,Theta_known = Theta_known, deal_data= deal_data,isholiday=0,Theta_est_nh=0,sales_estimates)
    while(inival%in%c(-Inf,Inf,NaN)){
      print("skip")
      Theta_est <- c(-5,log(mean(tv)),log(abs(sales_estimates[c(2,4)])))
      Theta_est <- Theta_est + runif(4,-2,2)*abs(Theta_est)
      inival <- sample_loglik(Theta_est=Theta_est,Theta_known = Theta_known, deal_data= deal_data,isholiday=0,Theta_est_nh=0,sales_estimates)
    }
    print(Theta_est)
    
    tryCatch(optim(
      par = Theta_est, fn=sample_loglik,
      #gr = gr,
      #grad = gr_num,
      #ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1), c(0, 0, -1)), ci = c(0, -1, 0, 0,-50),
      Theta_known = Theta_known, deal_data= deal_data,isholiday=0,Theta_est_nh=0,sales_est=sales_estimates,
      # method = "BFGS",
      method = "Nelder-Mead",
      control = list(factr = 1e-8, maxit = 5000)) -> optimTheta)
    
  }
  
  
  
  return(optimTheta)
}


summary(lm(data=nonhol_data%>%mutate(discprice = (1-discount)*original_price/partysize, # = price/partysize
                          logvol = log(total_volume),
                          perperson = original_price/partysize,
                          competitors_ct = (competitors/sd(competitors)),
                          logcomp = ifelse(competitors!=0,log(competitors),0),
                          fixlength = ifelse(offering_duration==15,1,
                                             ifelse(offering_duration==30,2,
                                                    ifelse(offering_duration==45,3,0)))),
   logvol~-1+log(offering_duration)+#I(log(offering_duration)^2)+
     discprice#+I(discprice^2)+
     # log(discprice)+I(log(discprice)^2)+
     #I(log(original_price))+I(1/log(original_price))+I(1/(log(original_price)^2))+
     #logcomp+weekends+platform:weekends+
     #platform+city
   )
  )
