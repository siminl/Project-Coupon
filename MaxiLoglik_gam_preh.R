
D2nhbar <- function(ThetaAll,para,deal_data,isholiday){
  if(isholiday%in%c(0,1)){
    # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
    # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
    # parai <- c(alphai, Ti, di)
    # parak <- c(alphak, Tk, dk)
    # obsi <- c(pricei,)
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    siggam <- unlist(ThetaAll[4])
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends
    discprice <- (1-para[,1])*original_price
    
    city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                     nrow = dim(deal_data)[1],byrow=FALSE) == 
                matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                           dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
    
    betapri <- beta
    # betapri[2] <- exp(beta[2])
    # betapri[3] <- -exp(beta[3])
    # betapri[4] <- -exp(beta[4])
    
    
    logD <- t(betapri)%*%rbind(rep(1,dim(para)[1]),
                               t(log(para[,3])),  
                               rep(log(original_price),dim(para)[1]),
                               rep(log(competitors),dim(para)[1]),
                               rep(weekends,dim(para)[1]),
                               kronecker(t(rep(1,dim(para)[1])),city))%>%as.matrix()
    
  }
  
  D <- exp(logD)
  return(D)
  
}

getIV <- function(ThetaAll,para,deal_data,D1,D2,isholiday,Theta_est_nh){
  if(isholiday==0){
    c <- unlist(ThetaAll[1])
    original_price <- deal_data$original_price
    siggam <- unlist(ThetaAll[4])
    IVmax <- max(max(D1),max(D2))
    
    IV_vec <- (((1-para[,1])*original_price*D1 + original_price*D2 - 
                  (exp(c)/para[,3])*((D1+D2)^2+exp(2*siggam[1]))))
    
    #IV_vec%>%as.data.frame()%>%mutate(expV1 = exp(V1)) ->tmp
    
    IV_vec_exp <- exp(IV_vec)
    IV <- log(sum(IV_vec_exp))
  }
  
  return(list(IV=IV,IVmax=IVmax))
  
}


dealloglik <- function(Theta_est,Theta_known,para,deal_data,isholiday,IV,normalize,Theta_est_nh){
  
  
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
  
    #D2bar <- (D2nhbar(ThetaAll,para,deal_data,isholiday=0))
    D2bar <- exp(Theta_est[2]+((Theta_est[4]))*(1-para[,1])*original_price+
                   -exp(Theta_est[3])*log(para[,3]))
    numer <- (discprice*total_volume + original_price*D2bar -
                (exp(c)/para[,3])*((total_volume+D2bar)^2+exp(2*siggam[1])))
    
    #print(numer)
    rt <- numer - IV
  }
  return(rt)
}




sample_loglik <- function(Theta_est,Theta_known,deal_data,isholiday,Theta_est_nh){
  
  if(isholiday==0){

    
    ThetaAll <- list(Theta_est[1],0,c(Theta_est[2:4]),Theta_known[1:2])
    single_loglik <- vector()
    
    for(i in 1:dim(deal_data)[1]){
      #print(i)
      total_sales <- deal_data[i,]$total_volume
      D1nhbar_t <- rep(total_sales,dim(para_choice)[1])
      #D2nhbar_t <- D2nhbar(ThetaAll,para=para_choice,deal_data[i,],isholiday)
      D2nhbar_t <- exp((Theta_est[2])+((Theta_est[4]))*(1-para_choice[,1])*deal_data[i,]$original_price+
                         -exp(Theta_est[3])*log(para_choice[,3]))
      IVout <- getIV(ThetaAll,para=para_choice,deal_data[i,],D1=(total_sales),D2=(D2nhbar_t),isholiday,Theta_est_nh)
      
      para_t <- t(c(deal_data$discount[i],deal_data$prep.period[i],deal_data$offering_duration[i]))
      single_loglik[i] <- dealloglik(Theta_est,Theta_known,para=para_t,deal_data[i,],isholiday,IV = IVout$IV, normalize = IVout$IVmax,Theta_est_nh)
    }
    
  }
  
  
  
  # if(max(single_loglik)==Inf){
  #   single_loglik[which(single_loglik%in%c(Inf))] <- max(single_loglik[!which(single_loglik%in%c(Inf))])
  # }
  # if(min(single_loglik)==-Inf){
  #   single_loglik[which(single_loglik%in%c(-Inf))] <- min(single_loglik[!which(single_loglik%in%c(-Inf))])
  # }
  single_loglik[which(single_loglik==-Inf)] <- 0
  single_loglik[which(single_loglik==Inf)] <- 100
  sample_loglik_neg <- -sum(single_loglik)
  
  # sample_loglik_neg <- -sum(single_loglik[!which(single_loglik%in%c(Inf))])
  
  # if(counter%%30 == 0){
  #   print("Loop")
  # }
  
  
  return(sample_loglik_neg)
}




counter <<- 0
max_loglik <- function(deal_data,sales_estimates,sales_estimates_h,isholiday,Theta_est_nh){

  
  if(isholiday == 0){
    
    tv <- deal_data$total_volume
    # Theta_est <- c(-1.5,sales_estimates[c(1,2,6,9,10)])
    # Theta_known <- c(sales_estimates[13:25],log(sqrt(var(tv[tv<quantile(tv,0.7)&tv>quantile(tv,0.3)]))),-2.3)
    Theta_est <- c(-1.5,log(mean(tv)),sales_estimates[c(2,4)])
    Theta_known <- c(log(sqrt(var(tv[tv<quantile(tv,0.7)&tv>quantile(tv,0.3)]))),-2.3)

    Theta_est <- Theta_est + runif(4,-2,2)*abs(Theta_est)
    print(Theta_est)
 
    
    tryCatch(optim(
      par = Theta_est, fn=sample_loglik,
      #gr = gr,
      #grad = gr_num,
      #ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1), c(0, 0, -1)), ci = c(0, -1, 0, 0,-50),
      Theta_known = Theta_known, deal_data= deal_data,isholiday=0,Theta_est_nh=0,
      # method = "BFGS",
      method = "Nelder-Mead",
      control = list(factr = 1e-3, maxit = 5000)) -> optimTheta)
    
  }
  
  
  
  return(optimTheta)
}





mleout_preh_1 <- list()
#mleout_h <- list()
predays <- 15
pre.windows <- c(seq(21-predays,21),seq(44-predays,44),seq(92-predays,92))


#mleout_preh_ms <- list()

txtStart("test.txt")
#2,5,7,11,1,10
for(iter in 1:30){
  for(i in c(5)){
    print(i)
    if(i %in%c(5)){
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
      para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)
      
      print(dim(nonhol_data))
      tryCatch(
        {tic()
          max_loglik(deal_data=nonhol_data,sales_estimates[i,],sales_estimates_h[i,],isholiday=0,Theta_est_nh = 0) -> mleout_preh_1[[i]]
          toc()}
      )
      

    }
  }
  
  mleout_preh_ms[[iter]][[i]] <- mleout_preh_1[[i]]
}

txtStop()

# type 2, no sign restrictions on the coefficients
# type 5, sign restriction on logd

lbvalue <- c(-3300,-10400)
#mleout_preh <- list()
mlefnvalue_ls <- list()
for(i in c(5)){
  mlefnvalue <- vector()
  for(iter in 1:30){
    mlefnvalue <- c(mlefnvalue,mleout_preh_ms[[iter]][[i]]$value)
  }
  mlefnvalue_ls[[i]] <- mlefnvalue
 #mleout_preh[[i]] <- mleout_preh_ms[[which(mlefnvalue==min(mlefnvalue[!mlefnvalue%in%c(0,lbvalue)]))]][[i]]
}


#mleout_h[[3]] <- list(par=c(-5.0185921,-6.2534314,-0.3468598,4.2358820,1.2887642,1.0921488,9.0062073,-1.0255964))


D2nhbar(ThetaAll=list(c(0),c(0),c(mleout_preh_ms[[3]][[2]]$par[2:6],sales_estimates[2,13:25])),para=nonhol_data[10,c("discount","prep.period","offering_duration")],deal_data=nonhol_data[10,],isholiday=0)
