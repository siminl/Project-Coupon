D1nhbar <- function(ThetaAll,para,deal_data,isholiday){
  if(isholiday==0){
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
    discprice <- (1-para[,1])*original_price
    
    platform <- t(cbind(deal_data%>%select(platform),deal_data%>%select(platform)) == 
                    matrix(rep(sub("platform",'',colnames(sales_estimates)[grepl("platform",colnames(sales_estimates))&
                                                                             !grepl(":",colnames(sales_estimates))]),dim(deal_data)[1]),ncol=2,byrow = TRUE))
    city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                     nrow = dim(deal_data)[1],byrow=FALSE) == 
                matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                           dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
    weekends <- deal_data$weekends
    pfwk <- platform*weekends
    
    logD <- t(Theta)%*%rbind(rep(1,dim(para)[1]),
                             t(log(para[,3])),t(log(para[,3])^2),
                             discprice,discprice^2,
                             rep(log(original_price),dim(para)[1]),rep(1/log(original_price),dim(para)[1]),rep(1/(log(original_price))^2,dim(para)[1]),
                             rep(log(competitors),dim(para)[1]),rep(weekends,dim(para)[1]),
                             kronecker(t(rep(1,dim(para)[1])),platform),kronecker(t(rep(1,dim(para)[1])),city),
                             kronecker(t(rep(1,dim(para)[1])),pfwk))%>%as.matrix()
    
  }else{
    
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    siggam <- unlist(ThetaAll[4])
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    discprice <- (1-para[,1])*original_price

    
    total_sales <- deal_data$total_volume
    
    platform <- t(cbind(deal_data%>%select(platform),deal_data%>%select(platform)) == 
                    matrix(rep(sub("platform",'',colnames(sales_estimates)[grepl("platform",colnames(sales_estimates))&
                                                                             !grepl(":",colnames(sales_estimates))]),dim(deal_data)[1]),ncol=2,byrow = TRUE))
    city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                     nrow = dim(deal_data)[1],byrow=FALSE) == 
                matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                           dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
    weekends <- deal_data$weekends
    pfwk <- platform*weekends
    
    
    logD <- t(Theta)%*%rbind(rep(1,dim(para)[1]),
                             t(log(para[,3])),t(log(para[,3])^2),
                             discprice,discprice^2,
                             t(log(para[,2])),t(log(para[,2])^2),
                             rep(log(original_price),dim(para)[1]),rep(1/log(original_price),dim(para)[1]),rep(1/(log(original_price))^2,dim(para)[1]),
                             rep(log(competitors),dim(para)[1]),rep(weekends,dim(para)[1]),
                             kronecker(t(rep(1,dim(para)[1])),platform),kronecker(t(rep(1,dim(para)[1])),city),
                             kronecker(t(rep(1,dim(para)[1])),pfwk))%>%as.matrix()
    
  }
  
  D <- exp(logD)
  return(D)
  
}


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
    
    
    logD <- t(beta)%*%rbind(rep(1,dim(para)[1]),
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
    IVmax <- max(D1)
    
    IV_vec <- (((1-para[,1])*original_price*D1 + original_price*D2 - 
                  (exp(c)/para[,3])*((D1+D2)^2+exp(2*(siggam[1] + siggam[2]*para[,3]))+exp(2*siggam[1]))))/IVmax
    
    #IV_vec%>%as.data.frame()%>%mutate(expV1 = exp(V1)) ->tmp
    
    IV_vec_exp <- exp(IV_vec)
    IV <- log(sum(IV_vec_exp))
  }else{
    
    ThetaAll_nh <- list(Theta_est_nh[1],Theta_known[1:28],c(Theta_est_nh[2:6],Theta_known[13:26]),c(Theta_est_nh[8],Theta_est_nh[7]))
    
    c <- unlist(ThetaAll[1])
    original_price <- deal_data$original_price
    siggam <- unlist(ThetaAll[4])
    
    D1bar <- deal_data$total_volume
    hollen <- deal_data$holiday.len
    cnh <- (Theta_est_nh[1])
    
    D2barnh <- t(D2nhbar(ThetaAll_nh,para,deal_data,0)) # estimated coefficients
    IVmax <- max(max(D1),max(D2),max(D2barnh))
    #IVmax <- max(c(D1,D2,D2barnh))
    
    IV_vec <-  ((1-para[,1])*D1bar + original_price*D2barnh -
                  (exp(cnh)/(para[,3]-hollen))*((D1bar- D1+D2barnh)^2+exp(2*(siggam[1] + siggam[2]*para[,2])) + exp(2*(Theta_est_nh[8]))) +
                  original_price*D2 -
                  (exp(c)/hollen)*((D1+D2)^2+exp(2*(siggam[1] + siggam[2]*para[,2]))+exp(2*siggam[1])))/(IVmax)

    #IV_vec%>%as.data.frame()%>%mutate(expV1 = exp(V1)) ->tmp
    
    IV_vec_exp <- exp(IV_vec)
    IV <- log(sum(IV_vec_exp))
  }

  
  #IV[which(IV==-Inf)] <- log(length(IV_vec))+median(IV_vec)
  #print(IV)
  return(list(IV=IV,IVmax=IVmax))
  
}


dealloglik <- function(Theta_est,Theta_known,para,deal_data,isholiday,IV,normalize,Theta_est_nh){
  

  if(isholiday==0){
    
    # ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
    ThetaAll <- list(Theta_est[1],Theta_known[-28],c(Theta_est[2:6],Theta_known[13:25]),c(Theta_known[28],Theta_est[7]))
    
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    siggam <- unlist(ThetaAll[4])
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends
    discprice <- (1-para[,1])*original_price
    
    total_sales <- deal_data$total_volume
    
    D1bar <- total_sales
    D2bar <- D2nhbar(ThetaAll,para,deal_data,isholiday)
    numer <- (discprice*D1bar + original_price*D2bar -
                (exp(c)/para[,3])*((D1bar+D2bar)^2+exp(2*(siggam[1] + siggam[2]*para[,3]))+exp(2*siggam[1])))/(normalize)
    
    #print(numer)
    rt <- numer - IV
  }else{
    
    # Theta_est <- c(cost, beta(2:6), gamma, theta(8,19))
    ThetaAll <- list(Theta_est[1],c(Theta_est[8:19],Theta_known[30:45],Theta_est[c(20,21)]),c(Theta_est[2:6],Theta_known[13:26]),c(Theta_known[29],Theta_est[7]))
    ThetaAll_nh <- list(Theta_est_nh[1],Theta_known[1:28],c(Theta_est_nh[2:6],Theta_known[13:26]),c(Theta_est_nh[8],Theta_est_nh[7]))
    
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    siggam <- unlist(ThetaAll[4])
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends
    discprice <- (1-para[,1])*original_price
    
    D1bar <- deal_data$total_volume
    hollen <- deal_data$holiday.len
    cnh <- (Theta_est_nh[1])
    

    D1barh <- D1nhbar(ThetaAll,para,deal_data,1)
    D2barh <- D2nhbar(ThetaAll,para,deal_data,1)
    D2barnh <- D2nhbar(ThetaAll_nh,para,deal_data,0) # estimated coefficients
    numer <- (discprice*D1bar + original_price*D2barnh -
                (exp(cnh)/(para[,3]-hollen))*((D1bar- D1barh+D2barnh)^2+exp(2*(siggam[1] + siggam[2]*(para[,2]))) + 
                                                exp(2*(Theta_est_nh[8]))) +
                original_price*D2barh -
                (exp(c)/hollen)*((D1barh+D2barh)^2+exp(2*(siggam[1] + siggam[2]*para[,2]))+exp(2*siggam[1])))/(normalize)
    
    #print(numer)
    rt <- numer - IV
  }
  
  return(rt)
}




sample_loglik <- function(Theta_est,Theta_known,deal_data,isholiday,Theta_est_nh){
  
  if(isholiday==0){
    
    # ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
    ThetaAll <- list(Theta_est[1],Theta_known[-28],c(Theta_est[2:6],Theta_known[13:25]),c(Theta_known[28],Theta_est[7]))
    single_loglik <- vector()
    
    for(i in 1:dim(deal_data)[1]){
      #print(i)
      total_sales <- deal_data[i,]$total_volume
      D1nhbar_t <- rep(total_sales,dim(para_choice)[1])
      D2nhbar_t <- D2nhbar(ThetaAll,para=para_choice,deal_data[i,],isholiday=0)
      IVout <- getIV(ThetaAll,para=para_choice,deal_data[i,],D1=(total_sales),D2=t(D2nhbar_t),isholiday,Theta_est_nh)
      
      para_t <- t(c(deal_data$discount[i],deal_data$prep.period[i],deal_data$offering_duration[i]))
      single_loglik[i] <- dealloglik(Theta_est,Theta_known,para=para_t,deal_data[i,],isholiday,IV = IVout$IV, normalize = IVout$IVmax,Theta_est_nh)
    }
    
  }else{
    
    # ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
    ThetaAll <- list(Theta_est[1],c(Theta_est[8:19],Theta_known[30:45],Theta_est[c(20,21)]),c(Theta_est[2:6],Theta_known[13:26]),c(Theta_known[29],Theta_est[7]))
    ThetaAll_nh <- list(Theta_est_nh[1],Theta_known[1:28],c(Theta_est_nh[2:6],Theta_known[13:26]),c(Theta_est_nh[8],Theta_est_nh[7]))
    #tic()
    single_loglik <- vector()
    
    for(i in 1:dim(deal_data)[1]){
      # print(i)
      total_sales <- deal_data[i,]$total_volume
      D1hbar_t <- D1nhbar(ThetaAll,para=para_choice,deal_data[i,],isholiday=1)
      D2hbar_t <- D2nhbar(ThetaAll,para=para_choice,deal_data[i,],isholiday=1)
      IVout <- getIV(ThetaAll,para=para_choice,deal_data[i,],D1=t(D1hbar_t),D2=t(D2hbar_t),isholiday,Theta_est_nh)
      
      para_t <- t(c(deal_data$discount[i],deal_data$prep.period[i],deal_data$offering_duration[i]))
      single_loglik[i] <- dealloglik(Theta_est,Theta_known,para=para_t,deal_data[i,],isholiday=1,IV = IVout$IV, normalize = IVout$IVmax,Theta_est_nh)
    }
    #toc()
    counter <<- counter +1 
    if(counter%%10 == 1){
      print(Theta_est)
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
  
  # Theta_known <- sales_estimates
  # sigs_ini <- c(4,0.01)
  # Theta_est <- c(-2,sales_estimates[c(1,2,6,9,10)],sigs_ini)
  
  if(isholiday == 0){
    tv <- deal_data$total_volume
    Theta_known <- c(sales_estimates,log(sqrt(var(tv[tv<quantile(tv,0.7)&tv>quantile(tv,0.3)]))))
    sigs_ini <- c(0.1)
    Theta_est <- c(-2,sales_estimates[c(1,2,6,9,10)],sigs_ini)
    
    
    tryCatch(optim(
      par = Theta_est, fn=sample_loglik,
      #gr = gr,
      #grad = gr_num,
      #ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1), c(0, 0, -1)), ci = c(0, -1, 0, 0,-50),
      Theta_known = Theta_known, deal_data= deal_data,isholiday=0,Theta_est_nh=0,
      # method = "BFGS",
      method = "Nelder-Mead",
      control = list(factr = 1e-3, maxit = 5000)) -> optimTheta)
    
  }else{
    tv <- deal_data$total_volume
    Theta_known <- c(sales_estimates,log(sqrt(var(tv[tv<quantile(tv,0.7)&tv>quantile(tv,0.3)]))),sales_estimates_h[13:28])
    sigs_ini <- c(0.1)
    Theta_est <- c(-2,sales_estimates[c(1,2,6,9,10)],sigs_ini,sales_estimates[c(seq(1,5),2,3,seq(6,10),seq(27,28))])

      
    tryCatch(optim(
      par = Theta_est, fn=sample_loglik,
      #gr = gr,
      #grad = gr_num,
      #ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1), c(0, 0, -1)), ci = c(0, -1, 0, 0,-50),
      Theta_known = Theta_known, deal_data= deal_data,isholiday=1,Theta_est_nh=Theta_est_nh,
      # method = "BFGS",
      method = "Nelder-Mead",
      control = list(factr = 1e-3, maxit = 5000, trace=6)) -> optimTheta)
    
  }
  

  
  return(optimTheta)
}



### test field ------------------------------------


txtStart("test.txt")
# pre-estimate platform and city effects, but not platform*wkends effects
tv <- deal_data$total_volume
Theta_known <- c(sales_estimates[1,],log(sqrt(var(tv[tv<quantile(tv,0.7)&tv>quantile(tv,0.3)]))),sales_estimates_h[1,13:28])
sigs_ini <- c(0.1)
Theta_est <- c(-2,sales_estimates[1,c(1,2,6,9,10)],sigs_ini,sales_estimates[1,c(seq(1,5),2,3,seq(6,10),seq(27,28))])
Theta_est_nh <- c(type1$par,3.850367)

dealloglik(Theta_est,Theta_known,para,deal_data[1,],isholiday=1,Theta_est_nh=Theta_est_nh) -> tmp
txtStop()

tic()
sample_loglik(Theta_est,Theta_known,deal_data=nonhol_data,isholiday=0)
toc()

tic()
Theta_est_nh <- c(type1$par,3.850367)
hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==1)%>%{.->>tmp}%>%
  filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
  mutate(original_price = original_price/sd(original_price))

max_loglik(deal_data=hol_data,sales_estimates[1,],isholiday=1,Theta_est_nh) -> type1h
toc()

#type 6(remove 30),9

# loop over all types 
keeptype1 <- type1
mleout <- list()
txtStart("test.txt")
for(i in 1:dim(sales_estimates)[1]){
  if(!i %in%c(3,6,9)){
    
    nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
      mutate(original_price = original_price/sd(original_price))

    disc_alpha <- unique(round(dealsfull_type$discount,digits = 2))
    disc_T <- unique(dealsfull_type$prep.period)
    disc_d <- unique(dealsfull_type$offering_duration)
    para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)
    
    print(dim(nonhol_data))
    tryCatch(
      {tic()
        max_loglik(deal_data=nonhol_data,sales_estimates[i,],sales_estimates_h[i,],isholiday=0,Theta_est_nh = 0) -> mleout[[i]]
        toc()}
    )
  }
}
txtStop()










mleout <- list()
mleout_h <- list()
txtStart("test.txt")
for(i in 1:dim(sales_estimates)[1]){
  if(i %in%c(1,2,5,8,11)){
    nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
      mutate(original_price = original_price/sd(original_price))
    
    disc_alpha <- unique(round(dealsfull_type$discount,digits = 2))
    disc_T <- unique(dealsfull_type$prep.period)
    disc_d <- unique(dealsfull_type$offering_duration)
    para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)
    
    print(dim(nonhol_data))
    tryCatch(
      {tic()
        max_loglik(deal_data=nonhol_data,sales_estimates[i,],sales_estimates_h[i,],isholiday=0,Theta_est_nh = 0) -> mleout[[i]]
        toc()}
    )
    
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
      mutate(original_price = original_price/sd(original_price))

    disc_alpha <- unique(round(hol_data$discount,digits = 2))
    disc_T <- unique(hol_data$prep.period)
    disc_d <- unique(hol_data$offering_duration)
    para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)%>%
      filter(d>(T))
    
    
    sigSnh <- log(sqrt(var(nonhol_data$total_volume[nonhol_data$total_volume<quantile(nonhol_data$total_volume,0.7)&
                                            nonhol_data$total_volume>quantile(nonhol_data$total_volume,0.3)])))
    Theta_est_nh <- c(mleout[[i]]$par,sigSnh)

    print(dim(hol_data))
    tryCatch(
      {tic()
        max_loglik(deal_data=hol_data,sales_estimates[i,],sales_estimates_h[i,],isholiday=1,Theta_est_nh=Theta_est_nh) -> mleout_h[[i]]
        toc()}
    )
  }
}
txtStop()


mleout_h_f1 <- list()
#mleout_preh <- list()
predays <- 15
pre.windows <- c(seq(21-predays,21),seq(44-predays,44),seq(92-predays,92))
txtStart("test.txt")
for(i in c(2,5,8,11,1,3)){
  if(i %in%c(2,5,8,11,1,3)){
    # # nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
    # #   filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    # #   mutate(original_price = original_price/sd(original_price))
    # # 
    # nonhol_data <- dealsfull%>%
    #   mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
    #   filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
    #   #filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    #   mutate(original_price = original_price/sd(original_price))
    # 
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
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      mutate(original_price = original_price/sd(original_price))

    disc_alpha <- unique(round(hol_data$discount,digits = 2))
    disc_T <- unique(hol_data$prep.period)
    disc_d <- unique(hol_data$offering_duration)
    para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)%>%
      filter(d>(T))


    sigSnh <- log(sqrt(var(nonhol_data$total_volume[nonhol_data$total_volume<quantile(nonhol_data$total_volume,0.7)&
                                                      nonhol_data$total_volume>quantile(nonhol_data$total_volume,0.3)])))
    Theta_est_nh <- c(mleout[[i]]$par,sigSnh)

    print(dim(hol_data))
    tryCatch(
      {tic()
        max_loglik(deal_data=hol_data,sales_estimates[i,],sales_estimates_h[i,],isholiday=1,Theta_est_nh=Theta_est_nh) -> mleout_h_f1[[i]]
        toc()}
    )
  }
}
txtStop()




