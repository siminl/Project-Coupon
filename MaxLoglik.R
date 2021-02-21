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
    
  }
  
  D <- exp(logD)
  return(D)
  
}


D2nhbar <- function(ThetaAll,para,deal_data,isholiday){
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
    weekends <- deal_data$weekends
    discprice <- (1-para[,1])*original_price
    
    city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                     nrow = dim(deal_data)[1],byrow=FALSE) == 
                matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                           dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
    
    
    logD <- (beta)%*%rbind(rep(1,dim(para)[1]),
                           t(log(para[,3])),  
                           rep(log(original_price),dim(para)[1]),
                           rep(log(competitors),dim(para)[1]),
                           rep(weekends,dim(para)[1]),
                           kronecker(t(rep(1,dim(para)[1])),city))%>%as.matrix()
    
  }
  
  D <- exp(logD)
  return(D)
  
}

getIV <- function(ThetaAll,para,deal_data,D1,D2){
  c <- unlist(ThetaAll[1])
  original_price <- deal_data$original_price
  siggam <- unlist(ThetaAll[4])
  
  IV_vec <- (((1-para[,1])*original_price*D1 + original_price*D2 - 
    (exp(c)/para[,3])*((D1+D2)^2+exp(2*(siggam[1] + siggam[2]*para[,3]))+exp(2*siggam[1]))))/max(D1)
  
  #IV_vec%>%as.data.frame()%>%mutate(expV1 = exp(V1)) ->tmp
  IVmax <- max(D1)
  IV_vec_exp <- exp(IV_vec)
  IV <- log(sum(IV_vec_exp))
  
  #IV[which(IV==-Inf)] <- log(length(IV_vec))+median(IV_vec)
  #print(IV)
  return(list(IV=IV,IVmax=IVmax))
  
}


dealloglik <- function(Theta_est,Theta_known,para,deal_data,isholiday,IV,normalize){
    
  ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
  if(isholiday==0){
    
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    siggam <- unlist(ThetaAll[4])
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends
    discprice <- (1-para[,1])*original_price
    
    D1bar <- D1nhbar(ThetaAll,para,deal_data,isholiday)
    D2bar <- D2nhbar(ThetaAll,para,deal_data,isholiday)
    numer <- (discprice*D1bar + original_price*D2bar -
      (exp(c)/para[,3])*((D1bar+D2bar)^2+exp(2*(siggam[1] + siggam[2]*para[,3]))+exp(2*siggam[1])))/(normalize)
    
    #print(numer)
    rt <- numer - IV
  }
  
  counter <<- counter +1 
  return(rt)
}






sample_loglik <- function(Theta_est,Theta_known,deal_data,isholiday){
  
  ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
  single_loglik <- vector()
  
  for(i in 1:dim(deal_data)[1]){
    #print(i)
    D1nhbar_t <- D1nhbar(ThetaAll,para=para_choice,deal_data[i,],isholiday=0)
    D2nhbar_t <- D2nhbar(ThetaAll,para=para_choice,deal_data[i,],isholiday=0)
    IVout <- getIV(ThetaAll,para=para_choice,deal_data[i,],D1=t(D1nhbar_t),D2=t(D2nhbar_t))
    
    para_t <- t(c(deal_data$discount[i],deal_data$prep.period[i],deal_data$offering_duration[i]))
    single_loglik[i] <- dealloglik(Theta_est,Theta_known,para=para_t,deal_data[i,],isholiday,IV = IVout$IV, normalize = IVout$IVmax)
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
  
  if(counter%%30 == 0){
    print("Loop")
  }
 
  
  return(sample_loglik_neg)
}



Theta_known <- sales_estimates
sigs_ini <- c(5,0.01)
Theta_est <- c(-5,sales_estimates[5,c(1,2,6,9,10)],sigs_ini)

counter <<- 0
max_loglik <- function(deal_data,sales_estimates,isholiday){
 
  Theta_known <- sales_estimates
  sigs_ini <- c(4,0.01)
  Theta_est <- c(-2,sales_estimates[c(1,2,6,9,10)],sigs_ini)
  
  # tryCatch(constrOptim(theta = Theta_est, f=sample_loglik, 
  #                      #grad = gr,
  #                      grad = numDeriv::grad(func=sample_loglik, x = Theta_est,
  #                                            Theta_known = Theta_known, deal_data= deal_data,isholiday),
  #                      ui = (rbind(c(1,rep(0,7)),c(rep(0,6),1,0),c(rep(0,7),1))), ci = rep(0,3),
  #                      Theta_known = Theta_known, deal_data= deal_data,isholiday=0,
  #                      #method = "Nelder-Mead",
  #                      method = "BFGS",
  #                      control = list(trace=6),
  #                      outer.eps = 1e-02) -> optimTheta)
  
  tryCatch(optim(
    par = Theta_est, fn=sample_loglik,
    #gr = gr,
    #grad = gr_num,
    #ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1), c(0, 0, -1)), ci = c(0, -1, 0, 0,-50),
    Theta_known = Theta_known, deal_data= deal_data,isholiday=0,
    # method = "BFGS",
    method = "Nelder-Mead",
    control = list(factr = 1e-3)) -> optimTheta)
  
  return(optimTheta)
}



txtStart("test.txt")
deallik(Theta_est,Theta_known,para,deal_data[1,],isholiday=0) -> tmp
txtStop()

tic()
sample_loglik(Theta_est,Theta_known,deal_data=nonhol_data,isholiday=0)
toc()

tic()
max_loglik(deal_data=nonhol_data,sales_estimates[4,],isholiday=0) -> type1
toc()

#type 6(remove 30),9

# loop over all types 

mleout <- list()
txtStart("test.txt")
for(i in 1:dim(sales_estimates)[1]){
  if(!i %in%c(3,6,9)){
    dealsfull_type <- dealsfull%>%filter(type==i,is.na(holiday)==1)
    disc_alpha <- unique(round(dealsfull_type$discount,digits = 2))
    disc_T <- unique(dealsfull_type$prep.period)
    disc_d <- unique(dealsfull_type$offering_duration)
    para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)
    
    nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>tmp}%>%
      filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
      mutate(original_price = original_price/sd(original_price))
    print(dim(nonhol_data))
    tryCatch(
      {tic()
        max_loglik(deal_data=nonhol_data,sales_estimates[i,],isholiday=0) -> mleout[[i]]
        toc()}
    )
  }
}
txtStop()


tic()
max_loglik(deal_data=nonhol_data[22,],sales_estimates[5,],isholiday=0) -> mleout[[i]]
toc()



