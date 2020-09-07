# minimization problem for each firm

sample_loglik <- function(ThetaAll,deal_data,isholiday){
  # isholiday is the indicator of whether it is for holiday or non-holiday deals calculations
  # Theta <- c(cnh, sigepsilon, sigxi, )
  # parai <- c(alphai, Ti, di)
  # parak <- c(alphak, Tk, dk)
  # obsi <- c(pricei,)

  single_likelihood <- vector()
  
  if(isholiday == 0){
    for(i in 1:dim(deal_data)[1]){
      parai <- deal_data[i,]%>%select(discount, prep.period, offering_duration)%>%as.matrix()
      # if(isholiday == 0 ){
      #   obsi <- deal_data%>%select(price,original_price,competitors,weekends,platform,city)%>%as.matrix()
      # }else{
      #   obsi <- deal_data%>%select(price,original_price,competitors_hol,weekends,holiday,platform,city)%>%as.matrix()
      # }
      single_likelihood[i] <- pnorm(get_deal_minimizer(ThetaAll,parai,deal_data[i,],isholiday))
    }
  }

  sample_loglik <- log(sum(single_likelihood))
  return(sample_loglik)
  
}

MLE_estimation <- function(deal_data,isholiday){
  constrOptim(theta = Thetaini, f=sample_loglik, 
              ui =, ci = ,
              deal_data,isholiday) -> optimTheta
  
  return(optimTheta)
}


partM <- function(ThetaAll,para,deal_data,isholiday){
  # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
  # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
  # parai <- c(alphai, Ti, di)
  # parak <- c(alphak, Tk, dk)
  # obsi <- c(pricei,)
  
  original_price <- deal_data$original_price
  
  M <- ((1-para[1])*original_price*D1nhtilde(ThetaAll,para,deal_data,isholiday) + 
          original_price*D2nhtilde(ThetaAll,para,deal_data,isholiday))
  
  return(M)
}

partS <- function(ThetaAll,para,deal_data,isholiday){
  # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
  # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
  # parai <- c(alphai, Ti, di)
  # parak <- c(alphak, Tk, dk)
  # obsi <- c(pricei,)
  
  original_price <- deal_data$original_price
  
  S <- (D1nhtilde(ThetaAll,para,deal_data,isholiday) + D2nhtilde(ThetaAll,para,deal_data,isholiday))
  return(S)
}


D1nhtilde <- function(ThetaAll,para,deal_data,isholiday){
  if(isholiday==0){
    # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
    # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
    # parai <- c(alphai, Ti, di)
    # parak <- c(alphak, Tk, dk)
    # obsi <- c(pricei,)
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    sigs <- unlist(ThetaAll[4])
    discprice <- deal_data%>%mutate(tmp=(1-para[1])*original_price)%>%select(tmp)
    offering_duration <- deal_data$offering_duration
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends  
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    
    platform <- t(cbind(deal_data%>%select(platform),deal_data%>%select(platform)) == 
                    matrix(rep(sub("platform",'',colnames(sales_estimates)[grepl("platform",colnames(sales_estimates))&
                                                                             !grepl(":",colnames(sales_estimates))]),dim(deal_data)[1]),ncol=2,byrow = TRUE))
    city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                     nrow = dim(deal_data)[1],byrow=FALSE) == 
                matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                           dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
    
    pfwk <- platform*weekends
    
    logD <- t(Theta)%*%rbind(rep(1,dim(deal_data)[1]),
                             rep(log(para[3]), dim(deal_data)[1]),
                             rep(log(para[3])^2, dim(deal_data)[1]),
                             t(cbind(discprice, (discprice)^2,
                                     log(original_price), 1/log(original_price), 1/(log(original_price))^2,
                                     log(competitors),weekends)),platform,city,pfwk)%>%as.matrix()
    
  }
  
  D <- exp(logD)
  return(D)
  
}


D2nhtilde <- function(ThetaAll,para,deal_data,isholiday){
  if(isholiday==0){
    # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
    # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
    # parai <- c(alphai, Ti, di)
    # parak <- c(alphak, Tk, dk)
    # obsi <- c(pricei,)
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    sigs <- unlist(ThetaAll[4])
    discprice <- deal_data%>%mutate(tmp=(1-para[1])*original_price)%>%select(tmp)
    offering_duration <- deal_data$offering_duration
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends  
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    
    city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                     nrow = dim(deal_data)[1],byrow=FALSE) == 
                matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                           dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
    
    
    logD <- (beta)%*%rbind(rep(1,dim(deal_data)[1]),
                           log(offering_duration),
                           log(original_price),weekends,log(competitors),city)%>%as.matrix()
    
  }
  
  D <- exp(logD)
  return(D)
  
}

sigmazsq <- function(ThetaAll,parai,parak,deal_data,isholiday){
  if(isholiday==0){
    # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
    # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
    # parai <- c(alphai, Ti, di)
    # parak <- c(alphak, Tk, dk)
    # obsi <- c(pricei,)
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    sigs <- unlist(ThetaAll[4])
    discprice <- deal_data%>%mutate(tmp=(1-para[1])*original_price)%>%select(tmp)
    offering_duration <- deal_data$offering_duration
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends  
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    
    expmu2minusmu1 <- 2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3])/
      (2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3])+(parak[1]-parai[1])*original_price)
    sigmazsq <- log(1+((exp(sigs[1]^2) -1) + (expmu2minusmu1^2)*exp(sigs[2]^2-sigs[1]^2)*(exp(sigs[2]^2)-1))/
                      (1+(expmu2minusmu1^2)*exp(sigs[2]^2-sigs[1]^2)+2*expmu2minusmu1*exp(sigs[2]^2-sigs[1]^2)/2))
    
  }
  
  return(sigmazsq)
  
}


muz <- function(ThetaAll,parai,parak,deal_data,isholiday){
  if(isholiday==0){
    # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
    # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
    # parai <- c(alphai, Ti, di)
    # parak <- c(alphak, Tk, dk)
    # obsi <- c(pricei,)
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    sigs <- unlist(ThetaAll[4])
    discprice <- deal_data%>%mutate(tmp=(1-para[1])*original_price)%>%select(tmp)
    offering_duration <- deal_data$offering_duration
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends  
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    
    muz <- log(2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3]+(parak[1]-parai[1])*original_price)*exp(sigs[1]^2/2)+
                 2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3])*exp(sigs[2]^2/2)) - 
      sigmazsq(ThetaAll,parai,parak,deal_data,isholiday)/2
    
  }
  
  return(muz)
}


deal_minimizer <- function(parak,ThetaAll,parai,deal_data,isholiday){
  if(isholiday==0){
    # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
    # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
    # parai <- c(alphai, Ti, di)
    # parak <- c(alphak, Tk, dk)
    # obsi <- c(pricei,)
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    sigs <- unlist(ThetaAll[4])
    discprice <- deal_data%>%mutate(tmp=(1-para[1])*original_price)%>%select(tmp)
    offering_duration <- deal_data$offering_duration
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends  
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    
    minimizer <- (log(0.0001+(partM(ThetaAll,parai,deal_data,isholiday)-partM(ThetaAll,parak,deal_data,isholiday)+
                         (c*partS(ThetaAll,parak,deal_data,isholiday)^2)/parak[3]-(c*partS(ThetaAll,parai,deal_data,isholiday)^2)/parai[3])/
                        ((2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3])+(parak[1]-parai[1])*original_price)*exp(sigs[1]^2/2)+
                           (2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3]))*exp(sigs[2]^2/2))) + sigmazsq(ThetaAll,parai,parak,deal_data,isholiday)/2)/
      sqrt(sigmazsq(ThetaAll,parai,parak,deal_data,isholiday))
    
  }
  
  return(minimizer)
}


get_deal_minimizer <- function(ThetaAll,parai,deal_data,isholiday){
  
  parakini <- c(0.5,10,12)
  
  fn <- function(parak,ThetaAll,parai,deal_data,isholiday) deal_minimizer(parak,ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0)
  gr <- function(parak,ThetaAll,parai,deal_data,isholiday) numDeriv::grad(fn, parak, ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0)
  
  tic()
  paraktry <- 
    constrOptim(theta = parakini, f=fn, 
                grad = gr,
                ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1)), ci = c(0, -1, 0, 0),
                ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0,
                method = "BFGS") -> deal_min
  toc()
  # paraktry <- 
  #   constrOptim(theta = parakini, f=deal_minimizer, 
  #               grad = numDeriv::grad(func=deal_minimizer, x = theta,
  #                                     ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=isholiday),
  #               ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1)), ci = c(0, -1, 0, 0),
  #               ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=isholiday) -> deal_min
  # 
  # 
  # 
  return(deal_min)
}



