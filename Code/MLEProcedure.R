# minimization problem for each firm

sample_loglik <- function(ThetaAll,deal_data,isholiday){
  # isholiday is the indicator of whether it is for holiday or non-holiday deals calculations
  # Theta <- c(cnh, sigepsilon, sigxi, )
  # parai <- c(alphai, Ti, di)
  # parak <- c(alphak, Tk, dk)
  # obsi <- c(pricei,)

  single_likelihood <- vector()
  min_value <- vector()

  if(isholiday == 0){
    for(i in 1:dim(deal_data)[1]){
      print(i)
      parai <- deal_data[i,]%>%select(discount, prep.period, offering_duration)%>%as.matrix()
      # if(isholiday == 0 ){
      #   obsi <- deal_data%>%select(price,original_price,competitors,weekends,platform,city)%>%as.matrix()
      # }else{
      #   obsi <- deal_data%>%select(price,original_price,competitors_hol,weekends,holiday,platform,city)%>%as.matrix()
      # }
      tryCatch(
        expr = {minimizer_out <- get_deal_minimizer(ThetaAll,
                                            parai=c(deal_data[i,"discount"],deal_data[i,"prep.period"],deal_data[i,"offering_duration"]),
                                            deal_data[i,],isholiday)
                min_value[i] <- minimizer_out$min_value
                single_likelihood[i] <- pnorm(min_value[i])
                print(paste("iter",i ,single_likelihood[i],sep=" "))},
        error = function(e){
          message("* Caught an error on itertion ", i)
          print(e)
        }
      )
    }
  }

  sample_loglik <- log(sum(single_likelihood,na.rm = TRUE))
  return(list(sample_loglik=sample_loglik,single_likelihood=single_likelihood))
  
}


txtStart("test.txt")
sample_loglik(ThetaAll,deal_data,isholiday=0) -> tmp
txtStop()



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
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    
    platform <- t(cbind(deal_data%>%select(platform),deal_data%>%select(platform)) == 
                    matrix(rep(sub("platform",'',colnames(sales_estimates)[grepl("platform",colnames(sales_estimates))&
                                                                             !grepl(":",colnames(sales_estimates))]),dim(deal_data)[1]),ncol=2,byrow = TRUE))
    city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                     nrow = dim(deal_data)[1],byrow=FALSE) == 
                matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                           dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
    weekends <- deal_data$weekends
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
    competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
    original_price <- deal_data$original_price
    weekends <- deal_data$weekends
    
    city <- t(matrix(rep(deal_data$city,length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]))),
                     nrow = dim(deal_data)[1],byrow=FALSE) == 
                matrix(rep(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))]),
                           dim(deal_data)[1]),ncol = length(sub("city",'',colnames(sales_estimates)[grepl("city",colnames(sales_estimates))])),byrow = TRUE))
    
    
    logD <- (beta)%*%rbind(rep(1,dim(deal_data)[1]),
                           rep(log(para[3]), dim(deal_data)[1]), 
                           log(original_price),weekends,log(competitors),city)%>%as.matrix()
    
  }
  
  D <- exp(logD)
  return(D)
  
}



sigmazsq <- function(ThetaAll,parai,parak,deal_data,isholiday){
  # normal distribution
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
       # discprice <- deal_data%>%mutate(tmp=(1-parai[1])*original_price)%>%select(tmp)
      #  offering_duration <- deal_data$offering_duration
        original_price <- deal_data$original_price
        weekends <- deal_data$weekends
        competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.

      rt <- (2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3]))^2*(exp(sigs[1])^2 + exp(sigs[2])^2)+
        ((parai[1]-parak[1])*original_price)^2 * (exp(sigs[1])^2)
  }
  return(rt)
}


deal_minimizer <- function(parak,ThetaAll,parai,deal_data,isholiday){
  # deal minimizer of the normal distribution assumption
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
    
    parai <- c(deal_data[,"discount"], deal_data[,"prep.period"], deal_data[,"offering_duration"])
    
    minimizer <- (partM(ThetaAll,parai,deal_data,isholiday)-partM(ThetaAll,parak,deal_data,isholiday)+
                                (c*partS(ThetaAll,parak,deal_data,isholiday)^2)/parak[3]-(c*partS(ThetaAll,parai,deal_data,isholiday)^2)/parai[3])/
      sqrt(sigmazsq(ThetaAll,parai,parak,deal_data,isholiday))
    
  }
  
  return(minimizer)
}


get_deal_minimizer <- function(ThetaAll,parai,deal_data,isholiday){
  # normal distribution
  
  # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
  # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
  # parai <- c(alphai, Ti, di)
  # parak <- c(alphak, Tk, dk)
  # obsi <- c(pricei,)
  Theta <- unlist(ThetaAll[2]) # sales_estimates
  beta <- unlist(ThetaAll[3])
  c <- unlist(ThetaAll[1])
  sigs <- unlist(ThetaAll[4])
  discprice <- deal_data%>%mutate(tmp=(1-parai[1])*original_price)%>%select(tmp)
  offering_duration <- deal_data$offering_duration
  original_price <- deal_data$original_price
  weekends <- deal_data$weekends  
  competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
  
  
  if(isholiday==0){
    

    
    fn <- function(parak,ThetaAll,parai,deal_data,isholiday) deal_minimizer(parak,ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0)
    gr <- function(parak,ThetaAll,parai,deal_data,isholiday){
      deriv_vec <- vector()
      parD1alpha <- (-Theta[4]*original_price - 2*Theta[5]*(1-parak[1])*original_price^2)
      parSsqalpha <- 2*partS(ThetaAll,parak,deal_data,isholiday)*parD1alpha
      parMalpha <- -original_price*D1nhtilde(ThetaAll,parak,deal_data,isholiday) + 
        (1-parak[1])*original_price*parD1alpha
      parsigzalpha <- (1/(2*sqrt(sigmazsq(ThetaAll,parai,parak,deal_data,isholiday))))*
        ((4*c/parak[3])*(partS(ThetaAll,parak,deal_data,isholiday)/parak[3]-partS(ThetaAll,parai,deal_data,isholiday)/parai[3]))*
        ((exp(sigs[1])^2 + exp(sigs[2])^2))*parD1alpha + 
        2*(original_price^2)*(parai[1]-parak[1])*(exp(sigs[1])^2)
      
      deriv_vec[1] <- (sqrt(sigmazsq(ThetaAll,parai,parak,deal_data,isholiday))*(-parMalpha+(c/parak[3])*parSsqalpha)-
        (partM(ThetaAll,parai,deal_data,isholiday)-partM(ThetaAll,parak,deal_data,isholiday)+
           (c*partS(ThetaAll,parak,deal_data,isholiday)^2)/parak[3]-(c*partS(ThetaAll,parai,deal_data,isholiday)^2)/parai[3])*parsigzalpha)/
        sigmazsq(ThetaAll,parai,parak,deal_data,isholiday)
      
      deriv_vec[2] <- 0
      
      parD1d <- -Theta[2]/parak[3] + Theta[3]*2*log(parak[3])/para[3]
      parD2d <- beta[2]/parak[3]
      parMd <- (1-parak[1])*original_price*parD1d + original_price*parD2d
      parSsqd <- 2*partS(ThetaAll,parak,deal_data,isholiday)*(parD1d+parD2d)
      parsigzd <- 4*(c/parak[3])*(partS(ThetaAll,parak,deal_data,isholiday)/parak[3]-partS(ThetaAll,parai,deal_data,isholiday)/parai[3])*
        parSsqalpha*((exp(sigs[1])^2 + exp(sigs[2])^2))
      
      deriv_vec[3] <- (sqrt(sigmazsq(ThetaAll,parai,parak,deal_data,isholiday))*(-parMd+(c/parak[3])*parSsqd - c*partS(ThetaAll,parak,deal_data,isholiday)^2/(parak[3]^2))-
                         (partM(ThetaAll,parai,deal_data,isholiday)-partM(ThetaAll,parak,deal_data,isholiday)+
                            (c*partS(ThetaAll,parak,deal_data,isholiday)^2)/parak[3]-(c*partS(ThetaAll,parai,deal_data,isholiday)^2)/parai[3])*parsigzd)/
        sigmazsq(ThetaAll,parai,parak,deal_data,isholiday)
      
      return(deriv_vec)
      
    } 
    
    
    tic()
    parak <- parai+0.1
    tryCatch(constrOptim(theta = parak, f=fn, 
                  #grad = gr,
                  grad = numDeriv::grad(func=deal_minimizer, x = parak,
                                      ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0),
                  ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1), c(0, 0, -1)), ci = c(0, -1, 0, 0,-50),
                  ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0,
                  method = "Nelder-Mead") -> deal_min)
    
      min_value <- fn(deal_min$par%>%round(digits = 3),ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0)
    toc()
    
  }

  return(list(deal_min=deal_min,min_value=min_value))
}

get_deal_minimizer(ThetaAll,parai=c(deal_data[1,"discount"],deal_data[1,"prep.period"],deal_data[1,"offering_duration"]),deal_data[1,],isholiday=0) -> tmp
get_deal_minimizer(ThetaAll,parai=c(deal_data[1,"discount"],deal_data[1,"prep.period"],deal_data[1,"offering_duration"]),deal_data[1,],isholiday)
ThetaAll <- list(c(0.1),sales_estimates[1,],c(-2,1.53,0.5,0.1,-0.4,sales_estimates[1,13:26]),c(5.6,3))
parai <- c(deal_data[1,"discount"],deal_data[1,"prep.period"],deal_data[1,"offering_duration"])
parak <- parai + 0.1
D1nhtilde(ThetaAll,para=c(deal_data[1,"discount"],deal_data[1,"prep.period"],deal_data[1,"offering_duration"]),
          deal_data[1,],isholiday=0)
D2nhtilde(ThetaAll,para=c(deal_data[1,"discount"],deal_data[1,"prep.period"],deal_data[1,"offering_duration"]),
          deal_data[1,],isholiday=0)



## log normal distribution assumption --------

# sigmazsq <- function(ThetaAll,parai,parak,deal_data,isholiday){
#   # sigma z of the log normal distribution assumption 
#   
#   if(isholiday==0){
#     # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
#     # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
#     # parai <- c(alphai, Ti, di)
#     # parak <- c(alphak, Tk, dk)
#     # obsi <- c(pricei,)
#     Theta <- unlist(ThetaAll[2]) # sales_estimates
#     beta <- unlist(ThetaAll[3])
#     c <- unlist(ThetaAll[1])
#     sigs <- unlist(ThetaAll[4])
#     discprice <- deal_data%>%mutate(tmp=(1-para[1])*original_price)%>%select(tmp)
#     offering_duration <- deal_data$offering_duration
#     original_price <- deal_data$original_price
#     weekends <- deal_data$weekends  
#     competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
#     
#     expmu2minusmu1 <- 2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3])/
#       (2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3])+(parak[1]-parai[1])*original_price)
#     sigmazsq <- log(1+((exp(sigs[1]^2) -1) + (expmu2minusmu1^2)*exp(sigs[2]^2-sigs[1]^2)*(exp(sigs[2]^2)-1))/
#                       (1+(expmu2minusmu1^2)*exp(sigs[2]^2-sigs[1]^2)+2*expmu2minusmu1*exp(sigs[2]^2-sigs[1]^2)/2))
#     
#   }
#   
#   return(sigmazsq)
#   
# }
# 
# 
# muz <- function(ThetaAll,parai,parak,deal_data,isholiday){
#    # muz of the log normal distribution assumption
#   if(isholiday==0){
#     # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
#     # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
#     # parai <- c(alphai, Ti, di)
#     # parak <- c(alphak, Tk, dk)
#     # obsi <- c(pricei,)
#     Theta <- unlist(ThetaAll[2]) # sales_estimates
#     beta <- unlist(ThetaAll[3])
#     c <- unlist(ThetaAll[1])
#     sigs <- unlist(ThetaAll[4])
#     discprice <- deal_data%>%mutate(tmp=(1-para[1])*original_price)%>%select(tmp)
#     offering_duration <- deal_data$offering_duration
#     original_price <- deal_data$original_price
#     weekends <- deal_data$weekends  
#     competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
#     
#     muz <- log(2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3]+(parak[1]-parai[1])*original_price)*exp(sigs[1]^2/2)+
#                  2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3])*exp(sigs[2]^2/2)) - 
#       sigmazsq(ThetaAll,parai,parak,deal_data,isholiday)/2
#     
#   }
#   
#   return(muz)
# }
# 
# 
# deal_minimizer <- function(parak,ThetaAll,parai,deal_data,isholiday){
#   # deal minimizer of the log normal distribution assumption
#   if(isholiday==0){
#     
#     # ThetaAll <- list({cnh},{theta0,thetad,thetad2,thetadp, thetadp2,thetap,thetap1,thetap2,thetacomp,thetawk,thetapf,thetacity,thetapfwk},
#     # {beta0,betad,betap,betawk,betacomp,betacity},{sigepsilon,sigxi})
#     # parai <- c(alphai, Ti, di)
#     # parak <- c(alphak, Tk, dk)
#     # obsi <- c(pricei,)
#     Theta <- unlist(ThetaAll[2]) # sales_estimates
#     beta <- unlist(ThetaAll[3])
#     c <- unlist(ThetaAll[1])
#     sigs <- unlist(ThetaAll[4])
#     discprice <- deal_data%>%mutate(tmp=(1-para[1])*original_price)%>%select(tmp)
#     offering_duration <- deal_data$offering_duration
#     original_price <- deal_data$original_price
#     weekends <- deal_data$weekends  
#     competitors <- deal_data$competitors + 1 # add 1 to avoid inf, when there is no competitors found.
#     
#     minimizer <- (log(0.0001+(partM(ThetaAll,parai,deal_data,isholiday)-partM(ThetaAll,parak,deal_data,isholiday)+
#                          (c*partS(ThetaAll,parak,deal_data,isholiday)^2)/parak[3]-(c*partS(ThetaAll,parai,deal_data,isholiday)^2)/parai[3])/
#                         ((2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3])+(parak[1]-parai[1])*original_price)*exp(sigs[1]^2/2)+
#                            (2*c*(partS(ThetaAll,parai,deal_data,isholiday)/parai[3]-partS(ThetaAll,parak,deal_data,isholiday)/parak[3]))*exp(sigs[2]^2/2))) + sigmazsq(ThetaAll,parai,parak,deal_data,isholiday)/2)/
#       sqrt(sigmazsq(ThetaAll,parai,parak,deal_data,isholiday))
#     
#   }
#   
#   return(minimizer)
# }

# 
# get_deal_minimizer <- function(ThetaAll,parai,deal_data,isholiday){
#   # log normal distribution 
#   
#   parakini <- c(0.5,10,12)
#   
#   fn <- function(parak,ThetaAll,parai,deal_data,isholiday) deal_minimizer(parak,ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0)
#   gr <- function(parak,ThetaAll,parai,deal_data,isholiday) numDeriv::grad(fn, parak, ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0)
#   
#   tic()
#   paraktry <- 
#     constrOptim(theta = parakini, f=fn, 
#                 grad = gr,
#                 ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1)), ci = c(0, -1, 0, 0),
#                 ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0,
#                 method = "BFGS") -> deal_min
#   toc()
#   # paraktry <- 
#   #   constrOptim(theta = parakini, f=deal_minimizer, 
#   #               grad = numDeriv::grad(func=deal_minimizer, x = theta,
#   #                                     ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=isholiday),
#   #               ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1)), ci = c(0, -1, 0, 0),
#   #               ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=isholiday) -> deal_min
#   # 
#   # 
#   # 
#   return(deal_min)
# }



