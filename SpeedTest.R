# # minimization problem for each firm
# 
# sample_loglik_neg <- function(Theta_est,Theta_known,deal_data,isholiday){
#   # isholiday is the indicator of whether it is for holiday or non-holiday deals calculations
#   # Theta <- c(cnh, sigepsilon, sigxi, )
#   # parai <- c(alphai, Ti, di)
#   # parak <- c(alphak, Tk, dk)
#   # obsi <- c(pricei,)
#   
#   ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
#   
#   single_likelihood <- vector()
#   min_value <- vector()
#   argmin_value <- vector()
#   
#   if(isholiday == 0){
#     for(i in 1:dim(deal_data[1:2,])[1]){
#       print(i)
#       parai <- deal_data[i,]%>%select(discount, prep.period, offering_duration)%>%as.matrix()
#       # if(isholiday == 0 ){
#       #   obsi <- deal_data%>%select(price,original_price,competitors,weekends,platform,city)%>%as.matrix()
#       # }else{
#       #   obsi <- deal_data%>%select(price,original_price,competitors_hol,weekends,holiday,platform,city)%>%as.matrix()
#       # }
#       tryCatch(
#         expr = {minimizer_out <- get_deal_minimizer(ThetaAll,
#                                                     parai=c(deal_data[i,"discount"],deal_data[i,"prep.period"],deal_data[i,"offering_duration"]),
#                                                     deal_data[i,],isholiday)
#         min_value[i] <- minimizer_out$min_value
#         single_likelihood[i] <- pnorm(min_value[i])
#         argmin_value <- rbind(argmin_value,minimizer_out$deal_min$par)},
#         error = function(e){
#           message("* Caught an error on itertion ", i)
#           print(e)
#         }
#       )
#     }
#   }
#   
#   sample_loglik_neg <- -sum(log(single_likelihood),na.rm = TRUE)
#   print(paste("loop",sample_loglik_neg,sep=" "))
#   return(list(sample_loglik_neg=sample_loglik_neg,min_value=min_value,
#               argmin_value=argmin_value))
#   
# }

sample_loglik_neg <- function(Theta_est,Theta_known,deal_data,isholiday){
  # isholiday is the indicator of whether it is for holiday or non-holiday deals calculations
  # Theta <- c(cnh, sigepsilon, sigxi, )
  # parai <- c(alphai, Ti, di)
  # parak <- c(alphak, Tk, dk)
  # obsi <- c(pricei,)
  
  ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
  
  single_likelihood <- vector()
  min_value <- vector()
  argmin_value <- vector()
  
  if(isholiday == 0){
    for(i in 1:dim(deal_data[1:2,])[1]){
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
        argmin_value <- rbind(argmin_value,minimizer_out$deal_min$par)},
        error = function(e){
          message("* Caught an error on itertion ", i)
          print(e)
        }
      )
    }
  }
  
  sample_loglik_neg <- -sum(log(single_likelihood),na.rm = TRUE)
  print(paste("loop",sample_loglik_neg,sep=" "))
  return(sample_loglik_neg)
  
}



txtStart("test.txt")
sample_loglik_neg(Theta_est,Theta_known,deal_data,isholiday=0) -> tmp
txtStop()



MLE_estimation <- function(deal_data,sales_estimates,isholiday){
  if(isholiday==0){
    
    # Thetaini= c(cnh, beta0,betad,betap,betawk,betacomp,sigepsilon,sigxi)
    Theta_known <- sales_estimates[1,]
    sigs_ini <- c(log(sd(deal_data$total_volume)),log(sd(deal_data$total_volume)))
    Theta_est <- c(0.1,sales_estimates[1,c(1,2,6,9,10)],sigs_ini)
    #Theta_est <- c(1,rep(0.1,5),5,5)
    
    ThetaAll <- list(Theta_est[1],Theta_known,c(Theta_est[2:6],Theta_known[13:26]),Theta_est[7:8])
    Theta <- unlist(ThetaAll[2]) # sales_estimates
    beta <- unlist(ThetaAll[3])
    c <- unlist(ThetaAll[1])
    sigs <- unlist(ThetaAll[4])
    
    
    fn <- function(Theta_est,Theta_known,deal_data,isholiday)
      sample_loglik_neg(Theta_est,Theta_known,deal_data,isholiday)
    
    
    fn_val <- sample_loglik_neg(Theta_est,Theta_known,deal_data,isholiday)
    para_opt <- fn_val$argmin_value
    
    parMV <- function(para_opt,deal_data,sales_estimates,isholiday){
      
      D2opt <- vector()
      D2i <- vector()
      partMopt <- vector()
      partSopt <- vector()
      partSi <- vector()
      partMi <- vector()
      sigmazsq_v <- vector()
      parsigzc <- vector()
      
      parmvc <- vector()
      parmvbeta0 <- vector()
      parmvbetad <- vector()
      parmvbetap <- vector()
      parmvbetawk <- vector()
      parmvbetacomp <- vector()
      parmvsigepsilon <- vector()
      parmvsigxi <- vector()
      
      for(i in 1:dim(deal_data[1:2,])[1]){
        original_price <- deal_data[i,"original_price"]
        weekends <- deal_data[i,"weekends"] 
        competitors <- deal_data[i,"competitors"] + 1
        
        parai_v <- c(deal_data[i,"discount"],deal_data[i,"prep.period"],deal_data[i,"offering_duration"])
        
        D2opt[i] <- D2nhtilde(ThetaAll,para_opt[i,],deal_data[i,],isholiday)
        D2i[i] <- D2nhtilde(ThetaAll,parai_v,deal_data[i,],isholiday)
        partMopt[i] <- partM(ThetaAll,para_opt[i,],deal_data[i,],isholiday)
        partSopt[i] <- partS(ThetaAll,para_opt[i,],deal_data[i,],isholiday)
        partSi[i] <- partS(ThetaAll,parai_v,deal_data[i,],isholiday)
        partMi[i] <- partM(ThetaAll,parai_v,deal_data[i,],isholiday)
        
        MV_numerator <- (partMi[i]-partMopt[i]-c*(partSi[i]^2/parai_v[3] - partSopt[i]^2/para_opt[3]))
        
        sigmazsq_v[i] <- sigmazsq(ThetaAll,parai_v,para_opt[i,],deal_data[i,],isholiday)
        parsigzc[i] <- (1/(2*sqrt(sigmazsq_v[i])))*8*c*((partSopt[i]/para_opt[3] - partSi[i]/parai_v[3])^2)*(exp(sigs[1])^2 + exp(sigs[2])^2)
        
        parmvc[i] <- -(MV_numerator*parsigzc[i])/sigmazsq_v[i]
        
        parmvbeta0[i] <- (sqrt(sigmazsq_v[i])*(original_price*(D2opt[i] - D2i[i]) - c(2*partSi[i]*D2i[i]/parai_v[3] - 2*partSopt[i]*D2opt[i]/para_opt[3])) -
          4*c*(partSopt[i]/para_opt[3] - partSi[i]/parai_v[3])*(D2opt[i]/para_opt[3] - D2i[i]/parai_v[3])*(exp(sigs[1])^2 + exp(sigs[2])^2)*MV_numerator)/
          sigmazsq_v[i]
        
        parmvbetad[i] <- (sqrt(sigmazsq_v[i])*(original_price*(D2i[i]*log(parai_v[3])-D2opt[i]*log(para_opt[3])) -
                                                c*(2*partSi[i]*D2i[i]*log(parai_v[3])/parai_v[3] - 2*partSopt[i]*D2opt[i]*log(para_opt[3])/para_opt[3]))-
          (1/(2*sqrt(sigmazsq_v[i])))*4*c*(partSopt[i]/para_opt[3] - partSi[i]/parai_v[3])*(D2opt[i]*log(para_opt[3])/para_opt[3] - D2i[i]*log(parai_v[3])/parai_v[3])*
          (exp(sigs[1])^2 + exp(sigs[2])^2)*MV_numerator)/sigmazsq_v[i]
        
        parmvbetap[i] <- (sqrt(sigmazsq_v[i])*(original_price*(D2i[i]*log(original_price)-D2opt[i]*log(original_price)) -
                                                 c*(2*partSi[i]*D2i[i]*log(original_price)/parai_v[3] - 2*partSopt[i]*D2opt[i]*log(original_price)/para_opt[3]))-
          (1/(2*sqrt(sigmazsq_v[i])))*4*c*(partSopt[i]/para_opt[3] - partSi[i]/parai_v[3])*(D2opt[i]*log(original_price)/para_opt[3] - D2i[i]*log(original_price)/parai_v[3])*
          (exp(sigs[1])^2 + exp(sigs[2])^2)*MV_numerator)/sigmazsq_v[i]
        
        parmvbetawk[i] <- (sqrt(sigmazsq_v[i])*(original_price*(D2i[i]*weekends-D2opt[i]*weekends) -
                                                 c*(2*partSi[i]*D2i[i]*weekends/parai_v[3] - 2*partSopt[i]*D2opt[i]*weekends/para_opt[3]))-
                            (1/(2*sqrt(sigmazsq_v[i])))*4*c*(partSopt[i]/para_opt[3] - partSi[i]/parai_v[3])*(D2opt[i]*weekends/para_opt[3] - D2i[i]*weekends/parai_v[3])*
                            (exp(sigs[1])^2 + exp(sigs[2])^2)*MV_numerator)/sigmazsq_v[i]
        
        parmvbetacomp[i] <- (sqrt(sigmazsq_v[i])*(original_price*(D2i[i]*competitors-D2opt[i]*competitors) -
                                                  c*(2*partSi[i]*D2i[i]*competitors/parai_v[3] - 2*partSopt[i]*D2opt[i]*competitors/para_opt[3]))-
                             (1/(2*sqrt(sigmazsq_v[i])))*4*c*(partSopt[i]/para_opt[3] - partSi[i]/parai_v[3])*(D2opt[i]*competitors/para_opt[3] - D2i[i]*competitors/parai_v[3])*
                             (exp(sigs[1])^2 + exp(sigs[2])^2)*MV_numerator)/sigmazsq_v[i]
    
        parmvsigepsilon[i] <- (-MV_numerator*(1/(2*sqrt(sigmazsq_v[i])))*((2*c*(partSopt[i]/para_opt[3] - partSi[i]/parai_v[3]))^2)*(2*exp(sigs[1])^2)+
          (((parai_v[1]-para_opt[1])*original_price)^2)*(2*exp(sigs[1])^2))/sigmazsq_v[i]
        
        parmvsigxi[i] <- (-MV_numerator*(1/(2*sqrt(sigmazsq_v[i])))*((2*c*(partSopt[i]/para_opt[3] - partSi[i]/parai_v[3]))^2)*(2*exp(sigs[2])^2))/sigmazsq_v[i]
      }
      
      return(list(parmvc=parmvc,
                  parmvbeta0=parmvbeta0,
                  parmvbetad=parmvbetad,
                  parmvbetap=parmvbetap,
                  parmvbetawk=parmvbetawk,
                  parmvbetacomp=parmvbetacomp,
                  parmvsigepsilon=parmvsigepsilon,
                  parmvsigxi=parmvsigxi))
    
    }
    

    gr <- function(Theta_est,Theta_known,deal_data,isholiday){
      
      parMV_val <- parMV(para_opt=para_opt,deal_data=deal_data,sales_estimates=sales_estimates,isholiday)
      grad_c <- sum((1/pnorm(fn_val$min_value))*dnorm(fn_val$min_value)*parMV_val$parmvc)
      grad_beta0 <- sum((1/pnorm(fn_val$min_value))*dnorm(fn_val$min_value)*parMV_val$parmvbeta0)
      grad_betad <- sum((1/pnorm(fn_val$min_value))*dnorm(fn_val$min_value)*parMV_val$parmvbetad)
      grad_betap <- sum((1/pnorm(fn_val$min_value))*dnorm(fn_val$min_value)*parMV_val$parmvbetap)
      grad_betawk <- sum((1/pnorm(fn_val$min_value))*dnorm(fn_val$min_value)*parMV_val$parmvbetawk)
      grad_betacomp <- sum((1/pnorm(fn_val$min_value))*dnorm(fn_val$min_value)*parMV_val$parmvbetacomp)
      grad_sigepsilon <- sum((1/pnorm(fn_val$min_value))*dnorm(fn_val$min_value)*parMV_val$parmvsigepsilon)
      grad_sigxi <- sum((1/pnorm(fn_val$min_value))*dnorm(fn_val$min_value)*parMV_val$parmvsigxi)
      
      return(c(grad_c,grad_beta0,grad_betad,grad_betap,grad_betawk,grad_betacomp,grad_sigepsilon,grad_sigxi))
      
    }
    constrOptim(theta = Theta_est, f=fn,
    #            grad = gr,
                grad = grad(func = sample_loglik_neg, x=Theta_est,
                                        Theta_known = Theta_known, deal_data= deal_data,isholiday=0),
                ui = (rbind(c(1,rep(0,7)),c(rep(0,6),1,0),c(rep(0,7),1))), ci = rep(0,3),
                Theta_known = Theta_known, deal_data= deal_data,isholiday=0,
                #method = "Nelder-Mead"
                method = "BFGS",
                outer.eps = 1e-02
                ) -> optimTheta    
    
  }

  
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
    
    #parai <- c(deal_data[,"discount"], deal_data[,"prep.period"], deal_data[,"offering_duration"])
    
    minimizer <- (partM(ThetaAll,parai,deal_data,isholiday)-partM(ThetaAll,parak,deal_data,isholiday)+
                    (c*partS(ThetaAll,parak,deal_data,isholiday)^2)/parak[3]-(c*partS(ThetaAll,parai,deal_data,isholiday)^2)/parai[3])/
      sqrt(sigmazsq(ThetaAll,parai,parak,deal_data,isholiday))

  }
  
  return(minimizer)
}


get_deal_minimizer <- function(ThetaAll,parai,deal_data,isholiday){
  # normal distribution
  
#  profvis({
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
    
    
    
    fn <- function(parak,ThetaAll,parai,deal_data,isholiday) deal_minimizer(parak,ThetaAll=ThetaAll, parai=parai,deal_data=deal_data,isholiday=0)
    gr <- function(parak,ThetaAll,parai,deal_data,isholiday){
      deriv_vec <- vector()
      parD1alpha <- (-Theta[4]*original_price - 2*Theta[5]*(1-parak[1])*(original_price^2))*D1nhtilde(ThetaAll,parak,deal_data,isholiday)
      partSk <- partS(ThetaAll,parak,deal_data,isholiday)
      partSi <- partS(ThetaAll,parai,deal_data,isholiday)
      partMk <- partM(ThetaAll,parak,deal_data,isholiday)
      partMi <- partM(ThetaAll,parai,deal_data,isholiday)
      
      
      parSsqalpha <- 2*partSk*parD1alpha
      sigmazsq_v <- sigmazsq(ThetaAll,parai,parak,deal_data,isholiday)
      parMalpha <- -original_price*D1nhtilde(ThetaAll,parak,deal_data,isholiday) + 
        (1-parak[1])*original_price*parD1alpha
      parsigzalpha <- (1/(2*sqrt(sigmazsq_v)))*
        ((4*c/parak[3])*(partSk/parak[3]-partSi/parai[3]))*
        ((exp(sigs[1])^2 + exp(sigs[2])^2))*parD1alpha + 
        2*(original_price^2)*(parai[1]-parak[1])*(exp(sigs[1])^2)
      
      deriv_vec[1] <- (sqrt(sigmazsq_v)*(-parMalpha+(c/parak[3])*parSsqalpha)-
                         (partMi-partMk+
                            (c*partSk^2)/parak[3]-(c*partSi^2)/parai[3])*parsigzalpha)/
        sigmazsq_v
      
      deriv_vec[2] <- 0
      
      parD1d <- (Theta[2]/parak[3] + Theta[3]*2*log(parak[3])/parak[3])*D1nhtilde(ThetaAll,parak,deal_data,isholiday)
      parD2d <- (beta[2]/parak[3])*D2nhtilde(ThetaAll,parak,deal_data,isholiday)
      parMd <- (1-parak[1])*original_price*parD1d + original_price*parD2d
      parSsqd <- 2*partSk*(parD1d+parD2d)
      parsigzd <- (1/(2*sqrt(sigmazsq_v)))*
        (4*(c/parak[3])*(partSk/parak[3]-partSi/parai[3])*
           (parD1d+parD2d)*(exp(sigs[1])^2 + exp(sigs[2])^2))
      
      deriv_vec[3] <- (sqrt(sigmazsq_v)*(-parMd+(c/parak[3])*parSsqd - c*partSk^2/(parak[3]^2))-
                         (partMi-partM(ThetaAll,parak,deal_data,isholiday)+
                            (c*partSk^2)/parak[3]-(c*partSi^2)/parai[3])*parsigzd)/
        sigmazsq_v
      
      return(deriv_vec)
      
    } 
    
    # gr_num <- function(parak,ThetaAll,parai,deal_data,isholiday) numDeriv::grad(func=deal_minimizer, x = parak, 
    #                                                                             ThetaAll=ThetaAll, parai=parai,deal_data=deal_data,isholiday=0)
    
    #tic()
    parak <- parai+0.01
    tryCatch(constrOptim(
                         theta = parak, f=fn, 
                         grad = gr,
                         #grad = gr_num,
                         ui = rbind(c(1,0,0),c(-1, 0, 0),c(0, 1, 0), c(0, -1, 1), c(0, 0, -1)), ci = c(0, -1, 0, 0,-50),
                         ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0,
                         method = "BFGS") -> deal_min)
    
    min_value <- fn(deal_min$par%>%round(digits = 3),ThetaAll=ThetaAll,parai=parai,deal_data=deal_data,isholiday=0)
    #toc()
    
  }
#  })
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



