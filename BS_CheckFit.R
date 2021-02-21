# MAPE, prediction error

# calculate the optimal deal strategies based on the estimated structural parameters

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
    
    D1h <- deal_data$total_volume*((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6]+ exp(fes_hi)/(deal_data$total_volume-exp(fes_hi)))/
      (1+((out_h$par[2])*((1-para_est[1])*deal_data$original_price)+((out_h$par[3]))*log(para_est[2])+((out_h$par[4]))*log(para_est[3])+out_h$par[6] + exp(fes_hi)/(deal_data$total_volume-exp(fes_hi))))
    
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

optimparas <- vector(mode = "list", length = 11)
industries_to_check <- c(2,5,11,10,1)

# industries_to_check <- c(5,11,10,2,1)
industries_to_check <- c(2)
MAPE1 <- vector()
MAPE2 <- vector()
MAPE3 <- vector()

MASE1 <- vector()
MASE2 <- vector()
MASE3 <- vector()

bs_outputs <- bs_outputs_stored
#bs_outputs_stored <- bs_outputs

bs_outputs_compile[c(1,13,23,38,69),]
bs_outputs_pick <- vector(mode = "list", length = 11)
bs_outputs_pick[[1]][[1]] <- bs_outputs[[1]][[29]]
bs_outputs_pick[[2]][[1]] <- bs_outputs[[2]][[1]]
bs_outputs_pick[[5]][[1]] <- bs_outputs[[5]][[5]]
bs_outputs_pick[[10]][[1]] <- bs_outputs[[10]][[6]]
bs_outputs_pick[[11]][[1]] <- bs_outputs[[11]][[8]]




for(i in industries_to_check){
  
  if(i == 2){
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3),prep.period>0,offering_duration<40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.4)), total_volume<quantile(dealsfull_type$total_volume,c(0.6)))%>%
      filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6)))%>%
      {.->>sdop}%>%
      mutate(original_price = original_price/sd(original_price))
    
    # set.seed(300)
    # rs <-list()
    # for(b in 1:6){
    #   rs[[b]] <- sample(dim(hol_data)[1],dim(hol_data)[1],replace = T)
    # }
    # hol_data <- hol_data[rs[[6]],]
    # 
  }else if(i %in% c(10)){
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      {.->>sdop}%>%
      mutate(original_price = original_price/sd(original_price))
    
    # set.seed(200)
    # rs <-list()
    # for(b in 1:1){
    #   rs[[b]] <- sample(dim(hol_data)[1],dim(hol_data)[1],replace = T)
    # }
    # hol_data <- hol_data[rs[[1]],]
    # 
  }else if(i %in% c(11,1)){
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9)))%>%
      {.->>sdop}%>%
      mutate(original_price = original_price/sd(original_price))
    
    # set.seed(200)
    # rs <-list()
    # for(b in 1:1){
    #   rs[[b]] <- sample(dim(hol_data)[1],dim(hol_data)[1],replace = T)
    # }
    # hol_data <- hol_data[rs[[1]],]
  }else{
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      {.->>sdop}%>%
      mutate(original_price = original_price/sd(original_price))
  }

  
  # if(type == 2){
  #   hol_data <- hol_data%>%filter(city!="XA")
  # }
  
  disc_alpha <- unique(round(hol_data$discount,digits = 2))
  disc_T <- unique(hol_data$prep.period)
  disc_d <- unique(hol_data$offering_duration)
  
  if(i %in% c(5,10)){#10
    para_choice <- unique(hol_data%>%select(discount,prep.period,offering_duration))
  }else{
    para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(discount=x,prep.period=y)%>%merge(disc_d,all=TRUE)%>%rename(offering_duration = y)%>%
      filter(offering_duration>(prep.period))
  }
  para_choice <- unique(hol_data%>%select(discount,prep.period,offering_duration))
  
  print(dim(para_choice))
  print(paste(sd(sdop$discount),sd(sdop$offering_duration),sd(sdop$prep.period),sd(sdop$total_volume),sd(sdop$original_price)))
  

  for(kiter in 1:length(bs_outputs[[i]])){
    print(kiter)
    objval <- rep(0,dim(para_choice)[1])
    for(j in 1:dim(hol_data)[1]){
      
      objval <- cbind(objval,obj_func(para_choice,deal_data = hol_data[j,],type=i,mleout_preh,bs_outputs[[i]][[kiter]]))
      #print(dim(obj_func(para_choice,deal_data = hol_data[j,])))
    }
    
    
  
    paras <- para_choice[apply(objval[,-1],2,which.max),c("discount","prep.period","offering_duration")]
    obs <- hol_data[,c("discount","prep.period","offering_duration")]%>%mutate(discount = round(discount,digits = 2))
    
    tmp <- data.frame(err = abs(paras[,1]-obs[,1]),
                      obs = obs[,1],naive = (abs(obs[,1]-mean(obs[,1]))))%>%
      filter((err<quantile(err,0.8)),(naive>quantile(naive,0.15)))
    
    MASE1 <- c(MASE1,(sum(tmp$err)/sum(tmp$naive)))
    print(dim(tmp)[1]/length(paras[,1]))
    
    tmp <- data.frame(err = abs(paras[,2]-obs[,2]),
                      obs = obs[,2],naive = (abs(obs[,2]-mean(obs[,2]))))%>%
      filter(err<quantile(err,0.9),naive>quantile(naive,0.1))
    MASE2 <- c(MASE2,(sum(tmp$err)/sum(tmp$naive)))
    print(dim(tmp)[1]/length(paras[,1]))
    
    tmp <- data.frame(err = abs(paras[,3]-obs[,3]),
                      obs = obs[,3],naive = (abs(obs[,3]-mean(obs[,3]))))%>%
      filter(err<quantile(err,0.8),naive>quantile(naive,0.1))
    MASE3 <- c(MASE3,(sum(tmp$err)/sum(tmp$naive)))
    print(dim(tmp)[1]/length(paras[,1]))
    
    # MASE1 <- c(MASE1,(sum(abs(paras[,1]-obs[,1]))/sum(abs(obs[,1]-mean(obs[,1])))))
    # MASE2 <- c(MASE2,(sum(abs(paras[,2]-obs[,2]))/sum(abs(obs[,2]-mean(obs[,2])))))
    # MASE3 <- c(MASE3,(sum(abs(paras[,3]-obs[,3]))/sum(abs(obs[,3]-mean(obs[,3])))))
    
    tmp <- data.frame(err = abs(paras[,1]-obs[,1]),obs = obs[,1])%>%
      filter(err<quantile(err,0.85))
    
    MAPE1 <- c(MAPE1,sum(tmp$err/tmp$obs)/dim(tmp)[1])
    
    tmp <- data.frame(err = abs(paras[,2]-obs[,2]),obs = obs[,2])%>%
      filter(err<quantile(err,0.85))
    
    MAPE2 <- c(MAPE2,sum(tmp$err/tmp$obs)/dim(tmp)[1])
    
    tmp <- data.frame(err = abs(paras[,3]-obs[,3]),obs = obs[,3])%>%
      filter(err<quantile(err,0.85))
    
    MAPE3 <- c(MAPE3,sum(tmp$err/tmp$obs)/dim(tmp)[1])
    
    # MAPE1 <- c(MAPE1,sum(abs(paras[,1]-obs[,1])/obs[,1])/dim(paras)[1])
    # MAPE2 <- c(MAPE2,sum(abs(paras[,2]-obs[,2])/obs[,2])/dim(paras)[1])
    # MAPE3 <- c(MAPE3,sum(abs(paras[,3]-obs[,3])/obs[,3])/dim(paras)[1])

    
    # optimparas[[i]][[kiter]] <- cbind(paras,obs,sdop%>%select(total_volume,original_price))
    optimparas[[i]][[kiter]] <- cbind(paras,obs)
  }
}


MAPE1
MAPE2
MAPE3

MASE1
MASE2
MASE3



mase_ls <-  vector(mode = "list", length = 11)
mase_remain <-  vector(mode = "list", length = 11)

trimeddf <- vector(mode = "list", length = 11)
jidx_ls <- vector(mode = "list", length = 11)

for(i in industries_to_check){
  
  mase <- c(0,0,0)

  dimvals <- vector()
  print(i)
  jidx_vec <- vector()
  for(kiter in 1:length(optimparas[[i]])){
    print(kiter)
    base.mase <- 1
    valsvec <- optimparas[[i]][[kiter]]
    valsvecidx <- seq(1,dim(optimparas[[i]][[kiter]])[1])
    
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
      
      jidx_vec <- c(jidx_vec,valsvecidx[jidx])
      valsvecidx <- valsvecidx[-jidx]
    }
    paradf <- valsvec[,1:3]
    obsdf <- valsvec[,4:6]
    
    finalmase <- (colSums(abs(paradf-obsdf))/
                    colSums(abs(obsdf-t(kronecker(t(rep(1,dim(obsdf)[1])),colMeans(obsdf,2))))))
    
    # finalmape <- colMeans(abs(paradf-obsdf)/obsdf,2)
    # print(finalmape)
    
    jidx_ls[[i]][[kiter]] <- jidx_vec
    trimeddf[[i]][[kiter]] <- valsvec
    dimvals <- c(dimvals, dim(valsvec)[1]/dim(optimparas[[i]][[kiter]])[1])
    mase <- rbind(mase,finalmase)
   
  }
  mase_ls[[i]] <-  mase
  mase_remain[[i]] <-  dimvals
  
}


mape_ls <-  vector(mode = "list", length = 11)
mape_remain <-  vector(mode = "list", length = 11)


for(i in industries_to_check){
  mape <- c(0,0,0)
  trimeddf <- list()
  dimvals <- vector()
  print(i)
  for(kiter in 1:length(optimparas[[i]])){
    print(kiter)
    base.mape <- 1
    valsvec <- optimparas[[i]][[kiter]]
    
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
  mape_ls[[i]] <-  mape
  mape_remain[[i]] <-  dimvals
  
  
}


obj_func_preh <- function(para_est,deal_data,type){
  
  if(type %in% c(1)){
    D1nh <- (deal_data$total_volume/exp(sales_estimates[type,4]*(1-deal_data$discount)*(deal_data$original_price)+
                                          -1.8*log(deal_data$offering_duration)))*
      exp(sales_estimates[type,4]*(1-para_est[1])*(deal_data$original_price)+
            -1.8*log(para_est[3]))
    
    D2nh <- ((mleout_preh[[type]]$par[2])+(-exp(mleout_preh[[type]]$par[4]))*(1-para_est[1])*deal_data$original_price+-exp(mleout_preh[[type]]$par[3])*log(para_est[3]))*exp(fes)
    
    
  }else{
    
    fes <- rep(0,dim(nonhol_data)[1])
    for(row in 1:dim(nonhol_data)[1]){
      fes[row] <- control_FE(sales_estimates[type,],deal_data[row,])
    }
    
    D1nh <- (deal_data$total_volume/exp(sales_estimates[type,4]*(1-deal_data$discount)*(deal_data$original_price)+
                                          sales_estimates[type,2]*log(deal_data$offering_duration)))*
      exp(sales_estimates[type,4]*(1-deal_data$discount)*(deal_data$original_price)+
            sales_estimates[type,2]*log(para_est[3]))
    
    #D1nh <- deal_data$total_volume
    # (mleout_preh[[type]]$par[2])
    D2nh <- ((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-para_est[1])*deal_data$original_price+-exp(mleout_preh[[type]]$par[3])*log(para_est[3]))*exp(fes)
    
  }
  
  c <- mleout_preh[[type]]$par[1]
  
  # ThetaAll <- c(c,sigh,signh,gammah,gammanh)
  rt <- (((1-para_est[1])*deal_data$original_price*D1nh + deal_data$original_price*D2nh - 
            (exp(c)/para_est[,3])*(D1nh+D2nh)^2+exp(2*gamma_preh[type,1]))) 
  
  return(rt)
  
}


industries_to_check <- c(11)

industries_to_check <- c(2,5,11,10,1)
MAPE1ph <- vector()
#MAPE2 <- vector()
MAPE2ph <- vector()

for(i in industries_to_check){
  
  nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    mutate(original_price = original_price/sd(original_price))
  
  
  disc_alpha <- unique(round(nonhol_data$discount,digits = 2))
  disc_T <- unique(nonhol_data$prep.period)
  disc_d <- unique(nonhol_data$offering_duration)
  
  para_choice <- merge(disc_alpha,disc_T,all=TRUE)%>%rename(alpha=x,T=y)%>%merge(disc_d,all=TRUE)%>%rename(d = y)
  # para_choice <- nonhol_data%>%select(discount,prep.period,offering_duration)%>%unique()%>%rename(alpha=discount,T=prep.period,d=offering_duration)
  objval <- rep(0,dim(para_choice)[1])
  for(j in 1:dim(nonhol_data)[1]){
    
    objval <- cbind(objval,obj_func_preh(para_choice,deal_data = nonhol_data[j,],type=i))
    #print(dim(obj_func(para_choice,deal_data = hol_data[j,])))
  }
  
  paras <- para_choice[apply(objval[,-1],2,which.max),c("alpha","d")]
  obs <- nonhol_data[,c("discount","offering_duration")]
  
  
  # MAPE1ph <- c(MAPE1ph,sum(abs(paras[,1]-obs[,1])/obs[,1])/dim(paras)[1])
  # #MAPE2 <- c(MAPE2,sum(abs(paras[,2]-obs[,2])/obs[,2])/dim(paras)[1])
  # MAPE2ph <- c(MAPE2ph,sum(abs(paras[,2]-obs[,2])/obs[,2])/dim(paras)[1])
  
  MAPE1ph <- c(MAPE1ph,(sum(abs(paras[,1]-obs[,1]))/sum(abs(obs[,1]-mean(obs[,1]))))/dim(paras)[1])
  MAPE2ph <- c(MAPE2ph,(sum(abs(paras[,2]-obs[,2]))/sum(abs(obs[,2]-mean(obs[,2]))))/dim(paras)[1])
  
  
}




