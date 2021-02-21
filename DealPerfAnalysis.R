industries_to_check <- c(2,5,11,10,1)
industries_to_check <- c(2)
r <- list()
D2hval <- list()
D1hvalpri <- list()
D2hvalpri <- list()
D2nhval <- list()
D2nhvalpri <- list()
D1hvalprid1 <- list()
D2hvalprid1 <- list()
D2nhvalprid1 <- list()
D2nhvalprid2 <- list()
Spri <- list()
dsidx <- list()
dsidx2 <- list()
dalter <- list()
Dtotalpri <- list()
nondealprofit <- list()
dealprofit <- list()
profitalter <- list()
chs <- vector()
cnhs <- vector()
for(i in industries_to_check){
  nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdopph}%>%
    mutate(original_price = original_price/sd(original_price))
  
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    {.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  if(i == 2){
    hol_data <- hol_data%>%filter(city!="XA")
  }else if(i %in% c(5)){
    hol_data <- hol_data%>%filter(platform!="ww",city!="GZ")
  }
  
  print(i)
  ch <- (exp(ceiling(mleout_h[[i]]$par[1])))
  cnh <- (exp(floor(mleout_preh[[i]]$par[1])))
  #print(paste(mleout_h[[i]]$par[1],mleout_preh[[i]]$par[1]))
  chs <- c(chs, (exp((mleout_h[[i]]$par[1])))*sd(sdop$original_price))
  cnhs <- c(cnhs,(exp((mleout_preh[[i]]$par[1])))*sd(sdopph$original_price))
  print(paste("mch = ",(exp((mleout_h[[i]]$par[1])))*sd(sdop$original_price)))
  print(paste("mcph = ",(exp((mleout_preh[[i]]$par[1])))*sd(sdopph$original_price)))
  

  
  if(i %in% c(2)){
    Spri[[i]] <- (hol_data$total_volume/exp(-3.0*(1-hol_data$discount)*hol_data$original_price +
                                         0.9*log(hol_data$offering_duration) + 
                                         1.23*log(hol_data$prep.period))*
                    exp(-3.0*hol_data$original_price))
    
    
    
  }else if(i %in% c(5)){
    
    Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                         1.24*log(hol_data$offering_duration) #+
                                         #1.07*log(hol_data$prep.period)
                                         )#*
                    # exp(sales_estimates_h[i,4]*hol_data$original_price)
                   )
    
    
    # Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
    #                                      1.24*log(hol_data$offering_duration) +
    #                                      1.07*log(hol_data$prep.period)
    #                                      )*
    #                  exp(sales_estimates_h[i,4]*hol_data$original_price)
    #                )
    
    # D1S <- (deal_data$total_volume)
  }else if(i %in% c(10)){
    Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                         1.7*log(hol_data$offering_duration) + 
                                         1.05*log(hol_data$prep.period))*
                     exp(sales_estimates_h[i,4]*hol_data$original_price))
    
  }else if(i %in% c(1)){
    
    Spri[[i]]  <-  (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                          1.2*log(hol_data$offering_duration)+
                                          1.24*log(hol_data$prep.period))*
                      exp(sales_estimates_h[i,4]*hol_data$original_price))
  }else{
    Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                         1.2*log(hol_data$offering_duration)+
                                         1.00*log(hol_data$prep.period))*
                     exp(sales_estimates_h[i,4]*hol_data$original_price))
    
  }
  
  if(i %in% c(1)){

    
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    Stotpri <- hol_data$total_volume
    Stot <- hol_data$total_volume
    
    # D1hval[[i]] <- hol_data$total_volume*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
    #   (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    # 
    # D1hvalpri[[i]] <- hol_data$total_volume*(mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
    #   (1+(mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    # 
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- Stotpri*(mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))    
    
  }else if (i %in%c(2)){
    # D1hval[[i]] <- hol_data$total_volume*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
    #   (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    # 
    # D1hvalpri[[i]] <- hol_data$total_volume*(+mleout_h[[i]]$par[6])/
    #   (1+(mleout_h[[i]]$par[6]))
    
    # r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
    #   (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    # 
    
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    
    Stotpri <-hol_data$total_volume
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*(-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- Stotpri*(((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
  
    D1hvalprid1[[i]] <- Stot*(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    
  }else if (i %in% c(5)){
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    
    Stotpri <-hol_data$total_volume
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    D1hvalpri[[i]] <- Stotpri*(+mleout_h[[i]]$par[6])/
      (1+(mleout_h[[i]]$par[6]))
    
    D1hvalprid1[[i]] <- Stot*(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6])/
      (1+(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6]))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
  }else if(i %in% c(11)){
    
    Stotpri <- hol_data$total_volume
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    D1hvalpri[[i]] <- Stotpri*(+mleout_h[[i]]$par[6])/
      (1+(mleout_h[[i]]$par[6]))
    
    D1hvalprid1[[i]] <- Stot*(+((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6])/
      (1+(+((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6]))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
  }else if(i %in% c(10)){
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    
    
    Stotpri <- hol_data$total_volume
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- Stotpri*(+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalprid1[[i]] <- Stot*(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
  }
  
  
  if(i %in% c(1,10)){
    
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- ((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                       -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    D2nhvalpri[[i]] <- ((mleout_preh[[i]]$par[2])+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    
    
    D2nhvalprid1[[i]] <- ((mleout_preh[[i]]$par[2])+
                          -exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes))
    
    fes_nh <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhvalprid2[[i]] <- ((mleout_preh[[i]]$par[2])+
                            -exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes_nh))
    
  }else if(i %in% c(11)){
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))
    
    D2nhvalpri[[i]] <-  exp((mleout_preh[[i]]$par[2])+
                              -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))
    
    D2nhvalprid1[[i]] <-  exp((mleout_preh[[i]]$par[2])+
                              -exp(mleout_preh[[i]]$par[3])*log(2))
    
    fes_nh <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhvalprid2[[i]] <-  exp((mleout_preh[[i]]$par[2])+
                                -exp(mleout_preh[[i]]$par[3])*log(2))
    
  }else if(i %in% c(5)){
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    
    D2nhvalpri[[i]] <- exp((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes))
    
    D2nhvalprid1[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes))
    
    fes_nh <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhvalprid2[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes_nh))
    
    
    
    
  }else{
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    
    D2nhvalpri[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    D2nhvalprid1[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes))
    
    fes_nh <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhvalprid2[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes_nh))
    
  }

  
  if(i %in% c(1)){
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                         D2nhval[[i]])
    
    D2hvalpri[[i]] <- exp(((mleout_h[[i]]$par[7]))+
                            D2nhvalpri[[i]])
    
    D2hvalprid1[[i]] <- exp((mleout_h[[i]]$par[5])*log(2)+((mleout_h[[i]]$par[7]))+
                            D2nhvalprid1[[i]])
    
  }else if(i %in% c(10)){
    D2hval[[i]] <- ((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                      abs(D2nhval[[i]]))
    
    D2hvalpri[[i]] <- (((mleout_h[[i]]$par[7]))+
                         abs(D2nhvalpri[[i]]))
    
    
    D2hvalprid1[[i]] <- ((mleout_h[[i]]$par[5])*log(2) + ((mleout_h[[i]]$par[7]))+
                         (D2nhvalprid1[[i]]))
    
  }else if(i %in% c(2)){
    
    D2hval[[i]] <- exp(-exp(mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                         (log(D2nhval[[i]])))
    
    D2hvalpri[[i]] <- exp(((mleout_h[[i]]$par[7]))+
                         (log(D2nhvalpri[[i]])))
    
    D2hvalprid1[[i]] <- exp(-exp(mleout_h[[i]]$par[5])*log(2)+((mleout_h[[i]]$par[7]))+
                            (log(D2nhvalprid1[[i]])))
    
  }else if(i %in% c(5)){
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7])))
    
    D2hvalpri[[i]] <- exp(((mleout_h[[i]]$par[7])))
    
    D2hvalprid1[[i]] <- exp((mleout_h[[i]]$par[5])*log(2)+((mleout_h[[i]]$par[7])))
    
  }else{
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7])))
    
    D2hvalpri[[i]] <- exp(((mleout_h[[i]]$par[7])))
    
    D2hvalprid1[[i]] <- exp((mleout_h[[i]]$par[5])*log(2)+((mleout_h[[i]]$par[7])))
    
  }
  
  
  nondealdemandhol <- D1hvalpri[[i]] + D2hvalpri[[i]] 
  nondealdemandnhol <- Stotpri - D1hvalpri[[i]] + pmax(D2nhvalpri[[i]],0)
  
  dealdemandhol <- D1hval[[i]] + D2hval[[i]]
  dealdemandnhol <- Stot - D1hval[[i]] + pmax(D2nhval[[i]],0)
  
  dsidx2[[i]] <- (-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
    ((hol_data$total_volume + D2hval[[i]] + pmax(D2nhval[[i]],0))/hol_data$holiday.len)-
    (-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
    ((Stotpri + D2hvalpri[[i]] + pmax(D2nhvalpri[[i]],0))/hol_data$holiday.len)
  
  
  nondealdemandhol <- D1hvalpri[[i]] 
  nondealdemandnhol <- hol_data$total_volume- D1hvalpri[[i]] 
  
  dealdemandhol <- D1hval[[i]] 
  dealdemandnhol <- hol_data$total_volume - D1hval[[i]] 
  
  dsidx[[i]] <- (-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)-
    (-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len) 
  
  
  avgdailydemand <- (hol_data$total_volume + D2hval[[i]] )/hol_data$holiday.len
  # print(mean(dsidx[[i]]/avgdailydemand,na.rm = TRUE))
  print(paste("dsidx = ", mean(dsidx[[i]],na.rm = TRUE)/mean(avgdailydemand)))
  print(paste("dsidx2 = ", mean(dsidx2[[i]],na.rm = TRUE)))
  #print(sd(sdop$original_price))
  #print(sd(sdop$original_price)*exp(mleout_h[[i]]$par[1]))
  Dtotalpri[[i]] <- as.numeric(Spri[[i]] + D2nhvalpri[[i]] + D2hvalpri[[i]]) 
  
  # dalter[[i]] <- (-(as.numeric(Spri[[i]] + max(D2nhvalpri[[i]],0 )+ max(D2hvalpri[[i]],0))) + 
  #   as.numeric(hol_data$total_volume + max(D2nhval[[i]],0)+ max(D2hval[[i]],0)))/
  #   ((as.numeric(Spri[[i]] + max(D2nhvalpri[[i]],0 )+ max(D2hvalpri[[i]],0))))
  
  
  dalter[[i]] <- (-(as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))) +
                    as.numeric(hol_data$total_volume + (D2nhval[[i]])+ (D2hval[[i]])))/
    ((as.numeric(Spri[[i]] + max(D2nhvalpri[[i]],0 )+ max(D2hvalpri[[i]],0))))
  
  nondealprofit[[i]] <- hol_data$original_price*Spri[[i]] + 
    hol_data$original_price*(pmax(D2nhvalpri[[i]],0) + pmax(D2hvalpri[[i]],0)) - 
    cnh*((pmax(D2nhvalpri[[i]],0)+pmax(nondealdemandnhol,0))^2) -
    ch*(pmax(D2hvalpri[[i]],0) + pmax(nondealdemandhol,0))^2
  
  dealprofit[[i]] <- hol_data$original_price*(1-hol_data$discount)*hol_data$total_volume + 
    hol_data$original_price*(pmax(D2nhval[[i]],0) +pmax(D2hval[[i]],0)) - 
    cnh*((pmax(D2nhval[[i]],0)+dealdemandnhol)^2) -
    ch*(pmax(D2hval[[i]],0) + dealdemandhol)^2
  
  profitalter[[i]] <- (-nondealprofit[[i]]+dealprofit[[i]])/abs(nondealprofit[[i]])
  
  print(paste("D1h = ",mean(D1hval[[i]]), " ", sd(D1hval[[i]])))
  print(paste("D2h = ", mean(D2hval[[i]]), " ", sd(D2hval[[i]])))
  print(paste("D2nh = ", mean(D2nhval[[i]]), " ", sd(D2nhval[[i]])))
  #print(mean(r[[i]][r[[i]]>0]))
  print(paste("ratio = ", mean(r[[i]])))
  print(paste("alterD = ", mean(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.8)&
                                            dalter[[i]]>quantile(dalter[[i]],0.2)])))
  print(paste("sig alterD = ", sd(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.8)&
                                              dalter[[i]]>quantile(dalter[[i]],0.2)])))
  print(paste("alterP = ", median(profitalter[[i]])))
  print(paste("alterP = ", sd(profitalter[[i]])))
  
  
  muc <- (exp(ceiling(mleout_h[[i]]$par[1])))*sd(sdop$original_price)
  mucp <- mean(muc/sdop$original_price[sdop$original_price<quantile(sdop$original_price,0.9)&
                                    sdop$original_price>quantile(sdop$original_price,0.1)])

  sigcp <- sd(muc/sdop$original_price[sdop$original_price<quantile(sdop$original_price,0.9)&
                                            sdop$original_price>quantile(sdop$original_price,0.1)])
  
  # muh <- mean((tt$hval))
  # munh <- mean((tt$nonval))
  # sigh <- sd((tt$hval))
  # signh<- sd(tt$nonval)

  
  print(paste("cp ratio mean ",mucp, "; sigcp", sigcp))
  
}



VarRed <- list()
for(i in industries_to_check){
  ch <- (exp(ceiling(mleout_h[[i]]$par[1])))
  cnh <- (exp(floor(mleout_preh[[i]]$par[1])))
  
  VarRed[[i]] <- (-cnh*exp(2*gamma_h_var[i,1]+2*gamma_h_var[i,2]*(hol_data$prep.period))-ch*exp(2*gamma_h_var[i,1]+2*gamma_h_var[i,2]*(hol_data$prep.period))+
    cnh*exp(2*gamma_h_var[i,1]) + ch*exp(2*gamma_h_var[i,1]))/
    (cnh*exp(2*gamma_h_var[i,1]) + ch*exp(2*gamma_h_var[i,1]))
  
  print(paste("VarRed index = ", mean(VarRed[[i]])))
  
}


deltaD <- list()
holnormdemand <- list()
nonholnormdemand <- list()

for(i in industries_to_check){
  
  # nonhol_data <- dealsfull%>%
  #   mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
  #   filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
  #   #filter(total_volume>quantile(tmp$total_volume,c(0.05)), total_volume<quantile(tmp$total_volume,c(0.95)))%>%
  #   mutate(original_price = original_price/sd(original_price))
  
  nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdopph}%>%
    mutate(original_price = original_price/sd(original_price))
  
  
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    {.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  
  if(i == 2){
    hol_data <- hol_data%>%filter(city!="XA")
  }else if(i %in% c(5)){
    hol_data <- hol_data%>%filter(platform!="ww",city!="GZ")
  }

  tmp <- nonhol_data%>%mutate(discprice = (1-discount)*original_price/partysize, # = price/partysize
                                         logvol = log(total_volume),
                                         perperson = original_price/partysize,
                                         competitors_ct = (competitors/sd(competitors)),
                                         logcomp = ifelse(competitors!=0,log(competitors),0),
                                         fixlength = ifelse(offering_duration==15,1,
                                                            ifelse(offering_duration==30,2,
                                                                   ifelse(offering_duration==45,3,0))))
  
  lm_nonhol <- lm(data=nonhol_data%>%mutate(discprice = (1-discount)*original_price/partysize, # = price/partysize
                                         logvol = log(total_volume),
                                         perperson = original_price/partysize,
                                         competitors_ct = (competitors/sd(competitors)),
                                         logcomp = ifelse(competitors!=0,log(competitors),0),
                                         fixlength = ifelse(offering_duration==15,1,
                                                            ifelse(offering_duration==30,2,
                                                                   ifelse(offering_duration==45,3,0)))),
                  logvol~log(offering_duration)+I(log(offering_duration)^2)+
                    discprice+I(discprice^2)+
                    # log(discprice)+I(log(discprice)^2)+
                    I(log(original_price))+I(1/log(original_price))+I(1/(log(original_price)^2))+
                    logcomp+weekends+platform:weekends+
                    platform+city)
  
  platform <- t(cbind(tmp%>%select(platform),tmp%>%select(platform)) == 
                  matrix(rep(sub("platform",'',rownames(coef((summary(lm_nonhol))))[grepl("platform",rownames(coef((summary(lm_nonhol)))))&
                                                                           !grepl(":",rownames(coef((summary(lm_nonhol)))))]),dim(tmp)[1]),ncol=2,byrow = TRUE))
  #platform <- platform[1,]
  
  city <- t(matrix(rep(tmp$city,length(sub("city",'',rownames(coef((summary(lm_nonhol))))[grepl("city",rownames(coef((summary(lm_nonhol)))))]))),
                   nrow = dim(tmp)[1],byrow=FALSE) == 
              matrix(rep(sub("city",'',rownames(coef((summary(lm_nonhol))))[grepl("city",rownames(coef((summary(lm_nonhol)))))]),
                         dim(tmp)[1]),ncol = length(sub("city",'',rownames(coef((summary(lm_nonhol))))[grepl("city",rownames(coef((summary(lm_nonhol)))))])),byrow = TRUE))
  weekends <- tmp$weekends
  pfwk <- (platform*weekends)
  
  if(i %in% c(5)){
    # nonholnormdemand[[i]] <-  exp(t(coef(summary(lm_nonhol))[,1])[c(1,6,7,8,9,10,11,12,seq(13,28))]%*%
    #                                 rbind(1,
    #                                       #log(2),(log(2)^2),
    #                                       #tmp$discprice,(tmp$discprice)^2,
    #                                       log(tmp$original_price),1/log(tmp$original_price),1/(log(tmp$original_price)^2),
    #                                       tmp$logcomp,
    #                                       weekends,platform,city,pfwk)%>%as.matrix()+lm_nonhol$residuals)/(tmp$offering_duration)+ #
    #   D2nhvalprid2[[i]] 
    
    nonholnormdemand[[i]] <- D2nhvalprid2[[i]] + nonhol_data$total_volume/exp(coef((summary(lm_nonhol)))[4]*(1-nonhol_data$discount)*nonhol_data$original_price #+
                                                                                #coef((summary(lm_nonhol)))[2]*log(nonhol_data$offering_duration)
                                                                              )*
      exp(coef((summary(lm_nonhol)))[4]*nonhol_data$original_price)
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + D2nhvalpri[[i]]+ (D2hvalpri[[i]]))) #Spri[[i]] +
  }else if(i %in% c(10,2)){
    
    nonholnormdemand[[i]] <- D2nhvalprid2[[i]] + nonhol_data$total_volume/exp(coef((summary(lm_nonhol)))[4]*(1-nonhol_data$discount)*nonhol_data$original_price +
                                                                                coef((summary(lm_nonhol)))[2]*log(nonhol_data$offering_duration))*
      exp(coef((summary(lm_nonhol)))[4]*nonhol_data$original_price)
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + D2nhvalpri[[i]]+ (D2hvalpri[[i]]))) #Spri[[i]] +
  }else if(i %in% c(1)){
    nonholnormdemand[[i]] <- D2nhvalprid2[[i]] + nonhol_data$total_volume/exp(coef((summary(lm_nonhol)))[4]*(1-nonhol_data$discount)*nonhol_data$original_price +
                                                                                coef((summary(lm_nonhol)))[2]*log(nonhol_data$offering_duration))*
      exp(coef((summary(lm_nonhol)))[4]*nonhol_data$original_price)
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + D2nhvalprid1[[i]]+ (D2hvalpri[[i]]))) #Spri[[i]] +
    
  }else if(i %in% c(11)){
    nonholnormdemand[[i]] <- D2nhvalprid2[[i]] + nonhol_data$total_volume#/exp(1.7*(1-nonhol_data$discount)*nonhol_data$original_price +
                                                                           #     1.41*log(nonhol_data$offering_duration))*
     # exp(1.7*nonhol_data$original_price)
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + D2nhvalpri[[i]]+ (D2hvalpri[[i]]))) #Spri[[i]] +
    
  }


 

     
  #holnormdemand[[i]] <- (D1hvalprid1[[i]]+D2hvalpri[[i]])

  
  
  # deltaD[[i]] <- (-median(nonholnormdemand[[i]][nonholnormdemand[[i]]>1]) + median(holnormdemand[[i]]))/(median(nonholnormdemand[[i]]))
  
  deltaD[[i]] <- (-mean(nonholnormdemand[[i]]) + mean(holnormdemand[[i]]))/(mean(nonholnormdemand[[i]]))
}


tt <- merge(data.frame(nonval = (nonholnormdemand[[i]])),
            data.frame(hval = (holnormdemand[[i]])),all=TRUE)%>%
  mutate(n9 = quantile(nonval,0.9),n05 = quantile(nonval,0.1))%>%
  mutate(h95 = quantile(hval,0.9),h05 = quantile(hval,0.1))%>%
  filter(nonval <n9, nonval>n05,hval<h95,hval>h05)%>%
  mutate(diff=(hval-nonval)/nonval)


ub <- 0.8
lb <- 0.2
tt <- merge(data.frame(nonval = (nonholnormdemand[[i]]),
                       op = nonhol_data$original_price*sd(sdopph$original_price)),
            data.frame(hval = (holnormdemand[[i]]),
                       op = hol_data$original_price*sd(sdop$original_price)),by="op",all=TRUE)%>%
  mutate(n9 = quantile(nonval,ub,na.rm = TRUE),n05 = quantile(nonval,lb,na.rm = TRUE))%>%
  mutate(h95 = quantile(hval,ub,na.rm = TRUE),h05 = quantile(hval,lb,na.rm = TRUE))%>%
  #filter(op<quantile(op,0.9),op>quantile(op,0.1))%>%
  filter(nonval <n9, nonval>n05,hval<h95,hval>h05)%>%
  mutate(diff=(hval-nonval)/nonval)%>%
  filter(is.na(diff)==0)

# %>%
#   mutate(n9 = quantile(nonval,0.9),n05 = quantile(nonval,0.1))%>%
#   mutate(h95 = quantile(hval,0.9),h05 = quantile(hval,0.1))%>%
#   filter(nonval <n9, nonval>n05,hval<h95,hval>h05)%>%
#   mutate(diff=(hval-nonval)/nonval)

muh <- mean(holnormdemand[[i]][holnormdemand[[i]]<quantile(holnormdemand[[i]],ub)&
                               holnormdemand[[i]]>quantile(holnormdemand[[i]],lb)])
munh <- mean(nonholnormdemand[[i]][nonholnormdemand[[i]]<quantile(nonholnormdemand[[i]],ub)&
                                     nonholnormdemand[[i]]>quantile(nonholnormdemand[[i]],lb)])

sigh <- sd(holnormdemand[[i]][holnormdemand[[i]]<quantile(holnormdemand[[i]],ub)&
                                 holnormdemand[[i]]>quantile(holnormdemand[[i]],lb)])
signh <- sd(nonholnormdemand[[i]][nonholnormdemand[[i]]<quantile(nonholnormdemand[[i]],ub)&
                                     nonholnormdemand[[i]]>quantile(nonholnormdemand[[i]],lb)])

# muh <- mean((tt$hval))
# munh <- mean((tt$nonval))
# sigh <- sd((tt$hval))
# signh<- sd(tt$nonval)
covh <- cov(tt$nonval,tt$hval)

sqrt(muh^2/munh^2*(sigh^2/muh^2+signh^2/munh^2-2*(covh)/(muh*munh)))
muh/munh + signh*muh/munh^3-covh/munh^2-1

ggplot()+
  geom_histogram(aes(x=nonholnormdemand[[i]]),fill="blue",alpha=0.4)+
  geom_histogram(aes(x=holnormdemand[[i]]),fill="green",alpha=0.4)



summary(lm(data=nonhol_data%>%mutate(discprice = (1-discount)*original_price/partysize, # = price/partysize
                                  logvol = log(total_volume),
                                  perperson = original_price/partysize,
                                  competitors_ct = (competitors/sd(competitors)),
                                  logcomp = ifelse(competitors!=0,log(competitors),0),
                                  fixlength = ifelse(offering_duration==15,1,
                                                     ifelse(offering_duration==30,2,
                                                            ifelse(offering_duration==45,3,0)))),
           logvol~-1+log(offering_duration)+I(log(offering_duration)^2)+
             discprice  #+ I(discprice^2)
           # log(discprice)+I(log(discprice)^2)+
           #I(log(original_price))+I(1/log(original_price))+I(1/(log(original_price)^2))+
           #logcomp+weekends+platform:weekends+
           #platform+city
)
)



