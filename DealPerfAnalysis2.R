
summary(lm(data=hol_data%>%mutate(discprice = (1-discount)*original_price, # = price/partysize
                                  logvol = log(total_volume),
                                  perperson = original_price/partysize,
                                  competitors_ct = (competitors/sd(competitors)),
                                  logcomp = ifelse(competitors!=0,log(competitors),0),
                                  fixlength = ifelse(offering_duration==15,1,
                                                     ifelse(offering_duration==30,2,
                                                            ifelse(offering_duration==45,3,0)))),
           logvol~log(offering_duration)+#I(log(offering_duration)^2)+
             I(1-discount)+#I((1-discount)^2)+
             I(log(prep.period+1)) + #I(log(prep.period)^2)+
           # log(discprice)+I(log(discprice)^2)+
           ((perperson))+#I(1/(perperson))+I(1/((perperson)^2))+
           logcomp+weekends+
            platform:weekends+
            platform+
            city
) ->lmt
)


industries_to_check <- c(2,5,11,10,1)
# industries_to_check <- c(1)
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
cvec <- vector()
cnhvec <- vector()
thetaalpha <- vector()
thetaT <- vector()
thetad <- vector()
betad <- vector()
betad_nh <- vector()
betaalpha_nh <- vector()
mucpvec <- vector()
sigcpvec <- vector()
dsidxvec<- vector()
dsidxsigvec<- vector()
dsidx2vec<- vector()
dsidx2sigvec<- vector()
ratiovec<- vector()
ratiosigvec<- vector()
D1hvec<- vector()
D1hsigvec<- vector()
D2hvec<- vector()
D2hsigvec<- vector()
D2nhvec<- vector()
D2nhsigvec<- vector()
alterDvec<- vector()
alterDsigvec<- vector()
alterP<- vector()
alterPsigvec<- vector()

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
  ch <- (exp((mleout_h[[i]]$par[1])))
  cnh <- (exp((mleout_preh[[i]]$par[1])))
  cvec <- c(cvec,ch)
  cnhvec <- c(cnhvec,cnh)
  #print(paste(mleout_h[[i]]$par[1],mleout_preh[[i]]$par[1]))
  chs <- c(chs, (exp((mleout_h[[i]]$par[1])))*sd(sdop$original_price))
  cnhs <- c(cnhs,(exp((mleout_preh[[i]]$par[1])))*sd(sdopph$original_price))
  print(paste("mch = ",(exp((mleout_h[[i]]$par[1])))*sd(sdop$original_price)))
  print(paste("mcph = ",(exp((mleout_preh[[i]]$par[1])))*sd(sdopph$original_price)))
  
  muc <- (exp((mleout_h[[i]]$par[1])))*sd(sdop$original_price)
  mucp <- mean(muc/sdop$original_price[sdop$original_price<quantile(sdop$original_price,0.9)&
                                         sdop$original_price>quantile(sdop$original_price,0.1)])
  
  sigcp <- sd(muc/sdop$original_price[sdop$original_price<quantile(sdop$original_price,0.9)&
                                        sdop$original_price>quantile(sdop$original_price,0.1)])
  
  mucpvec <- c(mucpvec,mucp)
  sigcpvec <- c(sigcpvec,sigcp)
  
  
  mucnh <- (exp((mleout_preh[[i]]$par[1])))*sd(sdopph$original_price)
  mucpnh <- mean(mucnh/sdopph$original_price[sdopph$original_price<quantile(sdopph$original_price,0.9)&
                                           sdopph$original_price>quantile(sdopph$original_price,0.1)])
  
  sigcpnh <- sd(mucnh/sdopph$original_price[sdopph$original_price<quantile(sdopph$original_price,0.9)&
                                          sdopph$original_price>quantile(sdopph$original_price,0.1)])
  

  
  print(paste("cp ratio mean hol",mucp, "; sigcp", sigcp))
  print(paste("cp ratio mean nhol",mucpnh, "; sigcpnh", sigcpnh))
  print(paste("mcp ratio: ", muc/mucnh))
  
  if(i %in% c(2)){
    Spri[[i]] <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                              0.9*log(hol_data$offering_duration) + 
                                              1.23*log(hol_data$prep.period)
                                            )*
                    exp(sales_estimates_h[i,4]*hol_data$original_price))*hol_data$offering_duration
    
   
    
    
  }else if(i %in% c(5)){
    
    # Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
    #                                            1.24*log(hol_data$offering_duration) #+
    #                                            #1.07*log(hol_data$prep.period)
    # )*
    # exp(sales_estimates_h[i,4]*hol_data$original_price+1.24*log(2)))*hol_data$offering_duration
    
    Spri[[i]]  <- (hol_data$total_volume/exp(1.24*log(hol_data$offering_duration)))*hol_data$offering_duration
    
    
  
    # 
    # Spri[[i]] <- (hol_data$total_volume/exp(-0.67*(1-hol_data$discount) +
    #                                           #-0.63*log(hol_data$offering_duration) #+ 
    #                                          0.08*log(hol_data$prep.period+1)
    # )*
    #   exp(-0.67))
    
    
    # D1S <- (deal_data$total_volume)
  }else if(i %in% c(10)){
    Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                               1.7*log(hol_data$offering_duration) + 
                                               1.05*log(hol_data$prep.period))*
                     exp(sales_estimates_h[i,4]*hol_data$original_price))*hol_data$offering_duration
    
    # Spri[[i]] <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
    #                                           sales_estimates_h[i,2]*log(hol_data$offering_duration) + 
    #                                           sales_estimates_h[i,6]*log(hol_data$prep.period))*
    #                 exp(sales_estimates_h[i,4]*hol_data$original_price))
    
    
    # Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]/10*(1-hol_data$discount)*hol_data$original_price +
    #                                            1.7/10*log(hol_data$offering_duration) + 
    #                                            1.05/10*log(hol_data$prep.period))*
    #                  exp(sales_estimates_h[i,4]/10*hol_data$original_price+
    #                        1.7/10*log(hol_data$holiday.len+1) ))
    # 
    
  }else if(i %in% c(1)){
    
    Spri[[i]]  <-  (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                                1.2*log(hol_data$offering_duration)+
                                                1.24*log(hol_data$prep.period)
                                              )*
                      exp(sales_estimates_h[i,4]*hol_data$original_price))*hol_data$offering_duration
    
    # Spri[[i]] <- (hol_data$total_volume/exp(-1.5*(1-hol_data$discount) +
    #                                           #0.12*log(hol_data$offering_duration) + 
    #                                           0.24*log(hol_data$prep.period))*
    #                 exp(-1.5))
  }else{
    Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                               1.2*log(hol_data$offering_duration)+
                                               1.00*log(hol_data$prep.period))*
                     exp(sales_estimates_h[i,4]*hol_data$original_price))*hol_data$offering_duration
    
    # Spri[[i]] <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
    #                                           sales_estimates_h[i,2]*log(hol_data$offering_duration) + 
    #                                           sales_estimates_h[i,6]*log(hol_data$prep.period))*
    #                 exp(sales_estimates_h[i,4]*hol_data$original_price))
    
  }
  
  if(i %in% c(1)){
    
    
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
    # D1hval[[i]] <- hol_data$total_volume*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
    #   (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    # 
    # D1hvalpri[[i]] <- hol_data$total_volume*(mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
    #   (1+(mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    # 
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- Stotpri*(((mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(((mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))    
    
    thetaalpha <- c(thetaalpha,(mleout_h[[i]]$par[2]))
    thetaT<- c(thetaT,((mleout_h[[i]]$par[3])))
    thetad<- c(thetad,((mleout_h[[i]]$par[4])))
    
    
    
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
    
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*(-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    # D1hvalpri[[i]] <- Stotpri*(((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
    #   (1+(((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- Stotpri*((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
      
    D1hvalprid1[[i]] <- Stot*(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6] + exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    
    r[[i]] <- (-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+(-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    
    thetaalpha <- c(thetaalpha,-exp(mleout_h[[i]]$par[2]))
    thetaT<- c(thetaT,((mleout_h[[i]]$par[3])))
    thetad<- c(thetad,(-exp(mleout_h[[i]]$par[4])))
    
  }else if (i %in% c(5)){
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    D1hvalpri[[i]] <- Stotpri*((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6])/
      (1+((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6]))
    
    D1hvalprid1[[i]] <- Stot*(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6])/
      (1+(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6]))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    thetaalpha <- c(thetaalpha,(mleout_h[[i]]$par[2]))
    thetaT<- c(thetaT,((mleout_h[[i]]$par[3])))
    thetad<- c(thetad,(-exp(mleout_h[[i]]$par[4])))
    
  }else if(i %in% c(11)){
    
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    D1hvalpri[[i]] <- Stotpri*((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6])/
      (1+((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6]))
    
    D1hvalprid1[[i]] <- Stot*(+((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6])/
      (1+(+((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6]))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
    
    thetaalpha <- c(thetaalpha,(mleout_h[[i]]$par[2]))
    thetaT<- c(thetaT,((mleout_h[[i]]$par[3])))
    thetad<- c(thetad,(-exp(mleout_h[[i]]$par[4])))
    
  }else if(i %in% c(10)){
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    
    
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- Stotpri*((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalprid1[[i]] <- Stot*(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+(((mleout_h[[i]]$par[3]))*log(2)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    thetaalpha <- c(thetaalpha,(mleout_h[[i]]$par[2]))
    thetaT<- c(thetaT,((mleout_h[[i]]$par[3])))
    thetad<- c(thetad,(-exp(mleout_h[[i]]$par[4])))
    
  }
  
  
  if(i %in% c(1,10)){
    
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- ((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                       -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    # 
    if(i %in% c(1)){
      D2nhvalpri[[i]] <- ((mleout_preh[[i]]$par[2])+
                            -exp(mleout_preh[[i]]$par[3])*log(hol_data$holiday.len+1))*exp(as.numeric(fes))
    }else{
      D2nhvalpri[[i]] <- ((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes))
    }
  
    # 
    # 
    
 
    
    fes_nh <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhvalprid2[[i]] <- ((mleout_preh[[i]]$par[2])+
                            -exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes_nh))
    
    betaalpha_nh <- c(betaalpha_nh,((mleout_preh[[i]]$par[4])))
    betad_nh <- c(betad_nh,-exp(mleout_preh[[i]]$par[3]))
    
    
  }else if(i %in% c(11)){
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))
    
    D2nhvalpri[[i]] <-  exp((mleout_preh[[i]]$par[2])+
                              -exp(mleout_preh[[i]]$par[3])*log(hol_data$holiday.len+1))
    
    D2nhvalprid1[[i]] <-  exp((mleout_preh[[i]]$par[2])+
                                -exp(mleout_preh[[i]]$par[3])*log(2))
    
    fes_nh <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhvalprid2[[i]] <-  exp((mleout_preh[[i]]$par[2])+
                                -exp(mleout_preh[[i]]$par[3])*log(2))
    
    betaalpha_nh <- c(betaalpha_nh,((mleout_preh[[i]]$par[4])))
    betad_nh <- c(betad_nh,-exp(mleout_preh[[i]]$par[3]))
    
  }else if(i %in% c(5)){
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    
    D2nhvalpri[[i]] <- exp(-exp(mleout_preh[[i]]$par[3])*log(hol_data$holiday.len+1)+(mleout_preh[[i]]$par[2]))*exp(as.numeric(fes))
    
    D2nhvalprid1[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes))
    
    fes_nh <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhvalprid2[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes_nh))
    
    
    betaalpha_nh <- c(betaalpha_nh,((mleout_preh[[i]]$par[4])))
    betad_nh <- c(betad_nh,-exp(mleout_preh[[i]]$par[3]))
    
  }else{
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    
    # D2nhvalpri[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    D2nhvalpri[[i]] <- exp((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes))
    
    D2nhvalprid1[[i]] <- exp((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes))
    
    fes_nh <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhvalprid2[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(2))*exp(as.numeric(fes_nh))
    
    betaalpha_nh <- c(betaalpha_nh,((mleout_preh[[i]]$par[4])))
    betad_nh <- c(betad_nh,-exp(mleout_preh[[i]]$par[3]))
  }
  
  
  if(i %in% c(1)){
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                         (D2nhval[[i]]))
    
    D2hvalpri[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$holiday.len+1)+((mleout_h[[i]]$par[7]))+
                            (D2nhvalpri[[i]]))
    
    D2hvalprid1[[i]] <- exp((mleout_h[[i]]$par[5])*log(2)+((mleout_h[[i]]$par[7]))+
                              D2nhvalprid1[[i]])
    
    betad <- c(betad,(mleout_h[[i]]$par[5]))
    
  }else if(i %in% c(10)){
    D2hval[[i]] <- ((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                      abs(D2nhval[[i]]))
    
    D2hvalpri[[i]] <- ((mleout_h[[i]]$par[5])*log(hol_data$holiday.len+1)+((mleout_h[[i]]$par[7]))+
                         abs(D2nhvalpri[[i]]))
    
    
    D2hvalpri[[i]] <- (((mleout_h[[i]]$par[7]))+
                           (D2nhvalpri[[i]]))
    
    betad <- c(betad,(mleout_h[[i]]$par[5]))
    
  }else if(i %in% c(2)){
    
    D2hval[[i]] <- exp(-exp(mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                         (log(D2nhval[[i]])))
    
    D2hvalpri[[i]] <- exp(((mleout_h[[i]]$par[7]))+
                            (log(D2nhvalpri[[i]])))
    
    
    D2hvalpri[[i]] <- exp(-exp(mleout_h[[i]]$par[5])*log(hol_data$holiday.len+1)+((mleout_h[[i]]$par[7]))+
                            (log(D2nhvalpri[[i]])))
    
    
    D2hvalprid1[[i]] <- exp(-exp(mleout_h[[i]]$par[5])*log(2)+((mleout_h[[i]]$par[7]))+
                              (log(D2nhvalprid1[[i]])))
    
    betad <- c(betad,-exp(mleout_h[[i]]$par[5]))
    
  }else if(i %in% c(5)){
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7])))
    
    D2hvalpri[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$holiday.len+1)+((mleout_h[[i]]$par[7])))
    
    D2hvalprid1[[i]] <- exp((mleout_h[[i]]$par[5])*log(2)+((mleout_h[[i]]$par[7])))
    
    betad <- c(betad,(mleout_h[[i]]$par[5]))
    
  }else{
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7])))
    
    D2hvalpri[[i]] <- exp(+((mleout_h[[i]]$par[7])))
    
    D2hvalprid1[[i]] <- exp((mleout_h[[i]]$par[5])*log(2)+((mleout_h[[i]]$par[7])))
    
    betad <- c(betad,(mleout_h[[i]]$par[5]))
    
  }
  
  
  nondealdemandhol <- D1hvalpri[[i]] + pmax(D2hvalpri[[i]]/hol_data$holiday.len ,0)
  nondealdemandnhol <- Stotpri - D1hvalpri[[i]]+ pmax(D2nhvalpri[[i]],0)
  
  dealdemandhol <- D1hval[[i]] + pmax(D2hval[[i]],0)
  dealdemandnhol <- Stot - D1hval[[i]] + pmax(D2nhval[[i]],0)
  
  dsidx2[[i]] <- ((-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
    ((dealdemandhol+dealdemandnhol))-
    (-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
    ((nondealdemandhol+nondealdemandnhol)))/
    abs((-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
    ((nondealdemandhol+nondealdemandnhol)))
  
  
  nondealdemandhol <- D1hvalpri[[i]] 
  nondealdemandnhol <- Spri[[i]] - D1hvalpri[[i]] 
  
  dealdemandhol <- D1hval[[i]] 
  dealdemandnhol <- hol_data$total_volume - D1hval[[i]] 
  
  # dsidx[[i]] <- (-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)-
  #   (-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len) 
  
  dsidx[[i]] <- ((-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/hol_data$total_volume-
    ((-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/Spri[[i]]))/
    ((-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/Spri[[i]]) 
  
  
  avgdailydemand <- (hol_data$total_volume + D2hval[[i]] )/hol_data$holiday.len
  # print(mean(dsidx[[i]]/avgdailydemand,na.rm = TRUE))
  
  
  dsidxvec <- c(dsidxvec,mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                           dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
  dsidxsigvec <- c(dsidxsigvec,sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                           dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
  
  dsidx2vec <- c(dsidx2vec,median(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.8)&
                                              dsidx2[[i]]>quantile(dsidx2[[i]],0.2)]))
  dsidx2sigvec <- c(dsidx2sigvec,sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.8)&
                                              dsidx2[[i]]>quantile(dsidx2[[i]],0.2)]))
 
  
  print(paste("dsidx = ", mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                            dsidx[[i]]>quantile(dsidx[[i]],0.1)])," ", 
              sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                               dsidx[[i]]>quantile(dsidx[[i]],0.1)])))
  print(paste("dsidx2 = ", mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.8)&
                                              dsidx2[[i]]>quantile(dsidx2[[i]],0.2)])," ", 
              sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.8)&
                              dsidx2[[i]]>quantile(dsidx2[[i]],0.2)])))
  #print(sd(sdop$original_price))
  #print(sd(sdop$original_price)*exp(mleout_h[[i]]$par[1]))
  Dtotalpri[[i]] <- as.numeric(Spri[[i]] + D2nhvalpri[[i]] + D2hvalpri[[i]]) 
  # 
  # dalter[[i]] <- (-(as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))) +
  #   as.numeric(hol_data$total_volume + (D2nhval[[i]])+ (D2hval[[i]])))/
  #   abs((as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))))
  
  
  dalter[[i]] <- (-(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]]/(hol_data$holiday.len+1),0))) +
                    as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))/
    ((as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]]/(hol_data$holiday.len+1),0))))
  
  nondealprofit[[i]] <- hol_data$original_price*Spri[[i]]*sd(sdop$original_price) + 
    hol_data$original_price*sd(sdop$original_price)*((D2nhvalpri[[i]]) + (D2hvalpri[[i]]/(hol_data$holiday.len+1))) - 
    cnh*sd(sdopph$original_price)/((hol_data$offering_duration - hol_data$holiday.len))*(((D2nhvalpri[[i]])+(nondealdemandnhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_preh[i,1]*2)) -
    ch*sd(sdop$original_price)/(hol_data$holiday.len)*(((D2hvalpri[[i]]/(hol_data$holiday.len+1)) + (nondealdemandhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_h[i,1]*2))
  
  dealprofit[[i]] <- hol_data$original_price*sd(sdop$original_price)*(hol_data$discount)*hol_data$total_volume + 
    hol_data$original_price*sd(sdop$original_price)*((D2nhval[[i]]) +(D2hval[[i]])) - 
    cnh*sd(sdopph$original_price)/(hol_data$offering_duration - hol_data$holiday.len)*(((D2nhval[[i]])+dealdemandnhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_preh[i,1]*2)) -
    ch*sd(sdop$original_price)/(hol_data$holiday.len)*(((D2hval[[i]]) + dealdemandhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_h[i,1]*2))
  
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
  print(paste("alterP = ", mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.8)&
                                                   profitalter[[i]]>quantile(profitalter[[i]],0.2)])))
  print(paste("sig alterP = ", sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.8)&
                                                     profitalter[[i]]>quantile(profitalter[[i]],0.2)])))
  
  
  
  ratiovec <- c(ratiovec, mean(r[[i]]))
  ratiosigvec <- c(ratiosigvec, sd(r[[i]]))

  D1hvec <- c(D1hvec,mean(D1hval[[i]]))
  D1hsigvec <- c(D1hsigvec,sd(D1hval[[i]]))

  D2nhvec <- c(D2nhvec,mean(D2nhval[[i]]))
  D2nhsigvec <- c(D2nhsigvec,sd(D2nhval[[i]]))
  
  D2hvec <- c(D2hvec,mean(D2hval[[i]]))
  D2hsigvec <- c(D2hsigvec,sd(D2hval[[i]]))
  
  alterDvec <- c(alterDvec,mean(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.8)&
                                              dalter[[i]]>quantile(dalter[[i]],0.2)]))
  alterDsigvec <- c(alterDsigvec,sd(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.8)&
                                              dalter[[i]]>quantile(dalter[[i]],0.2)]))
  alterP <- c(alterP,mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.8)&
                                                   profitalter[[i]]>quantile(profitalter[[i]],0.2)]))
  alterPsigvec <- c(alterPsigvec,sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.8)&
                                                         profitalter[[i]]>quantile(profitalter[[i]],0.2)]))
  
  

  
}



VarRed <- list()
Varidxvec <- vector()
for(i in industries_to_check){
  ch <- (exp(ceiling(mleout_h[[i]]$par[1])))
  cnh <- (exp(floor(mleout_preh[[i]]$par[1])))
  
  VarRed[[i]] <- (-cnh*exp(2*gamma_h[i,1]+2*gamma_h[i,2]*(hol_data$prep.period))-ch*exp(2*gamma_h[i,1]+2*gamma_h[i,2]*(hol_data$prep.period))+
                    cnh*exp(2*gamma_h[i,1]) + ch*exp(2*gamma_h[i,1]))/
    (cnh*exp(2*gamma_h[i,1]) + ch*exp(2*gamma_h[i,1]))
  
  print(paste("VarRed index = ", mean(VarRed[[i]])))
  
  Varidxvec <- c(Varidxvec,mean(VarRed[[i]]))
  
}


deltaD <- list()
holnormdemand <- list()
nonholnormdemand <- list()
D2nhndvalpri <- list()
deltaDsigvec <- vector()
deltaDvec <- vector()

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
  
 if(i %in% c(10)){
    
    fes_nhnd <- control_FE(sales_estimates[i,],nonhol_data)

    D2nhndvalpri[[i]] <- ((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes_nhnd))
    
    nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0) + 
      nonhol_data$total_volume/exp(sales_estimates[i,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
                                     sales_estimates[i,2]*log(nonhol_data$offering_duration)
      )*
      exp(sales_estimates[i,4]*nonhol_data$original_price)/nonhol_data$offering_duration
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + 
                                        pmax(D2nhvalpri[[i]]*hol_data$offering_duration,0) + 
                                        pmax(D2hvalpri[[i]]*hol_data$offering_duration,0)))/hol_data$offering_duration #Spri[[i]] +
    
    ub <- 0.9
    lb <- 0.1
    
    
 }else if(i %in% c(1)){
    fes_nhnd <- control_FE(sales_estimates[i,],nonhol_data)
    
    D2nhndvalpri[[i]] <- ((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes_nhnd))
    
    nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0) + 
      nonhol_data$total_volume/exp(sales_estimates[i,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
                                     sales_estimates[i,2]*log(nonhol_data$offering_duration)
      )*
      exp(sales_estimates[i,4]*nonhol_data$original_price)/nonhol_data$offering_duration
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + 
                                        pmax(D2nhvalpri[[i]]/(hol_data$holiday.len+1)*hol_data$offering_duration,0) + 
                                        pmax(D2hvalpri[[i]]/(hol_data$holiday.len+1)*hol_data$offering_duration,0)))/hol_data$offering_duration #Spri[[i]] +
    
    ub <- 0.9
    lb <- 0.1
    
  }else if(i %in% c(5)){
    
    fes_nhnd <- control_FE(sales_estimates[i,],nonhol_data)
    
  
    D2nhndvalpri[[i]] <- ((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes_nhnd))
 
    
    
    # nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0)+ 
    #   nonhol_data$total_volume/exp(sales_estimates[i,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
    #                                  sales_estimates[i,2]*log(nonhol_data$offering_duration)
    #                                                                           )*
    #   exp(sales_estimates[i,4]*nonhol_data$original_price)/nonhol_data$offering_duration
    
    nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0)+ 
      nonhol_data$total_volume/exp(sales_estimates[i,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
                                     sales_estimates[i,2]*log(nonhol_data$offering_duration)
      )*
      exp(sales_estimates[i,4]*nonhol_data$original_price)#/nonhol_data$offering_duration
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + 
                                        pmax(D2nhvalpri[[i]]*hol_data$offering_duration,0) + 
                                        pmax(D2hvalpri[[i]]*hol_data$offering_duration,0)))/hol_data$offering_duration #Spri[[i]] +
    
    ub <- 0.95
    lb <- 0.05
    
  }else if(i %in% c(2)){
    fes_nhnd <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhndvalpri[[i]] <- ((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes_nhnd))
    # 
    # nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0)+
    #   nonhol_data$total_volume/exp(#sales_estimates[i,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
    #                                  sales_estimates[i,2]*log(nonhol_data$offering_duration)
    #   )*
    #   exp(#sales_estimates[i,4]*nonhol_data$original_price+
    #         sales_estimates[i,2]*log(2))/nonhol_data$offering_duration
# 
#     nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0)+
#       nonhol_data$total_volume/exp(#sales_estimates[i,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
#         sales_estimates[i,2]*log(nonhol_data$offering_duration)
#       )/nonhol_data$offering_duration

    nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0)/nonhol_data$offering_duration+
      nonhol_data$total_volume/nonhol_data$offering_duration
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + 
                                        pmax(D2nhvalpri[[i]]*hol_data$offering_duration,0) + 
                                        pmax(D2hvalpri[[i]]/(hol_data$holiday.len+1)*hol_data$offering_duration,0)))/hol_data$offering_duration
    
    ub <- 0.9
    lb <- 0.1
    
  }
  else if(i %in% c(11)){
    fes_nhnd <- control_FE(sales_estimates[i,],nonhol_data)
    D2nhndvalpri[[i]] <- ((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes_nhnd))
    # 
    # nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0)+
    #   nonhol_data$total_volume/exp(#sales_estimates[i,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
    #                                  sales_estimates[i,2]*log(nonhol_data$offering_duration)
    #   )*
    #   exp(#sales_estimates[i,4]*nonhol_data$original_price+
    #         sales_estimates[i,2]*log(2))/nonhol_data$offering_duration
    # 
    #     nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0)+
    #       nonhol_data$total_volume/exp(#sales_estimates[i,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
    #         sales_estimates[i,2]*log(nonhol_data$offering_duration)
    #       )/nonhol_data$offering_duration
    
    nonholnormdemand[[i]] <- pmax(D2nhndvalpri[[i]],0)/nonhol_data$offering_duration+
      nonhol_data$total_volume/nonhol_data$offering_duration
    
    holnormdemand[[i]] <- (as.numeric(Spri[[i]] + 
                                        pmax(D2nhvalpri[[i]]*hol_data$offering_duration,0) + 
                                        pmax(D2hvalpri[[i]]/(hol_data$holiday.len+1)*hol_data$offering_duration,0)))/hol_data$offering_duration
    
    ub <- 0.8
    lb <- 0.2
    
  }
  
  
  # tt <- merge(data.frame(nonval = (nonholnormdemand[[i]]),
  #                        op = nonhol_data$original_price*sd(sdopph$original_price)),
  #             data.frame(hval = (holnormdemand[[i]]),
  #                        op = hol_data$original_price*sd(sdop$original_price)),by="op",all=TRUE)%>%
  #   mutate(n9 = quantile(nonval,ub,na.rm = TRUE),n05 = quantile(nonval,lb,na.rm = TRUE))%>%
  #   mutate(h95 = quantile(hval,ub,na.rm = TRUE),h05 = quantile(hval,lb,na.rm = TRUE))%>%
  #   #filter(op<quantile(op,0.9),op>quantile(op,0.1))%>%
  #   filter(nonval <n9, nonval>n05,hval<h95,hval>h05)%>%
  #   mutate(diff=(hval-nonval)/nonval)%>% # ()
  #   filter(is.na(diff)==0)
  # 
  # # %>%
  # #   mutate(n9 = quantile(nonval,0.9),n05 = quantile(nonval,0.1))%>%
  # #   mutate(h95 = quantile(hval,0.9),h05 = quantile(hval,0.1))%>%
  # #   filter(nonval <n9, nonval>n05,hval<h95,hval>h05)%>%
  # #   mutate(diff=(hval-nonval)/nonval)
  # 
  # muh <- mean(holnormdemand[[i]][holnormdemand[[i]]<quantile(holnormdemand[[i]],ub)&
  #                                  holnormdemand[[i]]>quantile(holnormdemand[[i]],lb)])
  # munh <- mean(nonholnormdemand[[i]][nonholnormdemand[[i]]<quantile(nonholnormdemand[[i]],ub)&
  #                                      nonholnormdemand[[i]]>quantile(nonholnormdemand[[i]],lb)])
  # 
  # sigh <- sd(holnormdemand[[i]][holnormdemand[[i]]<quantile(holnormdemand[[i]],ub)&
  #                                 holnormdemand[[i]]>quantile(holnormdemand[[i]],lb)])
  # signh <- sd(nonholnormdemand[[i]][nonholnormdemand[[i]]<quantile(nonholnormdemand[[i]],ub)&
  #                                     nonholnormdemand[[i]]>quantile(nonholnormdemand[[i]],lb)])
  # 
  # # muh <- mean((tt$hval))
  # # munh <- mean((tt$nonval))
  # # sigh <- sd((tt$hval))
  # # signh<- sd(tt$nonval)
  # covh <- cov(tt$nonval,tt$hval)
  # 
  # deltadsig <- sqrt(muh^2/munh^2*(sigh^2/muh^2+signh^2/munh^2-2*(covh)/(muh*munh)))
  # deltad <- muh/munh + signh*muh/munh^3-covh/munh^2-1
  # 
  # print(i)
  # print(paste("deltad = ", deltad))
  # print(paste("deltadsig = ", deltadsig))
  
  #holnormdemand[[i]] <- (D1hvalprid1[[i]]+D2hvalpri[[i]])
  
  
  
  # deltaD[[i]] <- (-median(nonholnormdemand[[i]][nonholnormdemand[[i]]>1]) + median(holnormdemand[[i]]))/(median(nonholnormdemand[[i]]))
  
  #deltaD[[i]] <- (-mean(nonholnormdemand[[i]]) + mean(holnormdemand[[i]]))/(mean(nonholnormdemand[[i]]))

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
  
  deltaDsigvec <- c(deltaDsigvec,sqrt(muh^2/munh^2*(sigh^2/muh^2+signh^2/munh^2-2*(covh)/(muh*munh))))
  deltaDvec <- c(deltaDvec,muh/munh + signh*muh/munh^3-covh/munh^2-1)
  
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



# the deal designs -- summaries -----

data.frame(type = c(2,5,10,11,1),
           thetaalpha = c(-2.07,4.86,4.35,5.32,2.92),
           thetaalphahat = c(-3.02,-7.87,-1.97,-1.16,-1.6),
           thetaT = c(1.27,-3.77,0.72,-0.043,-0.20),
           ThetaThat = c(1.23,1.07,1.05,1.00,1.24),
           thetad = c(-4.8,0.39,-2.5,-0.83,-0.62),
           thetadhat = c(0.9,1.24,1.7,1.20,1.2))


# check discount levels by similar capacity 


for(i in c(2,5,10,11,1)){
  
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    {.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  
  alpha_ls[[i]] <- hol_data%>%select(discount,total_volume,original_price)
}


View(merge(optimparas[[10]][,c(4,5,6,7,8)]%>%
             mutate(volumebins = total_volume - (total_volume%%40))%>%
             mutate(durationbins = offering_duration - (offering_duration%%3))%>%
             mutate(preptbins = prep.period - (prep.period%%3))%>%
             mutate(pricebins = original_price - (original_price%%50))%>%
             mutate(alphabins = discount - (discount%%0.2)),
       optimparas[[2]][,c(4,5,6,7,8)]%>%
         mutate(volumebins = total_volume - (total_volume%%40))%>%
         mutate(durationbins = offering_duration - (offering_duration%%3))%>%
         mutate(preptbins = prep.period - (prep.period%%3))%>%
         mutate(pricebins = original_price - (original_price%%50))%>%
         mutate(alphabins = discount - (discount%%0.2)),
       by=c("preptbins","volumebins"),all.x=TRUE)%>%
       group_by(preptbins,volumebins)%>%
       summarise(mean(discount.x),mean(discount.y),sd(discount.x),sd(discount.y)))


merge(merge(merge(merge(optimparas[[10]][,c(4,5,6,7,8)]%>%
             mutate(volumebins = total_volume - (total_volume%%40))%>%
             mutate(durationbins = offering_duration - (offering_duration%%3))%>%
             mutate(preptbins = prep.period - (prep.period%%3))%>%
             mutate(pricebins = original_price - (original_price%%50))%>%
             mutate(alphabins = discount - (discount%%0.2))%>%
              rename(disc10 = discount, T10 = prep.period),
           optimparas[[2]][,c(4,5,6,7,8)]%>%
             mutate(volumebins = total_volume - (total_volume%%40))%>%
             mutate(durationbins = offering_duration - (offering_duration%%3))%>%
             mutate(preptbins = prep.period - (prep.period%%3))%>%
             mutate(pricebins = original_price - (original_price%%50))%>%
             mutate(alphabins = discount - (discount%%0.2))%>%
             rename(disc2 = discount, T2 = prep.period),
           by=c("preptbins","volumebins"),all=TRUE)%>%
        #     group_by(volumebins,preptbins)%>%
        #     summarise(disc10mu=mean(disc10,na.rm = TRUE),disc2mu=mean(disc2,na.rm = TRUE),
        #               disc10sd=sd(disc10,na.rm = TRUE),disc2sd=sd(disc2,na.rm = TRUE))%>%
           select(volumebins,preptbins,disc2,disc10),
      optimparas[[11]][,c(4,5,6,7,8)]%>%
        mutate(volumebins = total_volume - (total_volume%%40))%>%
        mutate(durationbins = offering_duration - (offering_duration%%3))%>%
        mutate(preptbins = prep.period - (prep.period%%3))%>%
        mutate(pricebins = original_price - (original_price%%50))%>%
        mutate(alphabins = discount - (discount%%0.2))%>%
        rename(disc11 = discount, T11 = prep.period),
      by=c("preptbins","volumebins"),all=TRUE)%>%
      select(volumebins,preptbins, disc11,disc2,disc10),
      optimparas[[1]][,c(4,5,6,7,8)]%>%
        mutate(volumebins = total_volume - (total_volume%%40))%>%
        mutate(durationbins = offering_duration - (offering_duration%%3))%>%
        mutate(preptbins = prep.period - (prep.period%%3))%>%
        mutate(pricebins = original_price - (original_price%%50))%>%
        mutate(alphabins = discount - (discount%%0.2))%>%
        rename(disc1 = discount, T1 = prep.period),
      by=c("preptbins","volumebins"),all=TRUE)%>%
      select(volumebins,preptbins, disc11,disc2,disc10,disc1),
      optimparas[[5]][,c(4,5,6,7,8)]%>%
        mutate(volumebins = total_volume - (total_volume%%40))%>%
        mutate(durationbins = offering_duration - (offering_duration%%3))%>%
        mutate(preptbins = prep.period - (prep.period%%3))%>%
        mutate(pricebins = original_price - (original_price%%50))%>%
        mutate(alphabins = discount - (discount%%0.2))%>%
        rename(disc5 = discount, T5 = prep.period),
      by=c("preptbins","volumebins"),all=TRUE)%>%
  select(volumebins,preptbins, disc11,disc2,disc10,disc1,disc5)%>%
      group_by(preptbins,volumebins)%>%
  summarise(disc10mu=mean(disc10,na.rm = TRUE),disc2mu=mean(disc2,na.rm = TRUE),disc11mu=mean(disc11,na.rm = TRUE),
            disc1mu=mean(disc1,na.rm = TRUE),disc5mu=mean(disc5,na.rm = TRUE),
            disc10sd = sd(disc10,na.rm = TRUE),disc2sd = sd(disc2,na.rm = TRUE),disc11sd=sd(disc11,na.rm = TRUE),
            disc1sd=sd(disc1,na.rm = TRUE),disc5sd=sd(disc5,na.rm = TRUE))%>%
  filter(is.nan(disc10mu)+is.nan(disc2mu)+is.nan(disc11mu)+is.nan(disc1mu)+is.nan(disc5mu)==0) -> disc_cap_prept


colpats <- c("#66c2a5",
  "#fc8d62",
  "#8da0cb",
  "#e78ac3",
  "#a6d854")

colpats <- c("Outdoor"= "#66c2a5",
             "Casual Dine" = "#66c2a5",
             "Photography"= "#fc8d62",
             "Fine Dine" ="#66c2a5",
             "Body Care"= "#fc8d62")


ggplot(data=disc_cap_prept)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc10mu-0.5*disc10sd, ymax = disc10mu+0.5*disc10sd,color = "Outdoor",collbs = colpats[1]),width=0.1)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc2mu-0.5*disc2sd, ymax = disc2mu+0.5*disc2sd,color="Casual Dine",collbs = colpats[1]),width=0.1)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc11mu-0.5*disc11sd, ymax = disc11mu+0.5*disc11sd,color="Photography",collbs = colpats[2]),width=0.1)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc5mu-0.5*disc5sd, ymax = disc5mu+0.5*disc5sd,color="Fine Dine",collbs = colpats[2]),width=0.1)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc1mu-0.5*disc1sd, ymax = disc1mu+0.5*disc1sd,color = "Body Care",collbs = colpats[2]),width=0.1)+
  facet_grid(preptbins~.)+
  scale_color_manual(values = colpats)+
  labs(x="Capacity",y="Discount", color="Industry", facet="Release Date to Holiday") -> palpha

# can add in another metric, the % of cover or higher than the other


merge(merge(merge(merge(optimparas[[10]][,c(4,5,6,7,8)]%>%
                          mutate(volumebins = total_volume - (total_volume%%40))%>%
                          mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                          mutate(preptbins = prep.period - (prep.period%%3))%>%
                          mutate(pricebins = original_price - (original_price%%50))%>%
                          mutate(alphabins = discount - (discount%%0.2))%>%
                          rename(disc10 = discount, T10 = prep.period),
                        optimparas[[2]][,c(4,5,6,7,8)]%>%
                          mutate(volumebins = total_volume - (total_volume%%40))%>%
                          mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                          mutate(preptbins = prep.period - (prep.period%%3))%>%
                          mutate(pricebins = original_price - (original_price%%50))%>%
                          mutate(alphabins = discount - (discount%%0.2))%>%
                          rename(disc2 = discount, T2 = prep.period),
                        by=c("alphabins","volumebins"),all=TRUE)%>%
                    #     group_by(volumebins,preptbins)%>%
                    #     summarise(disc10mu=mean(disc10,na.rm = TRUE),disc2mu=mean(disc2,na.rm = TRUE),
                    #               disc10sd=sd(disc10,na.rm = TRUE),disc2sd=sd(disc2,na.rm = TRUE))%>%
                    select(volumebins,alphabins,T2,T10),
                  optimparas[[11]][,c(4,5,6,7,8)]%>%
                    mutate(volumebins = total_volume - (total_volume%%40))%>%
                    mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                    mutate(preptbins = prep.period - (prep.period%%3))%>%
                    mutate(pricebins = original_price - (original_price%%50))%>%
                    mutate(alphabins = discount - (discount%%0.2))%>%
                    rename(disc11 = discount, T11 = prep.period),
                  by=c("alphabins","volumebins"),all=TRUE)%>%
              select(volumebins,alphabins, T11,T2,T10),
            optimparas[[1]][,c(4,5,6,7,8)]%>%
              mutate(volumebins = total_volume - (total_volume%%40))%>%
              mutate(durationbins = offering_duration - (offering_duration%%3))%>%
              mutate(preptbins = prep.period - (prep.period%%3))%>%
              mutate(pricebins = original_price - (original_price%%50))%>%
              mutate(alphabins = discount - (discount%%0.2))%>%
              rename(disc1 = discount, T1 = prep.period),
            by=c("alphabins","volumebins"),all=TRUE)%>%
        select(volumebins,alphabins, T11,T2,T10,T1),
      optimparas[[5]][,c(4,5,6,7,8)]%>%
        mutate(volumebins = total_volume - (total_volume%%40))%>%
        mutate(durationbins = offering_duration - (offering_duration%%3))%>%
        mutate(preptbins = prep.period - (prep.period%%3))%>%
        mutate(pricebins = original_price - (original_price%%50))%>%
        mutate(alphabins = discount - (discount%%0.2))%>%
        rename(disc5 = discount, T5 = prep.period),
      by=c("alphabins","volumebins"),all=TRUE)%>%
  select(volumebins,alphabins, T11,T2,T10,T1,T5)%>%
  group_by(alphabins,volumebins)%>%
  summarise(T10mu=mean(T10,na.rm = TRUE),T2mu=mean(T2,na.rm = TRUE),T11mu=mean(T11,na.rm = TRUE),
            T1mu=mean(T1,na.rm = TRUE),T5mu=mean(T5,na.rm = TRUE),
            T10sd = sd(T10,na.rm = TRUE),T2sd = sd(T2,na.rm = TRUE),T11sd=sd(T11,na.rm = TRUE),
            T1sd=sd(T1,na.rm = TRUE),T5sd=sd(T5,na.rm = TRUE))%>%
  filter(is.nan(T10mu)+is.nan(T2mu)+is.nan(T11mu)+is.nan(T1mu)+is.nan(T5mu)==0) -> T_cap_prept


colpats <- c("#66c2a5",
             "#fc8d62",
             "#8da0cb",
             "#e78ac3",
             "#a6d854")

colpats <- c("Outdoor"= "#66c2a5",
             "Casual Dine" = "#fc8d62",
             "Photography"= "#66c2a5",
             "Fine Dine" = "#66c2a5",
             "Body Care"= "#fc8d62")

collabs <- c("Late Release"= "#66c2a5",
             "Early Release" = "#fc8d62")
               
ggplot(data=T_cap_prept)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T10mu-0.5*T10sd, ymax = T10mu+0.5*T10sd,color = "Outdoor",collabs="Late Release"),width=0.1)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T2mu-0.5*T2sd, ymax = T2mu+0.5*T2sd,color = "Casual Dine",collabs="Early Release"),width=0.1)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T11mu-0.5*T11sd, ymax = T11mu+0.5*T11sd,color = "Photography",collabs="Early Release"),width=0.1)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T5mu-0.5*T5sd, ymax = T5mu+0.5*T5sd,color = "Fine Dine",collabs="Late Release"),width=0.1)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T1mu-0.5*T1sd, ymax = T1mu+0.5*T1sd,color = "Body Care",collabs="Early Release"),width=0.1)+
  facet_grid(alphabins~.)+
  scale_color_manual(values = colpats)+
  labs(x="Capacity",y="Release Day to Holiday", color="Industry", facet="Discount") -> pT
# can add in another metric, the % of cover or higher than the other


