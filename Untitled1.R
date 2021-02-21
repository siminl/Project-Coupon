View(data.frame(Spri = Spri[[i]],S = hol_data$total_volume,
                nholr = D1hvalpri[[i]]/Spri[[i]], holr = D1hval[[i]]/hol_data$total_volume, 
                d2prinh = D2nhvalpri[[i]]/hol_data$holiday.len, d2nh = D2nhval[[i]],
                d2prih = D2hvalpri[[i]], d2h = D2hval[[i]],
                thetaalpha = (mleout_h[[i]]$par[2]),
                thetad= (mleout_h[[i]]$par[4]),thetaT = mleout_h[[i]]$par[3],
                thetaalphahat = sales_estimates_h[i,4],thetaThat = 1.2,thetadhat = 1.24,
                betadnh = -exp(mleout_preh[[i]]$par[3]),betaalphanh = (mleout_h[[i]]$par[4]),  
                betadh = (mleout_h[[i]]$par[5]))#%>%
       # mutate(SpriS = Spri/S,rr = nholr/holr)%>%
       # filter(rr>0,Spri>0)%>%
       # filter(rr<quantile(rr,0.8), rr>quantile(rr,0.2),SpriS<quantile(SpriS,0.8),SpriS>quantile(SpriS,0.2))%>%
       # summarise(mean(SpriS),sd(SpriS),mean(rr),sd(rr))
     )



chs <- vector()
cnhs <- vector()
cvec <- vector()
cnhvec <- vector()
# thetaalpha <- vector()
# thetaT <- vector()
# thetad <- vector()
# betad <- vector()
# betad_nh <- vector()
# betaalpha_nh <- vector()
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
alterPvec<- vector()
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
    
  
  if( i %in% c(10)){
    
    Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                               1.7*log(hol_data$offering_duration) + 
                                               1.05*log(hol_data$prep.period))*
                     exp(sales_estimates_h[i,4]*hol_data$original_price))*hol_data$offering_duration
    
    fes_h <- as.numeric(control_FE_h(sales_estimates_h[i,],hol_data))
    
    
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
    D1hval[[i]] <- Stot*((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
    
    D1hvalpri[[i]] <- Stotpri*((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((-exp(mleout_h[[i]]$par[4]))*log(hol_data$holiday.len+1)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
     
    r[[i]] <- ((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
      (1+((mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
   
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- ((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                       -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
  
    D2nhvalpri[[i]] <- ((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes))
    
    
    D2hval[[i]] <- ((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                      abs(D2nhval[[i]]))
  
    
    D2hvalpri[[i]] <- (((mleout_h[[i]]$par[7]))+
                         (D2nhvalpri[[i]]))
    
    
    nondealdemandhol <- D1hvalpri[[i]] + pmax(D2hvalpri[[i]],0)
    nondealdemandnhol <- Stotpri - D1hvalpri[[i]] + pmax(D2nhvalpri[[i]],0)
    
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
    print(paste("dsidx = ", mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                              dsidx[[i]]>quantile(dsidx[[i]],0.1)])," ", 
                sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                dsidx[[i]]>quantile(dsidx[[i]],0.1)])))
    print(paste("dsidx2 = ", mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                dsidx2[[i]]>quantile(dsidx2[[i]],0.1)])," ", 
                sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                 dsidx2[[i]]>quantile(dsidx2[[i]],0.1)])))
    
    dsidxvec <- c(dsidxvec,mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                             dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
    dsidxsigvec <- c(dsidxsigvec,sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                                 dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
    
    dsidx2vec <- c(dsidx2vec,mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                  dsidx2[[i]]>quantile(dsidx2[[i]],0.1)]))
    dsidx2sigvec <- c(dsidx2sigvec,sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                    dsidx2[[i]]>quantile(dsidx2[[i]],0.1)]))
    #print(sd(sdop$original_price))
    #print(sd(sdop$original_price)*exp(mleout_h[[i]]$par[1]))
    Dtotalpri[[i]] <- as.numeric(Spri[[i]] + D2nhvalpri[[i]] + D2hvalpri[[i]]) 
    # 
    # dalter[[i]] <- (-(as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))) +
    #   as.numeric(hol_data$total_volume + (D2nhval[[i]])+ (D2hval[[i]])))/
    #   abs((as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))))
    
    
    dalter[[i]] <- (-(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]],0))) +
                      as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))/
      (as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]],0)))
    
    nondealprofit[[i]] <- hol_data$original_price*Spri[[i]] + 
      hol_data$original_price*((D2nhvalpri[[i]]) + (D2hvalpri[[i]])) - 
      cnh/((hol_data$offering_duration - hol_data$holiday.len))*(((D2nhvalpri[[i]])+(nondealdemandnhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_preh[i,1]*2)) -
      ch/(hol_data$holiday.len)*(((D2hvalpri[[i]]) + (nondealdemandhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_h[i,1]*2))
    
    dealprofit[[i]] <- hol_data$original_price*(hol_data$discount)*hol_data$total_volume + 
      hol_data$original_price*((D2nhval[[i]]) +(D2hval[[i]])) - 
      cnh/(hol_data$offering_duration - hol_data$holiday.len)*(((D2nhval[[i]])+dealdemandnhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_preh[i,1]*2)) -
      ch/(hol_data$holiday.len)*(((D2hval[[i]]) + dealdemandhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_h[i,1]*2))
    
    profitalter[[i]] <- (-nondealprofit[[i]]+dealprofit[[i]])/abs(nondealprofit[[i]])
    
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
    print(paste("alterD = ", median(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.8)&
                                                  dalter[[i]]>quantile(dalter[[i]],0.2)])))
    print(paste("sig alterD = ", sd(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.95)&
                                                  dalter[[i]]>quantile(dalter[[i]],0.05)])))
    print(paste("alterP = ", mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                     profitalter[[i]]>quantile(profitalter[[i]],0.1)])))
    print(paste("sig alterP = ", sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                       profitalter[[i]]>quantile(profitalter[[i]],0.1)])))
    
    
    ratiovec <- c(ratiovec, mean(r[[i]]))
    ratiosigvec <- c(ratiosigvec, sd(r[[i]]))
    
    D1hvec <- c(D1hvec,mean(D1hval[[i]]))
    D1hsigvec <- c(D1hsigvec,sd(D1hval[[i]]))
    
    D2nhvec <- c(D2nhvec,mean(D2nhval[[i]]))
    D2nhsigvec <- c(D2nhsigvec,sd(D2nhval[[i]]))
    
    D2hvec <- c(D2hvec,mean(D2hval[[i]]))
    D2hsigvec <- c(D2hsigvec,sd(D2hval[[i]]))
    
    alterDvec <- c(alterDvec,median(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.8)&
                                                dalter[[i]]>quantile(dalter[[i]],0.2)]))
    alterDsigvec <- c(alterDsigvec,sd(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.95)&
                                                    dalter[[i]]>quantile(dalter[[i]],0.05)]))
    alterPvec <- c(alterPvec,mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                               profitalter[[i]]>quantile(profitalter[[i]],0.1)]))
    alterPsigvec <- c(alterPsigvec,sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                         profitalter[[i]]>quantile(profitalter[[i]],0.1)]))
    
    
    
    
  }else if(i %in% c(11)){
    Spri[[i]]  <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                               1.2*log(hol_data$offering_duration)+
                                               1.00*log(hol_data$prep.period))*
                     exp(sales_estimates_h[i,4]*hol_data$original_price))*hol_data$offering_duration
    
    
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
    
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))
    
    D2nhvalpri[[i]] <-  exp((mleout_preh[[i]]$par[2])+
                              -exp(mleout_preh[[i]]$par[3])*log(hol_data$holiday.len+1))
    
    
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7])))
    
    D2hvalpri[[i]] <- exp(+((mleout_h[[i]]$par[7])))
    
    nondealdemandhol <- D1hvalpri[[i]] + pmax(D2hvalpri[[i]],0)
    nondealdemandnhol <- Stotpri - D1hvalpri[[i]] + pmax(D2nhvalpri[[i]]/(hol_data$holiday.len+1),0)
    
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
    print(paste("dsidx = ", mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                              dsidx[[i]]>quantile(dsidx[[i]],0.1)])," ", 
                sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                dsidx[[i]]>quantile(dsidx[[i]],0.1)])))
    print(paste("dsidx2 = ", mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                dsidx2[[i]]>quantile(dsidx2[[i]],0.1)])," ", 
                sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                 dsidx2[[i]]>quantile(dsidx2[[i]],0.1)])))
    
    dsidxvec <- c(dsidxvec,mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                             dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
    dsidxsigvec <- c(dsidxsigvec,sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                                 dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
    
    dsidx2vec <- c(dsidx2vec,mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                dsidx2[[i]]>quantile(dsidx2[[i]],0.1)]))
    dsidx2sigvec <- c(dsidx2sigvec,sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                    dsidx2[[i]]>quantile(dsidx2[[i]],0.1)]))
  
    
    dalter[[i]] <- (-(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]]/(hol_data$holiday.len+1),0)+ pmax(D2hvalpri[[i]],0))) +
                      as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))/
      ((as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]]/(hol_data$holiday.len+1),0)+ pmax(D2hvalpri[[i]],0))))
    
    nondealprofit[[i]] <- hol_data$original_price*Spri[[i]] + 
      hol_data$original_price*((D2nhvalpri[[i]])/(hol_data$holiday.len+1) + (D2hvalpri[[i]])) - 
      cnh/((hol_data$offering_duration - hol_data$holiday.len))*(((D2nhvalpri[[i]]/(hol_data$holiday.len+1))+(nondealdemandnhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_preh[i,1]*2)) -
      ch/(hol_data$holiday.len)*(((D2hvalpri[[i]]) + (nondealdemandhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_h[i,1]*2))
    
    dealprofit[[i]] <- hol_data$original_price*(hol_data$discount)*hol_data$total_volume + 
      hol_data$original_price*((D2nhval[[i]]) +(D2hval[[i]])) - 
      cnh/(hol_data$offering_duration - hol_data$holiday.len)*(((D2nhval[[i]])+dealdemandnhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_preh[i,1]*2)) -
      ch/(hol_data$holiday.len)*(((D2hval[[i]]) + dealdemandhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_h[i,1]*2))
    
    profitalter[[i]] <- (-nondealprofit[[i]]+dealprofit[[i]])/abs(nondealprofit[[i]])
    
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
    print(paste("alterP = ", mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                     profitalter[[i]]>quantile(profitalter[[i]],0.1)])))
    print(paste("sig alterP = ", sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                       profitalter[[i]]>quantile(profitalter[[i]],0.1)])))
    
    
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
    alterPvec <- c(alterPvec,mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                               profitalter[[i]]>quantile(profitalter[[i]],0.1)]))
    alterPsigvec <- c(alterPsigvec,sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                         profitalter[[i]]>quantile(profitalter[[i]],0.1)]))
    
    
    
    
  }else if(i %in% c(5)){
    
    Spri[[i]]  <- (hol_data$total_volume/exp(1.24*log(hol_data$offering_duration)))*hol_data$offering_duration
    
    
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
    
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    
    D2nhvalpri[[i]] <- exp(+(mleout_preh[[i]]$par[2]))*exp(as.numeric(fes))
    
    D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7])))
    
    D2hvalpri[[i]] <- exp(+((mleout_h[[i]]$par[7])))
  
  
    
    nondealdemandhol <- D1hvalpri[[i]] + pmax(D2hvalpri[[i]],0)
    nondealdemandnhol <- Stotpri - D1hvalpri[[i]] + pmax(D2nhvalpri[[i]],0)
    
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
    print(paste("dsidx = ", mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                              dsidx[[i]]>quantile(dsidx[[i]],0.1)])," ", 
                sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                dsidx[[i]]>quantile(dsidx[[i]],0.1)])))
    print(paste("dsidx2 = ", mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                dsidx2[[i]]>quantile(dsidx2[[i]],0.1)])," ", 
                sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                 dsidx2[[i]]>quantile(dsidx2[[i]],0.1)])))
    
    dsidxvec <- c(dsidxvec,mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                             dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
    dsidxsigvec <- c(dsidxsigvec,sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                                 dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
    
    dsidx2vec <- c(dsidx2vec,mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                dsidx2[[i]]>quantile(dsidx2[[i]],0.1)]))
    dsidx2sigvec <- c(dsidx2sigvec,sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                    dsidx2[[i]]>quantile(dsidx2[[i]],0.1)]))
    #print(sd(sdop$original_price))
    #print(sd(sdop$original_price)*exp(mleout_h[[i]]$par[1]))
    Dtotalpri[[i]] <- as.numeric(Spri[[i]] + D2nhvalpri[[i]] + D2hvalpri[[i]]) 
    # 
    # dalter[[i]] <- (-(as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))) +
    #   as.numeric(hol_data$total_volume + (D2nhval[[i]])+ (D2hval[[i]])))/
    #   abs((as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))))
    
    
    dalter[[i]] <- (-(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]],0))) +
                      as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))/
      ((as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]],0))))
    
    nondealprofit[[i]] <- hol_data$original_price*Spri[[i]] + 
      hol_data$original_price*((D2nhvalpri[[i]])/(hol_data$holiday.len+1) + (D2hvalpri[[i]])) - 
      cnh/((hol_data$offering_duration - hol_data$holiday.len))*(((D2nhvalpri[[i]])+(nondealdemandnhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_preh[i,1]*2)) -
      ch/(hol_data$holiday.len)*(((D2hvalpri[[i]]) + (nondealdemandhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_h[i,1]*2))
    
    dealprofit[[i]] <- hol_data$original_price*(hol_data$discount)*hol_data$total_volume + 
      hol_data$original_price*((D2nhval[[i]]) +(D2hval[[i]])) - 
      cnh/(hol_data$offering_duration - hol_data$holiday.len)*(((D2nhval[[i]])+dealdemandnhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_preh[i,1]*2)) -
      ch/(hol_data$holiday.len)*(((D2hval[[i]]) + dealdemandhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_h[i,1]*2))
    
    profitalter[[i]] <- (-nondealprofit[[i]]+dealprofit[[i]])/abs(nondealprofit[[i]])
    
    
    nondealprofit[[i]] <- hol_data$original_price*Spri[[i]]*sd(sdop$original_price) + 
      hol_data$original_price*sd(sdop$original_price)*((D2nhvalpri[[i]]) + (D2hvalpri[[i]]/(hol_data$holiday.len+1))) - 
      cnh*sd(sdopph$original_price)/((hol_data$offering_duration - hol_data$holiday.len))*(((D2nhvalpri[[i]])+(nondealdemandnhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_preh[i,1]*2)) -
      ch*sd(sdop$original_price)/(hol_data$holiday.len)*(((D2hvalpri[[i]]/(hol_data$holiday.len+1)) + (nondealdemandhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_h[i,1]*2))
    
    dealprofit[[i]] <- hol_data$original_price*sd(sdop$original_price)*(hol_data$discount)*hol_data$total_volume + 
      hol_data$original_price*sd(sdop$original_price)*((D2nhval[[i]]) +(D2hval[[i]])) - 
      cnh*sd(sdopph$original_price)/(hol_data$offering_duration - hol_data$holiday.len)*(((D2nhval[[i]])+dealdemandnhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_preh[i,1]*2)) -
      ch*sd(sdop$original_price)/(hol_data$holiday.len)*(((D2hval[[i]]) + dealdemandhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_h[i,1]*2))
    
    profitalter[[i]] <- (-nondealprofit[[i]]+dealprofit[[i]])/abs(nondealprofit[[i]])
    
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
    print(paste("alterD = ", median(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.8)&
                                                dalter[[i]]>quantile(dalter[[i]],0.2)])))
    print(paste("sig alterD = ", sd(dalter[[i]][dalter[[i]]<quantile(dalter[[i]],0.8)&
                                                  dalter[[i]]>quantile(dalter[[i]],0.2)])))
    print(paste("alterP = ", mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                     profitalter[[i]]>quantile(profitalter[[i]],0.1)])))
    print(paste("sig alterP = ", sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                       profitalter[[i]]>quantile(profitalter[[i]],0.1)])))
    
    
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
    alterPvec <- c(alterPvec,mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                               profitalter[[i]]>quantile(profitalter[[i]],0.1)]))
    alterPsigvec <- c(alterPsigvec,sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.9)&
                                                         profitalter[[i]]>quantile(profitalter[[i]],0.1)]))
    
    
    
    
  }else if (i %in% c(1)){
    
    Spri[[i]]  <-  (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                                1.2*log(hol_data$offering_duration)+
                                                1.24*log(hol_data$prep.period))*
                      exp(sales_estimates_h[i,4]*hol_data$original_price))*hol_data$offering_duration
    
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
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- ((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                       -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
  
      D2nhvalpri[[i]] <- ((mleout_preh[[i]]$par[2])+
                            -exp(mleout_preh[[i]]$par[3])*log(hol_data$holiday.len+1))*exp(as.numeric(fes))
      
      
      D2hval[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                           (D2nhval[[i]]))
      
      D2hvalpri[[i]] <- exp((mleout_h[[i]]$par[5])*log(hol_data$holiday.len+1)+((mleout_h[[i]]$par[7]))+
                              (D2nhvalpri[[i]]))
     
      
      nondealdemandhol <- D1hvalpri[[i]] + pmax(D2hvalpri[[i]]/hol_data$holiday.len,0)
      nondealdemandnhol <- Stotpri - D1hvalpri[[i]] + pmax(D2nhvalpri[[i]]/hol_data$holiday.len,0)
      
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
      print(paste("dsidx = ", mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                                dsidx[[i]]>quantile(dsidx[[i]],0.1)])," ", 
                  sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                  dsidx[[i]]>quantile(dsidx[[i]],0.1)])))
      print(paste("dsidx2 = ", mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                  dsidx2[[i]]>quantile(dsidx2[[i]],0.1)])," ", 
                  sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                   dsidx2[[i]]>quantile(dsidx2[[i]],0.1)])))
      
      dsidxvec <- c(dsidxvec,mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                               dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
      dsidxsigvec <- c(dsidxsigvec,sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                                   dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
      
      dsidx2vec <- c(dsidx2vec,mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                  dsidx2[[i]]>quantile(dsidx2[[i]],0.1)]))
      dsidx2sigvec <- c(dsidx2sigvec,sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.9)&
                                                      dsidx2[[i]]>quantile(dsidx2[[i]],0.1)]))
      #print(sd(sdop$original_price))
      #print(sd(sdop$original_price)*exp(mleout_h[[i]]$par[1]))
      Dtotalpri[[i]] <- as.numeric(Spri[[i]] + D2nhvalpri[[i]] + D2hvalpri[[i]]) 
      # 
      # dalter[[i]] <- (-(as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))) +
      #   as.numeric(hol_data$total_volume + (D2nhval[[i]])+ (D2hval[[i]])))/
      #   abs((as.numeric(Spri[[i]] + (D2nhvalpri[[i]])+ (D2hvalpri[[i]]))))
      
      
      dalter[[i]] <- (-(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]]/hol_data$holiday.len,0)+ pmax(D2hvalpri[[i]]/hol_data$holiday.len,0))) +
                        as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))/
        ((as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]]/hol_data$holiday.len,0)+ pmax(D2hvalpri[[i]]/hol_data$holiday.len,0))))
      
      nondealprofit[[i]] <- hol_data$original_price*Spri[[i]] + 
        hol_data$original_price*((D2nhvalpri[[i]]/hol_data$holiday.len) + (D2hvalpri[[i]]/hol_data$holiday.len)) - 
        cnh/((hol_data$offering_duration - hol_data$holiday.len))*(((D2nhvalpri[[i]]/hol_data$holiday.len)+(nondealdemandnhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_preh[i,1]*2)) -
        ch/(hol_data$holiday.len)*(((D2hvalpri[[i]]/hol_data$holiday.len) + (nondealdemandhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_h[i,1]*2))
      
      dealprofit[[i]] <- hol_data$original_price*(hol_data$discount)*hol_data$total_volume + 
        hol_data$original_price*((D2nhval[[i]]) +(D2hval[[i]])) - 
        cnh/(hol_data$offering_duration - hol_data$holiday.len)*(((D2nhval[[i]])+dealdemandnhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_preh[i,1]*2)) -
        ch/(hol_data$holiday.len)*(((D2hval[[i]]) + dealdemandhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_h[i,1]*2))
      
      profitalter[[i]] <- (-nondealprofit[[i]]+dealprofit[[i]])/abs(nondealprofit[[i]])
      
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
      alterPvec <- c(alterPvec,mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.8)&
                                                 profitalter[[i]]>quantile(profitalter[[i]],0.2)]))
      alterPsigvec <- c(alterPsigvec,sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.8)&
                                                           profitalter[[i]]>quantile(profitalter[[i]],0.2)]))
      
      
      
      
      
  }else{
    
    Spri[[i]] <- (hol_data$total_volume/exp(sales_estimates_h[i,4]*(1-hol_data$discount)*hol_data$original_price +
                                              0.9*log(hol_data$offering_duration) + 
                                              1.23*log(hol_data$prep.period)
    )*
      exp(sales_estimates_h[i,4]*hol_data$original_price))*hol_data$offering_duration
    
    
    
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
    
    fes <- control_FE(sales_estimates[i,],hol_data)
    
    D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                          -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    
    # D2nhvalpri[[i]] <- exp((mleout_preh[[i]]$par[2])-exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
    
    D2nhvalpri[[i]] <- exp((mleout_preh[[i]]$par[2]))*exp(as.numeric(fes))
    
    
    D2hval[[i]] <- exp(-exp(mleout_h[[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h[[i]]$par[7]))+
                         (log(D2nhval[[i]])))
    
    D2hvalpri[[i]] <- exp(-exp(mleout_h[[i]]$par[5])*log(hol_data$holiday.len+1)+((mleout_h[[i]]$par[7]))+
                            (log(D2nhvalpri[[i]])))
    
    
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
    print(paste("dsidx = ", mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                              dsidx[[i]]>quantile(dsidx[[i]],0.1)])," ", 
                sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                dsidx[[i]]>quantile(dsidx[[i]],0.1)])))
    print(paste("dsidx2 = ", mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.8)&
                                                dsidx2[[i]]>quantile(dsidx2[[i]],0.2)])," ", 
                sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.8)&
                                 dsidx2[[i]]>quantile(dsidx2[[i]],0.2)])))
    #print(sd(sdop$original_price))
    
    dsidxvec <- c(dsidxvec,mean(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                             dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
    dsidxsigvec <- c(dsidxsigvec,sd(dsidx[[i]][dsidx[[i]]<quantile(dsidx[[i]],0.9)&
                                                 dsidx[[i]]>quantile(dsidx[[i]],0.1)]))
    
    dsidx2vec <- c(dsidx2vec,mean(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.8)&
                                                dsidx2[[i]]>quantile(dsidx2[[i]],0.2)]))
    dsidx2sigvec <- c(dsidx2sigvec,sd(dsidx2[[i]][dsidx2[[i]]<quantile(dsidx2[[i]],0.8)&
                                                    dsidx2[[i]]>quantile(dsidx2[[i]],0.2)]))
    
    
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
    alterPvec <- c(alterPvec,mean(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.8)&
                                               profitalter[[i]]>quantile(profitalter[[i]],0.2)]))
    alterPsigvec <- c(alterPsigvec,sd(profitalter[[i]][profitalter[[i]]<quantile(profitalter[[i]],0.8)&
                                                         profitalter[[i]]>quantile(profitalter[[i]],0.2)]))
    
    
    
    
  }
}




