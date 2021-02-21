
## performance analysis --------
getPerformance <- function(hol_data,sdop, nonhol_data, sdopph, output_ls, type,mleout_preh,mleout_h){
  
  mchs <- vector() #
  mcnhs <- vector() #
  mucpvec <- vector() #
  sigcpvec <- vector() #
  mucpnhvec <- vector() #
  sigcpnhvec <- vector() #
  
  thetaalpha <- vector()
  thetaT <- vector()
  thetad <- vector()
  betad <- vector()
  betad_nh <- vector()
  betaalpha_nh <- vector()
  
  
  Spri <- list() #
  r <- list()
  D1hval <- list()
  D1hvalpri <- list()
  D2hval <- list()
  D2hvalpri <- list()
  D2nhval <- list()
  D2nhvalpri <- list()
  
  
  D1hvalprid1 <- list()
  D2hvalprid1 <- list()
  D2nhvalprid1 <- vector(mode = "list", length = 11)
  D2nhvalprid2 <- list()
  
  dsidx <- list()
  dsidx2 <- list()
  dalter <- list()
  Dtotalpri <- list()
  Dtotal <- list()
  nondealprofit <- list()
  dealprofit <- list()
  profitalter <- list()
  changeind <- list()
  holgap <- list()
  nonholgap <- list()
  # cvec <- vector()
  # cnhvec <- vector()
  
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
  deltaDvec <- vector()
  deltaDsigvec <- vector()
  
  for(kiter in 1:length(output_ls)){
    
    
    # marginal service costs ---- 
    mchs <- c(mchs, (exp((output_ls[[kiter]]$par[1])))*sd(sdop$original_price)*median(sdop$partysize))
    mcnhs <- c(mcnhs,(exp((mleout_preh[[type]]$par[1])))*sd(sdopph$original_price)*median(sdopph$partysize))
    
    
    # marginal service cost price ratio, holiday -------
    muc <- (exp((output_ls[[kiter]]$par[1])))*sd(sdop$original_price)
    mucp <- mean(muc/sdop$original_price[sdop$original_price<quantile(sdop$original_price,0.9)&
                                           sdop$original_price>quantile(sdop$original_price,0.1)])
    
    sigcp <- sd(muc/sdop$original_price[sdop$original_price<quantile(sdop$original_price,0.9)&
                                          sdop$original_price>quantile(sdop$original_price,0.1)])
    
    mucpvec <- c(mucpvec,mucp)
    sigcpvec <- c(sigcpvec,sigcp)
    
    # marginal cost price ratio, non-holiday ----- 
    
    mucnh <- (exp((mleout_preh[[type]]$par[1])))*sd(sdopph$original_price)
    mucpnh <- mean(mucnh/sdopph$original_price[sdopph$original_price<quantile(sdopph$original_price,0.9)&
                                                 sdopph$original_price>quantile(sdopph$original_price,0.1)])
    
    sigcpnh <- sd(mucnh/sdopph$original_price[sdopph$original_price<quantile(sdopph$original_price,0.9)&
                                                sdopph$original_price>quantile(sdopph$original_price,0.1)])
    
    mucpnhvec <- c(mucpnhvec,mucpnh)
    sigcpnhvec <- c(sigcpnhvec,sigcpnh)
    
    # the value of S_omega and Spri ------
    if(type %in% c(2)){
      Spri[[kiter]] <- (hol_data$total_volume/exp(Scoef[Scoef$type==type,2]*(1-hol_data$discount)*hol_data$original_price +
                                                    Scoef[Scoef$type==type,3]*log(hol_data$offering_duration) + 
                                                    Scoef[Scoef$type==type,4]*(hol_data$prep.period)
      )*
        exp(Scoef[Scoef$type==type,2]*hol_data$original_price+
              Scoef[Scoef$type==type,3]*log(hol_data$offering_duration) )
      )*hol_data$offering_duration
      
    }else if(type %in% c(5)){
      Spri[[kiter]] <- (hol_data$total_volume/exp(Scoef[Scoef$type==type,2]*(1-hol_data$discount)*hol_data$original_price #+
                                                   # Scoef[Scoef$type==type,3]*log(hol_data$offering_duration) +
                                                   # Scoef[Scoef$type==type,4]*(hol_data$prep.period)
      )*
        exp(Scoef[Scoef$type==type,2]*hol_data$original_price)#+
        #Scoef[Scoef$type==type,3]*log(hol_data$holiday.len) +
       # Scoef[Scoef$type==type,4]
      )*hol_data$holiday.len
      
      # Spri[[kiter]] <- hol_data$total_volume
      
    }else if(type %in% c(10)){
      Spri[[kiter]] <- (hol_data$total_volume/exp(Scoef[Scoef$type==type,2]*(1-hol_data$discount)*hol_data$original_price +
                                                    Scoef[Scoef$type==type,3]*log(hol_data$offering_duration) + 
                                                    Scoef[Scoef$type==type,4]*(hol_data$prep.period)
      )*
        exp(Scoef[Scoef$type==type,2]*hol_data$original_price)
      )*hol_data$offering_duration
      
    }else{
    
      Spri[[kiter]] <- (hol_data$total_volume/exp(Scoef[Scoef$type==type,2]*(1-hol_data$discount)*hol_data$original_price +
                                                    Scoef[Scoef$type==type,3]*log(hol_data$offering_duration) + 
                                                    Scoef[Scoef$type==type,4]*(hol_data$prep.period)
      )*
        exp(Scoef[Scoef$type==type,2]*hol_data$original_price)+
        Scoef[Scoef$type==type,3]*log(hol_data$holiday.len)
        )*hol_data$offering_duration/hol_data$holiday.len
    }
    
    
    
    
    if(type%in%c(1)){
      fes_h <- as.numeric(control_FE_h(sales_estimates_h[type,],hol_data))
      Stotpri <- Spri[[kiter]]
      Stot <- hol_data$total_volume
      
      D1hval[[kiter]] <- Stot*((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
      
      D1hvalpri[[kiter]] <- Stotpri*(((output_ls[[kiter]]$par[4]))*log(hol_data$holiday.len)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+(((output_ls[[kiter]]$par[4]))*log(hol_data$holiday.len)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
      
      
      r[[kiter]] <- ((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))    
      
      thetaalpha <- c(thetaalpha,(output_ls[[kiter]]$par[2]))
      thetaT<- c(thetaT,((output_ls[[kiter]]$par[3])))
      thetad<- c(thetad,((output_ls[[kiter]]$par[4])))
      
      fes <- control_FE(sales_estimates[type,],hol_data)
      
      D2nhval[[kiter]] <- ((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                             -exp(mleout_preh[[type]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
      
      # D2nhvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2])+
      #                           -exp(mleout_preh[[type]]$par[3])*log(hol_data$holiday.len))*exp(as.numeric(fes))
      # 
      D2nhvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2]))*exp(as.numeric(fes))*(hol_data$offering_duration-hol_data$holiday.len)/hol_data$holiday.len
      
      
      D2hval[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$offering_duration)+((output_ls[[kiter]]$par[7]))+
                               (D2nhval[[kiter]]))
      
      D2hvalpri[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$holiday.len)+((output_ls[[kiter]]$par[7]))+
                                  (D2nhvalpri[[kiter]]))
      D2hvalpri[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$holiday.len)+((output_ls[[kiter]]$par[7])))
      
      betad <- c(betad,(output_ls[[kiter]]$par[5]))
      
    }else if(type == 2){
      fes_h <- as.numeric(control_FE_h(sales_estimates_h[type,],hol_data))
      Stotpri <- Spri[[kiter]]
      Stot <- hol_data$total_volume
      
      D1hval[[kiter]] <- Stot*((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
      
      D1hvalpri[[kiter]] <- Stotpri*(((output_ls[[kiter]]$par[4]))*log(hol_data$holiday.len)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+(((output_ls[[kiter]]$par[4]))*log(hol_data$holiday.len)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
      
      
      r[[kiter]] <- ((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))    
      
      thetaalpha <- c(thetaalpha,(output_ls[[kiter]]$par[2]))
      thetaT<- c(thetaT,((output_ls[[kiter]]$par[3])))
      thetad<- c(thetad,((output_ls[[kiter]]$par[4])))
      
      fes <- control_FE(sales_estimates[type,],hol_data)
      
      D2nhval[[kiter]] <- ((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                             -exp(mleout_preh[[type]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
      
      # D2nhvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2])+
      #                           -exp(mleout_preh[[type]]$par[3])*log(hol_data$holiday.len))*exp(as.numeric(fes))*(hol_data$offering_duration-hol_data$holiday.len)/hol_data$holiday.len
      # # 
      D2nhvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2]))*exp(as.numeric(fes))*(hol_data$offering_duration-hol_data$holiday.len)/hol_data$holiday.len
      
      
      D2hval[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$offering_duration)+((output_ls[[kiter]]$par[7]))+
                               (D2nhval[[kiter]]))
      
      D2hvalpri[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$holiday.len)+((output_ls[[kiter]]$par[7]))+
                                  (D2nhvalpri[[kiter]]))
      # D2hvalpri[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$holiday.len)+((output_ls[[kiter]]$par[7])))
      
      betad <- c(betad,(output_ls[[kiter]]$par[5]))
    }else if(type == 11){
      fes_h <- as.numeric(control_FE_h(sales_estimates_h[type,],hol_data))
      Stotpri <- Spri[[kiter]]
      Stot <- hol_data$total_volume
      
      D1hval[[kiter]] <- Stot*((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
      
      D1hvalpri[[kiter]] <- Stotpri*(((output_ls[[kiter]]$par[4]))*log(hol_data$holiday.len)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+(((output_ls[[kiter]]$par[4]))*log(hol_data$holiday.len)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
      
      
      r[[kiter]] <- ((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+((output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))    
      
      thetaalpha <- c(thetaalpha,(output_ls[[kiter]]$par[2]))
      thetaT<- c(thetaT,((output_ls[[kiter]]$par[3])))
      thetad<- c(thetad,((output_ls[[kiter]]$par[4])))
      
      fes <- control_FE(sales_estimates[type,],hol_data)
      
      D2nhval[[kiter]] <- ((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                             -exp(mleout_preh[[type]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
      
      # D2nhvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2])+
      #                           -exp(mleout_preh[[type]]$par[3])*log(hol_data$holiday.len))*exp(as.numeric(fes))*
      #   (hol_data$offering_duration-hol_data$holiday.len)/hol_data$holiday.len
      # 
      D2nhvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2]))*exp(as.numeric(fes))*(hol_data$offering_duration-hol_data$holiday.len)/hol_data$holiday.len
      
      D2hval[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$offering_duration)+((output_ls[[kiter]]$par[7])))
      
      D2hvalpri[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$holiday.len)+((output_ls[[kiter]]$par[7])))
      
      betad <- c(betad,(output_ls[[kiter]]$par[5]))
    }else{
      fes_h <- as.numeric(control_FE_h(sales_estimates_h[type,],hol_data))
      
      Stotpri <- Spri[[kiter]]
      Stot <- hol_data$total_volume
      
      D1hval[[kiter]] <- Stot*((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+(-exp(output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6])/
        (1+((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+(-exp(output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]))
      
      D1hvalpri[[kiter]] <- Stotpri*((-exp(output_ls[[kiter]]$par[4]))*log(hol_data$holiday.len)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+((-exp(output_ls[[kiter]]$par[4]))*log(hol_data$holiday.len)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
      
      # D1hvalprid1[[i]] <- Stot*(((output_ls[[i]]$par[3]))*log(2)+output_ls[[i]]$par[6])/
      #   (1+(((output_ls[[i]]$par[3]))*log(2)+output_ls[[i]]$par[6]))
      
      r[[kiter]] <- ((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+(-exp(output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
        (1+((output_ls[[kiter]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((output_ls[[kiter]]$par[3]))*log(hol_data$prep.period)+(-exp(output_ls[[kiter]]$par[4]))*log(hol_data$offering_duration)+output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
      
      thetaalpha <- c(thetaalpha,(output_ls[[kiter]]$par[2]))
      thetaT<- c(thetaT,((output_ls[[kiter]]$par[3])))
      thetad<- c(thetad,(-exp(output_ls[[kiter]]$par[4])))
      
      if( type == 5){
        fes <- control_FE(sales_estimates[type,],hol_data)
        D2nhval[[kiter]] <- exp((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                                  -exp(mleout_preh[[type]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
        
        # D2nhvalpri[[kiter]] <- exp((mleout_preh[[type]]$par[2])+
        #                              -exp(mleout_preh[[type]]$par[3])*log(hol_data$holiday.len))*exp(as.numeric(fes))*
        #   (hol_data$offering_duration-hol_data$holiday.len)
        
        D2nhvalpri[[kiter]] <- exp((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*hol_data$original_price)*exp(as.numeric(fes))*
          (hol_data$offering_duration-hol_data$holiday.len)
      
        
        D2hval[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$offering_duration)+((output_ls[[kiter]]$par[7])))
        
        # D2hvalpri[[kiter]] <- exp((output_ls[[kiter]]$par[5])*log(hol_data$holiday.len)+((output_ls[[kiter]]$par[7])))
        D2hvalpri[[kiter]] <- exp(((output_ls[[kiter]]$par[7])))*hol_data$holiday.len
        
        betad <- c(betad,(output_ls[[kiter]]$par[5]))
        
      }else{
        
        D1hvalpri[[kiter]] <- Stotpri*(output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h)))/
          (1+(output_ls[[kiter]]$par[6]+ exp(fes_h)/(hol_data$total_volume-exp(fes_h))))
        
        fes <- control_FE(sales_estimates[type,],hol_data)
        D2nhval[[kiter]] <- ((mleout_preh[[type]]$par[2])+((mleout_preh[[type]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                               -exp(mleout_preh[[type]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
        
        # D2nhvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2])+
        #                           -exp(mleout_preh[[type]]$par[3])*log(hol_data$holiday.len))*exp(as.numeric(fes))*
        #   (hol_data$offering_duration-hol_data$holiday.len)/hol_data$holiday.len
        
        D2nhvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2]))#*exp(as.numeric(fes))*
          #(hol_data$offering_duration-hol_data$holiday.len)/hol_data$holiday.len
        
        
        D2hval[[kiter]] <- ((output_ls[[kiter]]$par[5])*log(hol_data$offering_duration)+((output_ls[[kiter]]$par[7])))
        
        # D2hvalpri[[kiter]] <- ((output_ls[[kiter]]$par[5])*log(hol_data$holiday.len)+((output_ls[[kiter]]$par[7])))
        D2hvalpri[[kiter]] <- (((output_ls[[kiter]]$par[7])))*hol_data$holiday.len
        
        betad <- c(betad,(output_ls[[kiter]]$par[5]))
      }
      
    }
    
    
    # nondealdemandhol <- D1hvalpri[[kiter]] + pmax(D2hvalpri[[kiter]]/hol_data$holiday.len ,0)
    nondealdemandhol <- D1hvalpri[[kiter]] + pmax(D2hvalpri[[kiter]] ,0)
    nondealdemandnhol <- Stotpri - D1hvalpri[[kiter]]+ pmax(D2nhvalpri[[kiter]],0)

    dealdemandhol <- D1hval[[kiter]] + pmax(D2hval[[kiter]],0)
    dealdemandnhol <- Stot - D1hval[[kiter]] + pmax(D2nhval[[kiter]],0)
    
    # nondealdemandhol <- D1hvalpri[[kiter]] + D2hvalpri[[kiter]]
    # nondealdemandnhol <- Stotpri - D1hvalpri[[kiter]]+ D2nhvalpri[[kiter]]
    # 
    # dealdemandhol <- D1hval[[kiter]] + D2hval[[kiter]]
    # dealdemandnhol <- Stot - D1hval[[kiter]] + D2nhval[[kiter]]
    
    holgap[[kiter]] <- (-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
      ((dealdemandhol+dealdemandnhol))
    nonholgap[[kiter]] <- (-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
      ((nondealdemandhol+nondealdemandnhol))
    
    dsidx2[[kiter]] <- ((-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
                          ((dealdemandhol+dealdemandnhol))-
                          (-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
                          ((nondealdemandhol+nondealdemandnhol)))/
      abs((-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
            ((nondealdemandhol+nondealdemandnhol)))
    
    
    nondealdemandhol <- D1hvalpri[[kiter]]
    nondealdemandnhol <- Spri[[kiter]] - D1hvalpri[[kiter]]
    
    dealdemandhol <- D1hval[[kiter]]
    dealdemandnhol <- hol_data$total_volume - D1hval[[kiter]]
    

    # avgdailydemand <- (hol_data$total_volume + D2hval[[kiter]] )/hol_data$holiday.len
    # print(mean(dsidx[[kiter]]/avgdailydemand,na.rm = TRUE))
    
    dsidx[[kiter]] <- ((-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/hol_data$total_volume-
                         ((-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/Spri[[kiter]]))/
      ((-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/Spri[[kiter]])

    dsidxvec <- c(dsidxvec,mean(dsidx[[kiter]][dsidx[[kiter]]<quantile(dsidx[[kiter]],0.9,na.rm = TRUE)&
                                                 dsidx[[kiter]]>quantile(dsidx[[kiter]],0.1,na.rm = TRUE)],na.rm = TRUE))
    dsidxsigvec <- c(dsidxsigvec,sd(dsidx[[kiter]][dsidx[[kiter]]<quantile(dsidx[[kiter]],0.9,na.rm = TRUE)&
                                                     dsidx[[kiter]]>quantile(dsidx[[kiter]],0.1,na.rm = TRUE)],na.rm = TRUE))

    dsidx2vec <- c(dsidx2vec,median(dsidx2[[kiter]][dsidx2[[kiter]]<quantile(dsidx2[[kiter]],0.8,na.rm = TRUE)&
                                                      dsidx2[[kiter]]>quantile(dsidx2[[kiter]],0.2,na.rm = TRUE)],na.rm = TRUE))
    dsidx2sigvec <- c(dsidx2sigvec,sd(dsidx2[[kiter]][dsidx2[[kiter]]<quantile(dsidx2[[kiter]],0.8,na.rm = TRUE)&
                                                        dsidx2[[kiter]]>quantile(dsidx2[[kiter]],0.2,na.rm = TRUE)],na.rm = TRUE))
    
  
    
    Dtotalpri[[kiter]] <- as.numeric(Spri[[kiter]] + D2nhvalpri[[kiter]] + D2hvalpri[[kiter]])
    Dtotal[[kiter]] <- as.numeric(hol_data$total_volume + pmax(0,D2nhval[[kiter]])+ pmax(0,D2hval[[kiter]]))
    
    
    dalter[[kiter]] <- (-(as.numeric(Spri[[kiter]] + pmax(D2nhvalpri[[kiter]],0)+ pmax(D2hvalpri[[kiter]],0))) +
                          as.numeric(hol_data$total_volume + pmax(0,D2nhval[[kiter]])+ pmax(0,D2hval[[kiter]])))/
      abs((as.numeric(Spri[[kiter]] + pmax(D2nhvalpri[[kiter]],0)+ pmax(D2hvalpri[[kiter]],0))))
    
    changeind[[kiter]] <- (-(as.numeric(Spri[[kiter]] + pmax(D2nhvalpri[[kiter]],0)+ pmax(D2hvalpri[[kiter]],0))) +
                             as.numeric(hol_data$total_volume + pmax(0,D2nhval[[kiter]])+ pmax(0,D2hval[[kiter]])))
    
    nondealprofit[[kiter]] <- hol_data$original_price*Spri[[kiter]]*sd(sdop$original_price) +
      hol_data$original_price*sd(sdop$original_price)*((D2nhvalpri[[kiter]]) + (D2hvalpri[[kiter]])) -
      cnh*sd(sdopph$original_price)/((hol_data$offering_duration - hol_data$holiday.len))*(((D2nhvalpri[[kiter]])+(nondealdemandnhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_preh[i,1]*2)) -
      ch*sd(sdop$original_price)/(hol_data$holiday.len)*(((D2hvalpri[[kiter]]) + (nondealdemandhol))^2+exp(gamma_h[i,1]*2)+exp(gamma_h[i,1]*2))
    
    dealprofit[[kiter]] <- hol_data$original_price*sd(sdop$original_price)*(hol_data$discount)*hol_data$total_volume +
      hol_data$original_price*sd(sdop$original_price)*((D2nhval[[kiter]]) +(D2hval[[kiter]])) -
      cnh*sd(sdopph$original_price)/(hol_data$offering_duration - hol_data$holiday.len)*(((D2nhval[[kiter]])+dealdemandnhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_preh[i,1]*2)) -
      ch*sd(sdop$original_price)/(hol_data$holiday.len)*(((D2hval[[kiter]]) + dealdemandhol)^2+exp(gamma_h[i,1]*2+2*gamma_h[i,2]*hol_data$prep.period)+exp(gamma_h[i,1]*2))
    
    profitalter[[kiter]] <- (-nondealprofit[[kiter]]+dealprofit[[kiter]])/abs(nondealprofit[[kiter]])
    
    # print(paste("D1h = ",mean(D1hval[[kiter]]), " ", sd(D1hval[[kiter]])))
    # print(paste("D2h = ", mean(D2hval[[kiter]]), " ", sd(D2hval[[kiter]])))
    # print(paste("D2nh = ", mean(D2nhval[[kiter]]), " ", sd(D2nhval[[kiter]])))
    # #print(mean(r[[kiter]][r[[kiter]]>0]))
    # print(paste("ratio = ", mean(r[[kiter]])))
    # print(paste("alterD = ", mean(dalter[[kiter]][dalter[[kiter]]<quantile(dalter[[kiter]],0.8)&
    #                                             dalter[[kiter]]>quantile(dalter[[kiter]],0.2)])))
    # print(paste("sig alterD = ", sd(dalter[[kiter]][dalter[[kiter]]<quantile(dalter[[kiter]],0.8)&
    #                                               dalter[[kiter]]>quantile(dalter[[kiter]],0.2)])))
    # print(paste("alterP = ", mean(profitalter[[kiter]][profitalter[[kiter]]<quantile(profitalter[[kiter]],0.8)&
    #                                                  profitalter[[kiter]]>quantile(profitalter[[kiter]],0.2)])))
    # print(paste("sig alterP = ", sd(profitalter[[kiter]][profitalter[[kiter]]<quantile(profitalter[[kiter]],0.8)&
    #                                                    profitalter[[kiter]]>quantile(profitalter[[kiter]],0.2)])))
    
    
    
    ratiovec <- c(ratiovec, mean(r[[kiter]]))
    ratiosigvec <- c(ratiosigvec, sd(r[[kiter]]))
    
    D1hvec <- c(D1hvec,mean(D1hval[[kiter]]))
    D1hsigvec <- c(D1hsigvec,sd(D1hval[[kiter]]))
    
    D2nhvec <- c(D2nhvec,mean(D2nhval[[kiter]]))
    D2nhsigvec <- c(D2nhsigvec,sd(D2nhval[[kiter]]))
    
    D2hvec <- c(D2hvec,mean(D2hval[[kiter]]))
    D2hsigvec <- c(D2hsigvec,sd(D2hval[[kiter]]))
    
    alterDvec <- c(alterDvec,mean(dalter[[kiter]][dalter[[kiter]]<quantile(dalter[[kiter]],0.8,na.rm = TRUE)&
                                                    dalter[[kiter]]>quantile(dalter[[kiter]],0.2,na.rm = TRUE)],na.rm = TRUE))
    alterDsigvec <- c(alterDsigvec,sd(dalter[[kiter]][dalter[[kiter]]<quantile(dalter[[kiter]],0.8,na.rm = TRUE)&
                                                        dalter[[kiter]]>quantile(dalter[[kiter]],0.2,na.rm = TRUE)],na.rm = TRUE))
    alterP <- c(alterP,mean(profitalter[[kiter]][profitalter[[kiter]]<quantile(profitalter[[kiter]],0.8,na.rm = TRUE)&
                                                   profitalter[[kiter]]>quantile(profitalter[[kiter]],0.2,na.rm = TRUE)],na.rm = TRUE))
    alterPsigvec <- c(alterPsigvec,sd(profitalter[[kiter]][profitalter[[kiter]]<quantile(profitalter[[kiter]],0.8,na.rm = TRUE)&
                                                             profitalter[[kiter]]>quantile(profitalter[[kiter]],0.2,na.rm = TRUE)],na.rm = TRUE))
    
    
    ub <- 0.8
    lb <- 0.2
    fes_nhnd <- control_FE(sales_estimates[type,],nonhol_data)

    D2nhndvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2]))#*exp(as.numeric(fes_nhnd))
    
    if(type %in% c(11)){
      D2nhndvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2]))#*exp(as.numeric(fes_nhnd))
      
      nonholnormdemand[[kiter]] <- pmax(D2nhndvalpri[[kiter]],0)*nonhol_data$offering_duration+
        nonhol_data$total_volume
    }
    else if(type %in% c(2)){

      D2nhndvalpri[[kiter]] <- exp((mleout_preh[[type]]$par[2]))*exp(as.numeric(fes_nhnd))


      nonholnormdemand[[kiter]] <- pmax(D2nhndvalpri[[kiter]]*nonhol_data$offering_duration,0)+
        nonhol_data$total_volume

      nonholnormdemand[[kiter]] <- pmax(D2nhndvalpri[[kiter]]*nonhol_data$offering_duration,0) +
        nonhol_data$total_volume/exp(sales_estimates[type,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
                                       (sales_estimates[type,2])*log(nonhol_data$offering_duration)
        )*
        exp(sales_estimates[type,4]*(nonhol_data$original_price))*nonhol_data$offering_duration



    }
    else{
      D2nhndvalpri[[kiter]] <- ((mleout_preh[[type]]$par[2]))#*exp(as.numeric(fes_nhnd))
      
      nonholnormdemand[[kiter]] <- pmax(D2nhndvalpri[[kiter]]*nonhol_data$offering_duration,0) + 
        nonhol_data$total_volume/exp(sales_estimates[type,4]*(1-nonhol_data$discount)*nonhol_data$original_price +
                                       max(sales_estimates[type,2],0)*log(nonhol_data$offering_duration)
        )*
        exp(sales_estimates[type,4]*(nonhol_data$original_price))*nonhol_data$offering_duration
      
      
    }
    
    

    
    
    
    # holnormdemand[[kiter]] <- (as.numeric(Spri[[kiter]] + 
    #                                         pmax(D2nhvalpri[[kiter]]*hol_data$offering_duration,0) + 
    #                                         pmax(D2hvalpri[[kiter]]*hol_data$offering_duration,0)))/hol_data$offering_duration 
    
    holnormdemand[[kiter]] <- (as.numeric(Spri[[kiter]] + 
                                            pmax(D2nhvalpri[[kiter]],0)+ 
                                          pmax(D2hvalpri[[kiter]],0)))
    
    
    
    tt <- merge(data.frame(nonval = (nonholnormdemand[[kiter]]),
                           op = nonhol_data$original_price*sd(sdopph$original_price)),
                data.frame(hval = (holnormdemand[[kiter]]),
                           op = hol_data$original_price*sd(sdop$original_price)),by="op",all=TRUE)%>%
      mutate(n9 = quantile(nonval,ub,na.rm = TRUE),n05 = quantile(nonval,lb,na.rm = TRUE))%>%
      mutate(h95 = quantile(hval,ub,na.rm = TRUE),h05 = quantile(hval,lb,na.rm = TRUE))%>%
      #filter(op<quantile(op,0.9),op>quantile(op,0.1))%>%
      filter(nonval <n9, nonval>n05,hval<h95,hval>h05)%>%
      mutate(diff=(hval-nonval)/nonval)%>%
      filter(is.na(diff)==0)
    
    
    
    muh <- median(holnormdemand[[kiter]][holnormdemand[[kiter]]<quantile(holnormdemand[[kiter]],ub,na.rm = TRUE)&
                                         holnormdemand[[kiter]]>quantile(holnormdemand[[kiter]],lb,na.rm = TRUE)])
    munh <- median(nonholnormdemand[[kiter]][nonholnormdemand[[kiter]]<quantile(nonholnormdemand[[kiter]],ub,na.rm = TRUE)&
                                             nonholnormdemand[[kiter]]>quantile(nonholnormdemand[[kiter]],lb,na.rm = TRUE)])
    
    sigh <- sd(holnormdemand[[kiter]][holnormdemand[[kiter]]<quantile(holnormdemand[[kiter]],ub,na.rm = TRUE)&
                                        holnormdemand[[kiter]]>quantile(holnormdemand[[kiter]],lb,na.rm = TRUE)])
    signh <- sd(nonholnormdemand[[kiter]][nonholnormdemand[[kiter]]<quantile(nonholnormdemand[[kiter]],ub,na.rm = TRUE)&
                                            nonholnormdemand[[kiter]]>quantile(nonholnormdemand[[kiter]],lb,na.rm = TRUE)])
    
    
    covh <- cov(tt$nonval,tt$hval)
    
    deltaDvec <- c(deltaDvec,(muh-munh)/munh)
    deltaDsigvec <- c(deltaDsigvec,sqrt(muh^2/munh^2*(sigh^2/muh^2+signh^2/munh^2-2*(covh)/(muh*munh))))
    # deltaDvec <- c(deltaDvec,muh/munh + signh*muh/munh^3-covh/munh^2-1)
    
  }
  
  return(list(  mchs = mchs, mcnhs=mcnhs,
                mucpvec = mucpvec, sigcpvec = sigcpvec,
                mucpnhvec = mucpnhvec, sigcpnhvec = sigcpnhvec,
                thetaalpha=thetaalpha,
                thetaT  = thetaT,
                thetad = thetad,
                betad = betad,
                betad_nh = betad_nh,
                betaalpha_nh = betaalpha_nh,
                Spri = Spri,r = r,
                D1hval = D1hval,D1hvalpri = D1hvalpri,
                D2hval = D2hval,D2hvalpri = D2hvalpri,
                D2nhval = D2nhval,D2nhvalpri = D2nhvalpri,
                dsidx  = dsidx,dsidx2 = dsidx2,Dtotal = Dtotal,
                holgap = holgap, nonholgap = nonholgap,
                dalter = dalter,Dtotalpri = Dtotalpri,changeind = changeind,
                nondealprofit = nondealprofit ,dealprofit = dealprofit ,
                profitalter = profitalter,
                dsidxvec = dsidxvec, dsidxsigvec=dsidxsigvec,
                dsidx2vec = dsidx2vec,dsidx2sigvec = dsidx2sigvec,
                ratiovec = ratiovec,ratiosigvec = ratiosigvec,
                D1hvec =D1hvec,D1hsigvec=D1hsigvec,
                D2hvec = D2hvec,D2hsigvec = D2hsigvec,
                D2nhvec = D2nhvec,D2nhsigvec = D2nhsigvec,
                alterDvec = alterDvec,alterDsigvec = alterDsigvec,
                alterP = alterP,alterPsigvec = alterPsigvec,
                deltaDvec = deltaDvec, deltaDsigvec= deltaDsigvec
  ))
  
  
}





VarRed <- list()
Varidxvec <- vector()
for(i in industries_to_check){
  ch <- (exp(ceiling(output_ls[[i]]$par[1])))
  cnh <- (exp(floor(mleout_preh[[i]]$par[1])))
  
  VarRed[[i]] <- (-cnh*exp(2*gamma_h[i,1]+2*gamma_h[i,2]*(hol_data$prep.period))-ch*exp(2*gamma_h[i,1]+2*gamma_h[i,2]*(hol_data$prep.period))+
                    cnh*exp(2*gamma_h[i,1]) + ch*exp(2*gamma_h[i,1]))/
    (cnh*exp(2*gamma_h[i,1]) + ch*exp(2*gamma_h[i,1]))
  
  print(paste("VarRed index = ", mean(VarRed[[i]])))
  
  Varidxvec <- c(Varidxvec,mean(VarRed[[i]]))
  
}




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


