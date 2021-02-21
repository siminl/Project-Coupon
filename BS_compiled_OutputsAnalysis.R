industries_to_check <- c(2,5,11,10,1)

# bs_outputs_df <- vector()
# bs_outputs_df <- sapply(bs_outputs_stored, function(x) sapply(x,function(xx) cbind(bs_outputs_df,c(xx$par,xx$convergence,xx$value))))
# 
# 
# bs_outputs_compile <- (t(rep(0,dim(bs_outputs_df[[1]])[1]+2)))
# colnames(bs_outputs_compile) <- c("ch","thetaalpha","thetaT","thetad","betad","theta0","beta0","conv","funvalue","mase_dfremain","type")
# 
# for(i in industries_to_check){
#   tmp <- data.frame(t(bs_outputs_df[[i]]),
#                     mase_dfremain = t(t(mase_remain[[i]])),type=i)
#   colnames(tmp) <- c("ch","thetaalpha","thetaT","thetad","betad","theta0","beta0","conv","funvalue","mase_dfremain","type")
#   bs_outputs_compile <- rbind(bs_outputs_compile,tmp)
# }
# bs_outputs_compile <- bs_outputs_compile[-1,]
# bs_outputs_compile <- bs_outputs_compile
# 
# save(bs_outputs_compile,file= "bs_out_compile.Rdata")
# # the sales estimates during holiday time 
# Scoef <- data.frame(type = c(2,5,11,10,1),
#                     Salpha = c(-4.47,-1.40,-3.01,-1.40,-3.20),
#                     Sd = c(3.36,6.33,1.0,0.01,0.04),
#                     ST = c(2.60,0.88,2.8,0.69,1.03))


# save(compiled_results,file = "compiled_results.Rdata")
# save(compiled_results_ls,file = "compiled_results_ls.Rdata")

compiled_results <- vector()
compiled_results_ls <- vector(mode = "list",length=11)
for(i in industries_to_check){
  
  
  if(i == 2){
    nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type%in%c(2,3))%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6)))%>%
      {.->>sdopph}%>%
      mutate(original_price = original_price/sd(original_price))
    
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3),prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6)))%>%
      {.->>sdop}%>%
      mutate(original_price = original_price/sd(original_price))

    
    # set.seed(100)
    # rs <-list()
    # for(b in 1:6){
    #   rs[[b]] <- sample(dim(hol_data)[1],dim(hol_data)[1],replace = T)
    # }
    # hol_data <- hol_data[rs[[1]],]

  }else if(i %in% c(10)){
    nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdopph}%>%
      mutate(original_price = original_price/sd(original_price))
    
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=25)%>%{.->>dealsfull_type}%>%
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
    nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdopph}%>%
      mutate(original_price = original_price/sd(original_price))
    
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
    
    nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdopph}%>%
      mutate(original_price = original_price/sd(original_price))
    
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      {.->>sdop}%>%
      mutate(original_price = original_price/sd(original_price))
  }

  if(i == 2){
    hol_data <- hol_data%>%filter(city!="XA")
  }else if(i %in% c(10)){
    hol_data <- hol_data%>%filter(platform!="ww")
  }else if(i %in% c(5)){
    #hol_data <- hol_data%>%filter(platform!="ww",city!="GZ")
  }
  
  rt <- getPerformance(hol_data,sdop, nonhol_data, sdopph, output_ls=bs_outputs[[i]], type=i,mleout_preh,mleout_h )
  
  compiled_results_ls[[i]] <- rt
  compiled_results <- rbind(compiled_results,
                            data.frame(type.x=i,dsidx2 = rt$dsidx2vec, dsidx = rt$dsidxvec, 
                                       alterD = rt$alterDvec,alterDsigvec = rt$alterDsigvec,
                                 deltaD = rt$deltaDvec,alterP = rt$alterP,deltaDvec = rt$deltaDvec,
                                 mch = rt$mchs, mcnh = rt$mcnhs,mucp = rt$mucpvec,mucpnh=rt$mucpnhvec,
                                 D1h = rt$D1hvec, D2h = rt$D2hvec,
                                 D2nh = rt$D2nhvec, ratio = rt$ratiovec,
                                 thetaalphahat = rt$thetaalpha, thetaThat = rt$thetaT,
                                 thetadhat = rt$thetad, betadhat = rt$betad)
  )
  
}

#  ------- Start of displaying the estimators -------------

out_ls_name <- vector(mode="list",length=11)
bs_sample <- c(0,0)
for(i in industries_to_check){
  
  root_path_base <- "/Users/siminli/Documents/GitHub/Project-Coupon/Deals_bootstrap_outputs/"
  root_path_folder <- paste("output_type",i,"/",sep="")
  root_path <- paste(root_path_base,root_path_folder,sep="")
  
  if(i %in% c(10,11)){
    
    out_ls_name[[i]] <- paste(root_path,list.files(path=root_path, pattern="out\\_[[:digit:]][[:digit:]]\\_[[:digit:]].*\\.rda"),sep="")
    
  }else{
    out_ls_name[[i]] <- paste(root_path,list.files(path=root_path,pattern="out\\_[[:digit:]]\\_[[:digit:]].*\\.rda"),sep="")
    
  }
  
  if(i %in% c(2) ){
    bs_sample <- rbind(bs_sample,data.frame(type = i, 
                                            sample_number = word(str_extract(pattern="out\\_[[:digit:]]\\_[[:digit:]].*\\.rda",
                                                                             out_ls_name[[i]]), 3, sep = "_")))
  }else if(i %in% c(1)){
    bs_sample <- rbind(bs_sample,data.frame(type = i, 
                                            sample_number = word(str_extract(pattern="out\\_[[:digit:]]\\_[[:digit:]].*\\.rda",
                                                                             out_ls_name[[i]]), 2, sep = "_")))
    
    
  }else if (i %in% c(5)){
    bs_sample <- rbind(bs_sample,data.frame(type = i, 
                                            sample_number = word(word(str_extract(pattern="out\\_[[:digit:]]\\_[[:digit:]].*\\.rda",
                                                                                  out_ls_name[[i]]), 4, sep = "_"),1, sep="\\.")))
    
  }else{
    bs_sample <- rbind(bs_sample,data.frame(type = i, 
                                            sample_number = word(word(str_extract(pattern="out\\_[[:digit:]][[:digit:]]\\_[[:digit:]].*\\.rda",
                                                                                  out_ls_name[[i]]), 4, sep = "_"),1, sep="\\.")))
    
  }
  
}
bs_sample <- bs_sample[-1,]

compiled_results <- cbind(compiled_results,bs_sample)

bs_outputs_compile <- cbind(bs_outputs_compile,compiled_results)%>%
  filter(conv==0,mase_dfremain>0.55)

picked <- rep(0, dim(bs_outputs_compile)[1])
picked[c(1,13,23,38,66)] <- 1
estimates_sd <- (merge(unique(compiled_results), 
                      bs_outputs_compile %>% sl(funvalue,mase_dfremain,mch,D1h) %>% 
                        mt(picked = picked), by=c("mch","D1h"), all.y=TRUE) %>% 
  fl(is.na(funvalue)==0)) %>% arrange(type) 



estimates_sd[-c(24,25,26,27,28,seq(15,20),40,seq(59,64),32,33,34,36,37,39,45,44,47,48,49,50,4,5,10,3,7,9,56,70),] %>% 
  mutate(mcratio = mucp/mucpnh) %>% 
  # gb(type, sample_number) %>% 
  # mt(minfuncval = (funvalue==min(funvalue))) %>% 
  # fl(minfuncval == 1) %>% 
  gb(type) %>% mt(mumch = mean(mch),
                  mumcnh = mean(mcnh),
                  mumcratio = mean(mcratio),
                  mudeltaDvec = mean(deltaDvec),
                  mualterD = mean(alterD),
                  mudsidx2 = mean(dsidx2),
                  muthetaalpha= mean(thetaalphahat),
                  muthetaT = mean(thetaThat),
                  muthetad = mean(thetadhat),
                  mubetad = mean(betadhat),
                  muratio = mean(ratio),
                  mualterP = mean(alterP),
                  muD1h = mean(D1h),
                  muD2h = mean(D2h),
                  muD2nh = mean(D2nh)) %>% 
  gb(type) %>% 
  summarise(ssize = n(),semch = sqrt(sum((mch-mumch)^2)/ssize),
            semcnh = sqrt(sum((mcnh-mumcnh)^2)/ssize),
            semcratio = sqrt(sum((mcratio-mumcratio)^2)/ssize),
            sedeltaDvec = sqrt(sum((deltaDvec-mudeltaDvec)^2)/ssize),
            sealterD = sqrt(sum((alterD-mualterD)^2)/ssize),
            sedsidx2 = sqrt(sum((dsidx2-mudsidx2)^2)/ssize),
            sethetaalpha = sqrt(sum((thetaalphahat-muthetaalpha)^2)/ssize),
            sethetaT = sqrt(sum((thetaThat-muthetaT)^2)/ssize),
            sethetad = sqrt(sum((thetadhat-muthetad)^2)/ssize),
            sebetad = sqrt(sum((betadhat-mubetad)^2)/ssize),
            seratio = sqrt(sum((ratio-muratio)^2)/ssize),
            sealterP = sqrt(sum((alterP-mualterP)^2)/ssize),
            seD1h = sqrt(sum((D1h-muD1h)^2)/ssize),
            seD2h = sqrt(sum((D2h-muD2h)^2)/ssize),
            seD2nh = sqrt(sum((D2nh-muD2nh)^2)/ssize)) -> se_df


se_df <- se_df[c(2,3,5,4,1),]

save("se_df", file = "se_df.Rdata")
#36,37
estimates_sd[-c(24,25,26,27,28,seq(15,20),seq(59,64),32,33,34,39,45,40,44,47,46,48,49,51,3,4,5,7,10,9,56,70),] %>% 
  mutate(mcratio = mucp/mucpnh) %>% 
  # gb(type, sample_number) %>% 
  # mt(minfuncval = (funvalue==min(funvalue))) %>% 
  # fl(minfuncval == 1) %>% 
  gb(type) %>% mt(mumch = mean(mch),
                  mumcnh = mean(mcnh),
                  mumcratio = mean(mcratio),
                  mudeltaDvec = mean(deltaDvec),
                  mualterD = mean(alterD),
                  mudsidx2 = mean(dsidx2),
                  muthetaalpha= mean(thetaalphahat),
                  muthetaT = mean(thetaThat),
                  muthetad = mean(thetadhat),
                  mubetad = mean(betadhat),
                  muratio = mean(ratio),
                  mualterP = mean(alterP),
                  muD1h = mean(D1h),
                  muD2h = mean(D2h),
                  muD2nh = mean(D2nh)) %>% 
  gb(type) %>% 
  summarise(ssize = n(),semch = sqrt(sum((mch-mumch)^2)/ssize),
            semcnh = sqrt(sum((mcnh-mumcnh)^2)/ssize),
            semcratio = sqrt(sum((mcratio-mumcratio)^2)/ssize),
            sedeltaDvec = sqrt(sum((deltaDvec-mudeltaDvec)^2)/ssize),
            sealterD = sqrt(sum((alterD-mualterD)^2)/ssize),
            sedsidx2 = sqrt(sum((dsidx2-mudsidx2)^2)/ssize),
            sethetaalpha = sqrt(sum((thetaalphahat-muthetaalpha)^2)/ssize),
            sethetaT = sqrt(sum((thetaThat-muthetaT)^2)/ssize),
            sethetad = sqrt(sum((thetadhat-muthetad)^2)/ssize),
            sebetad = sqrt(sum((betadhat-mubetad)^2)/ssize),
            seratio = sqrt(sum((ratio-muratio)^2)/ssize),
            sealterP = sqrt(sum((alterP-mualterP)^2)/ssize),
            seD1h = sqrt(sum((D1h-muD1h)^2)/ssize),
            seD2h = sqrt(sum((D2h-muD2h)^2)/ssize),
            seD2nh = sqrt(sum((D2nh-muD2nh)^2)/ssize))  -> tt


# 69/66, 38/37, 13/16
(bs_outputs_compile[c(1,16,23,38,66),]%>%mutate(mcratio = mucp/mucpnh)%>%
       select(type,mch,mcnh,thetaalphahat,thetaThat,thetadhat,betadhat,dsidx,dsidx2,mcratio,mucp,alterD,alterP,deltaDvec,D1h,D2h,D2nh,ratio)) -> pest_df
#thetaThat = -0.87 for 5,


# S_alpha, S_T, S_d  --------

Scoef_se <- c(0,0,0,0)
for(i in industries_to_check){
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    mutate(original_price = original_price/sd(original_price))
  
  if(i == 2){
    hol_data <- hol_data%>%filter(city!="XA")
  }else if(i %in% c(10)){
    hol_data <- hol_data%>%filter(platform!="ww")
  }else if(i %in% c(5)){
    #hol_data <- hol_data%>%filter(platform!="ww",city!="GZ")
  }
  
  
  summary(lm(data=hol_data%>%mutate(discprice = (1-discount)*original_price, # = price/partysize
                                    logvol = log(total_volume),
                                    perperson = original_price/partysize,
                                    competitors_ct = (competitors/sd(competitors)),
                                    logcomp = ifelse(competitors!=0,log(competitors),0),
                                    fixlength = ifelse(offering_duration==15,1,
                                                       ifelse(offering_duration==30,2,
                                                              ifelse(offering_duration==45,3,0)))),
             logvol~log(offering_duration)+I(log(offering_duration)^2)+
               discprice+I(discprice^2)+
               log(prep.period) + I((log(prep.period))^2) +
               #log(discprice)+I(log(discprice)^2)+
               I((original_price))+I(1/log(original_price))+I(1/(log(original_price)^2))+
               logcomp+weekends+platform:weekends+
               platform+city
  )
  )  -> lm_S
  
  Scoef_se <- rbind(Scoef_se,data.frame(type = i, t(coef(lm_S)[c(2,4,6),2])))
  
}
Scoef_se <- Scoef_se[-1,]

# the MASE and MAPE table --------
digs <- 4
rbind(Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
      cbind(round(mase_ls[[2]][-1,][1,],digs),round(mase_ls[[5]][-1,][5,],digs),round(mase_ls[[11]][-1,][8,],digs),
            round(mase_ls[[10]][-1,][6,],digs),round(mase_ls[[1]][-1,][26,],digs)),
      cbind(round(mape_ls[[2]][-1,][1,],digs),round(mape_ls[[5]][-1,][5,],digs),round(mape_ls[[11]][-1,][8,],digs),
            round(mape_ls[[10]][-1,][6,],digs),round(mape_ls[[1]][-1,][26,],digs))) -> err_table


rownames(err_table) <- c("Industry",
                         "MASE (discount, $\\alpha$)","MASE (launch day to holiday, $T$)","MASE (duration, $d$)",
                         "MAPE (discount, $\\alpha$)","MAPE (launch day to holiday, $T$)","MAPE (duration, $d$)")


print(xtable(err_table[,c(4,1,2,5,3)]), only.contents=TRUE, include.rownames=T, 
      include.colnames=T, floating=T, sanitize.rownames.function = identity,
      file = 'err_table.tex')

# estimated structural parameters table -----
rbind(Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
      #     type = industries_to_check,
      # c = signif(cvec,2),
      # cnh = signif(cnhvec,2),
      thetaalpha = format(round(pest_df$thetaalphahat,2)),
      thetaalphasig = paste("(",format(round(se_df$sethetaalpha,2)),")",sep=""),
      thetaT = format(round(pest_df$thetaThat,2)),
      thetaTsig = paste("(",format(round(se_df$sethetaT,2)),")",sep=""),
      thetad = format(round(pest_df$thetadhat,2)),
      thetadsig = paste("(",format(round(se_df$sethetad,2)),")",sep=""),
      betad = format(round(pest_df$betadhat,2)),
      betadsig = paste("(",format(round(se_df$sebetad,2)),")",sep=""),
      Salpha = format(round(Scoef$Salpha,2)),
      Salphasig = paste("(",format(round(Scoef_se$discprice,2)),")",sep=""),
      ST = format(round(Scoef$ST,2)),
      STsig = paste("(",format(round(Scoef_se$log.prep.period.,2)),")",sep=""),
      Sd = format(round(Scoef$Sd,2)),
      Sdsig = paste("(",format(round(Scoef_se$log.offering_duration.,2)),")",sep=""),
      # betad_nh=round(betad_nh,2),
      # betaalpha_nh=round(betaalpha_nh,2),
      sigma = format(round(gamma_h[industries_to_check,1],2)),
      sigmasig = paste("(",format(round(gamma_h_sig[industries_to_check,1],2)),")",sep=""),
      gamma = format(round(gamma_h[industries_to_check,2],2)),
      gammasig = paste("(",format(round(gamma_h_sig[industries_to_check,2],2)),")",sep=""),
      mch = format(round(pest_df$mch,2)),
      mchsig = paste("(",format(round(se_df$semch,2)),")",sep=""),
      mcnh = format(round(pest_df$mcnh,2)),
      mcnhsig = paste("(",format(round(se_df$semcnh,2)),")",sep=""),
      mcratiovec = format(round(pest_df$mcratio,2)),
      mcratiovecsig = paste("(",format(round(se_df$semcratio,2)),")",sep=""),
      ratiovec = format(round(pest_df$ratio,2)),
      ratiosig = paste("(",format(round(se_df$seratio,2)),")",sep=""),
      D1hvec = format(round(pest_df$D1h,0)),
      D1hsigvec = paste("(",format(round(se_df$seD1h,0)),")",sep=""),
      D2hvec = format(round(pest_df$D2h,2)),
      D2hsigvec = paste("(",format(round(se_df$seD2h,2)),")",sep=""),
      D2nhvec = format(round(pest_df$D2nh,2)),
      D2nhsigvec = paste("(",format(round(se_df$seD2nh,2)),")",sep=""),
      # mucpvec  = signif(mucpvec,2), 
      # sigcpvec  = signif(sigcpvec,2), 
      #dsidxvec = round(dsidxvec,2),
      #dsidxsigvec = round(dsidxsigvec,2),
      deltaDvec = format(round(pest_df$deltaDvec,2)),
      deltaDsigvec = paste("(",format(round(se_df$sedeltaDvec,2)),")",sep=""),
      # Varidxvec = round(Varidxvec,2),
      dsidx2vec  = format(round(pest_df$dsidx2,2)) ,
      dsidx2sigvec = paste("(",format(round(se_df$sedsidx2,2)),")",sep=""),
      alterDvec = format(round(pest_df$alterD,2)),
      alterDsigvec = paste("(",format(round(se_df$sealterD,2)),")",sep=""),
      alterPvec = format(round(pest_df$alterP,2)),
      alterPsigvec = paste("(",format(round(se_df$sealterP,2)),")",sep="")
) -> StructEstTable 

# StructEstTable  <- StructEstTable[,c(4,1,2,5,3)]


# rownames(StructEstTable) <- c("Industry",
#                               # "Holiday cost parameter, $c_{h}$","Non-holiday cost parameter, $c_{nh}$",
#                               "Discount sensitivity in deal demand allocation, $\\widehat{\\theta}_{\\alpha}$",
#                               "s.e., discount sensitivity in demand allocation, $\\sigma_{\\theta_{\\alpha}}$",
#                               "Launch time sensitivity in deal demand allocation, $\\widehat{\\theta}_T$",
#                               "s.e., launch time sensitivity in demand allocation, $\\sigma_{\\theta_T}$",
#                               "Duration sensitivity in deal demand allocation, $\\widehat{\\theta}_d$",
#                               "s.e., duration sensitivity in demand allocation, $\\sigma_{\\theta_d}$",
#                               "Duration sensitivity, holiday full price customers, $\\widehat{\\beta}_d$",
#                               "s.e., duration sensitivity, holiday full price customers, $\\sigma_{\\beta_d}$",
#                               "Discount sensitivity in deal sales volume, $\\widehat{\\theta}_{S,\\alpha}$",
#                               "s.e., discount sensitivity in deal sales volume, $\\sigma_{\\theta_{S,\\alpha}}$",
#                               "Launch time sensitivity in deal sales volume, $\\widehat{\\theta}_{S,T}$",
#                               "s.e., launch time sensitivity in deal sales volume, $\\sigma_{\\theta_{S,T}}$",
#                               "Duration sensitivity in deal sales volume, $\\widehat{\\theta}_{S,d}$",
#                               "s.e., duration sensitivity in deal sales volume, $\\sigma_{\\theta_{S,d}}$",
#                               # "Duration sensitivity, non-holiday full price customers, $\\beta_{d,nh}$",
#                               # "Discount sensitivity, non-holiday full price customers,$\\beta_{\\alpha,nh}$",
#                               "Baseline holiday demand variance, $\\widehat{\\sigma}_h$",
#                               "Launch time sensitivity in demand variance, $\\widehat{\\gamma}_h$",
#                               "Holiday marginal service costs, $\\widehat{mc}_h$",
#                               "s.e., holiday marginal service costs, $\\sigma_{mc_{h}}$",
#                               "Non-holiday marginal service costs, $\\widehat{mc}_{nh}$",
#                               "s.e., non-holiday marginal service costs, $\\sigma_{mc_{nh}}$",
#                               "Ratio of marginal cost price ratio, $\\widehat{R}_c \\triangleq \\frac{\\widehat{mc}_{h}}{p_h}/\\frac{\\widehat{mc}_{nh}}{p_{nh}}$",
#                               "s.e., Ratio of marginal cost price ratio, $\\sigma_{R_c}$",
#                               "Holidays redemption ratio, $\\widehat{r}$",
#                               "s.e., holidays redemption ratio, $\\sigma_{r}$",
#                               "Holiday discount demand, $\\widehat{D}_{r,h}$",
#                               "s.e., holiday discount demand, $\\sigma_{D_{r,h}}$",
#                               "Holiday full price demand, $\\widehat{D}_{nr,h}$",
#                               "s.e., holiday full price demand, $\\sigma_{D_{nr,h}}$",
#                               "Non-holiday full price demand, $\\widehat{D}_{nr,nh}$",
#                               "s.e., non-holiday full price demand, $\\sigma_{D_{nr,nh}}$",
#                               # "Holiday margianl cost price ratio, $\\widehat{\\frac{mc_{h}}{p}}$",
#                               # "$\\sigma_{\\frac{mc_{h}}{p}}$",
#                               "Holiday demand swings, $\\widehat{\\delta}(\\omega')$", 
#                               "s.e., holiday demand swings, $\\sigma_{\\delta(\\omega')}$",
#                               # "Variance reduction index, $\\mathcal{R}$",
#                               "Demand smoothing index, $\\mathcal{B}(\\omega',\\omega)$",
#                               "s.e., demand smoothing index, $\\sigma_{\\mathcal{B}(\\omega',\\omega)}$",
#                               "Percentage total demand change, $\\widehat{\\mathcal{M}}(\\omega,\\omega')$", 
#                               "s.e., percentage total demand change, $\\sigma_{\\mathcal{M}(\\omega,\\omega')}$",
#                               "Total profit change, $\\widehat{\\mathcal{P}}(\\omega,\\omega')$", 
#                               "s.e., total profit change, $\\sigma_{\\mathcal{P}(\\omega,\\omega')}$")


rownames(StructEstTable) <- c("Industry",
                              # "Holiday cost parameter, $c_{h}$","Non-holiday cost parameter, $c_{nh}$",
                              "Discount sensitivity in deal demand allocation, $\\widehat{\\theta}_{\\alpha}$",
                              "$\\sigma_{\\theta_{\\alpha}}$",
                              "Launch time sensitivity in deal demand allocation, $\\widehat{\\theta}_T$",
                              "$\\sigma_{\\theta_T}$",
                              "Duration sensitivity in deal demand allocation, $\\widehat{\\theta}_d$",
                              "$\\sigma_{\\theta_d}$",
                              "Duration sensitivity, holiday full price customers, $\\widehat{\\beta}_d$",
                              "$\\sigma_{\\beta_d}$",
                              "Discount sensitivity in deal sales volume, $\\widehat{\\theta}_{S,\\alpha}$",
                              "$\\sigma_{\\theta_{S,\\alpha}}$",
                              "Launch time sensitivity in deal sales volume, $\\widehat{\\theta}_{S,T}$",
                              "$\\sigma_{\\theta_{S,T}}$",
                              "Duration sensitivity in deal sales volume, $\\widehat{\\theta}_{S,d}$",
                              "$\\sigma_{\\theta_{S,d}}$",
                              # "Duration sensitivity, non-holiday full price customers, $\\beta_{d,nh}$",
                              # "Discount sensitivity, non-holiday full price customers,$\\beta_{\\alpha,nh}$",
                              "Baseline holiday demand variance, $\\widehat{\\sigma}_h$",
                              "$\\sigma_{\\sigma_h}$",
                              "Launch time sensitivity in demand variance, $\\widehat{\\gamma}_h$",
                              "$\\sigma_{\\gamma_h}$",
                              "Holiday marginal service costs, $\\widehat{mc}_h$",
                              "$\\sigma_{mc_{h}}$",
                              "Non-holiday marginal service costs, $\\widehat{mc}_{nh}$",
                              "$\\sigma_{mc_{nh}}$",
                              "Ratio of marginal cost price ratio, $\\widehat{R}_c \\triangleq \\frac{\\widehat{mc}_{h}}{p_h}/\\frac{\\widehat{mc}_{nh}}{p_{nh}}$",
                              "$\\sigma_{R_c}$",
                              "Holidays redemption ratio, $\\widehat{r}$",
                              "$\\sigma_{r}$",
                              "Holiday discount demand, $\\widehat{D}_{r,h}$",
                              "$\\sigma_{D_{r,h}}$",
                              "Holiday full price demand, $\\widehat{D}_{nr,h}$",
                              "$\\sigma_{D_{nr,h}}$",
                              "Non-holiday full price demand, $\\widehat{D}_{nr,nh}$",
                              "$\\sigma_{D_{nr,nh}}$",
                              # "Holiday margianl cost price ratio, $\\widehat{\\frac{mc_{h}}{p}}$",
                              # "$\\sigma_{\\frac{mc_{h}}{p}}$",
                              "Holiday demand swings, $\\widehat{\\delta}(\\omega')$", 
                              "$\\sigma_{\\delta(\\omega')}$",
                              # "Variance reduction index, $\\mathcal{R}$",
                              "Demand smoothing index, $\\widehat{\\mathcal{B}}(\\omega^*,\\omega')$",
                              "$\\sigma_{\\mathcal{B}(\\omega^*,\\omega')}$",
                              "Percentage total demand change, $\\widehat{\\mathcal{M}}(\\omega^*,\\omega')$", 
                              "$\\sigma_{\\mathcal{M}(\\omega^*,\\omega')}$",
                              "Total profit change, $\\widehat{\\mathcal{P}}(\\omega^*,\\omega')$", 
                              "$\\sigma_{\\mathcal{P}(\\omega^*,\\omega')}$")


print(xtable(StructEstTable[,c(4,1,2,5,3)]),#tabular.environment = "longtable", 
      only.contents=TRUE, include.rownames=T, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      file = 'table.tex')



# end of S_alpha, S_T, S_d  --------

#  ------- End of displaying the estimators -------------

# 
View(bs_outputs_compile[c(1,14,23,25,32,40,64,66),]%>%mutate(mcratio = mucp/mucpnh)%>%
       select(type, dsidx,dsidx2,mcratio,mucp,alterD,alterP,deltaDvec))

#69/66, 38/37
View(bs_outputs_compile[c(1,13,23,38,66),]%>%mutate(mcratio = mucp/mucpnh)%>%
       select(type,mch,thetaalphahat,thetaThat,thetadhat,betadhat,dsidx,dsidx2,mcratio,mucp,alterD,alterP,deltaDvec))

# pt_est <- (bs_outputs_compile[c(1,13,23,38,66),]%>%mutate(mcratio = mucp/mucpnh)%>%
#              select(type,thetaalphahat,thetaThat,thetadhat,betadhat,dsidx,dsidx2,mcratio,mucp,alterD,alterP,deltaDvec))
# save(pt_est, file = "point_estimator.Rdata")


# View(bs_outputs_compile%>%mutate(mcratio = mucp/mucpnh)%>%
#        select(type, dsidx,dsidx2,mcratio,mucp,alterD,alterDsigvec,alterP,deltaDvec,mase_dfremain,funvalue,D1h,D2h,mch))



# mean estimators and the standard deviation -----




# demand-boosting ---- 
flattendemand_df <- rbind(data.frame(type = 1,dsidx2 = compiled_results_ls[[1]]$dsidx2[[29]]),
                          data.frame(type = 2,dsidx2 = compiled_results_ls[[2]]$dsidx2[[1]]),
                          data.frame(type = 5,dsidx2 = compiled_results_ls[[5]]$dsidx2[[5]]),
                          data.frame(type = 10,dsidx2 = compiled_results_ls[[10]]$dsidx2[[6]]),
                          data.frame(type = 11,dsidx2 = compiled_results_ls[[11]]$dsidx2[[8]]))
boostdemand_df <- rbind(data.frame(type = 1,changeind = compiled_results_ls[[1]]$dalter[[29]]),
                          data.frame(type = 2,changeind = compiled_results_ls[[2]]$dalter[[1]]),
                          data.frame(type = 5,changeind = compiled_results_ls[[5]]$dalter[[5]]),
                          data.frame(type = 10,changeind = compiled_results_ls[[10]]$dalter[[6]]),
                          data.frame(type = 11,changeind = compiled_results_ls[[11]]$dalter[[8]]))



plotdata <- rbind(data.frame(type = 1,totald = pmax(compiled_results_ls[[1]]$Dtotal[[26]],0),
                             totaldpri = compiled_results_ls[[1]]$Dtotalpri[[26]],
                             dalter = compiled_results_ls[[1]]$dalter[[26]]),
                  # data.frame(type = 2,totald = compiled_results_ls[[2]]$Dtotal[[1]][compiled_results_ls[[2]]$D2nhvalpri[[1]]<quantile(compiled_results_ls[[2]]$D2nhvalpri[[1]],0.6)],
                  #            totaldpri = compiled_results_ls[[2]]$Dtotalpri[[1]][compiled_results_ls[[2]]$D2nhvalpri[[1]]<quantile(compiled_results_ls[[2]]$D2nhvalpri[[1]],0.6)],
                  #            dalter = compiled_results_ls[[2]]$dalter[[1]][compiled_results_ls[[2]]$D2nhvalpri[[1]]<quantile(compiled_results_ls[[2]]$D2nhvalpri[[1]],0.6)]),
                  data.frame(type = 2,totald = compiled_results_ls[[2]]$Dtotal[[1]][compiled_results_ls[[2]]$Dtotalpri[[1]]<quantile(compiled_results_ls[[2]]$Dtotalpri[[1]],0.6)&
                                                                                      compiled_results_ls[[2]]$Dtotalpri[[1]]>quantile(compiled_results_ls[[2]]$Dtotalpri[[1]],0.1) ],
                             totaldpri = compiled_results_ls[[2]]$Dtotalpri[[1]][compiled_results_ls[[2]]$Dtotalpri[[1]]<quantile(compiled_results_ls[[2]]$Dtotalpri[[1]],0.6)&
                                                                                   compiled_results_ls[[2]]$Dtotalpri[[1]]>quantile(compiled_results_ls[[2]]$Dtotalpri[[1]],0.1)],
                             dalter = compiled_results_ls[[2]]$dalter[[1]][compiled_results_ls[[2]]$Dtotalpri[[1]]<quantile(compiled_results_ls[[2]]$Dtotalpri[[1]],0.6)&
                                                                             compiled_results_ls[[2]]$Dtotalpri[[1]]>quantile(compiled_results_ls[[2]]$Dtotalpri[[1]],0.1)]),
                  data.frame(type = 5,totald =pmax(compiled_results_ls[[5]]$Dtotal[[5]],0),
                             totaldpri = compiled_results_ls[[5]]$Dtotalpri[[5]],
                             dalter = compiled_results_ls[[5]]$dalter[[5]]),
                  data.frame(type = 10,totald = pmax(compiled_results_ls[[10]]$Dtotal[[6]],0),
                             totaldpri = compiled_results_ls[[10]]$Dtotalpri[[6]],
                             dalter = compiled_results_ls[[10]]$dalter[[6]]),
                  data.frame(type = 11,totald = pmax(compiled_results_ls[[11]]$Dtotal[[8]],0),
                             totaldpri = compiled_results_ls[[11]]$Dtotalpri[[8]],
                             dalter = compiled_results_ls[[11]]$dalter[[8]]))%>%
  group_by(type)%>%
  mutate(deltad = totald - totaldpri)%>%
  mutate(q90 = quantile(dalter,0.80),q10 = quantile(dalter,0.2))%>%
  filter(dalter<q90,dalter>q10)%>%
  merge(data.frame(type = c(2,5,11,10,1),
                   Industry = c("Casual Dining","Fine Dining","Photography","Outdoor","Body Care")),by="type",all.x = TRUE)



colpats <- c("Total Demand (Deals)"= "#66c2a5",
             "Total Demand (No Deals)" = "#fc8d62")

ggplot(data=plotdata%>%filter())+
  geom_histogram(aes(x=totaldpri,y = ..density..,fill="Total Demand (No Deals)"),color="grey30",bins = 50)+
  #geom_line(aes(x=totaldpri,y = ..density..), stat = 'density', colour = '#fc8d62') +
  geom_histogram(aes(x=totald,y = ..density..,fill = "Total Demand (Deals)"),color="grey30",bins = 50)+ #blueish
  # geom_line(aes(x=totaldpri,y = ..density..), stat = 'density', colour = '#fc8d62') 
  #geom_line(aes(x=totald,y = ..density..), stat = 'density', colour = '#66c2a5') +
  facet_grid(.~Industry,scales = "free")+
  scale_fill_manual(values = colpats)+
  labs(x="Total Demand (Units)", y = "", fill="")+
  theme(legend.position = "bottom")+
  coord_flip() -> p1

ggplot(data=plotdata)+
  geom_histogram(aes(x=deltad,y = ..density..),fill="grey60",color="grey30",bins = 100)+
  facet_grid(.~Industry,scales = "free")+
  geom_vline(xintercept=0,linetype=2)+
  labs(x="Change in Demand (Units)", y = "")+
  coord_flip() -> p2

grid.arrange(p1,p2,nrow=2) 
mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         mylegend, 
                         nrow=2,heights=c(16, 2)),
             p2 + theme(legend.position="none"),nrow=2) -> p_demandboosting

# box plots for change in demand  ------

ggplot(data=plotdata %>% filter(deltad>-1000))+
  geom_boxplot(aes(x=Industry,y=deltad))+
  # facet_grid(.~Industry,scales = "free")+
  theme_bw(base_size = 18)+
  labs(y="Change in Demand (Units)", x = "Industry") -> demanboosting_boxplot



library(snpar)
cdf_data <- c(0,0,0)
for(i in industries_to_check){
  plotdata_i <- plotdata %>% fl(type ==i)
  kde(plotdata_i$dalter, h = bw.nrd0(plotdata_i$dalter)/2,kernel = c("gaus")) -> cdf_plot
  
  cdf_data <- rbind(cdf_data,data.frame(Fhat = cdf_plot$Fhat, x = cdf_plot$x, type = i) )
  
}
cdf_data <- cdf_data[-1,]


colpats <- c("Demand Increase"= "#66c2a5",
             "Demand Valley" = "#fc8d62")

ggplot(cdf_data %>% #fl(type!=2) %>% 
         mt(DistType=ifelse(type%in%c(2,5),"Demand Increase","Demand Valley")), 
       aes( x = x, y = Fhat,
            color=DistType, group=as.factor(type)))+
  geom_line(aes(colour = DistType,group=as.factor(type)))+
  # geom_hline(yintercept = 0,  color="#993333")+
  #  geom_errorbar(aes(ymin=perc_cust_left_mean-1.96*perc_cust_left_sd, ymax=perc_cust_left_mean+1.96*perc_cust_left_sd), width=0.1, colour = 'blue')+
  scale_color_manual(values = colpats)+
  # xlim(c(-500,500))+
  # theme(
  #   axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  #   axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
  #   panel.grid.minor.y= element_line(colour = "grey", linetype = "dotted"))+
  # # scale_y_continuous(breaks = seq(0.3, 0.6, by = 0.05))+
  labs(title = "", x = "Percentage of Demand Boosting", y = "Cumulated Percentage of Firms",color ="")+
  theme(plot.title = element_text(hjust = 0.8))+
  scale_y_continuous(labels = scales::percent)+
  # theme(panel.grid.minor.y= element_line(colour = "grey", linetype = "dotted"))+
  theme(legend.position = "bottom",legend.title = element_blank()) -> cdf_dalter

colpats <- c("Demand Increase"= "grey10",
             "Demand Valley" = "grey70")

ggplot(cdf_data %>% #fl(type!=2) %>% 
         mt(DistType=ifelse(type%in%c(2,5),"Demand Increase","Demand Valley")), 
       aes( x = x, y = Fhat, 
            color=DistType, group=as.factor(type)))+
  scale_color_manual(values = colpats)+
  geom_line(aes(colour = DistType,group=as.factor(type)))+
  labs(title = "", x = "Percentage of Demand Boosting", y = "Cumulated Percentage of Firms",color ="")+
  theme_bw(base_size = 18) +
  theme(legend.position = "top",
        legend.background = element_rect(fill="grey80", size=0.5, linetype="solid", color="black"))+
  scale_y_continuous(labels = scales::percent) -> cdf_dalter_bw

mylegend<-g_legend(cdf_dalter_bw)


grid.arrange(demanboosting_boxplot, 
             arrangeGrob(mylegend, cdf_dalter_bw + theme(legend.position="none"),
                         nrow=2, heights = c(3, 18)),
                         ncol=2, widths = c(3,2)) -> demandboosting_bpcdf

grid.arrange(demanboosting_boxplot,cdf_dalter_bw ,
             ncol=2, widths = c(3,2)) -> demandboosting_bpcdf

ggsave("~/Documents/GitHub/Project-Coupon/demandboosting_bpcdf.pdf",demandboosting_bpcdf,width = 18.17 ,height = 7.16,units = "in")


# demand-smoothing -------

plotdata <- rbind(data.frame(type = 1,holgap = compiled_results_ls[[1]]$holgap[[26]],
                             nonholgap = compiled_results_ls[[1]]$nonholgap[[26]],
                             dsidx2 = compiled_results_ls[[1]]$dsidx2[[26]]),
                  data.frame(type = 2,holgap = compiled_results_ls[[2]]$holgap[[1]],
                             nonholgap = compiled_results_ls[[2]]$nonholgap[[1]],
                             dsidx2 = compiled_results_ls[[2]]$dsidx2[[1]]),
                  data.frame(type = 5,holgap =(compiled_results_ls[[5]]$holgap[[5]]),
                             nonholgap = compiled_results_ls[[5]]$nonholgap[[5]],
                             dsidx2 = compiled_results_ls[[5]]$dsidx2[[5]]),
                  data.frame(type = 10,holgap = (compiled_results_ls[[10]]$holgap[[6]]),
                             nonholgap = compiled_results_ls[[10]]$nonholgap[[6]],
                             dsidx2 = compiled_results_ls[[10]]$dsidx2[[6]]),
                  data.frame(type = 11,holgap = (compiled_results_ls[[11]]$holgap[[8]][compiled_results_ls[[11]]$dsidx2[[8]]<quantile(compiled_results_ls[[11]]$dsidx2[[8]],0.8)&
                                                   compiled_results_ls[[11]]$dsidx2[[8]]>quantile(compiled_results_ls[[11]]$dsidx2[[8]],0.2)]),
                             nonholgap = compiled_results_ls[[11]]$nonholgap[[8]][compiled_results_ls[[11]]$dsidx2[[8]]<quantile(compiled_results_ls[[11]]$dsidx2[[8]],0.8)&
                                                                                    compiled_results_ls[[11]]$dsidx2[[8]]>quantile(compiled_results_ls[[11]]$dsidx2[[8]],0.2)],
                             dsidx2 = compiled_results_ls[[11]]$dsidx2[[8]][compiled_results_ls[[11]]$dsidx2[[8]]<quantile(compiled_results_ls[[11]]$dsidx2[[8]],0.8)&
                                                                              compiled_results_ls[[11]]$dsidx2[[8]]>quantile(compiled_results_ls[[11]]$dsidx2[[8]],0.2)]))%>%
  group_by(type)%>%
  # mutate(smoothed = ifelse(type!=2,holval - nonholval, (nonholval - holval)))%>%
  mutate(smoothed = (holgap - nonholgap))%>%
  mutate(q90 = quantile(dsidx2,0.8),q10 = quantile(dsidx2,0.2))%>%
  filter(dsidx2<q90,dsidx2>q10)%>%
  merge(data.frame(type = c(2,5,11,10,1),
                   Industry = c("Casual Dining","Fine Dining","Photography","Outdoor","Body Care")),by="type",all.x = TRUE)



# tmp <- plotdata%>%filter(type==2)
# dsidx2_adj[[2]] <- (tmp$nonholval - tmp$holval)/abs(tmp$holval)

# 
# plotdata <- data.frame(holval =holval, nonholval = nonholval, type = typei)%>%
#   group_by(type)%>%
#   mutate(smoothed = ifelse(type!=2,holval - nonholval, ((nonholval) - holval)/holval))%>%
#   filter(type==2,nonholval>0,holval>0)%>%summarise(mean(smoothed))


colpats <- c("Demand Gap (Deals)"= "#66c2a5",
             "Demand Gap (No Deals)" = "#fc8d62")

ggplot(data=plotdata%>%filter())+
  geom_histogram(aes(x=nonholgap,y = ..density..,fill="Demand Gap (No Deals)"),color="grey30",bins = 50)+
  #geom_line(aes(x=totaldpri,y = ..density..), stat = 'density', colour = '#fc8d62') +
  geom_histogram(aes(x=holgap,y = ..density..,fill = "Demand Gap (Deals)"),color="grey30",bins = 50)+ #blueish
  # geom_line(aes(x=totaldpri,y = ..density..), stat = 'density', colour = '#fc8d62') 
  #geom_line(aes(x=totald,y = ..density..), stat = 'density', colour = '#66c2a5') +
  facet_grid(.~Industry,scales = "free")+
  scale_fill_manual(values = colpats)+
  labs(x="Demand Gap: Holiday-Non-holiday (Units Per Day)", y = "", fill="")+
  theme(legend.position = "bottom")+
  coord_flip() -> p1

ggplot(data=plotdata)+
  geom_histogram(aes(x=smoothed,y = ..density..),fill="grey60",color="grey30",bins = 100)+
  facet_grid(.~Industry,scales = "free")+
  labs(x="Change in Demand Gap (Units Per Day)", y = "")+
  geom_vline(xintercept=0,linetype =2)+
  coord_flip() -> p2

# box plots for smoothed  ------

ggplot(data=plotdata )+
  geom_boxplot(aes(x=Industry,y=smoothed))+
  # facet_grid(.~Industry,scales = "free")+
  theme_bw(base_size = 18)+
  labs(y="Change in Demand Gap (Units Per Day)", x = "Industry") -> demandsmoothing_boxplot



grid.arrange(p1,p2,nrow=2)
mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         mylegend, 
                         nrow=2,heights=c(16, 2)),
             p2 + theme(legend.position="none"),nrow=2) -> p_demandsmoothing


cdf_data <- c(0,0,0)
for(i in c(2,5,11,10,1)){
  plotdata_i <- plotdata %>% fl(type ==i)
  kde(plotdata_i$dsidx2, h = bw.nrd0(plotdata_i$dsidx2)/2,kernel = c("gaus")) -> cdf_plot
  
  cdf_data <- rbind(cdf_data,data.frame(Fhat = cdf_plot$Fhat, x = cdf_plot$x, type = i) )
  
}
cdf_data <- cdf_data[-1,]

colpats <- c("Large Marginal Cost Surge"= "#66c2a5",
             "Low Marginal Cost Surge" = "#fc8d62")

ggplot(cdf_data %>% #fl(type!=2) %>% 
         mt(DistType=ifelse(type%in%c(1,5,10),"Large Marginal Cost Surge","Low Marginal Cost Surge")), 
       aes( x = x, y = Fhat,
            color=DistType, group=as.factor(type)))+
  geom_line(aes(colour =DistType,group=as.factor(type)))+
  # geom_hline(yintercept = 0,  color="#993333")+
  #  geom_errorbar(aes(ymin=perc_cust_left_mean-1.96*perc_cust_left_sd, ymax=perc_cust_left_mean+1.96*perc_cust_left_sd), width=0.1, colour = 'blue')+
  scale_color_manual(values = colpats)+
  scale_y_continuous(labels = scales::percent)+
  # xlim(c(-500,500))+
  # theme(
  #   axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  #   axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
  #   panel.grid.minor.y= element_line(colour = "grey", linetype = "dotted"))+
  # # scale_y_continuous(breaks = seq(0.3, 0.6, by = 0.05))+
  labs(title = "", x = "Percentage of Demand Flattening", y = "Cumulated Percentage of Firms",color="")+
  theme(legend.position = "bottom",legend.title = element_blank())-> cdf_smoothed

colpats <- c("Large Marginal Cost Surge"= "grey10",
             "Low Marginal Cost Surge" = "grey70")

ggplot(cdf_data %>% #fl(type!=2) %>% 
         mt(DistType=ifelse(type%in%c(1,5,10),"Large Marginal Cost Surge","Low Marginal Cost Surge")), 
       aes( x = x, y = Fhat,
            color=DistType, group=as.factor(type)))+
  geom_line(aes(colour =DistType,group=as.factor(type)))+
  scale_color_manual(values = colpats)+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "", x = "Percentage of Demand Flattening", y = "Cumulated Percentage of Firms",color="")+
  theme_bw(base_size = 18)+
  theme(legend.position = "top",
        legend.background = element_rect(fill="grey80", size=0.5, linetype="solid", color="black")) -> cdf_smoothed_bw


grid.arrange(demandsmoothing_boxplot,cdf_smoothed_bw ,
             ncol=2, widths = c(3,2)) -> demandsmoothing_bpcdf


ggsave("~/Documents/GitHub/Project-Coupon/demandsmoothing_bpcdf.pdf",demandsmoothing_bpcdf,width = 18.17 ,height = 7.16,units = "in")


grid.arrange(cdf_dalter,cdf_smoothed,ncol=2)

grid.arrange(p_demandboosting,cdf_dalter,ncol=2,widths = c(2.5,1))
grid.arrange(p_demandsmoothing,cdf_smoothed,ncol=2,widths = c(2.5,1))
mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         mylegend, 
                         nrow=2,heights=c(16, 2)),
             p2 + theme(legend.position="none"),nrow=2) 


# the four quadrants ---------
figures_df <- bs_outputs_compile[c(1,13,23,38,66),]%>%mutate(mcratio = mucp/mucpnh)
figures_df <- bs_outputs_compile[c(1,16,23,38,66),]%>%mutate(mcratio = mucp/mucpnh)



industrynames <- dealsfull%>%filter(type %in% industries_to_check)%>%select(type,detailed_cat_price)%>%unique()
catnames <- data.frame(type = industries_to_check,
                       Industry = c("Casual Dine", "Fine Dine", "Photography", "Outdoor", "Body Care"))
plotdata <- merge(merge(industrynames, 
                        data.frame(type = industries_to_check, figures_df),
                        by="type"),catnames,by="type")%>%
  mutate(alterlabel = (Industry == "Fine Dine"))%>%{.->>plottmp}%>%
  mutate(order_alterD = 6-rank(plottmp$alterD), order_dsidx = rank(abs(plottmp$dsidx2)))%>%
  select(alterlabel,type, Industry,detailed_cat_price, mucp, mcratio, deltaD, alterD, dsidx2, order_alterD, order_dsidx)


ggplot(data=plotdata,aes(y=mcratio,x=deltaD))+geom_point(aes(size=order_dsidx,color=order_alterD))+
  #geom_errorbarh(aes(xmax=deltad+1*deltadsig,xmin=deltad-1*deltadsig,y=(mcpratio)),size=0.7,height=0.0015) +
  #geom_errorbar(aes(ymax=mcpratio+1*mcpratiosd,ymin=mcpratio-1*mcpratiosd,x=(deltad)),size=0.7,width=0.15) +
  # geom_rect(mapping=aes(xmin=deltaD-1*deltaDsig, xmax=deltaD+1*deltaDsig, ymin=mcratio-1*mcratiosd, ymax=mcratio+1*mcaratiosd), 
  #           fill="grey70", color="grey30",alpha=0.3)+
  geom_label(data=plotdata%>%filter(alterlabel==FALSE),
             aes(label=Industry),vjust = 1.0, hjust = 0.0 , nudge_x = 0.1)+
  geom_label(data=plotdata%>%filter(alterlabel==TRUE),
             aes(label=Industry),vjust = -0.2, hjust = 1.4, nudge_x = 0.1)+
  theme(panel.background = element_rect(fill="grey90",color="grey90"),
        #axis.title.x = element_blank(),axis.title.y=element_blank(),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(size=12,face="bold"),
        legend.justification = c("center", "bottom"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.background = element_rect(size=0.4, linetype="solid", 
                                         colour ="black"),
        plot.margin=unit(c(0.5,1.5,0.5,1.0), "cm"))+
  ylim(1, 8)+
  geom_hline(yintercept=1,linetype=2)+
  geom_vline(xintercept=0,linetype=2)+
  labs(y=TeX("Ratio of Marginal Cost Price Ratio, $R_c$"), 
       x = TeX("Holiday Demand Swing, $\\widehat{\\delta}(\\omega')$"),
       size=TeX("Mean-flattening (Order), $\\textit{B}(\\omega^*,\\omega')$"),
       color = TeX("Mean-boosting (Order), $\\textit{M}(\\omega^*,\\omega')$"))+
  theme(legend.text = element_text(size=10),legend.title = element_text(size=10),
        legend.background = element_rect(color = NA),
        legend.box = "horizontal",
        legend.position = "bottom")+
  scale_color_continuous(low = "black", high = "grey70")


# 




# the deal parameteres ---------
obs_deal_parameters <- c(0,0,0,0,0)
obs_deal_parameters_ls <- vector(mode="list",length=11)
for(i in industries_to_check){
  
  if(i == 2){
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3),prep.period>0,offering_duration<40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.4)), total_volume<quantile(dealsfull_type$total_volume,c(0.6)))%>%
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
  
  
  obs_deal_parameters <- rbind(obs_deal_parameters,data.frame(type = i, trimeddf[[i]][[1]][,4:6],
                                                              total_volume = hol_data$total_volume[-jidx_ls[[i]][[1]]],
                                                              original_price = sdop$original_price[-jidx_ls[[i]][[1]]]))
  
  obs_deal_parameters_ls[[i]] <- data.frame(trimeddf[[i]][[1]],
                                                              total_volume = hol_data$total_volume[-jidx_ls[[i]][[1]]],
                                                              original_price = sdop$original_price[-jidx_ls[[i]][[1]]])
  
  colnames(obs_deal_parameters_ls[[i]]) <- c("discount","prep.period","offering_duration",
                                             "discount","prep.period","offering_duration",
                                             "total_volume", "original_price")
  
}
obs_deal_parameters <- obs_deal_parameters[-1,]

obs_deal_parameters <- obs_deal_parameters %>% 
  mutate(volumebins = total_volume - (total_volume%%20))

ggplot(obs_deal_parameters%>%filter(total_volume<200)) + 
  geom_point(aes(x=as.factor(type),y=prep.period))+
  facet_wrap(.~volumebins)

# alpha --------
merge(merge(merge(merge(obs_deal_parameters_ls[[10]][,c(4,5,6,7,8)]%>%
                          mutate(volumebins = total_volume - (total_volume%%40))%>%
                          mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                          mutate(preptbins = prep.period - (prep.period%%3))%>%
                          mutate(pricebins = original_price - (original_price%%50))%>%
                          mutate(alphabins = discount - (discount%%0.2))%>%
                          rename(disc10 = discount, T10 = prep.period),
                        obs_deal_parameters_ls[[2]][,c(4,5,6,7,8)]%>%
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
                  obs_deal_parameters_ls[[11]][,c(4,5,6,7,8)]%>%
                    mutate(volumebins = total_volume - (total_volume%%40))%>%
                    mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                    mutate(preptbins = prep.period - (prep.period%%3))%>%
                    mutate(pricebins = original_price - (original_price%%50))%>%
                    mutate(alphabins = discount - (discount%%0.2))%>%
                    rename(disc11 = discount, T11 = prep.period),
                  by=c("preptbins","volumebins"),all=TRUE)%>%
              select(volumebins,preptbins, disc11,disc2,disc10),
            obs_deal_parameters_ls[[1]][,c(4,5,6,7,8)]%>%
              mutate(volumebins = total_volume - (total_volume%%40))%>%
              mutate(durationbins = offering_duration - (offering_duration%%3))%>%
              mutate(preptbins = prep.period - (prep.period%%3))%>%
              mutate(pricebins = original_price - (original_price%%50))%>%
              mutate(alphabins = discount - (discount%%0.2))%>%
              rename(disc1 = discount, T1 = prep.period),
            by=c("preptbins","volumebins"),all=TRUE)%>%
        select(volumebins,preptbins, disc11,disc2,disc10,disc1),
      obs_deal_parameters_ls[[5]][,c(4,5,6,7,8)]%>%
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
  filter(is.nan(disc10mu)+is.nan(disc2mu)+is.nan(disc11mu)+is.nan(disc1mu)+is.nan(disc5mu)<4) -> disc_cap_prept

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
  geom_point(aes(x=as.factor(volumebins),y=disc10mu,color = "Outdoor"))+
  geom_point(aes(x=as.factor(volumebins),y=disc2mu,color = "Casual Dine"))+
  geom_point(aes(x=as.factor(volumebins),y=disc11mu,color = "Photography"))+
  geom_point(aes(x=as.factor(volumebins),y=disc5mu,color = "Fine Dine"))+
  geom_point(aes(x=as.factor(volumebins),y=disc1mu,color = "Body Care"))+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc10mu-1*disc10sd, ymax = disc10mu+1*disc10sd,color = "Outdoor",collbs = colpats[1]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc2mu-1*disc2sd, ymax = disc2mu+1*disc2sd,color="Casual Dine",collbs = colpats[1]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc11mu-1*disc11sd, ymax = disc11mu+1*disc11sd,color="Photography",collbs = colpats[2]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc5mu-1*disc5sd, ymax = disc5mu+1*disc5sd,color="Fine Dine",collbs = colpats[2]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc1mu-1*disc1sd, ymax = disc1mu+1*disc1sd,color = "Body Care",collbs = colpats[2]),width=0.1,alpha=0.8)+
  facet_grid(preptbins~.)+
  scale_color_manual(values = colpats)+
  labs(x="Capacity",y="Discount", color="Industry", facet="Release Date to Holiday")+
  theme(legend.position = "bottom",legend.title = element_blank()) -> palpha


# Launch day to holiday  -------

merge(merge(merge(merge(obs_deal_parameters_ls[[10]][,c(4,5,6,7,8)]%>%
                          filter(offering_duration<40)%>%
                          mutate(volumebins = total_volume - (total_volume%%40))%>%
                          mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                          mutate(preptbins = prep.period - (prep.period%%3))%>%
                          mutate(pricebins = original_price - (original_price%%50))%>%
                          mutate(alphabins = discount - (discount%%0.2))%>%
                          rename(disc10 = discount, T10 = prep.period),
                        obs_deal_parameters_ls[[2]][,c(1,2,3,7,8)]%>%
                          filter(offering_duration<40)%>%
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
                  obs_deal_parameters_ls[[11]][,c(4,5,6,7,8)]%>%
                    filter(offering_duration<40)%>%
                    mutate(volumebins = total_volume - (total_volume%%40))%>%
                    mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                    mutate(preptbins = prep.period - (prep.period%%3))%>%
                    mutate(pricebins = original_price - (original_price%%50))%>%
                    mutate(alphabins = discount - (discount%%0.2))%>%
                    rename(disc11 = discount, T11 = prep.period),
                  by=c("alphabins","volumebins"),all=TRUE)%>%
              select(volumebins,alphabins, T11,T2,T10),
            obs_deal_parameters_ls[[1]][,c(4,5,6,7,8)]%>%
              filter(offering_duration<40)%>%
              mutate(volumebins = total_volume - (total_volume%%40))%>%
              mutate(durationbins = offering_duration - (offering_duration%%3))%>%
              mutate(preptbins = prep.period - (prep.period%%3))%>%
              mutate(pricebins = original_price - (original_price%%50))%>%
              mutate(alphabins = discount - (discount%%0.2))%>%
              rename(disc1 = discount, T1 = prep.period),
            by=c("alphabins","volumebins"),all=TRUE)%>%
        select(volumebins,alphabins, T11,T2,T10,T1),
      obs_deal_parameters_ls[[5]][,c(1,2,3,7,8)]%>%
        filter(offering_duration<40)%>%
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
  filter(is.nan(T10mu)+is.nan(T2mu)+is.nan(T11mu)+is.nan(T1mu)+is.nan(T5mu)<3) -> T_cap_prept


colpats <- c("#66c2a5",
             "#fc8d62",
             "#8da0cb",
             "#e78ac3",
             "#a6d854")

colpats <- c("Outdoor"= "#66c2a5",
             "Casual Dine" = "#66c2a5",
             "Photography"= "#66c2a5",
             "Fine Dine" = "#fc8d62",
             "Body Care"= "#fc8d62")

collabs <- c("Late Release"= "#66c2a5",
             "Early Release" = "#fc8d62")

ggplot(data=T_cap_prept)+
  geom_point(aes(x=as.factor(volumebins),y=T10mu,color = "Outdoor"))+
  geom_point(aes(x=as.factor(volumebins),y=T2mu,color = "Casual Dine"))+
  geom_point(aes(x=as.factor(volumebins),y=T11mu,color = "Photography"))+
  geom_point(aes(x=as.factor(volumebins),y=T5mu,color = "Fine Dine"))+
  geom_point(aes(x=as.factor(volumebins),y=T1mu,color = "Body Care"))+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T10mu-0.5*T10sd, ymax = T10mu+0.5*T10sd,color = "Outdoor",collabs="Late Release"),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T2mu-0.5*T2sd, ymax = T2mu+0.5*T2sd,color = "Casual Dine",collabs="Early Release"),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T11mu-0.5*T11sd, ymax = T11mu+0.5*T11sd,color = "Photography",collabs="Early Release"),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T5mu-0.5*T5sd, ymax = T5mu+0.5*T5sd,color = "Fine Dine",collabs="Late Release"),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = T1mu-0.5*T1sd, ymax = T1mu+0.5*T1sd,color = "Body Care",collabs="Early Release"),width=0.1,alpha=0.8)+
  facet_grid(alphabins~.)+
  scale_color_manual(values = colpats)+
  labs(x="Capacity",y="Launch Date to Holiday", color="Industry", facet="Discount")+
  theme(legend.position = "bottom",legend.title = element_blank()) -> pT
# can add in another metric, the % of cover or higher than the other

x <- seq(4.1,50,0.1)
K <- 0.8
y1 <- (1+0.88*K)*(x/(1+x)) - (x/(1+x))
y2 <- (1+6.33*K)*((x-3.74)/(1+x-3.74)) - (x/(1+x))

ggplot()+
  geom_line(aes(x=x,y=y1),color="red")+
  geom_line(aes(x=x,y=y2),color="blue")


# duration  ----------
merge(merge(merge(merge(obs_deal_parameters_ls[[10]][,c(4,5,6,7,8)]%>%
                          #filter(offering_duration<40)%>%
                          mutate(offering_duration = offering_duration-prep.period)%>%
                          mutate(volumebins = total_volume - (total_volume%%40))%>%
                          mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                          mutate(preptbins = prep.period - (prep.period%%3))%>%
                          mutate(pricebins = original_price - (original_price%%50))%>%
                          mutate(alphabins = discount - (discount%%0.2))%>%
                          rename(disc10 = discount, T10 = prep.period, d10 = offering_duration),
                        obs_deal_parameters_ls[[2]][,c(1,2,3,7,8)]%>%
                          #filter(offering_duration<40)%>%
                          mutate(offering_duration = offering_duration-prep.period)%>%
                          mutate(volumebins = total_volume - (total_volume%%40))%>%
                          mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                          mutate(preptbins = prep.period - (prep.period%%3))%>%
                          mutate(pricebins = original_price - (original_price%%50))%>%
                          mutate(alphabins = discount - (discount%%0.2))%>%
                          rename(disc2 = discount, T2 = prep.period, d2 = offering_duration),
                        by=c("alphabins","volumebins"),all=TRUE)%>%
                    #     group_by(volumebins,preptbins)%>%
                    #     summarise(disc10mu=mean(disc10,na.rm = TRUE),disc2mu=mean(disc2,na.rm = TRUE),
                    #               disc10sd=sd(disc10,na.rm = TRUE),disc2sd=sd(disc2,na.rm = TRUE))%>%
                    select(volumebins,alphabins,d2,d10),
                  obs_deal_parameters_ls[[11]][,c(4,5,6,7,8)]%>%
                   # filter(offering_duration<40)%>%
                    mutate(offering_duration = offering_duration-prep.period)%>%
                    mutate(volumebins = total_volume - (total_volume%%40))%>%
                    mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                    mutate(preptbins = prep.period - (prep.period%%3))%>%
                    mutate(pricebins = original_price - (original_price%%50))%>%
                    mutate(alphabins = discount - (discount%%0.2))%>%
                    rename(disc11 = discount, T11 = prep.period, d11 = offering_duration),
                  by=c("alphabins","volumebins"),all=TRUE)%>%
              select(volumebins,alphabins, d11,d2,d10),
            obs_deal_parameters_ls[[1]][,c(4,5,6,7,8)]%>%
             # filter(offering_duration<40)%>%
              mutate(offering_duration = offering_duration-prep.period)%>%
              mutate(volumebins = total_volume - (total_volume%%40))%>%
              mutate(durationbins = offering_duration - (offering_duration%%3))%>%
              mutate(preptbins = prep.period - (prep.period%%3))%>%
              mutate(pricebins = original_price - (original_price%%50))%>%
              mutate(alphabins = discount - (discount%%0.2))%>%
              rename(disc1 = discount, T1 = prep.period, d1 = offering_duration),
            by=c("alphabins","volumebins"),all=TRUE)%>%
        select(volumebins,alphabins, d11,d2,d10,d1),
      obs_deal_parameters_ls[[5]][,c(1,2,3,7,8)]%>%
        #filter(offering_duration<40)%>%
        mutate(offering_duration = offering_duration-prep.period)%>%
        mutate(volumebins = total_volume - (total_volume%%40))%>%
        mutate(durationbins = offering_duration - (offering_duration%%3))%>%
        mutate(preptbins = prep.period - (prep.period%%3))%>%
        mutate(pricebins = original_price - (original_price%%50))%>%
        mutate(alphabins = discount - (discount%%0.2))%>%
        rename(disc5 = discount, T5 = prep.period, d5 = offering_duration),
      by=c("alphabins","volumebins"),all=TRUE)%>%
  select(volumebins,alphabins, d11,d2,d10,d1,d5)%>%
  group_by(alphabins,volumebins)%>%
  summarise(d10mu=mean(d10,na.rm = TRUE),d2mu=mean(d2,na.rm = TRUE),d11mu=mean(d11,na.rm = TRUE),
            d1mu=mean(d1,na.rm = TRUE),d5mu=mean(d5,na.rm = TRUE),
            d10sd = sd(d10,na.rm = TRUE),d2sd = sd(d2,na.rm = TRUE),d11sd=sd(d11,na.rm = TRUE),
            d1sd=sd(d1,na.rm = TRUE),d5sd=sd(d5,na.rm = TRUE))%>%
  filter(is.nan(d10mu)+is.nan(d2mu)+is.nan(d11mu)+is.nan(d1mu)+is.nan(d5mu)<3) -> d_cap_prept

colpats <- c("#66c2a5",
             "#fc8d62",
             "#8da0cb",
             "#e78ac3",
             "#a6d854")

colpats <- c("Outdoor"= "#fc8d62",
             "Casual Dine" = "#fc8d62",
             "Photography"= "#66c2a5",
             "Fine Dine" ="#66c2a5",
             "Body Care"= "#66c2a5")


ggplot(data=d_cap_prept)+
  geom_point(aes(x=as.factor(volumebins),y=d10mu,color = "Outdoor"))+
  geom_point(aes(x=as.factor(volumebins),y=d2mu,color = "Casual Dine"))+
  geom_point(aes(x=as.factor(volumebins),y=d11mu,color = "Photography"))+
  geom_point(aes(x=as.factor(volumebins),y=d5mu,color = "Fine Dine"))+
  geom_point(aes(x=as.factor(volumebins),y=d1mu,color = "Body Care"))+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = d10mu-0.5*d10sd, ymax = d10mu+0.5*d10sd,color = "Outdoor",collbs = colpats[1]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = d2mu-0.5*d2sd, ymax = d2mu+0.5*d2sd,color="Casual Dine",collbs = colpats[1]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = d11mu-0.5*d11sd, ymax = d11mu+0.5*d11sd,color="Photography",collbs = colpats[2]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = d5mu-0.5*d5sd, ymax = d5mu+0.5*d5sd,color="Fine Dine",collbs = colpats[2]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = d1mu-0.5*d1sd, ymax = d1mu+0.5*d1sd,color = "Body Care",collbs = colpats[2]),width=0.1,alpha=0.8)+
  facet_grid(alphabins~.)+
  scale_color_manual(values = colpats)+
  labs(x="Capacity",y="Duration", color="Industry", facet="Discount")+
  theme(legend.position = "bottom",legend.title = element_blank()) -> pd

grid.arrange(palpha,pT,pd,ncol=3)


### data description --------

# t test -------

datadim_h <- vector()
datadim_nh <- vector()
data_h_ls <- vector(mode = "list", length = 11)
data_nh_ls <- vector(mode = "list", length = 11)

predays_t <- 20
pre.windows_t <- c(seq(21-predays_t,21),seq(44-predays_t,44),seq(92-predays_t,92))



for (i in industries_to_check){
  print(i)
  if(i == 2){
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3)) %>% 
      fl(prep.period>0,prep.period<40) %>%{.->>dealsfull_type}%>%
      filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
      {.->>sdop} %>% 
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    
    print(dim(sdop))
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    
    data_nh <- dealsfull%>% filter(type%in%c(2,3)) %>% 
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows_t)%>%{.->>dealsfull_type}%>%{.->>sdop} %>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type%in%c(2)) %>%
    #   {.->>dealsfull_type}%>%
    #   # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.4)), total_volume<quantile(dealsfull_type$total_volume,c(0.6))) %>% 
    #   {.->>sdop} %>% 
    #   filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))

    data_nh_ls[[i]] <- data_nh
    
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
    
    
    
  }else{
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type==i)%>%
      {.->>sdop}%>%{.->>dealsfull_type} %>% 
      fl(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    datadim_h <- c(datadim_h,dim(sdop)[1])

    data_nh <- dealsfull%>%
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows_t)%>%{.->>dealsfull_type}%>%{.->>sdop} %>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
      
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%
    #   {.->>sdop}%>%{.->>dealsfull_type} %>% 
    #   fl(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
  }

  
  print(dim(sdop))
  datasanalysis <- c(datasanalysis,dim(sdop)[1])
  
}


alphat <- vector()
Tt <- vector()
dt <- vector()
salest <- vector()
pt <- vector()

alphaks <- vector()
Tks <- vector()
dks <- vector()
salesks <- vector()
pks <- vector()

for(i in industries_to_check){
  
  alphat <- c(alphat,t.test((1-data_nh_ls[[i]]$discount)*data_nh_ls[[i]]$original_price_sd, 
                            (1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd)$p.value)
  # Tt <- c(Tt,t.test(data_nh_ls[[i]]$prep.period, data_h_ls[[i]]$prep.period)$p.value)
  dt <- c(dt,t.test(data_nh_ls[[i]]$offering_duration, data_h_ls[[i]]$offering_duration)$p.value)
  salest <- c(salest,t.test(data_nh_ls[[i]]$total_volume, data_h_ls[[i]]$total_volume)$p.value)
  pt <- c(pt,t.test(data_nh_ls[[i]]$original_price, data_h_ls[[i]]$original_price)$p.value)
  
  alphaks <- c(alphaks,ks.test((1-data_nh_ls[[i]]$discount)*data_nh_ls[[i]]$original_price_sd, 
                               (1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd)$p.value)
  # Tks <- c(Tks,ks.test(data_nh_ls[[i]]$prep.period, data_h_ls[[i]]$prep.period)$p.value)
  dks <- c(dks,ks.test(data_nh_ls[[i]]$offering_duration, data_h_ls[[i]]$offering_duration)$p.value)
  salesks <- c(salesks,ks.test(data_nh_ls[[i]]$total_volume, data_h_ls[[i]]$total_volume)$p.value)
  pks <- c(pks,ks.test(data_nh_ls[[i]]$original_price, data_h_ls[[i]]$original_price)$p.value)

}


## ks test --------

datadim_h <- vector()
datadim_nh <- vector()
data_h_ls <- vector(mode = "list", length = 11)
data_nh_ls <- vector(mode = "list", length = 11)

predays_t <- 15
pre.windows_t <- c(seq(21-predays_t,21),seq(44-predays_t,44),seq(92-predays_t,92))


for (i in industries_to_check){
  print(i)
  if(i == 2){
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3)) %>% 
      fl(offering_duration<40) %>%
      {.->>dealsfull_type}%>%
      filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
      {.->>sdop} %>% 
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    
    print(dim(sdop))
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    
    data_nh <- dealsfull%>% filter(type%in%c(2,3)) %>% 
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows_t)%>%{.->>dealsfull_type}%>%{.->>sdop} %>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type%in%c(2)) %>%
    #   {.->>dealsfull_type}%>%
    #   # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.4)), total_volume<quantile(dealsfull_type$total_volume,c(0.6))) %>% 
    #   {.->>sdop} %>% 
    #   filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
    
    
    
  }else{
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type==i)%>%
      fl(offering_duration<40) %>%
      {.->>sdop}%>%{.->>dealsfull_type} %>% 
      fl(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    data_nh <- dealsfull%>%
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows_t)%>%{.->>dealsfull_type}%>%{.->>sdop} %>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%
    #   {.->>sdop}%>%{.->>dealsfull_type} %>% 
    #   fl(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
  }
  
  
  print(dim(sdop))
  datasanalysis <- c(datasanalysis,dim(sdop)[1])
  
}


alphat_i <- matrix(rep(0,5^2), ncol= 5,byrow=T)
Tt_i <- matrix(rep(0,5^2), ncol= 5,byrow=T)
dt_i <- matrix(rep(0,5^2), ncol= 5,byrow=T)
alphaks_i <- matrix(rep(0,5^2), ncol= 5,byrow=T)
Tks_i <- matrix(rep(0,5^2), ncol= 5,byrow=T)
dks_i <-  matrix(rep(0,5^2), ncol= 5,byrow=T)

iidx <- 1
jidx <- 1
for(i in industries_to_check){
  jidx <- 1
  for(j in industries_to_check){

    
    alphat_i[iidx,jidx] <- t.test((1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd, 
                    (1-data_h_ls[[j]]$discount)*data_h_ls[[j]]$original_price_sd)$p.value
    
    Tt_i[iidx,jidx]<- t.test(data_h_ls[[i]]$prep.period, data_h_ls[[j]]$prep.period)$p.value
    
    dt_i[iidx,jidx]<- t.test(data_h_ls[[i]]$offering_duration, data_h_ls[[j]]$offering_duration)$p.value
    
    alphaks_i[iidx,jidx] <- ks.test((1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd, 
                          (1-data_h_ls[[j]]$discount)*data_h_ls[[j]]$original_price_sd)$p.value
    
    Tks_i[iidx,jidx] <- ks.test(data_h_ls[[i]]$prep.period, data_h_ls[[j]]$prep.period)$p.value
    
    dks_i[iidx,jidx] <- ks.test(data_h_ls[[i]]$offering_duration, data_h_ls[[j]]$offering_duration)$p.value
    
    jidx <- jidx + 1
  }
  iidx <- iidx +1
  
}


rbind(Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
      #     type = industries_to_check,
      # c = signif(cvec,2),
      # cnh = signif(cnhvec,2),
      alphaks = format(signif(alphaks,2)),
      alphat = format(signif(alphat,2)),
      dks = format(signif(dks,2)),
      dt = format(signif(dt,2)),
      pks = format(signif(pks,2)),
      pt = format(signif(pt,2))) ->  testrestuls

alphaks_i <-  format(signif(alphaks_i,2))
alphat_i <-  format(signif(alphat_i,2))
Tks_i <-  format(signif(Tks_i,2))
Tt_i <-  format(signif(Tt_i,2))
dks_i <-  format(signif(dks_i,2))
dt_i <-  format(signif(dt_i,2))


alphaks_i[lower.tri(alphaks_i,diag=TRUE)] <- NA
alphat_i[lower.tri(alphat_i,diag=TRUE)] <- NA
Tks_i[lower.tri(Tks_i,diag=TRUE)] <- NA
Tt_i[lower.tri(Tt_i,diag=TRUE)] <- NA
dks_i[lower.tri(dks_i,diag=TRUE)] <- NA
dt_i[lower.tri(dt_i,diag=TRUE)] <- NA

# rbind(c(c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
#         c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care")),
#   cbind((alphaks_i),alphat_i),
#       cbind((Tks_i),Tt_i),
#       cbind((dks_i),dt_i)) -> indtestresults
# rownames(indtestresults) <- c("Industry", 
#                               "Casual Dine,  $(1-\\alpha)p$","Fine Dine,  $(1-\\alpha)p$",
#                               "Photography, $(1-\\alpha)p$","Outdoor,  $(1-\\alpha)p$","Body Care,  $(1-\\alpha)p$",
#                               "Casual Dine, $T$","Fine Dine, $T$","Photography, $T$",
#                               "Outdoor, $T$","Body Care, $T$",
#                               "Casual Dine, $d$","Fine Dine, $d$","Photography, $d$",
#                               "Outdoor, $d$","Body Care, $d$")



rbind(c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
      (alphaks_i),alphat_i,
      (Tks_i),Tt_i,
      (dks_i),dt_i) -> indtestresults

rownames(indtestresults) <- c("Industry", 
                              "Casual Dine,  ks test, $(1-\\alpha)p$","Fine Dine,  ks test, $(1-\\alpha)p$",
                              "Photography, ks test, $(1-\\alpha)p$","Outdoor,  ks test, $(1-\\alpha)p$","Body Care,  ks test, $(1-\\alpha)p$",
                              "Casual Dine,  t test, $(1-\\alpha)p$","Fine Dine,  t test, $(1-\\alpha)p$",
                              "Photography, t test, $(1-\\alpha)p$","Outdoor,  t test, $(1-\\alpha)p$","Body Care,  t test, $(1-\\alpha)p$",
                              "Casual Dine, ks test, $T$","Fine Dine, ks test, $T$","Photography, ks test, $T$",
                              "Outdoor, ks test, $T$","Body Care, ks test, $T$",
                              "Casual Dine, t test, $T$","Fine Dine, t test, $T$","Photography, t test, $T$",
                              "Outdoor, t test, $T$","Body Care, t test, $T$",
                              "Casual Dine, ks test, $d$","Fine Dine, ks test, $d$","Photography, ks test, $d$",
                              "Outdoor, ks test, $d$","Body Care, ks test, $d$",
                              "Casual Dine, t test, $d$","Fine Dine, t test, $d$","Photography, t test, $d$",
                              "Outdoor, t test, $d$","Body Care, t test, $d$")


print(xtable(indtestresults),#tabular.environment = "longtable", 
      only.contents=TRUE, include.rownames=T, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      file = 'indtestresultstable1.tex')


# multivariate test, cramer test -------
datadim_h <- vector()
datadim_nh <- vector()
data_h_ls <- vector(mode = "list", length = 11)
data_nh_ls <- vector(mode = "list", length = 11)

predays_t <- 15
pre.windows_t <- c(seq(21-predays_t,21),seq(44-predays_t,44),seq(92-predays_t,92))


for (i in industries_to_check){
  print(i)
  if(i == 2){
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3)) %>% 
      fl(offering_duration<40) %>%
      {.->>dealsfull_type}%>%
      # filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
      {.->>sdop} %>% 
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    
    print(dim(sdop))
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    
    data_nh <- dealsfull%>% filter(type%in%c(2,3)) %>% 
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows_t)%>%{.->>dealsfull_type}%>%{.->>sdop} %>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type%in%c(2)) %>%
    #   {.->>dealsfull_type}%>%
    #   # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.4)), total_volume<quantile(dealsfull_type$total_volume,c(0.6))) %>% 
    #   {.->>sdop} %>% 
    #   filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
    
    
    
  }else{
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type==i)%>%
      fl(offering_duration<40) %>%
      {.->>sdop}%>%{.->>dealsfull_type} %>% 
      fl(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    data_nh <- dealsfull%>%
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows_t)%>%{.->>dealsfull_type}%>%{.->>sdop} %>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%
    #   {.->>sdop}%>%{.->>dealsfull_type} %>% 
    #   fl(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
  }
  
  
  print(dim(sdop))
  datasanalysis <- c(datasanalysis,dim(sdop)[1])
  
}

set.seed(123)
data_h_ls[[11]] <- data_h_ls[[11]][sample(dim(data_h_ls[[11]])[1],200,replace = T),]

library(cramer)
cramertestresult1 <- matrix(rep(0,5^2), ncol= 5,byrow=T)
cramertestresult_ls1 <- vector(mode="list",length=25)
iidx <- 1
jidx <- 1
ct <- 1
for(i in industries_to_check){
  jidx <- iidx+1

  for(j in industries_to_check[(iidx+1):5]){
    
    cramer_holi <- cbind((1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd, data_h_ls[[i]]$prep.period, data_h_ls[[i]]$offering_duration)
    cramer_holj <- cbind((1-data_h_ls[[j]]$discount)*data_h_ls[[j]]$original_price_sd, data_h_ls[[j]]$prep.period, data_h_ls[[j]]$offering_duration)
    
    cramertestresult_ls1[[ct]] <- cramer.test(cramer_holi,cramer_holj,conf.level=0.95,replicates=500,sim="permutation")
    # print(cramertestresult_ls1[[ct]]$p.value)
    cramertestresult1[iidx,jidx] <- cramertestresult_ls1[[ct]]$p.value
    
    jidx <- jidx + 1
    ct <- ct + 1
    print(ct)
  }
  iidx <- iidx +1
}
cramertestresult1[4,5] <- cramer.test(cbind((1-data_h_ls[[10]]$discount)*data_h_ls[[10]]$original_price_sd, data_h_ls[[10]]$prep.period, data_h_ls[[10]]$offering_duration),
                                      cbind((1-data_h_ls[[1]]$discount)*data_h_ls[[1]]$original_price_sd, data_h_ls[[1]]$prep.period, data_h_ls[[1]]$offering_duration),
                                      conf.level=0.95,replicates=500,sim="permutation")$p.value




cramertestresult1 <-  format(signif(cramertestresult1,2))
cramertestresult1[lower.tri(cramertestresult1,diag=TRUE)] <- NA

rbind(c("Casual Dining","Fine Dining","Photography","Outdoor","Body Care"),
      cramertestresult1) -> cramertable

rownames(cramertable) <- c("Industry", 
                              "Casual Dining","Fine Dining",
                              "Photography","Outdoor","Body Care")


print(xtable(cramertable),#tabular.environment = "longtable", 
      only.contents=TRUE, include.rownames=T, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      file = 'cramertable.tex')



rbind(c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
      (alphaks_i),
      (Tks_i),
      (dks_i),
      cramertestresult1)  -> cramer_kstable
rownames(cramer_kstable) <- c("Industry", 
                              "Casual Dining,  $(1-\\alpha)p$","Fine Dining,  $(1-\\alpha)p$",
                              "Photography, $(1-\\alpha)p$","Outdoor,  $(1-\\alpha)p$","Body Care,  $(1-\\alpha)p$",
                              "Casual Dining, $T$","Fine Dining, $T$","Photography, $T$",
                              "Outdoor, $T$","Body Care, $T$",
                              "Casual Dining, $d$","Fine Dining, $d$","Photography, $d$",
                              "Outdoor, $d$","Body Care, $d$",
                           "Casual Dining, Cramer","Fine Dining, Cramer",
                           "Photography, Cramer","Outdoor, Cramer","Body Care, Cramer")

print(xtable(cramer_kstable),#tabular.environment = "longtable", 
      only.contents=TRUE, include.rownames=T, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      file = 'cramer_kstable.tex')



##  multivariate ks test -------
# 
# library(GSAR)
# multikstestresult <- matrix(rep(0,5^2), ncol= 5,byrow=T)
# multikstestresult_ls <- vector(mode="list",length=25)
# iidx <- 1
# jidx <- 1
# ct <- 1
# for(i in industries_to_check){
#   jidx <- iidx+1
#   
#   for(j in industries_to_check[(iidx+1):5]){
#     
#     multiks_holi <- cbind((1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd, data_h_ls[[i]]$prep.period, data_h_ls[[i]]$offering_duration)
#     multiks_holj <- cbind((1-data_h_ls[[j]]$discount)*data_h_ls[[j]]$original_price_sd, data_h_ls[[j]]$prep.period, data_h_ls[[j]]$offering_duration)
#     
#     multikstestresult_ls[[ct]] <- GSAR::KStest(object = cbind(t(multiks_holi),t(multiks_holj)), 
#                                                group = c(rep(1,dim(multiks_holi)[1]),rep(2,dim(multiks_holj)[1])), nperm = 1000)
# 
#     multikstestresult[iidx,jidx] <- multikstestresult_ls[[ct]]$p.value
#     
#     jidx <- jidx + 1
#     ct <- ct + 1
#     print(ct)
#   }
#   iidx <- iidx +1
# }
# cramertestresult1[4,5] <- cramer.test(cbind((1-data_h_ls[[10]]$discount)*data_h_ls[[10]]$original_price_sd, data_h_ls[[10]]$prep.period, data_h_ls[[10]]$offering_duration),
#                                       cbind((1-data_h_ls[[1]]$discount)*data_h_ls[[1]]$original_price_sd, data_h_ls[[1]]$prep.period, data_h_ls[[1]]$offering_duration),
#                                       conf.level=0.95,replicates=500,sim="permutation")$p.value
# 
# 
# 


# plot alpha, t, d in  different industries ----

datadim_h <- vector()
datadim_nh <- vector()
data_h_ls <- vector(mode = "list", length = 11)
data_nh_ls <- vector(mode = "list", length = 11)

predays_t <- 15
pre.windows_t <- c(seq(21-predays_t,21),seq(44-predays_t,44),seq(92-predays_t,92))


for (i in industries_to_check){
  print(i)
  if(i == 2){
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3)) %>% 
      # fl(offering_duration<40) %>%
      {.->>dealsfull_type}%>%
      # filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
      {.->>sdop} %>% 
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    
    print(dim(sdop))
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    
    data_nh <- dealsfull%>% filter(type%in%c(2,3)) %>% 
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows_t)%>%{.->>dealsfull_type}%>%{.->>sdop} %>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type%in%c(2)) %>%
    #   {.->>dealsfull_type}%>%
    #   # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.4)), total_volume<quantile(dealsfull_type$total_volume,c(0.6))) %>% 
    #   {.->>sdop} %>% 
    #   filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
    
    
    
  }else{
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type==i)%>%
      # fl(offering_duration<40) %>% 
      {.->>sdop}%>%{.->>dealsfull_type} %>% 
      fl(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    data_nh <- dealsfull%>%
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows_t)%>%{.->>dealsfull_type}%>%{.->>sdop} %>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%
    #   {.->>sdop}%>%{.->>dealsfull_type} %>% 
    #   fl(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
  }
  
  
  print(dim(sdop))
  datasanalysis <- c(datasanalysis,dim(sdop)[1])
  
}



meanalpha <- vector()
sdalpha <- vector()
meanT <- vector()
sdT <- vector()
meand <- vector()
sdd <- vector()
for(i in industries_to_check){
  
  tmp <-  ((1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd)
  tmp <- tmp[tmp<quantile(tmp, 0.8)&tmp>quantile(tmp, 0.2)]
  meanalpha <- c(meanalpha,mean(((1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd)))
  sdalpha <- c(sdalpha,sd(tmp))
  
  tmp <-  (data_h_ls[[i]]$prep.period)
  tmp <- tmp[tmp<quantile(tmp, 0.65)&tmp>quantile(tmp, 0.35)]
  meanT <- c(meanT,mean( (data_h_ls[[i]]$prep.period)))
  sdT <- c(sdT,sd(tmp))
  
  tmp <- (data_h_ls[[i]]$offering_duration)
  tmp <- tmp[tmp<quantile(tmp, 0.65)&tmp>quantile(tmp, 0.35)]
  meand <- c(meand,mean((data_h_ls[[i]]$offering_duration)))
  sdd <- c(sdd,sd(tmp))
  
}

plotdata <- data.frame(Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
  meanalpha=meanalpha,sdalpha=sdalpha,
                  meanT=meanT,sdT=sdT,
                  meand=meand,sdd=sdd)
ggplot(data = plotdata)+
  geom_point(aes(x=Industry,y=meanalpha),color="grey70")+
  geom_errorbar(aes(x=Industry,ymin=meanalpha-sdalpha,ymax=meanalpha+sdalpha),color="grey70",width=0.2)+
  geom_point(aes(x=Industry,y=meanT),color="grey50")+
  geom_errorbar(aes(x=Industry,ymin=meanT-sdT,ymax=meanT+sdT),color="grey50",width=0.2)+
  geom_point(aes(x=Industry,y=meand),color="grey30")+
  geom_errorbar(aes(x=Industry,ymin=meand-sdd,ymax=meand+sdd),color="grey30",width=0.2)

plotdata <- data.frame(Industry = rep(c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),3),
                       meanValues = c(meanalpha, meanT, meand),
                       sdValues = c(sdalpha, sdT, sdd),
                       Parameter = c(rep("Discount price",5),rep("Launch day to holiday",5),rep("Offering_duration",5)))
ggplot(data = plotdata)+
  geom_point(aes(x=Industry, y = meanValues),width=0.2)+
  geom_errorbar(aes(x=Industry, ymin = meanValues-sdValues, ymax = meanValues+sdValues),width=0.2)+
  facet_grid(Parameter~.,scales = "free")


# placebo test --------
# randomly select 10 days 

set.seed(123)
pseudoholidays <- sample(seq(7,95)[-holidays.doy],10,replace = FALSE)

pseudoholidays_idx_table <- data.frame(holiday_idx = seq(1,length(pseudoholidays),1),
                                 h_first_day = pseudoholidays[order(pseudoholidays)],
                                 h_last_day = pseudoholidays[order(pseudoholidays)])

merge(merge(dealsfull%>% mt(end_dow_dm = launch_dow_dm + offering_duration), pseudoholidays_idx_table,all = TRUE) %>% 
  mt(pseudoholidayind = (launch_dow_dm<=h_last_day)*(end_dow_dm>=h_first_day)) %>%
  mt(prep.periodtmp = h_first_day - launch_dow_dm) %>% mt(prep.periodtmp1 = pmax(prep.periodtmp,0)) %>% 
  gb(event_id) %>% sm(pseudoholiday=sum(pseudoholidayind,na.rm=TRUE)>0, 
                      pre.period = (min(prep.periodtmp1))),dealsfull,by="event_id")  -> dealsfull_pseudo


datadim_h <- vector()
datadim_nh <- vector()
data_h_ls <- vector(mode = "list", length = 11)
data_nh_ls <- vector(mode = "list", length = 11)


for (i in industries_to_check){
  print(i)
  if(i == 2){
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3)) %>% 
      #fl(offering_duration<40) %>%
      {.->>dealsfull_type}%>%
      filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
      {.->>sdop} %>% 
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    
    print(dim(sdop))
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    
    data_nh <- dealsfull_pseudo%>%filter((pseudoholiday)==1,type%in%c(2,3)) %>% 
      # fl(prep.period>0,prep.period<40) %>%
      {.->>dealsfull_type}%>%
      filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
      {.->>sdop} %>% 
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type%in%c(2)) %>%
    #   {.->>dealsfull_type}%>%
    #   # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.4)), total_volume<quantile(dealsfull_type$total_volume,c(0.6))) %>% 
    #   {.->>sdop} %>% 
    #   filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
    
    
    
  }else{
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type==i)%>%
      {.->>sdop}%>%{.->>dealsfull_type} %>% #fl(offering_duration<40) %>%
      fl(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    data_nh <- dealsfull_pseudo%>%filter((pseudoholiday)==1,type==i)%>%
      {.->>sdop}%>%{.->>dealsfull_type} %>% 
      fl(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%
    #   {.->>sdop}%>%{.->>dealsfull_type} %>% 
    #   fl(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
  }
  
  
  print(dim(sdop))
  datasanalysis <- c(datasanalysis,dim(sdop)[1])
  
}


alphat <- vector()
Tt <- vector()
dt <- vector()
salest <- vector()
pt <- vector()

alphaks <- vector()
Tks <- vector()
dks <- vector()
salesks <- vector()
pks <- vector()

for(i in industries_to_check){
  
  alphat <- c(alphat,t.test((1-data_nh_ls[[i]]$discount)*data_nh_ls[[i]]$original_price_sd, 
                            (1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd)$p.value)
  Tt <- c(Tt,t.test(data_nh_ls[[i]]$prep.period, data_h_ls[[i]]$prep.period)$p.value)
  print(t.test(data_nh_ls[[i]]$prep.period, data_h_ls[[i]]$prep.period)$estimate)
  dt <- c(dt,t.test(data_nh_ls[[i]]$offering_duration, data_h_ls[[i]]$offering_duration)$p.value)
  print(t.test(data_nh_ls[[i]]$offering_duration, data_h_ls[[i]]$offering_duration)$estimate)
  salest <- c(salest,t.test(data_nh_ls[[i]]$total_volume, data_h_ls[[i]]$total_volume)$p.value)
  pt <- c(pt,t.test(data_nh_ls[[i]]$original_price, data_h_ls[[i]]$original_price)$p.value)
  
  alphaks <- c(alphaks,ks.test((1-data_nh_ls[[i]]$discount)*data_nh_ls[[i]]$original_price_sd, 
                               (1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd)$p.value)
  Tks <- c(Tks,ks.test(data_nh_ls[[i]]$prep.period, data_h_ls[[i]]$prep.period)$p.value)
  dks <- c(dks,ks.test(data_nh_ls[[i]]$offering_duration, data_h_ls[[i]]$offering_duration)$p.value)
  salesks <- c(salesks,ks.test(data_nh_ls[[i]]$total_volume, data_h_ls[[i]]$total_volume)$p.value)
  pks <- c(pks,ks.test(data_nh_ls[[i]]$original_price, data_h_ls[[i]]$original_price)$p.value)
  
}



rbind(Industry = rep(c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),2),
      #     type = industries_to_check,
      # c = signif(cvec,2),
      # cnh = signif(cnhvec,2),
      c(format(signif(alphaks,2)),
      format(signif(alphat,2))),
      c(format(signif(Tks,2)),
      format(signif(Tt,2))),
      c(format(signif(dks,2)),
      format(signif(dt,2))),
      c(format(signif(pks,2)),
      format(signif(pt,2)))) ->  testrestuls

rownames(testrestuls) <- c("Industry", "Discount price, $(1-\\alpha)p$",
                           "Launch day to holiday, $T$",
                           "Duration, $d$",  
                           "Original price, $p$")






# rbind(Industry = rep(c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),2),
#       #     type = industries_to_check,
#       # c = signif(cvec,2),
#       # cnh = signif(cnhvec,2),
#       cbind(alphaks = format(signif(alphaks,2)),
#             alphat = format(signif(alphat,2))),
#       cbind(Tks = format(signif(Tks,2)),
#             Tt = format(signif(Tt,2))),
#       cbind(dks = format(signif(dks,2)),
#             dt = format(signif(dt,2))),
#       cbind(pks = format(signif(pks,2)),
#             pt = format(signif(pt,2)))) ->  testrestuls


# rownames(testrestuls) <- c("Industry", "ks test: discount price, $(1-\\alpha)p$",
#                            "t test: discount price, $(1-\\alpha)p$", 
#                            "ks test: launch day to holiday, $T$",
#                            "t test: launch day to holiday, $T$", 
#                            "ks test: duration, $d$", "t test: duration, $d$", 
#                            "ks test: original price, $p$", "t test: original price, $p$")




print(xtable(testrestuls),#tabular.environment = "longtable", 
      only.contents=TRUE, include.rownames=T, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      file = 'testresultstable.tex')


# multivariate, placebo ------


set.seed(123)
pseudoholidays <- sample(seq(7,95)[-holidays.doy],10,replace = FALSE)

pseudoholidays_idx_table <- data.frame(holiday_idx = seq(1,length(pseudoholidays),1),
                                       h_first_day = pseudoholidays[order(pseudoholidays)],
                                       h_last_day = pseudoholidays[order(pseudoholidays)])

merge(merge(dealsfull%>% mt(end_dow_dm = launch_dow_dm + offering_duration), pseudoholidays_idx_table,all = TRUE) %>% 
        mt(pseudoholidayind = (launch_dow_dm<=h_last_day)*(end_dow_dm>=h_first_day)) %>%
        mt(prep.periodtmp = h_first_day - launch_dow_dm) %>% mt(prep.periodtmp1 = pmax(prep.periodtmp,0)) %>% 
        gb(event_id) %>% sm(pseudoholiday=sum(pseudoholidayind,na.rm=TRUE)>0, 
                            pre.period = (min(prep.periodtmp1))),dealsfull,by="event_id")  -> dealsfull_pseudo


datadim_h <- vector()
datadim_nh <- vector()
data_h_ls <- vector(mode = "list", length = 11)
data_nh_ls <- vector(mode = "list", length = 11)


for (i in industries_to_check){
  print(i)
  if(i == 2){
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type%in%c(2,3)) %>% 
      fl(offering_duration<40) %>%
      {.->>dealsfull_type}%>%
      # filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
      {.->>sdop} %>% 
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    
    print(dim(sdop))
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    
    data_nh <- dealsfull_pseudo%>%filter((pseudoholiday)==1,type%in%c(2,3)) %>% 
      fl(prep.period>0,prep.period<40) %>%
      {.->>dealsfull_type}%>%
      # filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
      {.->>sdop} %>% 
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type%in%c(2)) %>%
    #   {.->>dealsfull_type}%>%
    #   # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.4)), total_volume<quantile(dealsfull_type$total_volume,c(0.6))) %>% 
    #   {.->>sdop} %>% 
    #   filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
    
    
    
  }else{
    data_h <- dealsfull%>%filter(is.na(holiday)==0,type==i)%>%
      {.->>sdop}%>%{.->>dealsfull_type} %>% 
      fl(offering_duration<40) %>%
      fl(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    data_h_ls[[i]] <- data_h
    datadim_h <- c(datadim_h,dim(sdop)[1])
    
    data_nh <- dealsfull_pseudo%>%filter((pseudoholiday)==1,type==i)%>%
      {.->>sdop}%>%{.->>dealsfull_type} %>% 
      fl(total_volume>quantile(dealsfull_type$total_volume,c(0.1)), total_volume<quantile(dealsfull_type$total_volume,c(0.9))) %>% 
      mutate(original_price_sd = original_price/sd(original_price))
    
    # data_nh <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%
    #   {.->>sdop}%>%{.->>dealsfull_type} %>% 
    #   fl(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))
    
    data_nh_ls[[i]] <- data_nh
    datadim_nh <- c(datadim_nh,dim(sdop)[1])
  }
  
  
  print(dim(sdop))
  datasanalysis <- c(datasanalysis,dim(sdop)[1])
  
}


set.seed(123)
# data_h_ls[[11]] <- data_h_ls[[11]][sample(dim(data_h_ls[[11]])[1],200,replace = T),]

ct <- 1
cramernholvshol_ls <- vector(mode="list",length  = 5)
cramernholvsholtest <- vector()
for(i in industries_to_check){
  
  cramer_nhol <- cbind((1-data_nh_ls[[i]]$discount)*data_nh_ls[[i]]$original_price_sd, data_nh_ls[[i]]$prep.period, data_nh_ls[[i]]$offering_duration)
  cramer_hol <- cbind((1-data_h_ls[[i]]$discount)*data_h_ls[[i]]$original_price_sd, data_h_ls[[i]]$prep.period, data_h_ls[[i]]$offering_duration)
  
  cramernholvshol_ls[[ct]] <- cramer.test(cramer_nhol,cramer_hol,conf.level=0.95,replicates=500,sim="permutation")
  cramernholvsholtest <- c(cramernholvsholtest,cramernholvshol_ls[[ct]]$p.value)
  ct <- ct + 1
  print(ct)
}




