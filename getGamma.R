# This file is to get the gamma coefficients of in the variance of demand. 
# The idea is that we can find the perday sales volume in each industry for the deals. 
# fit Var(S_i(t)) ~ gamma t + var(S)
# this gamma is what we want. 


# do I want to fit the sales curve?
# approach 1: do not fit, directly use the raw data. 

dealsfull%>%group_by(type,detailed_cat_price)%>%summarise(meandur = mean(offering_duration))
cumvol_var_dt <- dealsfull%>%
  mutate(isholiday = (is.na(holiday)==0))%>%
  mutate(offering_duration_5day = RoundTo(offering_duration,multiple = 5))%>%
  group_by(type,detailed_cat_price,isholiday)%>%
  mutate(varS = var(total_volume),varS1 = var(first_day_volume),varS5=var(fifth_day_volume))%>%
  group_by(type,detailed_cat_price,isholiday,offering_duration)%>%
  mutate(vardur = var(total_volume),
         varS5_norm = var(fifth_day_volume/5),vardur_norm = var(total_volume/offering_duration),
         discprice = (1-discount)*original_price,
         cases=n())%>%
  filter(cases>2)%>%
  select(type,detailed_cat_price,isholiday,offering_duration,varS,varS1,varS5,vardur,varS5_norm,vardur_norm,discprice)



gamma_lm <- list(list())
gamma_lm_2<- list(list())

gamma_h <- vector()
gamma_preh <- vector()

gamma_h_sig <- vector()
gamma_preh_sig  <- vector()

gamma_h_var <- vector()
gamma_preh_var <- vector()
for(i in 1:length(unique(dealsfull$type))){
  for(j in c(0,1)){
    tmp <- cumvol_var_dt%>%filter(type==i,isholiday==j)
    tmp <- data.frame(dayt = c(rep(1,dim(tmp)[1]),rep(5,dim(tmp)[1]),tmp$offering_duration),
                      dp = c(tmp$discprice,tmp$discprice,tmp$discprice),
                      vars = c(tmp$varS1,tmp$varS5,tmp$vardur),
                      normalized_vars = c(tmp$varS1,tmp$varS5_norm,tmp$vardur_norm))%>%
      #mutate(normalized_vars = vars/dayt)%>%
      filter(vars!=0,normalized_vars!=0)
    
    print(paste(dim(tmp)[1],length(unique(tmp$dayt))))
    gamma_lm[[i+j*length(unique(dealsfull$type))]] <- lm(data = tmp, log(vars)/2 ~ dayt)
    gamma_lm_2[[i+j*length(unique(dealsfull$type))]] <- lm(data = tmp%>%mutate(y=log((normalized_vars))/2,
                                                                               dayt = (dayt)),y ~ dayt)
    # gamma_lm_2[[i+j*length(unique(dealsfull$type))]] <- glm(data = tmp%>%mutate(y=log((normalized_vars))/2),normalized_vars ~ I(2*dayt),
    #                                                         family = "poisson")
    

  }
  
  coef_df_h <- coef(summary(gamma_lm_2[[i+length(unique(dealsfull$type))]]))
  gamma_h <- rbind(gamma_h,coef_df_h[,1]*(coef_df_h[,4]<0.1))
 # gamma_h_var <- rbind(gamma_h_var,coef_df_h[,1]*(coef_df_h[,4]<0.1))
  
  gamma_h_sig <- rbind(gamma_h_sig, coef_df_h[,2])
  
  coef_df_preh <- coef(summary(gamma_lm_2[[i]]))
  gamma_preh <- rbind(gamma_preh,coef_df_preh[,1]*(coef_df_preh[,4]<0.1))
 # gamma_preh_var <- rbind(gamma_preh_var,coef_df_preh[,1]*(coef_df_preh[,4]<0.1))
  
  gamma_preh_sig <- rbind(gamma_preh_sig, coef_df_preh[,2])
  
}



gamma_h <- data.frame(intercept = gamma_h[,1],
                      gamma_h[,"dayt"]*(dealsfull%>%filter(is.na(holiday)==0)%>%group_by(type,detailed_cat_price)%>%summarise(meandur = mean(offering_duration)))[,"meandur"]*2)%>%
  as.matrix()
gamma_preh <- data.frame(intercept = gamma_preh[,1],
                         gamma_preh[,"dayt"]*(dealsfull%>%filter(is.na(holiday)==1)%>%group_by(type,detailed_cat_price)%>%summarise(meandur = mean(offering_duration)))[,"meandur"]*2)%>%
  as.matrix()


gamma_h_sig <- data.frame(intercept = gamma_h_sig[,1],
                          gamma_h_sig[,2]*((dealsfull%>%filter(is.na(holiday)==0)%>%group_by(type,detailed_cat_price)%>%summarise(meandur = mean(offering_duration)))[,"meandur"]*2))%>%
  as.matrix()
gamma_preh_sig <- data.frame(intercept = gamma_preh_sig[,1],
                             gamma_preh_sig[,2]*((dealsfull%>%filter(is.na(holiday)==1)%>%group_by(type,detailed_cat_price)%>%summarise(meandur = mean(offering_duration)))[,"meandur"]*2))%>%
  as.matrix()



