#2,5,11,10 done
# 6,7,8,3,1,4
industries_to_est <<- c(1)
NumIter <<- 10

# mleout_preh_ls <- list()
# mleout_h_ls <- list()

source("preholiday_models_1.1_Shat.R")
# source("preholiday_models_1.1.R")
# source("mle_uniq_paratupples.R")
source("mle_uniq_parakron.R")

# print(mleout_preh)
mleout_preh_ls[[1]] <- mleout_preh
STORE_mleout_preh_ms <- mleout_preh_ms

D2nhval <- list()
for(i in industries_to_est){
  
  nonhol_data <- dealsfull%>%filter(is.na(holiday)==1,type==i)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))

  # D2nhval[[i]] <- ((mleout_preh[[i]]$par[2])+(-exp(mleout_preh[[i]]$par[4]))*(1-nonhol_data$discount)*nonhol_data$original_price+
  #       -exp(mleout_preh[[i]]$par[3])*log(nonhol_data$offering_duration))

  k <- 4

  fes <- rep(0,dim(nonhol_data)[1])
  for(row in 1:dim(nonhol_data)[1]){
    fes[row] <- control_FE(sales_estimates[i,],nonhol_data[row,])
  }
  #(mleout_preh_ms[[k]][[i]]$par[2])
  D2nhval[[i]] <- ((mleout_preh_ms[[k]][[i]]$par[2])+ (-exp(mleout_preh_ms[[k]][[i]]$par[4]))*(1-nonhol_data$discount)*nonhol_data$original_price+
                        -exp(mleout_preh_ms[[k]][[i]]$par[3])*log(nonhol_data$offering_duration))*exp(as.numeric(fes))

}


# source("preholiday_models_1.R")
# source("mle_unique_parakron.R")
# 
# print(mleout_preh)
# mleout_preh_ls[2] <- mleout_preh
# 


# 
# source("preholiday_models_2.R")
# source("mle_unique_parakron.R")
# 
# print(mleout_preh)
# mleout_preh_ls[4] <- mleout_preh

industries_to_est <<- c(1)
models <- c(1,5)
NumIter <<- 6




industries_to_est <<- c(1)
source("holiday_models_1.1_Shat_new.R")
# source("mle_h_uniq_paratupples.R")
source("mle_h_uniq_parakron.R")

industries_to_est <<- c(5)
source("holiday_models_5.1_Shat.R")
source("mle_h_uniq_parakron.R")

industries_to_est <<- c(10)
source("holiday_models_10.1_Shat.R")
source("mle_h_uniq_parakron.R")

industries_to_est <<- c(11)
source("holiday_models_11.1_Shat.R")
source("mle_h_uniq_parakron.R")




for(i in industries_to_est){
  
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  
  # D2nhval[[i]] <- exp((mleout_preh[[i]]$par[2])+((mleout_preh[[i]]$par[4]))*(1-nonhol_data$discount)*nonhol_data$original_price+
  #       -exp(mleout_preh[[i]]$par[3])*log(nonhol_data$offering_duration))
  
  fes_h <- rep(0,dim(hol_data)[1])
  for(row in 1:dim(hol_data)[1]){
    fes_h[row] <- control_FE_h(sales_estimates_h[i,],hol_data[row,])
  }
  
  
  k <- 9
  D1hval[[i]] <- hol_data$total_volume*((mleout_h_ms[[k]][[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h_ms[[k]][[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h_ms[[k]][[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h_ms[[k]][[i]]$par[6]+ exp(fes_h)/(total_volume-exp(fes_h)))/
    (1+((mleout_h_ms[[k]][[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h_ms[[k]][[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h_ms[[k]][[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h_ms[[k]][[i]]$par[6]+ exp(fes_h)/(total_volume-exp(fes_h))))
  
  fes <- rep(0,dim(hol_data)[1])
  for(row in 1:dim(hol_data)[1]){
    fes[row] <- control_FE(sales_estimates[i,],hol_data[row,])
  }
  
  D2nh_est <- exp(mleout_preh[[i]]$par[2]+((mleout_preh[[i]]$par[4]))*(1-hol_data$discount)*hol_data$original_price+
                  -exp(mleout_preh[[i]]$par[3])*log(hol_data$offering_duration))*exp(as.numeric(fes))
  
  D2hval[[i]] <- exp((mleout_h_ms[[k]][[i]]$par[5])*log(hol_data$offering_duration)+((mleout_h_ms[[k]][[i]]$par[7]))+
        ((log(D2nh_est))))
  
 (-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6])/
    (1+(-exp(mleout_h[[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h[[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h[[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h[[i]]$par[6]))
}


STORE_mleout_h_ms_1 <- mleout_h_ms
print(mleout_h)
mleout_h_ls[[1]] <- mleout_h

source("holiday_models_5.1.R")
source("mle_h_uniq_paratupples.R")



STORE_mleout_h_ms_5 <- mleout_h_ms
print(mleout_h)
mleout_h_ls[[5]] <- mleout_h

source("holiday_models_2.R")
source("mle_h_uniq_paratupples.R")

STORE_mleout_h_ms_2 <- mleout_h_ms
print(mleout_h)
mleout_h_ls[[2]] <- mleout_h


source("holiday_models_3.R")
source("mle_h_uniq_paratupples.R")

STORE_mleout_h_ms_3 <- mleout_h_ms
print(mleout_h)
mleout_h_ls[[3]] <- mleout_h


source("holiday_models_4.R")
source("mle_h_uniq_paratupples.R")

STORE_mleout_h_ms_4 <- mleout_h_ms
print(mleout_h)
mleout_h_ls[[4]] <- mleout_h



# source("preholiday_models_2.R")
# source("mle_uniq_paratupples.R")
# 
# print(mleout_preh)
# mleout_preh_ls[[2]] <- mleout_preh




# 1     1 Body Care1           825
# 2     2 Casual Dine1         182
# 3     3 Casual Dine2        3752
# 4     4 Fine Dine3           913
# 5     5 Fine Dine4           485
# 6     6 Hotel2               423
# 7     7 Indoor1              216
# 8     8 Movie Theatre2       395
# 9     9 Outdoor1             160
# 10    10 Outdoor3             688
# 11    11 Photography3         145


for(j in models){
  D1hval[[j]] <- list()
  for(i in industries_to_est){
    hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%{.->>sdop}%>%
      mutate(original_price = original_price/sd(original_price))
    if(j ==1){
      D1hval[[j]][[i]] <- exp(-exp(mleout_h_ls[[j]][[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h_ls[[j]][[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h_ls[[j]][[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h_ls[[j]][[i]]$par[6])/
        (1+exp(-exp(mleout_h_ls[[j]][[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h_ls[[j]][[i]]$par[3]))*log(hol_data$prep.period)+((mleout_h_ls[[j]][[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h_ls[[j]][[i]]$par[6]))
      
    }else if(j==5){
      D1hval[[j]][[i]] <- exp(-exp(mleout_h_ls[[j]][[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h_ls[[j]][[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h_ls[[j]][[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h_ls[[j]][[i]]$par[6])/
        (1+exp(-exp(mleout_h_ls[[j]][[i]]$par[2])*((1-hol_data$discount)*hol_data$original_price)+((mleout_h_ls[[j]][[i]]$par[3]))*log(hol_data$prep.period)+(-exp(mleout_h_ls[[j]][[i]]$par[4]))*log(hol_data$offering_duration)+mleout_h_ls[[j]][[i]]$par[6]))
      
    }
    
  }
  
}


