
numCores <- 64
# save(list = c("sales_estimates", "sales_estimates_h",
#               "mleout_preh","mleout_h","gamma_h","gamma_preh","dealsfull"),
#      file = "prepdata.Rdata")
save(list = c("gamma_h","gamma_preh"),
     file = "gammas.Rdata")


load("prepdata.Rdata")
# sales_estimates
# sales_estimates_h
# mleout_preh

#source(getGamma.R)
NumIter <<- 5
Bsamples <<- 10
mleout_h_ms_bs <- vector(mode = "list", length = 11)

industries_to_est <- 1
registerDoParallel(numCores)
source("BS_holiday_models_1.1_Shat.R")
# source("mle_h_uniq_paratupples.R")
source("BS_mle_h_uniq_parakron.R")


