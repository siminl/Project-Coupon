# load all outputs ---

bs_outputs <- vector(mode = "list", length = 11)


root_path <- "/Users/siminli/Documents/GitHub/Project-Coupon/Deals_bootstrap_outputs/output_type1/"
ls_name <- paste(root_path,list.files(path=root_path,pattern="out\\_[[:digit:]]\\_[[:digit:]].*\\.rda"),sep="")

for(i in 1:length(ls_name)){
  
  load(file=ls_name[i]) 
  bs_outputs[[1]][[i]] <- out
}


root_path <- "/Users/siminli/Documents/GitHub/Project-Coupon/Deals_bootstrap_outputs/output_type2/"
ls_name <- paste(root_path,list.files(path=root_path,pattern="out\\_[[:digit:]]\\_[[:digit:]].*\\.rda"),sep="")

for(i in 1:length(ls_name)){
  
  load(file=ls_name[i]) 
  bs_outputs[[2]][[i]] <- out
}


root_path <- "/Users/siminli/Documents/GitHub/Project-Coupon/Deals_bootstrap_outputs/output_type5/"
ls_name <- paste(root_path,list.files(path=root_path,pattern="out\\_[[:digit:]]\\_[[:digit:]].*\\.rda"),sep="")

for(i in 1:length(ls_name)){
  
  load(file=ls_name[i]) 
  bs_outputs[[5]][[i]] <- out
}


root_path <- "/Users/siminli/Documents/GitHub/Project-Coupon/Deals_bootstrap_outputs/output_type10/"
ls_name <- paste(root_path,list.files(path=root_path,pattern="out\\_[[:digit:]][[:digit:]]\\_[[:digit:]].*\\.rda"),sep="")

for(i in 1:length(ls_name)){
  
  load(file=ls_name[i]) 
  bs_outputs[[10]][[i]] <- out
}

root_path <- "/Users/siminli/Documents/GitHub/Project-Coupon/Deals_bootstrap_outputs/output_type11/"
ls_name <- paste(root_path,list.files(path=root_path,pattern="out\\_[[:digit:]][[:digit:]]\\_[[:digit:]].*\\.rda"),sep="")

for(i in 1:length(ls_name)){
  
  load(file=ls_name[i]) 
  bs_outputs[[11]][[i]] <- out
}




