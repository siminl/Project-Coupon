# using PCA to determine within industry price variations thus clustering based on price

AlgoCat_PCA <- function(deal_data,optclusters){
  
  varexp <- vector()
  centers.dt <- vector()
  
  deal_data <- deal_data%>%mutate(detailed_cat_price = "")%>%
    mutate(perperson = ifelse(is.na(partysize)==0,original_price/partysize,original_price))
  
  # from the scree plot analysis, we categorize into the following number of clusters in each category
  
  # comment out on 03282019, adding it as an input variable
  # optclusters <- c(2,3,3,3,3,2,4,3,3,2,2,2)
  
  numcats <- length(unique(deal_data$platform_cat))
  wss <- matrix(rep(NA,5*numcats),ncol = 5)
  set.seed(123)
  
  for(k in 1:numcats){
    
    idx_event <- deal_data[deal_data$platform_cat%in%unique(deal_data$platform_cat)[k],"event_id"]
    
    # try original price/ price within sub-industry segementation
    pca.data <- deal_data%>%filter(deal_data$platform_cat%in%unique(deal_data$platform_cat)[k])
    pca.data.gp <- pca.data%>%select(perperson)
    
    kmeans.data <- pca.data.gp[,"perperson"]
    
    nclusters <- 5
    for (i in 1:nclusters) wss[k,i] <- sum(kmeans(kmeans.data, centers=i)$withinss)
    
    kmeans.fit <- kmeans(kmeans.data, optclusters[k])
    varexp <- c(varexp,kmeans.fit$betweenss/kmeans.fit$totss)
    centers.dt <- c(centers.dt,kmeans.fit$centers)
    deal_data[deal_data$event_id%in%idx_event,"detailed_cat_price"] = paste0(unique(deal_data$platform_cat)[k],kmeans.fit$cluster,seq="")
    
  }
  
  # remove categories that have less than 30 deals
  category_ss <- table(deal_data$detailed_cat_price)
  category_keep <- rownames(t(t(category_ss[category_ss>100])))
  deal_data <- deal_data%>%filter(detailed_cat_price%in%as.vector(category_keep))%>%select(-perperson)
  
  return(deal_data)
  
}

# 
# # plot the scree plots -----
# screeplot <- data.frame(wss = as.vector(t(wss)),
#                         group = kronecker(seq(1,numcats),rep(1,5)),
#                         x = rep(seq(1,5),numcats))
# 
# ggplot(data=screeplot%>%filter(group%in%c(1,2)),aes(x=x,y=wss))+geom_line()+facet_grid(.~group)+
#   theme(panel.background = element_rect(fill="white",color="grey50"))+
#   ylab("WCSS")+xlab("")



AlgoCat_PCA_SCREEPLOT <- function(deal_data,optclusters,nclusters ){
  
  varexp <- vector()
  centers.dt <- vector()
  
  deal_data <- deal_data%>%mutate(detailed_cat_price = "")
  
  # from the scree plot analysis, we categorize into the following number of clusters in each category
  
  # comment out on 03282019, adding it as an input variable
  # optclusters <- c(2,3,3,3,3,2,4,3,3,2,2,2)
  
  numcats <- length(unique(deal_data$platform_cat))
  wss <- matrix(rep(NA,nclusters*numcats),ncol = nclusters)
  set.seed(123)
  
  for(k in 1:numcats){
    
    idx_event <- deal_data[deal_data$platform_cat%in%unique(deal_data$platform_cat)[k],"event_id"]
    
    # try original price/ price within sub-industry segementation
    pca.data <- deal_data%>%filter(deal_data$platform_cat%in%unique(deal_data$platform_cat)[k])
    pca.data.gp <- pca.data%>%select(original_price)
    
    kmeans.data <- pca.data.gp[,"original_price"]
    

    for (i in 1:nclusters) wss[k,i] <- sum(kmeans(kmeans.data, centers=i)$withinss)
    
    kmeans.fit <- kmeans(kmeans.data, optclusters[k])
    varexp <- c(varexp,kmeans.fit$betweenss/kmeans.fit$totss)
    centers.dt <- c(centers.dt,kmeans.fit$centers)
    deal_data[deal_data$event_id%in%idx_event,"detailed_cat_price"] = paste0(unique(deal_data$platform_cat)[k],kmeans.fit$cluster,seq="")
    
  }
  
  # plot the scree plots -----
  screeplot <- data.frame(wss = as.vector(t(wss)),
                          group = kronecker(seq(1,numcats),rep(1,nclusters)),
                          x = rep(seq(1,nclusters),numcats))
  
  group_name_mat <- data.frame(name = unique(deal_data$platform_cat),
                               elbowclusters = optclusters,
                               varexp=varexp,
                               group = seq(1,numcats))
  
  screeplot <- merge(screeplot,group_name_mat,by="group",all.x = TRUE)
  
  

  return(list(deal_data=deal_data,wss = wss,varexp=varexp,screeplot = screeplot))
  
}



