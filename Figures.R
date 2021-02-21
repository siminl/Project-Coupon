# the mechanism


ratioofcosts <- chs/cnhs
types <- c(2,5,11,10,1)
deltad <- c(3.79,0.93,1.32,2.06,-0.96)
deltadsig <- c(sqrt(6.11),sqrt(2.25),0.92,sqrt(3.6),0.22)
deltad <- deltaDvec
mcpratio <- c(0.0013,0.0041,0.032,0.0039,0.010)
mcpratiosd <- c(0.00041,0.0017,0.013,0.0023,0.0096)

dsidx_ls <- dsidx2vec
dsidx_ls[1] <- -0.56
dsidxsig_ls <- dsidx2sigvec
dsidxsig_ls[1] <- 0.48
alterD_ls <- alterDvec
  
industrynames <- dealsfull%>%filter(type %in% industries_to_check)%>%select(type,detailed_cat_price)%>%unique()
catnames <- data.frame(type = industries_to_check,
                       Industry = c("Casual Dine", "Fine Dine", "Photography", "Outdoor", "Body Care"))
plotdata <- merge(merge(industrynames, 
                  data.frame(type = industries_to_check, #ratioofcosts, 
                             deltad,deltadsig,mcpratio,mcpratiosd,alterD_ls,dsidx_ls),
                  by="type"),catnames,by="type")%>%
  mutate(alterlabel = (Industry == "Photography"))%>%
  mutate(order_alterD = rank(alterD_ls), order_dsidx = rank(dsidx_ls))


ggplot(data=plotdata,aes(y=mcpratio,x=deltad))+geom_point(aes(size=order_alterD,color=order_dsidx))+
  #geom_errorbarh(aes(xmax=deltad+1*deltadsig,xmin=deltad-1*deltadsig,y=(mcpratio)),size=0.7,height=0.0015) +
  #geom_errorbar(aes(ymax=mcpratio+1*mcpratiosd,ymin=mcpratio-1*mcpratiosd,x=(deltad)),size=0.7,width=0.15) +
  geom_rect(mapping=aes(xmin=deltad-1*deltadsig, xmax=deltad+1*deltadsig, ymin=mcpratio-1*mcpratiosd, ymax=mcpratio+1*mcpratiosd), 
            fill="grey70", color="grey30",alpha=0.3)+
  geom_label(data=plotdata%>%filter(alterlabel==FALSE),
             aes(label=Industry),vjust = -0.3, hjust = 0.1, nudge_x = 0.1)+
  geom_label(data=plotdata%>%filter(alterlabel==TRUE),
             aes(label=Industry),vjust = -0.2, hjust = 0.1, nudge_x = 0.1)+
  theme(panel.background = element_rect(fill="grey90",color="grey90"),
        #axis.title.x = element_blank(),axis.title.y=element_blank(),
        axis.text.y = element_text(size=10,face="bold"),
        axis.text.x = element_text(size=12,face="bold"),
        legend.position = "top",
        legend.justification = c("center", "bottom"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.background = element_rect(size=0.4, linetype="solid", 
                                         colour ="black"),
        plot.margin=unit(c(0.5,1.5,0.5,1.0), "cm"))+
  #xlim(0, 7.5)+
  #geom_hline(yintercept=-0.1,linetype=2)+
  #geom_vline(xintercept= 3.75,linetype=2)+
  labs(y=TeX("Marginal Cost Price Ratio, $\\frac{\\widehat{mc}_{h}}{p_{h}}$"), 
       x = TeX("Holiday Demand Swing, $\\widehat{\\delta}_D$"),
       color=TeX("Demand-flattening Effect (Order), $\\textit{R}$"),
       size = TeX("Demand-boosting Effect (Order), $\\Delta D$"))+
  theme(legend.text = element_text(size=10),legend.title = element_text(size=10),
        legend.background = element_rect(color = NA),
        legend.box = "horizontal",
        legend.position = "bottom")+
  scale_color_continuous(low = "black", high = "grey70")


# plot the deal designs of industries 

(merge(optimparas[[5]][,c(4,5,7)]%>%mutate(volumebins = total_volume - (total_volume%%100)),
       optimparas[[11]][,c(4,5,7)]%>%mutate(volumebins = total_volume - (total_volume%%100)),
       by="volumebins",all.x=TRUE))

binsize <- 200
data.frame(rbind(optimparas[[5]][,c(4,5,7)]%>%
                   mutate(volumebins = total_volume - (total_volume%%binsize),type = 5),
                 optimparas[[11]][,c(4,5,7)]%>%
                   mutate(volumebins = total_volume - (total_volume%%binsize), type = 11),
                 optimparas[[2]][,c(4,5,7)]%>%
                   mutate(volumebins = total_volume - (total_volume%%binsize),type = 2),
                 optimparas[[10]][,c(4,5,7)]%>%
                   mutate(volumebins = total_volume - (total_volume%%binsize),type = 10),
                 optimparas[[1]][,c(4,5,7)]%>%
                   mutate(volumebins = total_volume - (total_volume%%binsize),type = 1)))%>%
  group_by(volumebins,type)%>%
  summarise(mudisc = mean(discount), sddisc = sd(discount), muT = mean(prep.period), sdT = sd(prep.period)) -> plotdata
  



sapply(optimparas,function(x) {if(is.null(dim(x))==0) apply(x,2,function(x) 
  mean(x[x<quantile(x,0.85)&x>quantile(x,0.15)]))})

sapply(optimparas,function(x) {if(is.null(dim(x))==0) apply(x,2,function(x) 
  sd(x[x<quantile(x,0.75)&x>quantile(x,0.25)]))})


sapply(optimparas,function(x) {if(is.null(dim(x))==0) apply(x,2,median)})
sapply(optimparas,function(x) {if(is.null(dim(x))==0) apply(x,2,sd)})

plotdata <- matrix(sapply(optimparas,function(x) {if(is.null(dim(x))==0) apply(x,2,function(x) 
  mean((x)[(x)<quantile((x),0.85)&(x)>quantile((x),0.15)]))})%>%unlist(),ncol=6,byrow=TRUE)%>%
  cbind(matrix(sapply(optimparas,function(x) {if(is.null(dim(x))==0) apply(x,2,function(x) 
    sd(log(x)[log(x)<quantile(log(x),0.75)&log(x)>quantile(log(x),0.25)]))})%>%unlist(),ncol=6,byrow=TRUE))%>%
  cbind(type = c(1,2,5,10,11))%>%as.data.frame()

colnames(plotdata) <- c("discpara","Tpara","dpara","discobs","Tobs","dobs",
                        "discparasd","Tparasd","dparasd","discobssd","Tobssd","dobssd","type")




plotdata <- matrix(sapply(optimparas,function(x) {if(is.null(dim(x))==0) apply(x,2,function(x) 
  mean((x)[(x)<quantile((x),0.85)&(x)>quantile((x),0.15)]))})%>%unlist(),ncol=6,byrow=TRUE)%>%
  cbind(matrix(sapply(optimparas,function(x) {if(is.null(dim(x))==0) apply(x,2,function(x) 
    sd(log(x)[log(x)<quantile(log(x),0.75)&log(x)>quantile(log(x),0.25)]))})%>%unlist(),ncol=6,byrow=TRUE))%>%
  cbind(type = c(1,2,5,10,11))%>%as.data.frame()

colnames(plotdata) <- c("discpara","Tpara","dpara","discobs","Tobs","dobs",
                        "discparasd","Tparasd","dparasd","discobssd","Tobssd","dobssd","type")




ggplot(data=plotdata)+
  geom_point(aes(x=type,y=Tobs))+
  geom_errorbar(aes(ymax=Tobs+1.44*Tobssd,ymin=Tobs-1.44*Tobssd,x=(type)),size=0.7,width=0.3) 


# draw 2D discount and T figure ---


# 
# 
# ggplot()+geom_histogram(aes(x=optimparas[[1]][,5],alpha=0.5),fill="blue")+
#   geom_histogram(aes(x=optimparas[[2]][,5],alpha=0.5),fill="red")+
#   geom_histogram(aes(x=optimparas[[10]][,5],alpha=0.5),fill="black")+
#   geom_histogram(aes(x=optimparas[[11]][,5],alpha=0.5),fill="green")+
#   geom_histogram(aes(x=optimparas[[5]][,5],alpha=0.5),fill="cyan")



###  variations in sales for gamma and sigma ---------
cumvol_var_dt <- dealsfull%>%
  mutate(isholiday = (is.na(holiday)==0))%>%
  mutate(offering_duration_5day = RoundTo(offering_duration,multiple = 3))%>%
  group_by(type,detailed_cat_price,isholiday)%>%
  mutate(varS = var(total_volume),varS1 = var(first_day_volume),varS5=var(fifth_day_volume))%>%
  group_by(type,detailed_cat_price,isholiday,offering_duration)%>%
  mutate(vardur = var(total_volume),
         varS5_norm = var(fifth_day_volume/5),vardur_norm = var(total_volume/offering_duration),
         discprice = (1-discount)*original_price,
         cases=n())%>%
  filter(cases>2)%>%
  select(type,detailed_cat_price,isholiday,offering_duration,varS,varS1,varS5,vardur,varS5_norm,vardur_norm,discprice,original_price) %>% 
  filter(offering_duration<45)


plotdata <- vector()
count <- 1
for(i in c(2,5,10,11,1)){
  #print(i)
  for(j in c(0,1)){
    if(i %in% c(2)){
      tmp <- cumvol_var_dt%>%filter(type%in%c(2,3),isholiday==j) %>% {.->>dealsfull_type} %>% 
        filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6)))
      
    }else{
      tmp <- cumvol_var_dt%>%filter(type==i,isholiday==j)    
    }

    tmp <- data.frame(dayt = c(rep(1,dim(tmp)[1]),rep(5,dim(tmp)[1]),tmp$offering_duration),
                      dp = c(tmp$discprice,tmp$discprice,tmp$discprice),
                      vars = c(tmp$varS1,tmp$varS5,tmp$vardur),
                      normalized_vars = c(tmp$varS1,tmp$varS5_norm,tmp$vardur_norm))%>%
      #mutate(normalized_vars = vars/dayt)%>%
      filter(vars!=0,normalized_vars!=0)
    if(count ==1){
      print(i)
      print(j)
      plotdata <- data.frame(tmp, type = rep(i,dim(tmp)[1]), isholiday = rep(j,dim(tmp)[1]) )
    }else{
      print(i)
      print(j)
      plotdata <- rbind(plotdata, 
                        data.frame(tmp,  type = rep(i,dim(tmp)[1]), isholiday = rep(j,dim(tmp)[1])))
    }
    count <- count + 1
  }
}

plotdata <- plotdata%>%select(-c(dp))%>%
  group_by(dayt,isholiday,type)%>%mutate(max_normal = max(normalized_vars))%>%
  mutate(holidx = ifelse(isholiday==0,"Pre-holiday Deals","Holiday Deals"))%>%
  merge(data.frame(type = c(2,5,11,10,1),
                   Industry = c("Casual Dining","Fine Dining","Photography","Outdoor","Body Care")),by="type",all.x = TRUE)

ggplot(data = plotdata%>%unique()) + 
  geom_point(aes(x=dayt, y = log(max_normal), color = as.factor(holidx)))+
  geom_smooth(aes(x=dayt, y = log(max_normal), color = as.factor(holidx)),method=lm, linetype = 2,se=TRUE) +
  facet_grid(.~Industry)+
  labs(x="t (days)", y = "log(Var(S(t)))", color = "")+
  scale_color_manual(values=c("grey10","grey50"))+
  theme_bw(base_size = 18)+  
  theme(legend.position="bottom")  -> pvaringamma
ggsave("~/Documents/GitHub/Project-Coupon/variationsforvariance.pdf",width = 12.17 ,height = 5.16,units = "in")




### variations to identify the non-holiday model -----
industries_to_check <- c(2,5,11,10,1)
count <- 1
plotdata <- vector()
predays <- 21
pre.windows <- c(seq(21-predays,21),seq(44-predays,44),seq(92-predays,92))

for(i in industries_to_check){
  
  if(i == 2){
    tmp <- rbind(dealsfull%>%
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type%in%c(2),end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
     # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      mutate(discprice = (1-discount)*original_price)%>%
      #filter(city %in% c("BJ","SH","GZ","SZ"), platform =="dp")%>%
      select(discprice,discount,prep.period,offering_duration,total_volume,city,original_price,type),
     dealsfull%>%
       mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
       filter(is.na(holiday)==1,type%in%c(3),end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
       filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>%
       # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
       mutate(discprice = (1-discount)*original_price)%>%
       #filter(city %in% c("BJ","SH","GZ","SZ"), platform =="dp")%>%
       select(discprice,discount,prep.period,offering_duration,total_volume,city,original_price,type))  
  }else{
    tmp <- dealsfull%>%
      mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==1,type==i,end_dow_dm%in%pre.windows)%>%{.->>dealsfull_type}%>%
     # filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      mutate(discprice = (1-discount)*original_price)%>%
      #filter(city %in% c("BJ","SH","GZ","SZ"), platform =="dp")%>%
      select(discprice,discount,prep.period,offering_duration,total_volume,city,original_price,type)    
  }

  
  if(count ==1){
    plotdata <- data.frame(tmp %>% select(-type),type = rep(i,dim(tmp)[1]))
  }else{
    plotdata <- rbind(plotdata,data.frame(tmp %>% select(-type),type = rep(i,dim(tmp)[1])))
  }
  
  count <- count + 1

  
}

plotdata <- plotdata%>%
  merge(data.frame(type = c(2,5,11,10,1),
                   Industry = c("Casual Dining","Fine Dining","Photography","Outdoor","Body Care")),by="type",all.x = TRUE)

ggplot()+
    geom_point(data=plotdata,aes(x=discount,y=offering_duration,size = total_volume))+
    # scale_color_brewer(palette = "Spectral",type=5)+
  facet_grid(.~Industry)+
    labs(x="Discount",y="Duration",color="Industry",size="Total Deal Sales")+
  theme_bw(base_size = 18)+  
  theme(legend.position="bottom") -> p1
ggsave("~/Documents/GitHub/Project-Coupon/varianceforpreholidaydeals.pdf",width = 12.17 ,height = 5.16,units = "in")

industries_to_check <- c(2,5,11,10,1)
count <- 1
plotdata <- vector()
for(i in industries_to_check){
  
  if(i== 2){
    tmp <- rbind(dealsfull%>%
      # mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==0,type==2,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
        filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      mutate(discprice = (1-discount)*original_price)%>%
      #filter(city %in% c("BJ","SH","GZ","SZ"), platform =="dp")%>%
      select(discprice,discount,prep.period,offering_duration,total_volume,city,original_price,type),
      dealsfull%>%
        # mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
        filter(is.na(holiday)==0,type==3,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
        filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
        filter(original_price>quantile(dealsfull_type$original_price,c(0.4)), original_price<quantile(dealsfull_type$original_price,c(0.6))) %>% 
        mutate(discprice = (1-discount)*original_price)%>%
        #filter(city %in% c("BJ","SH","GZ","SZ"), platform =="dp")%>%
        select(discprice,discount,prep.period,offering_duration,total_volume,city,original_price,type))
    
  }else{
    tmp <- dealsfull%>%
      # mutate(end_dow_dm = launch_dow_dm+offering_duration)%>%
      filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
      filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
      mutate(discprice = (1-discount)*original_price)%>%
      #filter(city %in% c("BJ","SH","GZ","SZ"), platform =="dp")%>%
      select(discprice,discount,prep.period,offering_duration,total_volume,city,original_price,type)
  }
  

  
  if(count ==1){
    plotdata <- data.frame(tmp %>% select(-type),type = rep(i,dim(tmp)[1]))
  }else{
    plotdata <- rbind(plotdata,data.frame(tmp %>% select(-type),type = rep(i,dim(tmp)[1])))
  }
  
  count <- count + 1
  
  
}
plotdata <- plotdata%>%
  merge(data.frame(type = c(2,5,11,10,1),
                   Industry = c("Casual Dining","Fine Dining","Photography","Outdoor","Body Care")),by="type",all.x = TRUE)


p2 <- ggplot()+
  geom_point(data=plotdata,aes(x=discount,y=prep.period,size = total_volume))+
  facet_grid(.~Industry)+
  # scale_color_brewer(palette = "Spectral",type=5)+
  labs(x="Discount",y="Launch Date to Holiday",color="Industry",size="Total Deal Sales")+
  theme_bw(base_size = 18)+  
  theme(legend.position="bottom")

p2

plotdata_long <- plotdata %>% select(type, discount, prep.period, offering_duration, total_volume) %>% 
  tidyr::gather(parameter, value, c(discount, prep.period, offering_duration)) %>% 
  merge(data.frame(parameter= c("discount","offering_duration","prep.period"),
                   paranames = c("Discount", "Duration","Launch Day to Holiday")),all.x = TRUE,by="parameter") %>% 
  mutate(paranames = as.factor(paranames))


plotdata_long <- plotdata_long%>%
  merge(data.frame(type = c(2,5,11,10,1),
                   Industry = c("Casual Dining","Fine Dining","Photography","Outdoor","Body Care")),by="type",all.x = TRUE)

plotdata_long$paranames_f = factor(plotdata_long$paranames, levels=c("Discount","Launch Day to Holiday", "Duration"))

ggplot()+
  geom_point(data=plotdata_long,aes(x=total_volume,y=value))+
  facet_grid(paranames_f~Industry,scales = "free")+
  # scale_color_brewer(palette = "Spectral",type=5)+
  labs(x="Deal Sales",y="Observed Values")+
  theme_bw(base_size = 18)+  
  theme(legend.position="bottom")
ggsave("~/Documents/GitHub/Project-Coupon/variationforholidaydeals.pdf",width = 12.17 ,height = 8.16,units = "in")


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p1)

grid.arrange(pvaringamma,arrangeGrob(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1)),ncol=2, widths=c(1,2.5))



# goodness of fit ----------
count <- 1
plotdata <- vector()
for(i in industries_to_check){
  tmp <- optimparas[[i]][,seq(1,6)]
  colnames(tmp) <- c("discount_paras","prepT_paras","duration_paras",
                     "discount","prepT","duration")
  tmp <- data.frame(idx = seq(1,dim(tmp)[1]), tmp, type = i)
  
  if(count == 1){
    plotdata <- tmp
  }else{
    plotdata <- rbind(plotdata,tmp)    
  }
  
  count <- count +1
}

ggplot(tmp)+geom_point(aes(x=discount,y=discount_paras))+geom_abline(intercept = 0, slope = 1)
ggplot(tmp%>%mutate(diff = prepT-prepT_paras))+geom_point(aes(x=prepT,y=prepT_paras))+
  geom_abline(intercept = 0, slope = 1)

# plotdata <- plotdata%>%
#   mutate(discount_paras=ifelse(1-discount_paras))%>%
#   mutate(discount_paras=1-discount_paras,duration_paras=duration_paras/2)

plotdata <- merge(plotdata,data.frame(type = c(2,5,11,10,1),
                    Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care")),
                  by="type",all.x = TRUE)

p1 <- ggplot(plotdata,aes(x=idx))+
  geom_ribbon(aes(ymin = prepT_paras - 1.96*2, ymax = prepT_paras + 1.96*2), fill = "grey70")+
  geom_point(aes(y=prepT),size = 0.4)+
  facet_grid(Industry~.,scales = "free")+
  labs(x="",y="Launch Date to Holiday")

# %>%mutate(discount_paras = 1- discount_paras)
p2 <- ggplot(plotdata,aes(x=idx))+
  geom_ribbon(aes(ymin = discount_paras - 1.96*0.1, ymax = discount_paras + 1.96*0.1), fill = "grey70")+
  geom_point(aes(y=discount),size = 0.4)+
  facet_grid(Industry~.,scales = "free")+
  labs(x="",y="Discount")

p3 <- ggplot(plotdata,aes(x=idx))+
  geom_ribbon(aes(ymin = duration_paras - 1.96*3, ymax = duration_paras + 1.96*3), fill = "grey70")+
  geom_point(aes(y=duration),size = 0.4)+
  facet_grid(Industry~.,scales = "free")+
  labs(x="",y="Duration")

grid.arrange(p2,p1,p3,ncol=3)

ggplot(tmp)+geom_point(aes(x=duration,y=duration_paras))+geom_abline(intercept = 0, slope = 1)



# QQ plot of the observed and predicted values
qqdf <- data.frame(qqdata_paras=0,qqdata=0,type=0)
for(i in industries_to_check){
  plotdata%>%filter(type ==i)%>%select(discount_paras,discount) -> tmp
  plotdata%>%filter(type ==i)%>%select(discount_paras,discount) -> tmp
  qqdata_paras <- quantile(tmp$discount_paras,seq(0.01,1,0.001))
  qqdata <- quantile(tmp$discount,seq(0.01,1,0.001))
  tmpdf <- data.frame(qqdata_paras=qqdata_paras,qqdata = qqdata, type = i)
  qqdf <- rbind(qqdf,tmpdf)

}

qqdf <- data.frame(qqdata_paras=0,qqdata=0,type=0)
for(i in industries_to_check){
  plotdata%>%filter(type ==i)%>%select(prepT_paras,prepT) -> tmp
  plotdata%>%filter(type ==i)%>%select(prepT_paras,prepT) -> tmp
  qqdata_paras <- quantile(tmp$prepT_paras,seq(0.01,1,0.001))
  qqdata <- quantile(tmp$prepT,seq(0.01,1,0.001))
  tmpdf <- data.frame(qqdata_paras=qqdata_paras,qqdata = qqdata, type = i)
  qqdf <- rbind(qqdf,tmpdf)
  
}

qqdf <- merge(qqdf[-1,],data.frame(type = c(2,5,11,10,1),
                                      Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care")),
                  by="type",all.x = TRUE)

ggplot(qqdf[-1,])+
  geom_point(aes(x=qqdata_paras,y=qqdata))+
  facet_grid(Industry~.)+
  geom_abline(slope=1,intercept = 0) -> pT


# MAPE table ------
MAPE3ph <- c(0.43,0.54,0.14,0.53,0.57)
MAPE1ph <- c(0.14,0.27,0.37,0.47,0.38)
rbind(Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
      MAPEalpha = round(MAPE1,2),
      MAPET = round(MAPE2,2),
      MAPEd = round(MAPE3,2),
      MAPEalphanh = round(MAPE1ph,2),
      MAPEdnh = round(MAPE3ph,2)) ->mapetable


mapetable  <- mapetable[,c(4,1,2,5,3)]
rownames(mapetable) <- c("Industry","Holiday discount, $\\alpha_h$",
                         "Holiday launch date to holiday, $T_h$",
                         "Holiday duration, $d_h$",
                         "Non-holiday discount, $\\alpha_{nh}$",
                         "Non-holiday duration, $d_{nh}$")

print(xtable(mapetable), only.contents=FALSE, include.rownames=T, 
      include.colnames=T, floating=T, sanitize.rownames.function = identity,
      file = 'table.tex')

# estimated structural parameters table -----
rbind(Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care"),
 #     type = industries_to_check,
      c = signif(cvec,2),
      cnh = signif(cnhvec,2),
      thetaalpha=round(thetaalpha,2),
      thetaT=round(thetaT,2),
      thetad=round(thetad,2),
      betad=round(betad,2),
      betad_nh=round(betad_nh,2),
      betaalpha_nh=round(betaalpha_nh,2),
      sigma = round(gamma_h[industries_to_check,1],2),
      gamma = signif(gamma_h[industries_to_check,2],2),
      mch = round(chs,2),
      mcnh = round(cnhs,2),
      ratiovec = round(ratiovec,2),
      ratiosigvec = round(ratiosigvec,2),
      D1hvec = round(D1hvec,0),
      D1hsigvec = signif(D1hsigvec,2),
      D2hvec = signif(D2hvec,2),
      D2hsigvec = signif(D2hsigvec,2),
      D2nhvec = signif(D2nhvec,2),
      D2nhsigvec = signif(D2nhsigvec,2),
      mucpvec  = signif(mucpvec,2), 
      sigcpvec  = signif(sigcpvec,2), 
      #dsidxvec = round(dsidxvec,2),
      #dsidxsigvec = round(dsidxsigvec,2),
      deltaDvec = round(deltaDvec,2),
      deltaDsigvec = round(deltaDsigvec,2),
      Varidxvec = round(Varidxvec,2),
      dsidx2vec  = round(dsidx2vec,2) ,
      dsidx2sigvec = round(dsidx2sigvec,2),
      alterDvec = round(alterDvec,2),
      alterDsigvec = round(alterDsigvec,2),
      alterPvec = round(alterPvec,2),
      alterPsigvec = round(alterPsigvec,2)
) -> StructEstTable 

StructEstTable  <- StructEstTable[,c(4,1,2,5,3)]


rownames(StructEstTable) <- c("Industry","Holiday cost parameter, $c_{h}$","Non-holiday cost parameter, $c_{nh}$",
                              "Discount sensitivity in demand allocation, $\\theta{\\alpha}$",
                              "Launch time sensitivity in demand allocation, $\\theta_T$",
                              "Duration sensitivity in demand allocation, $\\theta_d$",
                              "Duration sensitivity, holiday full price customers, $\\beta_d$",
                              "Duration sensitivity, non-holiday full price customers, $\\beta_{d,nh}$",
                              "Discount sensitivity, non-holiday full price customers,$\\beta_{\\alpha,nh}$",
                              "Baseline holiday demand variance, $\\widehat{\\sigma}_h$",
                              "Launch time sensitivity in demand variance, $\\widehat{\\gamma}_h$",
                              "Holiday marginal service costs, $mc_{h}$",
                              "Non-holiday marginal service costs, $mc_{nh}$",
                              "Holidays Redemption Ratio, $r$","$\\sigma_{r}$",
                              "Holiday discount demand, $\\overline{D}_{1,h}$","$\\sigma_{\\overline{D}_{1,h}}$",
                              "Holiday full price demand, $\\overline{D}_{2,h}$","$\\sigma_{\\overline{D}_{2,h}}$",
                              "Non-holiday full price demand, $\\overline{D}_{2,nh}$","$\\sigma_{\\overline{D}_{2,nh}}$",
                              "Holiday margianl cost price ratio, $\\widehat{\\frac{mc_{h}}{p}}$",
                              "$\\sigma_{\\frac{mc_{h}}{p}}$",
                              "Holiday demand swings, $\\widehat{\\delta_D}$", "$\\sigma_{\\delta_D}$",
                              "Variance reduction index, $\\mathcal{R}$",
                              "Demand smoothing index, $\\mathcal{B}$","$\\sigma_{\\mathcal{B}}$",
                              "Total demand change, $\\widehat{\\Delta_D}$", "$\\sigma_{\\Delta_D}$",
                              "Total profit change, $\\widehat{\\Delta_P}$", "$\\sigma_{\\Delta_P}$")

print(xtable(StructEstTable), only.contents=TRUE, include.rownames=T, 
      include.colnames=T, floating=T, sanitize.rownames.function = identity,
      file = 'table.tex')


## mean boosting object -------
totaldpri <- vector()
totald <- vector()
typei <- vector()
for(i in industries_to_check){
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    {.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  if(i == 2){
    hol_data <- hol_data%>%filter(city!="XA")
  }else if(i %in% c(5)){
    hol_data <- hol_data%>%filter(platform!="ww",city!="GZ")
  }
  
  if(i %in% c(10)){
    

    totaldpri <- c(totaldpri,(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]],0))))
    totald <- c(totald,as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))
    typei <- c(typei,rep(i,dim(hol_data)[1]))

  }else if(i %in% c(11)){
    
    totaldpri <- c(totaldpri,(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]]/(hol_data$holiday.len+1),0)+ pmax(D2hvalpri[[i]],0))))
    totald <- c(totald,as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
  }else if(i %in% c(5)){
    
    totaldpri <- c(totaldpri,(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]],0)))) 
    totald <-  c(totald,as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
  }else if(i %in% c(1)){
    
    totaldpri <- c(totaldpri,(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]]/hol_data$holiday.len,0)+ pmax(D2hvalpri[[i]]/hol_data$holiday.len,0)))) 
    totald <-  c(totald,as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
  }else{
    
    totaldpri <- c(totaldpri,(as.numeric(Spri[[i]] + pmax(D2nhvalpri[[i]],0)+ pmax(D2hvalpri[[i]]/(hol_data$holiday.len+1),0))))
    totald <-  c(totald,as.numeric(hol_data$total_volume + pmax(0,D2nhval[[i]])+ pmax(0,D2hval[[i]])))
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
  }
}

# tmp <- vector()
# for(i in industries_to_check){
#   tmp <- c(tmp,dalter[[i]])
#   
# }
# plotdata <- data.frame(plotdata,dalter = tmp)

plotdata <- data.frame(totaldpri =totaldpri, totald = totald, type = typei)%>%
  group_by(type)%>%
  mutate(deltad = totald - totaldpri)%>%
  mutate(q90 = quantile(deltad,0.85),q10 = quantile(deltad,0.15))%>%
  filter(deltad<q90,deltad>q10)%>%
  merge(data.frame(type = c(2,5,11,10,1),
                     Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care")),by="type",all.x = TRUE)



colpats <- c("Total Demand (Deals)"= "#66c2a5",
             "Total Demand (No Deals)" = "#fc8d62")

ggplot(data=plotdata%>%filter())+
  geom_histogram(aes(x=totaldpri,y = ..density..,fill="Total Demand (No Deals)"),color="grey30")+
  #geom_line(aes(x=totaldpri,y = ..density..), stat = 'density', colour = '#fc8d62') +
  geom_histogram(aes(x=totald,y = ..density..,fill = "Total Demand (Deals)"),color="grey30")+ #blueish
 # geom_line(aes(x=totaldpri,y = ..density..), stat = 'density', colour = '#fc8d62') 
  #geom_line(aes(x=totald,y = ..density..), stat = 'density', colour = '#66c2a5') +
  facet_grid(.~Industry,scales = "free")+
  scale_fill_manual(values = colpats)+
  labs(x="Total Demand", y = "", fill="")+
  theme(legend.position = "bottom")+
  coord_flip() -> p1

ggplot(data=plotdata)+
  geom_histogram(aes(x=deltad,y = ..density..),fill="grey60",color="grey30")+
  facet_grid(.~Industry,scales = "free")+
  labs(x="Change in Demand", y = "")+
  coord_flip() -> p2

grid.arrange(p1,p2,nrow=2)
mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         mylegend, 
                         nrow=2,heights=c(16, 2)),
             p2 + theme(legend.position="none"),nrow=2) 


library(snpar)
cdf_data <- c(0,0,0)
for(i in industries_to_check){
  plotdata_i <- plotdata %>% fl(type ==i)
  kde(plotdata_i$deltad, h = bw.nrd0(plotdata_i$deltad)/10,kernel = c("gaus")) -> cdf_plot
  
  cdf_data <- rbind(cdf_data,data.frame(Fhat = cdf_plot$Fhat, x = cdf_plot$x, type = i) )
  
}
cdf_data <- cdf_data[-1,]

ggplot(cdf_data %>% 
         mt(DistType=ifelse(type%in%c(2,5),"Demand Increase","Demand Valley")), 
       aes( x = x, y = Fhat,
            color=DistType, group=as.factor(type)))+
  geom_line(aes(colour =DistType,group=as.factor(type)))+
  # geom_hline(yintercept = 0,  color="#993333")+
  #  geom_errorbar(aes(ymin=perc_cust_left_mean-1.96*perc_cust_left_sd, ymax=perc_cust_left_mean+1.96*perc_cust_left_sd), width=0.1, colour = 'blue')+
  theme_classic(base_size = 15) +
  xlim(c(-500,500))+
  theme(
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
    panel.grid.minor.y= element_line(colour = "grey", linetype = "dotted"))+
  # scale_y_continuous(breaks = seq(0.3, 0.6, by = 0.05))+
  labs(title = "Placebo Test for Cleaner Exits", x = "Sample Number", y = "% of Customers Left after Cleaner Exit")+
  theme(plot.title = element_text(hjust = 0.8))+
  theme(panel.grid.minor.y= element_line(colour = "grey", linetype = "dotted"))



## mean smoothing object ------
holval <- vector()
nonholval <- vector()
typei <- vector()

for(i in industries_to_check){
  hol_data <- dealsfull%>%filter(is.na(holiday)==0,type==i,prep.period>0,offering_duration<=40)%>%{.->>dealsfull_type}%>%
    filter(total_volume>quantile(dealsfull_type$total_volume,c(0.05)), total_volume<quantile(dealsfull_type$total_volume,c(0.95)))%>%
    {.->>sdop}%>%
    mutate(original_price = original_price/sd(original_price))
  if(i == 2){
    hol_data <- hol_data%>%filter(city!="XA")
  }else if(i %in% c(5)){
    hol_data <- hol_data%>%filter(platform!="ww",city!="GZ")
  }
  
  if(i %in% c(10)){
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
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
    
    holval <- c(holval,(-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
                  ((dealdemandhol+dealdemandnhol)))
    
    nonholval <- c(nonholval,(-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
                     ((nondealdemandhol+nondealdemandnhol)))
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
  }else if(i %in% c(11)){
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
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
    
    holval <- c(holval,(-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
                  ((dealdemandhol+dealdemandnhol)))
    
    nonholval <- c(nonholval,(-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
                     ((nondealdemandhol+nondealdemandnhol)))
    
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
  }else if(i %in% c(5)){
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
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
    
    
    holval <- c(holval,(-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
                  ((dealdemandhol+dealdemandnhol)))
    
    nonholval <- c(nonholval,(-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
                     ((nondealdemandhol+nondealdemandnhol)))
    
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
  }else if(i %in% c(1)){
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
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
    
    
    holval <- c(holval,(-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
                  ((dealdemandhol+dealdemandnhol)))
    
    nonholval <- c(nonholval,(-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
                     ((nondealdemandhol+nondealdemandnhol)))
    
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
  }else{
    
    Stotpri <- Spri[[i]]
    Stot <- hol_data$total_volume
    
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
    
    
    holval <- c(holval,(-dealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ dealdemandhol/hol_data$holiday.len)/
                  ((dealdemandhol+dealdemandnhol)))
    
    nonholval <- c(nonholval,(-nondealdemandnhol/(hol_data$offering_duration-hol_data$holiday.len)+ nondealdemandhol/hol_data$holiday.len)/
                     ((nondealdemandhol+nondealdemandnhol)))
    
    typei <- c(typei,rep(i,dim(hol_data)[1]))
    
    
  }
  

}




plotdata <- data.frame(holval =holval, nonholval = nonholval, type = typei)%>%
  group_by(type)%>%
  mutate(smoothed = ifelse(type!=2,holval - nonholval, (nonholval - holval)))%>%
  mutate(q90 = quantile(smoothed,0.8),q10 = quantile(smoothed,0.2))%>%
  filter(smoothed<q90,smoothed>q10)%>%
  merge(data.frame(type = c(2,5,11,10,1),
                   Industry = c("Casual Dine","Fine Dine","Photography","Outdoor","Body Care")),by="type",all.x = TRUE)



  tmp <- plotdata%>%filter(type==2)
  dsidx2_adj[[2]] <- (tmp$nonholval - tmp$holval)/abs(tmp$holval)

# 
# plotdata <- data.frame(holval =holval, nonholval = nonholval, type = typei)%>%
#   group_by(type)%>%
#   mutate(smoothed = ifelse(type!=2,holval - nonholval, ((nonholval) - holval)/holval))%>%
#   filter(type==2,nonholval>0,holval>0)%>%summarise(mean(smoothed))


colpats <- c("Demand Gap (Deals)"= "#66c2a5",
             "Demand Gap (No Deals)" = "#fc8d62")

ggplot(data=plotdata%>%filter())+
  geom_histogram(aes(x=nonholval,y = ..density..,fill="Demand Gap (No Deals)"),color="grey30")+
  #geom_line(aes(x=totaldpri,y = ..density..), stat = 'density', colour = '#fc8d62') +
  geom_histogram(aes(x=holval,y = ..density..,fill = "Demand Gap (Deals)"),color="grey30")+ #blueish
  # geom_line(aes(x=totaldpri,y = ..density..), stat = 'density', colour = '#fc8d62') 
  #geom_line(aes(x=totald,y = ..density..), stat = 'density', colour = '#66c2a5') +
  facet_grid(.~Industry,scales = "free")+
  scale_fill_manual(values = colpats)+
  labs(x="Demand Gap: Holiday-Non-holiday", y = "", fill="")+
  theme(legend.position = "bottom")+
  coord_flip() -> p1

ggplot(data=plotdata)+
  geom_histogram(aes(x=smoothed,y = ..density..),fill="grey60",color="grey30")+
  facet_grid(.~Industry,scales = "free")+
  labs(x="Change in Demand Gap", y = "")+
  geom_vline(xintercept=0,linetype =2)+
  coord_flip() -> p2

grid.arrange(p1,p2,nrow=2)
mylegend<-g_legend(p1)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                         mylegend, 
                         nrow=2,heights=c(16, 2)),
             p2 + theme(legend.position="none"),nrow=2) 



## variance moderation ----

# the holiday demand swings  ------
deltaDsig_ls <- vector()
deltaDvec_ls <- vector()
alterD_ls <- vector()
mcp_ls <- vector()
typei <- vector()
dsidx_ls <- vector()
ct <- 1
dsidx2_adj <- list()
dsidx2_adj <- dsidx2


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
  
  muh <- mean(holnormdemand[[i]][holnormdemand[[i]]<quantile(holnormdemand[[i]],ub)&
                                   holnormdemand[[i]]>quantile(holnormdemand[[i]],lb)])
  munh <- mean(nonholnormdemand[[i]][nonholnormdemand[[i]]<quantile(nonholnormdemand[[i]],ub)&
                                       nonholnormdemand[[i]]>quantile(nonholnormdemand[[i]],lb)])
  
  sigh <- sd(holnormdemand[[i]][holnormdemand[[i]]<quantile(holnormdemand[[i]],ub)&
                                  holnormdemand[[i]]>quantile(holnormdemand[[i]],lb)])
  signh <- sd(nonholnormdemand[[i]][nonholnormdemand[[i]]<quantile(nonholnormdemand[[i]],ub)&
                                      nonholnormdemand[[i]]>quantile(nonholnormdemand[[i]],lb)])
  
  covh <- cov(tt$nonval,tt$hval)
  
  # deltaDsig_ls<- c(deltaDsig_ls,sqrt(muh^2/munh^2*(sigh^2/muh^2+signh^2/munh^2-2*(covh)/(muh*munh))))
  # deltaDvec_ls<- c(deltaDvec_ls,muh/munh + signh*muh/munh^3-covh/munh^2-1)
  deltaDvec_ls<- c(deltaDvec_ls,rep(muh/munh + signh*muh/munh^3-covh/munh^2-1,dim(hol_data)[1]))
  alterD_ls <- c(alterD_ls, dalter[[i]])
  dsidx_ls <- c(dsidx_ls, dsidx2[[i]])
  mcp_ls <- c(mcp_ls,cvec[ct]/hol_data$original_price)
  typei <- c(typei,rep(i,dim(hol_data)[1]))
  ct <- ct+1
}

plotdata <- data.frame(type = typei, deltaDvec_ls = deltaDvec_ls, mcp_ls=mcp_ls,
                       alterD_ls=alterD_ls,dsidx_ls)%>%
  mutate(totaldelta = (1+deltaDvec_ls)*(1+alterD_ls)-1)


ggplot(data=plotdata, aes(x=deltaDvec_ls,y=mcp_ls,color=alterD_ls))+
  geom_point()


lm1 <- (lm(data=plotdata,alterD_ls~deltaDvec_ls+mcp_ls+as.factor(type)))
lm2 <-  (lm(data=plotdata%>%filter(!type%in%c(2,10)),(dsidx_ls)~deltaDvec_ls+mcp_ls))
lm3 <- lm(data=plotdata%>%filter(dsidx_ls<0),dsidx_ls ~ totaldelta+mcp_ls)

stargazer::stargazer(lm1,lm2,lm3)


# the deal designs -- summaries -----

data.frame(type = c(2,5,10,11,1),
           thetaalpha = c(-2.07,4.86,4.35,5.32,2.92),
           thetaalphahat = c(-3.02,-7.87,-1.97,-1.16,-1.6),
           thetaT = c(1.27,-3.77,0.72,-0.043,-0.20),
           ThetaThat = c(1.23,1.07,1.05,1.00,1.24),
           thetad = c(-4.8,0.39,-2.5,-0.83,-0.62),
           thetadhat = c(0.9,1.24,1.7,1.20,1.2))


# check discount levels by similar capacity 


for(i in industries_to_check){
  
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
  geom_point(aes(x=as.factor(volumebins),y=disc10mu,color = "Outdoor"))+
  geom_point(aes(x=as.factor(volumebins),y=disc2mu,color = "Casual Dine"))+
  geom_point(aes(x=as.factor(volumebins),y=disc11mu,color = "Photography"))+
  geom_point(aes(x=as.factor(volumebins),y=disc5mu,color = "Fine Dine"))+
  geom_point(aes(x=as.factor(volumebins),y=disc1mu,color = "Body Care"))+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc10mu-0.5*disc10sd, ymax = disc10mu+0.5*disc10sd,color = "Outdoor",collbs = colpats[1]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc2mu-0.5*disc2sd, ymax = disc2mu+0.5*disc2sd,color="Casual Dine",collbs = colpats[1]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc11mu-0.5*disc11sd, ymax = disc11mu+0.5*disc11sd,color="Photography",collbs = colpats[2]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc5mu-0.5*disc5sd, ymax = disc5mu+0.5*disc5sd,color="Fine Dine",collbs = colpats[2]),width=0.1,alpha=0.8)+
  geom_errorbar(aes(x=as.factor(volumebins), ymin = disc1mu-0.5*disc1sd, ymax = disc1mu+0.5*disc1sd,color = "Body Care",collbs = colpats[2]),width=0.1,alpha=0.8)+
  facet_grid(preptbins~.)+
  scale_color_manual(values = colpats)+
  labs(x="Capacity",y="Discount", color="Industry", facet="Release Date to Holiday")+
  theme(legend.position = "bottom",legend.title = element_blank()) -> palpha

# can add in another metric, the % of cover or higher than the other


merge(merge(merge(merge(optimparas[[10]][,c(4,5,6,7,8)]%>%
                          filter(offering_duration<40)%>%
                          mutate(volumebins = total_volume - (total_volume%%40))%>%
                          mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                          mutate(preptbins = prep.period - (prep.period%%3))%>%
                          mutate(pricebins = original_price - (original_price%%50))%>%
                          mutate(alphabins = discount - (discount%%0.2))%>%
                          rename(disc10 = discount, T10 = prep.period),
                        optimparas[[2]][,c(4,5,6,7,8)]%>%
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
                  optimparas[[11]][,c(4,5,6,7,8)]%>%
                    filter(offering_duration<40)%>%
                    mutate(volumebins = total_volume - (total_volume%%40))%>%
                    mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                    mutate(preptbins = prep.period - (prep.period%%3))%>%
                    mutate(pricebins = original_price - (original_price%%50))%>%
                    mutate(alphabins = discount - (discount%%0.2))%>%
                    rename(disc11 = discount, T11 = prep.period),
                  by=c("alphabins","volumebins"),all=TRUE)%>%
              select(volumebins,alphabins, T11,T2,T10),
            optimparas[[1]][,c(4,5,6,7,8)]%>%
              filter(offering_duration<40)%>%
              mutate(volumebins = total_volume - (total_volume%%40))%>%
              mutate(durationbins = offering_duration - (offering_duration%%3))%>%
              mutate(preptbins = prep.period - (prep.period%%3))%>%
              mutate(pricebins = original_price - (original_price%%50))%>%
              mutate(alphabins = discount - (discount%%0.2))%>%
              rename(disc1 = discount, T1 = prep.period),
            by=c("alphabins","volumebins"),all=TRUE)%>%
        select(volumebins,alphabins, T11,T2,T10,T1),
      optimparas[[5]][,c(4,5,6,7,8)]%>%
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
  theme(legend.position = "bottom",legend.title = element_blank())-> pT
# can add in another metric, the % of cover or higher than the other




# duration  ----------
merge(merge(merge(merge(optimparas[[10]][,c(4,5,6,7,8)]%>%
                          filter(offering_duration<40)%>%
                          mutate(offering_duration = offering_duration-prep.period)%>%
                          mutate(volumebins = total_volume - (total_volume%%40))%>%
                          mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                          mutate(preptbins = prep.period - (prep.period%%3))%>%
                          mutate(pricebins = original_price - (original_price%%50))%>%
                          mutate(alphabins = discount - (discount%%0.2))%>%
                          rename(disc10 = discount, T10 = prep.period, d10 = offering_duration),
                        optimparas[[2]][,c(4,5,6,7,8)]%>%
                          filter(offering_duration<40)%>%
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
                  optimparas[[11]][,c(4,5,6,7,8)]%>%
                    filter(offering_duration<40)%>%
                    mutate(offering_duration = offering_duration-prep.period)%>%
                    mutate(volumebins = total_volume - (total_volume%%40))%>%
                    mutate(durationbins = offering_duration - (offering_duration%%3))%>%
                    mutate(preptbins = prep.period - (prep.period%%3))%>%
                    mutate(pricebins = original_price - (original_price%%50))%>%
                    mutate(alphabins = discount - (discount%%0.2))%>%
                    rename(disc11 = discount, T11 = prep.period, d11 = offering_duration),
                  by=c("alphabins","volumebins"),all=TRUE)%>%
              select(volumebins,alphabins, d11,d2,d10),
            optimparas[[1]][,c(4,5,6,7,8)]%>%
              filter(offering_duration<40)%>%
              mutate(offering_duration = offering_duration-prep.period)%>%
              mutate(volumebins = total_volume - (total_volume%%40))%>%
              mutate(durationbins = offering_duration - (offering_duration%%3))%>%
              mutate(preptbins = prep.period - (prep.period%%3))%>%
              mutate(pricebins = original_price - (original_price%%50))%>%
              mutate(alphabins = discount - (discount%%0.2))%>%
              rename(disc1 = discount, T1 = prep.period, d1 = offering_duration),
            by=c("alphabins","volumebins"),all=TRUE)%>%
        select(volumebins,alphabins, d11,d2,d10,d1),
      optimparas[[5]][,c(4,5,6,7,8)]%>%
        filter(offering_duration<40)%>%
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
  filter(is.nan(d10mu)+is.nan(d2mu)+is.nan(d11mu)+is.nan(d1mu)+is.nan(d5mu)==0) -> d_cap_prept

colpats <- c("#66c2a5",
             "#fc8d62",
             "#8da0cb",
             "#e78ac3",
             "#a6d854")

colpats <- c("Outdoor"= "#66c2a5",
             "Casual Dine" = "#fc8d62",
             "Photography"= "#66c2a5",
             "Fine Dine" ="#fc8d62",
             "Body Care"= "#fc8d62")


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

