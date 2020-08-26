LabeltoEng <- function(deal_data){
  
  toMatch.ent <- c("温泉","电影","影院","影城","酒店","宾馆","民宿","客栈","旅馆","度假村","剧场","相声","歌舞","KTV","电玩","游泳","保龄球","溜冰","养生","健身","瑜伽","户外","灯会","冰雪","乐园","采摘","高尔夫","滑雪","游","自由行","旅行",
                   "卡丁车","舞蹈","乐高","瑜伽","射箭","戏院","DIY","F1","御湖山庄","修身堂","滑稽剧","冰雕","好乐迪","剧院","雨花胜境","影都","风景区","农场","射击","K歌","PTV",
                     "公寓", "精品公寓", "真人CS", "F1", "度假","公园", "客房", "海洋世界", "床房","音乐会", "滑冰","激光水秀", "4D", "标.*间", "标.*房", "K BOX","王国","动物园")
  toMatch.res <- c("酒楼","饭店","料理","自助餐","自助晚餐","美食","餐厅","食府","烧烤","火锅","海鲜","菜","饭庄","香锅","烤肉","干锅","大骨","麻辣","汉堡","寿司","鱼","烤鸭","厨","肉","拉面","咖啡","营养","咖喱","西餐","佳肴","法式","西点",
                   "牛排","芋儿鸡","茶饮","意式","欧式","川味","粥","日料","和牛","饺子","涮","汤.*味","味.*汤","瓦罐汤","湘味","烹","焖","小吃","锅.*味","味.*锅","越式","比萨","鸡.*味","味.*鸡","小馆","素食","西式","味道","羊","烤鲜",
                   "蓉城老妈","上上生活馆","彩蝶名廷","蜀之鼎","锦宴楼","山珍堡","三明治","麒麟家","俏江南","蓝玛赫","周黑鸭","花家怡园","许留山","羊蝎子","乐蛙蛙","赛百味","老鸭汤","闻湘","京味","饺","盐水鸭","江南公社",
                   "沸腾渔乡","蚝","蟹","铁板烧","排骨","酒家","外婆家","包子","汤包","同庆楼","圆桌武士","微风新天地","桂花鸭","红楼人家","美满蕉园","过桥米线","荞面","炭烤","毛血旺")
  toMatch.salon <- c("SPA","美发","发廊","造型","烫染","脱毛","美容","美胸","美体","美甲","纤体美颜","私汤","按摩","护理","足道","足浴","洗剪吹","纤体","Massage")
  toMatch.ls <- c("数码影印","摄影","影像","写真","定制西服","西服定制","洗车","眼镜","灸" ,"健康","体检","教育","钢琴","早教","礼服")
  toMatch.shop <- c("配送","包邮","邮")
  
  # add in categories for danping ---- 
  vouchers_dp <- deal_data%>%filter(platform%in%c("大众点评团"))
  
  if(dim(vouchers_dp)[1]!=0){
    vouchers_dp[grep(paste(toMatch.ent,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Entertainment, Fitness"
    vouchers_dp[grep(paste(toMatch.res,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Food, Restaurants"
    vouchers_dp[grep(paste(toMatch.salon,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Beauty, Salon"
    vouchers_dp[grep(paste(toMatch.ls,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Life Style, Daily Services"
    vouchers_dp[grep(paste(toMatch.shop,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Online Shopping, Tickets, cards top up"
    vouchers_dp[vouchers_dp[,"category"]=="","category"] <- "Others"
    
  }
  
  
  
  # add in categories for meituan ----
  vouchers_mt <- deal_data%>%filter(platform=="美团")
  
  if(dim(vouchers_mt)[1]!=0){
    vouchers_mt[vouchers_mt$category == "其它","category"] = "其他"
    
    # using only meituan, coz 大众 has no pre-labeled category 
    
    vouchers_mt[vouchers_mt$category == "其他","category"]  <- "Others"
    vouchers_mt[vouchers_mt$category == "购物卡充值,票务,邮购","category"]  <- "Online Shopping, Tickets, cards top up"
    vouchers_mt[vouchers_mt$category == "娱乐,健身运动","category"] <- "Entertainment, Fitness"
    vouchers_mt[vouchers_mt$category == "美容,美发","category"] <- "Beauty, Salon"
    vouchers_mt[vouchers_mt$category == "美食","category"] <- "Food, Restaurants"
    vouchers_mt[vouchers_mt$category == "生活,日常服务","category"] <- "Life Style, Daily Services"
    
    vouchers_mt[grep(paste(toMatch.ent,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Entertainment, Fitness"
    vouchers_mt[grep(paste(toMatch.res,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Food, Restaurants"
    vouchers_mt[grep(paste(toMatch.salon,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Beauty, Salon"
    vouchers_mt[grep(paste(toMatch.ls,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Life Style, Daily Services"
    vouchers_mt[grep(paste(toMatch.shop,collapse="|"), vouchers_dp$deal_info, value=FALSE),"category"] <- "Online Shopping, Tickets, cards top up"
    vouchers_mt[vouchers_dp[,"category"]=="","category"] <- "Others"
    
  }
  
  # add in categories for wowo ----
  vouchers_ww <- deal_data%>%filter(platform=="窝窝团")
  
  if(dim(vouchers_ww)[1]!=0){
    remove_cat <- c("蛋糕甜品","食品饮料","化妆品","休闲其它","家居用品","饰品装饰","服装鞋帽",
                    "数码","教育培训","报刊杂志","其它网购","抽奖","配件饰品","孕婴儿童","数码家电",
                    "其它生活","旅游")
    # remove 台球(2),
    remove_keyword <- c("台球","全国配送","全国包邮")
    
    # using only meituan, coz 大众 has no pre-labeled category 
    
    vouchers_ww <- vouchers_ww%>%filter(!category%in%remove_cat)
    vouchers_ww <- vouchers_ww[-grep(paste(remove_keyword,collapse="|"), vouchers_ww$deal_info, value=FALSE),]
    #vouchers_ww[vouchers_ww$category %in% c(""),"category"]  <- "Others"
    vouchers_ww[vouchers_ww$category %in% c("运动健身","KTV","电影展览","其他娱乐","聚会欢畅","赛事演出","游乐游艺","酒店住宿","酒店") ,"category"] <- "Entertainment, Fitness"
    vouchers_ww[vouchers_ww$category %in% c("美容美发","养生按摩","健康护理","美容塑形") ,"category"] <- "Beauty, Salon"
    vouchers_ww[vouchers_ww$category %in% c("地方菜系","火锅烧烤","自助海鲜","日式韩系","其他餐饮","自助","日韩亚系","快餐休闲","西餐国际"),"category"] <- "Food, Restaurants"
    vouchers_ww[vouchers_ww$category %in% c("汽车护理","婚纱摄影","摄影写真") ,"category"] <- "Life Style, Daily Services"
    
  }
  
  
  deal_data_cat <- rbind(vouchers_mt,vouchers_dp,vouchers_ww)
  return(deal_data_cat)
}
