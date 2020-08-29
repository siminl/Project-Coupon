# Data Preparation: add in detailed category ------


FinerCategory <- function(deal_data){
  
  toMatch.movie <- c("电影","影院","影城","4D")
  toMatch.theatre <- c("剧场","相声","歌舞","音乐会","戏院","剧院","卡通剧","滑稽剧")
  toMatch.hotel <- c("酒店.*房","酒店.*住","商务酒店","宾馆","民宿","客栈","旅馆","度假.*房","客房", "床房","标.*间", "标.*房","公寓", "精品公寓","锦江之星","度假村","御湖山庄","公寓", "精品公寓")
  toMatch.indoor <- c("KTV","K BOX","电玩","游泳","保龄球","休闲会馆","运动","欢唱","俱乐部","桌游","游戏机","射箭","DIY","好乐迪","射击","K歌","PTV","卡丁车")
  toMatch.minicourses <- c("健身","瑜伽","教育","钢琴","早教","课程","舞蹈")
  toMatch.travel <- c("假.*游","自由行","旅行","旅游","度假","游.*行","自助游","观光.*游","上海.*游","香港.*游","西湖.*游","自驾游")
  toMatch.casualdine <- c("酒店.*食","酒店.*餐","酒楼","饭店","料理","自助.*餐","美食","餐厅","食府","烧烤","火锅","海鲜","菜","饭庄","香锅","烤肉","干锅","大骨","麻辣","汉堡","鱼","厨","肉","拉面","营养","咖喱","西餐","佳肴",
                          "芋儿鸡","茶饮","川味","粥","饺","涮","汤.*味","味.*汤","瓦罐汤","湘味","烹","焖","小吃","锅.*味","味.*锅","鸡","味.*餐","餐.*味","面","宴","虾",
                          "蓉城老妈","上上生活馆","彩蝶名廷","蜀之鼎","锦宴楼","山珍堡","三明治","麒麟家","周黑鸭","花家怡园","排骨","牛","芙蓉锦汇","来伊份","簋街","外婆家","灶丰年间","沈大成",
                          "红楼人家","川香情缘","湘西味道","大漠","风波庄","汉拿山","小馆","素食","味道","羊","烤鲜","羊蝎子","乐蛙蛙","赛百味","老鸭汤","闻湘","京味","饺","盐水鸭","江南公社",
                          "沸腾渔乡","蚝","蟹","铁板烧","排骨","酒家","外婆家","包子","汤包","同庆楼","圆桌武士","微风新天地","桂花鸭","红楼人家","美满蕉园","过桥米线","荞面","炭烤","毛血旺")
  toMatch.finedine <- c("寿司","烤鸭","私.*厨","咖啡","西餐","法式","西点","牛排","日料","和牛","比萨","披萨","俏江南","许留山","蓝玛赫","意式","欧式","越式","酒吧","西式","下午茶","料理")
  toMatch.outdoor <- c("真人CS", "F1", "公园","海洋世界","激光水秀","王国","动物园","户外","灯会","冰雪","乐园","采摘","高尔夫","滑雪","游船","花会","溜冰","滑冰",
                       "温泉","华侨城","葵园","景区","海底世界","踏青","大观园","门票","游乐场","亲子园","职业体验","夜游","弘阳欢乐世界","游览园","游戏币","乐高","冰雕","雨花胜境","浴城","影都","风景区","农场",
                       "度假","公园","激光水秀")
  toMatch.photography <- c("摄影","写真","数码影印","影像")
  toMatch.bodycare <- c("SPA","脱毛","美容","美胸","美体","美甲","纤体美颜","私汤","按摩","护理","造型","灸","足道","足浴","纤体","养生会所","浴足","养生","修身堂","Massage") 
  toMatch.hairsalon <- c("美发","发廊","造型.*发","发.*造型","烫染","洗剪吹")
  toMatch.mixed <- c("定制西服","西服定制","西装定制","量身定制","洗车","汽车维修","眼镜","体检","灸" ,"健康","礼服")
  toMatch.shop <- c("配送","包邮","邮","有龙凤爪","欧睿宇邦橱柜","兰缪官网")
  
  toMatch.list <- list(toMatch.movie,toMatch.theatre,toMatch.hotel,toMatch.indoor,toMatch.minicourses,
                       toMatch.travel,toMatch.casualdine,toMatch.finedine,toMatch.outdoor,toMatch.photography,
                       toMatch.bodycare,toMatch.hairsalon,toMatch.mixed,toMatch.shop)
  
  ServiceNames.list <- c("Movie","Theatre","Hotel","Indoor","Mini Courses",
                         "Travel","Casual Dine","Fine Dine","Outdoor","Photography",
                         "Body Care","Hair Salon", "Mixed", "Shop")
  
  deal_data$platform_cat = NA
  
  for(i in 1:length(toMatch.list)){
    tmp <- sapply(toMatch.list[[i]], regexpr, deal_data$deal_info, ignore.case=TRUE)
    deal_data[apply(tmp, 1, FUN=max)>0,"platform_cat"] <-  ServiceNames.list[i]
  }
  
  deal_data[is.na(deal_data[,"platform_cat"])==TRUE,"platform_cat"] <- "unmatched"
  
  deal_data <- deal_data%>%filter(!platform_cat%in%c("Shop","unmatched","Mixed"))

  
  return(deal_data)
}





