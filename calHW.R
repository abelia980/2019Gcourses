setwd("D:\\graduate_student\\2019-2020one\\计量\\HW")
data<-read.csv("data.csv")
colnames(data)
####all regression
#'time_dummy','city_dummy',
data$time = as.factor(data$time)
dataselect<-data[c('diff','time','labor','city','illiteracy','urbanization','PERGDP'
                   ,'edu_con','edu_install','elec','GDP','edu_year')]
fit1<-lm(dataselect$edu_year~.,data = dataselect)
summary(fit1)
summary(dataselect)
theme <-  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
data$time_dummy <- as.factor(data$time_dummy)
data$city_dummy <- as.factor(data$city_dummy)
data$time <- as.factor(data$time)
a<-as.data.frame(aggregate(data$PERGDP,list(data$city),mean))
new_levels<-a[sort(a$x,index.return=TRUE,decreasing = T)$ix,1]
p<-ggplot(data = data,aes(x = factor(data$city, level = new_levels),y = data$PERGDP))+geom_boxplot(aes(fill = data$city_dummy))+theme
p<-p+xlab("城市")+ylab("人均GDP")
p<-p+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank())
p

####折线图####
library(ggplot2)
b<-as.data.frame(aggregate(data$labor,list(data$city_dummy,data$time),mean))
b$Group.1<-as.factor(b$Group.1)
b$Group.2<-as.numeric(as.character(b$Group.2))
p1<-ggplot(data = b,aes(x = b$Group.2,y=b$x))+geom_line(aes(color = b$Group.1),size=1.5)
p1<-p1+xlab("时间")+ylab("劳动力流动")
p1<-p1+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank())
p1


####稳定性检验###
period1 <- rep(0,310)
data$period1<-period1
#data$time<-as.numeric(as.character(data$time))
data$period1<-ifelse(data$time >= 2010,1,0)
data$period2<-ifelse(data$time >= 2009,1,0)
data$period3<-ifelse(data$time >= 2008,1,0)
data$period4<-ifelse(data$time >= 2012,1,0)
data$period5<-ifelse(data$time >= 2013,1,0)

data$period5_city <- data$period5*data$city_dummy

fit2<-lm(data$edu_year~time+city+diff+labor+illiteracy+urbanization
         +PERGDP+edu_install+edu_con+elec,data = data)
summary(fit2)

plot(y)
lines(c(1:6),y,type = "b", pch = 22, col = "blue", lty = 2)
lines(c(1,1),period1_coef,type = "b", pch = 20, col = "red", lty = 2)
lines(c(2,2),period2_coef,type = "b", pch = 20, col = "red", lty = 2)
lines(c(3,3),period3_coef,type = "b", pch = 20, col = "red", lty = 2)
lines(c(4,4),period_coef,type = "b", pch = 20, col = "red", lty = 2)
lines(c(5,5),period4_coef,type = "b", pch = 20, col = "red", lty = 2)
lines(c(6,6),period5_coef,type = "b", pch = 20, col = "red", lty = 2)
abline(h = 0, col = "red")


####异质性检验#####
index_1 = data$city %in% c("辽宁","江苏","山东","吉林","河北","重庆","福建","上海","北京","天津","内蒙古","广东","黑龙江","宁夏","新疆")
data_1 = data[index_1,]
data_2 = data[!index_1,]
data_1$time = as.numeric(as.character(data_1$time))
data_2$time = as.numeric(as.character(data_2$time))
fit_1<-lm(edu_year~time+city+diff+labor+illiteracy+urbanization
         +PERGDP+edu_install+edu_con+elec,data = data_1)
summary(fit_1)

fit_2<-lm(edu_year~time+diff+city+labor+illiteracy+urbanization
          +PERGDP+edu_install+edu_con+elec,data = data_2)
summary(fit_2)


library(ggplot2)
plotzhexian<-function(data_1){
  b_1<-as.data.frame(aggregate(data_1$labor,list(data_1$city_dummy,data_1$time),mean))
  b_1$Group.1<-as.factor(b_1$Group.1)
  b_1$Group.2<-as.numeric(as.character(b_1$Group.2))
  p1<-ggplot(data = b_1,aes(x = b_1$Group.2,y=b_1$x))+geom_line(aes(color = b_1$Group.1),size=1.5)
  p1<-p1+xlab("时间")+ylab("劳动力流动")
  p1<-p1+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank())
  p1<-p1+scale_fill_discrete(labels=c("控制组", "对照组"))+theme(legend.title=element_blank())
  p1
  }
plotzhexian(data_2)
