placement<- read.csv("~/Downloads/Placement_Data_Full_Class.csv")


placement<-placement[,-15]#salary does not affect placement

table(placement$status)
table(placement$gender,placement$status)
#more male students were placed than female students

library(ggplot2)
library(caret)

#visulization 
ggplot(placement,aes(hsc_p,status))+geom_point(aes(color=hsc_s))
ggplot(placement,aes(degree_p,status))+geom_point(aes(color=degree_t))
ggplot(placement,aes(ssc_p,status))+geom_point()
ggplot(placement,aes(ssc_p,status))+geom_point(aes(color=ssc_b))
ggplot(placement,aes(hsc_p,status))+geom_point(aes(color=hsc_b))
ggplot(placement,aes(etest_p,status))+geom_point()
ggplot(placement,aes(mba_p,status))+geom_point(aes(color=specialisation))

#changing the required variables into factors
placement$gender<-as.factor(placement$gender)
placement$ssc_b<-as.factor(placement$ssc_b)
placement$hsc_b<-as.factor(placement$hsc_b)
placement$hsc_s<-as.factor(placement$hsc_s)
placement$degree_t<-as.factor(placement$degree_t)
placement$workex<-as.factor(placement$workex)
placement$specialisation<-as.factor(placement$specialisation)
placement$status<-as.factor(placement$status)



#dummy<-dummyVars(status~.,data=placement)
#new<-data.frame(predict(dummy,newdata = placement))
#change<-cbind(new,placement$status)


#splitting the data 
intrain<-createDataPartition(placement$status,p=0.7,list = FALSE)
train<-placement[intrain,]
test<-placement[-intrain,]

model1<-glm("status~ssc_p+hsc_p+degree_p+mba_p+workex",data=train,family = binomial)
p1<-predict(model1,newdata = train,type = 'response')
pred<-ifelse(p1>0.5,1,0)
table(pred,train$status)


new<-predict(model1,test)
pred2<-ifelse(new>0.5,1,0)
table(pred2,test$status)

#accuracy=82%
#sensitivity=0.8
#specificity=0.84
#precision=0.69
#f1score=0.74

