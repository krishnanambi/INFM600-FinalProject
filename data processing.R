f<-read.csv(file.choose())
#splitting salary into min and max for monthly and annual salary column
f$Annual_Salary<-as.character(f$Annual_Salary)
f$annual<-strsplit(f$Annual_Salary,'-')
f$Monthly_Salary<-as.character(f$Monthly_Salary)
f$monthly<-strsplit(f$Monthly_Salary,'-')
for (i in c(1:NROW(f$annual))){
  min1=trimws(f$annual[[i]][1])
  max1=trimws(f$annual[[i]][2])
  #stripping unwanted characters and converting to appropriate value based upon suffix
  if (str_sub(min1,-1,-1)=='K'){
    min1<-as.numeric(str_sub(min1,2,-2))*1000
  }
  else if (str_sub(min1,-1,-1)=='M'){
    min1<-as.numeric(str_sub(min1,2,-2))*1000000
  }
  else{
    min1<-as.numeric(str_sub(min1,2,-1))
  }
  
  if (str_sub(max1,-1,-1)=='K'){
    max1<-as.numeric(str_sub(max1,2,-2))*1000
  }
  else if (str_sub(max1,-1,-1)=='M'){
    max1<-as.numeric(str_sub(max1,2,-2))*1000000
  }
  else{
    max1<-as.numeric(str_sub(max1,2,-1))
  }
  
  # averaging the min and max values
  
  f$annual[[i]]<-(min1+max1)/2
  
  min2=trimws(f$monthly[[i]][1])
  max2=trimws(f$monthly[[i]][2])
  if (str_sub(min2,-1,-1)=='K'){
    min2<-as.numeric(str_sub(min2,2,-2))*1000
  }
  else if (str_sub(min2,-1,-1)=='M'){
    min2<-as.numeric(str_sub(min2,2,-2))*1000000
  }
  else{
    min2<-as.numeric(str_sub(min2,2,-1))
  }
  
  if (str_sub(max2,-1,-1)=='K'){
    max2<-as.numeric(str_sub(max2,2,-2))*1000
  }
  else if (str_sub(max2,-1,-1)=='M'){
    max2<-as.numeric(str_sub(max2,2,-2))*1000000
  }
  else{
    max2<-as.numeric(str_sub(max2,2,-1))
  }
  f$monthly[[i]]<-(min2+max2)/2
}

#creating linear regression model for interpolting missing or dubious salary value

#removing na and zero values(which might cause subscriber to salary ratio to be inifinity) in order to create linear regression model without anomalous data
x1<-f$Subscribers[!is.na(f$Subscribers) & !is.na(f$Views) & f$Subscribers!=0 & f$Views!=0 & f$monthly!=0]
x2<-f$Views[!is.na(f$Subscribers) & !is.na(f$Views) & f$Subscribers!=0 & f$Views!=0 & f$monthly!=0]
f$monthly<-as.numeric(f$monthly)
f$annual<-as.numeric(f$annual)
y1<-f$monthly[!is.na(f$Subscribers) & !is.na(f$Views) & f$Subscribers!=0 & f$Views!=0 & f$monthly!=0]
y2<-f$annual[!is.na(f$Subscribers) & !is.na(f$Views) & f$Subscribers!=0 & f$Views!=0 & f$monthly!=0]

#linear regression model for predicting monthly salary based upon subscribers and views
mod1 <- lm(y1 ~ x1+x2)
#linear regression model for predicting annual salary based upon subscribers and views
mod2 <- lm(y2 ~ x1+x2)

#deriving sample statistics which will be used for identifying outliers
mean_sub<-mean(x1)
mean_views<-mean(x2)
mean_monthly<-mean(y1)
ratio_mean<-mean(x1/y1)
sd_ratio <- sd(x1/y1)*sqrt((length(x1/y1)-1)/(length(x1/y1)))


for (i in c(1:NROW(f$annual))){
  ratio<-f$Subscribers[i]/f$monthly[i]
  p_value<-pnorm(ratio,ratio_mean,sd_ratio)
  #finding p-value for each channel's subscriber/salary ratio and replacing the outlier or missing value with the value
  #predicted by the linear regresion model
  
  if (is.na(p_value)){
    print(f$Subscribers[i])
    print(f$monthly[i])
  }
  
  else if (p_value <0.05){
    f$monthly[i]<-predict(mod1,data.frame(train_x =c(f$Subscribers,f$Views)))[i]
    f$annual[i]<-predict(mod1,data.frame(train_x =c(f$Subscribers,f$Views)))[i]
  }
  
}
