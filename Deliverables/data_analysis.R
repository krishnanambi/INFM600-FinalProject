#After the preparation and cleaning of the data extracted from the youtube API, we have analyzed the data on a few  
#aspects such as,
#1.Which are the most popular youtube genres and their relationship with the views generated to see if the users are
#actually interested on the videos based on genres or are those just random views generated.
#2.Is there a correlation between the number of views and subscribers count to identify the popular youtube channels
#3.Identify the growing youtube channels which have less scubscribers but are generating more views and outreach in 
#the youtube community.
#These will help us identify the relevant youtube channels for youtube brand advertisements and collaborations.
#---------------------------------------------------------------------------------------------------------------#

#Reading the data set
data<-read.csv(file.choose())
library(ggplot2)

#The following steps will feed the relevant data into variables required to analyze question 1 and provide an overall
#summary of the dataset
Genre<-data$Genre
views<-data$Views
subscribers<-data$Subscribers
View(data)
summary(data)

#The following plots help us identify the trend of various genres found in the youtube community based on the number 
#of views generated. The box plot helps us analyze the variabilty of the genre dataset and the outliers in various genres.
#It heps us identify the distribution of these genres and their skewness.

ggplot(data, aes(Genre, Views))+geom_bar(stat="identity")
plot(data$Genre,data$Views)


#[1.]**********************************************************************************************************************
#As we could see from the above plot, the genres "Entertainment","Music" and "Games" are the top three famous genres. 
#But we need to identify if there is actually a sigfnificant relationship between these popular genres and their generated 
#views. This will help us identify the right genre market for the Brands to collaborate with.

anova_results<-aov(data$Views~data$Genre)
summary(anova_results)

#The anova summary result above gives us a p-value(5.41e-10) way less than our threshold value of 0.05 and show us 
#that there is significant evidence of a relationship between the popularity of genres and the number of views 
#associated with it.

#[2.]**********************************************************************************************************************

#The following regression analysis will help us identify the relation between subscriber and views to identify if 
#these factors actually have an impact on youtube channel's popularity. We intend to study this by analyzing the strength 
#of these variables and whether they have a negative or positive correlation.
subscriber_view_correlation<-cor(data$Subscribers,data$Views)
cor.test(data$Views, data$Subscribers)

#The correlation test shows us that the number of subscriber and the number of views are strongly correlated with a
#correlation coeffcient of R=0.77. Therefore, the variable "number of subscribers" can be considered as a strong predictor 
#of the youtube channel's "number of views". This idicates that with an increment in the number of subscribers, there 
#would be a strong positive increment in the number of views too. There is also a significant relationship with p-value<0.05.

#[3.]**********************************************************************************************************************
#In order to identify the budding or growing youtube channel, we first calculated the ratio of subscriber count to 
#view count.The ratio calculated would be less for subscribers with more views and it would be greater for channels 
#with more subscribers with less views. The channels with less subscriber and more views are generally considered as 
#growing youtube channels,however, we need to identify if there is signifcant evidence in assuming the same.  

ratio<- subscribers/views
growing<-list()
#This is to identify the mean of these ratio values by eliminating the null values
ratio_avg<-mean(ratio[!is.na(ratio) & ratio<1])
#This is to identify the standard deviation of these ratio values by eliminating the null values
ratio_sd<-sd(ratio[!is.na(ratio) & ratio<1])

for (i in c(1:NROW(data$Subscribers))){
  
  ratio_sample<-ratio[i]
  if (is.na(ratio_sample)==FALSE && is.infinite(ratio_sample)==FALSE){
    p_value<-pnorm(ratio_sample, mean = ratio_avg, sd = ratio_sd)
    print(p_value<0.05)
    if (p_value<0.05){
      print(ratio_sample[i])
    }
  }
}
#From the above pvalues generated, we can conclude that there is not enough eveidence to claim the hypothesis
#that youtube channels with less subscribers but more views can be identified as growing youtube channels. We 
#observed that the probability value are greater than the threshold value of 0.05 and hence are not significant.
#To further analyze the relationship of these ratios we may want to look at a larger sample size with more data 
#variance.
#**********************************************************************************************************************
