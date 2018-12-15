data<-read.csv(file.choose())

library(ggplot2)
library(scales)

library(scales)
library(xkcd)


ggplot(data,aes(Genre, Views))+geom_bar(stat="identity",aes(fill = Genre))+theme(axis.text.x=element_text(angle=-90))

ggplot(data, aes(Genre, Views))+geom_bar(stat="identity",aes(fill = Genre))+theme(axis.text.x=element_text(angle=-90))+scale_y_continuous(name="Views", labels = scales::comma,breaks = round(seq(min(data$Views), max(data$Views), by = 1000000000),1))+ ggtitle("Bar plot of average views per Genre ")

data=data[data$Subscribers<20000000,]
plot(data$Views ~ data$Subscribers, xlab="Subscribers", ylab = "Views", main = "Relationship between number of subscribers and views generated")+scale_y_continuous(name="Views", labels = scales::comma,breaks = round(seq(min(data$Views), max(data$Views), by = 1000000000),1))
z<-lm(data$Views~data$Subscribers,data = data)
abline(z, col="blue")



                                                                                                                                                                                                                                                                                                           

theme_xkcd <- theme(
  panel.background = element_rect(fill="white"), 
  axis.ticks = element_line(colour="black"),
  panel.grid = element_line(colour=NA),
  axis.line.x = element_line(color="black", size = 2),
  axis.line.y = element_line(color="black", size = 2),
  axis.text.y = element_text(colour="blue"), 
  axis.text.x = element_text(colour="blue"),
  text = element_text(size=16, family="Comic Sans MS",color="red")
)


p<-ggplot(data=data, aes(x=Subscribers, y=Views))+
  geom_line(aes(y=views), position="jitter")+
  geom_line(colour="white", size=3, position="jitter")+
  geom_line(colour="red", size=1, position="jitter")+
  geom_text(family="Humor Sans", x=6, y=-1.2, label="A SIN AND COS CURVE")+
  geom_line(aes(y=xaxis), position = position_jitter(h = 0.005), colour="black")+
  scale_x_continuous(breaks=c(2, 5, 6, 9), 
                     labels = c("YARD", "STEPS", "DOOR", "INSIDE"))+labs(x="", y="")+theme_xkcd
ggsave("xkcd_ggplot.jpg", plot=p, width=8, height=5)



data<-data[order(-data$Monthly.Salary),]
group1<-data$Uploads[41:50]

y<-data.frame(group1)
y$name<-"top 41-50"
x<-rbind(x,y)
y

x$top_channels_by_salary<-x$name
ggplot(x)+geom_boxplot(aes(y=group1,x=name,fill=top_channels_by_salary))+theme(axis.text.x=element_text(angle=0))+scale_y_continuous(name="uploads", labels = scales::comma,breaks = round(seq(min(x$group1), max(x$group1),by=10000)))+  labs(x = "top 50 channels",title="Variance in uploads for top YouTube channels ")+theme_xkcd
