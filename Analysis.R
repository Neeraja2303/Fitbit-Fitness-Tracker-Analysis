
#Loading libraries
library(ggplot2)
library(dslabs)
library(dplyr)
library(caret)
library(tidyverse)
library(lubridate)

#Loading Data
a=read.csv("Fitness_tracker_data.csv")
a=subset(a,select=-c(TrackerDistance,LoggedActivitiesDistance))

#Including useful columns
a$ActivityDate=as.Date(a$ActivityDate,format="%m/%d/%Y")
a$day=weekdays(a$ActivityDate)
a$totalmin=a$VeryActiveMinutes+a$FairlyActiveMinutes+a$LightlyActiveMinutes
a$weekno=week(a$ActivityDate)
a$dayn=ifelse(a$day=="Saturday" | a$day=="Sunday","Week End","Week Day")

head(a)


#Plots

##Plot-1
a%>%group_by(day)%>%summarise(s=sum(totalmin))%>%ggplot(aes(day,s,fill=as.factor(day)))+geom_bar(stat = "identity")+scale_fill_hue(c = 70) +theme(legend.position="none")+labs(title="Total Activity Minutes Vs Day",y="Activity Minutes",x="Day")

##Plot-2
a%>%group_by(day)%>%summarise(s=sum(Calories))%>%ggplot(aes(day,s,fill=as.factor(day)))+geom_bar(stat = "identity")+scale_fill_hue(c = 70) +theme(legend.position="none")+labs(title="Total Calories Vs Day",y="Calories",x="Day")

##Plot-3
a%>%ggplot(aes(Calories))+geom_density(fill="light blue")+labs(title="Density function of calories",y="")

##Plot-4
a%>%group_by(dayn)%>%ggplot(aes(Calories,VeryActiveMinutes,color=dayn))+geom_point()+facet_wrap(~dayn)+labs(title="Calories Vs Very active minutes",color="")

##Plot-5
cor(a$VeryActiveDistance,a$TotalSteps)
cor(a$ModeratelyActiveDistance,a$TotalSteps)
cor(a$LightActiveDistance,a$TotalSteps)

##Plot-6
cor(a$SedentaryMinutes,a$Calories)


# Regression

dt = sort(sample(nrow(a), nrow(a)*.7))
train<-a[dt,]
test<-a[-dt,]

fit=lm(Calories~TotalSteps+totalmin+TotalDistance,data=train)
summary(fit)

pre=predict(fit,test)

#Regression Plot
ggplot(data=test,aes(x=1:nrow(test)))+geom_line(aes(y=Calories,color="blue"))+geom_line(aes(y=pre,color="red"))+labs(title="Actual Calories Vs Predicted Calories (Test Dataset)",x="",color="")+scale_color_manual(labels = c("Actual", "Predicted"), values = c("red", "black"))
