#Loading Libraries
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

head(a)


#Plots

a%>%ggplot(aes(Calories,totalmin))+geom_point()+facet_wrap(~day)

a%>%group_by(day)%>%summarise(s=sum(totalmin))%>%ggplot(aes(day,s,fill=as.factor(day)))+geom_bar(stat = "identity")+scale_fill_hue(c = 40) +theme(legend.position="none")

a%>%group_by(day)%>%summarise(s=sum(Calories))%>%ggplot(aes(day,s,fill=as.factor(day)))+geom_bar(stat = "identity")+scale_fill_hue(c = 40) +theme(legend.position="none")

cor(a$VeryActiveMinutes,a$SedentaryMinutes)

cor(a$LightlyActiveMinutes,a$Calories)



a%>%group_by(weekno,Id)%>%filter(Id==1503960366)%>%summarise(ts=sum(TotalSteps),tc=sum(Calories))%>%ggplot(aes(weekno,ts,fill=weekno))+geom_line()

a%>%group_by(weekno,Id)%>%filter(Id==1503960366)%>%summarise(ts=sum(TotalSteps),tc=sum(Calories))%>%ggplot(aes(weekno,tc))+geom_bar(stat="identity")

cor(a$TotalSteps,a$TotalDistance)

a%>%group_by(day)%>%summarise(st=sum(TotalDistance),sv=sum(VeryActiveDistance),sm=sum(ModeratelyActiveDistance),sl=sum(LightActiveDistance))%>%
  ggplot()
