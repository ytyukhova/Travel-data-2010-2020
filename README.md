# Travel data 2010-2020
## Analysis of flights

This repository includes the analysis of a personal data set of all flights in the period of 2010-2020. 
In the first version (1.1) of the file I created tables and visualized data to find out answers to questions such as the cheapest day/month/year to fly in my data set, the most popular airline, the number of flights taken on a certain day of the week, etc.

![Test]([https://xenforo.com/community/media/plane-jpg.2194/](https://unsplash.com/photos/FwdZYz0yc9g))

## Description of files
**Travel data 2010-2020 YT.pdf** includes code, plots, and the summary of the results.  
**Plots** folder includes all plots from this analysis.  
This file contains code performed on the data.  
**Summary** is located at the bottom of this file.

## Introduction
Below I explore my cleaned-up 2010-2020 flight data set that contains personal and business flights. The data set contains 5 variables - **Date, Departure, Destination, PriceDOLLARS, and Airline** - displayed as a heading with 329 observations. Code below is focused on basic visualization of different patterns such as, for example, the cheapest day to fly averaged across 10 years and other variables based on 1 person's data.

## Data Preparation

Load all packages
```{r, message=FALSE,warning=FALSE}
## For making data manipulation easier
library(dplyr)
## For creating simple tables
library(knitr)
## For creating graphics
library(ggplot2)
## For helping build common complex tables and manipulate table styles
library(kableExtra)
## For arranging multiple grid-based plots on a page
library(gridExtra) 
```
Load the data set
```{r}
Sys.setlocale("LC_ALL", "C")

## Load data
df<-read.csv("FlightsData_2.csv",header=TRUE,as.is=TRUE,
             na.strings=",")
data<-df
str(data)
## Read in date correctly
Date<-as.Date(df$Date,"%m/%d/%Y")
## Assigning new data column to the data instead of the old format date
data$Date<-Date
## Make sure I have 329 rows of data without NAs in Date
    ## sum(!is.na(data$Date))
## Add weekdays column from the Date
data<-mutate(data,weekday=weekdays(data$Date))
    ## head(data)
## Split Date into 3 columns - year, month, day
data2<-data.frame(date=data$Date,
                  year=as.numeric(format(data$Date,format="%Y")),
                  month=as.numeric(format(data$Date,format="%m")),
                  day=as.numeric(format(data$Date,format="%d")))
## Combine 2 data sets and get rid of the extra date column
data3<-cbind(data,data2)
data3<-mutate(data3,date=NULL)
tail(data3)
```

```{r}
# Sort the number of flights per weekday from the smallest to the largest 
sort(table(data3$weekday))
# Make sure I didn't lose data - 329 observations
sum(table(data3$weekday))
```

## The number of flights per year

```{r,, message=FALSE}
yeargr<-group_by(data3,year)
by_year<-summarize(yeargr,count=n())
dy<-as.data.frame(by_year)
names(dy)<-c("year","flights")
dy<-na.omit(dy)
dy
g1<-ggplot(data=dy, aes(x=factor(year),y=flights))+
    geom_bar(stat="identity",color="magenta",fill="magenta")+
    ggtitle(label="Flights per year 2010-2020")+
    labs(x="Year",y="Number of Flights")+
    theme(plot.title = element_text(size = 12,hjust=0.5,face="bold"))+
    geom_text(aes(label=flights), vjust=-0.3, size=3)
g1
```

The number of flights exceeded or were equal to 40 in **2011, 2012, 2013 and 2019**.

## The number of flights per weekday

```{r,, message=FALSE}
daygr<-group_by(data3,weekday)
by_day<-summarize(daygr,count=n())
dday<-as.data.frame(by_day)
names(dday)<-c("weekday","flights")
dday<-na.omit(dday)
dday
dday$weekday<-factor(dday$weekday,levels=c("Monday","Tuesday", "Wednesday", "Thursday",
                                           "Friday", "Saturday", "Sunday"))

g2<-ggplot(data=dday, aes(x=weekday,y=flights))+
    geom_bar(stat="identity",color="green",fill="green")+
    ggtitle(label="Flights per weekday 2010-2020")+
    labs(x="Weekday",y="Number of Flights")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1),
          plot.title = element_text(size = 12,hjust=0.5,face="bold"))+
    geom_text(aes(label=flights), vjust=-0.3, size=3)
g2
```
The majority of the flights were taken on **Sunday**. Otherwise, it is quite even across the entire week.

## The number of flights per month

```{r,, message=FALSE}
mgr<-group_by(data3,month)
by_month<-summarize(mgr,count=n())
dm<-as.data.frame(by_month)
names(dm)<-c("month","flights")
dm<-na.omit(dm)
dm

## Factor in x in aes allows to display x axis at discrete levels
g3<-ggplot(data=dm, aes(x=factor(month),y=flights))+
    geom_bar(stat="identity",color="orange",fill="orange")+
    ggtitle(label="Flights per month 2010-2020")+
    labs(x="Month",y="Number of Flights")+
    theme(plot.title = element_text(size = 12,hjust=0.5,face="bold"))+
    geom_text(aes(label=flights), vjust=-0.3, size=3)+
    annotate("text", x = 1.3, y = 42, label = "Months: \n 1=January \n 12=December",
                 col = "black", size = 3.5)
g3
```

In 2010-2020 the most popular month to travel or the highest number of connections was in **November** with 45 flights, then **May** with 36, and **August** with 33. 

## Flights per airlines

First, I had to display all unique names for airlines I used in my data set. Any empty cells were replaced with NAs. Then I combined similar names - for example, SW and Southwest mean the same, therefore, they were combined.  

```{r}
unique(data3$Airline)
data3$Airline[data3$Airline==""]<-NA
data3$Airline[data3$Airline=="Southwest"]<-"SW"
data3$Airline[data3$Airline=="Swiss International Air Lines"]<-"Swiss"
data3$Airline[data3$Airline=="LUFTHANSA"]<-"Lufthansa"
data3$Airline[data3$Airline=="Austrian Airlines"]<-"Austrian"
data3$Airline[data3$Airline=="AirFrance"]<-"Air France"
data3$Airline[data3$Airline=="Tyrolean Airlines for Lufthansa"]<-"Tyrolean"
data3$Airline[data3$Airline=="LOT Polish Airlines"]<-"LOT Polish"
unique(data3$Airline)
```

Now I have 21 unique airline names. 

```{r,, message=FALSE}
agr<-group_by(data3,Airline)
by_airline<-summarize(agr,count=n())
da<-as.data.frame(by_airline)
names(da)<-c("airline","flights")
da<-na.omit(da)
da

g4<-ggplot(data=da, aes(x=airline,y=flights))+
    geom_bar(stat="identity",color="skyblue1",fill="skyblue1")+
    ggtitle(label="Flights per Airline 2010-2020")+
    labs(x="Airline",y="Number of Flights")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
        plot.title = element_text(size = 12,hjust=0.5,face="bold"))+
    geom_text(aes(label=flights), vjust=-0.3, size=3)
g4
```

The overwhelming number of flights were taken on **Delta** 113, then **United** 81, **Southwest** 55, and **American** 23. 


## Most popular airline per year

```{r}
aygr<-group_by(data3,year,Airline)
by_yearairline<-summarize(aygr,count=n())
dya<-as.data.frame(by_yearairline)
names(dya)<-c("year","airline","flights")
dya<-na.omit(dya)
## dya
## The most popular airline by year
max_ay<-dya %>% group_by(year) %>% slice(which.max(flights))
maxay<-as.data.frame(max_ay)
maxay

class(maxay$year)
## To display every year on x axis, year from numeric to factor
maxay$year <- as.factor(maxay$year)

g5<-ggplot(data=maxay, aes(x=year,y=flights))+
    geom_bar(stat="identity",color="coral",fill="coral")+
    ggtitle(label="Most Popular Airline by Year")+
    labs(x="Year",y="Number of Flights")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
          plot.title = element_text(size = 12,hjust=0.5,face="bold"))+
    geom_text(aes(label=airline), vjust=-0.3, size=3)+
    annotate("text", x = 9.8, y = 25, label = "UA=United \n DL=Delta \n SW=Southwest",
                 col = "black", size = 3.5)
g5
```

**Delta** was the most popular airline for 6 years, then **Southwest** for 3 years, and **United** for 2.

## Most popular departure/layover and destination/layover airport

```{r}
## The number of departure/layover locations in alphabetic order
dep<-sort(table(data3$Departure),decreasing=T)
sum(table(data3$Departure))
class(dep)
dep<-as.data.frame(dep)
## Rename columns
names(dep)<-c("location","frequency")
## dep
dep1<-filter(dep,frequency>10)
dep1

## Most popular destination/layover airport
arr<-sort(table(data3$Destination),decreasing=T)
sum(table(data3$Destination))
arr<-as.data.frame(arr)
## Rename columns
names(arr)<-c("location","frequency")
## arr
arr1<-filter(arr,frequency>10)
arr1
```
The most popular departure/layover was **Atlanta** and **Omaha** with 52 flights each followed by **Chicago** with 30. The most popular arrival/layover was **Omaha** with 53 flights and **Atlanta** with 51 followed by Chicago with 30.

## Cost per weekday across 2010-2020
Cost analysis does not account for the number of flights/connections included for the given price. It is assumed that the price is valid for the total trip including the return ticket and disregarding the number of connections.
Some personal flight prices might include tickets for two people.
```{r}
## unique(data3$PriceDOLLARS)

## Info about the first leg of the trip that costs more than $1500
## filter(data3,PriceDOLLARS>1500)

## To order the days of the week correctly
data3$weekday<-factor(data3$weekday,levels=c("Monday","Tuesday", "Wednesday", 
                                             "Thursday","Friday", "Saturday",
                                             "Sunday"))

## Next two lines for labeling the subset of data in geom_text
top3<-subset(data3, PriceDOLLARS > 1500)
top3$PriceDOLLARS

g6<-ggplot(data=data3, aes(x=weekday,y=PriceDOLLARS))+
    geom_point(color="skyblue1",size=4, alpha=0.5)+
    ggtitle(label="Cost per weekday 2010-2020")+
    labs(x="Weekday",y="Cost,$")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
          plot.title = element_text(size = 12,hjust=0.5,face="bold"))+
        geom_text(data=subset(data3, PriceDOLLARS > 1500),
              aes(x=weekday,y=PriceDOLLARS),label=round(top3$PriceDOLLARS), hjust=1.3, size=3.5)
   g6
```

```{r}

## Prices over $1500 in the data set
##data3[which(data3$PriceDOLLARS>1500),]
## Row numbers
rownum<-as.numeric(rownames(data3[which(data3$PriceDOLLARS>1500),]))
## All data needed
trip1<-data[c(rownum[1],rownum[1]+1,rownum[1]+2,rownum[1]+3),]
trip2<-data[c(rownum[2],rownum[2]+1,rownum[2]+2),]
trip3<-data[c(rownum[3],rownum[3]+1,rownum[3]+2,rownum[3]+3,rownum[3]+4,rownum[3]+5),]
trip1
trip2
trip3
```

All three most expensive trips included travel abroad. The most expensive trip ($2959) was a trip to another country with 2 connections and no return ticket. The second most expensive trip included travel abroad with 1 connection for 2 people (during this trip there was an additional trip from there).The third most expensive trip was a trip to another country with only 1 stop and the return ticket.

```{r}
## Cost by weekday averaged across 10 years
cost_by_weekday_gr<-group_by(data3,weekday,PriceDOLLARS)
by_weekdaycost<-summarize(cost_by_weekday_gr,count=n())
weekdaycost<-as.data.frame(by_weekdaycost)
## The next line is very important for the final table not to have all values to be NAs
weekdaycost<-na.omit(weekdaycost)
average_weekdaycost<-weekdaycost%>%group_by(weekday)%>%summarize(round(mean(PriceDOLLARS),1))
average_weekdaycost<-as.data.frame(average_weekdaycost)
## Rename columns
names(average_weekdaycost)<-c("weekday","cost")
## Organize in the order of cost
average_weekdaycost<-arrange(average_weekdaycost,desc(cost))
##average_weekdaycost
kable(average_weekdaycost, caption="Cost of flight per weekday in the period of 2010-2020")%>%
  kable_styling(latex_options = "HOLD_position")
```
The most expensive day to fly in my data set was **Monday** with the price of **$733** averaged across 10 years, airlines, and months.

## Cost per year

```{r}
g7<-ggplot(data=data3, aes(x=factor(year),y=PriceDOLLARS))+
    geom_point(color="tomato2",size=4, alpha=0.5)+
    ggtitle(label="Cost per year")+
    labs(x="Year",y="Cost,$")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
          plot.title = element_text(size = 12,hjust=0.5,face="bold"))+
    geom_text(data=subset(data3, PriceDOLLARS > 1500),
              aes(x=factor(year),y=PriceDOLLARS),label=round(top3$PriceDOLLARS),hjust=1.3, size=3.5)
    
g7
```

```{r}
## Cost by year averaged across weekdays 
cost_by_year_gr<-group_by(data3,year,PriceDOLLARS)
by_yearcost<-summarize(cost_by_year_gr,count=n())
yearcost<-as.data.frame(by_yearcost)
yearcost<-na.omit(yearcost)
average_yearcost<-yearcost%>%group_by(year)%>%summarize(round(mean(PriceDOLLARS),1))
average_yearcost<-as.data.frame(average_yearcost)
## Rename columns
names(average_yearcost)<-c("year","cost")
## Organize in the order of cost
average_yearcost<-arrange(average_yearcost,desc(cost))

kable(average_yearcost, caption="Cost of flight per year in the period of 2010-2020")%>%
  kable_styling(latex_options = "HOLD_position")
```
The most expensive year to fly in the data set was **2019** followed by 2018 and 2012. The cheapest year was **2020**. This is not surprising, since the number of flights I had taken before the pandemic started was only a handful. 


## Cost per month
```{r}
g8<-ggplot(data=data3, aes(x=factor(month),y=PriceDOLLARS))+
    geom_point(color="seagreen4",size=4, alpha=0.5)+
    ggtitle(label="Cost per month 2010-2020")+
    labs(x="Month",y="Cost,$")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),
          plot.title = element_text(size = 12,hjust=0.5,face="bold"))+
    geom_text(data=subset(data3, PriceDOLLARS > 1500),
              aes(x=factor(month),y=PriceDOLLARS),label=round(top3$PriceDOLLARS),
              hjust=1.3, size=3.5)

g8
```

```{r}
## Cost by month averaged across 10 years
cost_by_month_gr<-group_by(data3,month,PriceDOLLARS)
by_monthcost<-summarize(cost_by_month_gr,count=n())
monthcost<-as.data.frame(by_monthcost)
monthcost<-na.omit(monthcost)
average_monthcost<-monthcost%>%group_by(month)%>%summarize(round(mean(PriceDOLLARS),1))
average_monthcost<-as.data.frame(average_monthcost)

## Rename columns
names(average_monthcost)<-c("month","cost")
## Organize in the order of cost
average_monthcost<-arrange(average_monthcost,desc(cost))

kable(average_monthcost, caption="Cost of trip per month in the period of 2010-2020")%>%
  kable_styling(latex_options = "HOLD_position")
```
The most expensive month to fly in the period of 2010-2020 was **June**, and it was almost twice as expensive as the next one on the list - **December**. Perhaps, it was driven by two outliers - two out of three most expensive trips during the period of 2010-2020 took place in June. The cheapest month to fly was **September**.


## Summary

* The number of flights exceeded or were equal to 40 in **2011, 2012, 2013 and 2019**.
* The majority of the flights were taken on **Sunday**. Otherwise, it is quite even across the entire week.
* In 2010-2020 the most popular month to travel or the highest number of connections was in **November** with 45 flights, then **May** with 36, and **August** with 33. 
* The overwhelming number of flights were taken on **Delta** 113, then **United** 81, **Southwest** 55, and **American** 23. 
* **Delta** was the most popular airline for 6 years, then **Southwest** for 3 years, and **United** for 2.
* The most popular departure/layover was **Atlanta** and **Omaha** with 52 flights each followed by **Chicago** with 30. The most popular arrival/layover was **Omaha** with 53 flights and **Atlanta** with 51 followed by Chicago with 30.
* The most expensive day to fly in my data set was **Monday** with the price of **$733** averaged across 10 years, all airlines, and months.
* The most expensive year to fly in my data set was **2019** followed by 2018 and 2012. The cheapest year was **2020** with only handful number of flights because of the pandemic.
* The most expensive month to fly in the period 2010-2020 was **June**, and it was almost twice as expensive as the next one on the list - **December**. The cheapest month to fly was **September**.

## Contributor
Yulia Tyukhova


