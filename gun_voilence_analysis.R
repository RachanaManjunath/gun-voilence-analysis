install.packages("dplyr") 
library(car)
library(corrplot) # plot correlations 
library(dplyr) # data aggregates 
library(Hmisc) # for correlation test of multiple variables 
library(gplots) 
install.packages("naniar") 
library(ggplot2)


library(naniar)

gun_deaths <- read.csv(file.choose(), header=T) 
View(gun_deaths)

str(gun_deaths)

gun_deaths$month <- as.factor(gun_deaths$month)
gun_deaths$year <- as.factor(gun_deaths$year)
gun_deaths$police <- as.factor(gun_deaths$police)

#solution a
gun_deaths%>%count(month)
per_month <- gun_deaths%>%count(month)

#solution b
barplot(per_month$n~per_month$month, mainlab= "Deaths per Month", 
        xlab= "Months", ylab= "Deaths", names.arg=c("Jan", "Feb", "March","April","May","June","July","Aug","Sept","Oct","Nov","Dec")
,width = c( 6,6,6,6,6,6,6,6,6,6,6,6))


#barplot(per_month$n~per_month$month, mainlab= "Deaths per Month", 
#        xlab= "Months", ylab= "Deaths", names.arg=per_month$month,width = c( 6,6,6,6,6,6,6,6,6,6,6,6))

#solution c
gun_deaths%>%count(intent)
per_intent <- gun_deaths%>%count(intent)
per_intent

options(max.print=1000000)

#na.omit(gun_deaths$intent)

#library("ggplot2")
#num <- c(1, 8, 4, 3, 6, 7, 5, 2, 11, 3)
#cat <- c(letters[1:10])
#data <- data.frame(num, cat)    
#ggplot(data,aes(x= reorder(cat,-num),num))+geom_bar(stat ="identity")

df <- per_intent[order(per_intent$n,decreasing = TRUE),]
barplot(df$n,names.arg = df$intent)

#solution d

boxplot(age ~ sex, data=gun_deaths, main="Age vs Sex", 
        xlab="Age", ylab="Sex",
        col=c("orange", "lightblue4"))

age_sex <- na.omit(gun_deaths[,c("age","sex")])

age_sex %>% group_by(sex) %>% summarise(avg = mean(age)) 


#e

White1 <- gun_deaths[(gun_deaths$race=='White' & gun_deaths$year=='2012' & gun_deaths$sex=='M'),]
View(White1)
subdf1 <- White1[!(White1$education=='Less than HS'),]
View(subdf1)


count(subdf1)  #15,485 white male who at least have a high school education got shot in 2012

#f

gun_deaths$month <- as.factor(gun_deaths$month)

months <- gun_deaths$month
winter <- gun_deaths[(gun_deaths$month==1 | gun_deaths$month==2 |gun_deaths$month==3 ),]
View(months)

count(winter) #23,656

Spring <- gun_deaths[(gun_deaths$month==4 | gun_deaths$month==5 |gun_deaths$month==6 ),]
View(winter)

count(Spring) #25,801

Summer <- gun_deaths[(gun_deaths$month==7 | gun_deaths$month==8 |gun_deaths$month==9 ),]

count(Summer) #26,281

Fall <- gun_deaths[(gun_deaths$month==10 | gun_deaths$month==11 |gun_deaths$month==12 ),]

count(Fall) #25,063

#Spring has the maximum number of deaths



#solution g


compare_deaths <- table(gun_deaths$race, gun_deaths$intent)

#solution h

summary(gun_deaths$police)

police_involvement <- table(gun_deaths$police,gun_deaths$intent, gun_deaths$place)

gun_deaths %>% 
  group_by(police) %>%
  summarise(no_rows = length(ID))

summary(ggplot2::gun_deaths$police)

library(dplyr)
library(magrittr)
gun_deaths %>% 
  group_by(police) %>%
  summarise(Count = n())




