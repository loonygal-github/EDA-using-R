######### Preliminary Code #########

## Clear your workspace
rm(list=ls())

## Set working directory
setwd("C:/Users/abhin/Desktop/VV/Application/Drexel/Courses - Q2/Github/EDA")

## Install packages
install.packages("rio")
install.packages("RCurl")
install.packages("bitops")
install_formats()
install.packages("corrplot")
install.packages("PerformanceAnalytics")
## Load libraries
library(rio)
library(RCurl)
library(bitops)
library(corrplot)
library(dplyr)
library(PerformanceAnalytics)
## Load and View the 2015 & 2016 World Happiness reports 
WHR2015<-read.csv(file="2015.csv",stringsAsFactors = FALSE)
View(WHR2015)
WHR2016<-read.csv(file="2016.csv",stringsAsFactors = FALSE)
View(WHR2016)

## Frequency plot for Happiness scores

par(mfrow = c(1,2)) #Split the plot window
plot(WHR2015$Happiness.Score,main = "2015 Happiness Score by Frequency",xlab = "Frequency",ylab="Happiness.Score", col="Dark Green")
plot(WHR2016$Happiness.Score,main = "2016 Happiness Score by Frequency",xlab = "Frequency",ylab="Happiness.Score", col="Dark Blue")


## Histogram of Happiness scores
par(mfrow = c(2,1)) #Split the plot window
hist(WHR2015$Happiness.Score, main = "Histogram of Happiness Score Worldwide in 2015", xlab = "Frequency",ylab="Happiness.Score",col = "Yellow")
hist(WHR2016$Happiness.Score, main = "Histogram of Happiness Score Worldwide in 2016", xlab = "Frequency",ylab="Happiness.Score", col = "Yellow")

## Boxplot for Happiness scores
par(mfrow = c(2,1)) #Split the plot window
boxplot(WHR2015$Happiness.Score~WHR2015$Region, main = "Boxplot of Happiness Score by region in 2015", xlab = "Happiness.Score",ylab="Region",col = "Light Blue")
boxplot(WHR2016$Happiness.Score~WHR2016$Region, main = "Boxplot of Happiness Score by region in 2016", xlab = "Happiness.Score",ylab="Region", col = "Light Blue")

## Dig into Sub Saharan Africa data
SSA2015<-subset(WHR2015,Region=="Sub-Saharan Africa")
SSA2016<-subset(WHR2016,Region=="Sub-Saharan Africa")
summary(SSA2015)
summary(SSA2016)
##histogram and comparison of means across years
hist(SSA2015$Happiness.Score,main = "Histogram of Happiness Score for Sub-Saharan Africa in 2015", xlab = "Happiness.Score",ylab="Frequency",col = "Maroon",breaks = 50)
mean(WHR2015$Happiness.Score)
mean(SSA2015$Happiness.Score)
mean(WHR2016$Happiness.Score)
mean(SSA2016$Happiness.Score)

boxplot(SSA2015$Happiness.Score ~ SSA2015$Country, main = "Boxplot of Happiness Score in Sub-Saharan Africa in 2015", xlab = "Country", ylab = "Happiness Score",col="Maroon")


## Finding the correlation matrix for Sub Saharan Africa data
mydata<-SSA2016[,c(4,7:13)]
View(mydata)
## Renaming the variables 
mydata<-mydata %>% 
  rename(
    HPS =  Happiness.Score ,
    EGDP = Economy..GDP.per.Capita. ,
    FAM = Family ,
    HLE = Health..Life.Expectancy.,
    FR = Freedom ,
    TGOVC = Trust..Government.Corruption.,
    GEN = Generosity ,
    DR = Dystopia.Residual
  )

## Obtaining correlations rounded off to 2 places
cormydata<-round(cor(mydata),2)
View(cormydata)
## Obtaining the correlation plot
corrplot(cormydata,method = "square", type = "lower", order = "FPC", tl.col = "black", tl.srt = 45,col = c('red','dark green') )
## Positive correlations are in dark green and negative correlations are in red






