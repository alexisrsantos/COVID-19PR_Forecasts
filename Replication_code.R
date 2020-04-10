library(readxl)
library(tidyverse)
library(forecast)
library(scales)
library(growthcurver)
options(scipen = 12) # Scientific Notation
options(digits = 6) # Specify Digits
noup<-1 #Days without update

#This is the pending etc
to_see <- read_excel("~/Pendientes.xlsx")

a<-ggplot(to_see, aes(x=Fecha, y=Casos_Confirmados)) + 
  geom_line(aes(y=Casos_Confirmados),color="black", linetype="dashed",size=1.5) + 
  theme_bw()+
  labs(x = "Date", 
       y = "Total Confirmed Cases", 
       title = "COVID-19 Confirmed Cases")+
  theme(legend.title=element_blank(),
        legend.position = "",
        plot.title=element_text(size=16),
        plot.subtitle =element_text(size=14),
        plot.caption=element_text(size=12))

b<-ggplot(to_see, aes(x=Fecha)) + 
  geom_line(aes(y=Growth_Rate_Positives),color="black", linetype="dashed",size=1.5) + 
  theme_bw()+
  labs(x = "Date", 
       y = "Percent Change", 
       title = "Percent Growth for Confirmed Cases")+
  theme(legend.title=element_blank(),
        legend.position = "",
        plot.title=element_text(size=16),
        plot.subtitle =element_text(size=14),
        plot.caption=element_text(size=12))

c<-ggplot(to_see, aes(x=Fecha, y=pct_pendiente)) + 
  geom_line(aes(y=pct_pendiente),color="red", linetype="dashed",size=1.5) + 
  theme_bw()+
  labs(x = "Date", 
       y = "% Pending Diagnostic", 
       title = "Percent of Tests Pending, excluding negatives")+
  theme(legend.title=element_blank(),
        legend.position = "",
        plot.title=element_text(size=16),
        plot.subtitle =element_text(size=14),
        plot.caption=element_text(size=12))+ylim(0,100)

d<-ggplot(to_see, aes(x=Fecha, y=Ratio_Positive)) + 
  geom_line(aes(y=Ratio_Positive),color="red", linetype="dashed",size=1.5) + 
  theme_bw()+
  labs(x = "Date", 
       y = "Ratio", 
       title = "Pending Cases/Confirmed Cases") +
  theme(legend.title=element_blank(),
        legend.position = "",
        plot.title=element_text(size=16),
        plot.subtitle =element_text(size=14),
        plot.caption=element_text(size=12))


df <- read_excel("~/Plates3.xlsx")
SummarizeGrowthByPlate(df)

summG <- function(x) {SummarizeGrowth(df$time,x,blank =TRUE)}
lapply(df[2:ncol(df)], summG)

models.all <- lapply(df[2:ncol(df)], function(x) SummarizeGrowth(df$time, x))

df.predicted.plate <- data.frame(time = df$time)
for (i in names(df[2:ncol(df)])) 
{df.predicted.plate[[i]] <- predict(models.all[[i]]$model)}

models.all <- lapply(df[2:ncol(df)], function(x) SummarizeGrowth(df[!is.na(x), 1], x[!is.na(x)]))
df.predicted.plate <- data.frame(time = df$time)
for (i in names(df[2:ncol(df)])) 
{df.predicted.plate[[i]] <- predict(models.all[[i]]$model, newdata = list(t = df$time))}

df<-as.data.frame(df)

library(reshape)
melt1 <- melt(df, id.vars = "time", variable.name = "sample", value.name = "od")
melt2 <- melt(df.predicted.plate, id.vars = "time", variable.name = "sample", value.name = "pred.od")
df.final <- cbind(melt1, pred.od=melt2[,3])
rm(melt1)
rm(melt2)

e<-ggplot(df.final, aes(x=time, y=value,colour=as.factor(variable))) + 
  geom_point(size=3,color="black") + 
  geom_line(aes(y=pred.od), linetype="dashed",size=1.5,alpha = 0.35) + 
  theme_bw()+
  labs(x = "Day since first case", 
       y = "Total Cases", 
       title = "Logistic curves for COVID-19 cases in Puerto Rico")+
  theme(legend.title=element_blank(),
        legend.position = "",
        plot.title=element_text(size=16),
        plot.subtitle =element_text(size=14),
        plot.caption=element_text(size=12))

## Data comes from the Maceira dashboard
coronavirus <- tribble(~perdaycases, 3,1,1,0,0,0,
                       1,
                       8,
                       7,
                       2,
                       8,
                       8,
                       12,
                       13,
                       15,
                       21,
                       27,
                       47,
                       65,
                       47,
                       30,
                       62,
                       74,
                       23,
                       38,60,47,63,42)

## Creating a cumulative sum of the per day # of cases.

coronavirus$cases <- cumsum(coronavirus$perdaycases)
coronavirus$id<-1:nrow(coronavirus)

## Create a daily Date object - helps my work on dates
inds <- seq(as.Date("2020-03-11"), as.Date(Sys.Date()-noup), by = "day")
coronavirus1<-subset(coronavirus,id<16)
## Creating a time series
cts <- ts(coronavirus1$cases,  start = c(2020, as.numeric(format(inds[1], "%j"))),
          frequency = 365)
set.seed(1)
## Forecast length
h0 = 30

## Creating a time series
estimates<-data.frame(Simulation=numeric(),Day=numeric(),Forecast=numeric()) #Empty Dataset

daysafter<-11
for(i in 1:daysafter) {
  cap<-subset(coronavirus,id<15+i)
  cts <- ts(cap$cases, start = 1,frequency = 365)
  cfore <- forecast(auto.arima(cts), h= h0, level = c(80))
  gc_fit <- SummarizeGrowth(seq(1,nrow(cap)),cap$cases)
  tt <- seq(from=nrow(cap)+1,to=90,by=1)
  forelog <- predict(gc_fit$model,newdata=list(t=tt))
  forecast<-forelog
  len<-as.numeric(length(forecast))
  df <- data.frame(
    Simulation = as.numeric(rep(i,len)),
    Day=1:len,
    Forecast = forecast
    
  )
  estimates <- bind_rows(estimates,df)
}

estimates$Forecast<-ifelse(estimates$Simulation==5,0,estimates$Forecast)
#estimates<-subset(estimates,Simulation!=5)

f<-ggplot(estimates,aes(x=Day,y=Forecast,group=Simulation,colour=as.factor(Simulation)))+ 
  geom_line(size=1.5,alpha=0.40) + 
  theme_bw()+
  labs(x = "Day since First Case", 
       y = "Total Cases", 
       title = "Logistic forecast")+
  theme(legend.title=element_blank(),
        legend.position = "",
        plot.title=element_text(size=16),
        plot.subtitle =element_text(size=14),
        plot.caption=element_text(size=12))

library(ggpubr)
ggarrange(a,b,c,d,e,f,nrow = 3,ncol =2,labels = c("A","B","C","D","E","F"))