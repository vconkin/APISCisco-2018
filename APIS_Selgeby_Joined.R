## This file uses the data extracted from 

rm(list=ls(all=TRUE)) #wipes your R workspace clean.

library(plyr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(readxl)
library(grid)


## load in your files
sdensity =read.csv("Selgeby_ExtractedData/GMDensity.csv", header=TRUE)
Stomachs =read.csv("Selgeby_ExtractedData/Selgeby_FullStomachs.csv", header=TRUE)
NC =read.csv("Selgeby_ExtractedData/NC.csv", header=TRUE)

excel_sheets("Lake_Superior_Data/APIS_Coregonus_2018.xlsx")
FTrawl <- read_excel("Lake_Superior_Data/APIS_Coregonus_2018.xlsx", sheet ="FTrawl") 
FCatch <- read_excel("Lake_Superior_Data/APIS_Coregonus_2018.xlsx", sheet ="FCatch") 

##==============Combine Trawl and Catch data================
FNumb <- merge(FTrawl, FCatch, by="Trawl")
## Create a Density variable and reset date & Week to be factors
## We also need to convert from liters to 1000 cubic meters
FNumb$Density_1000m<-((FNumb$Catch)/(FNumb$Volume_L)*10^6)
FNumb$Date<-as.factor(FNumb$Date)
FNumb$Week<-as.factor(FNumb$Week)

##===========Geometric mean plot===============
## To make a comparable plot, we actually need to calculate geometric mean
serror <- function(x) {sd(x)/sqrt(length(x))}
gm_mean = function(a){prod(a)^(1/length(a))}

## There are a lot of zeros in this data set, so we have a workaround:
## we add the smallest overall density in number / 1000 cubic meters
## to all values, calculate the mean, and subtract at the end 
x<- min(FNumb[FNumb$Density_1000m>0,30])

Density_g<-group_by(FNumb,Week)%>%
  summarize(Avg_Density = mean(Density_1000m), 
            gm = gm_mean((Density_1000m + x))-x,
            se = serror(Density_1000m),
            n = sum(Catch))

n_18<-Density_g$n
labs_18<-c("May 14-15","May 21-23", "May 29", "June 4-5", "June 12-13", "June 18-20",
        "June 26-28", "July 2-5", "July 9-11", "July 16-17", "July 23-25")

gm_18<-ggplot(Density_g, aes(Week, gm)) + 
  geom_errorbar(aes(ymin = gm-se, ymax=gm+se), width=.3) +
  geom_point(size=2) +
  scale_x_discrete(labels = labs_18, name = "Sample Date") +
  scale_y_continuous(name = "Geometric Mean Density per 1000 m^3\n",
                     expand = c(0,0)) +
  coord_cartesian(ylim=c(0, 90)) +
  labs(title = "2018\n", tag = "(B)") +
  theme_classic() +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        plot.tag=element_text(size=12),
        plot.tag.position = "topleft") +
  geom_text(aes(label=paste0("n=",n_18 )), y = 87, size = 3)

gm_18

## Now we do the same with the data from Selgeby
labs_74<-c("May 6-8", "May 13-15", "May 28-30", "June 11-13", "July 1-7")
n_74<-sdensity$n

gm_74<-ggplot(sdensity, aes(Week, gm_mean)) + 
  geom_errorbar(aes(ymin = gm_mean-se, ymax=gm_mean+se), width=.1) +
  geom_point() +
  scale_x_continuous(labels = labs_74, name = "Sample Date",
                     breaks = c(19,20,22,24,27)) +
  scale_y_continuous(name = "Geometric Mean Density per 1000 m^3\n",
                     expand = c(0,0)) +
  geom_hline(linetype = "solid", yintercept = 0) +
  coord_cartesian(ylim=c(0, 13)) +
  labs(title = "1974\n", tag = "(A)") +
  theme_classic()+
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        plot.tag=element_text(size=12),
        plot.tag.position = "topleft") +
  geom_text(aes(label=paste0("n=",n_74)), y = 12, size = 3)

gm_74
ggarrange(gm_74, gm_18, ncol=2, nrow=1)

##======Selgeby Density Plot============================================

## Calculate the density and overall catch by site for each week
Density_m<-group_by(FNumb,Station, Week)%>%
  summarize(Avg_Density = mean(Density_1000m), n=sum(Catch))
## Create custom labels for the time intervals
labs_18<-c("May 14-15","May 21-23", "May 29", "June 4-5", "June 12-13", "June 18-20",
        "June 26-28", "July 2-5", "July 9-11", "July 16-17", "July 23-25")

## to add n values, create a vector of counts for the week
counts<-group_by(Density_m, Week) %>% summarize(N = sum(n))
Ns <- as.vector(counts[["N"]])

## Number/L is converted to Number/1000m^3 by scaling up by 1e6
g <- ggplot(Density_m, aes(Week, Avg_Density))
g+ geom_boxplot(color = "red", fill= "orange", alpha=0.2, lwd= 0.1) +
  geom_jitter(size =1.75, width = 0.2)+
  scale_y_continuous(name ="Coregonus per 1000 m^3", limits= c(-5,220)) +
  scale_x_discrete(labels = labs_18)+
  annotate("text", x=1.15:11.15, y=-4:-4,label=(Ns), size=2.5)+
  annotate("text", x=0.85:10.85, y=-4:-4, label= "n =", size=2.5)+
  theme_classic() +
  theme(axis.text=element_text(size=8), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold")) 

##======Stomach content by Length=======================================
labs<-Stomachs$n

feeding_74<-ggplot(Stomachs,aes(Bin, PercentCalculated*100))+
  geom_line(linetype="longdash", color="#7b3294", size=1.4)+
  geom_point(size=2)+
  scale_y_continuous(name ="Percent With Food\n",limits = c(0,105),
                     expand = c(0,0)) +
  scale_x_continuous(name = "\nLength Bin (mm)", 
                     breaks = seq(6,18, by =1),limits = c(6,18))+
  labs(title = "1974\n", tag = "(A)") +
  theme_classic()+
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        plot.tag=element_text(size=12),
        plot.tag.position = "topleft") +
  annotate("text", x=9.20:17.20, y=2:2,label=labs, size=2.5) +
  annotate("text", x=8.80:16.80, y=2:2, label= "n = ", size=2.5)

feeding_74

##======Nauplii & Copepodites==========================================
## create n labels for the plot
labs<-Stomachs$n

feeding_74<-ggplot(Stomachs,aes(Bin, PercentCalculated*100))+
  geom_line(linetype="longdash", color="#7b3294", size=1.4)+
  geom_point(size=2)+
  scale_y_continuous(name ="Percent With Food\n",limits = c(0,105),
                     expand = c(0,0)) +
  scale_x_continuous(name = "\nLength Bin (mm)", 
                     breaks = seq(6,18, by =1),limits = c(6,18))+
  labs(title = "1974\n", tag = "(A)") +
  theme_classic()+
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        plot.tag=element_text(size=12),
        plot.tag.position = "topleft") +
  annotate("text", x=9.20:17.20, y=2:2,label=labs, size=2.5) +
  annotate("text", x=8.80:16.80, y=2:2, label= "n = ", size=2.5)

feeding_74