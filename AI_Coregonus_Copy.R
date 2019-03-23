## Create sumamry figures using the data from field sampling
## Files needed: Raw data from the field sampling, saved in the APIS excel file
## Clear the environment first
## 
setwd("C:/Users/vsluc/Documents/UVM/Data Analysis")
rm(list = ls(all.names=TRUE))

## Load Packages============================================== 

library(dplyr)
library(readxl) 
library(ggplot2)
library(ggridges)
library(viridis)

##Exclude blank cells in calculations
na.rm=TRUE

##=====Density Data===============================
##load data
excel_sheets("Lake_Superior_Data/APIS_Coregonus_2018.xlsx")
FTrawl <- read_excel("Lake_Superior_Data/APIS_Coregonus_2018.xlsx", sheet ="FTrawl") 
FCatch <- read_excel("Lake_Superior_Data/APIS_Coregonus_2018.xlsx", sheet ="FCatch") 
FLength <- read_excel("Lake_Superior_Data/APIS_Coregonus_2018.xlsx", sheet ="FLength")
OverB <- read_excel("Lake_Superior_Data/APIS_Coregonus_2018.xlsx", sheet ="OverB")

##==============Combine Trawl and Catch data================

FNumb <- merge(FTrawl, FCatch, by="Trawl")

##===========Establish palette choices for plots=============
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
pal1<-viridis(n=11, direction=-1, option="D")
pal2<-viridis(n=106)

##========Density by Station, Group=============
g <- ggplot(FNumb, aes(Jday, Catch_ha))
g + geom_point(size=3)  +
  geom_smooth(size=2) +
  scale_y_continuous(name ="Coregonus per hectare") +
  scale_x_continuous(name = "Day of the year", breaks=seq(134,206, by=7)) +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"),
        strip.text = element_text(size=10)) +
  facet_grid(Station ~., scales="free_y")

g <- ggplot(FNumb, aes(Jday, Catch_ha))
g + geom_point(size=3)  +
  geom_smooth() +
  scale_y_continuous(name ="Coregonus per hectare") +
  scale_x_continuous(name = "Day of the year", breaks=seq(134,204, by=7))+
  ggtitle("Larval Coregonus Density Across Island Groups") +
  theme(title=element_text(size=28, face="bold"),
        axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"),
        strip.text = element_text(size=10)) + 
  facet_grid(Group ~.) 

## Reorder the sites, otherwise they will be in numeric order
FNumb$Station =factor(FNumb$Station, 
                      levels=c("71","66","44","45","87",
                               "139","525","33","526","527"))

## Same as above but by week instead of individual day
g <- ggplot(FNumb, aes(Week, Catch_ha))
g + geom_point(size=3)  +
  geom_smooth(size=2) +
  scale_y_continuous(name ="Coregonus per hectare") +
  scale_x_continuous(name = "Week", breaks=seq(20,30, by=2)) +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"),
        strip.text = element_text(size=10)) +
  facet_grid(Station ~., scales="free_y")

g <- ggplot(FNumb, aes(Week, Catch_ha))
g + geom_point(size=3)  +
  geom_smooth() +
  scale_y_continuous(name ="Coregonus per hectare") +
  scale_x_discrete(name = "Week", breaks=seq(20,30, by=2))+
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"),
        strip.text = element_text(size=10)) + 
  facet_grid(Group ~.) 


NC =read.csv("Selgeby_ExtractedData/NC.csv", header=TRUE)
Stomachs =read.csv("Selgeby_ExtractedData/Selgeby_FullStomachs.csv", header=TRUE)

##======Selgeby Density Plot====================
## Create a Density variable and reset date & Week to be factors
FNumb$Density_L<-(FNumb$Catch)/(FNumb$Volume_L)
FNumb$Date<-as.factor(FNumb$Date)
FNumb$Week<-as.factor(FNumb$Week)

## Calculate the density and overall catch by site for each week
Density_m<-group_by(FNumb,Station, Week)%>%
  summarize(Avg_Density = mean(Density_L), n=sum(Catch))
## Create custom labels for the time intervals
labs<-c("May 14-15","May 21-23", "May 29", "June 4-5", "June 12-13", "June 18-20",
        "June 26-28", "July 2-5", "July 9-11", "July 16-17", "July 23-25")

## to add n values, create a vector of counts for the week
lab<-group_by(Density_m, Week) %>% summarize(N = sum(n))
Ns <- as.vector(lab[["N"]])

## Number/L is converted to Number/1000m^3 by scaling up by 1e6
g <- ggplot(Density_m, aes(Week, Avg_Density*1000000))
g+ geom_boxplot(color = "red", fill= "orange", alpha=0.2, lwd= 0.1) +
  geom_jitter(size =1.75, width = 0.2)+
  scale_y_continuous(name ="Coregonus per 1000 m^3", limits= c(-5,220)) +
  scale_x_discrete(labels = labs)+
  annotate("text", x=1.15:11.15, y=-4:-4,label=(Ns), size=2.5)+
  annotate("text", x=0.85:10.85, y=-4:-4, label= "n =", size=2.5)+
  theme(axis.text=element_text(size=8), 
        #axis.text.x = element_text(angle=30),
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold")) 
##===========Under construction===============
## To make a comparable plot, we actually need to calculate geometric mean

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

##===========Under construction===============

##=====Length Data=============================##
##Combine Fish and Length data with date station fields

FLength2 <- merge(FTrawl, FLength, by="Trawl")

##=================Length by week=======================
## Length distribution by day
ggplot(FLength2,aes(x=Jday,y=Length_mm)) +
  geom_point(position="jitter") +
  geom_smooth(stat="smooth", method="loess") +
  scale_y_continuous(name ="Total length (mm)") +
  scale_x_continuous(name = "Date",
                     breaks = seq(134,204,14),
                     labels = c("14 May","28 May","11 June",
                                "25 June","9 July", "22 July"))+
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold")) 

## Weekly lengths distribution boxplot
ggplot(FLength2,aes(x=factor(Week),y=Length_mm))+
  geom_boxplot(color="black", fill=pal1, varwidth = TRUE)+
  scale_y_continuous(name ="Total length (mm)") +
  scale_x_discrete(name = "Week", breaks=seq(20,30, by=2)) +
  labs(title="Length Distribution of Larval Coregonus Over Time") +
  theme_classic() +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"))+
  stat_smooth(method="lm")

## Length scatter plot summarized by week
ggplot(FLength2,aes(x=Week,y=Length_mm)) +
  geom_point() +
  stat_smooth(method="loess") +
  scale_y_continuous(name ="Total length (mm)") +
  scale_x_discrete(name = "Week", breaks=seq(20,30, by =2)) +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"))
##==============Joy Plot================================
## Length distribution ridge plots
FLength2$Week <-factor(FLength2$Week)

## geom_joy is not used anymore but the plot still works
ggplot(FLength2,aes(x=Length_mm,y=Week)) +
  geom_joy() +        # removes tails
  scale_y_discrete(expand = c(0.01, 0)) +  # removes cutoff top
  labs(x="Total Length (mm)",y="Week") +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold")) 

## Same plot but with updated ridges and color gradient 
ggplot(FLength2, aes(Length_mm, Week, fill=..x..)) +
  geom_density_ridges_gradient() +
  scale_y_discrete(expand = c(0,0),
                   breaks=seq(20,30, by=1), 
                   labels = c("14 May","21 May","28 May",
                              "4 June","11 June","18 June",
                              "25 June","2 July", "9 July", 
                              "16 July","23 July")) +
  scale_color_viridis_c(alpha=1, name="Length (mm)", direction = -1,
                        aesthetics = "fill", option="C") + 
  labs(x="Total Length (mm)",y="Week of\n", legend = "Length (mm)",
       title="Weekly Length Distributions of Larval Coregonus") +
  theme_classic() +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"))

##==By Station============================================
FLength2$Station <-factor(FLength2$Station)

j<-ggplot(FLength2,aes(x=Length_mm,y=Station))
  j + geom_joy() +        # removes tails
  scale_y_discrete(expand = c(0.01, 0)) +  # removes cutoff top
  labs(x="Total Length (mm)",y="Station") +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"))
  
###Try with ridges instead of joy###  
  
FLength2$Station =factor(FLength2$Station, 
                        levels=c("71","66","44","45","87",
                                 "139","525","33","526","527"))

ggplot(FLength2, aes(Length_mm, Station, fill=..x..)) +
  geom_density_ridges_gradient(scale=2) +
  scale_y_discrete(expand = c(0.01, 0)) +  # removes cutoff top
  scale_color_viridis_c(alpha=1, name="Length (mm)", direction = -1,
                        aesthetics ="fill", option="C") + 
  labs(x="Total Length (mm)",y="Station", legend=NULL, 
       title="11-Week Length Distributions of 
Larval Coregonus by Sample Site") +
  theme_classic() +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"))

###And then with boxplot instead of ridges###
# Add a "#" before geom_jitter to get rid of jitter points
g<-ggplot(FLength2, aes(x=Station, y=Length_mm, fill=Group))+
  geom_boxplot(lwd= 0.5)
g +geom_jitter(size =1.75, width = 0.2, alpha=0.3)+
  scale_y_continuous(name ="Total length (mm)") +
  scale_x_discrete(name = "Station") +
  scale_fill_manual(values=c("#af8dc3","#7fbf7b"),
                    name="Island Group", 
                    breaks=c("Inner","Outer"), 
                    labels=c("Inner", "Outer")) +
  labs( title="11-Week Length Distributions of 
Larval Coregonus by Sample Site") +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"))


##===By Group============================================

ggplot(FLength2,aes(x=Length_mm,y=Group, fill=Group)) +
  geom_joy(scale=1.5) +        # removes tails
  scale_y_discrete(expand = c(0.01, 0)) +  # removes cutoff top
  scale_fill_manual(values=c("cadetblue3","royalblue4"))+
  labs(x="Total Length (mm)",y="Island Group", legend="Island Group",
       title="11-Week Length Distribution of Larval Coregonus by Island Group") +
  theme_classic() +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"))

######===================Water Temp================

WTemp <- merge(OverB, FCatch, by="Trawl")
WTemp2 <-merge(WTemp,FTrawl, by="Trawl")

g <- ggplot(WTemp2, aes(SurfTemp_c, Catch_ha))
g + geom_point(size=3)  +
  geom_smooth() +
  scale_y_continuous(name ="Coregonus per hectare") +
  scale_x_continuous(name = "Surface water temperature (C)") +
  labs(title="Larval Coregonus Density Variation with Surface Water Temperature") +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"),
        strip.text = element_text(size=10)) + 
  facet_grid(Group ~.) 

g <- ggplot(WTemp2, aes(x=Jday, y=SurfTemp_c))
g + geom_point(size=3)  +
  geom_smooth() +
  scale_y_continuous(name ="Surface water temperature (C)") +
  scale_x_continuous(name = "Julian Day") +
  labs(title="Larval Coregonus Length & Surface Water Temperature", 
       subtitle="Variation Across Island Groups Over 11 Weeks") +
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"),
        strip.text = element_text(size=10)) + 
  facet_grid(Group ~.) 

WTemp3 <-merge(WTemp2,FLength, by="Trawl")

g <- ggplot(WTemp3, aes(SurfTemp_c, Length_mm))
g + geom_point(size=3)  +
  geom_smooth() +
  scale_y_continuous(name ="Total length (mm)") +
  scale_x_continuous(name = "Surface water temperature (C)") +
  labs(title="Total Length Variations with Surface Water Temperature")+
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"),
        strip.text = element_text(size=10)) + 
  facet_grid(Group ~.) 
