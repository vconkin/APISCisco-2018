## This pulls data from the cisco diet processing
## Files needed in path: the processing excel file which contains
## new measurements, yolk condition, and stomach contents

##=====Set up/format====================================
##Clear environment, exclude all blank cells
rm(list = ls(all.names = TRUE))
na.rm=TRUE
##=====Load Packages ===================================
library(car)
library(ggplot2)
library(plotly)
library(ggpubr)
library(ggridges)
library(plyr)
library(dplyr)
library(tidyr)
library(readxl)
library(grid)
library(gridExtra)

##=====Load in the data =================================
## Stomach_contents contains all contents of the original trawl fish,
## Stomach_Mod has all the fish plus those in zoop & genetics tows
excel_sheets("Stomachs/Larval_Coregonus_processing.xlsx")
PLength<-read_excel("Stomachs/Larval_Coregonus_Processing.xlsx", sheet="Length_Condition")
Contents<-read_excel("Stomachs/Larval_Coregonus_Processing.xlsx", sheet="Stomach_Mod")

##=====Stomach Content Proportion=========================
#Use the last row (totals) to make a table of all contents
Count<-as.numeric(subset(Contents[270,9:30]))
## list items individualy for formatting
Items<-c("Nauplii","Calanoid Copepodite","Cyclopoid Copepodite",      
               "Unknown/Fragment Calanoid","Unknown/Fragment Cyclopoid",
               "Rotifera","Limnocalanus macrurus","Holopedium gibberum",
               "L. minutus","L. sicilis","Daphnia sp.","Diacyclops thomasi",
               "Eucyclops sp.","Bythotrephes","Bosmina sp.","Diaphanosoma",
               "Invertebrate eggs","E. lacustrus","Acanthocyclops sp.",
               "Senecella calanoides","Leptodora kindi","Chironomid pupae")

## calculate percent of each item and bind into a grid.table object
Tot<-sum(Count)
Percent<-(((Count/Tot)*100))
Percent<-format(round(Percent,2), nsmall=0)
Props<-rbind(Items, Count, Percent)
###invert the table
Props<-t(Props)

colnames(Props)<-c("Food Items","Total Number\nof Items","Percent of\nTotal (%)")
tt1 <- ttheme_default(
  core=list(fg_params=list(hjust=0.02,x=0.05)),
  colhead=list(fg_params=list(fontface=4L)))
## if there is a plot in the plot area (you should have restarted R), use :
dev.off()
## then plot
grid.table(Props, theme=tt1)

##=====Stomachs with Food by Length========================
## Going forward, we need to remove those totals from the data frame
## and replace any NAs with zeros for zero counts
Contents<-Contents[-270,]
Contents[is.na(Contents)]<-0

## Subset the data and create a vector of n values, then calculate %
Full<-select(Contents, Length_Bin,InBin,WithFood) %>%
  group_by(Length_Bin) %>% 
  summarize(Percent=(sum(WithFood)/sum(InBin))*100, N = sum(InBin))

## Since we have no data for 24 mm, we need to create a list of n values
## as any list from the data will have fewer n values than x values
n<-c(3,1,5,18,66,75,72,105,82,57,38,39,22,18,4,12,2,2,0,1,1)

###Simple line
feeding_18<-ggplot(Full,aes(x=Length_Bin, y=Percent))+
  geom_line(linetype="longdash", color="#7b3294", size=1.4)+
  geom_point(size = 2)+
  scale_y_continuous(name ="Percent With Food\n",
                     breaks = seq(0,100, by =25),
                     limits = c(0,105),
                     expand = c(0,0)) +
  scale_x_continuous(name = "\nLength Bin (mm)", 
                     breaks = seq(6,26, by = 1),
                     limits = c(5.5, 26.5),
                     expand = c(0,0))+
  labs(title = "2018\n", tag = "(B)") +
  theme_classic() +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        plot.tag=element_text(size=12),
        plot.tag.position = "topleft") +
  annotate("text", x=c(6.20:26.20), y=2:2,label= n, size=2.5, na.rm = TRUE) +
  annotate("text", x=c(5.80:25.80), y=2:2, label= "n = ", size=2.5, na.rm = TRUE)

feeding_18
#For comparison, load in extracted Selgeby data and plot
Selgeby =read.csv("Selgeby_ExtractedData/Selgeby_FullStomachs.csv", header=TRUE)
labs<-Selgeby$n

feeding_74<-ggplot(Selgeby,aes(Bin, Calculated*100))+
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
ggarrange(feeding_74, feeding_18, ncol=2, nrow=1)

## Other visualizations for 2018 data
## Barplot with loess smoothed curve
ggplot(Full, aes(Length_Bin, Percent)) +
  geom_col(fill="#56B4E9") +
  geom_smooth(method="loess", se=FALSE) +
  scale_x_continuous(name ="Length bin (mm)") +
  scale_y_continuous(breaks=seq(0,100, by=20), 
                     expand = c(0,0),
                     name = "Percent with food") +
  theme_classic()

## Logistic curve
ggplot(Full, aes(Length_Bin, Percent/100)) +
  geom_col(fill="#56B4E9") +
  geom_smooth(method = "glm", 
              method.args = list(family = "quasibinomial"), 
              se = FALSE) +
  scale_y_continuous(name ="Proportion With Food", 
                     breaks=seq(0,1, by=0.2)) +
  scale_x_continuous(name = "Length Bin (mm)", 
                     breaks=seq(6,26, by= 2))+
  ggtitle("Proportion of Stomachs with Food: Quasibinomial Smooth") +
  theme(title=element_text(size=28, face="bold"),
        axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"))

###Smoothed logistic curve
ggplot(Full, aes(Length_Bin, (Percent/100)))+geom_point()+  
geom_smooth(method = "glm", 
            method.args = list(family = "quasibinomial"), 
            se = FALSE) +
  scale_y_continuous(name ="Proportion With Food") +
  scale_x_continuous(name = "Length Bin (mm)", breaks=seq(6,26, by= 2))+
  ggtitle("Proportion of Stomachs with Food:\n Quasibinomial Smooth") +
  theme_classic() +
  theme(title=element_text(size=28, face="bold"),
        axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"))

##====Stomachs with Food Over Time=========================
Full2<-select(Contents, Sample_Date,InBin,WithFood) %>%
  group_by(Sample_Date) %>% 
  summarize(Percent=(sum(WithFood)/sum(InBin))*100, N = sum(InBin))

n<-Full2$N
dates<-Full2$Sample_Date
###Simple line
ggplot(Full2,aes(x=Sample_Date, y=Percent))+
  geom_line(linetype="dashed", color="#E69F00", size=1)+
  geom_point() +
  geom_text(aes(label=paste0("n=",n )), y=1.04*max(Full2$Percent), size = 3) +
  scale_x_datetime(name = "Sample Date")+
  scale_y_continuous(name ="Percent with food",
                     limits = c(-1,105),
                     expand = c(0,0))+
  ggtitle("Percent of Stomachs with Food") +
  theme_classic()+
  theme(title=element_text(size=28, face="bold"),
        axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold")) 

##=====Yolk Sac Condition==================================
## set condition as a factor
PLength$Yolk_Sac_Cond<-as.factor(PLength$Yolk_Sac_Cond)
levels(PLength$Yolk_Sac_Cond)
PLength$Length_Bin<-as.factor(PLength$Length_Bin)

## Add 2 colorblind palettes and create n labels
pal<-c("#008837","#a6dba0","#7b3294")
## I prefer this one
pal1<-c("#80cdc1","#018571", "#dfc27d")

Yolk<-select(PLength,Length_Bin,Yolk_Sac_Cond)
table(Yolk)
n<-rowSums(table(Yolk))
## Plot condition by length bin using total number of fish
## to create a histogram-type plot
ggplot(PLength,aes(x=Length_Bin, fill=Yolk_Sac_Cond) )+
  geom_bar() +
  scale_x_discrete(na.translate=FALSE)+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = pal1) +
  labs(x="\nLength bin (mm)", y="No. of fish\n", fill = "Yolk Condition") +
  theme_classic() +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold")) +
  annotate("text", x=1.2:20.2, y=2:2,label=(n), size=2.5) +
  annotate("text", x=0.9:19.9, y=2:2, label= "n = ", size=2.5)

##Then break that down into proportions to fill the plot
ggplot(PLength,aes(x=Length_Bin, fill=Yolk_Sac_Cond))+
  geom_bar(position = "fill") +
  scale_x_discrete(na.translate=FALSE)+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = pal1) +
  labs(x="\nLength bin (mm)", y="No. of fish\n", fill = "Yolk Condition") +
  theme_classic() +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold")) +
  annotate("text", x=1.2:20.2, y=0.02:0.02,label=(n), size=2.5) +
  annotate("text", x=0.9:19.9, y=0.02:0.02, label= "n = ", size=2.5)

##=====Diet groups data wrangling=======================
## Select data for diet makeup and assign length bins size classes
diet<-select(Contents,Sample_Date,Site, Length_Bin,Nauplii:Chironomid_pupae)
levels <- c(5,11,16,21,26)
labels=c("Small", "Medium","Large", "Extra Large")
diet_grp<- mutate(diet, size = cut(Length_Bin, levels, labels = labels))

## Convert individual species categories into broad categories
diet_grp %>% select(Unknown_Fragment_Calanoid,Limnocalanus_macrurus,
            L.minutus, L.sicilis, E.lacustrus, Senecella_calanoides) %>% 
            rowSums(na.rm=TRUE) -> diet_grp$Calanoids
diet_grp %>% select(Unknown_Fragment_Cyclopoid,Diacyclops_thomasi,
            Eucyclops_sp.,Acanthocyclops_sp.) %>% 
            rowSums(na.rm=TRUE) -> diet_grp$Cyclopoids
diet_grp %>% select(Holopedium_gibberum,Daphnia_sp.,Bythotrephes,
                    Bosmina_sp., Diaphanosoma,Leptodora_kindi) %>% 
  rowSums(na.rm=TRUE) -> diet_grp$Cladocerans
diet_grp %>% select(Cal_Copepodite,Cyc_Copepodite) %>% 
  rowSums(na.rm=TRUE) -> diet_grp$Copepodites
diet_grp %>% select(Rotifera, Invertebrate_eggs, Chironomid_pupae) %>% 
  rowSums(na.rm=TRUE) -> diet_grp$Other

##=====Diet across time====================================
## Pull out just the variables we need to keep sorting and reassign broad 
## categories using gather to different prey types that can be called 
trim_diet<-select(diet_grp, Sample_Date, Site, Length_Bin, 
                  size, Nauplii, Calanoids:Other) %>%
  gather(Prey, Count, Nauplii:Other)%>% 
  group_by(Sample_Date, Prey) %>% 
  summarize(Total=sum(Count))

trim_diet$Prey<-as.factor(trim_diet$Prey)

## Total numbers of prey groups over time
total<-ggplot(trim_diet, aes(x=Sample_Date, y=Total, color=Prey, fill=Prey))+
  geom_area(position="stack", stat="identity")+
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Date", y = "Zooplankton Count\n", 
       color = "Prey Type", fill = "Prey Type")+
  theme_classic() +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size =10))
total

## Proportions of prey groups over time
## Remove May 21--3 fish all with empty stomachs
scaled<-ggplot(trim_diet, aes(Sample_Date, Total, color=Prey, fill=Prey)) +
  geom_density(alpha=0.8, position="fill", stat="identity", na.rm=TRUE) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_datetime(expand = c(0,0)) +
  labs(x = "Date", y = "Zooplankton Proportion\n", 
       color = "Prey Type", fill = "Prey Type")+
  theme_classic() +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size =10))
scaled

##=====Nauplii and Copepodites============================
## Use diet_grp to convert grouped prey types into counts, then 
## average over the number of fish per length bin
NC<-select(diet_grp, Sample_Date, Site, Length_Bin, 
                  size, Nauplii, Calanoids:Other) %>%
  gather(Prey, Count, Nauplii:Other)%>% 
  group_by(Length_Bin, Prey) %>% 
  filter(Prey == "Nauplii" | Prey == "Copepodites") %>%
  summarize(Total=sum(Count), n = n(), Avg_Per_Stomach = Total/n)

NC_18 <- ggplot(NC, aes(Length_Bin, Avg_Per_Stomach, color=Prey, fill=Prey)) +
  geom_line(size=1.4)+
  geom_point(size=2)+
  labs(title = "2018", tag = "(B)") +
  scale_x_continuous(name = "Length Bin (mm)", breaks=seq(6,26, by=3))+
  scale_y_continuous(name = "No. Per Stomach\n")+
  theme_classic2() +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        plot.tag=element_text(size=12),
        plot.tag.position = "topleft")

NC_18
### Add the Selgeby data ###
NC74<-read.csv("Selgeby_ExtractedData/NC.csv", header=TRUE)

NC_74<- ggplot(NC74, aes(Length_Bin, No.perStomach,color=Prey)) +
  geom_line(size=1.4)+
  geom_point(size=2)+
  labs(title = "1974", tag = "(A)") +
  scale_x_continuous(name = "Length Bin (mm)", 
                     breaks=seq(6,18, by=3),limits = c(6,18))+
  scale_y_continuous(name = "No. per stomach\n")+
  theme_classic2() +
  theme(axis.text=element_text(size=12, face="bold"), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        plot.tag=element_text(size=12),
        plot.tag.position = "topleft")

NC_74
ggarrange(NC_74,NC_18, ncol=1, nrow=2)

#Try by date instead of by length bin
NC2<-select(diet_grp, Sample_Date, Site, Length_Bin, 
            size, Nauplii, Calanoids:Other) %>%
  gather(Prey, Count, Nauplii:Other)%>% 
  group_by(Sample_Date, Prey) %>% 
  filter(Prey == "Nauplii" | Prey == "Copepodites") %>%
  summarize(Total=sum(Count), n = n(), Avg_Per_Stomach = Total/n)

ggplot(NC2, aes(Sample_Date, Avg_Per_Stomach, color=Prey, fill=Prey)) +
  geom_line(size=1.4)+
  geom_point(size=2)+
  scale_x_datetime(name = "Date")+
  scale_y_continuous(name = "Average No. Per Stomach",
                     limits = c(-1,105),
                     expand = c(0,0)) +
  theme_classic()

##=====Fish Length Distributions by Week==================
barfill<-"#56B4E9"
barlines<-"#1F3552"

PLength$Length_Bin<-as.numeric(PLength$Length_Bin)

## Create custom labels for the time intervals
Week_names<-c("20"="May 14-15",
              "21"="May 21-23", 
              "22"="May 29", 
              "23"="June 4-5", 
              "24"="June 12-13", 
              "25"="June 18-20",
              "26"="June 26-28",
              "27"="July 2-5", 
              "28"="July 9-11", 
              "29"="July 16-17", 
              "30"="July 23-25")

#Free y scale
ggplot(PLength, aes(Length_Bin))+
  geom_histogram(aes(y=..count..), binwidth = 1, position = "stack", 
                 color= barlines, fill=barfill)+
  scale_x_continuous(name = "\nLength bin (mm)")+
  scale_y_continuous(name ="No. of fish\n")+
  facet_wrap(nrow = 4, Week~., scales = "free_y",
           labeller = as_labeller(Week_names)) +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=10),
        strip.background = element_rect(color = "white", size=0.8))

#Scales to maximum
ggplot(PLength, aes(Length_Bin))+
  geom_histogram(aes(y=..count..), binwidth = 1, position = "stack", 
                 color= barlines, fill=barfill)+
  scale_x_continuous(name = "\nLength bin (mm)")+
  scale_y_continuous(name ="No. of fish\n")+
  facet_wrap(nrow = 4, Week~., labeller = as_labeller(Week_names)) +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=10),
        strip.background = element_rect(color = "white", size=0.8))

##=====Diet by size groups =================================
###look at size groups next###
sizes<-select(diet_grp, Sample_Date, Length_Bin, size, Nauplii, Calanoids:Other) %>%
  gather(Prey, Count, Nauplii:Other)%>% 
  group_by(Sample_Date, Prey, size) %>% 
  summarize(Total=sum(Count))

sizes$Prey<-as.factor(sizes$Prey)
sizegroups<-c("Small" = "Small\n5-11 mm",
              "Medium" = "Medium\n12-16 mm",
              "Large" = "Large\n17-21 mm",
              "Extra Large" = "Extra Large\n22-26 mm")

groups<-ggplot(sizes, aes(Sample_Date,Total, color=Prey, fill=Prey))
groups+geom_density(alpha=0.8, position="stack", stat="identity")+
  scale_x_datetime(name="Date") +
  scale_y_continuous(name ="No. of Zooplankton\n", expand = c(0,0)) +
  ggtitle("Diet Composition by Size Group") +
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=10),
        strip.background = element_rect(color = "white"))+
  facet_wrap(nrow=2, size~., scales="free_y", labeller = as_labeller(sizegroups))

###scaled doesn't provide a lot of useful info but here it is
gscaled<-ggplot(sizes, aes(Sample_Date, Total, color=Prey, fill=Prey))
gscaled+geom_density(alpha=0.7, position="fill", stat="identity")+
  scale_x_datetime(name="Date") +
  scale_y_continuous(name ="No. of Zooplankton\n", expand = c(0,0)) +
  ggtitle("Diet Composition by Size Group") +
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=10),
        strip.background = element_rect(color = "white"))+
  facet_wrap(nrow=2, size~., scales="free", labeller = as_labeller(sizegroups))

###diet histogram by length bin
###It works now!
bins<-select(diet_grp, Length_Bin, size, Nauplii, Calanoids:Other) %>%
  gather(Prey, Count, Nauplii:Other)%>% 
  group_by(Length_Bin, Prey) %>% 
  summarize(Total=sum(Count))

bins$Prey<-as.factor(bins$Prey)
bins$Length_Bin<-as.factor(bins$Length_Bin)

ggplot(bins,aes(x=Length_Bin,y=Total,fill=Prey))+
  geom_bar(stat="identity")+
  scale_y_continuous(name = "Total No. of items\n", 
                     breaks = seq(0,3000, by = 500),
                     expand = c(0,0))+
  scale_x_discrete(name = "Length bin (mm)") +
  theme_classic()+
  theme(axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        legend.text=element_text(size=10))

ggplot(bins,aes(x=Length_Bin,y=Total,fill=Prey))+
  geom_bar(stat="identity", position = "fill") +
  scale_y_continuous(name = "Total No. of items\n", expand = c(0,0))+
  scale_x_discrete(name = "Length bin (mm)") +
  theme_classic()+
  theme(title=element_text(size=10, face="bold"),
        axis.text=element_text(size=10, face="bold"), 
        axis.title=element_text(size=10,face="bold"), 
        plot.title=element_text(size=10,face="bold"),
        legend.text=element_text(size=10))

ungroup(diet_grp)
## Look at how items accumulate across length bins
bins2<-select(diet_grp, Length_Bin, size, Nauplii, Calanoids:Other) %>%
  gather(Prey, Count, Nauplii:Other)%>% 
  group_by(Length_Bin) %>% 
  summarize(n = n(), Total=sum(Count))%>%
  mutate(Running_Total = cumsum(Total))
View(bins2)

#Let's take a quick look at cumulative sums by prey type
cumulative<-ungroup(bins) %>%
  dplyr::arrange(Length_Bin)%>%
  group_by(Prey) %>%
  mutate(Cum_total = cumsum(Total)) %>%
  arrange(Prey)
View(cumulative)

##=====Diet by site========================================
## use the diet groups to look at diet composition over time
sites<-select(diet_grp, Sample_Date, Site, Length_Bin, Nauplii, Calanoids:Other) %>%
  gather(Prey, Count, Nauplii:Other)%>% 
  group_by(Sample_Date, Prey, Site) %>% 
  summarize(Total=sum(Count))

sites$Prey<-as.factor(sites$Prey)
sites$Site =factor(sites$Site, 
      levels=c("71","66","44","45","87","139","525","33","526","527"))

g<-ggplot(sites, aes(Sample_Date,Total, color=Prey, fill=Prey))
g+geom_density(alpha=0.8, position="stack", stat="identity")+
  scale_x_datetime(name="Date") +
  scale_y_continuous(name ="Prey Counts", expand = c(0,0)) +
  ggtitle("Diet Composition by Station") +
  theme_classic() +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12,face="bold"), 
        plot.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=10),
        strip.background = element_rect(color = "white"))+
  facet_wrap(nrow=2, Site~., scales="free_y")

##=====Diet by yolk condition==============================
#Start by making a data frame of trawl, length bin, and number within that bin
#We want to take an average length for each length bin, so the relationship
#is actually between length, yolk condition, and feeding
PLength$Yolk_Sac_Cond<-as.factor(PLength$Yolk_Sac_Cond)
PLength$TLength_mm<-as.numeric(PLength$TLength_mm)
Yolk_count<-select(PLength, Trawl, Sample_Date, TLength_mm, Length_Bin, Yolk_Sac_Cond)%>%
  group_by(Trawl, Length_Bin) %>%
  na.omit() %>%
  summarize(In_Bin= n(), L_avg = mean(TLength_mm))


median(PLength$TLength_mm, na.rm = TRUE)
min(PLength$TLength_mm, na.rm = TRUE)
max(PLength$TLength_mm, na.rm = TRUE)
#Then make a data frame of trawl, length bin, yolk condition for that bin, and 
#number of fish per length bin with that yolk condition
Yolk_cond<-select(PLength, Trawl, Sample_Date, Length_Bin, Yolk_Sac_Cond)%>%
  group_by(Trawl, Length_Bin, Yolk_Sac_Cond) %>%
  na.omit() %>%
  summarize(n= n()) 

sum(Yolk_cond$n)
sum(Yolk_count$In_Bin)

#Bind them together and filter out all instances where yolk condition is not
#uniform, i.e where number in bin is equal to number with a given yolk condition
Yolks<-left_join(Yolk_count,Yolk_cond, by = c("Trawl", "Length_Bin" ))%>%
  filter(In_Bin ==n)

sum(Yolks$n)
##Group the data by trawl number and species and calculate how many of each species
##we see at that site that day. Then remove all the zeros so we're only looking at 
##species observed for that site and day rather than overall
Stomachs<-select(Contents, Trawl, Length_Bin, Average_Food_items)
Stomachs<-Stomachs[-270,]
Stomachs[is.na(Stomachs)]<-0

#Some fish were examined for yolk condition but weren't dissected
#So the numbers are not perfect
SY<-inner_join(Yolks, Stomachs, by = c("Trawl", "Length_Bin")) %>%
  na.omit() %>% 
  mutate_if(is.numeric, round, 2)

p1<-c("#225ea8", "#41b6c4", "#a1dab4")
p2<-c("#5e3c99","#E69F00","#D55E00")

y<-ggplot(SY, aes(L_avg, Average_Food_items, 
                  shape=Yolk_Sac_Cond, color= Yolk_Sac_Cond))+
  geom_point(size = 4, alpha=0.8)+
  scale_y_continuous(name = "Average No. items per stomach\n", expand = c(0,0))+
  scale_x_continuous(name = "\nAverage length (mm)")+
  scale_shape_manual(values = c(16,15,17))+
  scale_color_manual(values = p2) +
  theme_classic()+
  labs(color = "Yolk Condition", shape = "Yolk Condition")+
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
y

##=======Items per stomach==========================================
barfill<-"#56B4E9"
barlines<-"#1F3552"

Stomachs$Length_Bin<-as.factor(Stomachs$Length_Bin)
Stomachs %>% group_by(Length_Bin) %>%
  summarize(Avg_contents = mean(Average_Food_items), n = n()) -> avg
n<-avg$n

ggplot(avg, aes(Length_Bin, Avg_contents))+
  geom_bar(stat="identity", color = barlines, fill = barfill)+
  scale_x_discrete(name = "\nLength Bin (mm)") +
  scale_y_continuous(name = "Average No. of Items\nPer Stomach\n",
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12)) +
  annotate("text", x=1.15:20.15, y=5:5,label=(n), size=3) +
  annotate("text", x=0.85:19.85, y=5:5, label= "n = ", size=3)
