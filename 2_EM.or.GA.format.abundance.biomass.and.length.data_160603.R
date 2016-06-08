
###### Format MaxN, Length and 3D point that results from Checked data outputed from GlobalArchive or EventMeasure ######

### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# The following code forms an appendix to the manuscript:
#  "Langlois et al. 2015. Length selectivity of commercial fish traps assessed from in situ comparisons with stereo-videos: is there evidence of sampling bias? Fisheries Research"
# Please cite it if you like it

### Designed to take queries from GlobalArchive. 

### objective is to 

# 1. Import checked data
# 2. Make factors
# 3. Make species richness and total
# 5. Make mass estimates from Length
# 6. Write long and wide data sets for further analysis


# Naming conventions----
# data objects in lower case
# column names Capitalized


# Libraries required
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(googlesheets)


# Set directories----
rm(list=ls())
study<-"PMCP_Sharks"

data.dir=("~/ownCloud/PMCP_Montes_Transect")
data.dir=("C:/Tims Documents/ownCloud/PMCP_Sharks_Synthesis")

ga.check=paste(data.dir,"Data/GA to check",sep="/")
tidy.data=paste(data.dir,"Data/Tidy data",sep="/")


# Read in the data----
setwd(tidy.data)
dir()
maxn.factors<-read.csv("2016-06-03_PMCP_Sharks_GA_maxn.factors_.csv")
maxn<-read.csv("2016-06-03_PMCP_Sharks_GA_maxn_.csv")

length.factors<-read.csv("2016-06-03_PMCP_Sharks_GA_length.factors_.csv")
length<-read.csv("2016-06-03_PMCP_Sharks_GA_length_.csv")



# MAKE mass data from Length data----
setwd(life.history)
dir()
# master<-read_excel("2016-04-21_PMCP_master.species.xlsx")

# using life history from google sheets
(my_sheets <- gs_ls())
GlobalArchive_Life.history <- gs_title("GlobalArchive_Life history")#register a sheet
master<-GlobalArchive_Life.history%>%
  gs_read_csv(ws = "Life history")%>%
  filter(grepl('Australia', Global.region))%>%
  filter(grepl('Ningaloo|Pilbara', Local.region))

head(master,7)
str(master)
names(master)



# Make complete master and length data with no 3D points
master<-master%>%
  distinct(Genus_species)%>%
  filter(!is.na(a))%>%
  select(Genus_species,Family,a,b,aLL,bLL)

length.no.na<-length%>%
  filter(!is.na(Length))

# 1.Check if we have species length-weight relationship---
setwd(ga.check)
taxa.missing.lw <- length.no.na%>%
  distinct(Genus_species)%>%
  anti_join(master, by="Genus_species")%>%
  select(Genus_species)
head(taxa.missing.lw)
write.csv(taxa.missing.lw,file=paste(Sys.Date(),study,"taxa.missing.lw.csv",sep = "_"), row.names=FALSE)
#We have a few missing Taxa -   you can add these into the master table - or we can  use Family averages



#2. Check if any familys are missing?----
family.missing.lw <- length.no.na%>%
  distinct(Family)%>%
  anti_join(master, by="Family")%>%
  select(Family)
head(family.missing.lw)
# We have 3 familys missing - they will be dropped from Mass cal - unitl the LW are filled in

#3. Make a family average master table----
master.Family <- master %>%
  group_by(Family) %>%
  summarise(a = mean(a,na.rm = T),
            b = mean(b,na.rm = T),
            aLL = mean(aLL,na.rm = T),
            bLL = mean(bLL,na.rm = T))
head(master.Family)

#4. Fill length data with relevant a and b and if blank use family?----
length.taxa.ab<-master%>% #done this way around to avoid duplicating Family coloum
  select(-Family)%>%
  inner_join(length,., by="Genus_species")

length.family.ab<-length%>%
  anti_join(master, by="Genus_species")%>%
  semi_join(master, by="Family")

length.mass<-length.taxa.ab%>%
  bind_rows(length.family.ab)%>%
  filter(!is.na(a))%>% #this gets rid of species with no lw
  mutate(Length.cm = Length/10)%>%
  mutate(AdjLength = ((Length.cm*bLL)+aLL)) %>% # Adjusted length  accounts for a b not coming from for Fork length
  mutate(mass = (AdjLength^b)*a*Number)
head(length.mass)

#5. Check the mass estimates across species - in kg's----
setwd(ga.check)
x<-"mass"
mass<- length.mass %>%
  group_by(Genus_species) %>%
  summarise(Mean = mean(mass,na.rm = TRUE))%>%
  arrange(-Mean)
head(mass) #looks OK
write.csv(head(mass,50),file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)
# NOTES: on mass estimates
# All looking good -


# Write WIDE and LONG data from maxn, length and mass----
setwd(tidy.data)

# Function to fill Samples with no fish as zeros - could try complete() next time?
left_join_NA <- function(x, y, ...) {
  left_join(x = x, y = y, by = ...) %>% 
    mutate_each(funs(replace(., which(is.na(.)), 0)))
}

name<-"maxn.taxa.W.factors"
maxn.taxa.W <- maxn %>%
  group_by(Genus_species,OpCode) %>%
  summarise(Abundance = sum(MaxN))%>%
  spread(Genus_species,Abundance, fill = 0)%>%
  mutate(Total=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))#Add in Totals
    Presence.Absence <- maxn.taxa.W[,2:(ncol(maxn.taxa.W))]
for (i in 1:dim(Presence.Absence)[2]){
  Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
}
  maxn.taxa.W.factors<-maxn.taxa.W%>%
    mutate(Rich = rowSums(Presence.Absence,na.rm = TRUE))%>%
    left_join_NA(maxn.factors,., by="OpCode") #there are warnings here but they are OK just becasue of factors - there are no NA's in factors maxn.taxa.W.factors[is.na(x),]
    head(maxn.taxa.W.factors)
write.csv(maxn.taxa.W.factors, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)

name<-"maxn.taxa.L.factors"
data<-maxn.taxa.W.factors
out<-data%>%
  gather(key=Genus_species, value = Abundance, (match("Method",names(data))+1):ncol(data))
  head(out)
write.csv(out, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)


name<-"maxn.family.W.factors"
maxn.family.W.factors <- maxn %>%
  group_by(Family,OpCode) %>%
  summarise(Abundance = sum(MaxN))%>%
  spread(Family,Abundance, fill = 0)%>%
  left_join_NA(maxn.factors,., by="OpCode")
  head(maxn.family.W.factors)
write.csv(maxn.family.W.factors, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)

data<-maxn.family.W.factors
name<-"maxn.family.L.factors"
out<-data%>%
  gather(key=Genus_species, value = Abundance, (match("Method",names(data))+1):ncol(data))
head(out)
write.csv(out, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)


name<-"stereo.maxn.taxa.W.factors"
stereo.maxn.taxa.W <- length %>%
  group_by(Genus_species,OpCode) %>%
  summarise(Abundance = sum(Number))%>%
  spread(Genus_species,Abundance, fill = 0)%>%
  mutate(Total=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))  #Add in Totals
  Presence.Absence <- stereo.maxn.taxa.W[,2:(ncol(stereo.maxn.taxa.W))]
for (i in 1:dim(Presence.Absence)[2]){
  Presence.Absence[,i] <- ifelse(Presence.Absence[,i]>0,1,0)
}
stereo.maxn.taxa.W$Rich <- rowSums(Presence.Absence,na.rm = TRUE) #add in species richness
stereo.maxn.taxa.W.factors<-stereo.maxn.taxa.W%>%
  mutate(Rich = rowSums(Presence.Absence,na.rm = TRUE))%>%
  left_join_NA(length.factors,., by="OpCode") #there are warnings here but they are OK just becasue of factors - make sure we use length factors
  head(stereo.maxn.taxa.W.factors)
write.csv(stereo.maxn.taxa.W.factors, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)

data<-stereo.maxn.taxa.W.factors
name<-"stereo.maxn.taxa.L.factors"
out<-data%>%
  gather(key=Genus_species, value = Abundance, (match("Method",names(data))+1):ncol(data))
  head(out)
write.csv(out, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)


name<-"stereo.maxn.family.W"
stereo.maxn.family.W <- length %>%
  group_by(Family,OpCode) %>%
  summarise(Abundance = sum(Number))%>%
  spread(Family,Abundance, fill = 0)%>%
  left_join_NA(length.factors,., by="OpCode") #there are warnings here but they are OK just becasue of factors - make sure we use length factors
  head(stereo.maxn.taxa.W.factors)
write.csv(stereo.maxn.taxa.W.factors, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)

data<-stereo.maxn.taxa.W.factors
name<-"stereo.maxn.taxa.L.factors"
out<-data%>%
  gather(key=Genus_species, value = Abundance, (match("Method",names(data))+1):ncol(data))
head(out)
write.csv(out, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)


name<-"mass.taxa.W.factors"
mass.taxa.W.factors <- length.mass %>%
  group_by(Genus_species,OpCode) %>%
  summarise(mass = sum(mass))%>%
  spread(Genus_species,mass, fill = 0)%>%
  mutate(Total=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>% #Add in Totals
  left_join_NA(length.factors,., by="OpCode") #there are warnings here but they are OK just becasue of factors - make sure we use length factors
  head(mass.taxa.W.factors)
write.csv(mass.taxa.W.factors, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)

data<-mass.taxa.W.factors
name<-"mass.taxa.L.factors"
out<-data%>%
  gather(key=Genus_species, value = Mass, (match("Method",names(data))+1):ncol(data))
  tail(out)
write.csv(out, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)


name<-"mass.family.W.factors"
mass.family.W.factors <- length.mass %>%
  group_by(Family,OpCode) %>%
  summarise(mass = sum(mass))%>%
  spread(Family,mass, fill = 0)%>%
  left_join_NA(length.factors,., by="OpCode") #there are warnings here but they are OK just becasue of factors - make sure we use length factors
  head(mass.family.W.factors)
write.csv(mass.family.W.factors, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)

data<-mass.family.W.factors
name<-"mass.family.L.factors"
out<-data%>%
  gather(key=Genus_species, value = Mass, (match("Method",names(data))+1):ncol(data))
head(out)
write.csv(out, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)


# Expand and tidy Length by Number of raw Length data and merge with Factors----
names(length.no.na) #length with no 3d points
length.expanded<-length.no.na%>%
  select(OpCode,Family,Genus,Genus_species,Length,Number)
# Expand the Length data by Number coloum
# This does a lot here as there were lots of schools on some length measures
unique(length.expanded$Number)
length.expanded <- length.expanded[rep(seq.int(1,nrow(length.expanded)), length.expanded$Number), 1:5];head(length.expanded)

name<-"length.expanded.factors"
length.expanded.factors<-length.expanded%>%
  inner_join(length.factors,., by="OpCode") #using inner_join here as we don't want zero/NA if there is no length data in a sample
  head(length.expanded.factors)
write.csv(length.expanded.factors, file=paste(Sys.Date(),study,name,".csv",sep = "_"), row.names=FALSE)
  


# # ggmaps to check the spatial extent of the MaxN and Length data---- 
# ggmap is not currently working? try updatin the package
setwd(ga.check)

head(maxn.taxa.W.factors)
head(stereo.maxn.taxa.W.factors)

library(ggplot2)
library(ggmap)
# ### Montes
lat <- mean(maxn.taxa.W.factors$Latitude)                
lon <- mean(maxn.taxa.W.factors$Longitude) 
# 
# # base map
ggcheck <- get_map(location = c(lon , lat ), source = "stamen", maptype = "toner-lite", zoom = 8)
ggcheck.map<-ggmap(ggcheck)
ggcheck.map

# Plotting----

ggmap.opcode<-ggcheck.map+
  geom_text(aes(Longitude,Latitude,label=OpCode),size=4,data=maxn, nudge_x=0.05)+
  geom_point(data=maxn,aes(Longitude,Latitude),size=3,colour="red")+ 
  geom_point(data=length,aes(Longitude,Latitude),colour="green",size=3, alpha=0.5)+ 
   
  xlab('Longitude')+
  ylab('Lattitude')
#   coord_cartesian(xlim=c(114.5,115.25),ylim=c(-30.4,-29.9))
ggmap.opcode
ggsave(ggmap.opcode,file=paste(Sys.Date(),study,"ggmap.opcode.png",sep = "_"), width = 25, height = 14,units = "cm")


