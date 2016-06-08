
###### Checking and writing tidy MaxN, Length and 3D points from EventMeasure or GlobalArchive queries 


### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au

# The following code forms an appendix to the manuscript:
#  "Langlois et al. 2015. Length selectivity of commercial fish traps assessed from in situ comparisons with stereo-videos: is there evidence of sampling bias? Fisheries Research"
# Please cite it if you like it

### Designed to: 
#   check data resulting in queries from GlobalArchive or EventMeasure. 
#   write tidy data for futher analyses. 

### objective is to 
# 1. Import data and add Genus_species column
# 2. run BASIC data checks
# 3. Limit lenght data by range and precision rules
# 4. run SERIOUS checks against a master species list
# 5. Remove species that can never be ID'd
# 6. Visualise what MaxN are missing in the stereoMaxN


# Naming and formatting conventions----
# data objects in lower case
# column names Capitalized
# folds indicated by - - - -
# always has a double return after a fold


# Libraries required----
library(tidyr)
library(plyr) #need to laod before dplyr()
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(googlesheets)


# Functions required----
# General search and replace
  gsr <- function(Source, Search, Replace) { 
    if (length(Search) != length(Replace))     stop("Search and Replace Must Have Equal Number of Items\n") 
    Changed <- as.character(Source) 
    for (i in 1:length(Search)) 
    { 
      cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
      Changed <- replace(Changed, Changed == Search[i], Replace[i])   } 
    cat("\n")    
    Changed 
  }


# Set directories----
rm(list=ls()) #clear memory
study<-"PMCP_Sharks"

# data.dir=("~/ownCloud/PMCP_Montes_Transect")
data.dir=("C:/Tims Documents/ownCloud/PMCP_Sharks_Synthesis")

ga.export=paste(data.dir,"Data/GA export",sep="/")
ga.check=paste(data.dir,"Data/GA to check",sep="/")
tidy.data=paste(data.dir,"Data/Tidy data",sep="/")


# Loop in MaxN files----
setwd(ga.export)
dir()
maxn.files <- list.files(pattern="_maxN.txt")
maxn <- NULL
for (f in maxn.files) {
  dat <- read.delim(f, header=T, stringsAsFactors = FALSE,strip.white = TRUE,na.strings = c("", " "))
  dat$Year <- unlist(strsplit(f,split="_",fixed=T))[1]
  dat$Month <- unlist(strsplit(f,split="_",fixed=T))[2]
  dat$Campaign <- unlist(strsplit(f,split="_",fixed=T))[3]
  dat$Method <- unlist(strsplit(f,split="_",fixed=T))[4]
  maxn <- rbind(maxn, dat)
}
# Check maxn
head(maxn,2)
unique(maxn$Campaign)
unique(maxn$Method)


# Loop in length/3d files----
length.files <- list.files(pattern="_Len3DPoints.txt")
length <- NULL
for (f in length.files) {
  dat <- read.delim(f, header=T, stringsAsFactors = FALSE,strip.white = TRUE,na.strings = c("", " "))
  dat$Year <- unlist(strsplit(f,split="_",fixed=T))[1]
  dat$Month <- unlist(strsplit(f,split="_",fixed=T))[2]
  dat$Campaign <- unlist(strsplit(f,split="_",fixed=T))[3]
  dat$Method <- unlist(strsplit(f,split="_",fixed=T))[4]
  length <- rbind.fill(length, dat)
}
# Check length
head(length,2)
unique(length$Campaign)
unique(length$Method)


# Specify if using EM or GA data----
# length$Length<-length$Length..mm. #ONLY for EM exports
# length$Range<-length$Range..mm.  #ONLY for EM exports
maxn$MaxN<-maxn$MaxSumN  #ONLY for GA exports


# Making Genus_species----
maxn<-maxn%>%
  mutate(Genus = ifelse(Genus == "", Family,Genus))%>% #fill in any blank Genus names with family
  mutate(Genus_species = paste(Genus, Species, sep = ' ')) #paste Genus species together

length<-length%>%
  mutate(Genus = ifelse(Genus == "", Family,Genus))%>% #fill in any blank Genus names with family
  mutate(Genus_species = paste(Genus, Species, sep = ' ')) #paste Genus species together


# Filter spatial extent of data for the particular study----
# maxn<-maxn%>%
#   filter(Campaign =="Montes.transect")
# 
# length<-length%>%
#   filter(Campaign =="Montes.transect")

# NOPE - we want deployments from both fished and SZs across all the Campaigns


# Make Factors to merge back in after summarises -----
# Factors are in the data. Take them from the data
maxn.factors<-maxn%>%
  select(-c(Family,Genus,Species,MaxSumN,MaxN,Genus_species))%>%
  distinct()

length.factors<-length%>%
  select(-c(Period,PeriodTime,Time,Length,Precision,RMS,Range,Direction,HorzDir,VertDir,MidX,MidY,MidZ,Family,Genus,Species,Code,Number,Stage,Activity,Comment_x,X,Y,Z,Genus_species))%>%
  distinct()


# Filter taxa for a particular study----
maxn<-maxn%>%
  filter(Family%in%c("Carcharhinidae","Sphyrnidae","Triakidae","Hemigaleidae"))

length<-length%>%
  filter(Family%in%c("Carcharhinidae","Sphyrnidae","Triakidae","Hemigaleidae"))


# BASIC Checks----
setwd(ga.check)
head(maxn)
head(length)
unique(maxn$Campaign)
unique(length$Campaign)

str(maxn)
maxn$MaxN<-as.numeric(maxn$MaxN)

str(length)
length$Number<-as.numeric(length$Number)
length$Length<-as.numeric(length$Length)
length$Range<-as.numeric(length$Range)


# Check if we have 3d points (Number) in addition to length----
na.lengths<-filter(length,is.na(Length))
three.d.points<-filter(na.lengths,!is.na(Number))
head(three.d.points) #if there are any records here we have 3d points - yes we do


# Check if we have schools associated with single length measures----
schools<-filter(length,Number>1) #no we  do not
head(schools)


#finding sync points that are not fish----
# normally Lengths without a Number
sync.points<-filter(length,is.na(Number))
head(sync.points) 


#Standardise for RANGE and Error for Length----
# To standardise for RANGE and Error we can remove any length observations outside Range and Error rules
# i.e. the length data, and any abundnance calculated from it, will be restricted by range
setwd(ga.check)
summary(length$Range)
  out.of.range<-filter(length,Range>10000);head(out.of.range)
  length <- filter(length,Range < 10000)
write.csv(out.of.range,file=paste(Sys.Date(),study,"out.of.range.csv",sep = "_"), row.names=FALSE)


# Check on the BIG fish length data----
fish.greater.than.1.meter<-filter(length,Length>1000) #All sharks, 
head(fish.greater.than.1.meter)
write.csv(fish.greater.than.1.meter,file=paste(Sys.Date(),study,"fish.greater.than.1.meter.csv",sep = "_"), row.names=FALSE)


# Plot to visualise length data----
ggplot(data=length, aes(as.numeric(Length))) + 
  geom_histogram(aes(y =..density..), 
                 col="red", 
                 fill="blue", 
                 alpha = .2) 


# Plot to visualise range data----
ggplot(data=length, aes(as.numeric(Range))) + 
  geom_histogram(aes(y =..density..), 
                 col="red", 
                 fill="green", 
                 alpha = .2) 



# SERIOUS data checking to compare taxa and min/max lengths----
# # Read in Species list to compare against----
# using life history from google sheets
(my_sheets <- gs_ls())
GlobalArchive_Life.history <- gs_title("GlobalArchive_Life history")#register a sheet
master<-GlobalArchive_Life.history%>%
  gs_read_csv(ws = "Life history")%>%
  filter(grepl('Australia', Global.region))%>%
  filter(grepl('Ningaloo|Pilbara', Local.region))

head(master)
str(master)


# Update names of species that may have changed----
change<-filter(master,!Change.to=="No change")
head(change)

# For MaxN
maxn$Genus_species <- gsr(maxn$Genus_species, change$Genus_species, change$Change.to)
# For Length
length$Genus_species <- gsr(length$Genus_species, change$Genus_species, change$Change.to)


# Check for taxa.not.match----
setwd(ga.check)


x<-"maxn.taxa.not.match" #a quick look at taxa that do not match master list
maxn.taxa.not.match<-
  master%>%
  select(Genus_species)%>%
  anti_join(maxn,.,by="Genus_species")%>%
  distinct(Genus_species)%>% 
  select(Genus_species)
head(maxn.taxa.not.match)
write.csv(maxn.taxa.not.match,file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)


x<-"maxn.taxa.and.opcode.not.match" #more useful list of taxa that do not match by OpCode
maxn.taxa.and.opcode.not.match<-
  master%>%
  select(Genus_species)%>%
  anti_join(maxn,.,by="Genus_species")%>%
  distinct(Genus_species,OpCode)%>% 
  select(Genus_species,OpCode)
head(maxn.taxa.and.opcode.not.match)
write.csv(maxn.taxa.and.opcode.not.match,file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)


x<-"length.taxa.not.match" #quick look at taxa in length that don't match
length.taxa.not.match<-
  master%>%
  select(Genus_species)%>%
  anti_join(length,.,by="Genus_species")%>%
  distinct(Genus_species)%>% 
  select(Genus_species)
head(length.taxa.not.match)
write.csv(length.taxa.not.match,file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)


x<-"length.taxa.and.opcode.not.match" #a more useful list of taxa in lenght that do not match
length.taxa.and.opcode.not.match<-
  master%>%
  select(Genus_species)%>%
  anti_join(length,.,by="Genus_species")%>%
  distinct(Genus_species,OpCode)%>% 
  select(Genus_species,OpCode)
head(length.taxa.and.opcode.not.match)
write.csv(length.taxa.and.opcode.not.match,file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)



### SERIOUS Check for Min Max Length compared to Master list----
x<-"wrong.length"
# Before running the length check we must NOT have any non-matching taxa - so first remove these from
keep<-select(master,Genus_species)

length10<-length%>%
  semi_join(keep,by="Genus_species")%>%
  filter(!is.na(Length))

# Make a vector of names to compare against
Genus_species.Vec<- sort(unique(length10$Genus_species)) #Need to order by name

# Make a dummy list for checking
wrong.length=vector('list',length=length(Genus_species.Vec))
names(wrong.length)=Genus_species.Vec
Matching.Species.Table=filter(master,Genus_species%in%Genus_species.Vec)
Matching.Species.Table=Matching.Species.Table[order(Matching.Species.Table$Genus_species),]
head(Matching.Species.Table)
Min=Matching.Species.Table$Min_length #Vector of Min lengths
Max=Matching.Species.Table$Max_length #Vector of Max lengths
names(Min)=names(Max)=Matching.Species.Table$Genus_species #Add names to the Min and Max - very important vectors are in order

# Run the loop to check the length data---
test=NA
for(i in 1:length(Genus_species.Vec))  
{
  
  Data=subset(length10,Genus_species==Genus_species.Vec[i])
  Data=subset(Data,!is.na(Length))
  test=which(Data$Length  <Min[i])
  test=c(test,which(Data$Length  >Max[i]))
  test=Data[test,]
  wrong.length[[i]]=test
}
wrong.length1<-do.call(rbind,wrong.length)

# Merge with Matching.Species.Table
wrong.length.taxa<-wrong.length1%>%
  inner_join(Matching.Species.Table,by="Genus_species")%>%
  select(OpCode, Genus_species,Length,Min_length,Max_length,everything())
head(wrong.length.taxa)

write.csv(wrong.length.taxa,file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)



###########################################
# # # Check how many MaxN per Genus_species are missing from StereoMaxN----
# e.g. how many lengths are missing from the possible MaxN
#############################################
setwd(ga.check)

length.to.match.maxn<-master%>%
  select(Genus_species)%>%
  semi_join(length,.,by="Genus_species")%>%
  distinct(Genus_species)%>% 
  select(Genus_species)%>%
  semi_join(maxn,.,by="Genus_species")%>%
  semi_join(length,.,by="Genus_species")%>% 
  select(Family,Genus_species,Number,OpCode)%>%
  mutate(Data = "Length")

length.OpCode <- length.to.match.maxn %>%
  distinct(OpCode)%>% 
  select(OpCode)


maxn.match.length<-master%>%
  select(Genus_species)%>%
  semi_join(maxn,.,by="Genus_species")%>%
  distinct(Genus_species)%>% 
  select(Genus_species)%>%
  semi_join(length,.,by="Genus_species")%>%
  semi_join(maxn,.,by="Genus_species")%>%
  semi_join(length.OpCode,by="OpCode")%>% # subset maxn to only those OpCode that match OpCodes from length
  select(Family,Genus_species,MaxN,OpCode)%>%
  mutate(Data = "MaxN")%>%
  rename(Number = MaxN)%>%
  bind_rows(length.to.match.maxn)


# Summarise the matched data by taxa
x<-"taxa.maxn.vs.stereo.summary"
taxa.maxn.vs.stereo.summary <- maxn.match.length %>%
  group_by(Genus_species,Family,OpCode,Data) %>%
  summarise(MaxN = sum(Number))%>%
  spread(Data,MaxN)%>%
  mutate(Percent.diff = (MaxN-Length)/MaxN)
head(taxa.maxn.vs.stereo.summary)
write.csv(taxa.maxn.vs.stereo.summary,file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)

# Summarise the matched data by family
x<-"family.maxn.vs.stereo.summary"
family.maxn.vs.stereo.summary <- maxn.match.length %>%
  group_by(Family,OpCode,Data) %>%
  summarise(MaxN = sum(Number))%>%
  spread(Data,MaxN)%>%
  mutate(Percent.diff = (MaxN-Length)/MaxN)
head(family.maxn.vs.stereo.summary)
write.csv(family.maxn.vs.stereo.summary,file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)


# Plot of MaxN versus StereoMaxN by family----

head(family.maxn.vs.stereo.summary)

x<-"ggMaxNCheckzoomout"
ggMaxNCheckzoomout<-ggplot(data=family.maxn.vs.stereo.summary,aes(x=MaxN,y=Length,colour=Family))+
  geom_point()+
  geom_text(aes(label=OpCode),hjust=0, vjust=0)+
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  geom_abline()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  ylab("StereoMaxN")+
  xlim(0, 45)+
  ylim(0, 45)
# coord_equal()
ggMaxNCheckzoomout
ggsave(ggMaxNCheckzoomout,file=paste(Sys.Date(),study,x,".png",sep = "_"),width = 8, height = 8,units = "in")

x<-"ggMaxNCheckzoomin"
ggMaxNCheckzoomin<-ggplot(data=family.maxn.vs.stereo.summary,aes(x=MaxN,y=Length,colour=Family))+
  geom_point()+
  geom_text(aes(label=OpCode),hjust=0, vjust=0,angle=330)+
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  geom_abline()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  ylab("StereoMaxN")+
  coord_cartesian(xlim = c(-2,16), ylim = c(-2,16))
ggMaxNCheckzoomin
ggsave(ggMaxNCheckzoomin,file=paste(Sys.Date(),study,x,".png",sep = "_"),width = 8, height = 8,units = "in")



###
### Congratulate yourself that you will not have to do checking of species and lengths using filter and sort functions in Excel.
##


# WRITE FINAL checked data----
setwd(tidy.data)
dir()

x<-"GA_maxn.factors"
write.csv(maxn.factors, file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)

x<-"GA_maxn"
GA_maxn<-
  master%>%
  select(Genus_species)%>%
  semi_join(maxn,.,by="Genus_species")
write.csv(GA_maxn, file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)


x<-"GA_length.factors"
write.csv(length.factors, file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)

x<-"GA_length"
# USE THIS PART IF YOU WANT TO REMOVE LENGTHS OUTSIDE THE MIN/MAX OF MASTER LIST
# drop.length<-wrong.length.taxa %>% 
#   distinct(OpCode, Genus_species,Length)%>%
#   select(OpCode, Genus_species,Length)%>%
#   mutate(key = paste(OpCode, Genus_species, Length, sep = '_'))
GA_length<-
  master%>%
  select(Genus_species)%>%
  semi_join(length,.,by="Genus_species")%>%
  mutate(key = paste(OpCode, Genus_species, Length, sep = '_'))
#   anti_join(drop.length,by="key") #for dropping wrong.lengths
write.csv(GA_length, file=paste(Sys.Date(),study,x,".csv",sep = "_"), row.names=FALSE)





