###........................................###
###
###    Meat weight/shell height modelling
###    SPA 2 -- all years 
###    J.Sameoto Oct 2024 
###........................................###

options(stringsAsFactors=FALSE)

#required packages
library(tidyverse)
library(ROracle)
library(lme4)
library(lattice)
library(lubridate)
library(statmod)

# Define: 
uid <- un.sameotoj
pwd <- pw.sameotoj

## read in holistic sample data to be combined with mw-sh data in database 
holistic.data <- read.csv("Z:/Projects/SPA2/Data/holistic.mwsh.2024.csv")

dim(holistic.data)
head(holistic.data)
str(holistic.data)

#assessmentyear <- 2024 #year in which you are conducting the survey 
area <- "2"  #SPA assessing recall SPA 1A, 1B, and 4 are grouped; options: "1A1B4and5", "3", "6" 
path.directory <- "Z:/Projects/SPA2"


# ROracle; note this can take ~ 10 sec or so, don't panic
chan <- dbConnect(dbDriver("Oracle"),username=uid, password=pwd,'ptran')

# Set SQL query 
quer1 <- paste0("SELECT * FROM scallsur.scwgthgt")
# read in shell height and meat weight data from database
# detailed meat weight/shell height sampling data
detail.dat <- dbGetQuery(chan, quer1)

#numbers by shell height bin
quer2 <- paste0("SELECT * FROM scallsur.scliveres")
livefreq.dat <- dbGetQuery(chan, quer2)

#add YEAR column to data
detail.dat$YEAR <- year(detail.dat$TOW_DATE)
livefreq.dat$YEAR <- as.numeric(substr(livefreq.dat$CRUISE,3,6))


#Remove  SPA 2 tows which are those in STRATA_ID == 57 or 26 
dim(detail.dat)
detail.dat <- detail.dat[detail.dat$STRATA_ID %in% c(26, 57) ,]
dim(detail.dat)

dim(livefreq.dat)
#Remove SPA 2 tows those as MGT_AREA_ID == 2
livefreq.dat <- livefreq.dat[livefreq.dat$MGT_AREA_ID == 2,]
dim(livefreq.dat)

detail.dat %>% group_by(STRATA_ID ) %>% summarise(n())

#check years with data 
table(detail.dat$YEAR)
table(livefreq.dat$YEAR)


# Dates of samples; June in 2024, end of May 2006, end of Aug 1996 -- model each separately bc of confounding seasonal effect 
unique(as.Date(detail.dat$TOW_DATE))
#"1996-08-29" "2006-05-25" "2024-06-21" "2024-06-22" "2024-07-08"

### Merge mwsh data from db and from holistic sample data 
head(detail.dat)
head(holistic.data)

dat.all <- rbind(detail.dat %>% select(CRUISE, TOW_NO, SHELL_NO, WET_MEAT_WGT, HEIGHT, YEAR ), holistic.data %>% select(CRUISE, TOW_NO, SHELL_NO, WET_MEAT_WGT, HEIGHT, YEAR) ) 

table(dat.all$YEAR)
#1996 2006 2024 
#100   36  212

# read in depth information and add it to the meat weight/shell height dataframe
# there is no strata_id in Olex file to select on, need a unique identifier for tows to link to depth ($ID)
dat.all$ID <- paste(dat.all$CRUISE, dat.all$TOW_NO,sep='.')
uniqueID <- unique(dat.all$ID)
## This is unique IDs for database 

#get depth standardized to mean water level for all tows 
OlexTows_all <- read.csv("Y:/Inshore/StandardDepth/towsdd_StdDepth.csv")
names(OlexTows_all)[which(colnames(OlexTows_all)=="RASTERVALU")] <- "OLEXDEPTH_M"   #rename "RASTERVALU" column
OlexTows_all$OLEXDEPTH_M[OlexTows_all$OLEXDEPTH_M==-9999] <- NA
OlexTows_all$ID <- paste(OlexTows_all$CRUISE,OlexTows_all$TOW_NO,sep='.')
OlexTows_bof <- subset(OlexTows_all, ID%in%uniqueID)
#should match length(uniqueID)
dim(OlexTows_bof)

dim(dat.all)
dat.all <- merge(dat.all,subset(OlexTows_bof,select=c("ID","OLEXDEPTH_M")), all.x=T)
dim(dat.all)
#348   samples from SPA 2 over all years 
head(dat.all)

dat.all$ADJ_DEPTH <- dat.all$OLEXDEPTH_M
dat.all$ADJ_DEPTH[is.na(dat.all$OLEXDEPTH_M)] <- -1*dat.all$DEPTH[is.na(dat.all$OLEXDEPTH_M)] #*-1 because DEPTH is positive

#add depth to the livefreq dataframe
livefreq.dat$ID <- paste(livefreq.dat$CRUISE,livefreq.dat$TOW_NO,sep='.')
uniqueIDb <- unique(livefreq.dat$ID)

OlexTows_freq <- subset(OlexTows_all,ID%in%uniqueIDb)
livefreq <- merge(livefreq.dat,subset(OlexTows_freq,select=c("ID","OLEXDEPTH_M")), all=T)
livefreq$ADJ_DEPTH <- livefreq$OLEXDEPTH_M
livefreq$ADJ_DEPTH[is.na(livefreq$OLEXDEPTH_M)] <- -1*livefreq$DEPTH[is.na(livefreq$OLEXDEPTH_M)] #*-1 because DEPTH is positive
summary(livefreq)
dim(livefreq)
# 87 tows over all years in SPA 2 
table(livefreq$CRUISE)



# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
summary(dat.all)
dat.all <- dat.all[complete.cases(dat.all[,c(which(names(dat.all)=="WET_MEAT_WGT"))]),]  #remove NAs from WET_MEAT_WEIGHT
dat.all <- dat.all[complete.cases(dat.all[,c(which(names(dat.all)=="HEIGHT"))]),]  #remove NAs from HEIGHT
summary(dat.all)


#---- Meat weight shell height modelling ----

### SET #### 
surveyyear <- 2006  #2024, 2006, 1996

#Construct data.frame similar to GMlivenfreq for weight per tow
livefreqYYYY <- subset(livefreq, YEAR==surveyyear)
liveweightYYYY <- livefreqYYYY
#should be the same 
dim(livefreqYYYY)
dim(liveweightYYYY)
table(livefreqYYYY$YEAR)
table(liveweightYYYY$YEAR)

#Subset for year
detail.foryear <- subset(dat.all, YEAR==surveyyear)

## unique tows & observations 
table(detail.foryear$TOW_NO)
## only 3 tows with detailed samples  in 2006 
## only 2 tows with detailed samples in 1996


#create dataset for model
test.data <- subset(detail.foryear, HEIGHT>40)
test.data$Log.HEIGHT <- log(test.data$HEIGHT)
test.data$Log.HEIGHT.CTR <- test.data$Log.HEIGHT - mean(test.data$Log.HEIGHT)
test.data$Log.DEPTH <- log(abs(test.data$ADJ_DEPTH)) #take abs to keep value positive
test.data$Log.DEPTH.CTR <- test.data$Log.DEPTH - mean(test.data$Log.DEPTH)
summary(test.data)

#plot depths by tow
png(paste0("Z:/Projects/SPA2/Figures/SPA2_towdepth_detailedsamples_",surveyyear,".png")) #!!!DEFINE
plot(ADJ_DEPTH~TOW_NO, data=test.data)
dev.off()

#run model
MWTSH.YYYY <-  glm(WET_MEAT_WGT~Log.HEIGHT.CTR+Log.DEPTH.CTR, data=test.data, family=Gamma(link=log), na.action = na.omit)
#MWTSH.YYYY.2 <-  glm(WET_MEAT_WGT~Log.HEIGHT.CTR+Log.DEPTH.CTR, data=test.data, family=gaussian(link=log), na.action = na.omit)

summary(MWTSH.YYYY)
plot(MWTSH.YYYY)

## Plot of Pearson residuals 
#plot(density(resid(MWTSH.YYYY, type='pearson')))

## Plot of Standardized Pearson residuals 
#plot(density(rstandard(MWTSH.YYYY, type='pearson')))

## Plot of Standardized deviance residuals 
#plot(density(rstandard(MWTSH.YYYY, type='deviance')))

## Checking independence of observations; standardized deviance residuals 
#scatter.smooth(1:dim(test.data)[1], rstandard(MWTSH.YYYY, type='deviance'), col='gray')

## Plot residuals against fitted values
#scatter.smooth(predict(MWTSH.YYYY, type='response'), rstandard(MWTSH.YYYY, type='deviance'), col='gray')

## Q-Q plots 
#qqnorm(qresid(MWTSH.YYYY)); qqline(qresid(MWTSH.YYYY))

## Cooks distance 
#plot(cooks.distance(MWTSH.YYYY), type='h')
#cooksd_MWTSH.YYYY <- cooks.distance(MWTSH.YYYY)
#length(cooksd_MWTSH.YYYY[cooksd_MWTSH.YYYY > mean(cooksd_MWTSH.YYYY) * 2])
#test.data[23,]



#Save summary to txt file
sink(paste0("Z:/Projects/SPA2/Data/MWTSH_SPA2_",surveyyear,"_ModelSummary.txt"))
print(summary(MWTSH.YYYY))
sink()

#diagnostics
latt <- data.frame(test.data, res=residuals(MWTSH.YYYY,"pearson"),fit=fitted(MWTSH.YYYY)) #update model name
head(latt)

#Residuals vs fitted - full area 
#plot(MWTSH.YYYY$residuals ~ MWTSH.YYYY$fitted.values )
plot(latt$res~ latt$fit )


#Plot of fitted values 
#plot(MWTSH.YYYY$model$WET_MEAT_WGT   ~ MWTSH.YYYY$fitted.values )
#abline(0,1)
plot(latt$WET_MEAT_WGT   ~ latt$fit )
abline(0,1)



#create matrix of depths by tow to use in predict function
Log.height.ctr <- log(seq(2.5, 197.5, by = 5)) - mean(test.data$Log.HEIGHT) #each shell height bin to predict on
log.ctr.adj_depth <- log(abs(livefreqYYYY$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) #depth by tow

temp <- matrix(NA,dim(livefreqYYYY)[1],40)

tow.pred <- (1:dim(livefreqYYYY)[1])

#Predict using fixed effects for tows that weren't sampled for meat weight shell height
for(i in tow.pred) temp[i,] <- as.vector(predict(MWTSH.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i], 40)),type="response"))
temp

#multply temp matrix (weight) by live numbers to get weight/size bin
liveweightYYYY[,grep("BIN_ID_0", colnames(liveweightYYYY)):grep("BIN_ID_195", colnames(liveweightYYYY))] <- temp*livefreqYYYY[,grep("BIN_ID_0", colnames(livefreqYYYY)):grep("BIN_ID_195", colnames(livefreqYYYY))]

#export file for later analysis
write.csv(liveweightYYYY, paste0("Z:/Projects/SPA2/Data/SPA2liveweight",surveyyear,".csv")) 


rm(uid)
rm(pwd)
rm(un.sameotoj)
rm(pw.sameotoj)

### save workspace as .RData object
save.image(file = paste0("Z:/Projects/SPA2/Data/SPA2_",surveyyear,".RData"))

## save only the objects we need later for showing MWSH plots for different years on same plots  
save(MWTSH.YYYY, latt, liveweightYYYY, detail.foryear, livefreqYYYY,
     file=paste0("Z:/Projects/SPA2/Data/SPA2modeloutput_",surveyyear,".RData"))



# ----  Condition for Spatial Map ----
# Only calculates the point condition for the single year you modelled above (i.e. surveyyear that's set above) 

livefreq.condition.spatial <- livefreqYYYY
#adjust depth, must center on same mean as used in original model
livefreq.condition.spatial$log.ctr.adj_depth <- log(abs(livefreq.condition.spatial$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) 
#subset to just those tows that were detailed sampled 
livefreq.condition.spatial <- livefreq.condition.spatial[is.element(livefreq.condition.spatial$TOW_NO, unique(test.data$TOW_NO)),c("TOW_NO","START_LAT", "START_LONG", "ADJ_DEPTH","log.ctr.adj_depth")]
livefreq.condition.spatial$YEAR <- surveyyear #add year to dataframe


#Predict a meat weight for 100mm for just sampled tows; #re.form=NULL (all random effects included since only predicting on those tows that were sampled)

livefreq.condition.spatial$Condition <- predict(MWTSH.YYYY,newdata=data.frame(Log.HEIGHT.CTR=rep(log(100), dim(livefreq.condition.spatial)[1])-mean(test.data$Log.HEIGHT),
                                    Log.DEPTH.CTR=livefreq.condition.spatial$log.ctr.adj_depth, 
                                    TOW_NO=livefreq.condition.spatial$TOW_NO),
                                     type="response")

head(livefreq.condition.spatial)

#export for spatial plot later 
write.csv(livefreq.condition.spatial, paste0("Z:/Projects/SPA2/Data/SPA2_ConditionForSpatialPlot_",surveyyear,".csv"))




# ---- For Condition Time Series Figure ----
#Mean Depth of area 
livefreqYYYY %>% group_by(STRATA_ID) %>% summarise(mean_depth = mean(ADJ_DEPTH))
#2024
#STRATA_ID mean_depth
#1    26      -86.8
#2    57      -84.7


# 2006
#STRATA_ID mean_depth
#1    26      -89.9
#2    57      -80.2


#1996
#STRATA_ID mean_depth
#1  26    -79.7


#set predition depth 
depth.xx <- 85

condition.100mm <- predict(MWTSH.YYYY, newdata = data.frame(Log.HEIGHT.CTR=log(100) - mean(test.data$Log.HEIGHT), 
                                 Log.DEPTH.CTR = log(abs(depth.xx)) - mean(test.data$Log.DEPTH)), type = "response")

condition.100mm

### Write out 

#### End #### 





























