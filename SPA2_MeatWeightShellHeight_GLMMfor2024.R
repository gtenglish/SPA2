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


### Merge mwsh data from db and from holistic sample data 
head(detail.dat)
head(holistic.data)

dat.all <- rbind(detail.dat %>% select(CRUISE, TOW_NO, SHELL_NO, WET_MEAT_WGT, HEIGHT, YEAR ), holistic.data %>% select(CRUISE, TOW_NO, SHELL_NO, WET_MEAT_WGT, HEIGHT, YEAR) ) 

table(dat.all$YEAR)

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

# check detail file for NAs (WET_MEAT_WEIGHT and HEIGHT) and positive depths
summary(dat.all)
dat.all <- dat.all[complete.cases(dat.all[,c(which(names(dat.all)=="WET_MEAT_WGT"))]),]  #remove NAs from WET_MEAT_WEIGHT
dat.all <- dat.all[complete.cases(dat.all[,c(which(names(dat.all)=="HEIGHT"))]),]  #remove NAs from HEIGHT
summary(dat.all)


#---- Meat weight shell height modelling ----

### SET #### 
surveyyear <- 2024  

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

#run model; update model name to correspond to year
MWTSH.YYYY <- glmer(WET_MEAT_WGT~Log.HEIGHT.CTR+Log.DEPTH.CTR+(Log.HEIGHT.CTR|TOW_NO),data=test.data,
                        family=Gamma(link=log), na.action = na.omit)

summary(MWTSH.YYYY)

#Save summary to txt file
sink(paste0("Z:/Projects/SPA2/Data/MWTSH_SPA2_",surveyyear,"_ModelSummary.txt"))
print(summary(MWTSH.YYYY))
sink()

#diagnostics
latt <- data.frame(test.data, res=residuals(MWTSH.YYYY,"pearson"),fit=fitted(MWTSH.YYYY)) #update model name

#Residuals vs fitted - full area 
png(paste0("Z:/Projects/SPA2/Figures/SPA2_MWTSH_resvfit_",surveyyear,".png"))
plot(MWTSH.YYYY, xlab="Fitted", ylab="Residuals") 
dev.off()

#Plot of tow level residuals
png(paste0("Z:/Projects/SPA2/Figures/SPA2_MWTSH_",surveyyear,"_towresid.png"))
xyplot(res~fit|as.factor(TOW_NO),data=latt, xlab="Fitted", ylab="Residuals",
  panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(h=0)
       })
dev.off()

#Plot of tow level fitted values 
png(paste0("Z:/Projects/SPA2/Figures/SPA2_MWTSH_",surveyyear,"_towfit.png"))
xyplot(WET_MEAT_WGT~fit|as.factor(TOW_NO),data=latt, xlab="Fitted meat weight (g)", ylab="Predicted meat weight (g)", 
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(0,1)
       })
dev.off()

#Construct data.frame similar to GMlivenfreq for weight per tow
livefreqYYYY <- subset(livefreq, YEAR==surveyyear)
liveweightYYYY <- livefreqYYYY
#should be the same 
dim(livefreqYYYY)
dim(liveweightYYYY)

#create matrix of depths by tow to use in predict function
Log.height.ctr <- log(seq(2.5, 197.5, by = 5)) - mean(test.data$Log.HEIGHT) #each shell height bin to predict on
log.ctr.adj_depth <- log(abs(livefreqYYYY$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) #depth by tow

temp <- matrix(NA,dim(liveweightYYYY)[1],40)

#Use random effects for tows in detail sample and fixed effects otherwise
#Random effects for tows that were sampled for meat weight shell height; here ID tows that were sampled  
random.pred <- (1:dim(liveweightYYYY)[1])[is.element(liveweightYYYY$TOW_NO,unique(test.data$TOW_NO))]

#fixed effects for tows that weren't sampled for meat weight shell height; here ID tows that were NOT sampled   
fixed.pred <- (1:dim(liveweightYYYY)[1])[!is.element(liveweightYYYY$TOW_NO,unique(test.data$TOW_NO))]

#Predict using Random effects for tows that were sampled for meat weight shell height
for(i in random.pred) temp[i,] <- as.vector(predict(MWTSH.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i], 40),TOW_NO=liveweightYYYY$TOW_NO[i]),re.form=NULL,type="response"))

#Predict using fixed effects for tows that weren't sampled for meat weight shell height
for(i in fixed.pred) temp[i,] <- as.vector(predict(MWTSH.YYYY,newdata=data.frame(Log.HEIGHT.CTR=Log.height.ctr,Log.DEPTH.CTR=rep(log.ctr.adj_depth[i], 40)), re.form=~0,type="response"))


#multply temp matrix (weight) by live numbers to get weight/size bin
liveweightYYYY[,grep("BIN_ID_0", colnames(liveweightYYYY)):grep("BIN_ID_195", colnames(liveweightYYYY))] <- temp*livefreqYYYY[,grep("BIN_ID_0", colnames(livefreqYYYY)):grep("BIN_ID_195", colnames(livefreqYYYY))]

#export file for later analysis
write.csv(liveweightYYYY, paste0("Z:/Projects/SPA2/Data/SPA2liveweight",surveyyear,".csv")) 


rm(uid)
rm(pwd)
rm(un.sameotoj)
rm(pw.sameotoj)

### save workspace as .RData object
save.image(file = paste0("Z:/Projects/SPA2/Data/SPA2",surveyyear,".RData"))

#NEW - save only the objects we need later
save(MWTSH.YYYY, latt, liveweightYYYY, GMdetail.foryear, GMlivefreq, livefreqYYYY,
     file=paste0(path.directory, assessmentyear, "/Assessment/Data/Growth/SPA",area,"/GMgrowth",surveyyear,".RData"))


#### End #### 









# ----  Condition for Spatial Map ----
# Only calculates the point condition for the single year you modelled above (i.e. surveyyear that's set above) 

livefreq.condition.spatial <- livefreqYYYY
#adjust depth, must center on same mean as used in original model
livefreq.condition.spatial$log.ctr.adj_depth <- log(abs(livefreq.condition.spatial$ADJ_DEPTH)) - mean(test.data$Log.DEPTH) 
#subset to just those tows that were detailed sampled 
livefreq.condition.spatial <- livefreq.condition.spatial[is.element(livefreq.condition.spatial$TOW_NO, unique(test.data$TOW_NO)),c("TOW_NO","START_LAT", "START_LONG", "ADJ_DEPTH","log.ctr.adj_depth")]
livefreq.condition.spatial$YEAR <- surveyyear #add year to dataframe


#Predict a meat weight for 100mm for just sampled tows; #re.form=NULL (all random effects included since only predicting on those tows that were sampled)

livefreq.condition.spatial$Condition <- predict(MWTSHGM.YYYY,newdata=data.frame(Log.HEIGHT.CTR=rep(log(100), dim(livefreq.condition.spatial)[1])-mean(test.data$Log.HEIGHT),
                                    Log.DEPTH.CTR=livefreq.condition.spatial$log.ctr.adj_depth, 
                                    TOW_NO=livefreq.condition.spatial$TOW_NO),
                                    re.form=NULL, type="response")

head(livefreq.condition.spatial)

#export for spatial plot later 
write.csv(livefreq.condition.spatial, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/GMConditionforMap",surveyyear,".csv"))


# ---- For Condition Time Series Figure ----

#mean.depth <- read.csv('Y:/INSHORE SCALLOP/BoF/StandardDepth/BoFMeanDepths.csv') #File for the constant depth to predict on by area
#DEFINE DEPTH:
#-54.74 is for VMS INSIDE AREA
#-56.65 is for VMS OUTER AREA


#Bring in file with depths by area, note some are by strata groups within area
mean.depth <- read.csv('Y:/Inshore/StandardDepth/BoFMeanDepths.csv')[ ,c("AREA", "MeanDepth_m")] #File for the constant depth to predict on by area
unique(mean.depth$AREA)
length(mean.depth$AREA)

mean.depth.GM <- mean.depth[mean.depth$AREA %in% c("SPA6 Modelled Area VMS IN",
                                                  "SPA6 OUT"),]
dim(mean.depth.GM)[1] == 2
unique(mean.depth.GM$AREA)


#create name column that matches condition file: 
mean.depth.GM$AREA_NAME <- NA
mean.depth.GM$AREA_NAME[mean.depth.GM$AREA == "SPA6 Modelled Area VMS IN"] <- "INVMS"
mean.depth.GM$AREA_NAME[mean.depth.GM$AREA == "SPA6 OUT"] <- "OUTVMS"

#add year, condition column 
mean.depth.GM$YEAR <- surveyyear
mean.depth.GM$Condition <- NA

for (i in 1:length(mean.depth.GM$AREA)){
  mean.depth.GM$Condition[i] <- predict(MWTSHGM.YYYY, newdata = data.frame(Log.HEIGHT.CTR=log(100) - mean(test.data$Log.HEIGHT),
                                                                           Log.DEPTH.CTR = log(abs(mean.depth.GM$MeanDepth_m[i])) - mean(test.data$Log.DEPTH)),
                                        re.form = NA, type = "response")  
}
mean.depth.GM
mean.depth.GM$Condition <- round(mean.depth.GM$Condition,3)


#Import previous year condition file: 
#note contains IN and OUT VMS strata condition; they don't vary much since the only predictor is depth and SH is constant at 100mm 
GM.con.ts <- read.csv(paste0(path.directory, assessmentyear-1,"/Assessment/Data/SurveyIndices/SPA6/SPA6_ConditionTimeSeries.csv"))

#GM.con.ts <- GM.con.ts[GM.con.ts$YEAR!=2019,] 
#GM.con.ts <- GM.con.ts %>% add_row(YEAR = 2020, STRATA = "INVMS", CONDITION = NA) %>% #Add in condition NA for 2020
#  add_row(YEAR = 2020, STRATA = "OUTVMS", CONDITION = NA)

#update timeseries and write out new file: 
GM.con.ts <- rbind(GM.con.ts %>% select(YEAR, STRATA, CONDITION), mean.depth.GM %>% select("YEAR",STRATA="AREA_NAME",CONDITION="Condition"))

GM.con.ts <- GM.con.ts[order(GM.con.ts$STRATA, GM.con.ts$YEAR),]
GM.con.ts
GM.con.ts$CONDITION <- round(GM.con.ts$CONDITION,3)

write.csv(GM.con.ts, paste0(path.directory, assessmentyear, "/Assessment/Data/SurveyIndices/SPA",area,"/SPA6_ConditionTimeSeries.csv"))


#... Plot Condition Time Series Figure:
GM.con.ts$strata.name <- NA 
GM.con.ts$strata.name[GM.con.ts$STRATA=="INVMS"] <- "Inside VMS"
GM.con.ts$strata.name[GM.con.ts$STRATA=="OUTVMS"] <- "Outside VMS"

condition.ts.plot <- ggplot(GM.con.ts %>% filter(STRATA %in% c("INVMS", "OUTVMS")),
                            aes(x=YEAR, y=CONDITION,group_by(strata.name), color=strata.name)) +  
  geom_line(aes(linetype=strata.name)) + geom_point( size = 3, aes(shape=strata.name)) +
  xlab("Year") + ylab("Condition (meat weight, g)") + theme_bw() +
  coord_cartesian(ylim=c(8, 20)) +
  scale_y_continuous(breaks=seq(5, 20, 5))+
  #scale_x_continuous(breaks=seq(1995,2023, 2))+
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        legend.position = c(.008, .20),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6),
        legend.title = element_blank(),
        text = element_text(size=20)) +
  guides(linetype=guide_legend(keywidth = 2.0, keyheight = 1.2))
condition.ts.plot


#Export plot 
ggsave(filename = paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA6_ConditionTimeSeries.png"), plot = condition.ts.plot, scale = 2.5, width = 9, height = 6, dpi = 300, units = "cm", limitsize = TRUE)

#png(paste0(path.directory, assessmentyear, "/Assessment/Figures/SPA6_ConditionTimeSeries.png" ),res = 200, width = 900, height = 600 )
#condition.ts.plot
#dev.off()
































