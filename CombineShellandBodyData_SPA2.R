### 
### SPA 2 Combine holistic sampling data for 2024 
### J.Sameoto Oct 2024 
###


library(tidyverse)


## read in body data and rename so consistent with database names 
bodies <- read.csv("Z:/Projects/Holistic_sampling_Inshore/Holistic_sampling_SPA2/2024/datasheets/SPA2 Holistic data 2024_Bodies.csv")

dim(bodies)
head(bodies)
str(bodies)
bodies$YEAR <- as.numeric(substr(bodies$Cruise,3,6))
names(bodies)
names(bodies)[grep("^Cruise$", names(bodies))] <- "CRUISE"
names(bodies)[grep("^Tow$", names(bodies))] <- "TOW_NO"
names(bodies)[grep("Scallop.ID", names(bodies))] <- "SHELL_NO"
names(bodies)[grep("Meat.colour..A.B.C.", names(bodies))] <- "MEAT_COLOUR"
names(bodies)[grep("Mycobacteria..Y.N.", names(bodies))] <- "MYCO_INFECTED"
names(bodies)[grep("Meat.wet.weight..g.", names(bodies))] <- "WET_MEAT_WGT"
bodies$ID <- paste(bodies$CRUISE, bodies$TOW_NO,sep='.')
head(bodies)


## read in shell data and rename so consistent with database names 
shells <- read.csv("Z:/Projects/Holistic_sampling_Inshore/Holistic_sampling_SPA2/2024/datasheets/SPA2 Holistic data 2024_Shells.csv")

dim(shells)
head(shells)
str(shells)
shells$YEAR <- as.numeric(substr(shells$Cruise,3,6))
names(shells)
names(shells)[grep("^Cruise$", names(shells))] <- "CRUISE"
names(shells)[grep("^Tow$", names(shells))] <- "TOW_NO"
names(shells)[grep("Shell.height..mm.", names(shells))] <- "HEIGHT"
names(shells)[grep("Shell.ID", names(shells))] <- "SHELL_NO"
shells$ID <- paste(shells$CRUISE, shells$TOW_NO,sep='.')
head(shells)

## Files should have same number of rows - check 
dim(bodies)[1] == dim(shells)[1]

### Combine data for dataset for MWSH modelling 
bodies.1 <- bodies %>% select(ID, CRUISE, TOW_NO, SHELL_NO , WET_MEAT_WGT, MEAT_COLOUR, MYCO_INFECTED, )
shells.1 <- shells %>% select(ID, CRUISE, TOW_NO, SHELL_NO , HEIGHT)

mwsh <- merge(bodies.1, shells.1, by = c("ID", "CRUISE","TOW_NO", "SHELL_NO"))
dim(mwsh)

unique(mwsh$CRUISE)
mwsh$YEAR <- as.numeric(substr(mwsh$CRUISE,3,6))
unique(mwsh$YEAR)

write.csv(mwsh, "Z:/Projects/SPA2/Data/holistic.mwsh.2024.csv", row.names = FALSE)


