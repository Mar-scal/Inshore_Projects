#import and calculate horse mussel length frequencies from samples from SPA 6 from only tows that caught HM
# 

library(tidyverse)


# data for all years and areas and all tows derived from SQL on github D:\GIthub\Inshore_Projects\Horse_Mussel\HorseMusselLengthFrequencies.sql
# Pulls horse mussel catches for every tow, even if zero catch but does NOT prorate to 800 m and 17.5 , but does take the catch to being a single 2 foot drag; 
#To get whole numbers that show the length frequencies of horse mussels that were caught on the survey, use the data extraction from this SQL, multiple by 2, and remove tows where sum of all length frequency bins = 0 
#i.e. remove tows that caught zero horse mussel
hm.lenfreq <- read.csv("Z:/Projects/MPC/CSAS_BioEcoOverviewSWNB/Data/HorseMusselLengthFrequencies.csv")

head(hm.lenfreq)

#just SPA 6 strata 
hm.lenfreq <- hm.lenfreq %>% filter(STRATA_ID %in% c(30, 31, 32))
dim(hm.lenfreq)

#just tows that have HM 
head(hm.lenfreq[,9:48])
hm.lenfreq$tot  <- apply(hm.lenfreq[,9:48],1,sum)
dim(hm.lenfreq)
head(hm.lenfreq)

#remove tows with zero HM 
hm.lenfreq <- hm.lenfreq %>% filter(tot > 0)
dim(hm.lenfreq)

#multiple by 2 to get whole numbers that correspond to total horse mussels per 2 drags of 2 feet each. (total 4 feet of lined gear) 
hm.lenfreq[,9:48] <- hm.lenfreq[,9:48]*2

head(hm.lenfreq)
hm.lenfreq <- hm.lenfreq %>% select(-tot)

head(hm.lenfreq)

#length frequencies of whole numbers that correspond to total horse mussels per 2 drags of 2 feet each. (total 4 feet of lined gear) 
write.csv(hm.lenfreq, "Z:/Projects/MPC/CSAS_BioEcoOverviewSWNB/Data/HorseMusselLengthFrequencies_SPA6_hm_from2lineddrags.csv")

