#####This pulls and formats inshore scallop tow location data for the ShinySpatialApp #####
###Amy Glass March 2023####
## This saves the .RDATA file into the E:/Inshore/Survey/ folder within the function.  
##This will need to be updated to a proper location
###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
#  
#  convert.dd.dddd    
#
###############################################################################################################

###############################################################################################################
# ARGUMENTS
# X:  
# format: 
#update source location
# source function ("E:/Inshore/RR_ShinySpatialApp/RR_scallsur_pull.r")

###############################################################################################################

###############################################################################################################
# EXAMPLE:
#
# RR_scallsur_pull(un.ID="chisholma", pwd.ID="******", db.con = "ptran")
# load(savelocation)
# head(data) # 20636 obs, 10 variables.
#
###############################################################################################################

RR_scallsur_pull<-function(un.ID, pwd.ID, db.con) 
  
{
  require(ROracle)
  con= dbConnect(dbDriver("Oracle"), un.ID, pwd.ID, db.con) 
  #using SCALLSUR.SCTOWS table because it only contains location data (no catch info)
  dat=dbGetQuery(con, ("SELECT MGT_AREA_ID, TOW_NO, TOW_DATE, START_LAT, START_LONG
FROM SCALLSUR.SCTOWS"))
  library(dplyr)
  library(lubridate)
  
  #convert lats and longs to decimal degrees
  funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r") 
  # Note: uses older contour.gen.r version (working on alternative to contour.gen altogether).
  dir <- getwd()
  for(fun in funcs) 
  {
    temp <- dir
    download.file(fun,destfile = basename(fun))
    source(paste0(dir,"/",basename(fun)))
    file.remove(paste0(dir,"/",basename(fun)))
  }
  dat$SLAT<-convert.dd.dddd(dat$START_LAT,format='dec.deg')
  dat$SLONG<-convert.dd.dddd(dat$START_LON,format='dec.deg')
  
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "1"] <- "Bay of Fundy"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "2"] <- "Bay of Fundy Approach"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "4"] <- "Bay of Fundy"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "5"] <- "Annapolis Basin"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "3"] <- "Brier/Lurcher"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "7"] <- "Brier/Lurcher"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "6A"] <- "Grand Manan"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "6B"] <- "Grand Manan"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "6C"] <- "Grand Manan"
  dat$MGT_AREA_ID[dat$MGT_AREA_ID == "29"] <- "SFA 29 West"
  
  #format column names
  names(dat)[1]<-"AREA"
  #format date and separate y m d
  dat$TOW_DATE = as.factor(as.Date(dat$TOW_DATE, format = "%Y-%m-%d"))
  dat<-dat %>% 
    mutate(TOW_DATE = ymd(TOW_DATE)) %>% 
    mutate_at(vars(TOW_DATE), list(year=year, month=month, day=day)) |> 
    arrange(TOW_DATE)
  
  savelocation <- paste0("Y:/Inshore/RR_ShinySpatialApp/ISdat_", year(Sys.Date()), ".csv")
  write.csv(dat, file = savelocation)

  #write.csv(dat, "Y:/Inshore/RR_ShinySpatialApp/ISdat.csv", row.names = FALSE)
  print("Data is saved here: Y:/Inshore/RR_ShinySpatialApp")
  
  
}

###############################################################################################################

#Run pull:

#un.ID <- un.sameotoj
#pwd.ID <- pw.sameotoj
un.ID=Sys.getenv("un.raperj") 
pwd.ID=Sys.getenv("pw.raperj") 
#un.ID <- keyring::key_list("Oracle")[1,2]
#pwd.ID <- keyring::key_get("Oracle", un.ID)



RR_scallsur_pull(un.ID=un.ID, pwd.ID=pwd.ID, db.con = "ptran")
data <- read.csv(savelocation)
head(data)
