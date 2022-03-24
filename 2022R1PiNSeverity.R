# The file to load and calculate the PiN Severity for HH data set
# Date source HNAP WASH HH assessment 2022 Round 1 [January - February]
# Author: Umar Daraz; udaraz@unicef.org, umar.daraz@gmail.com MY EDIT
# Contributes ( Ramy Zaki : rh.zaki@gmail.com)

# Loading relevant Libraries ----
  options(java.parameters = "-Xmx2048m")
  library(xlsx) #detach("package:xlsx", unload=TRUE)

# Data loading ----
{
   
  #Umar Computer
  filepath <- "C:\\Users\\udaraz\\OneDrive - UNICEF\\WASH_WoS_Sector_HNOs\\HNO-2023\\Round-1\\DataReceived_28022022\\"
  
  #rami Computer
  #filepath <- "C:\\Users\\udaraz\\OneDrive - UNICEF\\WASH_WoS_Sector_HNOs\\HNO-2023\\Round-1\\DataReceived_28022022\\"
  
  
  RData_Main <- read.csv(paste(filepath,"WASH_HH_Survey_Dataset_Feb_2022_Main.csv",sep=""),encoding = "UTF-8")
  RData_PresentMembers<-read.csv(paste(filepath,"WASH_HH_Survey_Dataset_Feb_2022_PresentMembers.csv",sep=""),encoding = "UTF-8")
  RData_AbsentMembers <- read.csv(paste(filepath,"WASH_HH_Survey_Dataset_Feb_2022_AbsentMembers.csv",sep=""),encoding = "UTF-8")
  RData_W15Repeat <- read.csv(paste(filepath,"WASH_HH_Survey_Dataset_Feb_2022_W15Repeat.csv",sep=""),encoding = "UTF-8")
  RData_Sample <- read.csv(paste(filepath,"WASH_HH_Survey_Dataset_Feb_2022_Sample.csv",sep=""),encoding = "UTF-8")
  RData_JanuaryBaseline <- read.csv(paste(filepath,"WASH_HH_Survey_Dataset_Feb_2022_JanuaryBaseline.csv",sep=""),encoding = "UTF-8")
  RData_VariableFullNames <- read.csv(paste(filepath,"WASH_HH_Survey_Dataset_Feb_2022_VariablesFullName.csv",sep=""),encoding = "UTF-8")
  RData_VariableDataOptions <- read.csv(paste(filepath,"WASH_HH_Survey_Dataset_Feb_2022_VariableDataOptions.csv",sep=""),encoding = "UTF-8")
  rm(filepath)
}
    

# Data cleaning Master [Over All]----

# Loading relevant data for PiN and SS in "PiNSeverityData" dataframe ----
{
  SN = c(1:nrow(RData_Main))
  PiNSeverityData <- as.data.frame(SN)
  
  PiNSeverityData$uuid <- RData_Main$X_uuid
  PiNSeverityData$id <- RData_Main$X_id
  PiNSeverityData$admin3PCode <- RData_Main$admin3
  PiNSeverityData$admin4PCode <- RData_Main$admin4
  PiNSeverityData$LocationType <- ifelse(RData_Main$locationType == 1, "Host-population", 
                                         ifelse(RData_Main$locationType == 2,"Returnee",
                                                ifelse(RData_Main$locationType == 3,"IDPs",NA)))

  PiNSeverityData$W1_MainWaterSource <- RData_Main$W1
  PiNSeverityData$MixingWaterSource <- RData_Main$W2_YesNo
  PiNSeverityData$MixingWaterSourceName <- RData_Main$W2
  PiNSeverityData$IndicatorFRC_CHK <- RData_Main$W18
  PiNSeverityData$IndicatorFRC <- RData_Main$W18
  
  
  
}

# Severity Scoring at HH ----
  ## indicators for severity scoring -----
  {

    ###Severity Scoring for Indicator 1.1 FRC ----
    {
      #Function Logic
      # if no data reported make it blank - no severity score assigned
      # if FRc > 0 and Only source is 
      
      # Load answer options for the FRC question
      #W18_Answer_Option <- as.data.frame(RData_VariableDataOptions[RData_VariableDataOptions$list_name == "F20_List", c(2,3)])
      W1_Answer_Option <-  as.data.frame(RData_VariableDataOptions[RData_VariableDataOptions$list_name == "q0601R", c(2,3)])
      # Load FRC Relevent data 
      PiNSeverityData$MainWaterSource <- RData_Main$W1
      PiNSeverityData$MixingWaterSource <- RData_Main$W2_YesNo
      PiNSeverityData$IndicatorFRC_CHK <- RData_Main$W18
      PiNSeverityData$IndicatorFRC <- RData_Main$W18
      
      PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "1", "IndicatorFRC"] <- 0
      PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "2", "IndicatorFRC"] <- 0.1
      PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "3", "IndicatorFRC"] <- 0.5
      PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "4", "IndicatorFRC"] <- 1
      PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "5", "IndicatorFRC"] <- 2
      PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "6", "IndicatorFRC"] <- 3
      PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "7", "IndicatorFRC"] <- -1
      
      #WASH_WoS_Sector_HNOs\HNO-2023\Round-1\Exceltool
      
      #=IFERROR(
      #IF(EC3/3/[@[HH_Average_Monthly_Income]]*100<=100,EC3/3/[@[HH_Average_Monthly_Income]]*100,[@[W.13.2. What percentage of monthly income is used on desludging of septic tank? (asked only if ?Connection to HH septic tank? reported in W.13)]]),[@[W.13.2. What percentage of monthly income is used on desludging of septic tank? (asked only if ?Connection to HH septic tank? reported in W.13)]])
      
      PiNSeverityData$IndicatorFRC_SS <- ifelse(PiNSeverityData$IndicatorFRC == -1, "",
                                                ifelse(PiNSeverityData$IndicatorFRC > 0 | xx |PiNSeverityData$MainWaterSource == Bottle,1,
                                                       ifelse(,3)))
      #=IF([@[W.18]="","",
      #IF(OR([@[W.18]]>0,Y3=1,N3="bottle"),1,
      #IF([@[W.4]]<>"No",3,
      #IF(OR([@[W1. What water source did your household use the most in the last 30 days?]]="Water trucking",
      #      [@[W1. What water source did your household use the most in the last 30 days?]]="Open well",
      #      [@[W1. What water source did your household use the most in the last 30 days?]]="River/Lake"),5,4))))
    }
    
    
    PiNSeverityData$uid <- RData_Main$X_id
    PiNSeverityData$uuid <- RData_Main$X_uuid
  }
  


# Summarizing at Sub District level ----

W2.Network
W2.Water_trucking
W2.Closed_well_network
W2.Closed_well_indivisual
W2.Open_well
W2.Springs
W2.River
W2.Bottle
W2.O
W2_OtWer 
