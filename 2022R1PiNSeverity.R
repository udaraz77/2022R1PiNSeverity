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
  
  #hh.uuid
  #Note: need to be inlined with HNAP coding system, to enforce unique identifier for the HH records
  
  # UUID
  PiNSeverityData$uuid <- RData_Main$X_uuid
  
  #Index _ID 
  PiNSeverityData$id <- RData_Main$X_id
  
  #hh.weights [Note: still pending from HNAP side, last check with 24/March/2022]
  #PiNSeverityData$hhWeights <- RData_Main
  
  #Sub- District PCode
  PiNSeverityData$admin3PCode <- RData_Main$admin3
  
  #Community PCode 
  PiNSeverityData$admin4PCode <- RData_Main$admin4
  
  #Is this an IDP site? ?The term "IDP sites" refers to IDP camps, informal settlements and collective centers/shelters?
  PiNSeverityData$IDPSite <- RData_Main$Is_this_an_IDP_site
  
  # What population type is this household?
  PiNSeverityData$HHtype <- ifelse(RData_Main$HH_Type == 1, "Host-population", 
                                         ifelse(RData_Main$HH_Type == 2,"Returnee",
                                                ifelse(RData_Main$HH_Type == 3,"IDP",NA)))
  
  PiNSeverityData$communityType <- ifelse(RData_Main$locationType == 1, "Community",
                                          ifelse(RData_Main$locationType == 2,"Neighnorhood",
                                                 ifelse(RData_Main$locationType == 3,"Camp", NA)))

  #Type of settlement
  #PiNSeverityData$SettlemenType <-
  
  #Type of Shelter
  #PiNSeverityData$ShelterType <-
  
  # How many households are you sharing your shelter with?
  #PiNSeverityData$ShelterSharedHH <-
  
  # How many individuals live in this shelter?
  #PiNSeverityData$ShelterIndividuls <-
  
  # Do you have access to a functioning toilet in this shelter?
  #PiNSeverityData$ShelterFunctioningToilets <-
  
  # If yes, do you share this toilet with other households?
  #PiNSeverityData$ShelterToiletShared <-
  
  
  PiNSeverityData$TypeofSettlement <- RData_Main$HH_Type
  
  # W1. What water source did your household use the most in the last 30 days?
  PiNSeverityData$W1_MainWaterSource <- RData_Main$W1
  
  # W2. Did you have to use other sources of water in the last 30 days?
  PiNSeverityData$MixingWaterSource <- RData_Main$W2_YesNo
  
  # W2.1. What other water sources did you use?
  PiNSeverityData$MixingWaterSourceName <- RData_Main$W2
  
  #W.18. Can I have a glass of water to drink? (Request household for glass of water to drink, perform test and mark following. 1 Test / Household) Please mark one value
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
      
      # Reference Code
      {
        # Load answer options for the FRC question
        # W18_Answer_Option <- as.data.frame(RData_VariableDataOptions[RData_VariableDataOptions$list_name == "F20_List", c(2,3)])
        # W1_Answer_Option <-  as.data.frame(RData_VariableDataOptions[RData_VariableDataOptions$list_name == "q0601R", c(2,3)])
      }

      
      
      # Load FRC Relevant data 
      {
        # Ramy: this can also be implemented with elseif statement. 
        
        PiNSeverityData$IndicatorFRC_CHK <- RData_Main$W18
        
        PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "1", "IndicatorFRC"] <- 0
        PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "2", "IndicatorFRC"] <- 0.1
        PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "3", "IndicatorFRC"] <- 0.5
        PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "4", "IndicatorFRC"] <- 1
        PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "5", "IndicatorFRC"] <- 2
        PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "6", "IndicatorFRC"] <- 3
        PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "7", "IndicatorFRC"] <- -1
      }


      

      
      #WASH_WoS_Sector_HNOs\HNO-2023\Round-1\Exceltool
      
      
      # Ramy to check
      PiNSeverityData$IndicatorFRC_SS <- ifelse(PiNSeverityData$IndicatorFRC == -1, "",
                                                ifelse(PiNSeverityData$IndicatorFRC > 0 | ifelse(grep("Bottle", PiNSeverityData$MixingWaterSourceName, ignore.case=TRUE),TRUE,FALSE)| PiNSeverityData$MainWaterSource == Bottle,1,
                                                       ifelse(xxxx,3)))
      
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
