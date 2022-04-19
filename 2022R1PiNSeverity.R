# The file to load and calculate the PiN Severity for HH data set
# Date source HNAP WASH HH assessment 2022 Round 1 [January - February]
# Author: Umar Daraz; udaraz@unicef.org, umar.daraz@gmail.com MY EDIT
# Contributers ( Ramy Zaki : rh.zaki@gmail.com)

# Loading relevant Libraries ----

   options(java.parameters = "-Xmx2048m")
  library(xlsx) #detach("package:xlsx", unload=TRUE)
  library(dplyr)
  #library(pivottabler)
  library(reshape2)

# Data loading ----
{
   
  #Umar Computer
 # filepath <- "C:\\Users\\udaraz\\OneDrive - UNICEF\\WASH_WoS_Sector_HNOs\\HNO-2023\\Round-1\\DataReceived_28022022\\"
  
  #rami Computer
  #Data Source : WASH_WoS_Sector_HNOs\HNO-2023\Round-1\Exceltool
  
filepath <- "C:\\Users\\rzaki\\OneDrive - UNICEF\\Umar Daraz\\WASH_WoS_Sector_HNOs\\HNO-2023\\Round-1\\DataReceived_28022022\\"
  
  
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
  
  # Weight of HH 
  PiNSeverityData$Weight <- RData_Main$hh_weights
  
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
  
  # W4 Are you differentiating in any way between your drinking water and non-drinking water, 
  # like treating water (boiling, use filters or chlorine), using different source, 
  # storing drinking water separately etc.? (choose all apply unless you selected NO)
  PiNSeverityData$DifferentiatingAnywayDrinkingWater <- RData_Main$W4
  ## replace 1 with "No"
  PiNSeverityData$DifferentiatingAnywayDrinkingWater <- gsub("1", "No", PiNSeverityData$DifferentiatingAnywayDrinkingWater)
  ## replace 2 with "Yes, Treating drinking water"
  PiNSeverityData$DifferentiatingAnywayDrinkingWater <- gsub("2", "Yes, Treating drinking water", PiNSeverityData$DifferentiatingAnywayDrinkingWater)
  ## replace 3 with "Yes, Using different source for drinking than for other household needs"
  PiNSeverityData$DifferentiatingAnywayDrinkingWater <- gsub("3", "Yes, Using different source for drinking than for other household needs", PiNSeverityData$DifferentiatingAnywayDrinkingWater)
  
  ## replace 4 with "Yes, Treating drinking water"
  PiNSeverityData$DifferentiatingAnywayDrinkingWater <- gsub("4", "Yes, Storing drinking water differently", PiNSeverityData$DifferentiatingAnywayDrinkingWater)
  
  ## replace 66 with "Yes, Other:"
  PiNSeverityData$DifferentiatingAnywayDrinkingWater <- gsub("66", "Yes, Other:", PiNSeverityData$DifferentiatingAnywayDrinkingWater)
  
  
  
  
 # PiNSeverityData$IndicatorFRC_CHK <- RData_Main$W18
  #W.18. Can I have a glass of water to drink? (Request household for glass of water to drink, perform test and mark following. 1 Test / Household) Please mark one value
  PiNSeverityData$IndicatorFRC <- RData_Main$W18
  
  unique(PiNSeverityData$IndicatorFRC)
  
  PiNSeverityData$IndicatorFRC <- ifelse(PiNSeverityData$IndicatorFRC==1, 0, 
         ifelse(PiNSeverityData$IndicatorFRC==2, 0.1, 
                ifelse(PiNSeverityData$IndicatorFRC==3, 0.5, 
                       ifelse(PiNSeverityData$IndicatorFRC==4, 1, 
                              ifelse(PiNSeverityData$IndicatorFRC==5, 2, 
                                     ifelse(PiNSeverityData$IndicatorFRC==6, 3, 
                                        ifelse(PiNSeverityData$IndicatorFRC==7, -1,"")))))))
  
  # #---
  # 
  # PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "1", "IndicatorFRC"] <- 0
  # PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "2", "IndicatorFRC"] <- 0.1
  # PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "3", "IndicatorFRC"] <- 0.5
  # PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "4", "IndicatorFRC"] <- 1
  # PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "5", "IndicatorFRC"] <- 2
  # PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "6", "IndicatorFRC"] <- 3
  # PiNSeverityData[PiNSeverityData$IndicatorFRC_CHK == "7", "IndicatorFRC"] <- -1
  # # 
  
  
 # W.6. Did you have enough water in the last 30 days to meet your household needs? YES OR NO
  PiNSeverityData$IndicatorWaterSufficiency<- RData_Main$W6
  
  #W.6.1. If no, How did you adjust for the lack of water? (choose all apply)
  PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water<- RData_Main$W6_1
  
  
# 1 -Reduce drinking water consumption
# 2- Spend money usually spent on other things to buy water
# 3- Modify hygiene practices bath less etc.
# 4-Receive water on credit borrow water or money for water
# 5-Drink water usually used for cleaning or purposes other than drinking
# 6-Not being able to wash hands with sufficient frequency
# 66-Other
#   
  PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water <- gsub("1", "Reduce drinking water consumption", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water)
  PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water <- gsub("2", "Spend money usually spent on other things to buy water", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water)
  PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water <- gsub("3", "Modify hygiene practices bath less etc.", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water)
  PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water <- gsub("4", "Receive water on credit borrow water or money for water", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water)
  PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water <- gsub("5", "Drink water usually used for cleaning or purposes other than drinking", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water)
  PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water <- gsub("6", "Not being able to wash hands with sufficient frequency", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water)
  PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water <- gsub("66", "Other", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water)
  

  
  #W.9. Was there one / multiple hygiene item your HH needed to procure in the last 30 days but could not access? (choose all apply) It needs to be asked in the way that stress on what HHs NEEDED but couldn?t access Probably better NOT to read a full list of items to HH but rather mark down what HH couldn?t access
  PiNSeverityData$IndicatorHygiene_Access<- RData_Main$w9
  PiNSeverityData$IndicatorHygiene_Access <- gsub("19", "Hand Sanitizer", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("11", "Toothpaste child", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("10", "Toothbrush child", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("1", "Sanitary pads", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("2", "Soap bar", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("3", "Jerry can bucket", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("5", "Shampobabies", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("6", "Babies disposable diapers", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("7", "Washing powder", PiNSeverityData$IndicatorHygiene_Access)
  PiNSeverityData$IndicatorHygiene_Access <- gsub("9", "Toothpaste adults", PiNSeverityData$IndicatorHygiene_Access)
  
  
  
  

  #W.10. What is the way you disposed of garbage in the last 30 days? (choose all apply)

  PiNSeverityData$IndicatorSolidWaste_w10<- RData_Main$W10
  
 
  PiNSeverityData$IndicatorSolidWaste_w10 <- gsub("1", "Public garbage collection free", PiNSeverityData$IndicatorSolidWaste_w10)
  PiNSeverityData$IndicatorSolidWaste_w10 <- gsub("2", "Private garbage collection paid", PiNSeverityData$IndicatorSolidWaste_w10)
  PiNSeverityData$IndicatorSolidWaste_w10 <- gsub("3", "Garbage left in public areas", PiNSeverityData$IndicatorSolidWaste_w10)
  PiNSeverityData$IndicatorSolidWaste_w10 <- gsub("4", "Garbage buried or burned", PiNSeverityData$IndicatorSolidWaste_w10)
  PiNSeverityData$IndicatorSolidWaste_w10 <- gsub("5", "Garbage disposed of by household to a dumping location", PiNSeverityData$IndicatorSolidWaste_w10)
  PiNSeverityData$IndicatorSolidWaste_w10 <- gsub("66", "Other", PiNSeverityData$IndicatorSolidWaste_w10)
  
  #W.10.2. If ?Public garbage collection (free)? or ?private garbage collection (paid)?, how frequently was garbage collected in the last 30 days? (choose one)
  
  PiNSeverityData$IndicatorSolidWaste_w10_2<- RData_Main$W10_2 
  PiNSeverityData$IndicatorSolidWaste_w10_2 <- gsub("1", "Less than once a week", PiNSeverityData$IndicatorSolidWaste_w10_2)
  PiNSeverityData$IndicatorSolidWaste_w10_2 <- gsub("2", "Once a week", PiNSeverityData$IndicatorSolidWaste_w10_2)
  PiNSeverityData$IndicatorSolidWaste_w10_2 <- gsub("3", "More than once a week", PiNSeverityData$IndicatorSolidWaste_w10_2)
  
  #W11 HOUSEHOLD SIZE
  PiNSeverityData$hhsize<- RData_Main$W11_hhnumber
  
  #W.13.1. Did you face any challenges related to wastewater disposal or toilet functionality in the past month? (choose all apply)
  PiNSeverityData$Indicatorwastewater_w13_1<- RData_Main$W13_1
  
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("NoProblem", "No Problem", PiNSeverityData$Indicatorwastewater_w13_1)
  
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("1", "Could not afford desludging of septic tank", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("2", "Toilets unclean", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("3", "It is not safe No light", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("4", "Connection to sewage blocked", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("5", "Lack of ability to get to the toilet without assistance", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("6", "Not enough facilities  -too crowded", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("7", "It is not safe Bothered on the way", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("8", "Sewage overflowing in the neighborhood", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("9", "Pipes blocked (inside the house)", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("10", "Lack of privacy no separation between men and women", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("11", "It is not safe Bothered at facility", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("12", "Septic tank not emptied due to unavailability of desludging service", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("13", "No water to flush", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("14", "It is not safe No Lock bolt", PiNSeverityData$Indicatorwastewater_w13_1)
  PiNSeverityData$Indicatorwastewater_w13_1 <- gsub("66", "Other: please specify", PiNSeverityData$Indicatorwastewater_w13_1)
  
  
  #W.17. Can you please show me the place where you usually wash hands? (choose one)
  PiNSeverityData$Indicatorhandwashing_w17<- RData_Main$W17
  
  #W.17.1 Soap and water at the handwashing facility? (OBSERVATION)
  PiNSeverityData$Indicatorhandwashing_w17_1<- RData_Main$W17_1
  PiNSeverityData$Indicatorhandwashing_w17_1<- gsub("1", "Soap only", PiNSeverityData$Indicatorhandwashing_w17_1)
  PiNSeverityData$Indicatorhandwashing_w17_1<- gsub("2", "Water only", PiNSeverityData$Indicatorhandwashing_w17_1)
  PiNSeverityData$Indicatorhandwashing_w17_1<- gsub("3", "both soap and water", PiNSeverityData$Indicatorhandwashing_w17_1)
  PiNSeverityData$Indicatorhandwashing_w17_1<- gsub("4", "No water and soap", PiNSeverityData$Indicatorhandwashing_w17_1)
  PiNSeverityData$Indicatorhandwashing_w17_1<- gsub("5", "No hand washing facility", PiNSeverityData$Indicatorhandwashing_w17_1)
  
  #W.14. Has your household received any water, sanitation and hygiene
  PiNSeverityData$HHassistancereceived <- RData_Main$W14
  PiNSeverityData$HHassistancereceived<- gsub("1","Yes",PiNSeverityData$HHassistancereceived)
  PiNSeverityData$HHassistancereceived<- gsub("2","No",PiNSeverityData$HHassistancereceived)
  PiNSeverityData$HHassistancereceived<- gsub("3","Don?t Know/ Unsure",PiNSeverityData$HHassistancereceived)
  
  #CALCULATED % WATER AND DESLUDGING
  
    # W.7. What percentage of monthly income is used to buy water?  (Specify)
  PiNSeverityData$Percent_MonthlyIncome_buy_Water<- RData_Main$W7
  
  # W.7.1. How much your HH spent in the past 30 days on purchasing water? (in SYP) (Specify)
  PiNSeverityData$hh_MoneySpent_purchasing_Water<- RData_Main$W7_1
  
  
  
  }


# Severity Scoring at HH ----
  ## indicators for severity scoring -----
  {

    ### Indicator 1.1 FRC ----
    {
      #Function Logic
      # if no data reported make it blank - no severity score assigned
      # if FRc > 0 and Only source is 
      
      # Reference Code
      # Load answer options for the FRC question
      # W18_Answer_Option <- as.data.frame(RData_VariableDataOptions[RData_VariableDataOptions$list_name == "F20_List", c(2,3)])
      # W1_Answer_Option <-  as.data.frame(RData_VariableDataOptions[RData_VariableDataOptions$list_name == "q0601R", c(2,3)])
      
      # Load FRC Relevant data 
      # Ramy: this can also be implemented with elseif statement. 
   
      
      PiNSeverityData$IndicatorFRC_SS <- ifelse(PiNSeverityData$IndicatorFRC == -1, "",
                                                ifelse(PiNSeverityData$IndicatorFRC > 0 |
                                                        grepl("Bottle", PiNSeverityData$MixingWaterSourceName) |
                                                         grepl("btl", PiNSeverityData$MixingWaterSourceName) |
                                                         grepl("River", PiNSeverityData$MixingWaterSourceName) |
                                                    
                                                         grepl(c("Bottle"), PiNSeverityData$W1_MainWaterSource),1,
                                                       ifelse(PiNSeverityData$DifferentiatingAnywayDrinkingWater != "No",3,
                                                              ifelse(PiNSeverityData$W1_MainWaterSource == "Water_trucking" |
                                                                       PiNSeverityData$W1_MainWaterSource =="Open_well" |
                                                                       PiNSeverityData$W1_MainWaterSource =="River",5,4))))
        
      #=IF([@[W.18.  Can I have a glass of water to drink? (Request household for glass of water to drink, perform test and mark following]="","",
      #IF(OR([@[W.18]]>0,Y3=1,N3="bottle"),1,
      #IF([@[W.4]]<>"No",3,
      #IF(OR([@[W1. What water source did your household use the most in the last 30 days?]]="Water trucking",
      #      [@[W1. What water source did your household use the most in the last 30 days?]]="Open well",
      #      [@[W1. What water source did your household use the most in the last 30 days?]]="River/Lake"),5,4))))
    }
  

    ## Indicator 1.2 Water Sufficiency ----
    {
      
  #"" =IF([@[W.6. Did you have enough water in the last 30 days to meet your household needs?]]="Yes",1,
  #IF(SUM
      #([@[W.6.1. If no, How did you adjust for the lack of water? (choose all apply)/Reduce drinking water consumption]],
      #[@[W.6.1. If no, How did you adjust for the lack of water? (choose all apply)/Drink water usually used for cleaning or purposes other than drinking]])>0,5,
  #IF(SUM
      #([@[W.6.1. If no, How did you adjust for the lack of water? (choose all apply)/Modify hygiene practices bath less etc.]],
  #[@[W.6.1. If no, How did you adjust for the lack of water? (choose all apply)/Not being able to wash hands with sufficient frequency]])>0,4,3)))
      

  PiNSeverityData$IndicatorWaterSufficiency_SS <- ifelse(PiNSeverityData$IndicatorWaterSufficiency =="Yes",1,
                                                           ifelse(grepl("Reduce drinking water consumption", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water) |
                                                                  grepl("Drink water usually used for cleaning or purposes other than drinking", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water), 5, 
                                                                      ifelse(grepl("Modify hygiene practices bath less etc.", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water) | 
                                                                      grepl("Not being able to wash hands with sufficient frequency", PiNSeverityData$IndicatorWaterSufficiency_adjust_lack_of_water)
                                                                     , 4, 3)))
                                         
                                                             
       }
      
    ## Indicator 1.3 Hygiene Access ----
    {
      
      ##=IF([@[W.9. All items could access]]=1,1,
      #IF([@[W.9. Soap bar]]=1,5,
      #IF(SUM(
      #[@[W.9. Sanitary pads 1 pack of 9]],
      #[@[W.9. Jerry can bucket]],
      #[@[W.9. Washing powder 1kg]],
      #[@[W.9. Shampobabies]],
      #[@[W.9. Disposable diapers 1 pack of 24]],
      #[@[W.9. Toothbrush child]],
      #[@[W.9. Toothpaste child]],
      #[@[W.9. Hand Sanitizer]])>0,3
      #,2)))
      
      
      
      
      PiNSeverityData$IndicatorHygiene_Access_SS <- ifelse(PiNSeverityData$IndicatorHygiene_Access =="All",1,
                                                           ifelse(grepl("Soap bar", PiNSeverityData$IndicatorHygiene_Access), 5, 
                                                                  ifelse(  grepl("Sanitary pads", PiNSeverityData$IndicatorHygiene_Access) |
                                                                           grepl("Jerry can bucket", PiNSeverityData$IndicatorHygiene_Access) |
                                                                           grepl("Washing powder", PiNSeverityData$IndicatorHygiene_Access) |
                                                                           grepl("Shampobabies", PiNSeverityData$IndicatorHygiene_Access) |
                                                                           grepl("Babies disposable diapers", PiNSeverityData$IndicatorHygiene_Access) |
                                                                           grepl("Toothbrush child", PiNSeverityData$IndicatorHygiene_Access) |
                                                                           grepl("Toothpaste child", PiNSeverityData$IndicatorHygiene_Access)|
                                                                           grepl("Hand Sanitizer", PiNSeverityData$IndicatorHygiene_Access), 3, 3)))
      
      
    }
    
    ## Indicator 1.4 Solid Waste ----
    {
      # IF
      # (
      #   OR (
              #  [@[W.10. What is the most common way you disposed of garbage in the last 30 days? (choose all apply)/Garbage left in public areas]]=1,
              #  [@[W.10.2. If ?Public garbage collection (free)? or ?private garbage collection (paid)?, how frequently was garbage collected in the last 30 days? (choose one)]]="PAID less than once a week",
              #  [@[W.10.2. If ?Public garbage collection (free)? or ?private garbage collection (paid)?, how frequently was garbage collected in the last 30 days? (choose one)]]="FREE less than once a week"
            #),4,
      
      #  IF([@[W.10. What is the most common way you disposed of garbage in the last 30 days? (choose all apply)/Garbage buried or burned]]=1,3,
      
      #      IF(SUM(
      #         [@[W.10. What is the most common way you disposed of garbage in the last 30 days? (choose all apply)/Private garbage collection paid]],
      #         [@[W.10. What is the most common way you disposed of garbage in the last 30 days? (choose all apply)/Garbage disposed of by household to a dumping location]],
      #         [@[W.10. What is the most common way you disposed of garbage in the last 30 days? (choose all apply)/Other]])>0,2,1)
      #     )
      #)      
            
      
    PiNSeverityData$IndicatorSolidWaste_SS <- ifelse (grepl("Garbage left in public areas", PiNSeverityData$IndicatorSolidWaste_w10) |
                                                             
                                                          (grepl("Private garbage collection paid", PiNSeverityData$IndicatorSolidWaste_w10) & PiNSeverityData$IndicatorSolidWaste_w10_2=="Less than once a week"),4,
                                                            
                                                            ifelse (grepl("Garbage buried or burned", PiNSeverityData$IndicatorSolidWaste_w10_2),3,
                                                                ifelse(sum(grepl("Private garbage collection paid", PiNSeverityData$IndicatorSolidWaste_w10),
                                                                       grepl("Garbage disposed of by household to a dumping location", PiNSeverityData$IndicatorSolidWaste_w10),
                                                                       grepl("Other", PiNSeverityData$IndicatorSolidWaste_w10))>0,2,1)))
                                                       
     
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         

      
    }
    
    ## Indicator 1.5 Sanitation Problems----
    {
      
    # IF([@['#:]]>20,"",
    #     IF(
    #       OR
    #             (SUM
    #             ([@[W.13.1. Did you face any challenges related to wastewater disposal or toilet functionality in the past month? (choose all apply)/Septic tank not emptied due to unavailability of desludging service]],
    #             [@[W.13.1. Did you face any challenges related to wastewater disposal or toilet functionality in the past month? (choose all apply)/Could not afford desludging of septic tank]],
    #             [@[W.13.1. Did you face any challenges related to wastewater disposal or toilet functionality in the past month? (choose all apply)/Sewage overflowing in the neighborhood]]
      #           )>0,
      
    #             [@['#:]]>4
    # 
                  #),4,
    #         
    #         IF([@[W.13.1. Did you face any challenges related to wastewater disposal or toilet functionality in the past month? (choose all apply)/No Problem]]=1,1,3)
    #       )
    #   )
      
      
    

PiNSeverityData$IndicatorSanitation_Problems_SS <- ifelse (PiNSeverityData$hhsize>20,"", 
                                                             ifelse(
                                                                 grepl("Septic tank not emptied due to unavailability of desludging service", PiNSeverityData$Indicatorwastewater_w13_1) | 
                                                                 grepl("Could not afford desludging of septic tank", PiNSeverityData$Indicatorwastewater_w13_1) | 
                                                                 grepl("Sewage overflowing in the neighborhood", PiNSeverityData$Indicatorwastewater_w13_1)|
                                                                 PiNSeverityData$hhsize> 4, 4,
                                                              
                                                                  ifelse(grepl("No Problem", PiNSeverityData$Indicatorwastewater_w13_1)==TRUE,1, 3))
                                                             )
                                                      
      
      
      
      
    }
    

    ## Indicator 1.6 % Spend water and desludging----
    {
      #Indicators and Thresholds '!$F$20 = 15
      #Indicators and Thresholds '!$E$20 = 10
      #Indicators and Thresholds '!$D$20 = 5
    
     
#    =IF([@[CALCULATED % WATER AND DESLUDGING]]>'Indicators and Thresholds '!$F$20, 5,
#       IF([@[CALCULATED % WATER AND DESLUDGING]]>'Indicators and Thresholds '!$E$20, 4
#         ,IF([@[CALCULATED % WATER AND DESLUDGING]]>'Indicators and Thresholds '!$D$20, 3, 
#         1)))
      
      #loading the data
      

#     [CALCULATED % WATER AND DESLUDGING]  =SUM(WASH__Data[@[CALCULATED % WATER SPEND]:[CALCULATED % Desludging Spend]])
      
#     CALCULATED % WATER SPEND =IFERROR(IF(
      
#       ([@[W.7.1. How much your HH spent in the past 30 days on purchasing water? (in SYP) (Specify)]]/[@[HH_Average_Monthly_Income]])*100<=100,
# 
#       ([@[W.7.1. How much your HH spent in the past 30 days on purchasing water? (in SYP) (Specify)]]/[@[HH_Average_Monthly_Income]])*100,
      
#       [@[W.7. What percentage of monthly income is used to buy water?  (Specify)]]),
# 
#       [@[W.7. What percentage of monthly income is used to buy water?  (Specify)]])
#       

      
      # HH average monthly income is added as 50 until the clear instruction on how to calculate is available
      PiNSeverityData$HH_average_monthly_Income<- 50

      #CALCULATED % WATER SPEND
      
      PiNSeverityData$percent_hh_sepend_water <- ifelse(
                                                      PiNSeverityData$hh_MoneySpent_purchasing_Water/PiNSeverityData$HH_average_monthly_Income*100<=100,
                                                      PiNSeverityData$hh_MoneySpent_purchasing_Water/PiNSeverityData$HH_average_monthly_Income*100,
                                                      PiNSeverityData$Percent_MonthlyIncome_buy_Water
                                                       )
        

      # CALCULATED % Desludging Spend=
# 
# IFERROR(IF(EC3/3/[@[HH_Average_Monthly_Income]]*100<=100,
# EC3/3/[@[HH_Average_Monthly_Income]]*100,
# [@[W.13.2. What percentage of monthly income is used on desludging of septic tank? (asked only if ?Connection to HH septic tank? reported in W.13)]]),
# [@[W.13.2. What percentage of monthly income is used on desludging of septic tank? (asked only if ?Connection to HH septic tank? reported in W.13)]])

    # EC3= W.13.3. How much your HH spent in the past 3 Months on septic tank desludging? (in SYP)
      PiNSeverityData$HH_moneySpent_3m_desludging_septic_tank<- RData_Main$W13_3
      PiNSeverityData$Percent_MonthlyIncome_desludging_septic_tank<- RData_Main$W13_2
    
      
      
PiNSeverityData$percent_hh_sepend_Desludging <- ifelse(
      
                                                    PiNSeverityData$HH_moneySpent_3m_desludging_septic_tank/3/PiNSeverityData$HH_average_monthly_Income*100<=100,
                                                    PiNSeverityData$HH_moneySpent_3m_desludging_septic_tank/3/PiNSeverityData$HH_average_monthly_Income*100,
                                                    PiNSeverityData$Percent_MonthlyIncome_desludging_septic_tank
                                                    )
                                                    
#PiNSeverityData$percent_hh_sepend_Desludging<- ifelse(is.na(PiNSeverityData$percent_hh_sepend_Desludging), 0,PiNSeverityData$percent_hh_sepend_Desludging)      
#PiNSeverityData$percent_hh_sepend_water<- ifelse(is.na(PiNSeverityData$percent_hh_sepend_water),0, PiNSeverityData$percent_hh_sepend_water)
        
        
#     [CALCULATED % WATER AND DESLUDGING]  =SUM(WASH__Data[@[CALCULATED % WATER SPEND]:[CALCULATED % Desludging Spend]])
      #UMAR to check  
    
    PiNSeverityData$percent_hh_sepend_IN_water_and_Desludging <- PiNSeverityData$percent_hh_sepend_water+ PiNSeverityData$percent_hh_sepend_Desludging
    
    
   
    
    #Indicators and Thresholds '!$F$20 = 15
    #Indicators and Thresholds '!$E$20 = 10
    #Indicators and Thresholds '!$D$20 = 5
    
    #IF([@[CALCULATED % WATER AND DESLUDGING]]>'Indicators and Thresholds '!$F$20,5,
  # IF([@[CALCULATED % WATER AND DESLUDGING]]>'Indicators and Thresholds '!$E$20,4,
   #    IF([@[CALCULATED % WATER AND DESLUDGING]]>'Indicators and Thresholds '!$D$20,3,1)))
    
    PiNSeverityData$percent_hh_sepend_IN_water_and_Desludging_SS<- ifelse(PiNSeverityData$percent_hh_sepend_IN_water_and_Desludging>15, 5, 
                                                                          ifelse(PiNSeverityData$percent_hh_sepend_IN_water_and_Desludging>10,4,
                                                                                 ifelse(PiNSeverityData$percent_hh_sepend_IN_water_and_Desludging>5,3,1)))
    
    
    
    
    
    }
    
    ## Indicator 1.7 Household handwashing facilities----
    {
      #=IF([@[W.17. Can you please show me the place where you usually wash hands? (choose one)]]="Refuse","",
      #   IF([@[W.17.1 Soap and water at the handwashing facility? (OBSERVATION)]]="both soap and water",1,
      #       IF([@[W.17.1 Soap and water at the handwashing facility? (OBSERVATION)]]="Soap only",3,4)
      #       )
      #    )
      
      
      PiNSeverityData$Indicatorhh_handwashing_facilities_SS<- ifelse(PiNSeverityData$Indicatorhandwashing_w17 =="Refuse","",
            ifelse(grepl("both soap and water", PiNSeverityData$Indicatorhandwashing_w17_1), 1, 
                   ifelse(grepl("Soap only", PiNSeverityData$Indicatorhandwashing_w17_1), 3, 4)
                   
                   )
            )
      
    }
    
    ## Indicator 1.8 Househould receives humanitarian assistance----
    {
      
      #   2.1 % of Households receiving humanitarian assistance		20%	40%	60%	80%
      
      TEMPdataframeAssistanceReceived <- PiNSeverityData %>% 
              group_by(admin3PCode,
              HHassistancereceived)%>% 
              summarise(counthhassisted=n())
      
     
     TEMPdataframeAssistanceReceived <- dcast(TEMPdataframeAssistanceReceived, admin3PCode ~ HHassistancereceived, value.var="counthhassisted", fun.aggregate=sum)
     
     TEMPdataframeAssistanceReceived$total <- TEMPdataframeAssistanceReceived$`Don?t Know/ Unsure` +
                                                  TEMPdataframeAssistanceReceived$Yes +
                                                    TEMPdataframeAssistanceReceived$No
       
     TEMPdataframeAssistanceReceived$PercentDKN <- TEMPdataframeAssistanceReceived$`Don?t Know/ Unsure` / TEMPdataframeAssistanceReceived$total
     TEMPdataframeAssistanceReceived$PercentYes <- TEMPdataframeAssistanceReceived$Yes / TEMPdataframeAssistanceReceived$total                                           
     TEMPdataframeAssistanceReceived$PercentNo <- TEMPdataframeAssistanceReceived$No / TEMPdataframeAssistanceReceived$total
     
     TEMPdataframeAssistanceReceived$Indicatorhh_Received_SS <- ifelse(TEMPdataframeAssistanceReceived$PercentYes >0.8,5, 
                                                  ifelse(TEMPdataframeAssistanceReceived$PercentYes >0.6,4, 
                                                         ifelse(TEMPdataframeAssistanceReceived$PercentYes >0.4,3, 
                                                                ifelse(TEMPdataframeAssistanceReceived$PercentYes >0.2,2,1)))) 
 
# to hide all un-needed columns and add the HHreceivedSS indicator to the PINseverity data frame
   
   TEMPdataframeAssistanceReceived = subset(TEMPdataframeAssistanceReceived, select = c(admin3PCode,Indicatorhh_Received_SS))
   PiNSeverityData<- merge(PiNSeverityData, TEMPdataframeAssistanceReceived, by="admin3PCode")
     
    }
    
    
  }

  

## Summarizing at Sub District level -----
{
  
    ###   Indicator 1.1 - Water safety IndicatorFRC_SS----
  {


#  PiNSeverityData$IndicatorFRC_SS[PiNSeverityData$IndicatorFRC_SS=="5"]  <- "2"
  
  
  aunique<- unique(PiNSeverityData$IndicatorFRC_SS)
  #check if SS 1, 2, 3, 4, 5 are all exist
  
  Missing_ss <- c("","","","","")
 
    for(i in 1:5){
        if(length(grep(as.character(i), aunique,ignore.case = TRUE)) == 0){
          Missing_ss[i] <- c(i)
        }
      }
  

  tempSDSeverity1.1 <- PiNSeverityData %>% 
            group_by(admin3PCode,
            IndicatorFRC_SS) %>% 
  summarise(FRCSS = sum(Weight,na.rm = TRUE))
  
  tempSDSeverity1.1 <- dcast(tempSDSeverity1.1,admin3PCode ~ tempSDSeverity1.1$IndicatorFRC_SS, value.var="FRCSS", fun.aggregate=sum )
  
  for(i in 1:5)
  {
    if(Missing_ss[i] ==  1)  { tempSDSeverity1.1$"1" <- 0  }
    if(Missing_ss[i] ==  2)  { tempSDSeverity1.1$"2" <- 0  }
    if(Missing_ss[i] ==  3)  { tempSDSeverity1.1$"3" <- 0  }
    if(Missing_ss[i] ==  4)  { tempSDSeverity1.1$"4" <- 0  }
    if(Missing_ss[i] ==  5)  { tempSDSeverity1.1$"5" <- 0  }
  }
  
  tempSDSeverity1.1$total <- tempSDSeverity1.1$`1`+tempSDSeverity1.1$`3`+tempSDSeverity1.1$`4`+ tempSDSeverity1.1$`5`+ tempSDSeverity1.1$`2`
  
  tempSDSeverity1.1$SS1 <- tempSDSeverity1.1$`1`/tempSDSeverity1.1$total
  tempSDSeverity1.1$SS1 <- if(Missing_ss[1]== "1"){ 0 } else {tempSDSeverity1.1$`1`/tempSDSeverity1.1$total}
  tempSDSeverity1.1$SS2 <- if(Missing_ss[2]== "2"){ 0 } else {tempSDSeverity1.1$`2`/tempSDSeverity1.1$total}
  tempSDSeverity1.1$SS3 <- if(Missing_ss[3]== "3"){ 0 } else {tempSDSeverity1.1$`3`/tempSDSeverity1.1$total}
  tempSDSeverity1.1$SS4 <- if(Missing_ss[4]== "4"){ 0 } else {tempSDSeverity1.1$`4`/tempSDSeverity1.1$total}
  tempSDSeverity1.1$SS5 <- if(Missing_ss[5]== "5"){ 0 } else {tempSDSeverity1.1$`5`/tempSDSeverity1.1$total}
  tempSDSeverity1.1$SST <- tempSDSeverity1.1$SS1 + tempSDSeverity1.1$SS2 + tempSDSeverity1.1$SS3 + tempSDSeverity1.1$SS4 + tempSDSeverity1.1$SS5
  
  
  #=IF(F21>0.1,"",IF(GETPIVOTDATA("hh.weights",$A$2,"A.9 Sub- District","SY010000","Indicator 1.1 FRC","")>0.1,""
  # ,IF(E21>=0.25,5,
  #  IF(SUM(D21:E21)>=0.25,4,
  #  IF(SUM(C21:E21)>=0.25,3,1)))))
  
  tempSDSeverity1.1$SSSD1.1 <- ifelse(tempSDSeverity1.1$SS5> 0.25, 5,
    ifelse(tempSDSeverity1.1$SS5+tempSDSeverity1.1$SS4> 0.25, 4,
           ifelse(tempSDSeverity1.1$SS5+tempSDSeverity1.1$SS4+tempSDSeverity1.1$SS3> 0.25, 3,
                  ifelse(tempSDSeverity1.1$SS5+tempSDSeverity1.1$SS4+tempSDSeverity1.1$SS3+tempSDSeverity1.1$SS2>0.25, 2,
                         ifelse(tempSDSeverity1.1$SS5+tempSDSeverity1.1$SS4+tempSDSeverity1.1$SS3+tempSDSeverity1.1$SS2+tempSDSeverity1.1$SS1>0.25, 1,1
                         )))))
    
  tempSDSeverity1.1$PINRatio <- tempSDSeverity1.1$SS5+tempSDSeverity1.1$SS4+tempSDSeverity1.1$SS3

                }
    
  ### Indicator 1.2 - Water Sufficiency ----
  {#PiNSeverityData$IndicatorWaterSufficiency_SS
    
    aunique<- unique(PiNSeverityData$IndicatorWaterSufficiency_SS)
    #check if SS 1, 2, 3, 4, 5 are all exist
    
    Missing_ss <- c("","","","","")
    
    for(i in 1:5){
      if(length(grep(as.character(i), aunique,ignore.case = TRUE)) == 0){
        Missing_ss[i] <- c(i)
      }
    }
    
    
    
    tempSDSeverity1.2 <- PiNSeverityData %>% 
      group_by(admin3PCode,
               IndicatorWaterSufficiency_SS) %>% 
      summarise(WaterSufficiency = sum(Weight,na.rm = TRUE))
    
    
    
    tempSDSeverity1.2 <- dcast(tempSDSeverity1.2,admin3PCode ~ tempSDSeverity1.2$IndicatorWaterSufficiency_SS, value.var="WaterSufficiency", fun.aggregate=sum )
    
    
    for(i in 1:5)
      {
      if(Missing_ss[i] ==  1)  { tempSDSeverity1.2$"1" <- 0  }
      if(Missing_ss[i] ==  2)  { tempSDSeverity1.2$"2" <- 0  }
      if(Missing_ss[i] ==  3)  { tempSDSeverity1.2$"3" <- 0  }
      if(Missing_ss[i] ==  4)  { tempSDSeverity1.2$"4" <- 0  }
      if(Missing_ss[i] ==  5)  { tempSDSeverity1.2$"5" <- 0  }
    }
    
    
 
    tempSDSeverity1.2$total <- tempSDSeverity1.2$`1`+tempSDSeverity1.2$`3`+tempSDSeverity1.2$`4`+ tempSDSeverity1.2$`5`+tempSDSeverity1.2$`2`
    tempSDSeverity1.2$SS1 <- tempSDSeverity1.2$`1`/tempSDSeverity1.2$total
    tempSDSeverity1.2$SS1 <- if(Missing_ss[1]== "1"){ 0 } else {tempSDSeverity1.2$`1`/tempSDSeverity1.2$total}
    tempSDSeverity1.2$SS2 <- if(Missing_ss[2]== "2"){ 0 } else {tempSDSeverity1.2$`2`/tempSDSeverity1.2$total}
    tempSDSeverity1.2$SS3 <- if(Missing_ss[3]== "3"){ 0 } else {tempSDSeverity1.2$`3`/tempSDSeverity1.2$total}
    tempSDSeverity1.2$SS4 <- if(Missing_ss[4]== "4"){ 0 } else {tempSDSeverity1.2$`4`/tempSDSeverity1.2$total}
    tempSDSeverity1.2$SS5 <- if(Missing_ss[5]== "5"){ 0 } else {tempSDSeverity1.2$`5`/tempSDSeverity1.2$total}
    tempSDSeverity1.2$SST <- tempSDSeverity1.2$SS1 + tempSDSeverity1.2$SS2 + tempSDSeverity1.2$SS3 + tempSDSeverity1.2$SS4 + tempSDSeverity1.2$SS5
    
    
    #IF(N4>=0.25,5,IF(SUM(M4:N4)>=0.25,4,IF(SUM(L4:N4)>=0.25,3,1)))
    
    
    
    tempSDSeverity1.2$SSSD1.2 <- ifelse(tempSDSeverity1.2$SS5> 0.25, 5,
                                      ifelse(tempSDSeverity1.2$SS5+tempSDSeverity1.2$SS4> 0.25, 4,
                                             ifelse(tempSDSeverity1.2$SS5+tempSDSeverity1.2$SS4+tempSDSeverity1.2$SS3> 0.25, 3,
                                                    ifelse(tempSDSeverity1.2$SS5+tempSDSeverity1.2$SS4+tempSDSeverity1.2$SS3+tempSDSeverity1.2$SS2>0.25, 2,
                                                           ifelse(tempSDSeverity1.2$SS5+tempSDSeverity1.2$SS4+tempSDSeverity1.2$SS3+tempSDSeverity1.2$SS2+tempSDSeverity1.2$SS1>0.25, 1,1
                                                           )))))
    
    
    tempSDSeverity1.2$PINRatio <- tempSDSeverity1.2$SS5+tempSDSeverity1.2$SS4+tempSDSeverity1.2$SS3
  }
  
  
  ### Indicator 1.3 - Hygiene items ----
  {
    
    
    aunique<- unique(PiNSeverityData$IndicatorHygiene_Access_SS)
    #check if SS 1, 2, 3, 4, 5 are all exist
    Missing_ss <- c("","","","","")
    
    for(i in 1:5){
      if(length(grep(as.character(i), aunique,ignore.case = TRUE)) == 0){
        Missing_ss[i] <- c(i)
      }
    }
    
    tempSDSeverity1.3 <- PiNSeverityData %>% 
      group_by(admin3PCode,
               IndicatorHygiene_Access_SS) %>% 
      summarise(HygineAccess = sum(Weight,na.rm = TRUE))
    tempSDSeverity1.3 <- dcast(tempSDSeverity1.3,admin3PCode ~ tempSDSeverity1.3$IndicatorHygiene_Access_SS, value.var="HygineAccess", fun.aggregate=sum )
    
    for(i in 1:5)
    {
      if(Missing_ss[i] ==  1)  { tempSDSeverity1.3$"1" <- 0  }
      if(Missing_ss[i] ==  2)  { tempSDSeverity1.3$"2" <- 0  }
      if(Missing_ss[i] ==  3)  { tempSDSeverity1.3$"3" <- 0  }
      if(Missing_ss[i] ==  4)  { tempSDSeverity1.3$"4" <- 0  }
      if(Missing_ss[i] ==  5)  { tempSDSeverity1.3$"5" <- 0  }
    }
    
    tempSDSeverity1.3$total <- tempSDSeverity1.3$`1`+tempSDSeverity1.3$`3`+tempSDSeverity1.3$`4`+ tempSDSeverity1.3$`5`+tempSDSeverity1.3$`2`
    tempSDSeverity1.3$SS1 <- tempSDSeverity1.3$`1`/tempSDSeverity1.3$total
    tempSDSeverity1.3$SS1 <- if(Missing_ss[1]== "1"){ 0 } else {tempSDSeverity1.3$`1`/tempSDSeverity1.3$total}
    tempSDSeverity1.3$SS2 <- if(Missing_ss[2]== "2"){ 0 } else {tempSDSeverity1.3$`2`/tempSDSeverity1.3$total}
    tempSDSeverity1.3$SS3 <- if(Missing_ss[3]== "3"){ 0 } else {tempSDSeverity1.3$`3`/tempSDSeverity1.3$total}
    tempSDSeverity1.3$SS4 <- if(Missing_ss[4]== "4"){ 0 } else {tempSDSeverity1.3$`4`/tempSDSeverity1.3$total}
    tempSDSeverity1.3$SS5 <- if(Missing_ss[5]== "5"){ 0 } else {tempSDSeverity1.3$`5`/tempSDSeverity1.3$total}
    tempSDSeverity1.3$SST <- tempSDSeverity1.3$SS1 + tempSDSeverity1.3$SS2 + tempSDSeverity1.3$SS3 + tempSDSeverity1.3$SS4 + tempSDSeverity1.3$SS5
    
    #IF(N4>=0.25,5,IF(SUM(M4:N4)>=0.25,4,IF(SUM(L4:N4)>=0.25,3,1)))
    tempSDSeverity1.3$SSSD1.3 <- ifelse(tempSDSeverity1.3$SS5> 0.25, 5,
                                      ifelse(tempSDSeverity1.3$SS5+tempSDSeverity1.3$SS4> 0.25, 4,
                                             ifelse(tempSDSeverity1.3$SS5+tempSDSeverity1.3$SS4+tempSDSeverity1.3$SS3> 0.25, 3,
                                                    ifelse(tempSDSeverity1.3$SS5+tempSDSeverity1.3$SS4+tempSDSeverity1.3$SS3+tempSDSeverity1.3$SS2>0.25, 2,
                                                           ifelse(tempSDSeverity1.3$SS5+tempSDSeverity1.3$SS4+tempSDSeverity1.3$SS3+tempSDSeverity1.3$SS2+tempSDSeverity1.3$SS1>0.25, 1,1
                                                           )))))
    tempSDSeverity1.3$PINRatio <- tempSDSeverity1.3$SS5+tempSDSeverity1.3$SS4+tempSDSeverity1.3$SS3  
  }
  
  
  ### Indicator 1.4 Solid Waste ----
  {
    
    aunique<- unique(PiNSeverityData$IndicatorSolidWaste_SS)
    #check if SS 1, 2, 3, 4, 5 are all exist
    Missing_ss <- c("","","","","")
    
    for(i in 1:5){
      if(length(grep(as.character(i), aunique,ignore.case = TRUE)) == 0){
        Missing_ss[i] <- c(i)
      }
    }
    
    tempSDSeverity1.4 <- PiNSeverityData %>% 
      group_by(admin3PCode,
               IndicatorSolidWaste_SS) %>% 
      summarise(SolidWaste = sum(Weight,na.rm = TRUE))
    tempSDSeverity1.4 <- dcast(tempSDSeverity1.4,admin3PCode ~ tempSDSeverity1.4$IndicatorSolidWaste_SS, value.var="SolidWaste", fun.aggregate=sum )
    
    for(i in 1:5)
    {
      if(Missing_ss[i] ==  1)  { tempSDSeverity1.4$"1" <- 0  }
      if(Missing_ss[i] ==  2)  { tempSDSeverity1.4$"2" <- 0  }
      if(Missing_ss[i] ==  3)  { tempSDSeverity1.4$"3" <- 0  }
      if(Missing_ss[i] ==  4)  { tempSDSeverity1.4$"4" <- 0  }
      if(Missing_ss[i] ==  5)  { tempSDSeverity1.4$"5" <- 0  }
    }
    
    tempSDSeverity1.4$total <- tempSDSeverity1.4$`1`+tempSDSeverity1.4$`3`+tempSDSeverity1.4$`4`+ tempSDSeverity1.4$`5`+tempSDSeverity1.4$`2`
    tempSDSeverity1.4$SS1 <- tempSDSeverity1.4$`1`/tempSDSeverity1.4$total
    tempSDSeverity1.4$SS1 <- if(Missing_ss[1]== "1"){ 0 } else {tempSDSeverity1.4$`1`/tempSDSeverity1.4$total}
    tempSDSeverity1.4$SS2 <- if(Missing_ss[2]== "2"){ 0 } else {tempSDSeverity1.4$`2`/tempSDSeverity1.4$total}
    tempSDSeverity1.4$SS3 <- if(Missing_ss[3]== "3"){ 0 } else {tempSDSeverity1.4$`3`/tempSDSeverity1.4$total}
    tempSDSeverity1.4$SS4 <- if(Missing_ss[4]== "4"){ 0 } else {tempSDSeverity1.4$`4`/tempSDSeverity1.4$total}
    tempSDSeverity1.4$SS5 <- if(Missing_ss[5]== "5"){ 0 } else {tempSDSeverity1.4$`5`/tempSDSeverity1.4$total}
    tempSDSeverity1.4$SST <- tempSDSeverity1.4$SS1 + tempSDSeverity1.4$SS2 + tempSDSeverity1.4$SS3 + tempSDSeverity1.4$SS4 + tempSDSeverity1.4$SS5
    
  
    #=IF(AD4>=0.25,4,IF(SUM(AC4:AD4)>=0.25,3,IF(SUM(AB4:AD4)>=0.25,2,1)))
    
    tempSDSeverity1.4$SSSD1.4 <- ifelse(tempSDSeverity1.4$SS5> 0.25, 5,
                                        ifelse(tempSDSeverity1.4$SS5+tempSDSeverity1.4$SS4> 0.25, 4,
                                               ifelse(tempSDSeverity1.4$SS5+tempSDSeverity1.4$SS4+tempSDSeverity1.4$SS3> 0.25, 3,
                                                      ifelse(tempSDSeverity1.4$SS5+tempSDSeverity1.4$SS4+tempSDSeverity1.4$SS3+tempSDSeverity1.4$SS2>0.25, 2,
                                                             ifelse(tempSDSeverity1.4$SS5+tempSDSeverity1.4$SS4+tempSDSeverity3$SS3+tempSDSeverity1.4$SS2+tempSDSeverity1.4$SS1>0.25, 1,1
                                                             )))))
    tempSDSeverity1.4$PINRatio <- tempSDSeverity1.4$SS5+tempSDSeverity1.4$SS4+tempSDSeverity1.4$SS3 
   }
  
  
  ### Indicator 1.5 - Sanitation Problems ----
  {
    
    aunique<- unique(PiNSeverityData$IndicatorSanitation_Problems_SS)
    #check if SS 1, 2, 3, 4, 5 are all exist
    Missing_ss <- c("","","","","")
    
    for(i in 1:5){
      if(length(grep(as.character(i), aunique,ignore.case = TRUE)) == 0){
        Missing_ss[i] <- c(i)
      }
    }
    
    tempSDSeverity1.5 <- PiNSeverityData %>% 
      group_by(admin3PCode,
               IndicatorSanitation_Problems_SS) %>% 
      summarise(SanitationProblems = sum(Weight,na.rm = TRUE))
    tempSDSeverity1.5 <- dcast(tempSDSeverity1.5,admin3PCode ~ tempSDSeverity1.5$IndicatorSanitation_Problems_SS, value.var="SanitationProblems", fun.aggregate=sum )
    
    for(i in 1:5)
    {
      if(Missing_ss[i] ==  1)  { tempSDSeverity1.5$"1" <- 0  }
      if(Missing_ss[i] ==  2)  { tempSDSeverity1.5$"2" <- 0  }
      if(Missing_ss[i] ==  3)  { tempSDSeverity1.5$"3" <- 0  }
      if(Missing_ss[i] ==  4)  { tempSDSeverity1.5$"4" <- 0  }
      if(Missing_ss[i] ==  5)  { tempSDSeverity1.5$"5" <- 0  }
    }
    
    tempSDSeverity1.5$total <- tempSDSeverity1.5$`1`+tempSDSeverity1.5$`3`+tempSDSeverity1.5$`4`+ tempSDSeverity1.5$`5`+tempSDSeverity1.5$`2`
    tempSDSeverity1.5$SS1 <- tempSDSeverity1.5$`1`/tempSDSeverity1.5$total
    tempSDSeverity1.5$SS1 <- if(Missing_ss[1]== "1"){ 0 } else {tempSDSeverity1.5$`1`/tempSDSeverity1.5$total}
    tempSDSeverity1.5$SS2 <- if(Missing_ss[2]== "2"){ 0 } else {tempSDSeverity1.5$`2`/tempSDSeverity1.5$total}
    tempSDSeverity1.5$SS3 <- if(Missing_ss[3]== "3"){ 0 } else {tempSDSeverity1.5$`3`/tempSDSeverity1.5$total}
    tempSDSeverity1.5$SS4 <- if(Missing_ss[4]== "4"){ 0 } else {tempSDSeverity1.5$`4`/tempSDSeverity1.5$total}
    tempSDSeverity1.5$SS5 <- if(Missing_ss[5]== "5"){ 0 } else {tempSDSeverity1.5$`5`/tempSDSeverity1.5$total}
    tempSDSeverity1.5$SST <- tempSDSeverity1.5$SS1 + tempSDSeverity1.5$SS2 + tempSDSeverity1.5$SS3 + tempSDSeverity1.5$SS4 + tempSDSeverity1.5$SS5
    
    
    #=IF(AD4>=0.25,4,IF(SUM(AC4:AD4)>=0.25,3,IF(SUM(AB4:AD4)>=0.25,2,1)))
    
    

    
    tempSDSeverity1.5$SSSD1.5 <- ifelse(tempSDSeverity1.5$SS5> 0.25, 5,
                                        ifelse(tempSDSeverity1.5$SS5+tempSDSeverity1.5$SS4> 0.25, 4,
                                               ifelse(tempSDSeverity1.5$SS5+tempSDSeverity1.5$SS4+tempSDSeverity1.5$SS3> 0.25, 3,
                                                      ifelse(tempSDSeverity1.5$SS5+tempSDSeverity1.5$SS4+tempSDSeverity1.5$SS3+tempSDSeverity1.5$SS2>0.25, 2,
                                                             ifelse(tempSDSeverity1.5$SS5+tempSDSeverity1.5$SS4+tempSDSeverity1.5$SS3+tempSDSeverity1.5$SS2+tempSDSeverity1.5$SS1>0.25, 1,1
                                                             )))))
    
    tempSDSeverity1.5$PINRatio <- tempSDSeverity1.5$SS5+tempSDSeverity1.5$SS4+tempSDSeverity1.5$SS3
    }
  
  
  
  ### Indicator 1.6 - WASH spend ----
  {
    
    aunique<- unique(PiNSeverityData$percent_hh_sepend_IN_water_and_Desludging_SS)
    #check if SS 1, 2, 3, 4, 5 are all exist
    Missing_ss <- c("","","","","")
    
    for(i in 1:5){
      if(length(grep(as.character(i), aunique,ignore.case = TRUE)) == 0){
        Missing_ss[i] <- c(i)
      }
    }
    
    tempSDSeverity1.6 <- PiNSeverityData %>% 
      group_by(admin3PCode,
               percent_hh_sepend_IN_water_and_Desludging_SS) %>% 
      summarise(HHSpendInWaterDesludging = sum(Weight,na.rm = TRUE))
    tempSDSeverity1.6 <- dcast(tempSDSeverity1.6,admin3PCode ~ tempSDSeverity1.6$percent_hh_sepend_IN_water_and_Desludging_SS, value.var="HHSpendInWaterDesludging", fun.aggregate=sum )
    
    for(i in 1:5)
    {
      if(Missing_ss[i] ==  1)  { tempSDSeverity1.6$"1" <- 0  }
      if(Missing_ss[i] ==  2)  { tempSDSeverity1.6$"2" <- 0  }
      if(Missing_ss[i] ==  3)  { tempSDSeverity1.6$"3" <- 0  }
      if(Missing_ss[i] ==  4)  { tempSDSeverity1.6$"4" <- 0  }
      if(Missing_ss[i] ==  5)  { tempSDSeverity1.6$"5" <- 0  }
    }
    
    tempSDSeverity1.6$total <- tempSDSeverity1.6$`1`+tempSDSeverity1.6$`3`+tempSDSeverity1.6$`4`+ tempSDSeverity1.6$`5`+tempSDSeverity1.6$`2`
    tempSDSeverity1.6$SS1 <- tempSDSeverity1.6$`1`/tempSDSeverity1.6$total
    tempSDSeverity1.6$SS1 <- if(Missing_ss[1]== "1"){ 0 } else {tempSDSeverity1.6$`1`/tempSDSeverity1.6$total}
    tempSDSeverity1.6$SS2 <- if(Missing_ss[2]== "2"){ 0 } else {tempSDSeverity1.6$`2`/tempSDSeverity1.6$total}
    tempSDSeverity1.6$SS3 <- if(Missing_ss[3]== "3"){ 0 } else {tempSDSeverity1.6$`3`/tempSDSeverity1.6$total}
    tempSDSeverity1.6$SS4 <- if(Missing_ss[4]== "4"){ 0 } else {tempSDSeverity1.6$`4`/tempSDSeverity1.6$total}
    tempSDSeverity1.6$SS5 <- if(Missing_ss[5]== "5"){ 0 } else {tempSDSeverity1.6$`5`/tempSDSeverity1.6$total}
    tempSDSeverity1.6$SST <- tempSDSeverity1.6$SS1 + tempSDSeverity1.6$SS2 + tempSDSeverity1.6$SS3 + tempSDSeverity1.6$SS4 + tempSDSeverity1.6$SS5
    
    
    #=IF(AD4>=0.25,4,IF(SUM(AC4:AD4)>=0.25,3,IF(SUM(AB4:AD4)>=0.25,2,1)))
    
    
    
    
    tempSDSeverity1.6$SSSD1.6 <- ifelse(tempSDSeverity1.6$SS5> 0.25, 5,
                                        ifelse(tempSDSeverity1.6$SS5+tempSDSeverity1.6$SS4> 0.25, 4,
                                               ifelse(tempSDSeverity1.6$SS5+tempSDSeverity1.6$SS4+tempSDSeverity1.6$SS3> 0.25, 3,
                                                      ifelse(tempSDSeverity1.6$SS5+tempSDSeverity1.6$SS4+tempSDSeverity1.6$SS3+tempSDSeverity1.6$SS2>0.25, 2,
                                                             ifelse(tempSDSeverity1.6$SS5+tempSDSeverity1.6$SS4+tempSDSeverity1.6$SS3+tempSDSeverity1.6$SS2+tempSDSeverity1.6$SS1>0.25, 1,1
                                                             )))))
    
    tempSDSeverity1.6$PINRatio <- tempSDSeverity1.6$SS5+tempSDSeverity1.6$SS4+tempSDSeverity1.6$SS3  
    
  }
  
  
  
  
  ### Indicator 1.7 - Handwashing Observations ----
  {
    
    aunique<- unique(PiNSeverityData$Indicatorhh_handwashing_facilities_SS)
    #check if SS 1, 2, 3, 4, 5 are all exist
    Missing_ss <- c("","","","","")
    
    for(i in 1:5){
      if(length(grep(as.character(i), aunique,ignore.case = TRUE)) == 0){
        Missing_ss[i] <- c(i)
      }
    }
    
    tempSDSeverity1.7 <- PiNSeverityData %>% 
      group_by(admin3PCode,
               Indicatorhh_handwashing_facilities_SS) %>% 
      summarise(Handwashingobservation = sum(Weight,na.rm = TRUE))
    tempSDSeverity1.7 <- dcast(tempSDSeverity1.7,admin3PCode ~ tempSDSeverity1.7$Indicatorhh_handwashing_facilities_SS, value.var="Handwashingobservation", fun.aggregate=sum )
    
    for(i in 1:5)
    {
      if(Missing_ss[i] ==  1)  { tempSDSeverity1.7$"1" <- 0  }
      if(Missing_ss[i] ==  2)  { tempSDSeverity1.7$"2" <- 0  }
      if(Missing_ss[i] ==  3)  { tempSDSeverity1.7$"3" <- 0  }
      if(Missing_ss[i] ==  4)  { tempSDSeverity1.7$"4" <- 0  }
      if(Missing_ss[i] ==  5)  { tempSDSeverity1.7$"5" <- 0  }
    }
    
    tempSDSeverity1.7$total <- tempSDSeverity1.7$`1`+tempSDSeverity1.7$`3`+tempSDSeverity1.7$`4`+ tempSDSeverity1.7$`5`+tempSDSeverity1.7$`2`
    tempSDSeverity1.7$SS1 <- tempSDSeverity1.7$`1`/tempSDSeverity1.7$total
    tempSDSeverity1.7$SS1 <- if(Missing_ss[1]== "1"){ 0 } else {tempSDSeverity1.7$`1`/tempSDSeverity1.7$total}
    tempSDSeverity1.7$SS2 <- if(Missing_ss[2]== "2"){ 0 } else {tempSDSeverity1.7$`2`/tempSDSeverity1.7$total}
    tempSDSeverity1.7$SS3 <- if(Missing_ss[3]== "3"){ 0 } else {tempSDSeverity1.7$`3`/tempSDSeverity1.7$total}
    tempSDSeverity1.7$SS4 <- if(Missing_ss[4]== "4"){ 0 } else {tempSDSeverity1.7$`4`/tempSDSeverity1.7$total}
    tempSDSeverity1.7$SS5 <- if(Missing_ss[5]== "5"){ 0 } else {tempSDSeverity1.7$`5`/tempSDSeverity1.7$total}
    tempSDSeverity1.7$SST <- tempSDSeverity1.7$SS1 + tempSDSeverity1.7$SS2 + tempSDSeverity1.7$SS3 + tempSDSeverity1.7$SS4 + tempSDSeverity1.7$SS5
    
   
   # =IF(BB4>0.1,"",IF(BA4>=0.25,4,IF(SUM(AZ4:BA4)>=0.25,3,1)))
    
    
    #TO be verfied with UMAR
    tempSDSeverity1.7$SSSD1.7 <- ifelse(tempSDSeverity1.7$SS5> 0.25, 5,
                                        ifelse(tempSDSeverity1.7$SS5+tempSDSeverity1.7$SS4> 0.25, 4,
                                               ifelse(tempSDSeverity1.7$SS5+tempSDSeverity1.7$SS4+tempSDSeverity1.7$SS3> 0.25, 3,
                                                      ifelse(tempSDSeverity1.7$SS5+tempSDSeverity1.7$SS4+tempSDSeverity1.7$SS3+tempSDSeverity1.7$SS2>0.25, 2,
                                                             ifelse(tempSDSeverity1.7$SS5+tempSDSeverity1.7$SS4+tempSDSeverity1.7$SS3+tempSDSeverity1.7$SS2+tempSDSeverity1.7$SS1>0.25, 1,1
                                                             )))))
    
    tempSDSeverity1.7$PINRatio <- tempSDSeverity1.7$SS5+tempSDSeverity1.7$SS4+tempSDSeverity1.7$SS3
    }
  
  
  

  
  
  
}
  
## Summarizing SS at Sub District level -----
    
    {
      
      PiNSeverityDataSS <- PiNSeverityData %>% 
        group_by(admin3PCode) %>% 
        summarise(sumUUID = sum(Weight,na.rm = TRUE))
      
    tempSDSeverity1.1 = subset(tempSDSeverity1.1, select = c(admin3PCode,SSSD1.1))
    
    
    PiNSeverityDataSS<- merge(PiNSeverityDataSS, tempSDSeverity1.1, by="admin3PCode")
    
    
    tempSDSeverity1.2 = subset(tempSDSeverity1.2, select = c(admin3PCode,SSSD1.2))
    PiNSeverityDataSS<- merge(PiNSeverityDataSS, tempSDSeverity1.2, by="admin3PCode")
    
    tempSDSeverity1.3 = subset(tempSDSeverity1.3, select = c(admin3PCode,SSSD1.3))
    PiNSeverityDataSS<- merge(PiNSeverityDataSS, tempSDSeverity1.3, by="admin3PCode")
    
    tempSDSeverity1.4 = subset(tempSDSeverity1.4, select = c(admin3PCode,SSSD1.4))
    PiNSeverityDataSS<- merge(PiNSeverityDataSS, tempSDSeverity1.4, by="admin3PCode")
    
    tempSDSeverity1.5 = subset(tempSDSeverity1.5, select = c(admin3PCode,SSSD1.5))
    PiNSeverityDataSS<- merge(PiNSeverityDataSS, tempSDSeverity1.5, by="admin3PCode")
    
    tempSDSeverity1.6 = subset(tempSDSeverity1.6, select = c(admin3PCode,SSSD1.6))
    PiNSeverityDataSS<- merge(PiNSeverityDataSS, tempSDSeverity1.6, by="admin3PCode")
    
    
    tempSDSeverity1.7 = subset(tempSDSeverity1.7, select = c(admin3PCode,SSSD1.7))
    PiNSeverityDataSS<- merge(PiNSeverityDataSS, tempSDSeverity1.7, by="admin3PCode")
    
    
    }
    
    
  
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

write.csv(PiNSeverityData,"C:\\Users\\udaraz\\OneDrive - UNICEF\\WASH_WoS_Sector_HNOs\\HNO-2023\\Round-1\\DataReceived_28022022\\Working_Folder_Data\\test1.csv")



