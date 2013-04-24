                     ################################################### 
                     ###################################################
                     #                                                 #
                     #            Pathways to Desistance               #
                     #                                                 #
                     ###################################################
                     ###################################################

library(gdata)  #this library contains the keep function 
                #for cleaning up the workspace

library(reshape) #this library is used to obtain the rename function
                     


list.files()  #files in my working directory?  
                                          
################################
#Setting up baseline data files#
################################

setwd("~/Desktop/Pathways/Data")

load("~/Desktop/Pathways/Data/01.rda") # Baseline data
load("~/Desktop/Pathways/Data/02.rda") #Six month followup


baseline.variables <- c(               #Select baseline variables to keep
                    #IDENTIFIER
  "CASEID",         #Case ID 
  
                    #SOCIODEMOGRAPHICS
  "S0AGE",          #Age
  "S0ETHN_R",       #Ethnicity recoded
  "S0SGEND",        #Gender
  
                    #CLINICAL
  "S0BSIGSI",       #Global severity index
  "S0WALDEN",       #Walden self-regulation scale
  "S0BYPCLS",       #Psychopathy Checklist - Youth Version Total Score
  "S0ALCHDEP",      #Lifetime alcohol dependence (CIDI)
  "S0DRUGDEP",      #Lifetime drug dependence (CIDI)
  "S0EXPVIC",       #Exposure to Violence Inventory
  "S0HEADIN",       #Head injury - ever
  "S0IMPULS",       #Impulse control (Weinberger Adjustment Inventory)
  "S0WASISU",       #Sum of vocab and reasoning T Scores of Wechsler 
                    #Abbreviated Scale of Intelligence (WASI)
  "S0PREFONT",      #Stroop diagnosed prefrontal disorders
  "S0PREPATH",      #Stroop diagnosed prefrontal pathology
  

     
       #BASELINE OUTCOME MEASURES- PAST YEAR

     "S0AGE1STOFFENSE",
     
     "S0SROFRQ",  #Frequency of past year offending
     "S0FRQND",     #Frequency of past year offending, no drug


  
                    #NETWORK and CONTEXTUAL INFLUENCES
  "S0PRBEHV",       #Peer Delinquency  Antisocial Behavior
  "S0DEM26",        #Enrolled in school
  "S0FRDQLT",       #Friendship quality
  "S0HOOD",         #Neighborhood conditions overall
  "S0GANG18",       #Lifetime gang membership
  "S0PARMNT",       #Parental monitoring
  "S0CADPRE",       #Contact with caring adult
  "S0ROUT"         #Unsupervised Routine Activities
 )

                     
                     
                     
#The following code is preparing to grab the variables from follow up data files
followup1.variables <- c(
   "CASEID",         #Case ID
   "S1SROPRV",            #Proportion using full list of offenses, including masked variables (e.g., forced sex)
   "S1SRSEND",            #Proportion of full list of offenses, including masked variables but not drugs 
   "S1SROFRQ",             #Frequency of full list of offenses, including masked variables
   "S1SROFRQND",           #Frequency of full list of offenses, including masked variables but not drugs
   "S1SROAGG",             #Proportion of aggressive offending
   "S1AGGFRQ",             #Frequency of aggressive offending 
   "S1SROIND",              #Propotion of income offending
   "S1INCFRQ",              #Frequency of income offending
  "S1SRO_DESTPROP",      #destroyed property
  "S1SRO_SETFIRE",       #set fire
  "S1SRO_ENTBLDSTEAL",   #enter building to steal
  "S1SRO_SHOPLIFT",      #shoplift
  "S1SRO_BSTOLEN",       #bough stolen goods
  "S1SRO_ILLCREDIT",     #illegal credit cards
  "S1SRO_STOLECAR",      #car theft
  "S1SRO_SOLDMAR",       #sold marijuana
  "S1SRO_SOLDOTHDR",     #sold other drugs
  "S1SRO_CARJACK",       #carjacked someone
  "S1SRO_DROVEDRUNK",    #drove drunk
  "S1SRO_PAIDSEX",       #paid for sex
  "S1SRO_SHOT",          #shot somebody
  "S1SRO_SHOTAT",        #shot at somebody
  "S1SRO_ROBWEAPON",     #robbery with a weapon
  "S1SRO_ROBNOWEAPON",   #robbery with no weapon
  "S1SRO_BEATENUP",      #beat up somebody
  "S1SRO_INFIGHT",       #been in fight
  "S1SRO_FIGHTGANG",     #been in gang fight
  "S1SRO_CARRGUN",       #carried a gun
  "S1SRO_ENTCARSTEAL",   #broke into care to steal
  "S1SRO_JOYRIDING"     #joy riding
   ) 

                     
data.01 <- da29961.0001[baseline.variables]
data.02 <- da29961.0002[followup1.variables]   

mergedata.1 <- merge(data.01, data.02, by = "CASEID") #merge dataframes

FOLLOWUP.1 <- rename(mergedata.1, c(
     S0AGE = "age", 
     S0ETHN_R = "ethnic", 
     S0SGEND = "gender", 
     S0BSIGSI = "globalSeverity",
     S0WALDEN = "selfRegulate",
     S0BYPCLS = "psychcopathy",
     S0ALCHDEP = "alcoholDep",
     S0DRUGDEP = "drugDep",
     S0EXPVIC = "violenceExp",
     S0HEADIN = "headInjury",
     S0IMPULS = "impulseControl",
     S0WASISU = "wechsler",
     S0PRBEHV = "peerDelinquency",
     S0DEM26 = "enrolledSchool",
     S0FRDQLT = "friendshipQuality",
     S0HOOD = "neighborhoodCondition",
     S0GANG18 = "gangLifetime",
     S0PARMNT = "parentalMonitor",
     S0CADPRE = "caringAdult",
     S0ROUT = "unsupervisedActivities",
     S0AGE1STOFFENSE ="ageFirstOffense",
     S0SROFRQ = "allFrequency.PY",
     S0FRQND = "nodrugFrequency.PY", 
     
     S1SROPRV = "allProportion.F1",
     S1SROFRQ = "allFrequency.F1",
     
     S1SRSEND = "nodrugProportion.F1",
     S1SROFRQND = "nodrugFrequency.F1",
     
     S1SROAGG =  "aggressiveProportion.F1",
     S1AGGFRQ =  "aggressiveFrequency.F1",            
     
     S1SROIND =  "incomeProportion.F1",              
     S1INCFRQ =  "incomeFrequency.F1",   

     S0PREFONT = "stroopDisorder",
     S0PREPATH = "stroopPathology",
     S1SRO_DESTPROP = "propertyDestroyF1",      
  S1SRO_SETFIRE=       "setFire.F1",
  S1SRO_ENTBLDSTEAL=   "breakEnter.F1",
  S1SRO_SHOPLIFT =      "shoplift.F1",
  S1SRO_BSTOLEN =       "stolenGoods.F1",
  S1SRO_ILLCREDIT =     "fraud.F1",
  S1SRO_STOLECAR =      "stealCar.F1",
  S1SRO_SOLDMAR =       "soldPot.F1",
  S1SRO_SOLDOTHDR =     "soldDrugs.F1",
  S1SRO_CARJACK =       "carjack.F1",
  S1SRO_DROVEDRUNK =    "droveDrunk.F1",
  S1SRO_PAIDSEX =       "paidSex.F1",
  S1SRO_SHOT =          "shotSomebody.F1",
  S1SRO_SHOTAT =        "shotAtSomebody.F1",
  S1SRO_ROBWEAPON =     "robberyWeapon.F1",
  S1SRO_ROBNOWEAPON =   "robberyNoWeapon.F1",
  S1SRO_BEATENUP =      "beatSomebody.F1",
  S1SRO_INFIGHT =       "fight.F1",
  S1SRO_FIGHTGANG =     "gangFight.F1",
  S1SRO_CARRGUN =       "guncarry.F1",
  S1SRO_ENTCARSTEAL =   "brokeCar.F1",
  S1SRO_JOYRIDING =    "joyRiding.F1"
     ))
     
     
     
names(FOLLOWUP.1)     

################################################################################
#                    HOUSEKEEPING                                              #
################################################################################

#FOllOWUP.1 <- na.omit(FOLLOWUP.TEMP) #Start with clean (no missing) data set to get started                 
keep(FOLLOWUP.1, sure=TRUE)   #Get rid of everything in workspace except this object 



################################################################################
#                    SAVE DATA FILE                                            #
################################################################################
                     
#This saves the dataframe as an .RDA file                     
save(FOLLOWUP.1, file = "~/Desktop/Pathways/Data/HeadInjury/FOLLOWUP_1.rda")
                     
                     


             


