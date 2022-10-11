#***-------------------------------------------------------------------------------------------------***#
#*** AutoMated tool for Antimicrobial resistance Surveillance System version 2.0 (AMASS version 2.0) ***#
#***-------------------------------------------------------------------------------------------------***#
# Aim: to enable hospitals with microbiology data available in electronic formats
# to analyze their own data and generate AMR surveillance reports systematically.

#### Dated changes ####
# Created on 20th April 2022
#----Sub-divided from AMASSplus released on 25th March 2021
#----[All sections]Updated clean_date_variable process
#----[All sections]Edited origin date "1899-12-30"
#----[All sections]Changed file structure of results from all sections (xxx_results.csv)
#----[All sections]Added condition for checking Configuration file
#----[Section 1-6]Added antibiotics including Nalidixir_acid, Nitrofurantoin, Trimethoprim
#----[Section 1-6]Added a condition for assigning "aND" to AST result when there is not applicable
#----[Section 2]Edited exported column from 'organism' to 'organism3' of exp_rpt2_1() function
#----[Section 5]Added exported file Report5_incidence_blood_samples_community_origin_antibiotic.csv, and "Report5_incidence_blood_samples_hospital_origin_antibiotic.csv"
#----[Section 6]Changed exported mortality file for report6
#----[Section 6]Changed from "Carbapenem-S\nand 3GC-S" to "3GC-S"
#----[Section 6]Edited a condition 3GC-S of hospital-community
#----[Annex A]Changed the deduplicated way for total number of patient
#----[Data verification]Added exported files for data_verification_logfile_report
#Updated on 27th June 2022
#----[Section 6]Edited rule for 3GC-NS and 3GC-S including tested and no tested 3GC results (E. coli and K. pneumoniae)


#### PART1 : Importing functions ####
# Clean working environment
rm(list=ls()) # Clean working environment
#setwd("./")
getwd()
# Call essential functions from AMASS_function.R####
source("./Programs/AMASS_amr/AMASS_analysis_amr_function_version_2.R")
source("./Programs/AMASS_amr/AMASS_analysis_amr_exportResult_version_2.R")

#args <- commandArgs(trailingOnly = TRUE)

#### Preparations ####
# Change directory: The following code works for R in window
set_wd <- function() {
  library(rstudioapi) # make sure installed
  current_path <- getActiveDocumentContext()$path
  setwd(dirname(current_path ))
  print( getwd() )
}

# Install the packages ####
pck_loaded <- (.packages())
# The followings packages are needed to run the R codes
#----data.table => for formating the tables
#----reshape => for reshaping wide format to long and long format to wide
#----xtable => for formating the table
#----plyr => the data cleaning
#----dplyr => the data cleaning
#----stringr => for processing characters
#----gtable => for the layout of the PDF report
#----rstudioapi => to set directory
#----writexl => for exporting excel files
pck_toload <- c("data.table", "reshape", "xtable", "plyr", "dplyr", "readxl",
                "stringr", "gtable", "writexl")
# Load the packages
for(i in 1:length(pck_toload)) {
  if (!pck_toload[i] %in% pck_loaded)
    print(pck_toload[i])
  library(pck_toload[i], character.only = TRUE)
}

#### PART2 : Parsing and analyzing data ####
# Start of an error log file saved in .txt format
#----This log file is for de-bugging and contains no individual patient information
zz <- file("error_analysis_amr.txt", open="wt")
sink(zz, type="message")
# Switch off and suppress error message
#----Allow the code to skip the error and move on
options(warn=-1)
options(error=expression(NULL))


## Date format
datefmt_text <- "%d %b %Y"

# Import datasets ####
#-----------------#
#Checking configuration
config <- data.frame()
MicroData <- data.frame()
HospData <- data.frame()
if (file.exists("./Configuration/Configuration.xlsx")){
  config <- readxl::read_excel("./Configuration/Configuration.xlsx", col_types = "text")
} else{}
check_config = config[config$`AMASS basic run`=="amr_surveillance_function",2]
if (check_config == "yes") {
  # Import microbiology dataset in (allow data in either .csv or .xlsx format)
  if (file.exists("microbiology_data_reformatted.xlsx")){
      MicroData <- readxl::read_excel("microbiology_data_reformatted.xlsx", col_types = "text")
  } else{ 
      if(file.exists("microbiology_data.xlsx")) {
          MicroData <- readxl::read_excel("microbiology_data.xlsx", col_types = "text")
      } else{
          MicroData <- read.csv("microbiology_data.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
      }
  }
  MicroData <- as.data.frame(MicroData) # Define the data as data frame
  
  if (file.exists("hospital_admission_data.xlsx")){
    # Import hospital admission dataset in (allow data in either .csv or .xlsx format)
    HospData <- readxl::read_excel("hospital_admission_data.xlsx", col_types = "text")
  } else {
      HospData <- read.csv("hospital_admission_data.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  }
  
}
HospData <- as.data.frame(HospData) # Define the data as data frame
#Removing variable HospData when length(HospData) == 0
if (length(HospData)==0) {
  rm (HospData)
} else {}
  
# [Checkpoint] Availability of microbiology data set ####
checkpoint_micro_data_ava <- ifelse(exists("MicroData")==FALSE, "no", "yes")
# [Checkpoint] Availability of hospital admission data set ####
checkpoint_hosp_adm_data_ava <- ifelse(exists("HospData")==FALSE, "no", "yes")

# Data dictionary: mapping variables names and values ####
# Import data dictionary for microbiology data
microdict <- read.csv("dictionary_for_microbiology_data.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
microdict <- readxl::read_excel("dictionary_for_microbiology_data.xlsx")
#microdict <- readxl::read_excel(args[2])
for (i in 1:nrow(microdict)){
  microdict[i,2] <- ifelse(is.na(microdict[i,2])==TRUE, "empty001_micro", microdict[i,2])
}
# Import data dictionary for hospital admission data
hospdict <- read.csv("dictionary_for_hospital_admission_data.csv", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
hospdict <- readxl::read_excel("dictionary_for_hospital_admission_data.xlsx")
#hospdict <- readxl::read_excel(args[4])
for (i in 1:nrow(hospdict)){
  hospdict[i,2] <- ifelse(is.na(hospdict[i,2])==TRUE, "empty001_hosp", hospdict[i,2])
}

# Export the list of variables
ls_micro <- as.data.frame(colnames(MicroData))
names(ls_micro) <- "variables_micro"
write.csv(ls_micro, "./Variables/variables_micro.csv", row.names = FALSE)
ls_hosp <- as.data.frame(colnames(HospData))
names(ls_hosp) <- "variables_hosp"
write.csv(ls_hosp, "./Variables/variables_hosp.csv", row.names = FALSE)

# Combining the two data dictionaries
microdict1 <- as.data.frame(microdict[2:nrow(microdict),1:2])
names(microdict1) <- c("amass", "hosp")
if (exists("hospdict")=="TRUE"){
  hospdict1 <- as.data.frame(hospdict[2:nrow(hospdict),1:2])
  names(hospdict1) <- c("amass", "hosp")
  datadict <- rbind(microdict1, hospdict1)
}else{datadict <- microdict1}

datadict_plus <- datadict
datadict$amass[grep("organism_acinetobacter",datadict$amass)] <- "organism_acinetobacter_spp"
datadict$amass[grep("organism_enterococcus",datadict$amass)] <- "organism_enterococcus_spp"
datadict$amass[grep("organism_salmonella",datadict$amass)] <- "organism_salmonella_spp"


rm(microdict1)
rm(hospdict)
rm(hospdict1)


# Define the variables as text for non-misssing cells
hos_var <- as.character(datadict$hosp[!is.na(datadict$hosp)])
std_var <- as.character(datadict$amass[!is.na(datadict$amass)])
# Apply the function to rename the variables
### Microbiology database
MicroData %>% select(one_of(hos_var)) %>% names()
fun_setvarname(MicroData, hos_var, std_var, allow.absent.cols=T)
### Hospital admission database
if (exists("HospData")==TRUE){
  HospData %>% select(one_of(hos_var)) %>% names()
  fun_setvarname(HospData, hos_var, std_var, allow.absent.cols=T)
}else{}


#Assigning variable for generating logfile based on microbiology_data (raw)
log_ast_raw <- MicroData
# Assigning variable for generating logfile based on hospital_admission_data (raw)
log_raw_hosp <- HospData

# Assign variable data to microbiology data
# Code "blood" samples
MicroData$blood <- mapvalues(MicroData$specimen_type, from=hos_var, to=std_var)
MicroData$blood <- ifelse(MicroData$blood=="specimen_blood", "blood", "non-blood")
# Code organisms of interest
MicroData$organism3 <- mapvalues(MicroData$organism, from=hos_var, to=std_var)

## Hospital admission dataset
if (exists("HospData")==TRUE){
  # to change the Hosp_Num variable in numeric (remove 0s for those that starts with 0)
  HospData$hn <- format(HospData$hospital_number, scientific = FALSE) # off the scientific notation
  HospData$hn <- as.character(tolower(HospData$hn)) # change to all character and lowersample
  HospData$hn <- gsub(" ","",HospData$hn)
}else{}

## Microbiology dataset
# to change the hn variable in numeric (remove 0s for those that starts with 0)
MicroData$hn <- format(MicroData$hospital_number, scientific = FALSE) # off the scientific notation
MicroData$hn <- as.character(tolower(MicroData$hn)) # change to all character and lowersample
MicroData$hn <- gsub(" ","",MicroData$hn)

# [Checkpoint] Availability of admission date ####
checkpoint_hosp_adm_date_ava <- ifelse(checkpoint_hosp_adm_data_ava=="yes" | any(names(MicroData) == 'date_of_admission')==TRUE, "yes", "no")
list_adm_date <- if(checkpoint_hosp_adm_date_ava=="yes" & checkpoint_hosp_adm_data_ava=="yes"){
  HospData[, "date_of_admission"]
}else if(checkpoint_hosp_adm_date_ava=="yes" & any(names(MicroData) == 'date_of_admission')==TRUE){
  MicroData[, "date_of_admission"]
}else{}
checkpoint_hosp_adm_date_ava <- ifelse(all(is.na(list_adm_date))==FALSE, "yes", "no")


# Clean date variable: specimen collection date ####
#-------------------------#
# Defining the possible formats of date variable
# If the date variable is in standard date format, the following code will not have effect.
# If the date variable is in text format, the following code will format the date variable into standard date format.
######## Sample collection date
### For when the date variable is in character and actual date format i.e. dd-mm-yyyy
MicroData2 <- fun_datevariable(MicroData, MicroData$specimen_collection_date)
colnames(MicroData2)[ncol(MicroData2)] <- "spcdate2"
MicroData2$DateSpc <- multidate(MicroData2$spcdate2)
### For when the date variable is in character and numeric format of excel i.e. xxxx
if (sum(is.na(MicroData2$DateSpc)==TRUE)==nrow(MicroData2)){
  MicroData$spcdate2 <- as.numeric(MicroData$specimen_collection_date)
  MicroData2$DateSpc <- as.Date(MicroData$spcdate2, origin="1899-12-30")
}else{}
## Reference on origin date:
# http://office.microsoft.com/training/training.aspx?AssetID=RC102786151033&CTT=6&Origin=RP102786121033
# https://r.789695.n4.nabble.com/Convert-number-to-Date-td1691251.html
# Generate Year, Month, and Date: specimen collection date ####
# Check formated Sample collection date variable
summary(is.na(MicroData2$DateSpc)) #check missing values
MicroData2$YearSpc <- format(MicroData2$DateSpc, '%Y') #generate Sample collection year
MicroData2$MonthSpc <- format(MicroData2$DateSpc, '%B') #generate Sample collection month
MicroData2$MonthSpc <- factor(MicroData2$MonthSpc, levels=month.name) #define month as factor
# Clean date variable: admission date (if available) ####
if (checkpoint_hosp_adm_date_ava=="yes"){
  list_adm_date2 <- as.data.frame(list_adm_date)
  list_adm_date2 <- fun_datevariable(list_adm_date2, list_adm_date2[,1])
  colnames(list_adm_date2)[ncol(list_adm_date2)] <- "admdate2"
  list_adm_date2$adm_date <- multidate(list_adm_date2$admdate2)
  if (sum(is.na(list_adm_date2$adm_date)==TRUE)==nrow(list_adm_date2)){
    list_adm_date2 <- as.numeric(as.character(list_adm_date))
    list_adm_date2 <- as.data.frame(list_adm_date2)
    colnames(list_adm_date2)[ncol(list_adm_date2)] <- "admdate2"
    list_adm_date2$adm_date <- as.Date(list_adm_date2$admdate2, origin="1899-12-30")
  }else{}
  list_adm_date3 <- as.data.frame(unique(list_adm_date2$adm_date))
  names(list_adm_date3) <- "adm_date"
}else{}
rm(list_adm_date)
list_adm_date <- list_adm_date3
rm(list_adm_date2)
rm(list_adm_date3)


#-----------------------------------#
# [Checkpoint] Date range for data in Section 3 and onwards ####
fun_date_range_data_include_adm_data_ava <- function(){
  min_date_data_include <- max(min(MicroData2$DateSpc, na.rm=TRUE), min(list_adm_date$adm_date, na.rm=TRUE))
  max_date_data_include <- min(max(MicroData2$DateSpc, na.rm=TRUE), max(list_adm_date$adm_date, na.rm=TRUE))
  return(list(min_date_data_include, max_date_data_include))
}
fun_date_range_data_include_adm_data_notava <- function(){
  min_date_data_include <- min(MicroData2$DateSpc, na.rm=TRUE)
  max_date_data_include <- max(MicroData2$DateSpc, na.rm=TRUE)
  return(list(min_date_data_include, max_date_data_include))
}
date_range_data_include <- if(checkpoint_hosp_adm_date_ava=="yes"){
  fun_date_range_data_include_adm_data_ava()
}else{fun_date_range_data_include_adm_data_notava()}
min_date_data_include <- as.Date(data.frame(date_range_data_include[1])[1,1])
min_date_data_include_fmt <- format(min_date_data_include, datefmt_text)
max_date_data_include <- as.Date(data.frame(date_range_data_include[2])[1,1])
max_date_data_include_fmt <- format(max_date_data_include, datefmt_text)
#-----------------------------------------#
# Label and identify organism of interest ####
#-----------------------------------------#
# MicroData2_2 contains data with only the organisms of interest
MicroData2_2 <- MicroData2
nrow(MicroData2_2)
# Give numeric labels to the organisms
searchString_org <- c("organism_staphylococcus_aureus", "organism_enterococcus_spp", "organism_escherichia_coli",
                      "organism_klebsiella_pneumoniae", "organism_pseudomonas_aeruginosa", "organism_acinetobacter_spp",
                      "organism_streptococcus_pneumoniae", "organism_salmonella_spp", "organism_no_growth")
category_org <- c(1,2,3,
                  4,5,6,
                  7,8,9)
# Function to search the organisms and label them
Fun_OrgCat <- function(data, searchString_org, category_org){
  data$Org_cat <- 0
  for (i in seq_along(searchString_org)){
    list <- grep(searchString_org[i], data$organism3, ignore.case=TRUE)
    if(length(list)>0){
      data$Org_cat[list] <- category_org[i]
    }
  }
  return(data$Org_cat)
}
# Use function to label the organism
MicroData2_2$organismCat <- Fun_OrgCat(MicroData2_2, searchString_org, category_org)
# Define "blank" as "culture negative"
MicroData2_2$organismCat <- ifelse(is.na(MicroData2_2$organism)==TRUE,9,MicroData2_2$organismCat)
# Check the labels
table(MicroData2_2$organism3, MicroData2_2$organismCat, exclude=NULL)

# Define other salmonella to salmonella spp
MicroData$blood <- ifelse(MicroData$blood=="specimen_blood", "blood", "non-blood")
datadict_salmonella <- datadict[grep("organism_salmonella", datadict$amass),]
for (i in datadict_salmonella$amass) {
  MicroData2_2$organismCat <- ifelse(MicroData2_2$organism3==i,8,MicroData2_2$organismCat)
}

# New variable for AST data ####
#-------------------------------------------#
# List of antibiotics include in the report
ASTelements <- c("Amikacin","Amoxicillin","Amoxicillin_and_clavulanic_acid","Ampicillin","Ampicillin_and_sulbactam","Aztreonam","Cefazolin",
                 "Cefepime","Cefotaxime","Cefotetan","Cefoxitin","Cefpodoxime","Ceftaroline",
                 "Ceftazidime","Ceftriaxone","Cefuroxime","Chloramphenicol","Ciprofloxacin",
                 "Clarithromycin","Clindamycin","Colistin","Dalfopristin_and_quinupristin",
                 "Daptomycin","Doripenem","Doxycycline","Ertapenem","Erythromycin","Fosfomycin",
                 "Fusidic_acid","Gentamicin","Imipenem","Levofloxacin","Linezolid","Meropenem","Methicillin","Minocycline",
                 "Moxifloxacin","Nalidixic_acid","Netilmicin","Nitrofurantoin","Oxacillin","Penicillin_G","Piperacillin_and_tazobactam",
                 "Polymyxin_B","Rifampin","Streptomycin","Teicoplanin","Telavancin","Tetracycline","Ticarcillin_and_clavulanic_acid","Tigecycline","Tobramycin","Trimethoprim","Sulfamethoxazole_and_trimethoprim","Vancomycin")

RISASTelements <- paste("RIS", ASTelements, sep="")
MicroData3 <- cbind(MicroData2_2, setNames(lapply(RISASTelements, function(x) x=NA), RISASTelements))

# Define pathogen-antibiotic combinations: RIS ####
# New variables for the antibiotics of interests (all samples)
## This section is for when the data is in SIR categories and to label "no result" as aND
## Define the R I S (based on the hospital defined RIS)
resist <- datadict[which(datadict[,1]=="resistant"),2]
interm <- datadict[which(datadict[,1]=="intermediate"),2]
suscep <- datadict[which(datadict[,1]=="susceptible"),2]

data_ast_value_ris <- microdict[which(microdict[,3]=="RIS"), 1]
ASTelements2 <- as.matrix(data_ast_value_ris)
for (i in ASTelements2){
  MicroData3[[paste("RIS", i, sep="")]] <-
    replace(MicroData3[[paste("RIS", i, sep="")]],
            (length(MicroData3[[i]])!=0) & (str_detect(MicroData3[[i]], paste(resist, collapse = "|"))==TRUE), values="R")
  MicroData3[[paste("RIS", i, sep="")]] <-
    replace(MicroData3[[paste("RIS", i, sep="")]],
            (length(MicroData3[[i]])!=0) & (str_detect(MicroData3[[i]], paste(interm, collapse = "|"))==TRUE), values="I")
  MicroData3[[paste("RIS", i, sep="")]] <-
    replace(MicroData3[[paste("RIS", i, sep="")]],
            (length(MicroData3[[i]])!=0) & (str_detect(MicroData3[[i]], paste(suscep, collapse = "|"))==TRUE), values="S")
  MicroData3[[paste("RIS",i,sep="")]] <-
    replace(MicroData3[[paste("RIS", i, sep="")]],
            is.na(MicroData3[[paste("RIS", i, sep="")]])==TRUE, values="aND")
}
rm(MicroData2_2)
rm(microdict)
# Define AMR and non-AMR isolates ####
# A function to label R
# AST classification
resist2 <- "[R I]"
fun_astlabel <- function(ast, ris, resist){
  ast = NA
  ast = replace(ast, (str_detect(ris, resist)==TRUE), value=1)
  ast = replace(ast, ris=="S", value=0)
  return(ast)
}
# Create 'ASTantibiotic' binary variable to represent
# non-susceptible (Resistant and Intermediate resistant strains)
# and susceptible (Susceptible strains)
for (i in ASTelements){
  MicroData3[[paste("AST", i, sep="")]] <- fun_astlabel(ast=MicroData3[[paste("AST", i, sep="")]],
                                                        ris=MicroData3[[paste("RIS", i, sep="")]],
                                                        resist=resist2)
}
# Third generation cephalosporins
MicroData3$AST3gc = 0
MicroData3$AST3gc = replace(MicroData3$AST3gc,
                            (MicroData3$ASTCeftriaxone==1 | MicroData3$ASTCefotaxime==1 | MicroData3$ASTCeftazidime==1),
                            value=1)
MicroData3$AST3gc = replace(MicroData3$AST3gc,
                            (MicroData3$RISCeftriaxone=="aND" & MicroData3$RISCefotaxime=="aND" & MicroData3$RISCeftazidime=="aND"),
                            value=NA)
# Carbapenem
MicroData3$ASTCarbapenem = 0
MicroData3$ASTCarbapenem = replace(MicroData3$ASTCarbapenem,
                                   (MicroData3$ASTImipenem==1 | MicroData3$ASTMeropenem==1 | MicroData3$ASTErtapenem==1 | MicroData3$ASTDoripenem==1),
                                   value=1)
MicroData3$ASTCarbapenem = replace(MicroData3$ASTCarbapenem,
                                   (MicroData3$RISImipenem=="aND" & MicroData3$RISMeropenem=="aND" & MicroData3$RISErtapenem=="aND" & MicroData3$RISDoripenem=="aND"),
                                   value=NA)
# Fluoroquinolones
MicroData3$ASTFluoroquin = 0
MicroData3$ASTFluoroquin = replace(MicroData3$ASTFluoroquin,
                                   (MicroData3$ASTCiprofloxacin==1 | MicroData3$ASTLevofloxacin==1),
                                   value=1)
MicroData3$ASTFluoroquin = replace(MicroData3$ASTFluoroquin,
                                   (MicroData3$RISCiprofloxacin=="aND" & MicroData3$RISLevofloxacin=="aND"),
                                   value=NA)
# Tetracyclines
MicroData3$ASTTetra = 0
MicroData3$ASTTetra = replace(MicroData3$ASTTetra,
                              (MicroData3$ASTTigecycline==1 | MicroData3$ASTMinocycline==1),
                              value=1)
MicroData3$ASTTetra = replace(MicroData3$ASTTetra,
                              (MicroData3$RISTigecycline=="aND" & MicroData3$RISMinocycline=="aND"),
                              value=NA)
# Aminoglycosides
MicroData3$ASTaminogly = 0
MicroData3$ASTaminogly = replace(MicroData3$ASTaminogly,
                                 (MicroData3$ASTGentamicin==1 | MicroData3$ASTAmikacin==1),
                                 value=1)
MicroData3$ASTaminogly = replace(MicroData3$ASTaminogly,
                                 (MicroData3$RISGentamicin=="aND" & MicroData3$RISAmikacin=="aND"),
                                 value=NA)
# Methicillins
MicroData3$ASTmrsa = 0
MicroData3$ASTmrsa = replace(MicroData3$ASTmrsa,
                             (MicroData3$ASTMethicillin==1 | MicroData3$ASTOxacillin==1 | MicroData3$ASTCefoxitin==1),
                             value=1)
MicroData3$ASTmrsa = replace(MicroData3$ASTmrsa,
                             (MicroData3$RISMethicillin=="aND" & MicroData3$RISOxacillin=="aND" & MicroData3$RISCefoxitin=="aND"),
                             value=NA)
# Penicillins
MicroData3$ASTpengroup = 0
MicroData3$ASTpengroup = replace(MicroData3$ASTpengroup,
                                 (MicroData3$ASTPenicillin_G==1 | MicroData3$ASTOxacillin==1),
                                 value=1)
MicroData3$ASTpengroup = replace(MicroData3$ASTpengroup,
                                 (MicroData3$RISPenicillin_G=="aND" & MicroData3$RISOxacillin=="aND"),
                                 value=NA)
# Define AMR
MicroData3$AMR <- rowSums(MicroData3[, c('ASTCarbapenem', 'AST3gc',
                                         'ASTMethicillin', 'ASTVancomycin',
                                         'ASTFluoroquin', 'ASTTetra',
                                         'ASTaminogly', 'ASTColistin',
                                         'ASTCefepime', 'ASTAmpicillin',
                                         'ASTSulfamethoxazole_and_trimethoprim','ASTOxacillin',
                                         'ASTPenicillin_G')], na.rm=TRUE)
MicroData3$AMRcat <- cut(MicroData3$AMR,
                         breaks=c(0,1,3),
                         labels=c("non-AMR", "AMR"),
                         right=FALSE)


#Assigning variable for generating logfile based on microbiology_data (clean)
log_ast_clean <- MicroData3

# Label and identify Specimen type ####
#-----------------------------------------#
## Identify blood Specimens
MicroData_bsi <- MicroData3[which(MicroData3$blood=="blood"),]
# [Checkpoint] Availability of blood specimen with no microbial growth ####
# Count the number of specimens after de-duplicated by patient ID and collection date
count_data_daterange <- nrow(MicroData_bsi)
count_data_nogrowth <- length(which(MicroData_bsi$organismCat==9))
count_data_posgrowth <- length(which(MicroData_bsi$organismCat!=9))
data_org <- MicroData_bsi[which(MicroData_bsi$organismCat==1 | MicroData_bsi$organismCat==2 |
                                  MicroData_bsi$organismCat==3 | MicroData_bsi$organismCat==4 |
                                  MicroData_bsi$organismCat==5 | MicroData_bsi$organismCat==6 |
                                  MicroData_bsi$organismCat==7 | MicroData_bsi$organismCat==8), c("hn", "specimen_collection_date")]
# Count the number of specimens with organism under the survey isolated after de-duplicated by patient ID and collection date
data_ava_nogrowth <- ifelse((count_data_nogrowth/count_data_daterange)<0.7, "no", "yes")
# Remove objects that will not be used further
rm(data_org)
# Blood specimen: de-duplicate data ####
MicroData_bsi <- MicroData_bsi[which(MicroData_bsi$organismCat!=0),]
## Staphylococcus aureus
blood_dedup_sa = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 1)
## Enterococcus species
blood_dedup_es = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 2)
## Streptococcus pneumoniae
blood_dedup_sp = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 7)
## Salmonella spp.
blood_dedup_ss = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 8)
## Escherichia coli
blood_dedup_ec = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 3)
## Klebsiella pneumoniae
blood_dedup_kp = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 4)
## Pseudomonas aeruginosa
blood_dedup_pa = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 5)
## Acinetobacter spp.
blood_dedup_as = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 6)
## no growth
blood_dedup_ng = fun_data_dedup_org(sampletype = MicroData_bsi, org_cat = 9)
# Summary statistics ####
### Isolate-based report ###
# Content: data overview
# Number of patients with ≥1 sample collected during the specified time period
# Data is de-duplicated (counts of patients)
n_isolate_patients = fun_count_patient_data(MicroData_bsi)
# Number of blood specimens collected and positive culture of organisms of interest during the specified time period
# Data is NOT de-duplicated (counts of blood specimens)
MicroData_bsi_OrgInter = MicroData_bsi[which(MicroData_bsi$organismCat!=9),]
n_isolate_samples_org = nrow(MicroData_bsi_OrgInter)
# Number of patients with ≥1 sample collected and positive culture of organisms of interest during the specified time period
# Data is de-duplicated (counts of patients)
n_isolate_patients_org = fun_count_patient_data(MicroData_bsi_OrgInter)
# Remove data set that will not be used further
rm(MicroData_bsi_OrgInter)
# Report: GP_Staphylococcus aureus ####
# data: blood_dedup_sa
### Blood
# Function for summary table
# to generate summary table for isolate-based and sample-based reports
fun_data_summary_sa <- function(data){
  data_a = data
  isolate_sa_MRSA <- fun_data_asttable1(data_a$ASTmrsa)
  row.names(isolate_sa_MRSA) <- "Methicillin"
  # Vancomycin
  isolate_sa_Van <- fun_data_asttable(data_a$RISVancomycin)
  row.names(isolate_sa_Van) <- "Vancomycin"
  # Clindamycin
  isolate_sa_clind <- fun_data_asttable(data_a$RISClindamycin)
  row.names(isolate_sa_clind) <- "Clindamycin"
  # combine individual antibiotics
  ib_blood_sa_a <- list(isolate_sa_MRSA, isolate_sa_Van, isolate_sa_clind)
  isolate_blood_stapha <- fun_data_astcombine(ib_blood_sa_a)
  isolate_blood_stapha$`0` <- NULL
  # estimate key parameters
  sample_blood_stapha <- fun_table_report(isolate_blood_stapha, conflevel = 0.95, Nsamples=n_isolate_patients)
  return(sample_blood_stapha)
}
# Format the summary table for isolate-based report
isoRep_blood_stapha_tb <- fun_data_summary_sa(blood_dedup_sa)
isoRep_blood_stapha_tb[,8:10] = NULL
isoRep_blood_stapha_table <- fun_data_asttable2(isoRep_blood_stapha_tb)
rm(isoRep_blood_stapha_tb)
isoRep_blood_stapha_graph <- fun_data_summary_sa(blood_dedup_sa)
# Report: GP_Enterococcus spp. ####
# data: blood_dedup_es
### Blood
# Function for summary table
# to generate summary table for isolate-based and sample-based reports
fun_data_summary_es <- function(data){
  data_a = data
  # Vancomycin
  isolate_es_Van <- fun_data_asttable(data_a$RISVancomycin)
  row.names(isolate_es_Van) <- "Vancomycin"
  # Teicoplanin
  isolate_es_Tei <- fun_data_asttable(data_a$RISTeicoplanin)
  row.names(isolate_es_Tei) <- "Teicoplanin"
  # Ampicillin
  isolate_es_Amx <- fun_data_asttable(data_a$RISAmpicillin)
  row.names(isolate_es_Amx) <- "Ampicillin"
  # Linezolid
  isolate_es_Lin <- fun_data_asttable(data_a$RISLinezolid)
  row.names(isolate_es_Lin) <- "Linezolid"
  # Daptomycin
  isolate_es_Dap <- fun_data_asttable(data_a$RISDaptomycin)
  row.names(isolate_es_Dap) <- "Daptomycin"
  # combine individual antibiotics
  ib_blood_es_a <- list(isolate_es_Amx, isolate_es_Van, isolate_es_Tei,
                        isolate_es_Lin, isolate_es_Dap)
  isolate_blood_enterococcus <- fun_data_astcombine(ib_blood_es_a)
  # estimate key parameters
  sample_blood_enterococcus <- fun_table_report(isolate_blood_enterococcus, conflevel = 0.95, Nsamples=n_isolate_patients)
  return(sample_blood_enterococcus)
}
# Format the summary table for isolate-based report
isoRep_blood_enterococcus_tb <- fun_data_summary_es(blood_dedup_es)
isoRep_blood_enterococcus_tb[,8:10] = NULL
isoRep_blood_enterococcus_table <- fun_data_asttable2(isoRep_blood_enterococcus_tb)
rm(isoRep_blood_enterococcus_tb)
isoRep_blood_enterococcus_graph <- fun_data_summary_es(blood_dedup_es)
# Report: GP_Streptococcus pneumoniae ####
fun_data_summary_sp <- function(data){
  data_a = data
  # Penicillin
  isolate_sp_pen <- fun_data_asttable(data_a$RISPenicillin_G)
  row.names(isolate_sp_pen) <- "Penicillin G"
  # Oxacillin
  isolate_sp_oxa <- fun_data_asttable(data_a$RISOxacillin)
  row.names(isolate_sp_oxa) <- "Oxacillin"
  # Penicillin group
  # isolate_sp_pengrp <- fun_data_asttable1(data_a$ASTpengroup)
  # row.names(isolate_sp_pengrp) <- "PENICILLINS"
  # Co-trimoxazole
  isolate_sp_cot <- fun_data_asttable(data_a$RISSulfamethoxazole_and_trimethoprim)
  row.names(isolate_sp_cot) <- "Co-trimoxazole"
  # Ceftriaxone
  isolate_sp_cefx <- fun_data_asttable(data_a$RISCeftriaxone)
  row.names(isolate_sp_cefx) <- "Ceftriaxone"
  # Cefotaxime
  isolate_sp_cefo <- fun_data_asttable(data_a$RISCefotaxime)
  row.names(isolate_sp_cefo) <- "Cefotaxime"
  # Third-generation cephalosporins
  isolate_sp_3gc <- fun_data_asttable1(data_a$AST3gc)
  row.names(isolate_sp_3gc) <- "3GC"
  # Erythromycin
  isolate_sp_ery <- fun_data_asttable(data_a$RISErythromycin)
  row.names(isolate_sp_ery) <- "Erythromycin"
  # Clindamycin
  isolate_sp_clin <- fun_data_asttable(data_a$RISClindamycin)
  row.names(isolate_sp_clin) <- "Clindamycin"
  # Levofloxacin
  isolate_sp_lev <- fun_data_asttable(data_a$RISLevofloxacin)
  row.names(isolate_sp_lev) <- "Levofloxacin"
  # combine individual antibiotics
  ib_blood_sp_a <- list(isolate_sp_pen, isolate_sp_oxa, isolate_sp_cot,
                        isolate_sp_3gc, isolate_sp_cefx, isolate_sp_cefo,
                        isolate_sp_ery, isolate_sp_clin, isolate_sp_lev)
  isolate_blood_streptopneu <- fun_data_astcombine(ib_blood_sp_a)
  # estimate key parameters
  sample_blood_streptopneu <- fun_table_report(isolate_blood_streptopneu, conflevel = 0.95, Nsamples=n_isolate_patients)
  return(sample_blood_streptopneu)
}
# Format the summary table for isolate-based report
isoRep_blood_streptopneu_tb <- fun_data_summary_sp(blood_dedup_sp)
isoRep_blood_streptopneu_tb[,8:10] = NULL
isoRep_blood_streptopneu_table <- fun_data_asttable2(isoRep_blood_streptopneu_tb)
rm(isoRep_blood_streptopneu_tb)
isoRep_blood_streptopneu_graph <- fun_data_summary_sp(blood_dedup_sp)
# Report: GN_Salmonella spp ####
fun_data_summary_ss <- function(data){
  data_a = data
  # Fluoroquinolones
  isolate_ss_fluo <- fun_data_asttable1(data_a$ASTFluoroquin)
  row.names(isolate_ss_fluo) <- "FLUOROQUINOLONES"
  # Ciprofloxacin
  isolate_ss_cip <- fun_data_asttable(data_a$RISCiprofloxacin)
  row.names(isolate_ss_cip) <- "Ciprofloxacin"
  # levofloxacin
  isolate_ss_lev <- fun_data_asttable(data_a$RISLevofloxacin)
  row.names(isolate_ss_lev) <- "Levofloxacin"
  # Third-generation cephalosporins
  isolate_ss_3gc <- fun_data_asttable1(data_a$AST3gc)
  row.names(isolate_ss_3gc) <- "3GC"
  # Ceftriaxone
  isolate_ss_cro <- fun_data_asttable(data_a$RISCeftriaxone)
  row.names(isolate_ss_cro) <- "Ceftriaxone"
  # Cefotaxime
  isolate_ss_ctx <- fun_data_asttable(data_a$RISCefotaxime)
  row.names(isolate_ss_ctx) <- "Cefotaxime"
  # Ceftazidime
  isolate_ss_caz <- fun_data_asttable(data_a$RISCeftazidime)
  row.names(isolate_ss_caz) <- "Ceftazidime"
  # Carbapenems
  isolate_ss_car <- fun_data_asttable1(data_a$ASTCarbapenem)
  row.names(isolate_ss_car) <- "CARBAPENEMS"
  # Imipenem
  isolate_ss_ipm <- fun_data_asttable(data_a$RISImipenem)
  row.names(isolate_ss_ipm) <- "Imipenem"
  # Meropenem
  isolate_ss_mem <- fun_data_asttable(data_a$RISMeropenem)
  row.names(isolate_ss_mem) <- "Meropenem"
  # Ertapenem
  isolate_ss_etp <- fun_data_asttable(data_a$RISErtapenem)
  row.names(isolate_ss_etp) <- "Ertapenem"
  # Doripenem
  isolate_ss_dor <- fun_data_asttable(data_a$RISDoripenem)
  row.names(isolate_ss_dor) <- "Doripenem"
  # combine individual antibiotics
  ib_blood_ss_a <- list(isolate_ss_fluo, isolate_ss_cip, isolate_ss_lev,
                        isolate_ss_3gc, isolate_ss_cro, isolate_ss_ctx, isolate_ss_caz,
                        isolate_ss_car, isolate_ss_ipm, isolate_ss_mem, isolate_ss_etp,
                        isolate_ss_dor)
  isolate_blood_salmonella <- fun_data_astcombine(ib_blood_ss_a)
  # estimate key parameters
  sample_blood_salmonella <- fun_table_report(isolate_blood_salmonella, conflevel = 0.95, Nsamples=n_isolate_patients)
  return(sample_blood_salmonella)
}
# Format the summary table for isolate-based report
isoRep_blood_salmonella_tb <- fun_data_summary_ss(blood_dedup_ss)
isoRep_blood_salmonella_tb[,8:10] = NULL
isoRep_blood_salmonella_table <- fun_data_asttable2(isoRep_blood_salmonella_tb)
rm(isoRep_blood_salmonella_tb)
isoRep_blood_salmonella_graph <- fun_data_summary_ss(blood_dedup_ss)
# Report: GN_Escherichia coli ####
fun_data_summary_ec <- function(data){
  data_a = data
  # Ampicillin
  isolate_ec_amp <- fun_data_asttable(data_a$RISAmpicillin)
  row.names(isolate_ec_amp) <- "Ampicillin"
  # Co-trimoxazole
  isolate_ec_cot <- fun_data_asttable(data_a$RISSulfamethoxazole_and_trimethoprim)
  row.names(isolate_ec_cot) <- "Co-trimoxazole"
  # Ciprofloxacin
  isolate_ec_cip <- fun_data_asttable(data_a$RISCiprofloxacin)
  row.names(isolate_ec_cip) <- "Ciprofloxacin"
  # Levofloxacin
  isolate_ec_lvx <- fun_data_asttable(data_a$RISLevofloxacin)
  row.names(isolate_ec_lvx) <- "Levofloxacin"
  # Fluoroquinolones
  isolate_ec_fluo <- fun_data_asttable1(data_a$ASTFluoroquin)
  row.names(isolate_ec_fluo) <- "FLUOROQUINOLONES"
  # Ceftriaxone
  isolate_ec_cro <- fun_data_asttable(data_a$RISCeftriaxone)
  row.names(isolate_ec_cro) <- "Ceftriaxone"
  # Cefotaxime
  isolate_ec_ctx <- fun_data_asttable(data_a$RISCefotaxime)
  row.names(isolate_ec_ctx) <- "Cefotaxime"
  # Ceftazidime
  isolate_ec_caz <- fun_data_asttable(data_a$RISCeftazidime)
  row.names(isolate_ec_caz) <- "Ceftazidime"
  # Cefpodoxime
  isolate_sp_cefpo <- fun_data_asttable(data_a$RISCefpodoxime)
  row.names(isolate_sp_cefpo) <- "Cefpodoxime"
  # Third-generation cephalosporins
  isolate_ec_3gc <- fun_data_asttable1(data_a$AST3gc)
  row.names(isolate_ec_3gc) <- "3GC"
  # Cefepime
  isolate_ec_fep <- fun_data_asttable(data_a$RISCefepime)
  row.names(isolate_ec_fep) <- "Cefepime"
  # Imipenem
  isolate_ec_ipm <- fun_data_asttable(data_a$RISImipenem)
  row.names(isolate_ec_ipm) <- "Imipenem"
  # Meropenem
  isolate_ec_mem <- fun_data_asttable(data_a$RISMeropenem)
  row.names(isolate_ec_mem) <- "Meropenem"
  # Ertapenem
  isolate_ec_etp <- fun_data_asttable(data_a$RISErtapenem)
  row.names(isolate_ec_etp) <- "Ertapenem"
  # Doripenem
  isolate_ec_dor <- fun_data_asttable(data_a$RISDoripenem)
  row.names(isolate_ec_dor) <- "Doripenem"
  # Carbapenems
  isolate_ec_car <- fun_data_asttable1(data_a$ASTCarbapenem)
  row.names(isolate_ec_car) <- "CARBAPENEMS"
  # Colistin
  isolate_ec_col <- fun_data_asttable(data_a$RISColistin)
  row.names(isolate_ec_col) <- "Colistin"
  # Gentamicin
  isolate_ec_gen <- fun_data_asttable(data_a$RISGentamicin)
  row.names(isolate_ec_gen) <- "Gentamicin"
  # Amikacin
  isolate_ec_ami <- fun_data_asttable(data_a$RISAmikacin)
  row.names(isolate_ec_ami) <- "Amikacin"
  # combine individual antibiotics
  ib_blood_ec_a <- list(isolate_ec_gen, isolate_ec_ami,
                        isolate_ec_cot, isolate_ec_amp, isolate_ec_fluo,
                        isolate_ec_cip, isolate_ec_lvx, isolate_ec_3gc, isolate_sp_cefpo,
                        isolate_ec_cro, isolate_ec_ctx, isolate_ec_caz,
                        isolate_ec_fep, isolate_ec_car, isolate_ec_ipm,
                        isolate_ec_mem, isolate_ec_etp, isolate_ec_dor,
                        isolate_ec_col)
  isolate_blood_ecoli <- fun_data_astcombine(ib_blood_ec_a)
  # estimate key parameters
  sample_blood_ecoli <- fun_table_report(isolate_blood_ecoli, conflevel = 0.95, Nsamples=n_isolate_patients)
  return(sample_blood_ecoli)
}
# Format the summary table for isolate-based report
isoRep_blood_ecoli_tb <- fun_data_summary_ec(blood_dedup_ec)
isoRep_blood_ecoli_tb[,8:10] = NULL
isoRep_blood_ecoli_table <- fun_data_asttable2(isoRep_blood_ecoli_tb)
rm(isoRep_blood_ecoli_tb)
isoRep_blood_ecoli_graph <- fun_data_summary_ec(blood_dedup_ec)
# Report: GN_Klebsiella pneumoniae ####
fun_data_summary_kp <- function(data){
  data_a = data
  # Co-trimoxazole
  isolate_kp_cot <- fun_data_asttable(data_a$RISSulfamethoxazole_and_trimethoprim)
  row.names(isolate_kp_cot) <- "Co-trimoxazole"
  # Ciprofloxacin
  isolate_kp_cip <- fun_data_asttable(data_a$RISCiprofloxacin)
  row.names(isolate_kp_cip) <- "Ciprofloxacin"
  # Levofloxacin
  isolate_kp_lvx <- fun_data_asttable(data_a$RISLevofloxacin)
  row.names(isolate_kp_lvx) <- "Levofloxacin"
  # Fluoroquinolones
  isolate_kp_fluo <- fun_data_asttable1(data_a$ASTFluoroquin)
  row.names(isolate_kp_fluo) <- "FLUOROQUINOLONES"
  # Ceftriaxone
  isolate_kp_cro <- fun_data_asttable(data_a$RISCeftriaxone)
  row.names(isolate_kp_cro) <- "Ceftriaxone"
  # Cefotaxime
  isolate_kp_ctx <- fun_data_asttable(data_a$RISCefotaxime)
  row.names(isolate_kp_ctx) <- "Cefotaxime"
  # Ceftazidime
  isolate_kp_caz <- fun_data_asttable(data_a$RISCeftazidime)
  row.names(isolate_kp_caz) <- "Ceftazidime"
  # Cefpodoxime
  isolate_sp_cefpo <- fun_data_asttable(data_a$RISCefpodoxime)
  row.names(isolate_sp_cefpo) <- "Cefpodoxime"
  # Third-generation cephalosporins
  isolate_kp_3gc <- fun_data_asttable1(data_a$AST3gc)
  row.names(isolate_kp_3gc) <- "3GC"
  # Cefepime
  isolate_kp_fep <- fun_data_asttable(data_a$RISCefepime)
  row.names(isolate_kp_fep) <- "Cefepime"
  # Imipenem
  isolate_kp_ipm <- fun_data_asttable(data_a$RISImipenem)
  row.names(isolate_kp_ipm) <- "Imipenem"
  # Meropenem
  isolate_kp_mem <- fun_data_asttable(data_a$RISMeropenem)
  row.names(isolate_kp_mem) <- "Meropenem"
  # Ertapenem
  isolate_kp_etp <- fun_data_asttable(data_a$RISErtapenem)
  row.names(isolate_kp_etp) <- "Ertapenem"
  # Doripenem
  isolate_kp_dor <- fun_data_asttable(data_a$RISDoripenem)
  row.names(isolate_kp_dor) <- "Doripenem"
  # Carbapenems
  isolate_kp_car <- fun_data_asttable1(data_a$ASTCarbapenem)
  row.names(isolate_kp_car) <- "CARBAPENEMS"
  # Colistin
  isolate_kp_col <- fun_data_asttable(data_a$RISColistin)
  row.names(isolate_kp_col) <- "Colistin"
  # Gentamicin
  isolate_kp_gen <- fun_data_asttable(data_a$RISGentamicin)
  row.names(isolate_kp_gen) <- "Gentamicin"
  # Amikacin
  isolate_kp_ami <- fun_data_asttable(data_a$RISAmikacin)
  row.names(isolate_kp_ami) <- "Amikacin"
  # combine individual antibiotics
  ib_blood_kp_a <- list(isolate_kp_gen, isolate_kp_ami,
                        isolate_kp_cot, isolate_kp_fluo,
                        isolate_kp_cip, isolate_kp_lvx, isolate_kp_3gc, isolate_sp_cefpo,
                        isolate_kp_cro, isolate_kp_ctx, isolate_kp_caz,
                        isolate_kp_fep, isolate_kp_car, isolate_kp_ipm,
                        isolate_kp_mem, isolate_kp_etp, isolate_kp_dor,
                        isolate_kp_col)
  isolate_blood_klebp <- fun_data_astcombine(ib_blood_kp_a)
  # estimate key parameters
  sample_blood_klebp <- fun_table_report(isolate_blood_klebp, conflevel = 0.95, Nsamples=n_isolate_patients)
  return(sample_blood_klebp)
}
# Format the summary table for isolate-based report
isoRep_blood_klebp_tb <- fun_data_summary_kp(blood_dedup_kp)
isoRep_blood_klebp_tb[,8:10] = NULL
isoRep_blood_klebp_table <- fun_data_asttable2(isoRep_blood_klebp_tb)
rm(isoRep_blood_klebp_tb)
isoRep_blood_klebp_graph <- fun_data_summary_kp(blood_dedup_kp)
# Report: GN_Pseudomonas aeruginosa ####
fun_data_summary_pa <- function(data){
  data_a = data
  # # Tigecycline
  # isolate_pa_tgc <- fun_data_asttable(data_a$RISTigecycline)
  # row.names(isolate_pa_tgc) <- "Tigecycline"
  # # Minocycline
  # isolate_pa_mno <- fun_data_asttable(data_a$RISMinocycline)
  # row.names(isolate_pa_mno) <- "Minocycline"
  # # Tetracyclines
  # isolate_pa_tet <- fun_data_asttable1(data_a$ASTTetra)
  # row.names(isolate_pa_tet) <- "TETRACYCLINES"
  # Ceftazidime
  isolate_pa_caz <- fun_data_asttable(data_a$RISCeftazidime)
  row.names(isolate_pa_caz) <- "Ceftazidime"
  # Ciprofloxacin
  isolate_pa_cip <- fun_data_asttable(data_a$RISCiprofloxacin)
  row.names(isolate_pa_cip) <- "Ciprofloxacin"
  # Piperacillin/tazobactam
  isolate_pa_tzp <- fun_data_asttable(data_a$RISPiperacillin_and_tazobactam)
  row.names(isolate_pa_tzp) <- "Piperacillin/tazobactam"
  # Gentamicin
  isolate_pa_gen <- fun_data_asttable(data_a$RISGentamicin)
  row.names(isolate_pa_gen) <- "Gentamicin"
  # Amikacin
  isolate_pa_amk <- fun_data_asttable(data_a$RISAmikacin)
  row.names(isolate_pa_amk) <- "Amikacin"
  # Aminoglycosides
  isolate_pa_ami <- fun_data_asttable1(data_a$ASTaminogly)
  row.names(isolate_pa_ami) <- "AMINOGLYCOSIDES"
  # Imipenem
  isolate_pa_ipm <- fun_data_asttable(data_a$RISImipenem)
  row.names(isolate_pa_ipm) <- "Imipenem"
  # Meropenem
  isolate_pa_mem <- fun_data_asttable(data_a$RISMeropenem)
  row.names(isolate_pa_mem) <- "Meropenem"
  # Doripenem
  isolate_pa_dor <- fun_data_asttable(data_a$RISDoripenem)
  row.names(isolate_pa_dor) <- "Doripenem"
  # Carbapenems
  isolate_pa_car <- fun_data_asttable1(data_a$ASTCarbapenem)
  row.names(isolate_pa_car) <- "CARBAPENEMS"
  # Colistin
  isolate_pa_col <- fun_data_asttable(data_a$RISColistin)
  row.names(isolate_pa_col) <- "Colistin"
  # combine individual antibiotics
  ib_blood_pa_a <- list(isolate_pa_caz, isolate_pa_cip, isolate_pa_tzp,
                        isolate_pa_ami, isolate_pa_gen, isolate_pa_amk,
                        isolate_pa_car, isolate_pa_ipm, isolate_pa_mem,
                        isolate_pa_dor, isolate_pa_col)
  isolate_blood_pseudoa <- fun_data_astcombine(ib_blood_pa_a)
  # estimate key parameters
  sample_blood_pseudoa <- fun_table_report(isolate_blood_pseudoa, conflevel = 0.95, Nsamples=n_isolate_patients)
  return(sample_blood_pseudoa)
}
# Format the summary table for isolate-based report
isoRep_blood_pseudoa_tb <- fun_data_summary_pa(blood_dedup_pa)
isoRep_blood_pseudoa_tb[,8:10] = NULL
isoRep_blood_pseudoa_table <- fun_data_asttable2(isoRep_blood_pseudoa_tb)
rm(isoRep_blood_pseudoa_tb)
isoRep_blood_pseudoa_graph <- fun_data_summary_pa(blood_dedup_pa)
# Report: GN_Acinetobacter spp. ####
fun_data_summary_as <- function(data){
  data_a = data
  # Tigecycline
  isolate_as_tgc <- fun_data_asttable(data_a$RISTigecycline)
  row.names(isolate_as_tgc) <- "Tigecycline"
  # Minocycline
  isolate_as_mno <- fun_data_asttable(data_a$RISMinocycline)
  row.names(isolate_as_mno) <- "Minocycline"
  # Tetracyclines
  isolate_as_tet <- fun_data_asttable1(data_a$ASTTetra)
  row.names(isolate_as_tet) <- "TETRACYCLINES"
  # Gentamicin
  isolate_as_gen <- fun_data_asttable(data_a$RISGentamicin)
  row.names(isolate_as_gen) <- "Gentamicin"
  # Amikacin
  isolate_as_amk <- fun_data_asttable(data_a$RISAmikacin)
  row.names(isolate_as_amk) <- "Amikacin"
  # Aminoglycosides
  isolate_as_ami <- fun_data_asttable1(data_a$ASTaminogly)
  row.names(isolate_as_ami) <- "AMINOGLYCOSIDES"
  # Imipenem
  isolate_as_ipm <- fun_data_asttable(data_a$RISImipenem)
  row.names(isolate_as_ipm) <- "Imipenem"
  # Meropenem
  isolate_as_mem <- fun_data_asttable(data_a$RISMeropenem)
  row.names(isolate_as_mem) <- "Meropenem"
  # Doripenem
  isolate_as_dor <- fun_data_asttable(data_a$RISDoripenem)
  row.names(isolate_as_dor) <- "Doripenem"
  # Carbapenems
  isolate_as_car <- fun_data_asttable1(data_a$ASTCarbapenem)
  row.names(isolate_as_car) <- "CARBAPENEMS"
  # Colistin
  isolate_as_col <- fun_data_asttable(data_a$RISColistin)
  row.names(isolate_as_col) <- "Colistin"
  # combine individual antibiotics
  ib_blood_as_a <- list(isolate_as_tgc, isolate_as_mno,
                        isolate_as_ami, isolate_as_gen, isolate_as_amk,
                        isolate_as_car, isolate_as_ipm, isolate_as_mem,
                        isolate_as_dor, isolate_as_col)
  isolate_blood_acine <- fun_data_astcombine(ib_blood_as_a)
  # estimate key parameters
  sample_blood_acine <- fun_table_report(isolate_blood_acine, conflevel = 0.95, Nsamples=n_isolate_patients)
  return(sample_blood_acine)
}
# Format the summary table for isolate-based report
isoRep_blood_acine_tb <- fun_data_summary_as(blood_dedup_as)
isoRep_blood_acine_tb[,8:10] = NULL
isoRep_blood_acine_table <- fun_data_asttable2(isoRep_blood_acine_tb)
rm(isoRep_blood_acine_tb)
isoRep_blood_acine_graph <- fun_data_summary_as(blood_dedup_as)
# Text on Cover Page ####
Cover_isolate_text0 <- "Surveillance report of antimicrobial resistance"
Cover_isolate_text1 <- "Isolate-based report generated from AutoMated tool for Antimicrobial resistance Surveillance System report (AMASS)"
Cover_sample_text0 <- "Surveillance report of antimicrobial resistance"
Cover_sample_text1 <- "Sample-based report generated from AutoMated tool for Antimicrobial resistance Surveillance System report (AMASS)"

# Text on Data overview Pages (Section [1]) ####
# Page 9
### Section for microbiology data
num_micro_data <- nrow(MicroData)
min_rawmicro_spcdate <- format(min(MicroData2$DateSpc, na.rm=TRUE), datefmt_text)
max_rawmicro_spcdate <- format(max(MicroData2$DateSpc, na.rm=TRUE), datefmt_text)
### Section for hospital admission data
if(checkpoint_hosp_adm_data_ava=="yes"){
  num_hosp_data <- nrow(HospData)
  min_rawhos_admdate <- format(min(list_adm_date$adm_date, na.rm=TRUE), datefmt_text)
  max_rawhos_admdate <- format(max(list_adm_date$adm_date, na.rm=TRUE), datefmt_text)
}else{
  num_hosp_data <- "NA"
  min_rawhos_admdate <- "NA"
  max_rawhos_admdate <- "NA"
}
# For Section [1] Data overview on counts of hospital admission records by month
if (checkpoint_hosp_adm_data_ava=="yes"){
  HospData2 <- fun_datevariable(HospData, HospData$date_of_admission)
  colnames(HospData2)[ncol(HospData2)] <- "admdate2"
  HospData2$DateAdm <- multidate(HospData2$admdate2)
  ### For when the date variable is in character and numeric format of excel i.e. xxxx
  if (sum(is.na(HospData2$DateAdm)==TRUE)==nrow(HospData2)){
    HospData2$admdate2 <- as.numeric(HospData2$date_of_admission)
    HospData2$DateAdm <- as.Date(HospData2$admdate2, origin="1899-12-30")
  }else{}
  # Check formated admission date variable
  HospData2$YearAdm <- format(HospData2$DateAdm, '%Y') #generate admission year
  HospData2$MonthAdm <- format(HospData2$DateAdm, '%B') #generate admission month
  HospData2$MonthAdm <- factor(HospData2$MonthAdm, levels=month.name) #define month as factor
}else{}
# Text on Isolate-based without stratification page (Section [2]) ####
Rpt2_pg2_totalrecords = length(which(MicroData_bsi$organismCat==1)) + length(which(MicroData_bsi$organismCat==2)) +
  length(which(MicroData_bsi$organismCat==3)) + length(which(MicroData_bsi$organismCat==4)) +
  length(which(MicroData_bsi$organismCat==5)) + length(which(MicroData_bsi$organismCat==6)) +
  length(which(MicroData_bsi$organismCat==7)) + length(which(MicroData_bsi$organismCat==8))
Rpt2_pg2_totalpatients = nrow(blood_dedup_sa) + nrow(blood_dedup_es) + nrow(blood_dedup_sp) + nrow(blood_dedup_ss) +
  nrow(blood_dedup_ec) + nrow(blood_dedup_kp) + nrow(blood_dedup_pa) + nrow(blood_dedup_as)
# count of the number of organism under survey
num_blood_org <- length(which(MicroData_bsi$organismCat==1 |
                                MicroData_bsi$organismCat==2 |
                                MicroData_bsi$organismCat==3 |
                                MicroData_bsi$organismCat==4 |
                                MicroData_bsi$organismCat==5 |
                                MicroData_bsi$organismCat==6 |
                                MicroData_bsi$organismCat==7 |
                                MicroData_bsi$organismCat==8))
#================#

# Part 2: merge microbiology and hospital data files ####
# To generate the extended report by merging Hospital admission and microbiology datasets
# Merge hospital and microbiology data files ####
if(checkpoint_hosp_adm_data_ava=="yes"){
  # Delete the columns in hospital data file that has the same name as that
  # in microbiology data file
  for (i in names(MicroData_bsi)){if(i!="hn"){HospData[i] <- NULL}else{}}
  # Merge raw hospital data file to cleaned microbiology data
  raw_HospMicroData_bsi <- merge(HospData, MicroData_bsi, by="hn", all=TRUE)
}else{
  raw_HospMicroData_bsi <- MicroData_bsi
}
# Clean Date variable ####
######## Admission date
raw_HospMicroData_bsi <- fun_datevariable(raw_HospMicroData_bsi, raw_HospMicroData_bsi$date_of_admission)
colnames(raw_HospMicroData_bsi)[ncol(raw_HospMicroData_bsi)] <- "admdate2"
raw_HospMicroData_bsi$DateAdm <- multidate(raw_HospMicroData_bsi$admdate2)
### For when the date variable is in character and numeric format of excel i.e. xxxx
if (sum(is.na(raw_HospMicroData_bsi$DateAdm)==TRUE)==nrow(raw_HospMicroData_bsi)){
  raw_HospMicroData_bsi$admdate2 <- as.numeric(raw_HospMicroData_bsi$date_of_admission)
  raw_HospMicroData_bsi$DateAdm <- as.Date(raw_HospMicroData_bsi$admdate2, origin="1899-12-30")
}else{}
######## Discharge date
raw_HospMicroData_bsi <- fun_datevariable(raw_HospMicroData_bsi, raw_HospMicroData_bsi$date_of_discharge)
colnames(raw_HospMicroData_bsi)[ncol(raw_HospMicroData_bsi)] <- "disdate2"
raw_HospMicroData_bsi$DateDis <- multidate(raw_HospMicroData_bsi$disdate2)
### For when the date variable is in character and numeric format of excel i.e. xxxx
if (sum(is.na(raw_HospMicroData_bsi$DateDis)==TRUE)==nrow(raw_HospMicroData_bsi)){
  raw_HospMicroData_bsi$disdate2 <- as.numeric(raw_HospMicroData_bsi$date_of_discharge)
  raw_HospMicroData_bsi$DateDis <- as.Date(raw_HospMicroData_bsi$disdate2, origin="1899-12-30")
}else{}
######## birthday date
avai_birthday <- datadict[which(datadict[,1]=="birthday_available"),2]
if(avai_birthday=="yes"){
  raw_HospMicroData_bsi <- fun_datevariable(raw_HospMicroData_bsi, raw_HospMicroData_bsi$birthday)
  colnames(raw_HospMicroData_bsi)[ncol(raw_HospMicroData_bsi)] <- "bdate2"
  raw_HospMicroData_bsi$DateBirth <- multidate(raw_HospMicroData_bsi$bdate2)
  ### For when the date variable is in character and numeric format of excel i.e. xxxx
  if (sum(is.na(raw_HospMicroData_bsi$DateBirth)==TRUE)==nrow(raw_HospMicroData_bsi)){
    raw_HospMicroData_bsi$bdate2 <- as.numeric(raw_HospMicroData_bsi$birthday)
    raw_HospMicroData_bsi$DateBirth <- as.Date(raw_HospMicroData_bsi$bdate2, origin="1899-12-30")
  }else{}
}else{}
# Check formated admission date variable
raw_HospMicroData_bsi$YearAdm <- format(raw_HospMicroData_bsi$DateAdm, '%Y') #generate admission year
raw_HospMicroData_bsi$MonthAdm <- format(raw_HospMicroData_bsi$DateAdm, '%B') #generate admission month
raw_HospMicroData_bsi$MonthAdm <- factor(raw_HospMicroData_bsi$MonthAdm, levels=month.name) #define month as factor
# Check formated discharge date variable
raw_HospMicroData_bsi$YearDis <- format(raw_HospMicroData_bsi$DateDis, '%Y') #generate discharge year
raw_HospMicroData_bsi$MonthDis <- format(raw_HospMicroData_bsi$DateDis, '%B') #generate discharge month
raw_HospMicroData_bsi$MonthDis <- factor(raw_HospMicroData_bsi$MonthDis, levels=month.name) #define month as factor
# Label observations with Samples collected between admission and discharge date ####
if(checkpoint_hosp_adm_date_ava=="yes" & ("date_of_admission" %in% names(MicroData2))==FALSE){
  raw_HospMicroData_bsi$AdmSpc <- as.numeric(raw_HospMicroData_bsi$DateSpc>=raw_HospMicroData_bsi$DateAdm &
                                               raw_HospMicroData_bsi$DateSpc<=raw_HospMicroData_bsi$DateDis)
}else{}
# Summary statistics on merged data ####
# Count the total number of records after merged
n_total_records <- nrow(raw_HospMicroData_bsi)
# Count the total number of records in Hospital admission data that does not have a match in microbiology data
n_total_records_unmerged_hosp <- length(which(is.na(raw_HospMicroData_bsi$DateSpc)==TRUE))
# Count the total number of records in microbiology data that does not have a match in hospital admission data
n_total_records_unmerged_micro <- length(which(is.na(raw_HospMicroData_bsi$DateAdm)==TRUE))
# Count the total number of missing hospital admission data
n_total_records_missAdmDate <- length(which(is.na(HospData2$DateAdm)==TRUE))
# Count the total number of missing specimen collection data
n_total_records_missSpcDate <- length(which(is.na(MicroData2$DateSpc)==TRUE))
# Keep observations that is within the admission and discharge period ####
if(checkpoint_hosp_adm_date_ava=="yes" & ("date_of_admission" %in% names(MicroData2))==FALSE){
  HospMicroData_bsi <- raw_HospMicroData_bsi[which(raw_HospMicroData_bsi$AdmSpc==1),]
  # Select only observations with admission date within the range defined above
  #HospMicroData_bsi <- raw_HospMicroData_bsi[which((raw_HospMicroData_bsi$DateAdm>=min_date_data_include) & (raw_HospMicroData_bsi$DateAdm<=max_date_data_include)),]
  # Keep blood samples collected within the range defined above
  #HospMicroData_bsi <- raw_HospMicroData_bsi[which((raw_HospMicroData_bsi$DateSpc>=min_date_data_include) & (raw_HospMicroData_bsi$DateSpc<=max_date_data_include)),]
}else{
  HospMicroData_bsi <- raw_HospMicroData_bsi
}
# Count the total number of merged record with specimen date within a set of admission and discharge dates
n_total_records_merged <- nrow(HospMicroData_bsi)

# Clean key variables: Gender ####
# Code gender
HospMicroData_bsi$gender2 <- mapvalues(HospMicroData_bsi$gender, from=hos_var, to=std_var)
HospMicroData_bsi$gender_cat <- factor(HospMicroData_bsi$gender2, levels=c("male", "female")) # define gender as a categorical variable
# Clean key variables: Age ####
# check whether variable on 'birthday' is available
avai_birthday <- datadict[which(datadict[,1]=="birthday_available"),2]
avai_age_year <- datadict[which(datadict[,1]=="age_year_available"),2]
avai_age_group <- datadict[which(datadict[,1]=="age_group_available"),2]
# calculate age in year from birthday
if(avai_birthday=="yes"){
  HospMicroData_bsi$YearAge1 <- as.numeric(HospMicroData_bsi$DateAdm)-as.numeric(HospMicroData_bsi$DateBirth)
  HospMicroData_bsi$YearAge2 <- floor(HospMicroData_bsi$YearAge1/365.25)
  HospMicroData_bsi$YearAge1 <- NULL
}else{}
# extraction age in year if variable on age year is available
HospMicroData_bsi$age_year <- as.numeric(HospMicroData_bsi$age_year)
HospMicroData_bsi$YearAge <- NA
HospMicroData_bsi$YearAge <- replace(HospMicroData_bsi$YearAge,
                                     avai_birthday=="no" & avai_age_year=="yes",
                                     values = HospMicroData_bsi$age_year)
HospMicroData_bsi$YearAge <- replace(HospMicroData_bsi$YearAge,
                                     avai_birthday=="yes",
                                     values = HospMicroData_bsi$YearAge2)
# categorise age into 10 groups
HospMicroData_bsi$YearAge_cat <- cut(HospMicroData_bsi$YearAge,
                                     breaks=c(0,1,5,15,25,35,45,55,65,81,200),
                                     labels=c("a<1" ,"b1-4", "c5-14", "d15-24", "e25-34", "f35-44", "g45-54", "h55-64", "i65-80", "j>80"),
                                     right=FALSE)
HospMicroData_bsi$YearAge_cat <- as.character(HospMicroData_bsi$YearAge_cat)
HospMicroData_bsi$YearAge_cat <- replace(HospMicroData_bsi$YearAge_cat,
                                         is.na(HospMicroData_bsi$YearAge_cat),
                                         "Unknown")
# label age group
HospMicroData_bsi$YearAge_label <- "Unknown"
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="a<1",
                                           values = "less than 1 year")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="b1-4",
                                           values = "1 to 4 years")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="c5-14",
                                           values = "5 to 14 years")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="d15-24",
                                           values = "15 to 24 years")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="e25-34",
                                           values = "25 to 34 years")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="f35-44",
                                           values = "35 to 44 years")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="g45-54",
                                           values = "45 to 54 years")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="h55-64",
                                           values = "55 to 64 years")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="i65-80",
                                           values = "65 to 80 years")
HospMicroData_bsi$YearAge_label <- replace(HospMicroData_bsi$YearAge_label,
                                           HospMicroData_bsi$YearAge_cat=="j>80",
                                           values = "over 80 years")
# use age group label if age_group is available
HospMicroData_bsi$YearAge_label2 <- "Unknown"
HospMicroData_bsi$YearAge_label2 <- replace(HospMicroData_bsi$YearAge_label2,
                                            avai_birthday=="no" & avai_age_year=="no" & avai_age_group=="yes",
                                            values = HospMicroData_bsi$age_group)
HospMicroData_bsi$YearAge_label2 <- replace(HospMicroData_bsi$YearAge_label2,
                                            (avai_birthday=="yes" | avai_age_year=="yes"),
                                            values = HospMicroData_bsi$YearAge_label)
# Clean key variables: Discharge outcome ####
# Code discharge status
HospMicroData_bsi$disoutcome2 <- mapvalues(HospMicroData_bsi$discharge_status, from=hos_var, to=std_var)
HospMicroData_bsi$disoutcome2 <- replace(HospMicroData_bsi$disoutcome2, HospMicroData_bsi$disoutcome2!="died", value="alive")
HospMicroData_bsi$disoutcome2_cat <- factor(HospMicroData_bsi$disoutcome2, levels=c("alive", "died")) # define discharge outcome as a categorical variable
HospMicroData_bsi$disoutcome_cat <- as.factor(HospMicroData_bsi$disoutcome2)
HospData2$disoutcome2 <- mapvalues(HospData2$discharge_status, from=hos_var, to=std_var)
HospData2$disoutcome2 <- replace(HospData2$disoutcome2, HospData2$disoutcome2!="died", value="alive")
HospData2$disoutcome2_cat <- factor(HospData2$disoutcome2, levels=c("alive", "died")) # define discharge outcome as a categorical variable
HospData2$disoutcome_cat <- as.factor(HospData2$disoutcome2)

# Assigning variable for generating logfile based on hospital_admission_data (clean)
log_clean_hosp <- HospData2

# Clean key variables: length of hospital stay ####
# Days of being in hospital since admission to Sample collection
if(checkpoint_hosp_adm_date_ava=="yes"){
  HospMicroData_bsi$losSpc <- HospMicroData_bsi$DateSpc-HospMicroData_bsi$DateAdm
}else{HospMicroData_bsi$losSpc <- NA}
# Clean key variables: Infection origin ####
# check whether variable on infection origin is available
avai_Infect_Ori <- datadict[which(datadict[,1]=="infection_origin_available"),2]
HospMicroData_bsi$InfOri_hosp1 <- mapvalues(HospMicroData_bsi$infection_origin, from=hos_var, to=std_var)
# clean infection origin variable
# community-origin = 0; hospital-origin = 1
HospMicroData_bsi$InfOri_hosp <- NA
HospMicroData_bsi$InfOri_hosp <- replace(HospMicroData_bsi$InfOri_hosp,
                                         HospMicroData_bsi$InfOri_hosp1=="community_origin",
                                         values = 0)
HospMicroData_bsi$InfOri_hosp <- replace(HospMicroData_bsi$InfOri_hosp,
                                         HospMicroData_bsi$InfOri_hosp1=="hospital_origin",
                                         values = 1)
# Label Community-origin vs Hospital-origin ####
HospMicroData_bsi$InfOri_cal <- NA
HospMicroData_bsi$InfOri_cal <- ifelse(HospMicroData_bsi$losSpc>=2, 1, 0)
HospMicroData_bsi <- fun_data_infori(HospMicroData_bsi)

# De-duplicate the merged dataset stratified by organism ####
## Staphylococcus aureus
merged_blood_dedup_sa = fun_data_dedup_org(sampletype = HospMicroData_bsi, org_cat = 1)
## Enterococcus species
merged_blood_dedup_es = fun_data_dedup_org(sampletype = HospMicroData_bsi, org_cat = 2)
## Streptococcus pneumoniae
merged_blood_dedup_sp = fun_data_dedup_org(sampletype = HospMicroData_bsi, org_cat = 7)
## Salmonella spp.
merged_blood_dedup_ss = fun_data_dedup_org(sampletype = HospMicroData_bsi, org_cat = 8)
## Escherichia coli
merged_blood_dedup_ec = fun_data_dedup_org(sampletype = HospMicroData_bsi, org_cat = 3)
## Klebsiella pneumoniae
merged_blood_dedup_kp = fun_data_dedup_org(sampletype = HospMicroData_bsi, org_cat = 4)
## Pseudomonas aeruginosa
merged_blood_dedup_pa = fun_data_dedup_org(sampletype = HospMicroData_bsi, org_cat = 5)
## Acinetobacter spp.
merged_blood_dedup_as = fun_data_dedup_org(sampletype = HospMicroData_bsi, org_cat = 6)
# Report: GP_Staphylococcus aureus ####
# data: merged_blood_dedup_sa
# Format the summary table for community-origin infections
co_extRep_blood_stapha <- merged_blood_dedup_sa[which(merged_blood_dedup_sa$InfOri==0),]
co_extRep_blood_stapha_tb <- fun_data_summary_sa(co_extRep_blood_stapha)
co_extRep_blood_stapha_tb[,8:10] = NULL
co_extRep_blood_stapha_table <- fun_data_asttable2(co_extRep_blood_stapha_tb)
co_extRep_blood_stapha_graph <- fun_data_summary_sa(co_extRep_blood_stapha)
rm(co_extRep_blood_stapha_tb)
# Format the summary table for hospital-origin infections
ho_extRep_blood_stapha <- merged_blood_dedup_sa[which(merged_blood_dedup_sa$InfOri==1),]
ho_extRep_blood_stapha_tb <- fun_data_summary_sa(ho_extRep_blood_stapha)
ho_extRep_blood_stapha_tb[,8:10] = NULL
ho_extRep_blood_stapha_table <- fun_data_asttable2(ho_extRep_blood_stapha_tb)
ho_extRep_blood_stapha_graph <- fun_data_summary_sa(ho_extRep_blood_stapha)
rm(ho_extRep_blood_stapha_tb)

# Format the mortality summary table for community-origin infections
co_extRep_blood_mssa <- fun_data_deathtable(co_extRep_blood_stapha, co_extRep_blood_stapha$ASTmrsa, 0, "MSSA")
co_extRep_blood_mrsa <- fun_data_deathtable(co_extRep_blood_stapha, co_extRep_blood_stapha$ASTmrsa, 1, "MRSA")
# combine individual antibiotics
co_blood_sa_a <- list(co_extRep_blood_mrsa, co_extRep_blood_mssa)
co_extRep_blood_stapha_tb <- fun_data_astcombine2(co_blood_sa_a)
co_extRep_blood_sa_deathgraph <- co_extRep_blood_stapha_tb[movecolumn(names(co_extRep_blood_stapha_tb), "Antibiotic first")]
co_extRep_blood_sa_deathtable <- co_extRep_blood_sa_deathgraph[,c(1,3,6)]
names(co_extRep_blood_sa_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(co_extRep_blood_stapha_tb)
rm(co_blood_sa_a)
# Format the mortality summary table for hospital-origin infections
ho_extRep_blood_mssa <- fun_data_deathtable(ho_extRep_blood_stapha, ho_extRep_blood_stapha$ASTmrsa, 0, "MSSA")
ho_extRep_blood_mrsa <- fun_data_deathtable(ho_extRep_blood_stapha, ho_extRep_blood_stapha$ASTmrsa, 1, "MRSA")
# combine individual antibiotics
ho_blood_sa_a <- list(ho_extRep_blood_mrsa, ho_extRep_blood_mssa)
ho_extRep_blood_stapha_tb <- fun_data_astcombine2(ho_blood_sa_a)
ho_extRep_blood_sa_deathgraph <- ho_extRep_blood_stapha_tb[movecolumn(names(ho_extRep_blood_stapha_tb), "Antibiotic first")]
ho_extRep_blood_sa_deathtable <- ho_extRep_blood_sa_deathgraph[,c(1,3,6)]
names(ho_extRep_blood_sa_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(ho_extRep_blood_stapha_tb)
rm(ho_blood_sa_a)
# Report: GP_Enterococcus spp. ####
# data: merged_blood_dedup_es
# Format the summary table for community-origin infections
co_extRep_blood_enterococcus <- merged_blood_dedup_es[which(merged_blood_dedup_es$InfOri==0),]
co_extRep_blood_enterococcus_tb <- fun_data_summary_es(co_extRep_blood_enterococcus)
co_extRep_blood_enterococcus_tb[,8:10] = NULL
co_extRep_blood_enterococcus_table <- fun_data_asttable2(co_extRep_blood_enterococcus_tb)
co_extRep_blood_enterococcus_graph <- fun_data_summary_es(co_extRep_blood_enterococcus)
rm(co_extRep_blood_enterococcus_tb)
# Format the summary table for hospital-origin infections
ho_extRep_blood_enterococcus <- merged_blood_dedup_es[which(merged_blood_dedup_es$InfOri==1),]
ho_extRep_blood_enterococcus_tb <- fun_data_summary_es(ho_extRep_blood_enterococcus)
ho_extRep_blood_enterococcus_tb[,8:10] = NULL
ho_extRep_blood_enterococcus_table <- fun_data_asttable2(ho_extRep_blood_enterococcus_tb)
ho_extRep_blood_enterococcus_graph <- fun_data_summary_es(ho_extRep_blood_enterococcus)
rm(ho_extRep_blood_enterococcus_tb)

# Format the mortality summary table for community-origin infections
co_extRep_blood_vansuscep <- fun_data_deathtable(co_extRep_blood_enterococcus, co_extRep_blood_enterococcus$ASTVancomycin, 0, "Vancomycin-S")
co_extRep_blood_vanresist <- fun_data_deathtable(co_extRep_blood_enterococcus, co_extRep_blood_enterococcus$ASTVancomycin, 1, "Vancomycin-NS")
# combine individual antibiotics
co_blood_es_a <- list(co_extRep_blood_vanresist, co_extRep_blood_vansuscep)
co_extRep_blood_enterococcus_tb <- fun_data_astcombine2(co_blood_es_a)
co_extRep_blood_es_deathgraph <- co_extRep_blood_enterococcus_tb[movecolumn(names(co_extRep_blood_enterococcus_tb), "Antibiotic first")]
co_extRep_blood_es_deathtable <- co_extRep_blood_es_deathgraph[,c(1,3,6)]
names(co_extRep_blood_es_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(co_blood_es_a)
# Format the mortality summary table for hospital-origin infections
ho_extRep_blood_vansuscep <- fun_data_deathtable(ho_extRep_blood_enterococcus, ho_extRep_blood_enterococcus$ASTVancomycin, 0, "Vancomycin-S")
ho_extRep_blood_vanresist <- fun_data_deathtable(ho_extRep_blood_enterococcus, ho_extRep_blood_enterococcus$ASTVancomycin, 1, "Vancomycin-NS")
# combine individual antibiotics
ho_blood_es_a <- list(ho_extRep_blood_vanresist, ho_extRep_blood_vansuscep)
ho_extRep_blood_enterococcus_tb <- fun_data_astcombine2(ho_blood_es_a)
ho_extRep_blood_es_deathgraph <- ho_extRep_blood_enterococcus_tb[movecolumn(names(ho_extRep_blood_enterococcus_tb), "Antibiotic first")]
ho_extRep_blood_es_deathtable <- ho_extRep_blood_es_deathgraph[,c(1,3,6)]
names(ho_extRep_blood_es_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(ho_extRep_blood_enterococcus_tb)
rm(ho_blood_es_a)
# Report: GP_Streptococcus pneumoniae ####
# data: merged_blood_dedup_sp
# Format the summary table for community-origin infections
co_extRep_blood_streptopneu <- merged_blood_dedup_sp[which(merged_blood_dedup_sp$InfOri==0),]
co_extRep_blood_streptopneu_tb <- fun_data_summary_sp(co_extRep_blood_streptopneu)
co_extRep_blood_streptopneu_tb[,8:10] = NULL
co_extRep_blood_streptopneu_table <- fun_data_asttable2(co_extRep_blood_streptopneu_tb)
co_extRep_blood_streptopneu_graph <- fun_data_summary_sp(co_extRep_blood_streptopneu)
rm(co_extRep_blood_streptopneu_tb)
# Format the summary table for hospital-origin infections
ho_extRep_blood_streptopneu <- merged_blood_dedup_sp[which(merged_blood_dedup_sp$InfOri==1),]
ho_extRep_blood_streptopneu_tb <- fun_data_summary_sp(ho_extRep_blood_streptopneu)
ho_extRep_blood_streptopneu_tb[,8:10] = NULL
ho_extRep_blood_streptopneu_table <- fun_data_asttable2(ho_extRep_blood_streptopneu_tb)
ho_extRep_blood_streptopneu_graph <- fun_data_summary_sp(ho_extRep_blood_streptopneu)
rm(ho_extRep_blood_streptopneu_tb)

# Format the mortality summary table for community-origin infections
co_extRep_blood_pens <- fun_data_deathtable(co_extRep_blood_streptopneu, co_extRep_blood_streptopneu$ASTPenicillin_G, 0, "Penicillin-S")
co_extRep_blood_penr <- fun_data_deathtable(co_extRep_blood_streptopneu, co_extRep_blood_streptopneu$ASTPenicillin_G, 1, "Penicillin-NS")
# combine individual antibiotics
co_blood_sp_a <- list(co_extRep_blood_penr, co_extRep_blood_pens)
co_extRep_blood_streptopneu_tb <- fun_data_astcombine2(co_blood_sp_a)
co_extRep_blood_sp_deathgraph <- co_extRep_blood_streptopneu_tb[movecolumn(names(co_extRep_blood_streptopneu_tb), "Antibiotic first")]
co_extRep_blood_sp_deathtable <- co_extRep_blood_sp_deathgraph[,c(1,3,6)]
names(co_extRep_blood_sp_deathtable) <- c("Mortality", "Mortality (n)", "95% CI")
rm(co_extRep_blood_streptopneu_tb)
rm(co_blood_sp_a)
# Format the mortality summary table for hospital-origin infections
ho_extRep_blood_pens <- fun_data_deathtable(ho_extRep_blood_streptopneu, ho_extRep_blood_streptopneu$ASTPenicillin_G, 0, "Penicillin-S")
ho_extRep_blood_penr <- fun_data_deathtable(ho_extRep_blood_streptopneu, ho_extRep_blood_streptopneu$ASTPenicillin_G, 1, "Penicillin-NS")
# combine individual antibiotics
ho_blood_sp_a <- list(ho_extRep_blood_penr, ho_extRep_blood_pens)
ho_extRep_blood_streptopneu_tb <- fun_data_astcombine2(ho_blood_sp_a)
ho_extRep_blood_sp_deathgraph <- ho_extRep_blood_streptopneu_tb[movecolumn(names(ho_extRep_blood_streptopneu_tb), "Antibiotic first")]
ho_extRep_blood_sp_deathtable <- ho_extRep_blood_sp_deathgraph[,c(1,3,6)]
names(ho_extRep_blood_sp_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(ho_extRep_blood_streptopneu_tb)
rm(ho_blood_sp_a)
# Report: GN_Salmonella spp ####
# data: merged_blood_dedup_ss
# Format the summary table for community-origin infections
co_extRep_blood_salmonella <- merged_blood_dedup_ss[which(merged_blood_dedup_ss$InfOri==0),]
co_extRep_blood_salmonella_tb <- fun_data_summary_ss(co_extRep_blood_salmonella)
co_extRep_blood_salmonella_tb[,8:10] = NULL
co_extRep_blood_salmonella_table <- fun_data_asttable2(co_extRep_blood_salmonella_tb)
co_extRep_blood_salmonella_graph <- fun_data_summary_ss(co_extRep_blood_salmonella)
rm(co_extRep_blood_salmonella_tb)
# Format the summary table for hospital-origin infections
ho_extRep_blood_salmonella <- merged_blood_dedup_ss[which(merged_blood_dedup_ss$InfOri==1),]
ho_extRep_blood_salmonella_tb <- fun_data_summary_ss(ho_extRep_blood_salmonella)
ho_extRep_blood_salmonella_tb[,8:10] = NULL
ho_extRep_blood_salmonella_table <- fun_data_asttable2(ho_extRep_blood_salmonella_tb)
ho_extRep_blood_salmonella_graph <- fun_data_summary_ss(ho_extRep_blood_salmonella)
rm(ho_extRep_blood_salmonella_tb)
# Format the mortality summary table for community-origin infections
co_extRep_blood_fluos <- fun_data_deathtable(co_extRep_blood_salmonella, co_extRep_blood_salmonella$ASTFluoroquin, 0, "Fluoroquinolone-S")
co_extRep_blood_fluor <- fun_data_deathtable(co_extRep_blood_salmonella, co_extRep_blood_salmonella$ASTFluoroquin, 1, "Fluoroquinolone-NS")
# combine individual antibiotics
co_blood_ss_a <- list(co_extRep_blood_fluor, co_extRep_blood_fluos)
co_extRep_blood_salmonella_tb <- fun_data_astcombine2(co_blood_ss_a)
co_extRep_blood_ss_deathgraph <- co_extRep_blood_salmonella_tb[movecolumn(names(co_extRep_blood_salmonella_tb), "Antibiotic first")]
co_extRep_blood_ss_deathtable <- co_extRep_blood_ss_deathgraph[,c(1,3,6)]
names(co_extRep_blood_ss_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(co_extRep_blood_salmonella_tb)
rm(co_blood_ss_a)
# Format the mortality summary table for hospital-origin infections
ho_extRep_blood_fluos <- fun_data_deathtable(ho_extRep_blood_salmonella, ho_extRep_blood_salmonella$ASTFluoroquin, 0, "Fluoroquinolone-S")
ho_extRep_blood_fluor <- fun_data_deathtable(ho_extRep_blood_salmonella, ho_extRep_blood_salmonella$ASTFluoroquin, 1, "Fluoroquinolone-NS")
# combine individual antibiotics
ho_blood_ss_a <- list(ho_extRep_blood_fluor, ho_extRep_blood_fluos)
ho_extRep_blood_salmonella_tb <- fun_data_astcombine2(ho_blood_ss_a)
ho_extRep_blood_ss_deathgraph <- ho_extRep_blood_salmonella_tb[movecolumn(names(ho_extRep_blood_salmonella_tb), "Antibiotic first")]
ho_extRep_blood_ss_deathtable <- ho_extRep_blood_ss_deathgraph[,c(1,3,6)]
names(ho_extRep_blood_ss_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(ho_extRep_blood_salmonella_tb)
rm(ho_blood_ss_a)
# Report: GN_Escherichia coli ####
# data: merged_blood_dedup_ec
# Format the summary table for community-origin infections
co_extRep_blood_ecoli <- merged_blood_dedup_ec[which(merged_blood_dedup_ec$InfOri==0),]
co_extRep_blood_ecoli_tb <- fun_data_summary_ec(co_extRep_blood_ecoli)
co_extRep_blood_ecoli_tb[,8:10] = NULL
co_extRep_blood_ecoli_table <- fun_data_asttable2(co_extRep_blood_ecoli_tb)
co_extRep_blood_ecoli_graph <- fun_data_summary_ec(co_extRep_blood_ecoli)
rm(co_extRep_blood_ecoli_tb)
# Format the summary table for hospital-origin infections
ho_extRep_blood_ecoli <- merged_blood_dedup_ec[which(merged_blood_dedup_ec$InfOri==1),]
ho_extRep_blood_ecoli_tb <- fun_data_summary_ec(ho_extRep_blood_ecoli)
ho_extRep_blood_ecoli_tb[,8:10] = NULL
ho_extRep_blood_ecoli_table <- fun_data_asttable2(ho_extRep_blood_ecoli_tb)
ho_extRep_blood_ecoli_graph <- fun_data_summary_ec(ho_extRep_blood_ecoli)
rm(ho_extRep_blood_ecoli_tb)
# Format the mortality summary table for community-origin infections
co_extRep_blood_ecoli$AST3gcsCarbs <- 0
co_extRep_blood_ecoli$AST3gcsCarbs <- replace(co_extRep_blood_ecoli$AST3gcsCarbs,
                                              co_extRep_blood_ecoli$AST3gc==0 & (co_extRep_blood_ecoli$ASTCarbapenem!=1 |
                                                                                is.na(co_extRep_blood_ecoli$ASTCarbapenem)==1),
                                              values = 1)
co_extRep_blood_ecoli$AST3gcsCarbs <- replace(co_extRep_blood_ecoli$AST3gcsCarbs,
                                              co_extRep_blood_ecoli$AST3gc==1 & (co_extRep_blood_ecoli$ASTCarbapenem!=1|
                                                                                is.na(co_extRep_blood_ecoli$ASTCarbapenem)==1),
                                              values = 2)
co_extRep_blood_3gcs <- fun_data_deathtable(co_extRep_blood_ecoli, co_extRep_blood_ecoli$AST3gcsCarbs, 1, "3GC-S")
co_extRep_blood_3gcr <- fun_data_deathtable(co_extRep_blood_ecoli, co_extRep_blood_ecoli$AST3gcsCarbs, 2, "3GC-NS")
co_extRep_blood_carr <- fun_data_deathtable(co_extRep_blood_ecoli, co_extRep_blood_ecoli$ASTCarbapenem, 1, "Carbapenem-NS")
# combine individual antibiotics
co_blood_ec_a <- list(co_extRep_blood_carr, co_extRep_blood_3gcr, co_extRep_blood_3gcs)
co_extRep_blood_ecoli_tb <- fun_data_astcombine2(co_blood_ec_a)
co_extRep_blood_ec_deathgraph <- co_extRep_blood_ecoli_tb[movecolumn(names(co_extRep_blood_ecoli_tb), "Antibiotic first")]
co_extRep_blood_ec_deathtable <- co_extRep_blood_ec_deathgraph[,c(1,3,6)]
names(co_extRep_blood_ec_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(co_extRep_blood_ecoli_tb)
rm(co_blood_ec_a)
# Format the mortality summary table for hospital-origin infections
ho_extRep_blood_ecoli$AST3gcsCarbs <- 0
ho_extRep_blood_ecoli$AST3gcsCarbs <- replace(ho_extRep_blood_ecoli$AST3gcsCarbs,
                                              ho_extRep_blood_ecoli$AST3gc==0 & (ho_extRep_blood_ecoli$ASTCarbapenem!=1|
                                                                                is.na(ho_extRep_blood_ecoli$ASTCarbapenem)==1),
                                              values = 1)
ho_extRep_blood_ecoli$AST3gcsCarbs <- replace(ho_extRep_blood_ecoli$AST3gcsCarbs,
                                              ho_extRep_blood_ecoli$AST3gc==1 & (ho_extRep_blood_ecoli$ASTCarbapenem!=1|
                                                                                is.na(ho_extRep_blood_ecoli$ASTCarbapenem)==1),
                                              values = 2)

ho_extRep_blood_3gcs <- fun_data_deathtable(ho_extRep_blood_ecoli, ho_extRep_blood_ecoli$AST3gcsCarbs, 1, "3GC-S")
ho_extRep_blood_3gcr <- fun_data_deathtable(ho_extRep_blood_ecoli, ho_extRep_blood_ecoli$AST3gcsCarbs, 2, "3GC-NS")
ho_extRep_blood_carr <- fun_data_deathtable(ho_extRep_blood_ecoli, ho_extRep_blood_ecoli$ASTCarbapenem, 1, "Carbapenem-NS")
# combine individual antibiotics
ho_blood_ec_a <- list(ho_extRep_blood_carr, ho_extRep_blood_3gcr, ho_extRep_blood_3gcs)
ho_extRep_blood_ecoli_tb <- fun_data_astcombine2(ho_blood_ec_a)
ho_extRep_blood_ec_deathgraph <- ho_extRep_blood_ecoli_tb[movecolumn(names(ho_extRep_blood_ecoli_tb), "Antibiotic first")]
ho_extRep_blood_ec_deathtable <- ho_extRep_blood_ec_deathgraph[,c(1,3,6)]
names(ho_extRep_blood_ec_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(ho_extRep_blood_ecoli_tb)
rm(ho_blood_ec_a)
# Report: GN_Klebsiella pneumoniae ####
# data: merged_blood_dedup_kp
# Format the summary table for community-origin infections
co_extRep_blood_klebp <- merged_blood_dedup_kp[which(merged_blood_dedup_kp$InfOri==0),]
co_extRep_blood_klebp_tb <- fun_data_summary_kp(co_extRep_blood_klebp)
co_extRep_blood_klebp_tb[,8:10] = NULL
co_extRep_blood_klebp_table <- fun_data_asttable2(co_extRep_blood_klebp_tb)
co_extRep_blood_klebp_graph <- fun_data_summary_kp(co_extRep_blood_klebp)
rm(co_extRep_blood_klebp_tb)
# Format the summary table for hospital-origin infections
ho_extRep_blood_klebp <- merged_blood_dedup_kp[which(merged_blood_dedup_kp$InfOri==1),]
ho_extRep_blood_klebp_tb <- fun_data_summary_kp(ho_extRep_blood_klebp)
ho_extRep_blood_klebp_tb[,8:10] = NULL
ho_extRep_blood_klebp_table <- fun_data_asttable2(ho_extRep_blood_klebp_tb)
ho_extRep_blood_klebp_graph <- fun_data_summary_kp(ho_extRep_blood_klebp)
rm(ho_extRep_blood_klebp_tb)
# Format the mortality summary table for community-origin infections
co_extRep_blood_klebp$AST3gcsCarbs <- 0
co_extRep_blood_klebp$AST3gcsCarbs <- replace(co_extRep_blood_klebp$AST3gcsCarbs,
                                              co_extRep_blood_klebp$AST3gc==0 & (co_extRep_blood_klebp$ASTCarbapenem!=1|
                                                                                is.na(co_extRep_blood_klebp$ASTCarbapenem)==1),
                                              values = 1)
co_extRep_blood_klebp$AST3gcsCarbs <- replace(co_extRep_blood_klebp$AST3gcsCarbs,
                                              co_extRep_blood_klebp$AST3gc==1 & (co_extRep_blood_klebp$ASTCarbapenem!=1|
                                                                                is.na(co_extRep_blood_klebp$ASTCarbapenem)==1),
                                              values = 2)

co_extRep_blood_3gcs <- fun_data_deathtable(co_extRep_blood_klebp, co_extRep_blood_klebp$AST3gcsCarbs, 1, "3GC-S")
co_extRep_blood_3gcr <- fun_data_deathtable(co_extRep_blood_klebp, co_extRep_blood_klebp$AST3gcsCarbs, 2, "3GC-NS")
co_extRep_blood_carr <- fun_data_deathtable(co_extRep_blood_klebp, co_extRep_blood_klebp$ASTCarbapenem, 1, "Carbapenem-NS")
# combine individual antibiotics
co_blood_kp_a <- list(co_extRep_blood_carr, co_extRep_blood_3gcr, co_extRep_blood_3gcs)
co_extRep_blood_klebp_tb <- fun_data_astcombine2(co_blood_kp_a)
co_extRep_blood_kp_deathgraph <- co_extRep_blood_klebp_tb[movecolumn(names(co_extRep_blood_klebp_tb), "Antibiotic first")]
co_extRep_blood_kp_deathtable <- co_extRep_blood_kp_deathgraph[,c(1,3,6)]
names(co_extRep_blood_kp_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(co_extRep_blood_klebp_tb)
rm(co_blood_kp_a)
# Format the mortality summary table for hospital-origin infections
ho_extRep_blood_klebp$AST3gcsCarbs <- 0
ho_extRep_blood_klebp$AST3gcsCarbs <- replace(ho_extRep_blood_klebp$AST3gcsCarbs,
                                              ho_extRep_blood_klebp$AST3gc==0 & (ho_extRep_blood_klebp$ASTCarbapenem!=1|
                                                                                is.na(ho_extRep_blood_klebp$ASTCarbapenem)==1),
                                              values = 1)
ho_extRep_blood_klebp$AST3gcsCarbs <- replace(ho_extRep_blood_klebp$AST3gcsCarbs,
                                              ho_extRep_blood_klebp$AST3gc==1 & (ho_extRep_blood_klebp$ASTCarbapenem!=1|
                                                                                is.na(ho_extRep_blood_klebp$ASTCarbapenem)==1),
                                              values = 2)

ho_extRep_blood_3gcs <- fun_data_deathtable(ho_extRep_blood_klebp, ho_extRep_blood_klebp$AST3gcsCarbs, 1, "3GC-S")
ho_extRep_blood_3gcr <- fun_data_deathtable(ho_extRep_blood_klebp, ho_extRep_blood_klebp$AST3gcsCarbs, 2, "3GC-NS")
ho_extRep_blood_carr <- fun_data_deathtable(ho_extRep_blood_klebp, ho_extRep_blood_klebp$ASTCarbapenem, 1, "Carbapenem-NS")
# combine individual antibiotics
ho_blood_kp_a <- list(ho_extRep_blood_carr, ho_extRep_blood_3gcr, ho_extRep_blood_3gcs)
ho_extRep_blood_klebp_tb <- fun_data_astcombine2(ho_blood_kp_a)
ho_extRep_blood_kp_deathgraph <- ho_extRep_blood_klebp_tb[movecolumn(names(ho_extRep_blood_klebp_tb), "Antibiotic first")]
ho_extRep_blood_kp_deathtable <- ho_extRep_blood_kp_deathgraph[,c(1,3,6)]
names(ho_extRep_blood_kp_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(ho_extRep_blood_klebp_tb)
rm(ho_blood_kp_a)
# Report: GN_Pseudomonas aeruginosa ####
# data: merged_blood_dedup_pa
# Format the summary table for community-origin infections
co_extRep_blood_pseudoa <- merged_blood_dedup_pa[which(merged_blood_dedup_pa$InfOri==0),]
co_extRep_blood_pseudoa_tb <- fun_data_summary_pa(co_extRep_blood_pseudoa)
co_extRep_blood_pseudoa_tb[,8:10] = NULL
co_extRep_blood_pseudoa_table <- fun_data_asttable2(co_extRep_blood_pseudoa_tb)
co_extRep_blood_pseudoa_graph <- fun_data_summary_pa(co_extRep_blood_pseudoa)
rm(co_extRep_blood_pseudoa_tb)
# Format the summary table for hospital-origin infections
ho_extRep_blood_pseudoa <- merged_blood_dedup_pa[which(merged_blood_dedup_pa$InfOri==1),]
ho_extRep_blood_pseudoa_tb <- fun_data_summary_pa(ho_extRep_blood_pseudoa)
ho_extRep_blood_pseudoa_tb[,8:10] = NULL
ho_extRep_blood_pseudoa_table <- fun_data_asttable2(ho_extRep_blood_pseudoa_tb)
ho_extRep_blood_pseudoa_graph <- fun_data_summary_pa(ho_extRep_blood_pseudoa)
rm(ho_extRep_blood_pseudoa_tb)
# Format the mortality summary table for community-origin infections
co_extRep_blood_carbs <- fun_data_deathtable(co_extRep_blood_pseudoa, co_extRep_blood_pseudoa$ASTCarbapenem, 0, "Carbapenem-S")
co_extRep_blood_carbr <- fun_data_deathtable(co_extRep_blood_pseudoa, co_extRep_blood_pseudoa$ASTCarbapenem, 1, "Carbapenem-NS")
# combine individual antibiotics
co_blood_pa_a <- list(co_extRep_blood_carbr, co_extRep_blood_carbs)
co_extRep_blood_pseudoa_tb <- fun_data_astcombine2(co_blood_pa_a)
co_extRep_blood_pa_deathgraph <- co_extRep_blood_pseudoa_tb[movecolumn(names(co_extRep_blood_pseudoa_tb), "Antibiotic first")]
co_extRep_blood_pa_deathtable <- co_extRep_blood_pa_deathgraph[,c(1,3,6)]
names(co_extRep_blood_pa_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(co_extRep_blood_pseudoa_tb)
rm(co_blood_pa_a)
rm(co_extRep_blood_carbs)
rm(co_extRep_blood_carbr)
# Format the mortality summary table for hospital-origin infections
ho_extRep_blood_carbs <- fun_data_deathtable(ho_extRep_blood_pseudoa, ho_extRep_blood_pseudoa$ASTCarbapenem, 0, "Carbapenem-S")
ho_extRep_blood_carbr <- fun_data_deathtable(ho_extRep_blood_pseudoa, ho_extRep_blood_pseudoa$ASTCarbapenem, 1, "Carbapenem-NS")
# combine individual antibiotics
ho_blood_pa_a <- list(ho_extRep_blood_carbr, ho_extRep_blood_carbs)
ho_extRep_blood_pseudoa_tb <- fun_data_astcombine2(ho_blood_pa_a)
ho_extRep_blood_pa_deathgraph <- ho_extRep_blood_pseudoa_tb[movecolumn(names(ho_extRep_blood_pseudoa_tb), "Antibiotic first")]
ho_extRep_blood_pa_deathtable <- ho_extRep_blood_pa_deathgraph[,c(1,3,6)]
names(ho_extRep_blood_pa_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(ho_extRep_blood_pseudoa_tb)
rm(ho_blood_pa_a)
rm(ho_extRep_blood_carbs)
rm(ho_extRep_blood_carbr)
# Report: GN_Acinetobacter spp. ####
# data: merged_blood_dedup_as
# Format the summary table for community-origin infections
co_extRep_blood_acines <- merged_blood_dedup_as[which(merged_blood_dedup_as$InfOri==0),]
co_extRep_blood_acines_tb <- fun_data_summary_as(co_extRep_blood_acines)
co_extRep_blood_acines_tb[,8:10] = NULL
co_extRep_blood_acines_table <- fun_data_asttable2(co_extRep_blood_acines_tb)
co_extRep_blood_acines_graph <- fun_data_summary_as(co_extRep_blood_acines)
rm(co_extRep_blood_acines_tb)
# Format the summary table for hospital-origin infections
ho_extRep_blood_acines <- merged_blood_dedup_as[which(merged_blood_dedup_as$InfOri==1),]
ho_extRep_blood_acines_tb <- fun_data_summary_as(ho_extRep_blood_acines)
ho_extRep_blood_acines_tb[,8:10] = NULL
ho_extRep_blood_acines_table <- fun_data_asttable2(ho_extRep_blood_acines_tb)
ho_extRep_blood_acines_graph <- fun_data_summary_as(ho_extRep_blood_acines)
rm(ho_extRep_blood_acines_tb)
# Format the mortality summary table for community-origin infections
co_extRep_blood_carbs <- fun_data_deathtable(co_extRep_blood_acines, co_extRep_blood_acines$ASTCarbapenem, 0, "Carbapenem-S")
co_extRep_blood_carbr <- fun_data_deathtable(co_extRep_blood_acines, co_extRep_blood_acines$ASTCarbapenem, 1, "Carbapenem-NS")
# combine individual antibiotics
co_blood_as_a <- list(co_extRep_blood_carbr, co_extRep_blood_carbs)
co_extRep_blood_acines_tb <- fun_data_astcombine2(co_blood_as_a)
co_extRep_blood_as_deathgraph <- co_extRep_blood_acines_tb[movecolumn(names(co_extRep_blood_acines_tb), "Antibiotic first")]
co_extRep_blood_as_deathtable <- co_extRep_blood_as_deathgraph[,c(1,3,6)]
names(co_extRep_blood_as_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(co_extRep_blood_acines_tb)
rm(co_blood_as_a)
rm(co_extRep_blood_carbs)
rm(co_extRep_blood_carbr)
# Format the mortality summary table for hospital-origin infections
ho_extRep_blood_carbs <- fun_data_deathtable(ho_extRep_blood_acines, ho_extRep_blood_acines$ASTCarbapenem, 0, "Carbapenem-S")
ho_extRep_blood_carbr <- fun_data_deathtable(ho_extRep_blood_acines, ho_extRep_blood_acines$ASTCarbapenem, 1, "Carbapenem-NS")
# combine individual antibiotics
ho_blood_as_a <- list(ho_extRep_blood_carbr, ho_extRep_blood_carbs)
ho_extRep_blood_acines_tb <- fun_data_astcombine2(ho_blood_as_a)
ho_extRep_blood_as_deathgraph <- ho_extRep_blood_acines_tb[movecolumn(names(ho_extRep_blood_acines_tb), "Antibiotic first")]
ho_extRep_blood_as_deathtable <- ho_extRep_blood_as_deathgraph[,c(1,3,6)]
names(ho_extRep_blood_as_deathtable) <- c("Type of pathogen", "Mortality (n)", "95% CI")
rm(ho_extRep_blood_acines_tb)
rm(ho_blood_as_a)
rm(ho_extRep_blood_carbs)
rm(ho_extRep_blood_carbr)

# Remove unused variables ####
# Remove variables that will not be used further
rm(admdate2)
rm(disdate2)
rm(spcdate2)
HospData2$do.call.rbind..admdate2.  <- NULL
HospData2$do.call.rbind..disdate2.  <- NULL
MicroData2$do.call.rbind..spcdate2. <- NULL
HospData2$admdate2  <- NULL
HospData2$disdate2  <- NULL
MicroData2$spcdate2 <- NULL
HospData2$do.call.rbind..bdate2. <- NULL
HospData2$bdate2 <- NULL
# Text in Section 3 (Isolate-based with stratification) ####
min_spc_date_report3 <- format(min(MicroData3$DateSpc, na.rm=TRUE), datefmt_text)
max_spc_date_report3 <- format(max(MicroData3$DateSpc, na.rm=TRUE), datefmt_text)
data_blood_pos_survey <- HospMicroData_bsi[which(HospMicroData_bsi$organismCat!=9), c("hn", "specimen_collection_date")]
num_blood_pos_survey <- nrow(merged_blood_dedup_sa)+nrow(merged_blood_dedup_es)+nrow(merged_blood_dedup_sp)+
  nrow(merged_blood_dedup_ss)+nrow(merged_blood_dedup_ec)+nrow(merged_blood_dedup_kp)+
  nrow(merged_blood_dedup_pa)+nrow(merged_blood_dedup_as)
if(checkpoint_hosp_adm_data_ava=="yes"){
  min_adm_date_report3 <- format(min(HospData2$DateAdm, na.rm=TRUE), datefmt_text)
  max_adm_date_report3 <- format(max(HospData2$DateAdm, na.rm=TRUE), datefmt_text)
  min_dis_date_report3 <- format(min(HospData2$DateDis, na.rm=TRUE), datefmt_text)
  max_dis_date_report3 <- format(max(HospData2$DateDis, na.rm=TRUE), datefmt_text)
  report3_rawadm <- nrow(HospData)
}else{
  min_adm_date_report3 <- "NA"
  max_adm_date_report3 <- "NA"
  min_dis_date_report3 <- "NA"
  max_dis_date_report3 <- "NA"
  report3_rawadm <- "NA"
}
if(checkpoint_hosp_adm_date_ava=="yes"){
  min_merged_adm_date_report3 <- format(min(HospMicroData_bsi$DateAdm, na.rm=TRUE), datefmt_text)
  max_merged_adm_date_report3 <- format(max(HospMicroData_bsi$DateAdm, na.rm=TRUE), datefmt_text)
}else{
  min_merged_adm_date_report3 <- "NA"
  max_merged_adm_date_report3 <- "NA"
}
min_merged_spc_date_report3 <- format(min(HospMicroData_bsi$DateSpc, na.rm=TRUE), datefmt_text)
max_merged_spc_date_report3 <- format(max(HospMicroData_bsi$DateSpc, na.rm=TRUE), datefmt_text)

Rpt3_pg2_totalpatients = nrow(blood_dedup_sa) + nrow(blood_dedup_es) +
  nrow(blood_dedup_sp) + nrow(blood_dedup_ss) +
  nrow(blood_dedup_ec) + nrow(blood_dedup_kp) +
  nrow(blood_dedup_pa) + nrow(blood_dedup_as)
Rpt3_pg2_totalco = length(which(merged_blood_dedup_sa$InfOri==0)) + length(which(merged_blood_dedup_es$InfOri==0)) +
  length(which(merged_blood_dedup_sp$InfOri==0)) + length(which(merged_blood_dedup_ss$InfOri==0)) +
  length(which(merged_blood_dedup_ec$InfOri==0)) + length(which(merged_blood_dedup_kp$InfOri==0)) +
  length(which(merged_blood_dedup_pa$InfOri==0)) + length(which(merged_blood_dedup_as$InfOri==0))
Rpt3_pg2_totalho = length(which(merged_blood_dedup_sa$InfOri==1)) + length(which(merged_blood_dedup_es$InfOri==1)) +
  length(which(merged_blood_dedup_sp$InfOri==1)) + length(which(merged_blood_dedup_ss$InfOri==1)) +
  length(which(merged_blood_dedup_ec$InfOri==1)) + length(which(merged_blood_dedup_kp$InfOri==1)) +
  length(which(merged_blood_dedup_pa$InfOri==1)) + length(which(merged_blood_dedup_as$InfOri==1))
Rpt3_pg2_totalunk = Rpt3_pg2_totalpatients - Rpt3_pg2_totalco - Rpt3_pg2_totalho

# Text on data information ####
## Country and hospital names of the data
country = datadict[which(datadict[,1]=="country"),2]
hospital_name = datadict[which(datadict[,1]=="hospital_name"),2]
contact_person = datadict[which(datadict[,1]=="contact_person"),2]
contact_address = datadict[which(datadict[,1]=="contact_address"),2]
contact_email = datadict[which(datadict[,1]=="contact_email"),2]
notes_on_the_cover <- ifelse(datadict[which(datadict[,1]=="notes_on_the_cover"),2]=="empty001_micro", "",datadict[which(datadict[,1]=="notes_on_the_cover"),2])
#PopulationCoverage = datadict[which(datadict[,1]=="population_coverage"),2]
## Report created date
today <- as.character(format(Sys.Date(), datefmt_text))
Cover_today <- paste("Created on: ", today, sep="")
## Minimum admission date in raw hospital dataset and merged dataset
min_merged_admdate <- format(min(HospMicroData_bsi$DateAdm, na.rm=TRUE), datefmt_text)
## Maximum admission date in raw hospital dataset and merged dataset
max_merged_admdate <- format(max(HospMicroData_bsi$DateAdm, na.rm=TRUE), datefmt_text)
## Minimum discharge date in raw hospital dataset and merged dataset
min_rawhos_disdate <- format(min(HospData2$DateDis, na.rm=TRUE), datefmt_text)
min_merged_disdate <- format(min(HospMicroData_bsi$DateDis, na.rm=TRUE), datefmt_text)
## Maximum discharge date in raw hospital dataset and merged dataset
max_rawhos_disdate <- format(max(HospData2$DateDis, na.rm=TRUE), datefmt_text)
max_merged_disdate <- format(max(HospMicroData_bsi$DateDis, na.rm=TRUE), datefmt_text)
## Minimum Specimen collection date in raw Microbiology dataset, MicroData_bsi, and merged dataset
min_merged_spcdate <- format(min(HospMicroData_bsi$DateSpc, na.rm=TRUE), datefmt_text)
## Maximum Specimen collection date in raw Microbiology dataset, MicroData_bsi, and merged dataset
max_merged_spcdate <- format(max(HospMicroData_bsi$DateSpc, na.rm=TRUE), datefmt_text)


# Denominator for Sample-based report [Sample-based without stratification] ####
# [Report 4] Number of patients sampled per specimen type (blood) per year
blood <- MicroData2[which(MicroData2$blood=="blood"),]
#blood <- blood[which((blood$DateSpc>=min_date_data_include) & (blood$DateSpc<=max_date_data_include)),]
# The number of records on blood samples identified in the raw micro data file with the survey time period above
num_sampled_blood = nrow(blood)
# The number of patients sampled for blood culture
num_patients_blood = length(unique(blood$hn))
rm(blood)
# [Report 4] Number of patients with positive blood culture for the 8 organisms included in this report
a <- # per sample type
  MicroData_bsi %>%
  # per pathogen
  dplyr::filter(organismCat!=9) %>%
  # order data by hospital number (patient identifier), and sample collection date
  dplyr::arrange(hn, DateSpc, desc(AMR)) %>%
  # group by patient (hn)
  dplyr::group_by(hn) %>%
  dplyr::mutate(inanalysis=row_number()) %>%
  # keep only the first isolate
  dplyr::filter(inanalysis==1) %>%
  # keep only sample within the date range
  dplyr::filter(DateSpc>=min_date_data_include & DateSpc<=max_date_data_include)
num_blood_pos_org <- nrow(a)
rm(a)
# Denominator for Sample-based report [Sample-based with stratification] ####
# [Report 5] Patient-risk among those stayed in the hospital more than 2 days
### Merge blood cultures in the raw microbiology data file to raw hospital admission data file
blood <- MicroData3[which(MicroData3$blood=="blood"),]
if(checkpoint_hosp_adm_data_ava=="yes"){
  # Delete the columns in hospital data file that has the same name as that
  # in microbiology data file
  for (i in names(blood)){if(i!="hn"){HospData2[i] <- NULL}else{}}
  # Merge raw hospital data file to cleaned microbiology data
  HospMicroData_all <- merge(HospData2, blood, by="hn", all=TRUE)
}else{
  HospMicroData_all <- blood
}
######## Admission date
HospMicroData_all <- fun_datevariable(HospMicroData_all, HospMicroData_all$date_of_admission)
colnames(HospMicroData_all)[ncol(HospMicroData_all)] <- "admdate2"
HospMicroData_all$DateAdm <- multidate(HospMicroData_all$admdate2)
### For when the date variable is in character and numeric format of excel i.e. xxxx
if (sum(is.na(HospMicroData_all$DateAdm)==TRUE)==nrow(HospMicroData_all)){
  HospMicroData_all$admdate2 <- as.numeric(HospMicroData_all$date_of_admission)
  HospMicroData_all$DateAdm <- as.Date(HospMicroData_all$admdate2, origin="1899-12-30")
}else{}
######## Discharge date
HospMicroData_all <- fun_datevariable(HospMicroData_all, HospMicroData_all$date_of_discharge)
colnames(HospMicroData_all)[ncol(HospMicroData_all)] <- "disdate2"
HospMicroData_all$DateDis <- multidate(HospMicroData_all$disdate2)
### For when the date variable is in character and numeric format of excel i.e. xxxx
if (sum(is.na(HospMicroData_all$DateDis)==TRUE)==nrow(HospMicroData_all)){
  HospMicroData_all$disdate2 <- as.numeric(HospMicroData_all$date_of_discharge)
  HospMicroData_all$DateDis <- as.Date(HospMicroData_all$disdate2, origin="1899-12-30")
}else{}
### Keep observations that is within the admission and discharge period
if(checkpoint_hosp_adm_date_ava=="yes" & ("date_of_admission" %in% names(MicroData2))==FALSE){
  ### Label observations with Samples collected between admission and discharge date
  HospMicroData_all$AdmSpc <- as.numeric(HospMicroData_all$DateSpc>=HospMicroData_all$DateAdm &
                                           HospMicroData_all$DateSpc<=HospMicroData_all$DateDis)
  HospMicroData_all$missingSpc <- ifelse(is.na(HospMicroData_all$DateSpc)==TRUE, 1, 0)
  ### Delete admissions without a specimen collected
  HospMicroData_all <- HospMicroData_all[which(HospMicroData_all$missingSpc==0),]
  HospMicroData_all <- HospMicroData_all[which(HospMicroData_all$AdmSpc==1),]
}else{
  HospMicroData_all <- HospMicroData_all
}
### A new data frame for admission date, discharge date, and specimen date
HospMicroData_all_3 <- data.frame(HospMicroData_all$hn, HospMicroData_all$DateAdm, HospMicroData_all$DateSpc, HospMicroData_all$specimen_collection_date)
names(HospMicroData_all_3) <- c("hn", "DateAdm", "DateSpc", "spcdate")
### Calculate the number of days in hospital from admission to blood collected
HospMicroData_all_3$diff_adm_spc <- (HospMicroData_all_3$DateSpc-HospMicroData_all_3$DateAdm)+1
### [Report 5] Total number of blood specimens collected at admission
num_blood_at_admission <- length(which(HospMicroData_all_3$diff_adm_spc<=2))
### [Report 5] Total number of blood specimens collected after 2 days of hospital stay
num_blood_after_2days <- length(which(HospMicroData_all_3$diff_adm_spc>=2))
### Keep only the first blood specimen within the admission (de-duplicated by hn and DateAdm)
HospMicroData_all_3 <- HospMicroData_all_3[which(HospMicroData_all_3$diff_adm_spc>0),]
HospMicroData_all_3$type <- ifelse(HospMicroData_all_3$diff_adm_spc>2,1,0) #1=HO; 0=CO
HospMicroData_all_4 <-
  # data
  HospMicroData_all_3 %>%
  # order data by hospital number (patient identifier), and sample collection date
  dplyr::arrange(hn, DateAdm, DateSpc) %>%
  # group by patient (hn)
  dplyr::group_by(hn, DateAdm) %>%
  dplyr::mutate(inanalysis=row_number()) %>%
  # keep only the first isolate
  dplyr::filter(inanalysis==1)
HospMicroData_all_4 <-
  # data
  HospMicroData_all_4 %>%
  # order data by hospital number (patient identifier), and sample collection date
  dplyr::arrange(hn, DateAdm, DateSpc) %>%
  # group by patient (hn)
  dplyr::group_by(hn, type) %>%
  dplyr::mutate(inanalysis2=row_number()) %>%
  # keep only the first isolate
  dplyr::filter(inanalysis2==1)
### [Report 5] Total number of patients with first blood samples collected after 2 calendar days
num_blood_dedup_ha <- length(which(HospMicroData_all_4$diff_adm_spc>2))
### [Report 5] Total number of patients with first blood samples collected at admission
num_blood_dedup_ca <- length(which(HospMicroData_all_4$diff_adm_spc<=2))
### Keep only the first blood specimen within the admission (de-duplicated by hn and DateAdm)
HospMicroData_all_5 <-
  # data
  HospMicroData_all_3 %>%
  # order data by hospital number (patient identifier), and sample collection date
  dplyr::arrange(hn, DateAdm, DateSpc) %>%
  # group by patient (hn)
  dplyr::group_by(hn) %>%
  dplyr::mutate(inanalysis=row_number()) %>%
  # keep only the first isolate
  dplyr::filter(inanalysis==1)
### [Section 5] Total number of patients is in neither group mentioned above
# Reason could be no matched DateAdm or DateAmd<SpcDate
num_blood_dedup_unk <- length(unique(blood$hn))-
  length(which(HospMicroData_all_5$diff_adm_spc>2))-
  length(which(HospMicroData_all_5$diff_adm_spc<=2))
# Count the number of patients with more than 1 admissions
table_hn <- data.frame(table(HospMicroData_all_4$hn))
num_blood_dedup_more_than_one_admissions <- nrow(table_hn[table_hn$Freq > 1,])

### Remove data that will not be used further
rm(HospMicroData_all)
rm(HospMicroData_all_3)
rm(HospMicroData_all_4)
rm(HospMicroData_all_5)
rm(table_hn)
rm(blood)

# Sample-based report tables ####
# Incidence of infection for all organisms (without stratification)
incidence_blood <- fun_data_incidence(blood_dedup_sa, blood_dedup_es, blood_dedup_sp,
                                      blood_dedup_ss, blood_dedup_ec, blood_dedup_kp,
                                      blood_dedup_pa, blood_dedup_as,
                                      num_patients_blood, 100000)
table_incidence_blood <- incidence_blood
table_incidence_blood$Incidence <- paste(ceiling(table_incidence_blood$incid_tested), " (",
                                         ceiling(table_incidence_blood$incid_tested_lci), "-",
                                         ceiling(table_incidence_blood$incid_tested_uci), ")", sep = "")
#table_incidence_blood$Incidence <- replace(table_incidence_blood$Incidence, table_incidence_blood[,"NumberofPatients"]==0, values="NA")
table_incidence_blood <- table_incidence_blood[, c("Organism", "Incidence")]
names(table_incidence_blood) <- c("Organisms", "**Frequency (95% CI)")
# Incidence of infection for all organisms (community-origin)
incidence_blood_co <- fun_data_incidence(co_extRep_blood_stapha, co_extRep_blood_enterococcus,
                                         co_extRep_blood_streptopneu, co_extRep_blood_salmonella,
                                         co_extRep_blood_ecoli, co_extRep_blood_klebp,
                                         co_extRep_blood_pseudoa, co_extRep_blood_acines,
                                         num_blood_dedup_ca, 100000)
table_incidence_blood_co <- incidence_blood_co
table_incidence_blood_co$Incidence <- paste(ceiling(table_incidence_blood_co$incid_tested), " (",
                                            ceiling(table_incidence_blood_co$incid_tested_lci), "-",
                                            ceiling(table_incidence_blood_co$incid_tested_uci), ")", sep = "")
table_incidence_blood_co <- table_incidence_blood_co[, c("Organism", "Incidence")]
names(table_incidence_blood_co) <- c("Organism", "**Frequency (95% CI)")
# Incidence of infection for all organisms (hospital-origin)
incidence_blood_ho <- fun_data_incidence(ho_extRep_blood_stapha, ho_extRep_blood_enterococcus,
                                         ho_extRep_blood_streptopneu, ho_extRep_blood_salmonella,
                                         ho_extRep_blood_ecoli, ho_extRep_blood_klebp,
                                         ho_extRep_blood_pseudoa, ho_extRep_blood_acines,
                                         num_blood_dedup_ha, 100000)
table_incidence_blood_ho <- incidence_blood_ho
table_incidence_blood_ho$Incidence <- paste(ceiling(table_incidence_blood_ho$incid_tested), " (",
                                            ceiling(table_incidence_blood_ho$incid_tested_lci), "-",
                                            ceiling(table_incidence_blood_ho$incid_tested_uci), ")", sep = "")
table_incidence_blood_ho <- table_incidence_blood_ho[, c("Organism", "Incidence")]
names(table_incidence_blood_ho) <- c("Organism", "**Frequency (95% CI)")
# Incidence of NS infection (per 100,000 tested population)
incidence_blood_antibiotic <- fun_data_incidence_2(blood_dedup_sa$ASTmrsa, blood_dedup_es$ASTVancomycin,
                                                   blood_dedup_sp$ASTPenicillin_G, blood_dedup_ss$ASTFluoroquin,
                                                   blood_dedup_ec$AST3gc, blood_dedup_ec$ASTCarbapenem,
                                                   blood_dedup_kp$AST3gc, blood_dedup_kp$ASTCarbapenem,
                                                   blood_dedup_pa$ASTCarbapenem, blood_dedup_as$ASTCarbapenem,
                                                   num_patients_blood, 100000)
table_incidence_blood_antibiotic <- incidence_blood_antibiotic
table_incidence_blood_antibiotic$Incidence <- paste(ceiling(table_incidence_blood_antibiotic$incid_tested), " (",
                                                    ceiling(table_incidence_blood_antibiotic$incid_tested_lci), "-",
                                                    ceiling(table_incidence_blood_antibiotic$incid_tested_uci), ")", sep = "")
#table_incidence_blood_antibiotic$Incidence <- replace(table_incidence_blood_antibiotic$Incidence, table_incidence_blood_antibiotic[,"NumberofPatients"]==0, values="NA")
table_incidence_blood_antibiotic <- table_incidence_blood_antibiotic[, c("Priority", "Incidence")]
names(table_incidence_blood_antibiotic) <- c("Organism", "**Frequency (95% CI)")
# Community-origin: Incidence of NS infection (per 100,000 tested population)
incidence_blood_antibiotic_co <- fun_data_incidence_2(co_extRep_blood_stapha$ASTmrsa, co_extRep_blood_enterococcus$ASTVancomycin,
                                                      co_extRep_blood_streptopneu$ASTPenicillin_G, co_extRep_blood_salmonella$ASTFluoroquin,
                                                      co_extRep_blood_ecoli$AST3gc, co_extRep_blood_ecoli$ASTCarbapenem,
                                                      co_extRep_blood_klebp$AST3gc, co_extRep_blood_klebp$ASTCarbapenem,
                                                      co_extRep_blood_pseudoa$ASTCarbapenem, co_extRep_blood_acines$ASTCarbapenem,
                                                      num_blood_dedup_ca, 100000)
table_incidence_blood_antibiotic_co <- incidence_blood_antibiotic_co
table_incidence_blood_antibiotic_co$Incidence <- paste(ceiling(table_incidence_blood_antibiotic_co$incid_tested), " (",
                                                       ceiling(table_incidence_blood_antibiotic_co$incid_tested_lci), "-",
                                                       ceiling(table_incidence_blood_antibiotic_co$incid_tested_uci), ")", sep = "")
table_incidence_blood_antibiotic_co <- table_incidence_blood_antibiotic_co[, c("Priority", "Incidence")]
names(table_incidence_blood_antibiotic_co) <- c("Organism", "**Frequency (95% CI)")
# Hospital-origin: Incidence of NS infection (per 100,000 tested patients)
incidence_blood_antibiotic_ho <- fun_data_incidence_2(ho_extRep_blood_stapha$ASTmrsa, ho_extRep_blood_enterococcus$ASTVancomycin,
                                                      ho_extRep_blood_streptopneu$ASTPenicillin_G, ho_extRep_blood_salmonella$ASTFluoroquin,
                                                      ho_extRep_blood_ecoli$AST3gc, ho_extRep_blood_ecoli$ASTCarbapenem,
                                                      ho_extRep_blood_klebp$AST3gc, ho_extRep_blood_klebp$ASTCarbapenem,
                                                      ho_extRep_blood_pseudoa$ASTCarbapenem, ho_extRep_blood_acines$ASTCarbapenem,
                                                      num_blood_dedup_ha, 100000)
table_incidence_blood_antibiotic_ho <- incidence_blood_antibiotic_ho
table_incidence_blood_antibiotic_ho$Incidence <- paste(ceiling(table_incidence_blood_antibiotic_ho$incid_tested), " (",
                                                       ceiling(table_incidence_blood_antibiotic_ho$incid_tested_lci), "-",
                                                       ceiling(table_incidence_blood_antibiotic_ho$incid_tested_uci), ")", sep = "")
table_incidence_blood_antibiotic_ho <- table_incidence_blood_antibiotic_ho[, c("Priority", "Incidence")]
names(table_incidence_blood_antibiotic_ho) <- c("Organism", "**Frequency (95% CI)")
# Data availability checkpoint ####
# no = data is not available; yes = data is available
checkpoint1_Report2 = checkpoint_micro_data_ava
checkpoint2_Report3 = ifelse(checkpoint_hosp_adm_date_ava=="no" & avai_Infect_Ori=="no",
                             "no", "yes")
checkpoint3_Report4_5_popcov = ifelse(all(PopulationCoverage=="unk") | str_detect(PopulationCoverage, "empty"),
                                      "no", "yes")
checkpoint3_Report4_5_nogrowth = ifelse(count_data_nogrowth<=0,
                                        "no", "yes")
checkpoint5_Report6 = ifelse(all(is.na(HospMicroData_bsi[,"discharge_status"]))=="TRUE",
                             "no", "yes")
# Text for Section 4 ####
#num_admissions <- num_admissions
Rp4_min_date_data_include_fmt <- min_rawmicro_spcdate
Rp4_max_date_data_include_fmt <- max_rawmicro_spcdate
# Text for Section 5 ####
Rp5_min_date_data_include_fmt <- Rp4_min_date_data_include_fmt
Rp5_max_date_data_include_fmt <- Rp4_max_date_data_include_fmt
Rpt5_pg1_totalunk <- num_blood_dedup_unk

# Text for Report 6 ####
# overall mortality
HospData2$disoutcome_numorder <- ifelse(HospData2$disoutcome2_cat=="died", 0, 1)
data_patient_hospadm <-
  HospData2 %>%
  # order data by hospital number (patient identifier) and Sample date
  dplyr::arrange(hn, disoutcome_numorder, DateAdm) %>%
  # identify the unique patient using HN (unique identifier)
  dplyr::group_by(hn) %>%
  dplyr::mutate(inanalysis=row_number()) %>%
  # keep only the first unique patient identifier
  dplyr::filter(inanalysis==1)

if(checkpoint_hosp_adm_data_ava=="yes"){
  Rpt6_no_raw_hosp <- nrow(HospData)
  count_patient_hospadm <- nrow(data_patient_hospadm)
  count_deaths_hospadm <- length(which(data_patient_hospadm$disoutcome2_cat=="died"))
  cfr <- round((count_deaths_hospadm/count_patient_hospadm)*100,0)
}else{
  Rpt6_no_raw_hosp <- "NA"
  count_patient_hospadm <- "NA"
  count_deaths_hospadm <- "NA"
  cfr <- "NA"
}

# Mortality by organism
cfr_sa_per <- round(length(which(merged_blood_dedup_sa$disoutcome_cat=="died"))/nrow(merged_blood_dedup_sa)*100,0)
cfr_es_per <- round(length(which(merged_blood_dedup_es$disoutcome_cat=="died"))/nrow(merged_blood_dedup_es)*100,0)
cfr_sp_per <- round(length(which(merged_blood_dedup_sp$disoutcome_cat=="died"))/nrow(merged_blood_dedup_sp)*100,0)
cfr_ss_per <- round(length(which(merged_blood_dedup_ss$disoutcome_cat=="died"))/nrow(merged_blood_dedup_ss)*100,0)
cfr_ec_per <- round(length(which(merged_blood_dedup_ec$disoutcome_cat=="died"))/nrow(merged_blood_dedup_ec)*100,0)
cfr_kp_per <- round(length(which(merged_blood_dedup_kp$disoutcome_cat=="died"))/nrow(merged_blood_dedup_kp)*100,0)
cfr_pa_per <- round(length(which(merged_blood_dedup_pa$disoutcome_cat=="died"))/nrow(merged_blood_dedup_pa)*100,0)
cfr_as_per <- round(length(which(merged_blood_dedup_as$disoutcome_cat=="died"))/nrow(merged_blood_dedup_as)*100,0)

# Mortality by organism and infection origin
crf_sa_co <- length(which(merged_blood_dedup_sa$InfOri==0 & merged_blood_dedup_sa$disoutcome_cat=="died"))
crf_es_co <- length(which(merged_blood_dedup_es$InfOri==0 & merged_blood_dedup_es$disoutcome_cat=="died"))
crf_sp_co <- length(which(merged_blood_dedup_sp$InfOri==0 & merged_blood_dedup_sp$disoutcome_cat=="died"))
crf_ss_co <- length(which(merged_blood_dedup_ss$InfOri==0 & merged_blood_dedup_ss$disoutcome_cat=="died"))
crf_ec_co <- length(which(merged_blood_dedup_ec$InfOri==0 & merged_blood_dedup_ec$disoutcome_cat=="died"))
crf_kp_co <- length(which(merged_blood_dedup_kp$InfOri==0 & merged_blood_dedup_kp$disoutcome_cat=="died"))
crf_pa_co <- length(which(merged_blood_dedup_pa$InfOri==0 & merged_blood_dedup_pa$disoutcome_cat=="died"))
crf_as_co <- length(which(merged_blood_dedup_as$InfOri==0 & merged_blood_dedup_as$disoutcome_cat=="died"))

crf_sa_ho <- length(which(merged_blood_dedup_sa$InfOri==1 & merged_blood_dedup_sa$disoutcome_cat=="died"))
crf_es_ho <- length(which(merged_blood_dedup_es$InfOri==1 & merged_blood_dedup_es$disoutcome_cat=="died"))
crf_sp_ho <- length(which(merged_blood_dedup_sp$InfOri==1 & merged_blood_dedup_sp$disoutcome_cat=="died"))
crf_ss_ho <- length(which(merged_blood_dedup_ss$InfOri==1 & merged_blood_dedup_ss$disoutcome_cat=="died"))
crf_ec_ho <- length(which(merged_blood_dedup_ec$InfOri==1 & merged_blood_dedup_ec$disoutcome_cat=="died"))
crf_kp_ho <- length(which(merged_blood_dedup_kp$InfOri==1 & merged_blood_dedup_kp$disoutcome_cat=="died"))
crf_pa_ho <- length(which(merged_blood_dedup_pa$InfOri==1 & merged_blood_dedup_pa$disoutcome_cat=="died"))
crf_as_ho <- length(which(merged_blood_dedup_as$InfOri==1 & merged_blood_dedup_as$disoutcome_cat=="died"))

crf_sa_co_percent <- round(crf_sa_co/length(which(merged_blood_dedup_sa$InfOri == 0))*100,0)
crf_es_co_percent <- round(crf_es_co/length(which(merged_blood_dedup_es$InfOri == 0))*100,0)
crf_sp_co_percent <- round(crf_sp_co/length(which(merged_blood_dedup_sp$InfOri == 0))*100,0)
crf_ss_co_percent <- round(crf_ss_co/length(which(merged_blood_dedup_ss$InfOri == 0))*100,0)
crf_ec_co_percent <- round(crf_ec_co/length(which(merged_blood_dedup_ec$InfOri == 0))*100,0)
crf_kp_co_percent <- round(crf_kp_co/length(which(merged_blood_dedup_kp$InfOri == 0))*100,0)
crf_pa_co_percent <- round(crf_pa_co/length(which(merged_blood_dedup_pa$InfOri == 0))*100,0)
crf_as_co_percent <- round(crf_as_co/length(which(merged_blood_dedup_as$InfOri == 0))*100,0)

crf_sa_ho_percent <- round(crf_sa_ho/length(which(merged_blood_dedup_sa$InfOri == 1))*100,0)
crf_es_ho_percent <- round(crf_es_ho/length(which(merged_blood_dedup_es$InfOri == 1))*100,0)
crf_sp_ho_percent <- round(crf_sp_ho/length(which(merged_blood_dedup_sp$InfOri == 1))*100,0)
crf_ss_ho_percent <- round(crf_ss_ho/length(which(merged_blood_dedup_ss$InfOri == 1))*100,0)
crf_ec_ho_percent <- round(crf_ec_ho/length(which(merged_blood_dedup_ec$InfOri == 1))*100,0)
crf_kp_ho_percent <- round(crf_kp_ho/length(which(merged_blood_dedup_kp$InfOri == 1))*100,0)
crf_pa_ho_percent <- round(crf_pa_ho/length(which(merged_blood_dedup_pa$InfOri == 1))*100,0)
crf_as_ho_percent <- round(crf_as_ho/length(which(merged_blood_dedup_as$InfOri == 1))*100,0)

# Texts used in the report
total_deaths_co = crf_sa_co + crf_es_co + crf_sp_co +
  crf_ss_co + crf_ec_co + crf_kp_co + crf_pa_co + crf_as_co
total_deaths_ho = crf_sa_ho + crf_es_ho + crf_sp_ho +
  crf_ss_ho + crf_ec_ho + crf_kp_ho + crf_pa_ho + crf_as_ho

totalpercent_deaths_co = round((total_deaths_co/Rpt3_pg2_totalco)*100,0)
totalpercent_deaths_ho = round((total_deaths_ho/Rpt3_pg2_totalho)*100,0)


## AMASSplus part ####
##De-duplicating data by hn
fun_deduplicate <- function(posDF){
  posDF_dedup <- # per sample type 
    posDF %>%
    # order data by hospital number (patient identifier), and sample collection date
    dplyr::arrange(hn, DateSpc) %>%
    # group by patient (hn)
    dplyr::group_by(hn) %>%
    dplyr::mutate(inanalysis=row_number()) %>%
    # keep only the first isolate
    dplyr::filter(inanalysis==1)
  return (posDF_dedup)
}

##Grouping data based on organisms counted by #spctype
fun_groupbyorgspc <- function(posDF_dedup) {
  posDF_dedup_group <- posDF_dedup %>% ##summary information
    dplyr::group_by(organismplus) %>%
    dplyr::count(spctypeplus)
  return (posDF_dedup_group)
}

##Creating first capital of each value in the vector
fun_firstCapital <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#parsing data for Annex A
MicroData2plus <- MicroData2
#retrieving rows by each organism in Annex A
org_bps <- datadict_plus[grep("organism_burkholderia_pseudomallei", datadict_plus$amass),]
org_bru <- datadict_plus[grep("organism_brucella", datadict_plus$amass),]
org_cdi <- datadict_plus[grep("organism_corynebacterium_diphtheriae", datadict_plus$amass),]
org_ngo <- datadict_plus[grep("organism_neisseria_gonorrhoeae", datadict_plus$amass),]
org_nme <- datadict_plus[grep("organism_neisseria_meningitidis", datadict_plus$amass),]
org_sal <- datadict_plus[grep("organism_salmonella", datadict_plus$amass),] #contains Salmonella paratyphi and Salmonella typhi
org_shi <- datadict_plus[grep("organism_shigella", datadict_plus$amass),]
org_ssu <- datadict_plus[grep("organism_streptococcus_suis", datadict_plus$amass),]
org_vib <- datadict_plus[grep("organism_vibrio", datadict_plus$amass),]
#assigning organism_XXX_spp to column amass 
org_bru$amass[!(org_bru$amass=="organism_brucella") ] <- "organism_brucella_spp"
org_sal$amass[!(org_sal$amass=="organism_salmonella_paratyphi") & !(org_sal$amass=="organism_salmonella_typhi")] <- "organism_non-typhoidal_salmonella_spp"
org_shi$amass[!(org_shi$amass=="organism_shigella") ] <- "organism_shigella_spp"
org_vib$amass[!(org_vib$amass=="organism_vibrio") ] <- "organism_vibrio_spp"
dict_organismplus <- rbind(org_bps, org_bru, org_cdi, org_ngo, org_nme, org_sal, org_shi, org_ssu, org_vib)
#preparing organisms for further steps ex. organism_salmonella_spp >>> salmonella spp
orgName <- dict_organismplus
orgName$amass <- gsub("organism_", "", orgName$amass)
orgName$amass <- gsub("_", " ", orgName$amass)
#retrieving unique organism of Annex A
orgName <- dplyr::pull(orgName, "amass") #dataframe to vector
orgName <- unique(orgName)
#mapping organism from user's name to amass' name
MicroData2plus$organismplus <- mapvalues(MicroData2plus$organism, from=dict_organismplus$hosp, to=dict_organismplus$amass)
#assigning organism_amassplus_no_growth and non-organismplus
for (idx in 1:nrow(MicroData2plus)) {
    if (is.na(MicroData2plus[idx,'organism3'])){ #if MicroData2plus[idx,"organism3"] is nan >>> assigning "non-organismplus"
        MicroData2plus[idx,'organismplus'] <- "non-organismplus"
    }
    else{
        if (MicroData2plus[idx,'organism3'] == "organism_no_growth") { #if MicroData2plus[idx,"organism3"] is "organism_no_growth" >>> assigning "organism_no_growth"
            MicroData2plus[idx,'organismplus'] <- "organism_no_growth"
        }
        else{
            if (grepl("organism_", MicroData2plus[idx,'organismplus'], fixed = TRUE)==FALSE) { #if MicroData2plus[idx,"organism3"] is not mapped >>> assigning "non-organismplus"
                MicroData2plus[idx,'organismplus'] <- "non-organismplus"
            }
        }
    }
}
#preparing organisms of MicroData2plus ex. organism_salmonella_spp >>> salmonella spp
MicroData2plus$organismplus <- gsub("organism_", "", MicroData2plus$organismplus)
MicroData2plus$organismplus <- gsub("_", " ", MicroData2plus$organismplus)

#retrieving rows by specimen types in Annex A
spcName_amass = c("specimen_blood", "specimen_cerebrospinal_fluid",  "specimen_genital_swab", "specimen_respiratory_tract", "specimen_stool", "specimen_urine", "specimen_others")
dict_specimenplus <- datadict_plus[datadict_plus$amass %in% spcName_amass,]
#mapping specimen types from user's name to amass' name
MicroData2plus$spctypeplus <- mapvalues(MicroData2plus$specimen_type, from=dict_specimenplus$hosp, to=dict_specimenplus$amass)
#preparing specimen types for further steps
spcName = c("blood", "csf",  "genital swab", "rts", "stool", "urine", "others")
#preparing specimen types of MicroData2plus
MicroData2plus$spctypeplus <- gsub("specimen_", "", MicroData2plus$spctypeplus)
MicroData2plus$spctypeplus <- gsub("_", " ", MicroData2plus$spctypeplus)
MicroData2plus$spctypeplus <- gsub("respiratory tract", "rts", MicroData2plus$spctypeplus)
MicroData2plus$spctypeplus <- gsub("cerebrospinal fluid", "csf", MicroData2plus$spctypeplus)
#renaming 'other non-interested specimens' (ex.pus) to 'others'
for (idx in 1:nrow(MicroData2plus)) {
  if (!(MicroData2plus[idx,'spctypeplus'] %in% spcName)) {
    MicroData2plus[idx,'spctypeplus'] <- "others"
  }
}


##creating positive and negative patient data frame
micro_pos <- MicroData2plus[!(MicroData2plus$organismplus == 'no growth'),]
micro_pos_org <- micro_pos[micro_pos$organismplus!="non-organismplus",] #selecting notifiable microorganisms
micro_neg <- MicroData2plus[MicroData2plus$organismplus == 'no growth',]
##de-duplicating data of total positive patients
#micro_pos_patient <- fun_deduplicate(micro_pos_org)
##creating patient summary table of P.39
col <- append('Total',spcName)
col <- append('Organisms',col)
micro_pos_dedup <- setNames(data.frame(matrix(ncol = 9, nrow = length(orgName))),col)
rownames(micro_pos_dedup) <- orgName # 11 rows with 10 origanisms and 1 total

##De-duplicating and Linking to dataframe 'micro_pos_dedup' (summary positive spcimens)
for (spc in spcName) {
  for (org in orgName) {
    micro_pos_spc <- micro_pos[micro_pos$spctypeplus==spc & micro_pos$organismplus==org,]
    micro_pos_spc_dedup <- fun_deduplicate(micro_pos_spc)
    micro_pos_dedup[org,spc] <- nrow(micro_pos_spc_dedup[micro_pos_spc_dedup$spctypeplus==spc&micro_pos_spc_dedup$organismplus==org,])
    micro_pos_dedup[org,'Total'] <- nrow(fun_deduplicate(micro_pos_org[micro_pos_org$organismplus==org,])) #add this column
    #micro_pos_dedup[org,'Total'] <- nrow(micro_pos_patient[micro_pos_patient$organismplus==org,])
    micro_pos_dedup[org,'Organisms'] <- org
  }
}

##Summarizing total positive specimens
micro_pos_dedup['Total',] <- c('Total',sum(micro_pos_dedup$Total),sum(micro_pos_dedup$blood),sum(micro_pos_dedup$csf),sum(micro_pos_dedup$`genital swab`),
                               sum(micro_pos_dedup$rts),sum(micro_pos_dedup$stool),sum(micro_pos_dedup$urine),
                               sum(micro_pos_dedup$others))
micro_pos_dedup$Organisms <- fun_firstCapital(micro_pos_dedup$Organisms)
for (col in colnames(micro_pos_dedup)) {
  if (col %in% spcName) {
    if (nrow(MicroData2plus[MicroData2plus$spctypeplus==col,]) == 0) {
      micro_pos_dedup[col] <- "NA"
    }
  }
}

colnames(micro_pos_dedup) <- gsub('rts', 'RTS', colnames(micro_pos_dedup))
colnames(micro_pos_dedup) <- gsub('Total', 'Total number\nof patients*', colnames(micro_pos_dedup))
#colnames(micro_pos_dedup) <- gsub('others', 'Others', colnames(micro_pos_dedup))
colnames(micro_pos_dedup) <- gsub('csf', 'CSF', colnames(micro_pos_dedup))
colnames(micro_pos_dedup) <- fun_firstCapital(colnames(micro_pos_dedup))

##Mortality
# Part 2: merge microbiology and hospital data files #### 
# To generate the extended report by merging Hospital admission and microbiology datasets
# Merge hospital and microbiology data files ####
if(checkpoint_hosp_adm_data_ava=="yes"){
  # Delete the columns in hospital data file that has the same name as that
  # in microbiology data file
  for (i in names(micro_pos_org)){if(i!="hn"){HospData2[i] <- NULL}else{}}
  # Merge raw hospital data file to cleaned microbiology data
  raw_merge_mortal <- merge(HospData2, micro_pos_org, by="hn", all=TRUE)
}else{
  raw_merge_mortal <- micro_pos_org
}
# Clean Date variable ####
######## Admission date
raw_merge_mortal <- fun_datevariable(raw_merge_mortal, raw_merge_mortal$date_of_admission)
colnames(raw_merge_mortal)[ncol(raw_merge_mortal)] <- "admdate2"
raw_merge_mortal$DateAdm <- multidate(raw_merge_mortal$admdate2)
### For when the date variable is in character and numeric format of excel i.e. xxxx
if (sum(is.na(raw_merge_mortal$DateAdm)==TRUE)==nrow(raw_merge_mortal)){
  raw_merge_mortal$admdate2 <- as.numeric(raw_merge_mortal$date_of_admission)
  raw_merge_mortal$DateAdm <- as.Date(raw_merge_mortal$admdate2, origin="1899-12-30")
}else{}
######## Discharge date
raw_merge_mortal <- fun_datevariable(raw_merge_mortal, raw_merge_mortal$date_of_discharge)
colnames(raw_merge_mortal)[ncol(raw_merge_mortal)] <- "disdate2"
raw_merge_mortal$DateDis <- multidate(raw_merge_mortal$disdate2)
### For when the date variable is in character and numeric format of excel i.e. xxxx
if (sum(is.na(raw_merge_mortal$DateDis)==TRUE)==nrow(raw_merge_mortal)){
  raw_merge_mortal$disdate2 <- as.numeric(raw_merge_mortal$date_of_discharge)
  raw_merge_mortal$DateDis <- as.Date(raw_merge_mortal$disdate2, origin="1899-12-30")
}else{}
######## birthday date 
avai_birthday <- datadict[which(datadict[,1]=="birthday_available"),2]
if(avai_birthday=="yes"){
  raw_merge_mortal <- fun_datevariable(raw_merge_mortal, raw_merge_mortal$birthday)
  colnames(raw_merge_mortal)[ncol(raw_merge_mortal)] <- "bdate2"
  raw_merge_mortal$DateBirth <- multidate(raw_merge_mortal$bdate2)
  ### For when the date variable is in character and numeric format of excel i.e. xxxx
  if (sum(is.na(raw_merge_mortal$DateBirth)==TRUE)==nrow(raw_merge_mortal)){
    raw_merge_mortal$bdate2 <- as.numeric(raw_merge_mortal$birthday)
    raw_merge_mortal$DateBirth <- as.Date(raw_merge_mortal$bdate2, origin="1899-12-30")
  }else{}
}else{}
# Check formated admission date variable 
raw_merge_mortal$YearAdm <- format(raw_merge_mortal$DateAdm, '%Y') #generate admission year
raw_merge_mortal$MonthAdm <- format(raw_merge_mortal$DateAdm, '%B') #generate admission month
raw_merge_mortal$MonthAdm <- factor(raw_merge_mortal$MonthAdm, levels=month.name) #define month as factor
# Check formated discharge date variable 
raw_merge_mortal$YearDis <- format(raw_merge_mortal$DateDis, '%Y') #generate discharge year
raw_merge_mortal$MonthDis <- format(raw_merge_mortal$DateDis, '%B') #generate discharge month
raw_merge_mortal$MonthDis <- factor(raw_merge_mortal$MonthDis, levels=month.name) #define month as factor 
# Label observations with Samples collected between admission and discharge date ####
if(checkpoint_hosp_adm_date_ava=="yes" & ("date_of_admission" %in% names(MicroData2))==FALSE){
  raw_merge_mortal$AdmSpc <- as.numeric(raw_merge_mortal$DateSpc>=raw_merge_mortal$DateAdm & 
                                          raw_merge_mortal$DateSpc<=raw_merge_mortal$DateDis)
}else{}
# Summary statistics on merged data ####
# Count the total number of records after merged
n_total_records <- nrow(raw_merge_mortal)
# Count the total number of records in Hospital admission data that does not have a match in microbiology data
n_total_records_unmerged_hosp <- length(which(is.na(raw_merge_mortal$DateSpc)==TRUE))
# Count the total number of records in microbiology data that does not have a match in hospital admission data
n_total_records_unmerged_micro <- length(which(is.na(raw_merge_mortal$DateAdm)==TRUE))
# Count the total number of missing hospital admission data
n_total_records_missAdmDate <- length(which(is.na(HospData2$DateAdm)==TRUE))
# Count the total number of missing specimen collection data
n_total_records_missSpcDate <- length(which(is.na(MicroData2$DateSpc)==TRUE))
# Keep observations that is within the admission and discharge period ####
if(checkpoint_hosp_adm_date_ava=="yes" & ("date_of_admission" %in% names(MicroData2))==FALSE){
  merge_mortal <- raw_merge_mortal[which(raw_merge_mortal$AdmSpc==1),]
  # Select only observations with admission date within the range defined above
  #merge_mortal <- raw_merge_mortal[which((raw_merge_mortal$DateAdm>=min_date_data_include) & (raw_merge_mortal$DateAdm<=max_date_data_include)),]
  # Keep blood samples collected within the range defined above
  #merge_mortal <- raw_merge_mortal[which((raw_merge_mortal$DateSpc>=min_date_data_include) & (raw_merge_mortal$DateSpc<=max_date_data_include)),]
}else{
  merge_mortal <- raw_merge_mortal
}
# Count the total number of merged record with specimen date within a set of admission and discharge dates
n_total_records_merged <- nrow(merge_mortal)
#write.csv(merge_mortal, file="./merge_mortal.csv", row.names=FALSE)
micro_mortal_dedup <- #de-dulicating based on hn and spcdate
  merge_mortal %>%
  dplyr::arrange(hn, DateSpc) %>% # order data by hospital number (patient identifier), and sample collection date
  dplyr::group_by(hn) %>%
  dplyr::mutate(inanalysis=row_number()) %>%
  dplyr::filter(inanalysis==1)
#write.csv(micro_mortal_dedup, file="./micro_mortal_dedup.csv", row.names=FALSE)
#Preparing column : total patients
merge_mortal_count <- micro_mortal_dedup %>%
  dplyr::group_by(organismplus) %>%
  dplyr::count(disoutcome_cat)

merge_mortal_total <- merge_mortal_count
colnames(merge_mortal_total) <- c('organismplus','disoutcome_cat','total')
merge_mortal_total <- aggregate(merge_mortal_total$total, by=list(merge_mortal_total$organismplus), FUN=sum)
colnames(merge_mortal_total) <- c('organismplus','total')

#Preparing column : dead patients
merge_mortal_dead <- merge_mortal_count[merge_mortal_count$disoutcome_cat=='died',]
colnames(merge_mortal_dead) <- c('organismplus','disoutcome_cat','dead')
merge_mortal_dead <- merge_mortal_dead[c('organismplus','dead')]

##Creating for summary table of P.40 (Preparing)
merge_mortal_tab <- setNames(data.frame(matrix(ncol = 4, nrow = length(orgName))),c('organismplus','mortality','lower','upper'))
merge_mortal_tab$organismplus <- orgName
merge_mortal_tab$num <- c(1:nrow(merge_mortal_tab))

##Merging tables and calculating values in summary table 
merge_mortal_2 <- merge(merge_mortal_tab,merge_mortal_total,all.x = TRUE) #Merging merged and blank summary tables
merge_mortal_2 <- merge(merge_mortal_2,merge_mortal_dead,by ='organismplus',all.x = TRUE) #Merging total and dead tables

merge_mortal_2[is.na(merge_mortal_2)] <- 0
merge_mortal_2$mortality <- round(merge_mortal_2$dead*100/merge_mortal_2$total ,0) #Calculating %mortality
merge_mortal_2$lower <- fun_wilson_lowerCI(x=merge_mortal_2$dead, n=merge_mortal_2$total, conflevel=0.95, decimalplace=2) #lower 95% confident interval
merge_mortal_2$upper <- fun_wilson_upperCI(x=merge_mortal_2$dead, n=merge_mortal_2$total, conflevel=0.95, decimalplace=2) #upper 95% confident interval
rownames(merge_mortal_2) <- merge_mortal_2$organismplus
merge_mortal_2 <- merge_mortal_2[order(merge_mortal_2$num, decreasing = FALSE),]
merge_mortal_2 = subset(merge_mortal_2, select = -c(num))
colnames(merge_mortal_2) <- c('organism','mortality','lower','upper','total','dead')
merge_mortal_2$organism <- fun_firstCapital(merge_mortal_2$organism)

##check point: AMASSplus
checkpoint6_Report7 = checkpoint_micro_data_ava
checkpoint7_Report7 = checkpoint_hosp_adm_data_ava
# End of AMASS plus part 


#### PART4 : Exporting results in CSV format ####
# Aggregated data to be exported in csv: Overall ####
#Report1_page3_results.csv
Type_of_data_file <- c("microbiology_data", "microbiology_data", "microbiology_data",
                       "microbiology_data", "microbiology_data", "microbiology_data",
                       "microbiology_data", "microbiology_data", "microbiology_data",
                       "hospital_admission_data", "hospital_admission_data", "hospital_admission_data")
Parameters <- c("Hospital_name", "Country", "Contact_person", 
                "Contact_address", "Contact_email", "notes_on_the_cover",
                "Number_of_records", "Minimum_date", "Maximum_date",
                "Number_of_records", "Minimum_date", "Maximum_date")
Values <- c(hospital_name, country, contact_person, 
            contact_address, contact_email, notes_on_the_cover,
            num_micro_data, min_rawmicro_spcdate, max_rawmicro_spcdate, 
            num_hosp_data, min_rawhos_admdate, max_rawhos_admdate)
write.csv(data.frame(Type_of_data_file, Parameters, Values), file="./ResultData/Report1_page3_results.csv", row.names=FALSE)
#Report2_page5_results.csv
Type_of_data_file <- c("microbiology_data", "microbiology_data", "microbiology_data",
                       "microbiology_data", "microbiology_data", "microbiology_data")
Parameters <- c("Minimum_date", "Maximum_date", "Number_of_blood_specimens_collected",
                "Number_of_blood_culture_negative", "Number_of_blood_culture_positive", "Number_of_blood_culture_positive_for_organism_under_this_survey")
Values <- c(min_rawmicro_spcdate, max_rawmicro_spcdate, count_data_daterange, 
            count_data_nogrowth, count_data_posgrowth, num_blood_org)
write.csv(data.frame(Type_of_data_file, Parameters, Values), file="./ResultData/Report2_page5_results.csv", row.names=FALSE)
#Report3_page12_results.csv
if (checkpoint2_Report3 == "yes"){
  Type_of_data_file <- c("microbiology_data", "microbiology_data", "merged_data",
                         "merged_data", "merged_data", "merged_data")
  Parameters <- c("Minimum_date", "Maximum_date", "Number_of_patients_with_blood_culture_positive_for_organism_under_this_survey",
                  "Number_of_patients_with_community_origin_BSI","Number_of_patients_with_hospital_origin_BSI","Number_of_patients_with_unknown_origin_BSI")
  Values <- c(min_rawmicro_spcdate, max_rawmicro_spcdate, Rpt3_pg2_totalpatients, 
              Rpt3_pg2_totalco, Rpt3_pg2_totalho, Rpt3_pg2_totalunk)
  write.csv(data.frame(Type_of_data_file, Parameters, Values), file="./ResultData/Report3_page12_results.csv", row.names=FALSE)
}
#Report4_page24_results.csv
if (checkpoint3_Report4_5_nogrowth == "yes"){
  Type_of_data_file <- c("merged_data", "merged_data", "merged_data", "merged_data")
  Parameters <- c("Minimum_date", "Maximum_date", "Number_of_blood_specimens_collected", "Number_of_patients_sampled_for_blood_culture")
  Values <- c(Rp4_min_date_data_include_fmt, Rp4_max_date_data_include_fmt, num_sampled_blood, num_patients_blood)
  write.csv(data.frame(Type_of_data_file, Parameters, Values), file="./ResultData/Report4_page24_results.csv", row.names=FALSE)
}
#Report5_page27_results.csv
if (checkpoint_hosp_adm_data_ava == "yes" & checkpoint3_Report4_5_nogrowth == "yes"){
  Type_of_data_file <- c("merged_data", "merged_data", "merged_data",
                         "merged_data", "merged_data", "merged_data", 
                         "merged_data", "merged_data")
  Parameters <- c("Minimum_date", "Maximum_date", "Number_of_blood_specimens_collected",
                  "Number_of_patients_sampled_for_blood_culture","Number_of_patients_with_blood_culture_within_first_2_days_of_admission", "Number_of_patients_with_blood_culture_within_after_2_days_of_admission", 
                  "Number_of_patients_with_unknown_origin", "Number_of_patients_had_more_than_one_admission")
  Values <- c(Rp4_min_date_data_include_fmt, Rp4_max_date_data_include_fmt, num_sampled_blood,
              num_patients_blood, num_blood_dedup_ca, num_blood_dedup_ha, 
              Rpt5_pg1_totalunk, num_blood_dedup_more_than_one_admissions)
  write.csv(data.frame(Type_of_data_file, Parameters, Values), file="./ResultData/Report5_page27_results.csv", row.names=FALSE)
}
#Report6_page32_results.csv
if (checkpoint5_Report6 == "yes"){
  perc_mortal <- paste(cfr, "%", " (",count_deaths_hospadm,"/", count_patient_hospadm,")", sep = "")
  Type_of_data_file <- c("microbiology_data", "microbiology_data", "merged_data",
                         "merged_data", "merged_data", "hospital_admission_data", 
                         "hospital_admission_data", "merged_data", "merged_data", 
                         "merged_data", "merged_data")
  Parameters <- c("Minimum_date", "Maximum_date", "Number_of_blood_culture_positive_for_organism_under_this_survey", 
                  "Number_of_patients_sampled_for_blood_culture", "Number_of_patients_with_community_origin_BSI", "Minimum_date", 
                  "Maximum_date", "Number_of_records", "Number_of_patients_included", 
                  "Number_of_deaths", "Mortality")
  Values <- c(min_rawmicro_spcdate, max_rawmicro_spcdate, Rpt3_pg2_totalpatients, 
              Rpt3_pg2_totalco, Rpt3_pg2_totalho, min_rawhos_admdate, 
              max_rawhos_admdate, Rpt6_no_raw_hosp, count_patient_hospadm, 
              count_deaths_hospadm, perc_mortal)
  write.csv(data.frame(Type_of_data_file, Parameters, Values), file="./ResultData/Report6_page32_results.csv", row.names=FALSE)
}
#AnnexA_page39_results.csv
Type_of_data_file <- c("microbiology_data", "microbiology_data", "microbiology_data",
                       "microbiology_data", "microbiology_data", "microbiology_data", 
                       "microbiology_data", "microbiology_data", "microbiology_data", "microbiology_data")
Parameters <- c("Minimum_date", "Maximum_date", "Number_of_all_culture_positive", 
                "Number_of_blood_culture_positive", "Number_of_csf_culture_positive", "Number_of_genital_swab_culture_positive", 
                "Number_of_rts_culture_positive", "Number_of_stool_culture_positive", "Number_of_urine_culture_positive", "Number_of_others_culture_positive")
Values <- c(min_rawmicro_spcdate, max_rawmicro_spcdate, nrow(micro_pos_org),
            nrow(micro_pos_org[micro_pos_org$spctypeplus=='blood',]), nrow(micro_pos_org[micro_pos_org$spctypeplus=='csf',]), nrow(micro_pos_org[micro_pos_org$spctypeplus=='genital swab',]), 
            nrow(micro_pos_org[micro_pos_org$spctypeplus=='rts',]), nrow(micro_pos_org[micro_pos_org$spctypeplus=='stool',]),nrow(micro_pos_org[micro_pos_org$spctypeplus=='urine',]), nrow(micro_pos_org[micro_pos_org$spctypeplus=='others',]))
write.csv(data.frame(Type_of_data_file, Parameters, Values), file="./ResultData/AnnexA_page39_results.csv", row.names=FALSE)


# Aggregated data to be exported in csv: Report 1 ####
if (checkpoint_hosp_adm_data_ava=="yes"){
  write.csv(exp_rpt1_1(MicroData2,HospData2), file="./ResultData/Report1_page4_counts_by_month.csv", row.names=FALSE)
}else{
  write.csv(exp_rpt1_2(MicroData2), file="./ResultData/Report1_page4_counts_by_month.csv", row.names=FALSE)
}

# Aggregated data to be exported in csv: Report 2 ####
write.csv(exp_rpt2_1(MicroData_bsi), file="./ResultData/Report2_page6_counts_by_organism.csv", row.names=FALSE)
write.csv(exp_rpt2_2(blood_dedup_sa,blood_dedup_es,blood_dedup_sp,blood_dedup_ss,blood_dedup_ec,blood_dedup_kp,blood_dedup_pa,blood_dedup_as),
          file="./ResultData/Report2_page6_patients_under_this_surveillance_by_organism.csv", row.names=FALSE)
## Tables in Report 2
write.csv(exp_rpt2_3(isoRep_blood_stapha_graph,isoRep_blood_enterococcus_graph,
                     isoRep_blood_streptopneu_graph,isoRep_blood_salmonella_graph,
                     isoRep_blood_ecoli_graph,isoRep_blood_klebp_graph,
                     isoRep_blood_pseudoa_graph,isoRep_blood_acine_graph), 
          file="./ResultData/Report2_AMR_proportion_table.csv", row.names=FALSE)

# Aggregated data to be exported in csv: Report 3 ####
write.csv(exp_rpt3_1(blood_dedup_sa,blood_dedup_es,blood_dedup_sp,blood_dedup_ss,blood_dedup_ec,blood_dedup_kp,blood_dedup_pa,blood_dedup_as,
                     merged_blood_dedup_sa,merged_blood_dedup_es,merged_blood_dedup_sp,merged_blood_dedup_ss,
                     merged_blood_dedup_ec,merged_blood_dedup_kp,merged_blood_dedup_pa,merged_blood_dedup_as), 
          file="./ResultData/Report3_page13_counts_by_origin.csv", row.names=FALSE)
## Tables in Report 3
if (checkpoint2_Report3=="yes"){
  write.csv(exp_rpt3_2(co_extRep_blood_stapha_graph,co_extRep_blood_enterococcus_graph,co_extRep_blood_streptopneu_graph,co_extRep_blood_salmonella_graph,
                       co_extRep_blood_ecoli_graph,co_extRep_blood_klebp_graph,co_extRep_blood_pseudoa_graph,co_extRep_blood_acines_graph,
                       ho_extRep_blood_stapha_graph,ho_extRep_blood_enterococcus_graph,ho_extRep_blood_streptopneu_graph,ho_extRep_blood_salmonella_graph,
                       ho_extRep_blood_ecoli_graph,ho_extRep_blood_klebp_graph,ho_extRep_blood_pseudoa_graph,ho_extRep_blood_acines_graph), 
            file="./ResultData/Report3_table.csv", row.names=FALSE)
}else{}

# Aggregated data to be exported in csv: Report 4 ####
if (checkpoint3_Report4_5_nogrowth=="yes"){
  ## Frequency of organisms under survey per 100,000 tested patients
  write.csv(exp_rpt4_1(incidence_blood), file="./ResultData/Report4_frequency_blood_samples.csv", row.names=FALSE)
  ## Frequency of priority pathogens under survey per 100,000 tested patients
  write.csv(exp_rpt4_2(incidence_blood_antibiotic), file="./ResultData/Report4_frequency_priority_pathogen.csv", row.names=FALSE)
}else{}

# Aggregated data to be exported in csv: Report 5 ####
if (checkpoint_hosp_adm_data_ava=="yes" & checkpoint3_Report4_5_nogrowth=="yes"){
  ## Community-origin: Frequency of organisms under survey per 100,000 tested patients
  write.csv(exp_rpt5_1(incidence_blood_co), file="./ResultData/Report5_incidence_blood_samples_community_origin.csv", row.names=FALSE)
  ## Community-origin: Frequency of priority pathogens under survey per 100,000 tested patients
  write.csv(exp_rpt5_2(incidence_blood_antibiotic_co), file="./ResultData/Report5_incidence_blood_samples_community_origin_antibiotic.csv", row.names=FALSE)
  ## Hospital-origin: Frequency of priority pathogens under survey per 100,000 tested patients
  write.csv(exp_rpt5_3(incidence_blood_ho), file="./ResultData/Report5_incidence_blood_samples_hospital_origin.csv", row.names=FALSE)
  ## Hospital-origin: Frequency of priority pathogens under survey per 100,000 tested patients
  write.csv(exp_rpt5_4(incidence_blood_antibiotic_ho), file="./ResultData/Report5_incidence_blood_samples_hospital_origin_antibiotic.csv", row.names=FALSE)
}else{}

# Aggregated data to be exported in csv: Report 6 ####
rpt6 <- exp_rpt6(co_extRep_blood_sa_deathgraph,co_extRep_blood_es_deathgraph,co_extRep_blood_sp_deathgraph,co_extRep_blood_ss_deathgraph,
                 co_extRep_blood_ec_deathgraph,co_extRep_blood_kp_deathgraph,co_extRep_blood_pa_deathgraph,co_extRep_blood_as_deathgraph)
Number_of_deaths <- c(nrow(co_extRep_blood_stapha[which(co_extRep_blood_stapha$ASTmrsa == "1" & co_extRep_blood_stapha$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_stapha[which(co_extRep_blood_stapha$ASTmrsa == "0" & co_extRep_blood_stapha$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_enterococcus[which(co_extRep_blood_enterococcus$ASTVancomycin == "1" & co_extRep_blood_enterococcus$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_enterococcus[which(co_extRep_blood_enterococcus$ASTVancomycin == "0" & co_extRep_blood_enterococcus$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_streptopneu[which(co_extRep_blood_streptopneu$ASTPenicillin_G == "1" & co_extRep_blood_streptopneu$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_streptopneu[which(co_extRep_blood_streptopneu$ASTPenicillin_G == "0" & co_extRep_blood_streptopneu$disoutcome_cat == "died"),]), 
                      nrow(co_extRep_blood_salmonella[which(co_extRep_blood_salmonella$ASTFluoroquin == "1" & co_extRep_blood_salmonella$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_salmonella[which(co_extRep_blood_salmonella$ASTFluoroquin == "0" & co_extRep_blood_salmonella$disoutcome_cat == "died"),]), 
                      nrow(co_extRep_blood_ecoli[which(co_extRep_blood_ecoli$ASTCarbapenem == "1" & co_extRep_blood_ecoli$disoutcome_cat == "died"),]), #Carb-NS
                      nrow(co_extRep_blood_ecoli[which(co_extRep_blood_ecoli$AST3gcsCarbs == "2" & co_extRep_blood_ecoli$disoutcome_cat == "died"),]), #3GC-NS&(Carb-S|no Carb)
                      nrow(co_extRep_blood_ecoli[which(co_extRep_blood_ecoli$AST3gcsCarbs == "1" & co_extRep_blood_ecoli$disoutcome_cat == "died"),]), #3GC-S&(Carb-S|no Carb)
                      nrow(co_extRep_blood_klebp[which(co_extRep_blood_klebp$ASTCarbapenem == "1" & co_extRep_blood_klebp$disoutcome_cat == "died"),]), #Carb-NS
                      nrow(co_extRep_blood_klebp[which(co_extRep_blood_klebp$AST3gcsCarbs == "2" & co_extRep_blood_klebp$disoutcome_cat == "died"),]), #3GC-NS&(Carb-S|no Carb)
                      nrow(co_extRep_blood_klebp[which(co_extRep_blood_klebp$AST3gcsCarbs == "1" & co_extRep_blood_klebp$disoutcome_cat == "died"),]), #3GC-S&(Carb-S|no Carb)
                      nrow(co_extRep_blood_pseudoa[which(co_extRep_blood_pseudoa$ASTCarbapenem == "1" & co_extRep_blood_pseudoa$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_pseudoa[which(co_extRep_blood_pseudoa$ASTCarbapenem == "0" & co_extRep_blood_pseudoa$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_acines[which(co_extRep_blood_acines$ASTCarbapenem == "1" & co_extRep_blood_acines$disoutcome_cat == "died"),]),
                      nrow(co_extRep_blood_acines[which(co_extRep_blood_acines$ASTCarbapenem == "0" & co_extRep_blood_acines$disoutcome_cat == "died"),]), 
                      nrow(ho_extRep_blood_stapha[which(ho_extRep_blood_stapha$ASTmrsa == "1" & ho_extRep_blood_stapha$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_stapha[which(ho_extRep_blood_stapha$ASTmrsa == "0" & ho_extRep_blood_stapha$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_enterococcus[which(ho_extRep_blood_enterococcus$ASTVancomycin == "1" & ho_extRep_blood_enterococcus$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_enterococcus[which(ho_extRep_blood_enterococcus$ASTVancomycin == "0" & ho_extRep_blood_enterococcus$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_streptopneu[which(ho_extRep_blood_streptopneu$ASTPenicillin_G == "1" & ho_extRep_blood_streptopneu$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_streptopneu[which(ho_extRep_blood_streptopneu$ASTPenicillin_G == "0" & ho_extRep_blood_streptopneu$disoutcome_cat == "died"),]), 
                      nrow(ho_extRep_blood_salmonella[which(ho_extRep_blood_salmonella$ASTFluoroquin == "1" & ho_extRep_blood_salmonella$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_salmonella[which(ho_extRep_blood_salmonella$ASTFluoroquin == "0" & ho_extRep_blood_salmonella$disoutcome_cat == "died"),]), 
                      nrow(ho_extRep_blood_ecoli[which(ho_extRep_blood_ecoli$ASTCarbapenem == "1" & ho_extRep_blood_ecoli$disoutcome_cat == "died"),]), #Carb-NS
                      nrow(ho_extRep_blood_ecoli[which(ho_extRep_blood_ecoli$AST3gcsCarbs == "2" & ho_extRep_blood_ecoli$disoutcome_cat == "died"),]), #3GC-NS&(Carb-S|no Carb)
                      nrow(ho_extRep_blood_ecoli[which(ho_extRep_blood_ecoli$AST3gcsCarbs == "1" & ho_extRep_blood_ecoli$disoutcome_cat == "died"),]), #3GC-S&(Carb-S|no Carb)
                      nrow(ho_extRep_blood_klebp[which(ho_extRep_blood_klebp$ASTCarbapenem == "1" & ho_extRep_blood_klebp$disoutcome_cat == "died"),]), #Carb-NS
                      nrow(ho_extRep_blood_klebp[which(ho_extRep_blood_klebp$AST3gcsCarbs == "2" & ho_extRep_blood_klebp$disoutcome_cat == "died"),]), #3GC-NS&(Carb-S|no Carb)
                      nrow(ho_extRep_blood_klebp[which(ho_extRep_blood_klebp$AST3gcsCarbs == "1" & ho_extRep_blood_klebp$disoutcome_cat == "died"),]), #3GC-S&(Carb-S|no Carb)
                      nrow(ho_extRep_blood_pseudoa[which(ho_extRep_blood_pseudoa$ASTCarbapenem == "1" & ho_extRep_blood_pseudoa$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_pseudoa[which(ho_extRep_blood_pseudoa$ASTCarbapenem == "0" & ho_extRep_blood_pseudoa$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_acines[which(ho_extRep_blood_acines$ASTCarbapenem == "1" & ho_extRep_blood_acines$disoutcome_cat == "died"),]),
                      nrow(ho_extRep_blood_acines[which(ho_extRep_blood_acines$ASTCarbapenem == "0" & ho_extRep_blood_acines$disoutcome_cat == "died"),]))
Total_number_of_patients <- c(nrow(co_extRep_blood_stapha[which(co_extRep_blood_stapha$ASTmrsa == "1"),]), 
                              nrow(co_extRep_blood_stapha[which(co_extRep_blood_stapha$ASTmrsa == "0"),]), 
                              nrow(co_extRep_blood_enterococcus[which(co_extRep_blood_enterococcus$ASTVancomycin == "1"),]),
                              nrow(co_extRep_blood_enterococcus[which(co_extRep_blood_enterococcus$ASTVancomycin == "0"),]),
                              nrow(co_extRep_blood_streptopneu[which(co_extRep_blood_streptopneu$ASTPenicillin_G == "1"),]),
                              nrow(co_extRep_blood_streptopneu[which(co_extRep_blood_streptopneu$ASTPenicillin_G == "0"),]), 
                              nrow(co_extRep_blood_salmonella[which(co_extRep_blood_salmonella$ASTFluoroquin == "1"),]),
                              nrow(co_extRep_blood_salmonella[which(co_extRep_blood_salmonella$ASTFluoroquin == "0"),]), 
                              nrow(co_extRep_blood_ecoli[which(co_extRep_blood_ecoli$ASTCarbapenem == "1"),]), #Carb-NS
                              nrow(co_extRep_blood_ecoli[which(co_extRep_blood_ecoli$AST3gcsCarbs == "2"),]), #3GC-NS&(Carb-S|no Carb)
                              nrow(co_extRep_blood_ecoli[which(co_extRep_blood_ecoli$AST3gcsCarbs == "1"),]), #3GC-S&(Carb-S|no Carb)
                              nrow(co_extRep_blood_klebp[which(co_extRep_blood_klebp$ASTCarbapenem == "1"),]), #Carb-NS
                              nrow(co_extRep_blood_klebp[which(co_extRep_blood_klebp$AST3gcsCarbs == "2"),]), #3GC-NS&(Carb-S|no Carb)
                              nrow(co_extRep_blood_klebp[which(co_extRep_blood_klebp$AST3gcsCarbs == "1"),]), #3GC-S&(Carb-S|no Carb)
                              nrow(co_extRep_blood_pseudoa[which(co_extRep_blood_pseudoa$ASTCarbapenem == "1"),]),
                              nrow(co_extRep_blood_pseudoa[which(co_extRep_blood_pseudoa$ASTCarbapenem == "0"),]),
                              nrow(co_extRep_blood_acines[which(co_extRep_blood_acines$ASTCarbapenem == "1"),]),
                              nrow(co_extRep_blood_acines[which(co_extRep_blood_acines$ASTCarbapenem == "0"),]), 
                              nrow(ho_extRep_blood_stapha[which(ho_extRep_blood_stapha$ASTmrsa == "1"),]),
                              nrow(ho_extRep_blood_stapha[which(ho_extRep_blood_stapha$ASTmrsa == "0"),]),
                              nrow(ho_extRep_blood_enterococcus[which(ho_extRep_blood_enterococcus$ASTVancomycin == "1"),]),
                              nrow(ho_extRep_blood_enterococcus[which(ho_extRep_blood_enterococcus$ASTVancomycin == "0"),]),
                              nrow(ho_extRep_blood_streptopneu[which(ho_extRep_blood_streptopneu$ASTPenicillin_G == "1"),]),
                              nrow(ho_extRep_blood_streptopneu[which(ho_extRep_blood_streptopneu$ASTPenicillin_G == "0"),]), 
                              nrow(ho_extRep_blood_salmonella[which(ho_extRep_blood_salmonella$ASTFluoroquin == "1"),]),
                              nrow(ho_extRep_blood_salmonella[which(ho_extRep_blood_salmonella$ASTFluoroquin == "0"),]), 
                              nrow(ho_extRep_blood_ecoli[which(ho_extRep_blood_ecoli$ASTCarbapenem == "1"),]), #Carb-NS
                              nrow(ho_extRep_blood_ecoli[which(ho_extRep_blood_ecoli$AST3gcsCarbs == "2"),]), #3GC-NS
                              nrow(ho_extRep_blood_ecoli[which(ho_extRep_blood_ecoli$AST3gcsCarbs == "1"),]), #3GC-S
                              nrow(ho_extRep_blood_klebp[which(ho_extRep_blood_klebp$ASTCarbapenem == "1"),]), #Carb-NS
                              nrow(ho_extRep_blood_klebp[which(ho_extRep_blood_klebp$AST3gcsCarbs == "2"),]), #3GC-NS
                              nrow(ho_extRep_blood_klebp[which(ho_extRep_blood_klebp$AST3gcsCarbs == "1"),]), #3GC-S
                              nrow(ho_extRep_blood_pseudoa[which(ho_extRep_blood_pseudoa$ASTCarbapenem == "1"),]),
                              nrow(ho_extRep_blood_pseudoa[which(ho_extRep_blood_pseudoa$ASTCarbapenem == "0"),]),
                              nrow(ho_extRep_blood_acines[which(ho_extRep_blood_acines$ASTCarbapenem == "1"),]),
                              nrow(ho_extRep_blood_acines[which(ho_extRep_blood_acines$ASTCarbapenem == "0"),]))
write.csv(data.frame(rpt6,Number_of_deaths,Total_number_of_patients), file="./ResultData/Report6_mortality_table.csv", row.names=FALSE)

#P.39
micro_pos_dedup_csv <- micro_pos_dedup
colnames(micro_pos_dedup_csv) <- c('Organism','Total_number_of_patients', 'Number_of_patients_with_blood_positive_deduplicated',
                                   'Number_of_patients_with_csf_positive_deduplicated','Number_of_patients_with_genitLal_swab_positive_deduplicated',
                                   'Number_of_patients_with_rts_positive_deduplicated','Number_of_patients_with_stool_positive_deduplicated',
                                   'Number_of_patients_with_urine_positive_deduplicated','Number_of_patients_with_others_positive_deduplicated')
write.csv(micro_pos_dedup_csv, file="./ResultData/AnnexA_patients_with_positive_specimens.csv", row.names=FALSE)

#P.40
merge_mortal_csv <- merge_mortal_2
merge_mortal_csv <- subset(merge_mortal_csv, select = c('organism','dead','total','mortality','lower','upper') )
colnames(merge_mortal_csv) <- c('Organism','Number_of_deaths','Total_number_of_patients','Mortality(%)','Mortality_lower_95ci','Mortality_upper_95ci')
merge_mortal_csv[is.na(merge_mortal_csv)] <- 0
write.csv(merge_mortal_csv, file="./ResultData/AnnexA_mortlity_table.csv", row.names=FALSE)

# Data stratified by infection origin, gender, and age group ####
aggreg_table_sa <- fun_aggreg_table(merged_blood_dedup_sa, merged_blood_dedup_sa$ASTmrsa,
                                    "MSSA", "MRSA")
aggreg_table_es <- fun_aggreg_table(merged_blood_dedup_es, merged_blood_dedup_es$ASTVancomycin,
                                    "Vancomycin-S", "Vancomycin-NS")
aggreg_table_sp <- fun_aggreg_table(merged_blood_dedup_sp, merged_blood_dedup_sp$ASTPenicillin,
                                    "PenicillinG-S", "PenicillinG-NS")
aggreg_table_ss <- fun_aggreg_table(merged_blood_dedup_ss, merged_blood_dedup_ss$ASTFluoroquin,
                                    "Fluoroquinolone-S", "Fluoroquinolone-NS")
aggreg_table_pa <- fun_aggreg_table(merged_blood_dedup_pa, merged_blood_dedup_pa$ASTCarbapenem,
                                    "Carbapenem-S", "Carbapenem-NS")
aggreg_table_as <- fun_aggreg_table(merged_blood_dedup_as, merged_blood_dedup_as$ASTCarbapenem,
                                    "Carbapenem-S", "Carbapenem-NS")
aggreg_table_ec <- fun_aggreg_table2(merged_blood_dedup_ec)
aggreg_table_kp <- fun_aggreg_table2(merged_blood_dedup_kp)

write.csv(aggreg_table_sa, file="./ResultData/Summary_stratified_gender_age_origin_of_infection_sa.csv", row.names=FALSE)
write.csv(aggreg_table_es, file="./ResultData/Summary_stratified_gender_age_origin_of_infection_es.csv", row.names=FALSE)
write.csv(aggreg_table_sp, file="./ResultData/Summary_stratified_gender_age_origin_of_infection_sp.csv", row.names=FALSE)
write.csv(aggreg_table_ss, file="./ResultData/Summary_stratified_gender_age_origin_of_infection_ss.csv", row.names=FALSE)
write.csv(aggreg_table_ec, file="./ResultData/Summary_stratified_gender_age_origin_of_infection_ec.csv", row.names=FALSE)
write.csv(aggreg_table_kp, file="./ResultData/Summary_stratified_gender_age_origin_of_infection_kp.csv", row.names=FALSE)
write.csv(aggreg_table_pa, file="./ResultData/Summary_stratified_gender_age_origin_of_infection_pa.csv", row.names=FALSE)
write.csv(aggreg_table_as, file="./ResultData/Summary_stratified_gender_age_origin_of_infection_as.csv", row.names=FALSE)

# Remove variables that will not be used again ####
rm(list = ls(pattern = "^blood_dedup_"))
rm(list = ls(pattern = "^isoRep_blood_"))
rm(list = ls(pattern = "^table_"))
rm(list = ls(pattern = "^ho_"))
rm(list = ls(pattern = "^co_"))

# Data log file ####
#overall information
Type_of_data_file <- c("microbiology_data", "microbiology_data", "microbiology_data", "microbiology_data", 
                       "microbiology_data", "microbiology_data", "microbiology_data", "microbiology_data", 
                       "hospital_admission_data", "hospital_admission_data", "hospital_admission_data", "hospital_admission_data", "hospital_admission_data",
                       "merged_data", "merged_data", "merged_data", "merged_data", "merged_data", "merged_data")
Parameters <- c("Hospital_name", "Country","Minimum_date", "Maximum_date",
                "Number_of_missing_specimen_date", "Number_of_missing_specimen_type", "Number_of_missing_culture_result", "format_of_specimen_date", 
                "Number_of_missing_admission_date", "Number_of_missing_discharge_type", "Number_of_missing_outcome_result", "format_of_admission_date", "format_of_discharge_date", 
                "Number_of_missing_specimen_date", "Number_of_missing_admission_date", "Number_of_missing_discharge_type", "Number_of_missing_age", "Number_of_missing_gender", "Number_of_missing_infection_origin_data")
Values <- c()
if (checkpoint_hosp_adm_date_ava == "yes"){
  HospData2 <- fun_datevariable(HospData2, HospData2$date_of_discharge)
  colnames(HospData2)[ncol(HospData2)] <- "disdate2"
  HospData2$DateDis <- multidate(HospData2$disdate2)
  ### For when the date variable is in character and numeric format of excel i.e. xxxx
  if (sum(is.na(HospData2$DateDis)==TRUE)==nrow(HospData2)){
    HospData2$disdate2 <- as.numeric(HospData2$date_of_discharge)
    HospData2$DateDis <- as.Date(HospData2$disdate2, origin="1899-12-30")
  }else{}
  Values <- c(hospital_name, country,min_rawmicro_spcdate, max_rawmicro_spcdate,
              length(which(is.na(MicroData2$specimen_collection_date))), 
              length(which(is.na(MicroData2$spctype))), 
              length(which(is.na(MicroData2$organism))), 
              paste("[", format(MicroData2[1,"DateSpc"],datefmt_text), "]", "[", format(MicroData2[2,"DateSpc"],datefmt_text), "]"), 
              length(which(is.na(HospData2$date_of_admission))), 
              length(which(is.na(HospData2$date_of_discharge))), 
              length(which(is.na(HospData2$discharge_status))), 
              paste("[", format(HospData2[1,"DateAdm"],datefmt_text), "]", "[", format(HospData2[2,"DateAdm"],datefmt_text), "]"),
              paste("[", format(HospData2[1,"DateDis"],datefmt_text), "]", "[", format(HospData2[2,"DateDis"],datefmt_text), "]"), 
              length(which(is.na(raw_HospMicroData_bsi$DateSpc))), 
              length(which(is.na(raw_HospMicroData_bsi$DateAdm))), 
              length(which(is.na(raw_HospMicroData_bsi$DateDis))), 
              length(which(is.na(raw_HospMicroData_bsi$age_year))), 
              length(which(is.na(raw_HospMicroData_bsi$gender))), 
              length(which(is.na(raw_HospMicroData_bsi$InfOri_hosp1))))
}else{
  Values <- c(hospital_name, country,min_rawmicro_spcdate, max_rawmicro_spcdate,
              length(which(is.na(MicroData2$specimen_collection_date))), 
              length(which(is.na(MicroData2$spctype))), 
              length(which(is.na(MicroData2$organism))), 
              paste("[", format(MicroData2[1,"DateSpc"],datefmt_text), "]", "[", format(MicroData2[2,"DateSpc"],datefmt_text), "]"), 
              "NA", 
              "NA", 
              "NA", 
              paste("[", "NA", "]", "[", "NA", "]"),
              paste("[", "NA", "]", "[", "NA", "]"), 
              length(which(is.na(raw_HospMicroData_bsi$DateSpc))), 
              length(which(is.na(raw_HospMicroData_bsi$DateAdm))), 
              "NA", 
              "NA", 
              "NA", 
              "NA")
}
logfile_results <- data.frame(Type_of_data_file, Parameters, Values)
write.csv(logfile_results, file="./ResultData/logfile_results.csv", row.names=FALSE)
# List of organisms
table_organism <- as.data.frame(table(MicroData$organism, exclude = NULL))
colnames(table_organism) <- c("Organism", "Frequency")
write_xlsx(table_organism, "./ResultData/logfile_organism.xlsx")
#write.csv(table_organism, file="./ResultData/logfile_organism.csv", row.names=FALSE)
# List of specimens
table_specimen <- as.data.frame(table(MicroData$specimen_type, exclude = NULL))
colnames(table_specimen) <- c("Specimen", "Frequency")
write_xlsx(table_specimen, "./ResultData/logfile_specimen.xlsx")
#write.csv(table_specimen, file="./ResultData/logfile_specimen.csv", row.names=FALSE)
# Gender - raw
table_gender <- as.data.frame(table(log_raw_hosp$gender, exclude = NULL))
colnames(table_gender) <- c("Gender", "Frequency")
write_xlsx(table_gender, "./ResultData/logfile_gender.xlsx")
#write.csv(table_gender, file="./ResultData/logfile_gender.csv", row.names=FALSE)
# Age - raw
# calculate age in year from birthday
if(avai_age_year=="yes"){
  log_raw_hosp$YearAge2 <- as.numeric(log_raw_hosp$age_year)
}else{
  if (avai_birthday=="yes") {
    # Clean Date variable ####
    ######## Admission date
    log_raw_hosp <- fun_datevariable(log_raw_hosp, log_raw_hosp$date_of_admission)
    colnames(log_raw_hosp)[ncol(log_raw_hosp)] <- "admdate2"
    log_raw_hosp$DateAdm <- multidate(log_raw_hosp$admdate2)
    ### For when the date variable is in character and numeric format of excel i.e. xxxx
    if (sum(is.na(log_raw_hosp$DateAdm)==TRUE)==nrow(log_raw_hosp)){
      log_raw_hosp$admdate2 <- as.numeric(log_raw_hosp$date_of_admission)
      log_raw_hosp$DateAdm <- as.Date(log_raw_hosp$admdate2, origin="1899-12-30")
    }else{}
    
    ######## birthday date
    avai_birthday <- datadict[which(datadict[,1]=="birthday_available"),2]
    if(avai_birthday=="yes"){
      log_raw_hosp <- fun_datevariable(log_raw_hosp, log_raw_hosp$birthday)
      colnames(log_raw_hosp)[ncol(log_raw_hosp)] <- "bdate2"
      log_raw_hosp$DateBirth <- multidate(log_raw_hosp$bdate2)
      ### For when the date variable is in character and numeric format of excel i.e. xxxx
      if (sum(is.na(log_raw_hosp$DateBirth)==TRUE)==nrow(log_raw_hosp)){
        log_raw_hosp$bdate2 <- as.numeric(log_raw_hosp$birthday)
        log_raw_hosp$DateBirth <- as.Date(log_raw_hosp$bdate2, origin="1899-12-30")
      }else{}
    }else{}
    
    # Assigning age to column YearAge2
    log_raw_hosp$YearAge1 <- as.numeric(log_raw_hosp$DateAdm)-as.numeric(log_raw_hosp$DateBirth)
    log_raw_hosp$YearAge2 <- floor(log_raw_hosp$YearAge1/365.25)
    log_raw_hosp$YearAge1 <- NULL
  }
  else {
    log_raw_hosp$YearAge2 <- NULL
  }
}
# categorise age into 10 groups
log_raw_hosp$YearAge_cat <- cut(log_raw_hosp$YearAge2,
                                     breaks=c(0,1,5,15,25,35,45,55,65,81,200),
                                     labels=c("Less than 1 year" ,"1 to 4 years", "5 to 14 years", "15 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years", "55 to 64 years", "65 to 80 years", "More than 80 years"),
                                     right=FALSE)

table_raw_age <- as.data.frame(table(log_raw_hosp$YearAge2, exclude = NULL))
colnames(table_raw_age) <- c("Age", "Frequency")
write_xlsx(table_raw_age, "./ResultData/logfile_age.xlsx")
#write.csv(table_raw_age, file="./ResultData/logfile_age.csv", row.names=FALSE)
# Discharge - raw
table_discharge <- as.data.frame(table(log_raw_hosp$discharge_status, exclude = NULL))
colnames(table_discharge) <- c("Discharge status", "Frequency")
write_xlsx(table_discharge, "./ResultData/logfile_discharge.xlsx")
#write.csv(table_discharge, file="./ResultData/logfile_discharge.csv", row.names=FALSE)
#AST - raw
log_ast_raw_final <- setNames(data.frame(matrix(ncol=2)),c("Antibiotics","frequency_raw"))
log_ast_raw[is.na(log_ast_raw)] <- "No data"
for (i in colnames(log_ast_raw)) {
  log_ast_raw_sel <- as.data.frame(table(log_ast_raw[,i]))
  log_ast_raw_count <- log_ast_raw_sel[log_ast_raw_sel$Var1!="No data","Freq"]
  log_ast_raw_final[i,"frequency_raw"] <- sum(log_ast_raw_count)
}
log_ast_raw_final$Antibiotics <- row.names(log_ast_raw_final)
write_xlsx(log_ast_raw_final, "./ResultData/logfile_ast.xlsx")
#write.csv(log_ast_raw_final, file="./ResultData/logfile_ast.csv", row.names=TRUE)

sink()
