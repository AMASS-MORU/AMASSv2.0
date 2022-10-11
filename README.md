# AMASSv2.0
#################
# Project Title #
#################
AutoMated tool for Antimicrobial resistance Surveillance System version 2.0 (AMASSv2.0)

###############
# Description #
###############

AutoMated tool for Antimicrobial resistance Surveillance System version 2.0 (AMASSv2.0) was developed as an offline, open-access and easy-to-use application that allows a hospital to perform data analysis independently and generate isolate-based and sample-based surveillance reports stratified by infection origin from routinely collected electronic databases. AMASS performs data analysis and generates reports automatically. The application can be downloaded from https://www.amass.website

AMASSv2.0 application is an extension of AMASSplus that was released on 25 March 2021. 
AMASSv2.0 additionally generates two Annexes (Annex A: Supplementary report on notifiable bacterial infections and Annex B: Supplementary report on data indicators) in this report. The Annexes present summary reports without identifiers. Therefore, this AMR surveillance report can be made open-access, and shared with national and international organizations to support action plans on AMR.

AMASSv2.0 separately generates Supplementary data indictors report (in PDF and Excel formats) in a new folder "Report_with_patient_identifiers" to support users to check and validate records with notifiable bacteria, notifiable antibiotic-pathogen combinations, infrequent phenotypes or potential errors in the AST results at the local level. 
The identifiers listed include hospital number and specimen collection date. The files are generated in a separate folder "Report_with_patient_identifiers" so that it is clear that users should not share or transfer the Supplementary Data Indictors report (in PDF and Excel format) to any party outside of the hospital without data security management and confidential agreement.

The AMASSv2.0 separately generates data verification logfile report to support users to check and validate the raw microbiology data and hospital admission data.

The organisms included for Annex A are:
    - Burkholderia pseudomallei
    - Brucella spp.
    - Corynebacterium diphtheriae
    - Neisseria gonorrhoeae
    - Neisseria meningitidis
    - Salmonella enterica serotype paratyphi
    - Salmonella enterica serotype typhi 
    - Non-typhoidal Salmonella spp.
    - Shigella spp.
    - Streptococcus suis
    - Vibrio spp.

The organisms included for Annex B are provided in list_of_indicators.xlsx under Configuration folder. AMASSv2.0 provided the default set of indicators for checking and validating records with notifiable bacteria, notifiable antibiotic-pathogen combinations, infrequent phenotypes or potential errors in the AST results at the local level. Moreover, This application allows users to add, edit, or remove set of organisms and antibiotics in list_of_indicators.xlsx (under Configuration folder) based on user's interests.

###################
# Getting Started #
###################

### Requirements ###
- Computer with Microsoft Windows 7 or 10 
AMASS could work well in other Microsoft Windows and other operating systems. However, thorough testing and adjustment have not been performed in this beta version. 
- AMASSv2.0.zip package file
The AMASS application is to be downloaded from https://www.amass.website, and unzipped to generate an AMASS folder that could be stored under any folder in the computer. The AMASS folder contains 3 files (AMASS.bat, dictionary_for_microbiology_data.xlsx, and dictionary_for_hospital_admission_data.xlsx), and 5 folders (Configuration, Example_Dataset_1_WHONET, Example_Dataset_2, Example_Dataset_3_longformat, and Programs). 
- Microbiology data file (microbiology_data in .csv or .xlsx file format) 
The user needs to obtain microbiology data, and then copy & paste this data file into the same folder as the AMASS.bat file.
- Hospital admission data file (hospital_admission_data.csv), containing data of The If available, the user could obtain hospital admission data, and then copy & paste this data file into the same folder as the AMASS.bat file.

### The followings are not required: ###
- Internet to run AMASS application 
AMASS application will run offline. No data are transferred while the application is running and reports are being generated; the reports are in PDF format (do not contain any patient identifier) and can be shared under the user's jurisdiction. 
- R and Python
The download package (AMASSv2.0.zip) included R portable, Python portable and their libraries that the AMASS application requires. The user does not need to install any programme before using the AMASS. The user also does not have to uninstall R or Python if the computer already has the programme installed. The user does not need to know how to use R and Python.

### Installation ###
No installation is needed. Just unzip the file and copy & paste to any folder in your computer.

#########
# Notes #
#########
[1] Please ensure that the file names of microbiology data file (microbiology_data.csv) and the hospital admission data file (hospital_admission_data.csv) are identical to what is written here. Please make sure that all are lower-cases and using underscore "_" at each space. 
[2] Please ensure that both microbiology and the hospital admission data files have no empty rows before the row of the variable names (i.e. the variable names are the first rows in both files). 
[3] For the first run, user may need to fill the data dictionary files to make sure that the AMASS application understands your variable names and values.

AMASS uses a tier-based approach. In cases when only the microbiology data file with the results of culture positive samples is available, only section one and two would be generated for users. Section three would be generated only when data on admission date are available. This is because these data are required for the stratification by origin of infection. Section four would be generated only when data of specimens with culture negative (no microbial growth) are available in the microbiology data. This is because these are required for the sample-based approach. Section five would be generated only when both data of specimens with culture negative and admission date are available. Section six would be generated only when mortality data are available. 

Mortality was calculated from the number of in-hospital deaths (numerator) over the total number of patients with blood culture positive for the organism (denominator). Please note that this is the all-cause mortality calculated using the outcome data in the data file, and may not necessarily represent the mortality directly due to the infections.

#################################################
# How to run AMASS using the example data files #
#################################################
1. Copy 4 files in the folder ".../AMASSv2.0/Example_Dataset_2"
2. Paste 4 files in the folder "../AMASSv2.0" (the same folder as the AMASS application)
3. Double click "AMASS.bat"
4. Wait for about 1-3 minutes for the AMASS to run
5. Open and review the "AMR_surveillance_report.pdf" newly generated in the "../AMASSv2.0" folder
6. Open and read the "Supplementary_data_indicators_report.pdf" newly generated in the "../AMASSv2.0/Report_with_patient_identifiers" folder
7. Open and read the "Data_verification_logfile_report.pdf" newly generated in the "../AMASSv2.0" folder

#################################################################
# How to configure data dictionary files for your hospital data #
#################################################################
In cases when variable names in the microbiology and hospital admission data files were not the same as the one that AMASS used, the data dictionary files could be edited. The raw microbiology and hospital admission data files were to be left unchanged. The data dictionary files provided could be edited and re-used automatically when the microbiology and hospital admission data files were updated and the AMASS.bat were to be double-clicked again (i.e. the data dictionary files would allow the user to re-analyze data files without the need to adjust variable names and data value again every time).

For example:
If variable name for "hospital number" is written as "hn" in the raw data file, the user would need to add "hn" in the cell next to "hospital_number". If data value for blood specimens is defined by "Blood-Hemoculture" in the raw data file, then the user would need to add "Blood-Hemoculture" in the cell next to "blood_specimen".

Please see the "Explanation" column in the dictionary files, for more details.

#############################################
# How to run AMASS using your hospital data #
#############################################
1. Make sure that you are the hospital staff and can analyze hospital data based on your duty and responsibility. If your aim is to generate cumulative AST (antimicrobial susceptibility report) for your hospital for service, no ethical clearance is usually needed. Please consult your line manager if you are uncertain on whether permission to analyze hospital data is granted.�
2. Export microbiology laboratory data as .csv or .xlsx. Please refer to Example_Dataset_2 for an example on data format and the list of essential variables. If "no growth" data is not available, AMASS can still generate a basic report for you without the "no growth" data. [Please check the list of essential variables in the user manual]
3. Export admission data as .csv or .xlsx. Please refer to Example_Dataset_2 for an example on data format and the list of essential variables. If hospital admission data is not available, AMASS can still generate a basic report for you without this file. [Please check the list of essential variables in the user manual]
4. Check your variable names and data values in your microbiology_data file, and configure the data_dictionary_for_microbiology_data
5. Check your variable names and data values in your hospital_admission_data file, and configure the data_dictionary_for_hospita_admission_data
6. Put your files in the folder "../AMASSv2.0" (the same folder with AMASS application)
7. Rename your file names as hospital_admission_data and microbiology_data
8. Double click "AMASS.bat"
9. Wait for about 1-3 minutes for the AMASS to run
10. Open and read the "AMR_surveillance_report.pdf" newly generated in the "../AMASSv2.0" folder
11. Open and read the "Supplementary_data_indicators_report.pdf" newly generated in the "../AMASSv2.0/Report_with_patient_identifiers" folder
12. Open and read the "Data_verification_logfile_report.pdf" newly generated in the "../AMASSv2.0" folder

######################
# Investigation team #
######################
AMASS is being developed by Cherry Lim, Clare Ling, Elizabeth Ashley, Paul Turner, Rahul Batra, Rogier van Doorn, Soawapak Hinjoy, Sopon Iamsirithaworn, Susanna Dunachie, Tri Wangrangsimakul, Viriya Hantrakun, William Schilling, John Stelling, Jonathan Edgeworth, Guy Thwaites, Nicholas PJ Day, Ben Cooper and Direk Limmathurotskul.
AMASSv2.0 is being developed by Chalida Rangsiwutisak, Cherry Lim, Paul Tuner, John Stelling and Direk Limmathurotsakul.

For any query on AMASS, please contact:
Chalida Rangsiwutisak (chalida@tropmedes.ac),
Cherry Lim (cherry@tropmedres.ac), and 
Direk Limmathurotsakul (direk@tropmedres.ac)

###########
# License #
###########
AMASS is available under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0). The AMASS application can be downloaded at https://www.amass.website

###################
# Acknowledgement #
###################
AMASS version 1.0 was funded by the Wellcome Trust (grant no. 206736 and 101103). C.L. is funded by a Research Training Fellowship (grant no. 206736) and D.L. is funded by an Intermediate Training Fellowship (grant no. 101103) from the Wellcome Trust. 

AMASS version 2.0 was funded by the Wellcome Trust Institutional Translational Partnership Award- MORU.

Special thanks to Prapass Wannapinij, who has been providing an amazing support on testing AMASS, and who is the developer of AMASS website (https://www.amass.website). 

Special thanks to those who posted thoughts and codes on Stackflow, and the codes that inspired some of R codes used by AMASS. 
