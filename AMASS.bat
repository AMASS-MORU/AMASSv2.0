@ECHO OFF

del ".\Report_with_patient_identifiers\*.xlsx"
del ".\Report_with_patient_identifiers\*.pdf"
del ".\ResultData\*.xlsx"
del ".\ResultData\*.csv"
del ".\Variables\*.csv"

del ".\AMR_surveillance_report.pdf"
del ".\microbiology_data_reformatted.xlsx"
del ".\data_verification_logfile_report.pdf"
del ".\error_log.txt"

rmdir ".\Report_with_patient_identifiers\"
rmdir ".\ResultData\"
rmdir ".\Variables\"
mkdir Report_with_patient_identifiers
mkdir ResultData
mkdir Variables

echo Start Preprocessing
.\Programs\Python-Portable\Portable_Python-3.7.9\App\Python\python.exe -W ignore .\Programs\AMASS_preprocess\AMASS_preprocess_version_2.py
.\Programs\Python-Portable\Portable_Python-3.7.9\App\Python\python.exe -W ignore .\Programs\AMASS_preprocess\AMASS_preprocess_whonet_version_2.py
echo Start AMR analysis
.\Programs\R-Portable\App\R-Portable\bin\x64\Rscript.exe .\Programs\AMASS_amr\AMASS_analysis_amr_version_2.R
echo Start Data indicator
.\Programs\Python-Portable\Portable_Python-3.7.9\App\Python\python.exe -W ignore .\Programs\AMASS_data\AMASS_analysis_data_version_2.py
echo Start generating "AMR surveillance report"
.\Programs\Python-Portable\Portable_Python-3.7.9\App\Python\python.exe -W ignore .\Programs\AMASS_amr\AMASS_report_amr_version_2.py
del ".\ResultData\*.png"
echo Start generating "Supplementary data indicators report"
.\Programs\Python-Portable\Portable_Python-3.7.9\App\Python\python.exe -W ignore .\Programs\AMASS_data\AMASS_report_data_version_2.py
echo Start generating "Data verificator logfile report"
.\Programs\Python-Portable\Portable_Python-3.7.9\App\Python\python.exe -W ignore .\Programs\AMASS_logfile\AMASS_logfile_version_2.py
.\Programs\Python-Portable\Portable_Python-3.7.9\App\Python\python.exe -W ignore .\Programs\AMASS_logfile\AMASS_logfile_err_version_2.py
del ".\ResultData\logfile_age.xlsx"
del ".\ResultData\logfile_ast.xlsx"
del ".\ResultData\logfile_discharge.xlsx"
del ".\ResultData\logfile_gender.xlsx"
del ".\ResultData\logfile_organism.xlsx"
del ".\ResultData\logfile_specimen.xlsx"
del ".\error_analysis*.txt"
del ".\error_report*.txt"
del ".\error_logfile_amass.txt"
del ".\error_preprocess.txt"
del ".\error_preprocess_whonet.txt"
del ".\Report_with_patient_identifiers\Report_with_patient_identifiers_annexA_withstatus.xlsx"
del ".\Report_with_patient_identifiers\Report_with_patient_identifiers_annexB_withstatus.xlsx"
echo Finish running AMASS