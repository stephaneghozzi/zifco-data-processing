This folder contains the files needed to clean and process ZIFCO data and to export them in files that can be contributed to the NAKO. Documentation is included as well. Details are described in "data-processing-confidential.html" and "data-processing-public.html". The original raw-data-set files are read from their original various folders on NEON, as set in "config.yml", see below. 

The files indicated with (*) are stored in the confidential folder on HZI's internal NEON: S:\PROJACTIVE\ZIFCO-NAKO-Daten\i.Vacc_Skripte\ZIFCO_cleaning\stephane-ghozzi\zifco-data-processing  

- the folder "documents" contains references useful for the project, currently only the PDF file detailing the requirements for contributing 

- (*) the folder "data" contains 
  - the folder "data-processed-rds": data sets at the end of processing in the R-native format RDS,  which convenient for importing in R and keeps variable types and structures; meant as basis for further analyses 
  - the folder "data-raw-rds": raw data in the format RDS
  - the folder "for-nako": meta-data and data sets for contribution to the NAKO repository
  - "pipeline-output.RData": the complete data generated at the end of the processing pipeline; this is an R workspace that can be readily imported in R; convenient mostly for generating the reports "data-processing-confidential.html" and "data-processing-public.html" without having to re-do the calculations
  
- the folder "R" contains the R scripts with all functions needed for processing and export of the data sets, as well as the performing of various checks

- "config.yml" is the configuration file for data processing, see details in "data-processing-public.html" or "data-processing-confidential.html"; it is a text-based format can be opened and edited with any text processing editor, it can also be viewed in any internet browser

- (*) "data-processing-confidential.html" is a documentation and report on the data processing and its results; it includes previews of data relating to individual participants

- "data-processing-public.html" is the same as "data-processing-confidential.html" but it only contains information that can be publicly shared, i.e., no data relating to individual participants

- "data-processing.Rmd" is the R source code to generate the documents "data-processing-public.html"

- "data-processing.Rproj" is the RStudio project used for development and execution of the R scripts

- "pipeline.R" is the R script containing the various functions called to load, process and export ZIFCO data, as well as perform various checks

- "README.txt" is this document

- "sample-nako-export.R" is a script that reads the processing pipeline output and generates samples of the NAKO exports of the questionnaires

