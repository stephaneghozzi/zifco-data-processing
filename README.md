# Standardization, cleaning and export for NAKO of questionnaires and labor data of the project ZIFCO

This folder contains the files needed to clean and process ZIFCO data and to export them in files that can be contributed to the NAKO. A report, including detailed documentation of the processing pipeline, can be generated as well. A public version of the report is included: [data-processing-public.html](data-processing-public.html).

## Data processing pipeline

By default: To process the and export the data, download this repository, by default in a location where confidential can be stored. The raw datasets are expected to be available as RDS files in the folder [data/data-raw-rds/](data/data-raw-rds/) within this repository. 

Then you can execute the script [pipeline.R](pipeline.R). (We recommend opening first the RStudio project [data-processing.Rproj](data-processing.Rproj) and running the script from there.) It will read the raw data and PIA code book and save the processed datasets as well as export them in a format fulfilling NAKO's requirements.

As detailed in the [report](data-processing-public.html), all paths can be changed and set in the file [config.yml](config.yml).

## Generation of public and confidential reports

The report provided here is the public version: it doesn't contain any person-related data. To generate the full, confidential report: open [data-processing.Rmd](data-processing.Rmd), change the variable 
`report_is_confidential` to `TRUE`.

By default, it is expected that the processing pipeline has been executed before.

## Updated raw datasets

When raw data are updated, the pipeline has to be executed again and the raw data have to be read from the initial files (not the RDS files). To do this change [config.yml](config.yml) by setting `read_native` to `false` before running the pipeline. 

It is recommended to set `save_native` to `true` also: this saves those raw data as RDS which is much faster to read and use in R than the initial raw data files.

## Explore NAKO export

You can execute [sample-nako-export.R](sample-nako-export.R) to generate samples of the datasets exported for the NAKO. This can allow one to explore individual datasets better than the sometimes large (and for Excel too large) full export. Parameters such as sample size and number of samples generated can be set in the script.

## Update NAKO metadata

You can execute [update-nako-metadata.R](update-nako-metadata.R) to update the variable and names and descriptions as well as dataset descriptions for the NAKO without having to re-run the whole pipeline. This requires the pipeline has been run before and its output has been stored.

## Folder content

- the folder [documents/](documents/) contains references useful for the project
- the folder [R/](R/) contains the R scripts with all functions needed for processing and export of the data sets, as well as the performing of various checks
- [config.yml](config.yml) is the configuration file for data processing, see details in [data-processing-public.html](data-processing-public.html); it is a text-based format can be opened and edited with any text processing editor, it can also be viewed in any internet browser
- [data-processing-public.html](data-processing-public.html) is a report documenting the data processing pipeline and presenting the results; it doesn't include previews of data relating to individual participants
- [data-processing.Rmd](data-processing.Rmd) is the R source code to generate the reports
- [data-processing.Rproj](data-processing.Rproj) is the RStudio project used for development and execution of the R scripts
- [issues.md](issues.md) is a list of issues (possible improvements) for this project
- [LCIENSE](LCIENSE) the license of the project
- [pipeline.R](pipeline.R) is the R script containing the various functions called to load, process and export ZIFCO data, as well as perform various checks
- [README.md](README.md) is this document
- [sample-nako-export.R](sample-nako-export.R) is a script that reads the processing pipeline output and generates samples of the NAKO exports of the questionnaires
- [update-nako-metadata.R](update-nako-metadata.R) is a script that runs the end of the processing pipeline, e.g., to only change the description of variables and data sets the NAKO metadata exported

## License

The code is provided under an [MIT License](LICENSE).