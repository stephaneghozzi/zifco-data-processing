Code:
- make package
- use the package targets
- write tests
- organize package in modules: "NAKO generic", "PIA generic", "ZIFCO [this project] specific"
- refactor, especially standardization of variable names
- harmonize input/output code for data and meta data (map+do.call vs. for)	
- add test: in NAKO export, in columns for missings, only `null` or `-1`
- better integrate sampling of NAKO export in pipeline: 
	- put its options in the config file
	- make it one function
	- integrate it in NAKO export instead of re-loading data

Report:
- what data-set and variable names are used explicitly in code and cannot be changed in config
- print data with non-missing comments
- some plots
- dependencies to external libraries that change data (e.g., times and dates)
- separate report in 
	- automated processing/quality report, includes private data
	- automated technical description and requirements, no private data
	- manual interpretation and overview, no private data

Documentation:
- functions

Data processing:
- change NAKO metadata, variable names without re-processing the data
- produce have synthetic data to try and test stuff
- print in console and report when data has been modified (e.g., line break or semi-colon removed from text to avoid breaking CSV formatting)
- add a way to set the column order in the NAKO export
- define in config which fields of the code book are exported (but that might not be possible if filed-specific formatting or converting is required)

Other:
- proper coding of missing values for NAKO beyond "-1" for all
- config: add specific description for each PIA questionnaire
- export full dictionary of variable names at each step, including in NAKO export

