# Instructions

To re-run the analyses, simply run the analysis.R file in the scripts folder using either RStudio or Rscript. Once the script has run, you can similarly knit the report.Rmd file in RStudio to generate the report.html file.

# Project structure

```
spring_map_preact_correlation/
├── figs/ - Plots created by the analysis.R file
├── output/ - Output data/csvs from the analysis.R file
├── queries/ - SQL query
├── scripts/ - Script for analysis
├── report.html - Final report of findings
├── report.Rmd - Script for generating final report
├── README.md
├── .gitignore
└── .here
```

The `.here` file defines the main project directory. When using the `here` library, this file helps R figure out where to look within the main project for reading/writing files and folders.
