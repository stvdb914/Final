# DSAA811-Final-Report
Assignment 2 of DSAA811, explaining the structure of the report

All files used to create this report are located in the src directory.

The data for the report is being housed in the data directory. 
There are four csv files in one zip file (Olympic_Atheletes.zip). 
  The zip file contains the original download of the data from Kaggle.
There in one csv file that is not in the zip file called country-coord.zip. 
  It is this file that is responsible for the geolocation of data to plot the 
  countries on a map

The four csv files are athlete_events_data_dictionary.csv 
  athlete_events.csv country_definitions_data_dictionary.csv country_definitions.csv

Once all the files are knitted into a pdf file the report will be located in the report directly
This goes for individually compiling each part. To make each section run independantly
you need to change the line in each code set

```{r child = 'src/00_loader.Rmd', Include = TRUE, eval = FALSE}
```

to be 

```{r child = 'src/00_loader.Rmd', Include = TRUE, eval = TRUE}
```

This needs to be changed back to the first one to complete the compile of the 
entire report.

Each of the src files can be knitted individually to create a pdf file. 
This speeds up the processing time when running scripts over generating the entire pdf
so I can focus on one section over another.

The images folder has the signature files and the UOW logo to make the title page.

This project uses the following

This was sourced from git to generate the apa standard for the bibliography citation_package: apa-annotated.csl

library(knitr) version 1.5 library(tinytex) version 0.56 library(tidyverse) version 2

I advise that you load this into your R as a project using the DSAA811-Preliminary-Report.Rproj 
as this will dynamically set up the paths for the output and input files.

The main report file is called DSAA811_Final.rmd.
There are many functions that are stored in functions.R and allow for many combinations 
of the Games type and Year for a deeper analysis.

There  are some other files that I have left in the repository due to a bug that occurred in Git 
Changing all my code to numbers on the day it was due.

99 Cutting_room_floor has been used to dump and store the typed statements from the preliminary report 
so that I had easy access to restore the files

testing.R and testing.Rmd allowed for testing in a less structured way to see how the 
queries respond in different environments and was handy during the testing stages of the report.