# establish and upload the working manuscript file
source(here::here("load_packages.R"))
here::i_am("sub-projects/restored-marsh-diversity/R/05_ms-actions.R")

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)
library(trackdown)

#### ---- FOLLOW THE INSTRUCTIONS BELOW TO AVOID OVERWRITING CHANGES ---- ####

## Before updating local manuscript file, make sure to accept any changes on 
## Google doc file 

## Download the Google Doc and overwrite changes to local. 
## !!!! Heed the warning about local changes being overwritten
trackdown::download_file(here::here("sub-projects/restored-marsh-diversity/doc/restored-marsh-biodiversity_ms.rmd"),
                         gpath = "saltmarsh-manuscripts")

## Update the markdown file and update the Google doc in drive.
## !!!! Heed the warning about remote changes being overwritten. 

trackdown::update_file(here::here("sub-projects/restored-marsh-diversity/doc/restored-marsh-biodiversity_ms.rmd"),
                       gpath = "saltmarsh-manuscripts",
                       # do you want to show code??
                       hide_code = FALSE)
