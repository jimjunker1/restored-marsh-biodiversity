here::i_am("code/01_load-data.R")
# load required project packages here
## script to load and munge data for Measurement of Biodiversity analysis of restored vs unrestored data.
if(!require(devtools)) install.packages('devtools') else{library(devtools)}
if(!require(pacman)) install.packages('pacman') else{library(pacman)}
if (!require("pairwiseAdonis")) install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
devtools::install_github("MoBiodiv/mobr")
# Install packages ----------------
pacman::p_load(here, styler, rmarkdown, readxl, tidyverse, rfishbase, taxize, mobr, vegan, parallel, usedist, gplots)
theme_set(theme_minimal())

# !++++    HELPER FUNCTIONS    ++++!#

'%ni%' <- Negate('%in%')

#'
#'@title read_excel_allsheets
#'@description This function reads all sheets of an excel .xls* file and returns them as a list
#'@import readxl
#'@param filename string; point to the .xls* filepath
#'@param tibble logical; should the function return sheets as tibbles? Default is TRUE
#'@returns a nested list of all excel sheets as tibble objects. 
#'

read_excel_allsheets <- function(filename, tibble = TRUE,...) {
  if (grepl(".csv", filename)) {
    x <- read.csv(filename, header = TRUE)
  } else {
    require(readxl)
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,...))
    if (!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
  }
  x
}
#' @description This function attempts to detect the format of lat-long coordinates
#'  and then standardizes them to decimal degrees.
#' @export numeric vector of converted decimal degrees

fix_latlong <- function(x, combined = FALSE, combined_sep = NULL) {
  require(dplyr)
  require(stringr)
  # function to convert dmd to
  f <- function(x) {
    x <- gsub("\\s", "", x)
    ifelse((substr(x, 1, 1) == "-"), as.numeric(paste0("-", as.numeric(substr(x, 2, 3)) + as.numeric(substring(x, 4)) / 60)),
           as.numeric(substr(x, 1, 2)) + as.numeric(substring(x, 3)) / 60
    )
  }
  
  # if the values are combined into a single cell, split them
  if (combined) {
    if (grepl("\\s| ", combined_sep) & length(grep("\\s", x)) > 1) {
      x_split <- stringr::str_extract_all(x, "(\\D\\d{2,3}\\s{1}\\d{2}.\\d+)")
      # x_lat = sapply(x_split, function(x) gsub("N", c(""), x))
      # x_latlong = sapply(x_lat, function(x) gsub("W","-",x))
      x_latlong <- lapply(x_split, function(a) stringr::str_replace_all(a, c("N", "W"), c("", "-")))
      x_f <- lapply(x_latlong, f)
      x_return <- lapply(x_f, function(a) paste(a, sep = " "))
      return(x_return)
    } else {
      x_split <- sapply(x, function(a) str_split(a, combined_sep))
    }
    
    x_spec_pres <- all(grepl("º|°", x_split))
    x_deg_char <- all(grepl("\\'|\\’|\"", x_split))
  }
  
  # detect the form of the lat-long
  if (all(is.numeric(x))) {
    if (all(abs(x) <= 180) & all(grepl("\\d{2,3}.\\d{1,}", x))) {
      return(x)
    }
  } else {
    x_spec_pres <- all(grepl("º|°", x))
    x_deg_char <- all(grepl("\\'|\\’|\"", x))
    if (x_spec_pres & x_deg_char) {
      x_spec_trim <- sapply(x, function(x) stringr::str_replace_all(x, c(`º` = " ", `°` = " ", "\\'" = "", "\\’" = "", "\"" = "")))
      x_long <- sapply(x_spec_trim, function(x) ifelse(grepl("W", x), paste0("-", x), x))
      x_latlong <- sapply(x_long, function(x) gsub("N|W", "", x))
      x_f <- sapply(x_latlong, f)
      return(unname(x_f))
    }
    #
    #
    # x_latlong = sapply(x, )
    # x_lat = sapply(x, function(x) gsub("N", c(""), x))
    # x_latlong = sapply(x_lat, function(x) gsub("W","-",x))
    # x_spec_trim = sapply(x_latlong, function(x) gsub("º|°","",x))
    # unname(x_spec_trim)
  }
}

#' @title diversity_clean_names
#' @description This function cleans the column names from the excel sheet data template to convert them to useR friendly
#'  column names in R. This function should only manipulate the column names for sample description, not species names.
#'  If non-standard column are included there is an option to include an additional list of column names and replacements,
#'  following tidyverse naming convention, `list(new_name = "old_name")`
#' @param x dataframe; the data file from template
#' @param additional named list;
#'
diversity_clean_cols <- function(x, additional = NULL, remove_additional = FALSE, ...) {
  "%ni%" <- Negate("%in%")
  # run checks on any additional columns provided.
  if (!is.null(additional)) {
    if (all(!is.list(additional) & !is.vector(additional, mode = "character"))) stop("Error: additional must be list or character vector. ")
    if (is.null(names(additional))) stop("Error: additional must be named list.")
    if (any(lapply(additional, lengths) > 1)) stop("Error: Additional replacement colnames must be length of 1. Lengths > 1 provided.")
  }
  
  # create clean names list
  
  standard_bad_names <- list(
    c("Marsh ID", "Site", "Site_id", "Marsh ID...1"),
    c("Trap_ID", "Plot ID", "Plot ID...2"),
    c("Plot size (m2)"),
    c("Lat (plot)", "Latitude"),
    c("Long (plot)", "Longitude"),
    c("Date (plot)", "Date", "Date (plot) 5-21,22, 23 2018"),
    "Sampling technique",
    c("Area of sampling gear (m2)", "Area of the sampling gear (m2)"),
    "Mesh size (mm)",
    "Sampling effort (min)",
    c("Sampling effort (hr)", "Sampling effort (hours)")
  )
  
  standard_clean_names <- list(
    "marsh_id",
    "plot_id",
    "plot_size_m2",
    "lat_dd",
    "long_dd",
    "date",
    "technique",
    "gear_area_m2",
    "mesh_size_mm",
    "effort_min",
    "effort_hr"
  )
  
  standard_keyval <- purrr::map2(standard_bad_names, standard_clean_names, function(x, y) setNames(rep(y, length(x)), nm = x)) %>%
    flatten() %>%
    unlist()
  
  if (!is.null(additional)) {
    if (is.list(additional)) {
      standard_keyval <- c(standard_keyval, unlist(additional))
    } else {
      standard_keyval <- c(standard_keyval, additional)
    }
  }
  
  # custom_rename = function(OG_name){
  #   renames <- standard_keyval
  #   renames[[OG_name]]
  # }
  y <- x %>% plyr::rename(replace = standard_keyval, warn_missing = TRUE)
  
  non_standard_names <- colnames(y)[colnames(y) %ni% names(standard_keyval)] %>% .[!grepl("species.*", ., ignore.case = TRUE)]
  
  if (remove_additional) {
    y <- select(-non_standard_names)
  } else if (!is.null(non_standard_names) & length(non_standard_names) >= 1) {
    message(cat("Message: some non-standard columns were present and not checked for compatibility. Column names were:", non_standard_names))
  }
  
  return(y)
}

recode_site_names <- function(x,...){
  old_site <- list(
    c("WPH1", "WHP1"),
    "WPH2",
    "PS7"
  )
  
  new_site <- list(
    "WPH1",
    "WPH02",
    "PS07"
  )
  
  site_recode <- purrr::map2(old_site, new_site, function(x, y) setNames(rep(y, length(x)), nm = x)) %>%
    flatten() %>%
    unlist()
  x %>% dplyr::mutate(marsh_id = recode(marsh_id, !!!site_recode))
}

restored_keyval = setNames(c("WPH01","WPH02","PS07","LHC","LHA","LHB"), nm = c(rep("Natural",4),rep('Restored',2)))
#!++++   END HELPER FUNCTIONS    ++++!#
# Load and clean data ---------------

# 1. Load the community matrices

## all final community matrices should be named "*comm-type/gear*2018_matfull"
## remove all the intermediate objects except the final community matrices and loaded functions

plants2018_matfull <- read_csv("./data/2018_NOAA-Restore-Plant-Data.csv")
microbes2018_0.2_matfull <- read_csv("./data/2018_NOAA-Restore-Microbes_0.2-Data.csv")
microbes2018_8.10_matfull <- read_csv("./data/2018_NOAA-Restore-Microbes_8.10-Data.csv")
minnow2018_matfull <- read_csv("./data/2018_NOAA-Restore-Killitrap-Data.csv")
trawl2018_matfull <- read_csv("./data/2018_NOAA-Restore-Trawl-Data.csv")
infauna2018_matfull <- read_csv("./data/2018_NOAA-Restore-Infauna-Data.csv")
spiders2018_matfull <- read_csv("./data/2018_NOAA-Restore-Spiders-Data.csv")

# create community list and attribute objects
comm <- list()
plot_attr <- list()
taxa_names <- c("plants", 
                "microbes_0.2", 
                "microbes_8.10", 
                "pond_minnow", 
                "minnow", 
                "trawl",
                "infauna",
                "spiders")

# plants
plant_comm <- plants2018_matfull[, 8:ncol(plants2018_matfull)]
comm[[1]] <- plant_comm[, colSums(plant_comm) > 0]

plot_attr[[1]] <- plants2018_matfull[, c("marsh_id", "long_dd", "lat_dd")]
plot_attr[[1]]$marsh_id  <- recode_factor (plot_attr[[1]]$marsh_id,
                                           PS07 = "PS7",
                                           WPH01 = "WPH1",
                                           WPH02 = "WPH2")
plot_attr[[1]]$marsh_id <- factor (plot_attr[[1]]$marsh_id, levels= c("LHA","LHB","LHC",
                                                                      "WPH1","WPH2","PS7"))
# microbes 0-2
microbe0.2_comm <- microbes2018_0.2_matfull[, 9:578] # Omitting Unknown unclassified (order)
comm[[2]] <- microbe0.2_comm[, colSums(microbe0.2_comm) > 0]

plot_attr[[2]] <- microbes2018_0.2_matfull[, c("marsh_id", "long_dd", "lat_dd")]
plot_attr[[2]]$marsh_id  <- recode_factor (plot_attr[[2]]$marsh_id,
                                           PS07 = "PS7",
                                           WPH02 = "WPH2")
plot_attr[[2]]$marsh_id <- factor (plot_attr[[2]]$marsh_id, levels= c("LHA","LHB","LHC",
                                                                      "WPH1","WPH2","PS7"))

# microbes 8-10
microbe8.10_comm <- microbes2018_8.10_matfull[, 9:578] # Omitting Unknown unclassified (order)
comm[[3]] <- microbe8.10_comm[, colSums(microbe8.10_comm) > 0]

plot_attr[[3]] <- microbes2018_8.10_matfull[, c("marsh_id", "long_dd", "lat_dd")]
plot_attr[[3]]$marsh_id  <- recode_factor (plot_attr[[3]]$marsh_id,
                                           PS07 = "PS7",
                                           WPH02 = "WPH2")
plot_attr[[3]]$marsh_id <- factor (plot_attr[[3]]$marsh_id, levels= c("LHA","LHB","LHC",
                                                                      "WPH1","WPH2","PS7"))

# pond minnow
pond_minnow2018 <- minnow2018_matfull[startsWith(minnow2018_matfull$plot_id, "POND"), ]
pond_minnow_comm <- pond_minnow2018[, 9:ncol(pond_minnow2018)]
comm[[4]] <- pond_minnow_comm[, colSums(pond_minnow_comm) > 0]


plot_attr[[4]] <- pond_minnow2018[, c("marsh_id", "long_dd", "lat_dd")]
plot_attr[[4]]$marsh_id  <- recode_factor (plot_attr[[4]]$marsh_id,
                                           PS07 = "PS7",
                                           WPH02 = "WPH2")
plot_attr[[4]]$marsh_id <- factor (plot_attr[[4]]$marsh_id, levels= c("LHA","LHB","LHC",
                                                                      "WPH1","WPH2","PS7"))

# minnow
minnow_comm <- minnow2018_matfull[, 9:ncol(minnow2018_matfull)]
comm[[5]] <- minnow_comm[, colSums(minnow_comm) > 0]

plot_attr[[5]] <- minnow2018_matfull[, c("marsh_id", "long_dd", "lat_dd")]

# trawl
trawl_comm <- trawl2018_matfull[, 11:ncol(trawl2018_matfull)]
comm[[6]] <- trawl_comm[, colSums(trawl_comm) > 0]

plot_attr[[6]] <- trawl2018_matfull[, c("marsh_id", "long_dd", "lat_dd")]
plot_attr[[6]]$marsh_id  <- recode_factor (plot_attr[[6]]$marsh_id,
                                           PS07 = "PS7",
                                           WPH02 = "WPH2")
plot_attr[[6]]$marsh_id <- factor (plot_attr[[6]]$marsh_id, levels= c("LHA","LHB","LHC",
                                                                      "WPH1","WPH2","PS7"))

# infauna
infauna_comm <- infauna2018_matfull[,11:ncol(infauna2018_matfull)]
comm[[7]] <- infauna_comm[, colSums(infauna_comm) > 0]

plot_attr[[7]] <- infauna2018_matfull[,c("marsh_id", "long_dd", "lat_dd")]
plot_attr[[7]]$marsh_id  <- recode_factor (plot_attr[[7]]$marsh_id,
                                           PS07 = "PS7",
                                           WPH02 = "WPH2")
plot_attr[[7]]$marsh_id <- factor (plot_attr[[7]]$marsh_id, levels= c("LHA","LHB","LHC",
                                                                      "WPH1","WPH2","PS7"))

# spiders
spider_comm <- spiders2018_matfull[,6:ncol(spiders2018_matfull)]
comm[[8]] <- spider_comm[, colSums(spider_comm) > 0]

plot_attr[[8]] <- spiders2018_matfull[,c("marsh_id", "long_dd", "lat_dd")]
plot_attr[[8]]$marsh_id  <- recode_factor (plot_attr[[8]]$marsh_id,
                                           PS07 = "PS7",
                                           WPH02 = "WPH2")
plot_attr[[8]]$marsh_id <- factor (plot_attr[[8]]$marsh_id, levels= c("LHA","LHB","LHC",
                                                                      "WPH1","WPH2","PS7"))
# remove extra objects. Keep matrices and functions
rm(list = ls()[!grepl("matfull|comm|plot_attr|taxa_names", ls()) & (ls() %ni% lsf.str())])
