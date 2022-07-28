# load required project packages here
## script to load and munge data for Measurement of Biodiversity analysis of restored vs unrestored data.
if(!require(devtools)) install.packages('devtools') else{library(devtools)}
if(!require(pacman)) install.packages('pacman') else{library(pacman)}
if (!require("pairwiseAdonis")) install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
devtools::install_github("MoBiodiv/mobr")
# Install packages ----------------
pacman::p_load(here, styler, rmarkdown, readxl, tidyverse, rfishbase, taxize, mobr, vegan, parallel, usedist, gplots)
here::i_am("code/01_load_data.R")
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

# Need to import excel files and combine metadata together
# Step 1: identify the files for each gear/taxonomy type

## minnow trap files
minnow2018 <- list.files(path = here::here("data/"), pattern = "Killi.*xls*", full.names = TRUE) %>%
  lapply(., read_excel_allsheets) %>%
  pluck(1)

## trawl files

trawl2018 <- list.files(path = here::here("data/"), pattern = ".*Trawl.*xls*", full.names = TRUE) %>%
  lapply(., read_excel_allsheets) %>%
  flatten()

## plant files

plants2018 <- list.files(path = here::here("data/"), pattern = "Plant-Data", full.names = TRUE, ignore.case = TRUE) %>%
  # need to skip the first row and columns beyond AI
  lapply(., read_excel_allsheets, range = "A2:AI147") %>%
  flatten()

## microbe files

microbes2018 <- list.files(path = here::here("data/"), pattern = "Microbial", full.names = TRUE, ignore.case = TRUE) %>%
  lapply(., read_excel_allsheets) %>%
  flatten()

## spider/terr. insect files
### this is going to take a lot of cleaning to sift through

spiders2018 <- list.files(path = here::here("data/"), pattern = ".*Spiders.*", full.names = TRUE, ignore.case = TRUE) %>%
  lapply(., read_excel_allsheets) %>%
  flatten() ## need to clean these columns

## macroinfauna

infauna2018 <- list.files(path = here::here("data/"), pattern = ".*macroinfauna.*", full.names = TRUE, ignore.case = TRUE) %>%
  lapply(., read_excel_allsheets) %>%
  flatten()

# Step 2: Separate the data from metadata and combine/clean as needed

## minnow2018 -----------

### minnow trap site metadata

minnow2018_sites <- minnow2018[["Sites"]] %>%
  dplyr::filter(across(`Waypoint Name`, ~ !grepl("old", .x, ignore.case = TRUE))) %>%
  dplyr::select(Site, Location, habnum = `Location Number`, coords_dd = `Coordinates (dds)`) %>%
  na.omit() %>%
  # correct the spatial locations
  dplyr::mutate(across("coords_dd", ~ fix_latlong(.x, combined = TRUE, combined_sep = " ")),
    lat_dd = map(coords_dd, ~ stringr::str_extract_all(., "(^|^-)\\d{2,3}\\.\\d+") %>%
      pluck(1) %>%
      as.numeric()),
    long_dd = map(coords_dd, ~ stringr::str_extract_all(., "(^|^-)\\d{2,3}\\.\\d+") %>%
      pluck(2) %>%
      as.numeric())
  ) %>%
  dplyr::select(-coords_dd) %>%
  unnest(cols = c(lat_dd, long_dd))

### minnow trap deployment metadata

minnow2018_env <- minnow2018[["env"]] %>%
  dplyr::select(date, marsh_id = "site", habitat, habnum, trap, effort_min = "durationmin")

### merge minnowtrap site & deployment metadata to get trap-level attributes

minnow2018_attr <- minnow2018_env %>%
  dplyr::mutate(across(habitat, ~ toupper(.x))) %>%
  left_join(minnow2018_sites %>% rename(marsh_id = "Site", habitat = "Location")) %>%
  # create a plot_id from habitat, number, and trap
  tidyr::unite("plot_id", habitat:trap) %>%
  # add in additional required columns that are constant
  dplyr::mutate(
    technique = "minnow_trap",
    plot_size_m2 = 2.5,
    mesh_size_mm = 3.0
  ) %>%
  # reorder columns
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, mesh_size_mm, effort_min)

### minnow trap abundance data

minnow2018_abun <- minnow2018[["abun"]] %>%
  dplyr::select(date, marsh_id = "site", habitat, habnum, trap, common_name = "common", species_name = "gensp", abun = "n") %>%
  # create plot_id from habitat, number, and trap
  tidyr::unite("plot_id", habitat:trap) %>%
  dplyr::mutate(abun = case_when(
    !is.numeric(abun) ~ gsub("[^[:alnum:] ]", "", abun),
    TRUE ~ abun
  )) %>%
  dplyr::mutate(across(abun, ~ as.numeric(.x)))

### pivot to wide, fix column names, and add sample attributes
minnow2018_mat <- minnow2018_abun %>%
  dplyr::select(-common_name) %>%
  na.omit() %>%
  pivot_wider(names_from = "species_name", values_from = "abun", values_fill = 0) %>%
  dplyr::filter(marsh_id != "PS7" | date != as.Date("2018-05-23") )

# plots to add

site_locs = minnow2018_attr %>% dplyr::select(marsh_id, plot_id, lat_dd, long_dd) %>% unique

dates = as.Date(c("2018-05-18","2018-05-19","2018-05-18","2018-05-18",
          "2018-05-19","2018-05-18","2018-05-18","2018-05-20",
          "2018-05-18","2018-05-18","2018-05-18","2018-05-20",
          "2018-05-18","2018-05-20","2018-05-18","2018-05-18",
          "2018-05-21","2018-05-22","2018-05-21",
          "2018-05-21","2018-05-22","2018-05-23"))

minnow2018_toadd = tibble(minnow2018_mat %>% slice(1)) %>% .[-1,] %>%
  bind_rows(tibble(date = dates,
                marsh_id = c(rep("LHA",8),rep("LHC",8),rep("WPH1",3),rep("WPH2",3)),
                plot_id = c(rep("POND_1_A",2), "POND_1_B","POND_2_A","POND_2_C","POND_3_B",
                            rep("POND_3_C",2),"POND_1_C","POND_2_A",rep("POND_2_B",2),"POND_2_C",
                            "POND_2_C","POND_3_A","POND_3_B","POND_1_C","POND_3_A","POND_3_A",
                            "POND_2_A","POND_2_B","POND_2_B"))) %>%
  dplyr::mutate(across(c(-date, -marsh_id, -plot_id), ~replace_na(.x, 0))) %>%
  left_join(site_locs, by = c("marsh_id","plot_id")) %>%
  dplyr::select(marsh_id, plot_id, lat_dd,long_dd, date, everything())

### create community matrix

minnow2018_matfull <- minnow2018_attr %>%
  right_join(minnow2018_mat %>% dplyr::mutate(across(plot_id, ~ toupper(.x)))) %>%
  bind_rows(minnow2018_toadd) %>%
  # convert column names to clean working names
  diversity_clean_cols() %>%
  recode_site_names()

## trawl2018 ------------

### create community matrix

trawl2018_matfull <- trawl2018 %>%
  # this pulls in the abundance data
  pluck(1) %>%
  # convert column names to clean working names
  diversity_clean_cols() %>%
  # correct the spatial locations
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE)))%>%
  recode_site_names()

## plants2018 ---------

plants2018_matfull <- plants2018 %>%
  # this pulls in the wide biomass data
  pluck(1) %>%
  # filter just 2018
  dplyr::filter(Year == 2018) %>%
  # add in plot size variable
  dplyr::mutate(
    plot_size_m2 = 0.25,
    Transect = paste0("T", Transect),
    Distance = paste0(Distance, "m"),
    technique = "quadrat"
  ) %>%
  # clean unnecessary columns
  dplyr::select(-Year) %>%
  # create a plot_id from transect_distance_plot
  tidyr::unite("plot_id", Transect, Distance, Plot) %>%
  # convert column names to clean working names
  diversity_clean_cols() %>%
  # correct the spatial locations
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE))) %>%
  # rearrange columns for attributes in first rows
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, technique, everything()) %>%
  # convert NAs to 0 for biomass
  dplyr::mutate(across(`Spartina alterniflora`:`mustard`, ~ tidyr::replace_na(.x, 0)))%>%
  recode_site_names()

## microbes 2018 --------
### microbes, ORDER

#### 0-2 cm

microbes2018_0.2_matfull <- microbes2018 %>%
  # extract the order surface data.frame
  .[grepl("order.surface.*", names(.), ignore.case = TRUE)] %>%
  pluck(1) %>%
  # clean unnecessary columns
  dplyr::select(-matches("Total"), -...13) %>%
  # rename transects and extract the transect distances
  dplyr::mutate(
    `Marsh ID` = sub(".*_.*_(\\w{3,4})_.*", "\\1", `Marsh ID`),
    distance = case_when(
      grepl("edge", `Sample Description`, ignore.case = TRUE) ~ "0m",
      TRUE ~ gsub(" ", "", sub(".* (\\d{1,3} m).*$", "\\1", `Sample Description`))
    ),
    `Transect ID` = paste0("T", stringr::str_sub(`Transect ID`, -1, -1))
  ) %>%
  tidyr::unite("plot_id", `Transect ID`, distance) %>%
  diversity_clean_cols() %>%
  # correct the spatial locations, if needed
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE))) %>%
  # rearrange columns for attributes in first rows
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, technique, everything())%>%
  recode_site_names()

#### 8-10 cm

microbes2018_8.10_matfull <- microbes2018 %>%
  # extract the order surface data.frame
  .[grepl("order.depth.*", names(.), ignore.case = TRUE)] %>%
  pluck(1) %>%
  # clean unnecessary columns
  dplyr::select(-matches("Total"), -...13) %>%
  # rename transects and extract the transect distances
  dplyr::mutate(
    `Marsh ID` = sub(".*_.*_(\\w{3,4})_.*", "\\1", `Marsh ID`),
    distance = case_when(
      grepl("edge", `Sample Description`, ignore.case = TRUE) ~ "0m",
      TRUE ~ gsub(" ", "", sub(".* (\\d{1,3} m).*$", "\\1", `Sample Description`))
    ),
    `Transect ID` = paste0("T", stringr::str_sub(`Transect ID`, -1, -1))
  ) %>%
  tidyr::unite("plot_id", `Transect ID`, distance) %>%
  diversity_clean_cols() %>%
  # correct the spatial locations, if needed
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE))) %>%
  # rearrange columns for attributes in first rows
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, technique, everything())%>%
  recode_site_names()

## microbes, PHYLUM -----

### 0-2 cm
microbes2018_0.2_PHYLUM_matfull <- microbes2018 %>%
  # extract the order surface data.frame
  .[grepl("phylum.surface.*", names(.), ignore.case = TRUE)] %>%
  pluck(1) %>%
  # clean unnecessary columns
  dplyr::select(-matches("Total"), -...13) %>%
  # rename transects and extract the transect distances
  dplyr::mutate(
    `Marsh ID` = sub(".*_.*_(\\w{3,4})_.*", "\\1", `Marsh ID`),
    distance = case_when(
      grepl("edge", `Sample Description`, ignore.case = TRUE) ~ "0m",
      TRUE ~ gsub(" ", "", sub(".* (\\d{1,3} m).*$", "\\1", `Sample Description`))
    ),
    `Transect ID` = paste0("T", stringr::str_sub(`Transect ID`, -1, -1))
  ) %>%
  tidyr::unite("plot_id", `Transect ID`, distance) %>%
  diversity_clean_cols() %>%
  # correct the spatial locations, if needed
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE))) %>%
  # rearrange columns for attributes in first rows
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, technique, everything())%>%
  recode_site_names()

### 8-10 cm
microbes2018_8.10_PHYLUM_matfull <- microbes2018 %>%
  # extract the order surface data.frame
  .[grepl("phylum.depth.*", names(.), ignore.case = TRUE)] %>%
  pluck(1) %>%
  # clean unnecessary columns
  dplyr::select(-matches("Total"), -...13) %>%
  # rename transects and extract the transect distances
  dplyr::mutate(
    `Marsh ID` = sub(".*_.*_(\\w{3,4})_.*", "\\1", `Marsh ID`),
    distance = case_when(
      grepl("edge", `Sample Description`, ignore.case = TRUE) ~ "0m",
      TRUE ~ gsub(" ", "", sub(".* (\\d{1,3} m).*$", "\\1", `Sample Description`))
    ),
    `Transect ID` = paste0("T", stringr::str_sub(`Transect ID`, -1, -1))
  ) %>%
  tidyr::unite("plot_id", `Transect ID`, distance) %>%
  diversity_clean_cols() %>%
  # correct the spatial locations, if needed
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE))) %>%
  # rearrange columns for attributes in first rows
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, technique, everything())%>%
  recode_site_names()

## microbes, CLASS -----

### 0-2 cm
microbes2018_0.2_CLASS_matfull <- microbes2018 %>%
  # extract the order surface data.frame
  .[grepl("class.surface.*", names(.), ignore.case = TRUE)] %>%
  pluck(1) %>%
  # clean unnecessary columns
  dplyr::select(-matches("Total"), -...13) %>%
  # rename transects and extract the transect distances
  dplyr::mutate(
    `Marsh ID` = sub(".*_.*_(\\w{3,4})_.*", "\\1", `Marsh ID`),
    distance = case_when(
      grepl("edge", `Sample Description`, ignore.case = TRUE) ~ "0m",
      TRUE ~ gsub(" ", "", sub(".* (\\d{1,3} m).*$", "\\1", `Sample Description`))
    ),
    `Transect ID` = paste0("T", stringr::str_sub(`Transect ID`, -1, -1))
  ) %>%
  tidyr::unite("plot_id", `Transect ID`, distance) %>%
  diversity_clean_cols() %>%
  # correct the spatial locations, if needed
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE))) %>%
  # rearrange columns for attributes in first rows
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, technique, everything())%>%
  recode_site_names()

### 8-10 cm
microbes2018_8.10_CLASS_matfull <- microbes2018 %>%
  # extract the order surface data.frame
  .[grepl("class.depth.*", names(.), ignore.case = TRUE)] %>%
  pluck(1) %>%
  # clean unnecessary columns
  dplyr::select(-matches("Total"), -...13) %>%
  # rename transects and extract the transect distances
  dplyr::mutate(
    `Marsh ID` = sub(".*_.*_(\\w{3,4})_.*", "\\1", `Marsh ID`),
    distance = case_when(
      grepl("edge", `Sample Description`, ignore.case = TRUE) ~ "0m",
      TRUE ~ gsub(" ", "", sub(".* (\\d{1,3} m).*$", "\\1", `Sample Description`))
    ),
    `Transect ID` = paste0("T", stringr::str_sub(`Transect ID`, -1, -1))
  ) %>%
  tidyr::unite("plot_id", `Transect ID`, distance) %>%
  diversity_clean_cols() %>%
  # correct the spatial locations, if needed
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE))) %>%
  # rearrange columns for attributes in first rows
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, technique, everything())%>%
  recode_site_names()

## spiders --------

### create morpho species naming keyvalue
spider_bad_names <- list(
  "Linyphiia",
  "Grammonata",
  "Paradosa",
  "Spider",
  "Tertagnatha"
)

spider_good_names <- list(
  "Linyphia",
  "Grammonota",
  "Pardosa",
  "Pardosa",
  "Tetragnatha"
)

spider_morpho_names <- list(
  "Spider A",
  "Spider L",
  "Spider N",
  "Spider O",
  "Spider R",
  "Spider S",
  "Spider V"
)

spider_family_names <- list(
  "Salticidae",
  "Spider L",
  "Linyphiidae",
  "Spider O",
  "Spider R",
  "Spider S",
  "Tetragnathidae"
)

spider_genus_names <- list(
  c("Marpissa", "Jumping Spider"),
  "Grammonota",
  "Spider N",
  "Linyphiia",
  "Gasteracantha",
  "Pardosa",
  "Tetragnatha"
)

spider_name_recode <- purrr::map2(spider_bad_names, spider_good_names, function(x, y) setNames(rep(y, length(x)), nm = x)) %>%
  flatten() %>%
  unlist()
spider_family_recode <- purrr::map2(spider_morpho_names, spider_family_names, function(x, y) setNames(rep(y, length(x)), nm = x)) %>%
  flatten() %>%
  unlist()
spider_family_recode2 <- purrr::map2(spider_family_names, spider_morpho_names, function(x, y) setNames(rep(y, length(x)), nm = x)) %>%
  flatten() %>%
  unlist()
spider_genus_recode <- purrr::map2(spider_morpho_names, spider_genus_names, function(x, y) setNames(rep(y, length(x)), nm = x)) %>%
  flatten() %>%
  unlist()
spider_genus_recode2 <- purrr::map2(spider_genus_names, spider_morpho_names, function(x, y) setNames(rep(y, length(x)), nm = x)) %>%
  flatten() %>%
  unlist()
### read in and code data

spiders2018_matfull <- spiders2018 %>%
  pluck(1) %>%
  dplyr::select(Date:Notes, -Order) %>%
  dplyr::mutate(Date = case_when(grepl("2019", Date) ~ as.POSIXct(gsub("2019","2018",Date), "%Y-%m-%d"),
                                 grepl("2020", Date) ~ as.POSIXct(gsub("2020","2018",Date), "%Y-%m-%d"),
                                 TRUE ~ Date)) %>%
  # clean up the species names and get to standardized name scheme
  dplyr::mutate(across(Site, ~ gsub(" ", "", .x)),
    across(Family:Species, ~ recode(.x, !!!spider_name_recode)),
    across(Family:Species, ~ trimws(.x)),
    `Common Name` = case_when(
      grepl("Spider B", `Common Name`) ~ NA_character_,
      grepl("Wolf spiders", `Common Name`) ~ gsub("Wolf spiders|/", "", `Common Name`),
      TRUE ~ `Common Name`
    ),
    across(Family:Genus, R.utils::capitalize),
    across(Family:Species, ~ gsub("-", NA_character_, .x)),
    across(Genus, ~ gsub("sp|sp.", "", .x)),
    across(Species, ~ gsub("sp|sp.", NA_character_, .x)),
    across(Genus, ~ gsub("LInyphiidae", "Linyphiidae", .x, ignore.case = FALSE)),
    Species = case_when(
      grepl("eperigoma", Species) ~ Species,
      TRUE ~ NA_character_
    ),
    # Species = recode(Genus, !!!spider_genus_recode),
    spp_id = NA_character_,
    spp_id = case_when(
      is.na(Species) & grepl("Spider.*", Family) ~ Family,
      is.na(Species) & grepl("Spider.*", Notes) ~ Notes,
      is.na(Species) & grepl("Spider.*", `Common Name`) ~ `Common Name`,
      is.na(Species) & !is.na(Genus) & any(Genus %in% names(spider_genus_recode2)) ~ Genus,
      is.na(Species) & is.na(Genus) & any(Family %in% names(spider_family_recode2)) ~ Family,
      TRUE ~ Species
    ),
    spp_id = recode(spp_id, !!!spider_genus_recode2),
    spp_id = recode(spp_id, !!!spider_family_recode2)
  ) %>%
  dplyr::select(Site, Date, lat_dd = Latitude, long_dd = Longitude, spp_id, abun = `How many?`) %>%
  group_by(Site, Date, lat_dd, long_dd, spp_id) %>%
  dplyr::summarise(abun = sum(abun, na.rm = TRUE)) %>% ungroup %>%
  group_by(Site, Date, lat_dd, long_dd) %>%
  pivot_wider(names_from = spp_id, values_from = abun, values_fill = 0) %>%
  as_tibble %>%
  diversity_clean_cols() %>%
  # correct the spatial locations
  dplyr::mutate(across(c(lat_dd, long_dd), ~ fix_latlong(.x, combined = FALSE)))%>%
  recode_site_names() %>%
  tidyr::unite("plot_id", c(marsh_id, date), remove = FALSE)

## macroinfauna -----
### plot lat/long are pulled from plant data for each size

infauna_latlong <- plants2018_matfull %>%
  dplyr::select(marsh_id, plot_id, lat_dd, long_dd) %>%
  dplyr::filter(grepl("10m|50m", plot_id, ignore.case = TRUE)) %>%
  unique() %>%
  dplyr::mutate(plot_temp = substr(plot_id, 1, 6)) %>%
  dplyr::select(-plot_id)

### 45.6 cm^2 plot area
#### site recode list for renaming sites to merge coords
# old_site <- list(
#   c("WPH1", "WHP1"),
#   "WPH2",
#   "PS7"
# )
# 
# new_site <- list(
#   "WPH01",
#   "WPH02",
#   "PS07"
# )
# 
# site_recode <- purrr::map2(old_site, new_site, function(x, y) setNames(rep(y, length(x)), nm = x)) %>%
#   flatten() %>%
#   unlist()

### final cleaning
infauna2018_matfull <- infauna2018 %>%
  # grab data
  pluck(1) %>%
  # remove rows with all NAs
  dplyr::filter(!is.na(`Marsh ID...1`)) %>%
  # remove unneeded columns
  dplyr::select(-`Marsh ID...12`, -`Plot ID...13`) %>%
  # clean the column names
  diversity_clean_cols() %>%
  recode_site_names() %>%
  # fill in other columns
  dplyr::mutate(
    Transect = gsub("\\.", "", Transect),
    distance = paste0(substr(plot_id, 1, 2), "m"),
    rep = toupper(substr(plot_id, 3, 3)),
    plot_size_m2 = 0.00456,
    effort_hr = NA
  ) %>%
  tidyr::unite("plot_id", Transect, distance, rep, sep = "_") %>%
  dplyr::mutate(plot_temp = substr(plot_id, 1, 6)) %>%
  dplyr::select(-lat_dd, -long_dd) %>%
  left_join(infauna_latlong, by = c("marsh_id", "plot_temp")) %>%
  dplyr::select(-plot_temp) %>%
  # reorder columns
  dplyr::select(marsh_id, plot_id, plot_size_m2, lat_dd, long_dd, date, technique, everything()) %>%
  # replace NAs in abundances with zeros
  dplyr::mutate(across(`Nematoda`:`Paraprionospio pinnata`, ~ replace_na(.x, 0)))

## all final community matrices should be named "*comm-type/gear*2018_matfull"
## remove all the intermediate objects except the final community matrices and loaded functions


# help (manual)
# vignette("mobr_intro",package="mobr")

# speed up the analysis with parallel

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

rm(list = ls()[!grepl("matfull|comm|plot_attr|taxa_names", ls()) & (ls() %ni% lsf.str())])

###### SPARE(D) code ########
#
# bad_names = x$species[x$species %ni% taxonomy_valid_common$Species]
#
# map(bad_names, ~agrepl(.x, taxonomy_valid_common$Species, max.distance = 0.3))
#
# fish_synonyms = lapply(taxonomy_valid_common %>% dplyr::filter(server == "rfishbase") %>% dplyr::select(Species) %>% unlist, rfishbase::synonyms, server = 'rfishbase' )
#
# sealife_synonyms = lapply(taxonomy_valid_common %>% dplyr::filter(server == "sealifebase") %>% dplyr::select(Species) %>% unlist, rfishbase::synonyms, server = 'sealifebase')
#
