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
  # dplyr::filter(if_all(`Waypoint Name`), ~ !grepl('old', .x, ignore.case = TRUE)) %>% # Code update for when using `across()` in `filter()` is deprecated
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

###
### read in community matrix

minnow2018_matfull <- minnow2018_attr %>%
  right_join(minnow2018_mat %>% dplyr::mutate(across(plot_id, ~ toupper(.x)))) %>%
  bind_rows(minnow2018_toadd) %>%
  # convert column names to clean working names
  diversity_clean_cols() %>%
  recode_site_names()

write.csv(minnow2018_matfull, "./data/2018_NOAA-Restore-Killitrap-Data.csv", quote = FALSE, row.names = FALSE)

minnow2018_matfull <- read.csv("./data/2018_NOAA-Restore-Killitrap-Data.csv")
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
write.csv(trawl2018_matfull, "./data/2018_NOAA-Restore-Trawl-Data.csv", quote = FALSE, row.names = FALSE)

trawl2018_matfull <- read.csv("./data/2018_NOAA-Restore-Trawl-Data.csv")
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

write.csv(plants2018_matfull, "./data/2018_NOAA-Restore-Plant-Data.csv", quote = FALSE, row.names = FALSE)

plants2018_matfull <- read.csv("./data/2018_NOAA-Restore-Plant-Data.csv")
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

write.csv(microbes2018_0.2_matfull, "./data/2018_NOAA-Restore-Microbes_0.2-Data.csv", quote = FALSE, row.names = FALSE)

microbes2018_0.2_matfull <- read_csv("./data/2018_NOAA-Restore-Microbes_0.2-Data.csv")
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
write.csv(microbes2018_8.10_matfull, "./data/2018_NOAA-Restore-Microbes_8.10-Data.csv", quote = FALSE, row.names = FALSE)

microbes2018_8.10_matfull <- read_csv("./data/2018_NOAA-Restore-Microbes_0.2-Data.csv")
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

write_csv(spiders2018_matfull, "./data/2018_NOAA-Restore-Spiders-Data.csv")

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
