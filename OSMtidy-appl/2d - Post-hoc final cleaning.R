# =========================================================================
# 2d - Post-hoc final cleaning.R
# Compatible with OSMtidy v0.0.5

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2021-07-26
#
# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2021-08-27
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Ensure latest version and preferred branch of certain packages are installed
devtools::install_github("avisserquinn/OSMtidy@dev")

# Load required packages
pacman::p_load(OSMtidy, progress, tidyverse, s2, sf, gdata)


# 0.2. EXTRA FUNCTIONS ----------------------------------------------------

# Function to read in Excel sheets into a list
function_loadXL <- function(wb_source) {
  
  wb_sheets <- readxl::excel_sheets(wb_source) 
  
  output <-
    wb_sheets %>%
    purrr::map(function(sheet){ # iterate through each sheet name
      readxl::read_xlsx(path = wb_source, sheet = sheet)
    })
  
  names(output) <- wb_sheets
  
  return(output)
  
}










### BRISTOL ### 

# 1. READ IN DATA ---------------------------------------------------------

# Read in most current OSMAH key
# This is the most current set of desc terms to check against
OSMAHkey = read_csv("OSM-AH-key_2.0_20210630.csv")

# Read in inspected desc file
inspected <- readRDS("~descCheck/descInspect_20210604_20210806.RDS") ## Specify

### BRISTOL FILES ###
# FINAL_Bristol_4_dataWrangle-noDetail_20200527-171413_20210616_20210806.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_Bristol_5_dataFilter-unfiltered_20200527-195725_20210614.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_Bristol_6_dataTidy-filtered-updated_20210604_20210806_20210826.RDS # filtered file with updated desc terms
# FINAL_Bristol_5_dataFilter-validate_20200527-195731_20210806.xlsx # validated file

# Read in noDetail file
noDetail <- function_loadXL(
  wb_source = "Bristol/FINAL_Bristol_4_dataWrangle-noDetail_20200527-171413_20210616_20210806.xlsx") ## Specify

# Read in unfiltered file
unfiltered <- function_loadXL(
  wb_source = "Bristol/FINAL_Bristol_5_dataFilter-unfiltered_20200527-195725_20210614.xlsx") ## Specify

# Read in RDS files for filtered data
filtered <- readRDS("Bristol/FINAL_Bristol_6_dataTidy-filtered-updated_20210604_20210806_20210826.RDS") ## Specify

# Read in unfiltered file
validated <- function_loadXL(
  wb_source = "Bristol/FINAL_Bristol_5_dataFilter-validate_20200527-195731_20210806.xlsx") ## Specify


# 1. TIDY DATA ------------------------------------------------------------

### NO DETAIL ###
# Create and format single dataframe
noDetail <-
  lapply(noDetail, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>% 
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(noDetail) <- c()


### UNFILTERED ###
# Create and format single dataframe
unfiltered <-
  lapply(unfiltered, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(unfiltered) <- c()


### VALIDATED ###
# Create and format single dataframe
validated <-
  lapply(validated, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(validated) <- c()


### FILTERED ###
# Check already in correct format
filtered


### INSPECTED ### 
# Create and format single dataframe (for London Central only)
inspected_Bristol <- 
  inspected$Bristol %>%
  select(osm_id, desc, geometry) %>%
  mutate(geometry = sf::st_as_sfc(geometry))


# 3. FIND & REPLACE DEPRECATED DESC ---------------------------------------

### ALL ###

# Create single dataframe for most data
all <- rbind(noDetail, unfiltered, filtered, validated)

# Create vector of osm_id for inspected and desc-reassigned rows to replace in 'all'
osm_id_toReplace <- inspected_Bristol %>% pull(osm_id)

# Replace some rows with inspected desc and just in case, remove duplicated rows
all_step2 <-
  all %>%
  filter(!osm_id %in% osm_id_toReplace) %>%
  rbind(inspected_Bristol) %>%
  unique()


### FINAL CHECK DESC ARE ALL UP-TO-DATE ###

# Find all deprecatedDesc in this set of OSMtidy outputs
currentDesc <- OSMAHkey %>% drop_na(desc) %>% select(desc) %>% unique %>% pull

deprecatedDesc <- 
  all_step2 %>% 
  #      sf::st_drop_geometry() %>% # only needed if going from post-processing sf objects
  select(desc) %>%
  filter(!desc %in% currentDesc) %>% 
  unique()
# All that should remain are desc = 
# NA; 
# any phrase including "remove", "Remove", "keyword" or "Keyword"
# "Airport; Aerodrome, terminal and gates" (these get taken care of in postProcessing steps)
# "Transport infrastructure; Airport apron, runways and taxiways" (these get taken care of in postProcessing steps)

# Public transport; Ferry terminals and marinas facilities
# filtered %>% filter(desc == "Public transport; Ferry terminals and marinas facilities")
# Actually a Harbour Master's office so can be changed to "Industry; Harbour"

# Logistics; Depots and sorting offices
# validated %>% filter(desc == "Logistics; Depots and sorting offices")
# should be changed to "Logistics; Warehouses and sorting offices" manually in validated file

# Harbours
# validated %>% filter(desc == "Harbours")
# should be changed to "Industry; Harbour" manually in validated file

# Healthcare; Alternative medicine
# filtered %>% filter(desc == "Healthcare; Alternative medicine")
# inspected_Bristol %>% filter(desc == "Healthcare; Alternative medicine")
# all could qualify as "Healthcare; Specialist"

# filtered1 <-
#  filtered %>%
#  mutate(desc = replace(desc, 
#                       desc == "Public transport; Ferry terminals and marinas facilities", 
#                       "Industry; Harbour"))

# filtered1 %>% filter(desc == "Public transport; Ferry terminals and marinas facilities")

# filtered_updated <-
#  filtered1 %>%
#  mutate(desc = replace(desc, 
#                       desc == "Healthcare; Alternative medicine", 
#                       "Healthcare; Specialist"))

# filtered_updated %>% filter(desc == "Healthcare; Alternative medicine")

# saveRDS(filtered_updated, "Bristol/FINAL_Bristol_6_dataTidy-filtered-updated_20210604_20210806.RDS")


# inspected$Bristol <-
#  inspected$BristoldataWrangle %>%
#  mutate(desc = replace(desc, 
#                        desc == "Healthcare; Alternative medicine", 
#                        "Healthcare; Specialist"))

# saveRDS(inspected, "~descCheck/descInspect_20210604_20210806.RDS")

# Food production and horticulture; Agriculture needs changed to Brownfield sites as filter incorrectly assigned
# filtered %>% filter(desc == "Food production and horticulture; Agriculture")

# filtered_updated <-
#   filtered %>%
#   mutate(desc = replace(desc, 
#                         desc == "Food production and horticulture; Agriculture", 
#                         "Brownfield sites"))

# saveRDS(filtered_updated, "Bristol/FINAL_Bristol_6_dataTidy-filtered-updated_20210604_20210806_20210826.RDS")


# Double check there are no duplicated rows
check_rows = all_step2[duplicated(all_step2) | duplicated(all_step2, fromLast=TRUE), ]
# 0 items

# Find rows with osm_id that are duplicated
check_osm = all_step2[duplicated(all_step2$osm_id) | duplicated(all_step2$osm_id, fromLast=TRUE), ]
# 66794 items

# Find rows with osm_id that are duplicated that have an entry for desc
check_osm_notNA = check_osm %>% filter(!is.na(desc))
# 33397 items

# Find rows with osm_id that are duplicated, that have an entry for desc, that are duplicates
check_osm_notNA_duplicated = check_osm_notNA[duplicated(check_osm_notNA$osm_id) | duplicated(check_osm_notNA$osm_id, fromLast=TRUE), ]
# 0 items

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA)
check_osm_NA = check_osm %>% filter(is.na(desc))
# 33397 items

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA), that are duplicates
check_osm_NA_duplicated = check_osm_NA[duplicated(check_osm_NA$osm_id) | duplicated(check_osm_NA$osm_id, fromLast=TRUE), ]
# 0 items

# By this check, we can assume all duplicate osm_id rows are ones with NA values 
# and these will be removed via OSMtidy step 6 function dataTidy or similar step

# Export before removing NA or remove rows just in case
# saveRDS(all_step2, "Bristol/FINAL_Bristol_6_dataTidy-pre-cleaning_20210826.RDS")


# 4. CREATE DATATIDY-STYLE OUTPUT -----------------------------------------

dataTidy <- list()

dataTidy$filtered <-
  all_step2 %>%
  filter(!is.na(desc)) %>%
  filter(!str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$removed <-
  all_step2 %>%
  filter(is.na(desc) | str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$summary <- 
  all_step2 %>%
  mutate(desc = replace(desc, 
                        is.na(desc) | 
                          str_detect(desc, "remove|Remove|keyword|Keyword"), 
                        "removed")) %>%
  count(desc, sort = TRUE) %>%
  mutate(percentage_overall = round((n/sum(n))*100, 5))

class(dataTidy) <- c(class(dataTidy), "OSMtidy_dataTidy")

# Export output similar to OSMtidy step 6 dataTidy
# saveRDS(dataTidy, "Bristol/FINAL_Bristol_6_dataTidy-post-cleaning_20210826.RDS")


# 5. POST-PROCESSING ------------------------------------------------------

# If needed, remove all objects except dataTidy
gdata::keep(dataTidy, sure = TRUE)

# If needed, read in dataTidy again
# dataTidy <- readRDS("Bristol/FINAL_Bristol_6_dataTidy-post-cleaning_20210806.RDS")

# Extract filtered data
dg_og <- dataTidy$filtered

# Ensure filepath name and file/location name are specified
path <- "Bristol/"
name <- "FINAL_Bristol"

# Make chosen data valid
dg <- dg_og %>% makeValid()
# Inspect step 6 filtered data
dg

# The output could be exported now using exportOSMtidy()
# Or after application of the five simplifying functions

# Simplify airport and helipad data
dg <- dg %>% simplifyAirports
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Check for 'Airports (to validate)'
dg %>% filter(desc == "Airports (to validate)")
# Check OpenStreetMap/GoogleMaps coordinates of any 'Airports (to validate)'
# If needed change this to appropriate desc e.g. (only an example!)
# dg <- dg %>% mutate(desc = str_replace(desc, "Airports \\(to validate\\)", "Public transport; Helipad"))
# Check it's worked
dg %>% filter(str_detect(desc, "irport|elipad"))

# Simplify golf course data
dg <- dg %>% simplifyGolf
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify rail station data
dg <- dg %>% simplifyRail
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Ignore error: "No preset descNew has been found for this object type. Ensure you have specified a descNew argument."
# Inspect simplified data
dg %>% filter(str_detect(desc, "Public transport; Rail station"))

# Simplify water and wastewater treatment works data
dg <- dg %>% simplifyTreatment
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify events venues e.g. stadiums data
dg <- dg %>% simplifyVenues
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Ensure filepath and file/location name are specified
path
name

# Export the final output from OSMtidy in three formats
# Specify output object (dg = __)
# Specify filepath name (path = __)
# Specify file/location name (name = __)
# Specify if you want output /not/ in simple feature format (sf = FALSE)
# Specify file format (ext = __)
exportOSMtidy(dg, path, name, ext = ".RDS") # Gives output as RDS in simple feature format
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".RDS") # Gives output as RDS in dataframe format (easier for later data wrangling)
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".csv") # Gives output as CSV in dataframe format
exportOSMtidy(dg, path, name, ext = ".shp") # Gives output as shapefile










### EDINBURGH ### 

# 1. READ IN DATA ---------------------------------------------------------

# Read in most current OSMAH key
# This is the most current set of desc terms to check against
OSMAHkey = read_csv("OSM-AH-key_2.0_20210630.csv")

# Read in inspected desc file
inspected <- readRDS("~descCheck/descInspect_20210604_20210806.RDS") ## Specify

### EDINBURGH FILES ###
# FINAL_Edinburgh_4_dataWrangle-noDetail_20200527-173229_20210616_20210806.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_Edinburgh_5_dataFilter-unfiltered_20200527-220454_20210614.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_Edinburgh_5.5_dataTidy-filtered-updated_20210604_20210826.RDS # filtered file with updated desc terms
# FINAL_Edinburgh_5_dataFilter-validate_20200527-220509_20210806.xlsx # validated file

# Read in noDetail file
noDetail <- function_loadXL(
  wb_source = "Edinburgh/FINAL_Edinburgh_4_dataWrangle-noDetail_20200527-173229_20210616_20210806.xlsx") ## Specify

# Read in unfiltered file
unfiltered <- function_loadXL(
  wb_source = "Edinburgh/FINAL_Edinburgh_5_dataFilter-unfiltered_20200527-220454_20210614.xlsx") ## Specify

# Read in RDS files for filtered data
filtered <- readRDS("Edinburgh/FINAL_Edinburgh_5.5_dataTidy-filtered-updated_20210604_20210826.RDS") ## Specify

# Read in unfiltered file
validated <- function_loadXL(
  wb_source = "Edinburgh/FINAL_Edinburgh_5_dataFilter-validate_20200527-220509_20210806.xlsx") ## Specify


# 1. TIDY DATA ------------------------------------------------------------

### NO DETAIL ###
# Create and format single dataframe
noDetail <-
  lapply(noDetail, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>% 
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(noDetail) <- c()


### UNFILTERED ###
# Create and format single dataframe
unfiltered <-
  lapply(unfiltered, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(unfiltered) <- c()


### VALIDATED ###
# Create and format single dataframe
validated <-
  lapply(validated, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(validated) <- c()


### FILTERED ###
# Check already in correct format
filtered


### INSPECTED ### 
# Create and format single dataframe (for London Central only)
inspected_Edinburgh <- 
  inspected$EdinburghdataWrangle %>%
  select(osm_id, desc, geometry) %>%
  mutate(geometry = sf::st_as_sfc(geometry))


# 3. FIND & REPLACE DEPRECATED DESC ---------------------------------------

### ALL ###

# Create single dataframe for most data
all <- rbind(noDetail, unfiltered, filtered, validated)

# Create vector of osm_id for inspected and desc-reassigned rows to replace in 'all'
osm_id_toReplace <- inspected_Edinburgh %>% pull(osm_id)

# Replace some rows with inspected desc and just in case, remove duplicated rows
all_step2 <-
  all %>%
  filter(!osm_id %in% osm_id_toReplace) %>%
  rbind(inspected_Edinburgh) %>%
  unique()


### FINAL CHECK DESC ARE ALL UP-TO-DATE ###

# Find all deprecatedDesc in this set of OSMtidy outputs
currentDesc <- OSMAHkey %>% drop_na(desc) %>% select(desc) %>% unique %>% pull

deprecatedDesc <- 
  all_step2 %>% 
  #      sf::st_drop_geometry() %>% # only needed if going from post-processing sf objects
  select(desc) %>%
  filter(!desc %in% currentDesc) %>% 
  unique()
# All that should remain are desc = 
# NA; 
# any phrase including "remove", "Remove", "keyword" or "Keyword"
# "Airport; Aerodrome, terminal and gates" (these get taken care of in postProcessing steps)
# "Transport infrastructure; Airport apron, runways and taxiways" (these get taken care of in postProcessing steps)

# Harbours
# validated %>% filter(desc == "Harbours")
# should be changed to "Industry; Harbour" manually in validated file

# Logistics; Depots and sorting offices
# validated %>% filter(desc == "Logistics; Depots and sorting offices")
# should be changed to "Logistics; Warehouses and sorting offices" manually in validated file

# Food production and horticulture; Agriculture needs changed to Brownfield sites as filter incorrectly assigned
# filtered %>% filter(desc == "Food production and horticulture; Agriculture")

# filtered_updated <-
#   filtered %>%
#   mutate(desc = replace(desc, 
#                         desc == "Food production and horticulture; Agriculture", 
#                         "Brownfield sites"))

# saveRDS(filtered_updated, "Edinburgh/FINAL_Edinburgh_6_dataTidy-filtered-updated_20210604_20210806_20210826.RDS")


# Double check there are no duplicated rows
check_rows = all_step2[duplicated(all_step2) | duplicated(all_step2, fromLast=TRUE), ]
# 0 items

# Find rows with osm_id that are duplicated
check_osm = all_step2[duplicated(all_step2$osm_id) | duplicated(all_step2$osm_id, fromLast=TRUE), ]
# 570532 items

# Find rows with osm_id that are duplicated that have an entry for desc
check_osm_notNA = check_osm %>% filter(!is.na(desc))
# 285274 items

# Find rows with osm_id that are duplicated, that have an entry for desc, that are duplicates
check_osm_notNA_duplicated = check_osm_notNA[duplicated(check_osm_notNA$osm_id) | duplicated(check_osm_notNA$osm_id, fromLast=TRUE), ]
# 20 (remaining have same osm_id but different geometries)

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA)
check_osm_NA = check_osm %>% filter(is.na(desc))
# 285258 items

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA), that are duplicates
check_osm_NA_duplicated = check_osm_NA[duplicated(check_osm_NA$osm_id) | duplicated(check_osm_NA$osm_id, fromLast=TRUE), ]
# 0 items

# By this check, we can assume all duplicate osm_id rows are ones with NA values 
# and these will be removed via OSMtidy step 6 function dataTidy or similar step

# Export before removing NA or remove rows just in case
# saveRDS(all_step2, "Edinburgh/FINAL_Edinburgh_6_dataTidy-pre-cleaning_20210826.RDS")


# 4. CREATE DATATIDY-STYLE OUTPUT -----------------------------------------

dataTidy <- list()

dataTidy$filtered <-
  all_step2 %>%
  filter(!is.na(desc)) %>%
  filter(!str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$removed <-
  all_step2 %>%
  filter(is.na(desc) | str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$summary <- 
  all_step2 %>%
  mutate(desc = replace(desc, 
                        is.na(desc) | 
                          str_detect(desc, "remove|Remove|keyword|Keyword"), 
                        "removed")) %>%
  count(desc, sort = TRUE) %>%
  mutate(percentage_overall = round((n/sum(n))*100, 5))

class(dataTidy) <- c(class(dataTidy), "OSMtidy_dataTidy")

# Export output similar to OSMtidy step 6 dataTidy
# saveRDS(dataTidy, "Edinburgh/FINAL_Edinburgh_6_dataTidy-post-cleaning_20210826.RDS")


# 5. POST-PROCESSING ------------------------------------------------------

# If needed, remove all objects except dataTidy
gdata::keep(dataTidy, sure = TRUE)

# If needed, read in dataTidy again
# dataTidy <- readRDS("Edinburgh/FINAL_Edinburgh_6_dataTidy-post-cleaning_20210806.RDS")

# Extract filtered data
dg_og <- dataTidy$filtered

# Ensure filepath name and file/location name are specified
path <- "Edinburgh/"
name <- "FINAL_Edinburgh"

# Make chosen data valid
dg <- dg_og %>% makeValid()
# Inspect step 6 filtered data
dg

# The output could be exported now using exportOSMtidy()
# Or after application of the five simplifying functions

# Simplify airport and helipad data
dg <- dg %>% simplifyAirports
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Check for 'Airports (to validate)'
dg %>% filter(desc == "Airports (to validate)")
# Check OpenStreetMap/GoogleMaps coordinates of any 'Airports (to validate)'
# If needed change this to appropriate desc e.g. (only an example!)
dg <- dg %>% mutate(desc = str_replace(desc, "Airports \\(to validate\\)", "Public transport; Helipad"))
# Check it's worked
dg %>% filter(str_detect(desc, "irport|elipad"))

# Simplify golf course data
dg <- dg %>% simplifyGolf
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify rail station data
dg <- dg %>% simplifyRail
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Ignore error: "No preset descNew has been found for this object type. Ensure you have specified a descNew argument."
# Inspect simplified data
dg %>% filter(str_detect(desc, "Public transport; Rail station"))

# Simplify water and wastewater treatment works data
dg <- dg %>% simplifyTreatment
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify events venues e.g. stadiums data
dg <- dg %>% simplifyVenues
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Ensure filepath and file/location name are specified
path
name

# Export the final output from OSMtidy in three formats
# Specify output object (dg = __)
# Specify filepath name (path = __)
# Specify file/location name (name = __)
# Specify if you want output /not/ in simple feature format (sf = FALSE)
# Specify file format (ext = __)
exportOSMtidy(dg, path, name, ext = ".RDS") # Gives output as RDS in simple feature format
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".RDS") # Gives output as RDS in dataframe format (easier for later data wrangling)
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".csv") # Gives output as CSV in dataframe format
exportOSMtidy(dg, path, name, ext = ".shp") # Gives output as shapefile










### LONDON CENTRAL ### 

# 1. READ IN DATA ---------------------------------------------------------

# Read in most current OSMAH key
# This is the most current set of desc terms to check against
OSMAHkey = read_csv("OSM-AH-key_2.0_20210630.csv")

# Read in inspected desc file
inspected <- readRDS("~descCheck/descInspect_20210604_20210806.RDS") ## Specify

### LONDON CENTRAL FILES ###
# FINAL_London-Central_4_dataWrangle-noDetail_20200527-174407_20210616_20210806.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_London-Central_5_dataFilter-unfiltered_20200527-232509_20210614.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_London-Central_5.5_dataTidy-filtered-updated_20210604_20210806_20210826.RDS # filtered file with updated desc terms
# FINAL_London-Central_5_dataFilter-validate_20200527-232538_20210806.xlsx # validated file

# Read in noDetail file
noDetail <- function_loadXL(
  wb_source = "London Central/FINAL_London-Central_4_dataWrangle-noDetail_20200527-174407_20210616_20210806.xlsx") ## Specify

# Read in unfiltered file
unfiltered <- function_loadXL(
  wb_source = "London Central/FINAL_London-Central_5_dataFilter-unfiltered_20200527-232509_20210614.xlsx") ## Specify

# Read in RDS files for filtered data
filtered <- readRDS("London Central/FINAL_London-Central_5.5_dataTidy-filtered-updated_20210604_20210806_20210826.RDS") ## Specify

# Read in unfiltered file
validated <- function_loadXL(
  wb_source = "London Central/FINAL_London-Central_5_dataFilter-validate_20200527-232538_20210806.xlsx") ## Specify


# 1. TIDY DATA ------------------------------------------------------------

### NO DETAIL ###
# Create and format single dataframe
noDetail <-
  lapply(noDetail, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>% 
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(noDetail) <- c()


### UNFILTERED ###
# Create and format single dataframe
unfiltered <-
  lapply(unfiltered, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(unfiltered) <- c()


### VALIDATED ###
# Create and format single dataframe
validated <-
  lapply(validated, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(validated) <- c()


### FILTERED ###
# Check already in correct format
filtered


### INSPECTED ### 
# Create and format single dataframe (for London Central only)
inspected_LondonCen <- 
  inspected$LondonCentral %>%
  select(osm_id, desc, geometry) %>%
  mutate(geometry = sf::st_as_sfc(geometry))


# 3. FIND & REPLACE DEPRECATED DESC ---------------------------------------

### ALL ###

# Create single dataframe for most data
all <- rbind(noDetail, unfiltered, filtered, validated)

# Create vector of osm_id for inspected and desc-reassigned rows to replace in 'all'
osm_id_toReplace <- inspected_LondonCen %>% pull(osm_id)

# Replace some rows with inspected desc and just in case, remove duplicated rows
all_step2 <-
  all %>%
  filter(!osm_id %in% osm_id_toReplace) %>%
  rbind(inspected_LondonCen) %>%
  unique()


### FINAL CHECK DESC ARE ALL UP-TO-DATE ###

# Find all deprecatedDesc in this set of OSMtidy outputs
currentDesc <- OSMAHkey %>% drop_na(desc) %>% select(desc) %>% unique %>% pull

deprecatedDesc <- 
  all_step2 %>% 
  #      sf::st_drop_geometry() %>% # only needed if going from post-processing sf objects
  select(desc) %>%
  filter(!desc %in% currentDesc) %>% 
  unique()
# All that should remain are desc = 
# NA; 
# any phrase including "remove", "Remove", "keyword" or "Keyword"
# "Airport; Aerodrome, terminal and gates" (these get taken care of in postProcessing steps)
# "Transport infrastructure; Airport apron, runways and taxiways" (these get taken care of in postProcessing steps)

# Harbours
# validated %>% filter(desc == "Harbours")
# should be changed to "Industry; Harbour" manually in validated file

# Social facility; Food bank
# validated %>% filter(desc == "Social facility; Food bank")
# should be changed to "Social facility; Food banks and soup kitchens" manually in validated file

# Telecommunications; Retail
# validated %>% filter(desc == "Telecommunications; Retail")
# should be changed to "Telecommunications; Office" manually in validated file

# Healthcare; Alternative medicine
# Check same osm_id to avoid re-validating double what is needed
# filtered_health = filtered %>% filter(desc == "Healthcare; Alternative medicine") %>% pull(osm_id)
# inspected_health = inspected_LondonCen %>% filter(desc == "Healthcare; Alternative medicine") %>% pull(osm_id)
# filtered_health %in% inspected_health
# Revalidate inspected_LondonCen %>% filter(desc == "Healthcare; Alternative medicine")
# Should be validated case-by-case, however when attempting revalidation noticed a sample of ~10/50 were all Healthcare; Specialist; think I just reassigned the incorrect desc here in the inspected xlsx
# All can be assigned "Healthcare; Specialist"

# filtered_updated <-
#   filtered %>%
#   mutate(desc = replace(desc, 
#                        desc == "Healthcare; Alternative medicine", 
#                        "Healthcare; Specialist"))

# filtered_updated %>% filter(desc == "Healthcare; Alternative medicine")

# saveRDS(filtered_updated, "London Central/FINAL_London-Central_6_dataTidy-filtered-updated_20210604_20210806.RDS")


# inspected$LondonCentral <-
#   inspected$LondonCentraldataWrangle %>%
#   mutate(desc = replace(desc, 
#                         desc == "Healthcare; Alternative medicine", 
#                         "Healthcare; Specialist"))

# saveRDS(inspected, "~descCheck/descInspect_20210604_20210806.RDS")


# Food production and horticulture; Agriculture needs changed to Brownfield sites as filter incorrectly assigned
# filtered %>% filter(desc == "Food production and horticulture; Agriculture")

# filtered_updated <-
#   filtered %>%
#   mutate(desc = replace(desc, 
#                         desc == "Food production and horticulture; Agriculture", 
#                         "Brownfield sites"))

# saveRDS(filtered_updated, "London Central/FINAL_London-Central_6_dataTidy-filtered-updated_20210604_20210806_20210826.RDS")


# Double check there are no duplicated rows
check_rows = all_step2[duplicated(all_step2) | duplicated(all_step2, fromLast=TRUE), ]
# 0 items

# Find rows with osm_id that are duplicated
check_osm = all_step2[duplicated(all_step2$osm_id) | duplicated(all_step2$osm_id, fromLast=TRUE), ]
# 261782 items

# Find rows with osm_id that are duplicated that have an entry for desc
check_osm_notNA = check_osm %>% filter(!is.na(desc))
# 130919 items

# Find rows with osm_id that are duplicated, that have an entry for desc, that are duplicates
check_osm_notNA_duplicated = check_osm_notNA[duplicated(check_osm_notNA$osm_id) | duplicated(check_osm_notNA$osm_id, fromLast=TRUE), ]
# 58 (remaining have same osm_id but different geometries)

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA)
check_osm_NA = check_osm %>% filter(is.na(desc))
# 130863 items

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA), that are duplicates
check_osm_NA_duplicated = check_osm_NA[duplicated(check_osm_NA$osm_id) | duplicated(check_osm_NA$osm_id, fromLast=TRUE), ]
# 0 items


# By this check, we can assume all duplicate osm_id rows are ones with NA values 
# and these will be removed via OSMtidy step 6 function dataTidy or similar step

# Export before removing NA or remove rows just in case
# saveRDS(all_step2, "London Central/FINAL_London-Central_6_dataTidy-pre-cleaning_20210826.RDS")


# 4. CREATE DATATIDY-STYLE OUTPUT -----------------------------------------

dataTidy <- list()

dataTidy$filtered <-
  all_step2 %>%
  filter(!is.na(desc)) %>%
  filter(!str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$removed <-
  all_step2 %>%
  filter(is.na(desc) | str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$summary <- 
  all_step2 %>%
  mutate(desc = replace(desc, 
                        is.na(desc) | 
                          str_detect(desc, "remove|Remove|keyword|Keyword"), 
                        "removed")) %>%
  count(desc, sort = TRUE) %>%
  mutate(percentage_overall = round((n/sum(n))*100, 5))

class(dataTidy) <- c(class(dataTidy), "OSMtidy_dataTidy")

# Export output similar to OSMtidy step 6 dataTidy
# saveRDS(dataTidy, "London Central/FINAL_London-Central_6_dataTidy-post-cleaning_20210826.RDS")


# 5. POST-PROCESSING ------------------------------------------------------

# If needed, remove all objects except dataTidy
gdata::keep(dataTidy, sure = TRUE)

# If needed, read in dataTidy again
# dataTidy <- readRDS("London Central/FINAL_London-Central_6_dataTidy-post-cleaning_20210806.RDS")

# Extract filtered data
dg_og <- dataTidy$filtered

# Ensure filepath name and file/location name are specified
path <- "London Central/"
name <- "FINAL_London-Central"

# Make chosen data valid
dg <- dg_og %>% makeValid()
# Inspect step 6 filtered data
dg

# The output could be exported now using exportOSMtidy()
# Or after application of the five simplifying functions

# Simplify airport and helipad data
dg <- dg %>% simplifyAirports
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Check for 'Airports (to validate)'
dg %>% filter(desc == "Airports (to validate)")
# Check OpenStreetMap/GoogleMaps coordinates of any 'Airports (to validate)'
# If needed change this to appropriate desc e.g. (only an example!)
dg <- dg %>% mutate(desc = str_replace(desc, "Airports \\(to validate\\)", "Public transport; Helipad"))
# Check it's worked
dg %>% filter(str_detect(desc, "irport|elipad"))

# Simplify golf course data
dg <- dg %>% simplifyGolf
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify rail station data
dg <- dg %>% simplifyRail
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Ignore error: "No preset descNew has been found for this object type. Ensure you have specified a descNew argument."
# Inspect simplified data
dg %>% filter(str_detect(desc, "Public transport; Rail station"))

# Simplify water and wastewater treatment works data
dg <- dg %>% simplifyTreatment
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify events venues e.g. stadiums data
dg <- dg %>% simplifyVenues
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Ensure filepath and file/location name are specified
path
name

# Export the final output from OSMtidy in three formats
# Specify output object (dg = __)
# Specify filepath name (path = __)
# Specify file/location name (name = __)
# Specify if you want output /not/ in simple feature format (sf = FALSE)
# Specify file format (ext = __)
exportOSMtidy(dg, path, name, ext = ".RDS") # Gives output as RDS in simple feature format
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".RDS") # Gives output as RDS in dataframe format (easier for later data wrangling)
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".csv") # Gives output as CSV in dataframe format
exportOSMtidy(dg, path, name, ext = ".shp") # Gives output as shapefile










##### GLASGOW APPLICATION #####

# 1. READ IN DATA ---------------------------------------------------------

# Read in most current OSMAH key
# This is the most current set of desc terms to check against
OSMAHkey = read_csv("OSM-AH-key_2.0_20210630.csv")

# Read in inspected desc file
inspected <- readRDS("~descCheck/descInspect_20210604_20210806.RDS") ## Specify

### GLASGOW FILES ###
# FINAL_Glasgow_4_dataWrangle-noDetail_20210112-154522_20210615_20210805.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_Glasgow_5_dataFilter-unfiltered_20210224-163843.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_Glasgow_5.5_dataTidy-filtered-updated_20210604_20210805_20210809_20210827.RDS # filtered file with updated desc terms
# FINAL_Glasgow_5_dataFilter-validate_20210414_KM_20210809.xlsx # validated file

# Read in noDetail file
noDetail <- function_loadXL(
  wb_source = "Glasgow/FINAL_Glasgow_4_dataWrangle-noDetail_20210112-154522_20210615_20210805.xlsx") ## Specify

# Read in unfiltered file
unfiltered <- function_loadXL(
  wb_source = "Glasgow/FINAL_Glasgow_5_dataFilter-unfiltered_20210224-163843.xlsx") ## Specify

# Read in RDS files for filtered data
filtered <- readRDS("Glasgow/FINAL_Glasgow_5.5_dataTidy-filtered-updated_20210604_20210805_20210809_20210827.RDS") ## Specify

# Read in unfiltered file
validated <- function_loadXL(
  wb_source = "Glasgow/FINAL_Glasgow_5_dataFilter-validate_20210414_KM_20210809.xlsx") ## Specify


# 1. TIDY DATA ------------------------------------------------------------

### NO DETAIL ###
# Create and format single dataframe
noDetail <-
  lapply(noDetail, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>% 
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(noDetail) <- c()


### UNFILTERED ###
# Create and format single dataframe
unfiltered <-
  lapply(unfiltered, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(unfiltered) <- c()


### VALIDATED ###
# Create and format single dataframe
validated <-
  lapply(validated, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(validated) <- c()


### FILTERED ###
# Check already in correct format
filtered


### INSPECTED ### 
# Create and format single dataframe (for Glasgow only)
inspected_Glasgow <- 
  inspected$Glasgow %>%
  select(osm_id, desc, geometry) %>%
  mutate(geometry = sf::st_as_sfc(geometry))


# 3. FIND & REPLACE DEPRECATED DESC ---------------------------------------

### ALL ###

# Create single dataframe for most data
all <- rbind(noDetail, unfiltered, filtered, validated)

# Create vector of osm_id for inspected and desc-reassigned rows to replace in 'all'
osm_id_toReplace <- inspected_Glasgow %>% pull(osm_id)

# Replace some rows with inspected desc and just in case, remove duplicated rows
all_step2 <-
  all %>%
  filter(!osm_id %in% osm_id_toReplace) %>%
  rbind(inspected_Glasgow) %>%
  unique()


### FINAL CHECK DESC ARE ALL UP-TO-DATE ###

# Find all deprecatedDesc in this set of OSMtidy outputs
currentDesc <- OSMAHkey %>% drop_na(desc) %>% select(desc) %>% unique %>% pull

deprecatedDesc <- 
  all_step2 %>% 
  #      sf::st_drop_geometry() %>% # only needed if going from post-processing sf objects
  select(desc) %>%
  filter(!desc %in% currentDesc) %>% 
  unique()
# All that should remain are desc = 
# NA; 
# any phrase including "remove", "Remove", "keyword" or "Keyword"
# Airport; Aerodrome, terminal and gates (these get taken care of in postProcessing steps)
# Transport infrastructure; Airport apron, runways and taxiways (these get taken care of in postProcessing steps)

# Healthcare; Alternative medicine
# filtered %>% filter(desc == "Healthcare; Alternative medicine")
# id 2271894240 = "Retail; Speciality and luxury goods"
# id 49807713 = "Healthcare; Specialist"
# id 5254214146 = "Healthcare; Specialist"
# id 6797611005 = "Trades and services; Beauty industry"

# Manufacturing; Food production and horticulture
# filtered %>% filter(desc == "Manufacturing; Food production and horticulture")
# change to "Food production and horticulture; Manufacturing"

# filtered_updated <-
#   filtered %>%
#   mutate(desc = replace(desc, 
#                       osm_id == 2271894240, 
#                       "Retail; Speciality and luxury goods"))

# filtered_updated2 <-
#   filtered_updated %>%
#   mutate(desc = replace(desc, 
#                         osm_id == 49807713, 
#                         "Healthcare; Specialist"))

# filtered_updated3 <-
#   filtered_updated2 %>%
#   mutate(desc = replace(desc, 
#                         osm_id == 6797611005, 
#                         "Healthcare; Specialist"))

# filtered_updated4 <-
#   filtered_updated3 %>%
#   mutate(desc = replace(desc, 
#                         osm_id == 5254214146, 
#                         "Trades and services; Beauty industry"))

# filtered_updated5 <-
#   filtered_updated4 %>%
#   mutate(desc = replace(desc, 
#                         desc == "Manufacturing; Food production and horticulture", 
#                         "Food production and horticulture; Manufacturing"))

# filtered_updated5 %>% filter(desc == "Healthcare; Alternative medicine")
# filtered_updated5 %>% filter(desc == "Manufacturing; Food production and horticulture")

# saveRDS(filtered_updated5, "Glasgow/Glasgow_6_dataTidy-filtered-updated_20210604_20210805.RDS")

# Make one-time adjustment to military-related items
# Replace desc terms for Military; Land & Military; training grounds with Military; Administration
# validated %>% filter(str_detect(desc, "Military; Land"))
# validated %>% filter(str_detect(desc, "Military; Training grounds"))
# validated can be changed manually in Excel spreadsheet
# filtered %>% filter(str_detect(desc, "Military; Land"))
# filtered %>% filter(str_detect(desc, "Military; Training grounds"))

# filtered_updated6 = 
#   filtered %>%
#   mutate(desc =
#            desc %>% 
#            replace(., grepl('Military; Land', .), "Military; Administration") %>%
#            replace(., grepl('Military; Training grounds', .), "Military; Administration"))

# filtered_updated6 %>% filter(str_detect(desc, "Military; Land"))
# filtered_updated6 %>% filter(str_detect(desc, "Military; Training grounds"))
# filtered_updated6 %>% filter(str_detect(desc, "Military; Administration"))

# saveRDS(filtered_updated6, "Glasgow/FINAL_Glasgow_5.5_dataTidy-filtered-updated_20210604_20210805_20210809.RDS")

# inspected$Glasgow <-
#   inspected$Glasgow %>%
#   mutate(desc = replace(desc,
#                         desc == "Manufacturing; Food production and horticulture", 
#                         "Food production and horticulture; Manufacturing"))

# saveRDS(inspected, "~descCheck/descInspect_20210604_20210806.RDS")

# Food production and horticulture; Agriculture needs changed to Brownfield sites as filter incorrectly assigned
# filtered %>% filter(desc == "Food production and horticulture; Agriculture")

# filtered_updated <-
#  filtered %>%
#  mutate(desc = replace(desc, 
#                        desc == "Food production and horticulture; Agriculture", 
#                        "Brownfield sites"))

# saveRDS(filtered_updated, "Glasgow/FINAL_Glasgow_6_dataTidy-filtered-updated_20210604_20210805_20210809_20210827.RDS")


# Double check there are no duplicated rows
check_rows = all_step2[duplicated(all_step2) | duplicated(all_step2, fromLast=TRUE), ]
# 0 items

# Find rows with osm_id that are duplicated
check_osm = all_step2[duplicated(all_step2$osm_id) | duplicated(all_step2$osm_id, fromLast=TRUE), ]
# 33712 items

# Find rows with osm_id that are duplicated that have an entry for desc
check_osm_notNA = check_osm %>% filter(!is.na(desc))
# 16857 items

# Find rows with osm_id that are duplicated, that have an entry for desc, that are duplicates
check_osm_notNA_duplicated = check_osm_notNA[duplicated(check_osm_notNA$osm_id) | duplicated(check_osm_notNA$osm_id, fromLast=TRUE), ]
# 2 items (both remove)

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA)
check_osm_NA = check_osm %>% filter(is.na(desc))
# 16855 items

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA), that are duplicates
check_osm_NA_duplicated = check_osm_NA[duplicated(check_osm_NA$osm_id) | duplicated(check_osm_NA$osm_id, fromLast=TRUE), ]
# 0 items


# By this check, we can assume all duplicate osm_id rows are ones with NA values 
# and these will be removed via OSMtidy step 6 function dataTidy or similar step

# Export before removing NA or remove rows just in case
# saveRDS(all_step2, "Glasgow/FINAL_Glasgow_6_dataTidy-pre-cleaning_20210827.RDS")


# 4. CREATE DATATIDY-STYLE OUTPUT -----------------------------------------

dataTidy <- list()

dataTidy$filtered <-
  all_step2 %>%
  filter(!is.na(desc)) %>%
  filter(!str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$removed <-
  all_step2 %>%
  filter(is.na(desc) | str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$summary <- 
  all_step2 %>%
  mutate(desc = replace(desc, 
                        is.na(desc) | 
                          str_detect(desc, "remove|Remove|keyword|Keyword"), 
                        "removed")) %>%
  count(desc, sort = TRUE) %>%
  mutate(percentage_overall = round((n/sum(n))*100, 5))

class(dataTidy) <- c(class(dataTidy), "OSMtidy_dataTidy")

# Export output similar to OSMtidy step 6 dataTidy
# saveRDS(dataTidy, "Glasgow/FINAL_Glasgow_6_dataTidy-post-cleaning_20210827.RDS")


# 5. POST-PROCESSING ------------------------------------------------------

# If needed, remove all objects except dataTidy
gdata::keep(dataTidy, sure = TRUE)

# If needed, read in dataTidy again
# dataTidy <- readRDS("Glasgow/FINAL_Glasgow_6_dataTidy-post-cleaning_20210809.RDS")

# Extract filtered data
dg_og <- dataTidy$filtered

# Ensure filepath name and file/location name are specified
path <- "Glasgow/"
name <- "FINAL_Glasgow"

# Make chosen data valid
dg <- dg_og %>% makeValid()
# Ignore warning: "Spherical geometry (s2) switched off"
# Inspect step 6 filtered data
dg

# The output could be exported now using exportOSMtidy()
# Or after application of the five simplifying functions

# Simplify airport and helipad data
dg <- dg %>% simplifyAirports
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Check for 'Airports (to validate)'
dg %>% filter(desc == "Airports (to validate)")
# Check OpenStreetMap/GoogleMaps coordinates of any 'Airports (to validate)'
# If needed change this to appropriate desc e.g. (only an example!)
dg <- dg %>% mutate(desc = str_replace(desc, "Airports \\(to validate\\)", "Public transport; Helipad"))
# Check it's worked
dg %>% filter(str_detect(desc, "irport|elipad"))

# Simplify golf course data
dg <- dg %>% simplifyGolf
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify rail station data
dg <- dg %>% simplifyRail
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Ignore error: "No preset descNew has been found for this object type. Ensure you have specified a descNew argument."
# Inspect simplified data
dg %>% filter(str_detect(desc, "Public transport; Rail station"))

# Simplify water and wastewater treatment works data
dg <- dg %>% simplifyTreatment
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify events venues e.g. stadiums data
dg <- dg %>% simplifyVenues
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Ensure filepath and file/location name are specified
path
name

# Export the final output from OSMtidy in three formats
# Specify output object (dg = __)
# Specify filepath name (path = __)
# Specify file/location name (name = __)
# Specify if you want output /not/ in simple feature format (sf = FALSE)
# Specify file format (ext = __)
exportOSMtidy(dg, path, name, ext = ".RDS") # Gives output as RDS in simple feature format
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".RDS") # Gives output as RDS in dataframe format (easier for later data wrangling)
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".csv") # Gives output as CSV in dataframe format
exportOSMtidy(dg, path, name, ext = ".shp") # Gives output as shapefile










##### MANCHESTER APPLICATION #####

# 1. READ IN DATA ---------------------------------------------------------

# Read in most current OSMAH key
# This is the most current set of desc terms to check against
OSMAHkey = read_csv("OSM-AH-key_2.0_20210630.csv")

# Read in inspected desc file
inspected <- readRDS("~descCheck/descInspect_20210604_20210806.RDS") ## Specify

### MANCHESTER FILES ###
# FINAL_Manchester_4_dataWrangle-noDetail_20200522-131433_20210616_20210727.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_Manchester_5_dataFilter-unfiltered_20200528-214239_20210614.xlsx # unfiltered file with additional items including Buildings; Unspecified
# FINAL_Manchester_5.5_dataTidy-filtered-updated_20210604_20210827.RDS # filtered file with updated desc terms
# FINAL_Manchester_5_dataFilter-validate_20200528-214254_20210727.xlsx # validated file

# Read in noDetail file
noDetail <- function_loadXL(
  wb_source = "Manchester/FINAL_Manchester_4_dataWrangle-noDetail_20200522-131433_20210616_20210727.xlsx") ## Specify

# Read in unfiltered file
unfiltered <- function_loadXL(
  wb_source = "Manchester/FINAL_Manchester_5_dataFilter-unfiltered_20200528-214239_20210614.xlsx") ## Specify

# Read in RDS files for filtered data
filtered <- readRDS("Manchester/FINAL_Manchester_5.5_dataTidy-filtered-updated_20210604_20210827.RDS") ## Specify

# Read in unfiltered file
validated <- function_loadXL(
  wb_source = "Manchester/FINAL_Manchester_5_dataFilter-validate_20200528-214254_20210727.xlsx") ## Specify


# 1. TIDY DATA ------------------------------------------------------------

### NO DETAIL ###
# Create and format single dataframe
noDetail <-
  lapply(noDetail, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>% 
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(noDetail) <- c()


### UNFILTERED ###
# Create and format single dataframe
unfiltered <-
  lapply(unfiltered, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(unfiltered) <- c()


### VALIDATED ###
# Create and format single dataframe
validated <-
  lapply(validated, function(x) { x %>% select(osm_id, desc, geometry) }) %>%
  do.call("rbind", .) %>%
  as.data.frame() %>%
  mutate(geometry = sf::st_as_sfc(geometry))
rownames(validated) <- c()


### FILTERED ###
# Check already in correct format
filtered


### INSPECTED ### 
# Create and format single dataframe (for Manchester only)
inspected_Manchester <- 
  inspected$ManchesterdataWrangle %>%
  select(osm_id, desc, geometry) %>%
  mutate(geometry = sf::st_as_sfc(geometry))


# 3. FIND & REPLACE DEPRECATED DESC ---------------------------------------

### ALL ###

# Create single dataframe for most data
all <- rbind(noDetail, unfiltered, filtered, validated)

# Create vector of osm_id for inspected and desc-reassigned rows to replace in 'all'
osm_id_toReplace <- inspected_Manchester %>% pull(osm_id)

# Replace some rows with inspected desc and just in case, remove duplicated rows
all_step2 <-
  all %>%
  filter(!osm_id %in% osm_id_toReplace) %>%
  rbind(inspected_Manchester) %>%
  unique()


### FINAL CHECK DESC ARE ALL UP-TO-DATE ###

# Find all deprecatedDesc in this set of OSMtidy outputs
currentDesc <- OSMAHkey %>% drop_na(desc) %>% select(desc) %>% unique %>% pull

deprecatedDesc <- 
    all_step2 %>% 
    #      sf::st_drop_geometry() %>% # only needed if going from post-processing sf objects
    select(desc) %>%
    filter(!desc %in% currentDesc) %>% 
    unique()
# All that should remain are desc = 
# NA; 
# any phrase including "remove"
# Airport; Aerodrome, terminal and gates (these get taken care of in postProcessing steps)
# Transport infrastructure; Airport apron, runways and taxiways (these get taken care of in postProcessing steps)


# Food production and horticulture; Agriculture needs changed to Brownfield sites as filter incorrectly assigned
# filtered %>% filter(desc == "Food production and horticulture; Agriculture")

# filtered_updated <-
#   filtered %>%
#   mutate(desc = replace(desc, 
#                         desc == "Food production and horticulture; Agriculture", 
#                         "Brownfield sites"))

# saveRDS(filtered_updated, "Manchester/FINAL_Manchester_5.5_dataTidy-filtered-updated_20210604_20210827.RDS")


# Double check there are no duplicated rows
check_rows = all_step2[duplicated(all_step2) | duplicated(all_step2, fromLast=TRUE), ]
# 0 items

# Find rows with osm_id that are duplicated
check_osm = all_step2[duplicated(all_step2$osm_id) | duplicated(all_step2$osm_id, fromLast=TRUE), ]
# 20798 items

# Find rows with osm_id that are duplicated that have an entry for desc
check_osm_notNA = check_osm %>% filter(!is.na(desc))
# 10399 items

# Find rows with osm_id that are duplicated, that have an entry for desc, that are duplicates
check_osm_notNA_duplicated = check_osm_notNA[duplicated(check_osm_notNA$osm_id) | duplicated(check_osm_notNA$osm_id, fromLast=TRUE), ]
# 0 items

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA)
check_osm_NA = check_osm %>% filter(is.na(desc))
# 10399 items

# Find rows with osm_id that are duplicated, that do not have an entry for desc (are NA), that are duplicates
check_osm_NA_duplicated = check_osm_NA[duplicated(check_osm_NA$osm_id) | duplicated(check_osm_NA$osm_id, fromLast=TRUE), ]
# 0 items


# By this check, we can assume all duplicate osm_id rows are ones with NA values 
# and these will be removed via OSMtidy step 6 function dataTidy or similar step

# Export before removing NA or remove rows just in case
# saveRDS(all_step2, "Manchester/FINAL_Manchester_6_dataTidy-pre-cleaning_20210827.RDS")


# 4. CREATE DATATIDY-STYLE OUTPUT -----------------------------------------

dataTidy <- list()

dataTidy$filtered <-
  all_step2 %>%
  filter(!is.na(desc)) %>%
  filter(!str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$removed <-
  all_step2 %>%
  filter(is.na(desc) | str_detect(desc, "remove|Remove|keyword|Keyword"))

dataTidy$summary <- 
  all_step2 %>%
  mutate(desc = replace(desc, 
                        is.na(desc) | 
                          str_detect(desc, "remove|Remove|keyword|Keyword"), 
                        "removed")) %>%
  count(desc, sort = TRUE) %>%
  mutate(percentage_overall = round((n/sum(n))*100, 5))

class(dataTidy) <- c(class(dataTidy), "OSMtidy_dataTidy")

# Export output similar to OSMtidy step 6 dataTidy
# saveRDS(dataTidy, "Manchester/FINAL_Manchester_6_dataTidy-post-cleaning_20210827.RDS")


# 5. POST-PROCESSING ------------------------------------------------------

# If needed, remove all objects except dataTidy
gdata::keep(dataTidy, sure = TRUE)

# If needed, read in dataTidy again
# dataTidy <- readRDS("Manchester/FINAL_Manchester_6_dataTidy-post-cleaning_20210728.RDS")

# Extract filtered data
dg_og <- dataTidy$filtered

# Ensure filepath name and file/location name are specified
path <- "Manchester/"
name <- "FINAL_Manchester"

# Make chosen data valid
dg <- dg_og %>% makeValid()
# Inspect step 6 filtered data
dg

# The output could be exported now using exportOSMtidy()
# Or after application of the five simplifying functions

# Simplify airport and helipad data
dg <- dg %>% simplifyAirports
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Check for 'Airports (to validate)'
dg %>% filter(desc == "Airports (to validate)")
# Check OpenStreetMap/GoogleMaps coordinates of any 'Airports (to validate)'
# If needed change this to appropriate desc e.g. (only an example!)
dg <- dg %>% mutate(desc = str_replace(desc, "Airports \\(to validate\\)", "Public transport; Helipad"))
# Check it's worked
dg %>% filter(str_detect(desc, "irport|elipad"))

# Simplify golf course data
dg <- dg %>% simplifyGolf
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify rail station data
dg <- dg %>% simplifyRail
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Ignore error: "No preset descNew has been found for this object type. Ensure you have specified a descNew argument."
# Inspect simplified data
dg %>% filter(str_detect(desc, "Public transport; Rail station"))

# Simplify water and wastewater treatment works data
dg <- dg %>% simplifyTreatment
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Simplify events venues e.g. stadiums data
dg <- dg %>% simplifyVenues
# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Ensure filepath and file/location name are specified
path
name

# Export the final output from OSMtidy in three formats
# Specify output object (dg = __)
# Specify filepath name (path = __)
# Specify file/location name (name = __)
# Specify if you want output /not/ in simple feature format (sf = FALSE)
# Specify file format (ext = __)
exportOSMtidy(dg, path, name, ext = ".RDS") # Gives output as RDS in simple feature format
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".RDS") # Gives output as RDS in dataframe format (easier for later data wrangling)
exportOSMtidy(dg, path, name, sf = FALSE, ext = ".csv") # Gives output as CSV in dataframe format
exportOSMtidy(dg, path, name, ext = ".shp") # Gives output as shapefile




# NOTES -------------------------------------------------------------------

PO_EC = USAH_2.0_template_baseline_20210719$results %>% filter(level == 5, type != "WVBC") %>% arrange(centrality) %>% mutate(centrality = centrality %>% as.character)
check = USAH_2.0_template_baseline_20210719$edgelist %>% filter(layer == "l4ORP_l5PO") %>%
  filter(to == "Airports (major)")

ORP_EC = USAH_2.0_template_baseline_20210719$results %>% filter(level == 4, type != "WVBC") %>% arrange(centrality) %>% mutate(centrality = centrality %>% as.character)
checkDown = USAH_2.0_template_baseline_20210719$edgelist %>% filter(layer == "l4ORP_l5PO") %>%
  filter(from == "Support physical health and fitness")
checkUp = USAH_2.0_template_baseline_20210719$edgelist %>% filter(layer == "l3GF_l4ORP") %>%
  filter(to == "Support physical health and fitness")

check1 <-
  dg_og %>%
  st_as_sf(crs = 4326) %>%
  st_make_valid()

check2 <-
  check1 %>% mutate(type = st_geometry_type(geometry) %>% as.character)

check3 <-
  check2 %>%
  select(desc, type, geometry)

check4 <-
  check3 %>%
  split(., .$type)

check5 <-
  dg_og %>% rowwise %>% mutate(valid = st_is_valid(geometry))

check6 <- 
  list(check5 %>%
         filter(valid == FALSE) %>%
         ungroup %>%
         st_as_sf() %>%
         st_make_valid,
       check5 %>%
         filter(valid == TRUE) %>%
         st_as_sf %>%
         st_make_valid()) %>%
  .bind_rows_sf %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(type = st_geometry_type(geometry) %>% as.character) %>%
  select(desc = desc, type, geometry) %>%
  split(., .$type)

vec <- c("POINT", "LINESTRING", "POLYGON")

check7 <- 
  lapply(vec, function(x) {
    tryCatch(
      suppressWarnings(
        check6[["GEOMETRYCOLLECTION"]] %>%
          st_make_valid %>%
          st_collection_extract(x) %>%
          st_cast(x, warn = FALSE)
      ),
      error = function(e) NULL)
  })

check8 <-
  lapply(vec, function(x) {
    tryCatch(
      check6[[paste0("MULTI", x)]] %>%
        st_make_valid %>%
        st_cast(x, warn = FALSE),
      error = function(e) NULL)
  })

check9 <-
  lapply(vec, function(x) {
    tryCatch( check6[[x]]
              %>% st_make_valid,
              error = function(e) NULL)
  })

check10 <- c(check7, check8, check9)

check11 <-
  check10 %>%
  Filter(Negate(is.null), .)

check12 <-
  check11 %>%
  .bind_rows_sf()

check13 <-
  check12 %>%
  mutate(type = st_geometry_type(geometry))

##
check14 <-
  check13 %>% rowwise %>% mutate(valid = st_is_valid(geometry))

# https://r-spatial.github.io/sf/articles/sf7.html
# For S2, ring direction is essential. For that reason, st_as_s2 has an argument oriented = FALSE, which will check and correct ring directions, assuming that all exterior rings occupy an area smaller than half the globe

# https://github.com/r-spatial/sf/issues/1649
sf::sf_use_s2()
sf::sf_use_s2(FALSE)
sf::sf_use_s2()

# Check types
dg_count <- dg %>% countType(group = quo(desc), AHgen = FALSE)
# R^2 workaround effective for countType

# Troubleshooting .shp output
# https://stackoverflow.com/questions/60454067/cpl-write-ogr-error-when-writing-out-shape-file-in-r
# https://community.rstudio.com/t/issues-with-writing-a-dataframe-to-a-shapefile/42449


# Create vector of dg class(es)
class <- class(dg) %>% as.vector()

if("sf" %in% class) { # If dg class includes "sf" (simple feature)
  
  dg <- 
    dg %>% 
    as.data.frame() %>% 
    mutate(type = sf::st_geometry_type(geometry), # Add column for geometry type
           geometry = sf::st_as_text(geometry)) # Reformat geometry as character class for speedier wrangling
  
} else {
  
  dg <- dg %>% as.data.frame()
  
}
