# =========================================================================
# 2a - Post-hoc desc check.R
# Compatible with OSMtidy v0.0.5

# Created by: Dr Melissa Bedinger (m.bedinger@hw.ac.uk)
# Created: 2021-04-12

# Last revised by: Dr Melissa Bedinger (m.bedinger@hw.ac.uk)
# Last revised: 2021-07-20
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Prepare the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set working directory to the script's folder
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Ensure latest version of OSMtidy package is installed
devtools::install_github("avisserquinn/OSMtidy")

# Load the required packages
pacman::p_load(tidyverse, tools, data.table, OSMtidy, 
               sf, janitor, xlsx, writexl, readxl)


# 0.2. FUNCTIONS ----------------------------------------------------------

# Function to read in all RDS files from a specified folder
read_folder <- function(folderName) {
  
  outputs <- 
    list.files(path = folderName, pattern = ".RDS", full.names = TRUE)
  
  outputs <- 
    lapply(outputs, function (x) readRDS(x))
  
  outputNames <-
    list.files(path = folderName, pattern = ".RDS") %>% 
    tools::file_path_sans_ext()
  
  outputNames <- 
    gsub("[_0-9-]", "", outputNames)
  
  names(outputs) <- outputNames
  
  return(outputs)
  
}

# Function to find all deprecated desc in a set of OSMtidy outputs
find_deprecatedDesc <- function(OSMAHkey, outputsList) {
  
  currentDesc <- 
    OSMAHkey %>% drop_na(desc) %>% select(desc) %>% unique %>% pull
  
  find_deprecated <- function(x) {
    
    deprecatedDesc <- 
      x %>% 
#      sf::st_drop_geometry() %>% # only needed if going from post-processing sf objects
      select(desc) %>%
      filter(!desc %in% currentDesc) %>% 
      unique
    
    return(deprecatedDesc)
    
  }
  
  deprecatedDesc <- 
    lapply(outputsList, find_deprecated) %>%
    do.call("rbind", .) %>% 
    data.table::setDT(keep.rownames = TRUE) %>%
    separate(rn, c("scenario", "id"), sep = "[^[:alnum:]]+") %>%
    as_tibble()
  
  return(deprecatedDesc)
  
}

# Function to check if existing deprecatedDesc key requires review/additions 
# i.e. if newly deprecated desc arise from this set of OSMtidy outputs
check_dKey <- function(dKey, deprecatedDesc) {
  
  dKey_old <-  
    dKey %>%
    dplyr::rename(desc = deprecatedDesc)
  
  deprecatedCheck <- 
    deprecatedDesc %>% full_join(dKey_old, by = "desc")
  
  newlyDeprecated <- subset(deprecatedCheck,is.na(newDesc))
  
  if(nrow(newlyDeprecated) == 0) {
    
    return(print("All deprecated desc are already included in the key. Proceed!"))
    
  } else {
    
    check %>% 
      dplyr::rename(deprecatedDesc = desc) %>% 
      write.csv("./~descInspect/deprecatedDesc.csv")
    
    return(print("Please open updated deprecatedDesc.csv to review and assign newDesc before proceeding."))
    
  }
}

# Function to return deprecatedDesc requiring manual inspection/reassignment 
# for a single OSMtidy output
extract_toInspect <- function(wrangled, tidied) { # requires step 4 wrangled data & step 6 tidied data as inputs
  
  id_toInspect <-  
    tidied %>%
    left_join(replace_manual, by = "desc") %>% 
    filter(!is.na(newDesc)) %>% 
    select(osm_id, desc, newDesc) %>%
    dplyr::rename(reason = newDesc)
  
  id_toInspect_vec <- 
    id_toInspect %>% select(osm_id) %>% pull(osm_id)
  
  step1 <- 
    wrangled %>% 
    flatten %>% 
    lapply(function(x) {x %>% as.data.frame() %>% mutate(geometry = st_as_text(geometry))})
  
  step2 <- lapply(step1, function(x) {filter(x, osm_id %in% id_toInspect_vec)})
  
  step3 <- Filter(function(x) {nrow(x) > 0}, step2)
  
  step4 <- plyr::rbind.fill(lapply(step3, function(x) {as.data.frame((x),stringsAsFactors = FALSE)} ))
  
  step5 <- step4 %>% left_join(id_toInspect, by = "osm_id")
  
  step6 <- 
    step5 %>% 
    relocate(reason, osm_id, desc, geometry) %>% 
    janitor::remove_empty(which = "cols")
  
  return(step6)
  
}

# Function to assign newDesc from inspected and manually revalidated data 
# for a set of OSMtidy outputs
replace_newDesc_inspected <- function(inspected, toUpdate) {
  
  revalidated <-
    inspected %>%
    select(osm_id, desc, geometry) %>%
    mutate(geometry = st_as_sfc(geometry))
  
  revalidated_vec <- inspected %>% pull(osm_id)
  
  output <- 
    toUpdate %>% 
    select(osm_id, desc, geometry) %>%
    filter(!osm_id %in% revalidated_vec) %>%
    rbind(revalidated) %>%
    as.list()
  
  return(output)
  
}

# Function to automatically assign newDesc from replace_auto key
# for a set of OSMtidy outputs
replace_newDesc_automated <- function(input, replace_auto) {

  input <- input %>% as.data.frame()
  
  output <-
    left_join(input, replace_auto, by = "desc") %>% 
    mutate(desc = ifelse(is.na(newDesc), desc, newDesc)) %>% 
    select(-newDesc) %>%
    filter(!desc == "remove") %>%
    as.list()
  
  return(output)
  
}


# 0.3. CURRENT STATUS OF CITY OUTPUTS -------------------------------------

# Bristol, Edinburgh, Glasgow, London Central, Manchester complete
# require confirmation that no key object types are absent

# London Boroughs complete to end of step 5
# requires restitching

# Belfast requires step 1-7


# 1. READ IN DATA ---------------------------------------------------------

# Read in step 5 (filtered) OSMtidy outputs to be checked from ~descCheck folder
# This is the set of desc terms to check
outputs_step6 = read_folder(folderName = "outputs/Jakarta/Cakung/Unedited_current/Jakarta_Cakung_6_dataTidy-filtered_20240104-193907.RDS")

# Read in most current OSMAH key
# This is the most current set of desc terms to check against
OSMAHkey = read_csv("data/OSM-AH-adaptedkey_3.0_20230717.csv")

# Read in step 4 (wrangled) OSMtidy outputs from ~descCheck folder
# This will help provide extra information if any manual re-validation is needed
outputs_step4 = read_folder(folderName = "outputs/Jakarta/Cakung/Jakarta_Cakung_4_dataWrangle_20231212-000611.RDS")

# Manual tweak to structure of Glasgow step4-wrangled data make for consistent 
# processing across all city outputs
outputs_step4$GlasgowdataWrangle$noDetail <- 
  list(outputs_step4$GlasgowdataWrangle$noDetail)


# 2. REVIEW DEPRECATEDDESC FROM THIS SET OF OUTPUTS -----------------------

# Find all deprecatedDesc in this set of OSMtidy outputs
deprecatedDesc = 
  find_deprecatedDesc(OSMAHkey = OSMAHkey, outputsList = outputs_step6)


# 3. CREATE OR UPDATE DEPRECATEDDESCK KEY ---------------------------------

# For first round, create deprecatedDesc output to review/assign newDesc
# deprecatedDesc %>% 
#   select(desc) %>% 
#   unique %>% 
#   rename(deprecatedDesc = desc) %>% 
#   write.csv("deprecatedDesc.csv")

# For other rounds, read in existing deprecatedDesc key to build on
dKey = read_csv("deprecatedDesc.csv") %>% select(-X1) # Ignore warning message: Missing column names filled in

# Check if existing deprecatedDesc key requires additions
check_dKey(dKey = dKey, deprecatedDesc = deprecatedDesc)

# If needed use OSMAHkey to assign relevant newDesc as relevant
# Then read in dKey again
# dKey = 
#   read_csv("deprecatedDesc.csv") %>% select(-X1) # Ignore warning message: Missing column names filled in

# If dKey specifies newDesc as "NA; requires manual check" or 
# "updated post-processing", we do not want to automate reassignment of newDesc
# Create vector of newDesc to be ignored in automated reassignment
ignore = c("NA; requires manual check", "NA; requires updated post-processing")

# Create key for automated reassignment of newDesc
replace_auto =  
  dKey %>%
  dplyr::rename(desc = deprecatedDesc) %>%
  filter(!newDesc %in% ignore) %>% # removing the newDesc we want to manually inspect and re-assign
  select(desc, newDesc)

# Create key for manual revalidation of desc
# replace_manual =  
#   dKey %>%
#   dplyr::rename(desc = deprecatedDesc) %>%
#   filter(newDesc %in% ignore) %>% # taking only the newDesc we want to manually inspect and re-assign
#   select(desc, newDesc)


# 4. INSPECT AND MANUALLY REASSIGN NEWDESC TO SOME DEPRECATEDDESC ---------

# Get all objects for manual revalidation by applying extract_toInspect function
# Takes a while!
#  all = 
#   mapply(extract_toInspect, # function to apply
#          wrangled = outputs_step4, # input 1: wrangled data
#          tidied = outputs_step6) # input 2: tidied data

# Export xlsx for inspecting & manually revalidating desc
# all %>% writexl::write_xlsx("./~descInspect/descInspect_2021-06-04.xlsx") # Specify filename; make sure to change file name to avoid overwriting older files you want to keep
# For keeping separate cities up-to-date, would be better if exported as separate XLSX workbooks (not worksheets/tabs)


# 5. UPDATE ALL DEPRECATEDDESC WITH NEWDESC -------------------------------

# Re-import xlsx with inspected and revalidated newDesc
wb_source <- "./~descInspect/descInspect_2021-06-04.xlsx" # Specify

wb_sheets <- readxl::excel_sheets(wb_source) 

outputs_inspected <-
  wb_sheets %>%
  purrr::map(function(sheet){ # iterate through each sheet name
    readxl::read_xlsx(path = wb_source, sheet = sheet)
  })

names(outputs_inspected) <- wb_sheets

# Assign newDesc from inspected and manually revalidated data
outputs_updated_step1 = 
  mapply(replace_newDesc_inspected, # function to apply
         inspected = outputs_inspected, # input 1: inspected and revalidated data
         toUpdate = outputs_step6, # input 2: data to update
         SIMPLIFY = FALSE) # SIMPLIFY = FALSE returns output in list format (not large matrix format)

# Automatically assign newDesc from replace_auto key
outputs_updated_step2 = 
  lapply(X = outputs_updated_step1, # input 1: output updated with inspected and manually revalidated data
         replace_auto = replace_auto, # input 2: key of newDesc that do not need manual inspection and can be automatically reassigned
         FUN = replace_newDesc_automated) # function to apply

## think about also updating step 6 removed exports based on this



# 6. SAVE OUTPUTS ---------------------------------------------------------

# Went with more specific export names for post-hoc corrections
# outputs_updated_step2$BristoldataWrangle %>% as.data.frame() %>% 
#   write_rds("./Bristol/Bristol_6_dataTidy-filtered-updated_20210604.RDS")
# outputs_updated_step2$EdinburghdataWrangle %>% as.data.frame() %>% 
#   write_rds("./Edinburgh/Edinburgh_6_dataTidy-filtered-updated_20210604.RDS")
# outputs_updated_step2$GlasgowdataWrangle %>% as.data.frame() %>% 
#   write_rds("./Glasgow/Glasgow_6_dataTidy-filtered-updated_20210604.RDS")
# outputs_updated_step2$LondonCentraldataWrangle %>% as.data.frame() %>% 
#   write_rds("./London Central/London_Central_6_dataTidy-filtered-updated_20210604.RDS")
# outputs_updated_step2$ManchesterdataWrangle %>% as.data.frame() %>% 
#   write_rds("./Manchester/Manchester_6_dataTidy-filtered-updated_20210604.RDS")

# Normally you can export with exportOSMtidy in up to three formats
# exportOSMtidy(dg, locationName, path = "Glasgow/", ext = ".RDS") # Specify output object, locationName, filepath, and file extension type
# exportOSMtidy(dg, locationName, path = "Glasgow/", ext = ".csv") # Specify output object, locationName, filepath, and file extension type
# exportOSMtidy(dg, locationName, path = "Glasgow/", ext = ".shp") # Specify output object, locationName, filepath, and file extension type