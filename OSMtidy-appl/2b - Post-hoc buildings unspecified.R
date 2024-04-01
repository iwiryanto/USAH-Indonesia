# =========================================================================
# 2b - Post-hoc other corrections.R
# Compatible with OSMtidy v0.0.5

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2021-06-11
#
# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2021-07-20
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Ensure latest version of OSMtidy package is installed
devtools::install_github("avisserquinn/OSMtidy@dev")

# Load required packages
pacman::p_load(OSMtidy, progress, tidyverse, openxlsx, xlsx)

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# -------------------------------------------------------------------------

# BUILDINGS; UNSPECIFIED
# INCORPORATING CHANGES TO NODETAIL FILE

# Review dataWrangle$noDetail to assign desc terms for features where 
# confidence is high or small number can be manually validated
# e.g. 'Buildings; Unspecified', 'Electricity network; Substation'
city <- `Manchester_4_dataWrangle_20200522-131433`

noDetail_whole <- city$noDetail %>% do.call("rbind", .)

.rmCols <- function(input) { input %>% Filter(function(x) !all(is.na(x)), .) }

dataNoDetail <-
  noDetail_whole %>%
  as_tibble %>%
  mutate(geometryType = st_geometry_type(geometry),
         geometry = st_as_text(geometry),
         desc = "",
         feature = str_replace(feature, ":", "_")) %>%
  select(osm_id, desc, geometryType, geometry, everything()) %>%
  split(., .$feature) %>%
  modify(. %>% .rmCols())

exportExcel <- function(tibbleList, filename) {
  
  require(openxlsx)
  
  wb <- openxlsx::createWorkbook()
  
  input <- tibbleList
  
  # sheetnames <- paste0("Sheet", seq_along(input))
  sheetnames <- names(input) %>% str_sub(1,20)
  lsn = length(sheetnames)
  snid = .create_unique_ids(lsn, char_len = 3)
  sheetnames <- paste0(1:lsn, "_", snid, "_", sheetnames)
  
  Map(function(data, nameofsheet){
    
    openxlsx::addWorksheet(wb, nameofsheet)
    openxlsx::writeDataTable(wb, nameofsheet, data, rowNames = FALSE)
    
  }, input, sheetnames)
  
  openxlsx::saveWorkbook(wb, file = filename)
  
}

exportExcel(tibbleList = dataNoDetail, filename = "noDetail.xlsx")

# -------------------------------------------------------------------------

# BUILDINGS; UNSPECIFIED
# INCORPORATING CHANGES TO UNFILTERED FILE

# If needed re-import xlsx for dataFilter$unfiltered if anything has been
# manually changed (usually Sheet 1 has some Buildings; Unspecified)
# Re-import xlsx for dataWrangle$noDetail
# Join both to existing postProcessing file

wb_source <- 
  "../OSMtidy-appl/Manchester/Manchester_4_dataWrangle-noDetail_20200522-131433_20210616.xlsx" # Specify

wb_sheets <- readxl::excel_sheets(wb_source) 

noDetail <-
  wb_sheets %>%
  purrr::map(function(sheet){ # iterate through each sheet name
    readxl::read_xlsx(path = wb_source, sheet = sheet)
  })

names(noDetail) <- wb_sheets

# unfiltered_add <- noDetail$Sheet1 %>% select(osm_id, desc, geometry)
# unfiltered_add

# Pull out only observations where a desc has been assigned
test <- lapply(noDetail, function(x) {x %>% filter(!is.na(desc)) %>% select(osm_id, desc, geometry)})
test2 <- test[sapply(test, nrow) > 0]
test3 <- test2 %>% do.call("rbind", .)
# test3 <- test3 %>% rbind(unfiltered_add)
test4 <- test3 %>% mutate(geometry = st_as_sfc(geometry))
test5 <- test4 %>% makeValid() %>% select(desc, geometry) 
# Depending on what you have dealt with (i.e. none require simplify functions) 
# these can be attached to the existing OSMtidy output
test6 <- test5 %>% rbind(`Manchester_postProcessing_20210608-200231`) %>% makeValid()

# Export
locationName <- "Manchester"
exportOSMtidy(test6, locationName, ".RDS")
exportOSMtidy(test6, locationName, ".csv")
exportOSMtidy(test6, locationName, ".shp")


# -------------------------------------------------------------------------

# RESIDENTIAL GARDENS

# All cities require update to dataExtract to include access = private
# Not enough memory on my set-up to use dataExtract for Glasgow
# so improbable for other cities without high performance computer/ing
# Already changed in wrangleVars to include access information so this can
# be done in the future

locationName <- "Edinburgh_City"; locationName

shp <- dataShapefile(filename = paste0(locationName, ".shp"))

dlCut <- readRDS("Glasgow_3_dataCut_20210112-154150.RDS")

dlWrangle <- dataWrangle(dataCut = dlCut)

dlFilter <- dataFilter(dataWrangle = dlWrangle, 
                       filters = "filters_20210611.xlsx")

dataExport(data = dlFilter, name = locationName)



# -------------------------------------------------------------------------
# MILITARY; LAND & MILITARY; TRAINING GROUNDS 
# Validation stage correction for Glasgow

# Read in OSMtidy output
Glasgow_input = readRDS(
  "Glasgow/Glasgow_postProcessing_20210615-090434.RDS")

# Replace desc terms for Military; Land & Military; training grounds with Military; Administration
Glasgow = 
  Glasgow_input %>%
  mutate(desc =
           desc %>% 
           replace(., grepl('Military; Land', .), "Military; Administration") %>%
           replace(., grepl('Military; Training grounds', .), "Military; Administration"))
  

# Check it has worked
Glasgow %>% filter(desc == "Military; Land" | desc == "Military; Training grounds")  
Glasgow %>% filter(desc == "Military; Administration")  


# Export as simple feature class
Glasgow %>% ## Specify output object
  saveRDS(filenameTimestamp(
  prefix = "Glasgow/Glasgow_postProcessing", ## Specify output filepath and filename
  extension = ".RDS")) ## Specify output filename

# Can exportOSMtidy output (RDS object is not of simple feature class)
locationName = "Glasgow"
pathName = "Glasgow/"
exportOSMtidy(Glasgow, locationName, pathName, ext = ".RDS") # Specify output object, locationName, filepath, and file extension type
## Still having issues with exportOSMtidy for shapefiles - probably something wrong with input to do with buildings; unspecified, sf update, invalid geometries?


# -------------------------------------------------------------------------

# BUILDINGS; UNSPECIFIED
# DEALING WITH INVALID GEOMETRIES

# Sometimes addition of Buildings; Unspecified has resulted in some invalid geometries
# Steps in this section remove (usually 5-20 rows) containing invalid geometries
# Need to explore why this has occurred but for now, not too worried about Buildings; Unspecified

# Read in OSMtidy output
geoData = readRDS(
  "Manchester/Manchester_postProcessing_20210617-104812.RDS") 

# Prep geoData inputs - why is this still invalid? Something to do with update to sf package 2021-06-29?
class(geoData)
invalid_check1 = st_is_valid(geoData) %>% as_tibble() %>% filter(value != TRUE)

geoData_check = sf::st_make_valid(geoData)
class(geoData_check)

invalid_check2 = st_is_valid(geoData_check) %>% as_tibble() %>% filter(value != TRUE)
invalid_check3 = st_is_valid(geoData_check) %>% as_tibble() %>% setDT(keep.rownames = TRUE)

invalid_check4 = invalid_check3 %>% filter(value != TRUE)
rn_invalid <- invalid_check4 %>% pull(rn) %>% as.integer()
geoData_invalid = geoData %>% slice(rn_invalid)

invalid_check5 = invalid_check3 %>% filter(value == TRUE)
rn_valid = invalid_check5 %>% pull(rn) %>% as.integer()
geoData_clean = geoData %>% slice(rn_valid)


# -------------------------------------------------------------------------
# SAVE OUTPUTS

# Specify location name & pathName
locationName <- "Manchester"
pathName <- "Manchester/"

# Can export OSMtidy outputs in up to three formats
exportOSMtidy(geoData_clean, locationName, pathName, ext = ".RDS") # Specify output object, locationName, filepath, and file extension type
exportOSMtidy(geoData_clean, locationName, pathName, ext = ".csv") # Specify output object, locationName, filepath, and file extension type
exportOSMtidy(geoData_clean, locationName, pathName, ext = ".shp") # Specify output object, locationName, filepath, and file extension type