# =========================================================================
# 1 - Application.R
# Compatible with OSMtidy v0.0.6

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2020-07-03
#
# Last revised by: Ian Wiryanto (ian.wiryanto@gmail.com)
# Last revised: 2024-01-24
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory to the file path where this script is saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Ensure latest version of OSMtidy is installed
devtools::install_github("avisserquinn/OSMtidy")

# Load required packages
pacman::p_load(tidyverse, OSMtidy, sf, xlsx)


# 0.2. EXPLORE SHAPEFILE DATA ---------------------------------------------

shp_ind = st_read("shapefiles/Indonesia/idn_admbndp_admALL_bps_itos_20200401.shp")

# Inspect the different columns of data to see what's in them
# First drop the geometry column so we can inspect this like text spreadsheet,
# instead of like a shapefile
shp_ind_text = shp_ind %>% st_drop_geometry()

# According to documentation that came with zip folder of shapefiles,
# Level 0 is country-level, so all entries should be "Indonesia"
shp_ind_text %>% select(ADM0_EN) %>% unique() %>% arrange(ADM0_EN)

# Level 1 is province-level
shp_ind_text %>% select(ADM1_EN) %>% unique() %>% arrange(ADM1_EN)

# Level 2 is city, district, regency
shp_ind_text %>% select(ADM2_EN) %>% unique() %>% arrange(ADM2_EN)

# Level 3 is subdistrict
shp_ind_text %>% select(ADM3_EN) %>% unique() %>% arrange(ADM3_EN)
# Too many of these to print in the console so we'll save it as an R object, 
# then double click on it in the top right window to inspect it like a spreadsheet
level3 = shp_ind_text %>% select(ADM3_EN) %>% unique() %>% arrange(ADM3_EN)

# Level 4 is village
shp_ind_text %>% select(ADM4_EN) %>% unique() %>% arrange(ADM4_EN)
# Too many of these to print in the console so we'll save it as an R object, 
# then double click on it in the top right window to inspect it like a spreadsheet
level4 = shp_ind_text %>% select(ADM4_EN) %>% unique() %>% arrange(ADM4_EN)

# To test this script I started with a small test, a subdistrict within Jakarta
# with a smaller area to extract data for (Senen), found from https://en.wikipedia.org/wiki/List_of_districts_of_Jakarta
shp_cakung = shp_ind %>% filter(ADM3_EN == "Cakung")

# Then I discovered this dataset provides a good overview of the administrative boundaries
# but the geometry provided is only "point" rather than "polygon" (boundary)
# From here I'll load a new shapefile

shp_ind_L3 = shp_ind = st_read("shapefiles/Indonesia/idn_admbnda_adm3_bps_20200401.shp")
# From double clicking on shp_ind_L3 in the top right window, we can inspect
# this in the form of a spreadsheet and see that geometry data includes polygons


# 0.3. PREPARE SHAPEFILE --------------------------------------------------

# To test this script I started with a small test, a subdistrict within Jakarta
# with a smaller area to extract data for (Senen), found from https://en.wikipedia.org/wiki/List_of_districts_of_Jakarta
# We can just overwrite the previous version of shp_senen for this
shp_cakung = shp_ind_L3 %>% filter(ADM3_EN == "Cakung")

# Sometimes the drawings of boundaries have lots of detail, and this requires
# more computing power to extract data from Open Street Map, to not very much 
# benefit - so we can smooth out or 'simplify' the boundary lines with st_simplify
# We also need to make sure the shapefile is in the correct map projection using st_transform
shp_cakung = 
  shp_cakung %>%
  st_simplify(dTolerance = 25) %>%
  st_transform(4326)

# Inspect the boundary
plot(shp_cakung)

# Write out the shapefile to the "shapefiles" folder for future use 
# so you don't have to do this step again if you don't want to
shp_cakung %>% st_write("shapefiles/Indonesia/Cakung/Jakarta_Cakung.shp", quiet = TRUE)

# Remove any objects you don't need anymore
rm(level3, level4, shp_ind, shp_ind_L3, shp_ind_text, shp_cakung)


# 1. DATA INPUT -----------------------------------------------------------

# Specify folder where you want outputs to be sent to
directory = "outputs/Jakarta/Cakung"

# Specify location name
locationName = "Jakarta_Cakung"; locationName

# Read in shapefile based on location name - this is calling from the "shapefiles" folder
shp = dataShapefile(filename = paste0("shapefiles/Indonesia/Cakung/", locationName, ".shp"))

# Inspect a summary of the shapefile information
shp %>% dataSummary

# Export step 1 (prepared shapefile) from R to working directory
dataExport(data = shp, name = locationName, directory = directory)


# 0.4. EXPLORE HOW dataExtract WORKS --------------------------------------

# Before you extract data you can have a look at what Open Street Map features 
# we typically extract data for
# This information is stored in the package OSMtidy
OSMtidy::features

# See how many features we'll extract data for
nVec <- length(features) %>% as.numeric()
# See which features we'll extract data for
features

# For an example of how data is extracted, see below
# Let's find all the point data for bridges within the shapefile boundary
features[5]


# Extract the data using the osmdata_sf function
tmp <-
  shp %>%
  st_bbox() %>%
  opq(timeout = 300, memsize = 1073741824) %>%
  add_osm_feature(features[5]) %>%
  osmdata_sf(quiet = TRUE)

# Convert to nicer format
points <- list()
points[[features[5]]] <- tmp$osm_points %>% as.data.table %>% mutate(feature = features[5])
points <- points %>% .rmEmptyList %>% .rmNullList()

# Inspect
points
# 417 data entries for bridge points suggests that the Open Street Map data for
# this area is pretty active, a good sign!

# Now we can extract all the data we need for the area (not just bridges)

# The dataExtract function uses the prepared shapefile to estimate a rectangle 
# around the shapefile, which is the area Open Street Map data will be downloaded for
dlExtract <- dataExtract(dataShapefile = shp)

# Inspect a summary of extracted data
dlExtract %>% dataSummary

# Export step 2 (extracted data from estimated bounding box) from R 
# to working directory
dataExport(data = dlExtract, name = locationName, directory = directory)


# 3. DATA CUT -------------------------------------------------------------

# Apply shapefile to the data extracted for the estimated bounding box / rectangle
# to cookie-cutter only the data that is within the specific shapefile
dlCut <- dataCut(dataExtracted = dlExtract, dataShapefile = shp)

# Inspect summary of cookie-cuttered data
dlCut %>% dataSummary

# Export step 3 (cookie-cuttered data) from R to working directory
dataExport(data = dlCut, name = locationName, directory = directory)


# 4. DATA WRANGLE ---------------------------------------------------------

# This step sorts the data into four categories:
# dataWrangled, where the OSM data included some information or detail describing the data
# noDetail, where the OSM data didn't include any additional descriptive information 
# which might tell us what the data is, just that it is a point, line or polygon related to a high-level feature

# Wrangle cookie-cuttered data into a more manageable format
dlWrangle <- dataWrangle(dataCut = dlCut)

# Inspect summary of wrangled data
dlWrangle %>% dataSummary

# Export step 4 (wrangled data) from R to working directory
dataExport(data = dlWrangle, name = locationName, directory = directory)


# 5. DATA FILTER ----------------------------------------------------------

# Inspect filters you want to apply
# These are preset filters, validated in a UK context
# It is likely that after applying these filters and getting the outputs from 
# this step (step 5) that 
filterOverview("data/filters_20230517.xlsx")

# Apply filters to the wrangled data
# The dataFilter function sorts the wrangled data from step 4 into 
# three categories: 
# "filtered" - data where we are confident that the filters can be applied accurately
# "validate" - data where we are not 100% confident that the filters will work accurately every time,
# meaning that this data must be exported as a spreadsheet and reviewed manually by you to see
# if some need to be changed to a more accurate desc term
# "unfiltered" - data (usually a small amount) which had some descriptive information, but 
# there was no filter in our current filters file which would assign it to a desc term
# This next line of code will take a few minutes!
dlFilter =
  dataFilter(dataWrangle = dlWrangle, filters = "data/filters_20230517.xlsx")

# Inspect summary of filtered data
dlFilter %>% dataSummary

# Export step 5 (data categorised as filtered, validate, and unfiltered)
dataExport(data = dlFilter, name = locationName, directory = directory)


# 6. MANUAL STEPS ---------------------------------------------------------

# At this stage, you will need to look at the "validate" spreadsheet
# (and if you wish, the "noDetail" spreadsheet). From many entries you
# will be able to tell from descriptive columns about what the row of data is and
# whether the desc term it has already been assigned in the "desc" column is
# actually accurate or not.

# For example, in this example in the "validate" spreadsheet on Tab 2 
# ("2_ah8_Buildings; Nurseries") the data relates to a filter which tried to find all the 
# kindergartens or nurseries which look after small children of pre-school age. 

# The filter searches the data for observations which include the partial terms:
# "kinderg|nursery|preschool|pre-school|pre_school|nurser|creche|playschool|play_school|play school|playgroup|pre school|magic roundabout nurs".

# This is a filter which we need to validate because (at least in the UK) it
# sometimes picks up buildings which are actually for plant nurseries or garden centres.

# If you inspect the spreadsheet, there is a column named "amenity", and you can 
# see that most of the data in this tab has been assigned the desc term for 
# "Buildings; Nurseries" because the "amenity" is "kindergarten", which is correct.
# None of these data observations need to be changed, so they can be left as they are.

# Other tabs may take more time and effort. Tab 6 ("6_P1T_Keyword filter; Coll") 
# relates to a filter which tried to find all the data about colleges.
# These each need to be manually assigned a desc term. Currently, the desc term is 
# "Keyword filter; Colleges" and any that are not changed in this spreadsheet will
# be removed from the data at a later stage.

# To figure out which desc term should be assigned to each data observation, you
# will need to use the information in the other columns, and possibly copy and paste
# the coordinates into Google Maps to cross-check what it is.
# Then you can open the spreadsheet in the "data" folder called 
# "OSM-AH-key_3.0_withValidationGuide_20230717.xlsx". The second tab of this
# spreadsheet includes suggestions of what desc terms could be used, for example
# "School; Secondary" or "Training; Beauty school".

# Tips: Make sure you save your changes to the spreadsheet early & often!
# When I change a desc term, I highlight that cell yellow, so I can be sure of
# which ones I've changed, or how far I've reached in validating the entire spreadsheet.


# 7. DATA TIDY ------------------------------------------------------------

# Once you are finished with validating in step 6, you can read all of these 
# files back in to R.

# First clean up the R environment with all the old data in it, to prevent
# any confusion
rm(list = ls(all.names = TRUE))

#reset directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Specify location name
locationName = "Jakarta_Cakung"; locationName


# Note: 
# Read in RDS files for noDetail & filtered data before application of dataTidy function.
# Read in XLSX files for unfiltered & validated data within application of dataTidy function.

# Read in RDS files for noDetail & filtered data
# Make sure you change the filenames below to include the correct location name and date
dlWrangle = readRDS("outputs/Jakarta/Cakung/Jakarta_Cakung_4_dataWrangle_20240129-151821.RDS")
filtered = readRDS("outputs/Jakarta/Cakung/Jakarta_Cakung_5_dataFilter-filtered_20240129-152453.RDS")


# Tidy data
# Make sure you change the filename below to include the correct location name and date
dlTidy =
  dataTidy(dataList =
             list(dlWrangle$noDetail,
                  "outputs/Jakarta/Cakung/Jakarta_Cakung_5_dataFilter-unfiltered_20240129-152452.xlsx",
                  filtered,
                  "outputs/Jakarta/Cakung/Jakarta_Cakung_5_dataFilter-validate_20240129-152503.xlsx"))

# Create summary of tidied data in R
dlSummary = dlTidy %>% dataSummary

# Inspect summary of tidied data in R
dlSummary

# Export tidied data from R to working directory
directory = "outputs/Jakarta/Cakung"

dataExport(data = dlTidy, name = locationName, directory = directory)
dataExport(data = dlSummary, name = locationName, directory = directory)


# Export summary of tidied data to RDS file
# Make sure you change the filename below to include the correct location name and date
dlSummary %>% write_rds("Jakarta_Cakung_dataSummary_20231212-005551.RDS")


# 8. POST-PROCESS ---------------------------------------------------------

# So far the data has been extracted, cut, wrangled, filtered, and validated.
# This step is for post-processing of some specific types of buildings which 
# we think should be combined into a single object.

# Typically Open Street Map users tag individual parking lots, runways, restaurants, 
# etc. within an airport. The simplifyAirports function searches for things like this
# relating to airports that are within a very close distance from one another, and combines
# them all into one big multipolygon, so that rather than counting these as 40 airports,
# it counts them as one single airport.

# Read in the final filtered data, the output of step 7
# Make sure you change the filename below to include the correct location name and date
dg = readRDS("outputs/Jakarta/Cakung/Jakarta_Cakung_6_dataTidy-filtered_20240129-153654.RDS")

# Inspect the data
dg

# Reset the location name and directory where you want outputs to be exported to
locationName = "Jakarta_Cakung"
directory = "outputs/Jakarta/Cakung/"

# Make chosen data valid
dg <- dg %>% makeValid()
# Inspect step 6 filtered data
dg

# The output could be exported now using exportOSMtidy()
# Or after application of the five simplifying functions


### AIRPORTS ###

# Simplify airport and helipad data
dg <- dg %>% simplifyAirports

# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Check for 'Airports (to validate)'
dg %>% filter(desc == "Airports (to validate)")

# Check OpenStreetMap/GoogleMaps coordinates of any 'Airports (to validate)'

# If needed change this to appropriate desc e.g.
dg <- dg %>% mutate(desc = str_replace(desc, "Airports \\(to validate\\)", "Public transport; Helipad"))

# Check it's worked
dg %>% filter(str_detect(desc, "irport|elipad"))

# Inspect simplified data
dg


### GOLF COURSES ###

# Simplify golf course data
dg <- dg %>% simplifyGolf 

# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Inspect simplified data
dg


### RAIL STATIONS ###

# Simplify rail station data
dg <- dg %>% simplifyRail

# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"
# Ignore error: "No preset descNew has been found for this object type. Ensure you have specified a descNew argument."

# Inspect simplified data
dg


### WATER/WASTEWATER TREATMENT WORKS ###

# Simplify water and wastewater treatment works data
dg <- dg %>% simplifyTreatment

# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Inspect simplified data
dg


### EVENTS VENUES ###

# Simplify events venues e.g. stadiums data
dg <- dg %>% simplifyVenues

# Ignore error: "although coordinates are longitude/latitude, st_union assumes that they are planar"

# Inspect simplified data
dg


# SAVE OUTPUTS ------------------------------------------------------------

# Export the final output from OSMtidy in three formats
# Specify output object (dg = __)
# Specify filepath name (path = __)
# Specify file/location name (name = __)
# Specify if you want output /not/ in simple feature format (sf = FALSE)
# Specify file format (ext = __)
exportOSMtidy(dg, directory, locationName, ext = ".RDS") # Gives output as RDS in simple feature format
exportOSMtidy(dg, directory, locationName, sf = FALSE, ext = ".RDS") # Gives output as RDS in dataframe format (easier for later data wrangling)
exportOSMtidy(dg, directory, locationName, sf = FALSE, ext = ".csv") # Gives output as CSV in dataframe format
exportOSMtidy(dg, directory, locationName, ext = ".shp") # Gives output as shapefile

exportOSMtidy(dg, directory, type=".RDS")
exportOSMtidy(dg, locationName, type=".RDS")
exportOSMtidy(dg, locationName, type=".csv")
exportOSMtidy(dg, locationName, type=".shp")
