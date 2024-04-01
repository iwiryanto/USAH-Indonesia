# =========================================================================
# 2 - Create shapefile - Rawa Terate.R
# Compatible with OSMtidy v0.0.6

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2020-07-03
#
# Last revised by: Ian Wiryanto (ian.wiryanto@gmail.com)
# Last revised: 2024-02-02
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
shp_RawaTerate = shp_ind %>% filter(ADM4_EN == "Rawa Terate")

# Then I discovered this dataset provides a good overview of the administrative boundaries
# but the geometry provided is only "point" rather than "polygon" (boundary)
# From here I'll load a new shapefile

shp_ind_L4 = shp_ind = st_read("shapefiles/Indonesia/idn_admbnda_adm4_bps_20200401.shp")
# From double clicking on shp_ind_L3 in the top right window, we can inspect
# this in the form of a spreadsheet and see that geometry data includes polygons


# 0.3. PREPARE SHAPEFILE --------------------------------------------------

# To test this script I started with a small test, a subdistrict within Jakarta
# with a smaller area to extract data for (Senen), found from https://en.wikipedia.org/wiki/List_of_districts_of_Jakarta
# We can just overwrite the previous version of shp_senen for this
shp_RawaTerate = shp_ind_L4 %>% filter(ADM4_EN == "Rawa Terate")

# Sometimes the drawings of boundaries have lots of detail, and this requires
# more computing power to extract data from Open Street Map, to not very much 
# benefit - so we can smooth out or 'simplify' the boundary lines with st_simplify
# We also need to make sure the shapefile is in the correct map projection using st_transform
shp_RawaTerate = 
  shp_RawaTerate %>%
  st_simplify(dTolerance = 25) %>%
  st_transform(4326)

# Inspect the boundary
plot(shp_RawaTerate)

# Write out the shapefile to the "shapefiles" folder for future use 
# so you don't have to do this step again if you don't want to
shp_RawaTerate %>% st_write("shapefiles/Jakarta_RawaTerate.shp", quiet = TRUE)