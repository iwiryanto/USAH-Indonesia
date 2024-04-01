# =========================================================================
# 2c - Key modification.R
# Compatible with OSMtidy v0.0.5

# Created by: Dr Melissa Bedinger (m.bedinger@hw.ac.uk)
# Created: 2021-06-28
#
# Last revised by: Dr Melissa Bedinger (m.bedinger@hw.ac.uk)
# Last revised: 2021-08-05
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# Load required packages
pacman::p_load(tidyverse)

# Helper function to attach a date and time to outputs
filenameTimestamp <- function(prefix, extension, sep = "_") {
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  
  paste0(prefix, sep, timestamp, extension)
  
}


# 1. READ IN DATA ---------------------------------------------------------

# Read in OSMtidy-AHgen key for allocating OSMtidy descs to AHgen objects
key = read_csv(
  "OSMAHkey_20210630.csv") ## Specify filename for OSMtidy-AHgen key


# 2. TIDY CATEGORIES AND MODIFY KEY ---------------------------------------

# Inspect OSMtidy-AHgen key
key

# If missing, add category to OSMtidy-AHgen key
key_new = 
 key %>%
 mutate(category_OSMtidy = gsub("(.*);.*", "\\1", desc)) %>% # Find the name1 (from OSMtidy filters) or broad category (from validation)
 mutate(category_mod = # Modify / clean up these categories a little
           category_OSMtidy %>% 
          replace(., grepl('bsorption tower', .), "Environmental manaagement") %>%
          replace(., grepl('irport|elipad', .), "Transport infrastructure") %>%
          replace(., grepl('irbase', .), "Military") %>%
          replace(., grepl('Bank', .), "Business and commerce") %>%
          replace(., grepl('Barrier', .), "Transport infrastructure") %>%
          replace(., grepl('Brownfield site', .), "Outdoors and natural environment") %>%
          replace(., grepl('College', .), "University") %>%
          replace(., grepl('Council', .), "Governance") %>%
          replace(., grepl('Exhibition centre', .), "Attractions") %>%
          replace(., grepl('Gas provider offices', .), "Utilities office") %>%
          replace(., grepl('Golf', .), "Sports and games") %>%
          replace(., grepl('fitness and leisure', .), "Sports and games") %>%
          replace(., grepl('Home and outdoor', .), "Trades and services") %>%
          replace(., grepl('anufacturing', .), "Manufacturing") %>%
          replace(., grepl('Mental health', .), "Healthcare") %>%
          replace(., grepl('Outdoor facility', .), "Sports and games") %>%
          replace(., grepl('Point of interest', .), "Historic") %>%
          replace(., grepl('Private clubs', .), "Community centres") %>%
          replace(., grepl('Regeneration programme', .), "Governance") %>%
          replace(., grepl('Road and rail maintenance', .), "Industry") %>%
          replace(., grepl('Sports facilities', .), "Sports and games") %>%
          replace(., grepl('Town square', .), "Amenity") %>%
          replace(., grepl('Waterway', .), "Water"))

# Check if further modifications are needed
check = key_new %>% arrange(category_mod) %>% select(category_mod) %>% unique()

# Write out to csv for easier checking
# write.csv(check, "key_category_check.csv")


# 3. SAVE OUTPUT ----------------------------------------------------------

# Write out new version of key with category modifications
key_new %>% write.csv(
 filenameTimestamp(prefix = "OSMAHkey", extension = ".csv")) ## Specify output filename
# Ensure you also update OSMAHkey_withValidationGuide at this stage