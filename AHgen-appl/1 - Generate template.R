# =========================================================================
# 1 - Generate template.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com) & Dr Annie Visser-Quinn (annievisserquinn@gmail.com)
# Created: 2020-10-10

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2024-02-02
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the R Studio environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# CTRL + SHIFT + F10 will detach any loaded packages and restart R

# Set the working directory to the folder where this script is saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# If you are doing this for the first time
# install the package pacman which checks to see if a package is installed, and if not it attempts to install the package from CRAN and/or any other repository in the pacman repository list
# and install the package devtools which will allow you to download a package straight from GitHub i.e. if they are not yet on CRAN and/or the pacman repository list
# install.packages("devtools")
# install.packages("pacman")

# Ensure latest version of AHgen is installed
devtools::install_github("avisserquinn/AHgen@dev", dependencies = TRUE)

# If package has 'non-zero exit status', try re-installing devtools & rlang
# install.packages("devtools")
# install.packages("rlang")

# Load required packages
pacman::p_load(tidyverse, readxl, installr, janitor, vctrs, AHgen)


# 1. READ IN DATA ---------------------------------------------------------

# Set name, version, location, and scenario
AH_name = "USAH"
AH_version = "3.0"
AH_location = "template"
AH_scenario = "baseline_IDN"

# Set directory for outputs
directory = "data/template/3.0/current/"

# Read in template USAH adjacency matrix
adjMat_template = read_adjMat(
  "data/template/3.0/current/USAH_3.0_template_baseline_IDN_adjMat_20240129.xlsx") ## Specify input filename

# Create template USAH vInfo (basic)
vInfo_template = 
  adjMat_template %>% select(level, levelName_full, levelName, Node)

# Read in template USAH vInfo (full)
vInfo_template_full = read_xlsx(
  "data/template/3.0/current/USAH_3.0_template_baseline_IDN_vInfo_full_20240124.xlsx") ## Specify input filename

# Read in OSMtidy-AHgen key for allocating OSMtidy descs to AHgen objects
key = read_csv(
  "data/template/3.0/current/OSM-AH-key_IDN_3.0_20240129.csv", show_col_types = FALSE) ## Specify input filename


# 2. CHECK INPUTS ---------------------------------------------------------

# Confirm adjMat has completely symmetrical edges i.e. there are no input errors
adjMat_template %>% check_symmetry()
# If any edges are unsymmetrical check these
# checkSymmetry = adjMat_template %>% check_symmetry()

# Confirm adjMat has no vertices with redundant sets of edges i.e. no vertices
# are connected to the same set of other vertices between two levels
adjMat_template %>% check_redundancy()

# Check node names from different data inputs are compatible
check_names(adjMat_template, vInfo_template_full, key)
# If some node names do not match
# checkNames = check_names(adjMat_template, vInfo_template_full, key)
# checkNames %>% write.csv("checkNames.csv")
  

# 3. GENERATE USAH_TEMPLATE_BASELINE OUTPUT -------------------------------

# Generate standard AHgen output for USAH baseline template
USAH_template_baseline_IDN = 
  gen_AH(vInfo = vInfo_template_full, 
         vIncluded = vInfo_template,
         adjMat = adjMat_template,
         AH_name = AH_name, 
         AH_version = AH_version, 
         AH_location = AH_location, 
         AH_scenario = AH_scenario)

# Inspect elements
USAH_template_baseline_IDN$vInfo
USAH_template_baseline_IDN$vIncluded
USAH_template_baseline_IDN$vExcluded
USAH_template_baseline_IDN$adjMat
USAH_template_baseline_IDN$edgelist
USAH_template_baseline_IDN$igraph
USAH_template_baseline_IDN$results
USAH_template_baseline_IDN$summary


# 4. EXPORT ---------------------------------------------------------------

# Write output to RDS file
USAH_template_baseline_IDN %>% 
  export_AHgen(
    type = "USAH", # Specify type of AHgen output being exported
    directory = directory,
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = AH_scenario)

# If subnetwork assignment has been updated, write out to xlsx for future use
USAH_template_baseline$vInfo %>% 
  write.xlsx(
    filenameTimestamp(
      prefix = paste0(directory,
                      AH_name, "_", AH_version, "_", AH_location, "_", AH_scenario, 
                      "_vInfo-full"),
      extension = ".xlsx"))