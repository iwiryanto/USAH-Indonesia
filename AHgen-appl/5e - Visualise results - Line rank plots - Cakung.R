# =========================================================================
# 5e - Visualise results - Line rank plots - Cakung.R

# Created by: Dr Annie Visser-Quinn (annievisserquinn@gmail.com), Dr David Morrison (dh48@hw.ac.uk), Dr Gordon Aitken (ga41@hw.ac.uk) & Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Created: 2021-09-06

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2024-02-28
# =========================================================================


# PREP --------------------------------------------------------------------

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the environment
rm(list = ls()); cat("/014"); gc(verbose = TRUE)

# CTRL + SHIFT + F10 will detach any loaded packages and restart R

# Set the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

# If you are doing this for the first time
# install the package pacman which checks to see if a package is installed, and if not it attempts to install the package from CRAN and/or any other repository in the pacman repository list
# and install the package devtools which will allow you to download a package straight from GitHub i.e. if they are not yet on CRAN and/or the pacman repository list
# install.packages("devtools")
# install.packages("pacman")

# Ensure latest version of AHgen is installed
devtools::install_github("avisserquinn/AHgen@dev", dependencies = TRUE)

# Load required packages
pacman::p_load(tidyverse, showtext, ggrepel, AHgen)


# 0.2. STYLE --------------------------------------------------------------

# You can specify your font of choice in the plot functions from section 0.3
# by using the argument "family"
# I prefer a custom font which loosely matches Nature journals
# After downloading ttf files, load the custom font using the showtext package
font_add(family = "Harding", regular = "./aes/fonts/HardingText-Regular-Web.ttf")
showtext_auto()

# Colours can be loaded from AHgen example data e.g. colsFloodRiver_df or colsFloodRiver
# Or you can add custom colours by editing the plot functions in section 0.3


# DATA --------------------------------------------------------------------

# 1.1. READ IN DATA -------------------------------------------------------

# Set names for directory and study
directory = "./plots/"
study = "Cakung-RawaTerate-offline"

# Read in data
# E.g. comparison of scenarios
allScenarios_compared =
  read_rds("outputs/comparison_USAH_3.0_Cakung-baseline_Cakung-RawaTerate-offline_20240202-163835.RDS") ## Specify filename


# 1.2. FORMAT DATA --------------------------------------------------------

# Reformat scenario column to rename into a more visually friendly format
# and assign to ordered levels
results = 
  allScenarios_compared$results %>%
  mutate(scenario = recode(scenario,
                           'RawaTerate-offline' = 'Rawa Terate offline')) %>%
  mutate(scenario = factor(scenario, 
                           levels = c("baseline", "Rawa Terate offline"))) %>%
  full_join(cols100RC_df)


# LINE RANK PLOTS ---------------------------------------------------------

# 2.1. LINE RANK PLOT - FACETED - OUTCOMES - EC VALUES --------------------

rank_outcomes_ECvals = 
  vis_plotRank(
    results = results, 
    metricName = "EC", 
    levels = "Outcomes", 
    change.only = TRUE)

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_rank_outcomes_ECvals"), extension = ".png"),
  plot = rank_outcomes_ECvals, width = 5, height = 4, dpi = 600)


# 2.2. LINE RANK PLOT - FACETED - TASKS - EC VALUES -----------------------

# Create plot
rank_tasks_ECvals = 
  vis_plotRank(
    results = results, 
    metricName = "EC", 
    levels = "Tasks", 
    change.only = TRUE)

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_rank_tasks_ECvals"), extension = ".png"),
  plot = rank_tasks_ECvals, width = 5, height = 6, dpi = 600)


# 2.3. LINE RANK PLOT - FACETED - TASKS - SBC VALUES ----------------------

# Create plot
rank_tasks_SBCvals = 
  vis_plotRank(
    results = results, 
    metricName = "SBC_norm", 
    levels = "Tasks", 
    change.only = TRUE)

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_rank_tasks_SBCvals"), extension = ".png"),
  plot = rank_tasks_SBCvals, width = 5, height = 6, dpi = 600)


# 2.4. LINE RANK PLOT - FACETED - PROCESSES - SBC VALUES ------------------

# Create plot
rank_processes_SBCvals = 
  vis_plotRank(
    results = results, 
    metricName = "SBC_norm", 
    levels = "Processes", 
    change.only = TRUE)

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_rank_processes_SBCvals"), extension = ".png"),
  plot = rank_processes_SBCvals, width = 5, height = 6, dpi = 600)


# 2.5. LINE RANK PLOT - FACETED - RESOURCES - SBC VALUES ------------------

# Create plot
rank_resources_SBCvals = 
  vis_plotRank(
    results = results, 
    metricName = "SBC_norm", 
    levels = "Resources", 
    change.only = TRUE)

# Export
ggsave(
  filenameTimestamp(
    prefix = paste0(directory, study, "_rank_resources_SBCvals"), extension = ".png"),
  plot = rank_resources_SBCvals, width = 5, height = 6, dpi = 600)