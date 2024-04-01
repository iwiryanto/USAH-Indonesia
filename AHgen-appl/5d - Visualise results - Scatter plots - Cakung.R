# =========================================================================
# 5d - Visualise results - Scatter plots - Cakung.R

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
pacman::p_load(tidyverse, showtext, tidytext, ggh4x, AHgen)


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

colsCakung_df = 
  AHgen::colsFloodRiver_df %>%
  filter(scenario != "1 in 200-year flood") %>%
  mutate(scenario = recode(scenario,
                           '1 in 100-year flood' = 'Rawa Terate offline'))


results = 
  allScenarios_compared$results %>%
  mutate(scenario = recode(scenario,
                           'RawaTerate-offline' = 'Rawa Terate offline')) %>%
  mutate(scenario = factor(scenario, 
                           levels = c("baseline", 
                                      "Rawa Terate offline"))) %>%
  full_join(colsCakung_df)


# SCATTER PLOTS -----------------------------------------------------------

# 2.1. SCATTER PLOT - EC VALUES ---------------------------------

# Create plot
scatter_ECvals_purposes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "values", levels = "Purposes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvals_Purposes"), extension = ".png"),
  plot = scatter_ECvals_purposes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECvals_outcomes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "values", levels = "Outcomes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvals_Outcomes"), extension = ".png"),
  plot = scatter_ECvals_outcomes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECvals_tasks = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "values", levels = "Tasks")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvals_Tasks"), extension = ".png"),
  plot = scatter_ECvals_tasks, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECvals_processes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "values", levels = "Processes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvals_Processes"), extension = ".png"),
  plot = scatter_ECvals_processes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECvals_resources = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "values", levels = "Resources")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvals_Resources"), extension = ".png"),
  plot = scatter_ECvals_resources, width = 4, height = 3, dpi = 600)


# 2.2. SCATTER PLOT - EC VALUES CHANGE --------------------------

# Create plot
scatter_ECvalsChange_purposes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "valuesChange", levels = "Purposes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvalsChange_Purposes"), extension = ".png"),
  plot = scatter_ECvalsChange_purposes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECvalsChange_outcomes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "valuesChange", levels = "Outcomes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvalsChange_Outcomes"), extension = ".png"),
  plot = scatter_ECvalsChange_outcomes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECvalsChange_tasks = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "valuesChange", levels = "Tasks")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvalsChange_Tasks"), extension = ".png"),
  plot = scatter_ECvalsChange_tasks, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECvalsChange_processes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "valuesChange", levels = "Processes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvalsChange_Processes"), extension = ".png"),
  plot = scatter_ECvalsChange_processes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECvalsChange_resources = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "valuesChange", levels = "Resources")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECvalsChange_Resources"), extension = ".png"),
  plot = scatter_ECvalsChange_resources, width = 4, height = 3, dpi = 600)


# 2.3. SCATTER PLOT - EC PERCENT CHANGE -------------------------

# Create plot
scatter_ECpctChange_purposes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "percentChange", levels = "Purposes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECpctChange_Purposes"), extension = ".png"),
  plot = scatter_ECpctChange_purposes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECpctChange_outcomes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "percentChange", levels = "Outcomes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECpctChange_Outcomes"), extension = ".png"),
  plot = scatter_ECpctChange_outcomes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECpctChange_tasks = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "percentChange", levels = "Tasks")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECpctChange_Tasks"), extension = ".png"),
  plot = scatter_ECpctChange_tasks, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECpctChange_processes = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "percentChange", levels = "Processes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECpctChange_Processes"), extension = ".png"),
  plot = scatter_ECpctChange_processes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_ECpctChange_resources = 
  vis_plotScatter(
    results = results, metricName = "EC", type = "percentChange", levels = "Resources")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_ECpctChange_Resources"), extension = ".png"),
  plot = scatter_ECpctChange_resources, width = 4, height = 3, dpi = 600)


# 2.4. SCATTER PLOT - SBC VALUES --------------------------------

# NOTE - PLOT NOT WORKING FOR PURPOSES, NEED TO INVESTIGATE WHY

# Create plot
scatter_SBCvals_purposes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", type = "values", levels = "Purposes")

check = results %>% filter(metric == "SBC_norm") %>% filter(level == 1)
# SBC_norm for all of these is 0
# Interesting to note in results!

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvals_Purposes"), extension = ".png"),
  plot = scatter_SBCvals_purposes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCvals_outcomes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", type = "values", levels = "Outcomes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvals_Outcomes"), extension = ".png"),
  plot = scatter_SBCvals_outcomes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCvals_tasks = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", type = "values", levels = "Tasks")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvals_Tasks"), extension = ".png"),
  plot = scatter_SBCvals_tasks, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCvals_processes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", type = "values", levels = "Processes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvals_Processes"), extension = ".png"),
  plot = scatter_SBCvals_processes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCvals_resources = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", type = "values", levels = "Resources")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvals_Resources"), extension = ".png"),
  plot = scatter_SBCvals_resources, width = 4, height = 3, dpi = 600)


# 2.5. SCATTER PLOT - SBC VALUES CHANGE -------------------------

# Create plot
scatter_SBCvalsChange_purposes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "valuesChange", levels = "Purposes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvalsChange_Purposes"), extension = ".png"),
  plot = scatter_SBCvalsChange_purposes, width = 4, height = 3, dpi = 600)

# Again these values at Purposes level are all 0 (and 0 change)

# Create plot
scatter_SBCvalsChange_outcomes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "valuesChange", levels = "Outcomes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvalsChange_Outcomes"), extension = ".png"),
  plot = scatter_SBCvalsChange_outcomes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCvalsChange_tasks = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "valuesChange", levels = "Tasks")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvalsChange_Tasks"), extension = ".png"),
  plot = scatter_SBCvalsChange_tasks, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCvalsChange_processes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "valuesChange", levels = "Processes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvalsChange_Processes"), extension = ".png"),
  plot = scatter_SBCvalsChange_processes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCvalsChange_resources = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "valuesChange", levels = "Resources")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCvalsChange_Resources"), extension = ".png"),
  plot = scatter_SBCvalsChange_resources, width = 4, height = 3, dpi = 600)


# 2.6. SCATTER PLOT - FACETED - SBC PERCENT CHANGE ------------------------

# Create plot
scatter_SBCpctChange_purposes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "percentChange", levels = "Purposes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCpctChange_Purposes"), extension = ".png"),
  plot = scatter_SBCpctChange_purposes, width = 4, height = 3, dpi = 600)

# Again because all Purposes values are 0 there was no percent change and no plot

# Create plot
scatter_SBCpctChange_outcomes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "percentChange", levels = "Outcomes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCpctChange_Outcomes"), extension = ".png"),
  plot = scatter_SBCpctChange_outcomes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCpctChange_tasks = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "percentChange", levels = "Tasks")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCpctChange_Tasks"), extension = ".png"),
  plot = scatter_SBCpctChange_tasks, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCpctChange_processes = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "percentChange", levels = "Processes")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCpctChange_Processes"), extension = ".png"),
  plot = scatter_SBCpctChange_processes, width = 4, height = 3, dpi = 600)

# Create plot
scatter_SBCpctChange_resources = 
  vis_plotScatter(
    results = results, metricName = "SBC_norm", 
    type = "percentChange", levels = "Resources")

# Export
ggsave(
  filenameTimestamp(
    prefix = 
      paste0(directory, study, "_scatter_SBCpctChange_Resources"), extension = ".png"),
  plot = scatter_SBCpctChange_resources, width = 4, height = 3, dpi = 600)