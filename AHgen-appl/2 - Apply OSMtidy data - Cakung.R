# =========================================================================
# 2 - Apply OSMtidy data.R

# Created by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com), Dr Annie Visser-Quinn (annievisserquinn@gmail.com) & Kerri McClymont (km39@hw.ac.uk)
# Created: 2020-10-10

# Last revised by: Dr Melissa Bedinger (dr.m.bedinger@gmail.com)
# Last revised: 2024-02-02
# =========================================================================

# 0.1. PREPARE ENVIRONMENT ------------------------------------------------

# Clear the RStudio environment
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

# Load required packages
pacman::p_load(tidyverse, sf, installr, janitor, AHgen)


# 1. READ IN DATA ---------------------------------------------------------

# Set system name, version, location, and scenario
AH_name = "USAH"
AH_version = "3.0"
AH_location = "Cakung"
AH_scenario = "baseline"

# Set directory for outputs
directory = "outputs/"

# Read in USAH_template_baseline
USAH_template_baseline_IDN = read_rds(
  "data/template/3.0/current/USAH_3.0_template_baseline_IDN_20240202-154732.RDS") ## Specify filename for USAH_template_baseline (master copy with all vertex and edge info)


# Read in OSMtidy output for location (if RDS file)
geoData = read_rds(
  "../OSMtidy-appl/outputs/Jakarta/Cakung/Jakarta_Cakung_postProcessing_20240129-153824.RDS") ## Specify filename for OSMtidy output

# Read in OSMtidy output for location (if CVS file)
# geoData = read_csv(
#   "../OSMtidy-appl/outputs/city-folder/OSMtidy-output-filename.csv") ## Specify filename for OSMtidy output

# Read in OSMtidy-AHgen key for allocating OSMtidy descs to AHgen objects
key = read_csv(
  "data/template/3.0/current/OSM-AH-key_IDN_3.0_20240129.csv", show_col_types = FALSE) ## Specify filename for OSMtidy-AHgen key


# 2. DETECT, CHECK, & REVISE PHYSICAL OBJECTS TO BE EXCLUDED --------------

# Check which desc terms were detected in the OSM data
# If the input geoData is of simple feature class, this can take a while 
# due to reformatting geometry to character class which then allows us to filter
desc_check = check_desc(geoData = geoData, key = key)

# Inspect physical objects that were not detected in Open Street Map data 
# but are set by the key to always be included
checkAlwaysInclude = desc_check$notDetected_alwaysInclude

# Inspect physical objects that were not detected in Open Street Map data
# and will be excluded
checkExclude = desc_check$notDetected_exclude

# Google each physical object type + location name 
# to ensure this is not just missing in the Open Street Map data
checkExclude %>% select(resource) %>% unique() 

# Create a vector of physical object names that should be included
# but are currently set to be excluded for this location

manualInclude_vec_Cakung <- 
  c("ATMs", #Multiple ATMs in area
    "Accountancy firms", #BC DPR, KJA Budihardjo, Sekretariat SPOI BRJ,...
    "Advertising", #PT Beruang Advertising Solusindo, Vehicle Registration Services Bureau Fortuna, ...
    "Animal care", #Pawradise Pet Care, Radhiyan Pet and Care, Momi Pet Care 2, ...
    "Animal shelters", #House Cats Beloved Friend Indonesia
    "Apparel retail", #Ibu Rurin Boutique, Multiple in shopping centres
    "Apparel services", #Laundry Care, Bamboe Laundry,...
    "Attractions", #Karaoke Billiard dan kost, Infinite lounge & KTV,....
    "Bars and nightclubs", #Kanpai Lounge & Bar, Infinite lounge & KTV,...
    "Beauty services", #Le Meriann House of Beauty, Latezza Beauty Parlour JGC, ....
    "Bridges", #Jembatan Pasar Cakung
    "Buildings (other)", #Multiple unidentified buildings
    "Charity shops", #Masjid Jami Nurul Amal, (place of worship doubles as charity locations)
    "Cinemas", #CGV Cinema
    "Construction and engineering", #Wasa Mitra Engineering
    "Couriers", #GOJEK, DHL Express Cakung
    "Creative spaces", #Jaya Foto Studio, Studio by Jawatra
    "Dental and orthodontal practices", #Ahli Gigi Awan Dental, Klinik Gigi Puri
    "Dining and catering services", #Dapur Miya Katering, Friend's Catering, ....
    "Driving schools", #CV Dwi Jaya, Satria Jayanti Kayu Tinggi,...
    "EV charging stations", #SPKLU Charging Station
    "Exterior building trades", #Akbar jaya (Pangkalan pasir dan batu bata)-Stonemason
    "Furnishing services", #Intermulti Kreasi, HHK PRoduction, Abadi Interior Design
    "Gas terminals", #Multiple gas stations PERTAMINA
    "Headquarters (other)", #Multiple non-descriptive Head offices
    "Healthcare (physiotherapy)", # LC Home Care Physiotherapy
    "Healthcare (specialist)", #Independent practioners
    "Home and outdoor maintenance services", #Green Pest control, Pest Terminator Jakarta
    "Interior building trades", #PD. Sabar Jaya (Carpenter)
    "Internet cafes", #Multiple internet cafes
    "IT software and services", #IT Solution tech
    "Legal services", #Kantor Pengacara Jakarta HDP
    "Locksmiths and cobblers", #Tukang Kunci Pasar Kayu Tinggi
    "Other discretionary services", #Pijit Spa dan Rilex
    "Pharmacies", #PT Dankos Farma Tbk.
    "Post offices", #Kantor Pos Cakung Barat
    "Public toilets", #Multiple public toilets (available in gas stations)
    "Real estate management offices", #REMAX Premier JGC, 9Pro Savoy
    "Research and consulting services", #A01 Architecture & Interior Deisgn, PT. Putra Segarra
    "Street lights", #Exists on main roads
    "Union offices", #Regional leadership council of the LEM trade union federation
    "Utility trades", #multiple plumbing & electricians
    "Veterinary practices", #Radhiyan Pet and Care, MOMi Pet Care 2, ...
    "Waste collection service offices") #Managed by government throughout Indonesia KPKNL Lahat
    

# !!! Specify for each location, see NOTES below !!!

# Create a dataframe of rows to be removed from desc_check$notDetected_exclude
# and added to desc_check$notDetected_alwaysInclude
manualInclude =
  desc_check$notDetected_exclude %>%
  filter(resource %in% manualInclude_vec_Cakung) %>% ## Specify which manualInclude_vec to use
  mutate(includedBy = "manualCheck")

# Remove from desc_check$notDetected_exclude
desc_check$notDetected_exclude =
  desc_check$notDetected_exclude %>%
  filter(!resource %in% manualInclude_vec_Cakung) ## Specify which manualInclude_vec to use
desc_check$notDetected_exclude

# Add to desc_check$notDetected_alwaysInclude
desc_check$notDetected_alwaysInclude = 
  desc_check$notDetected_alwaysInclude %>%
  mutate(includedBy = "automatedKey") %>%
  rbind(manualInclude)
desc_check$notDetected_alwaysInclude

# Inspect to ensure this has worked
checkAlwaysInclude = desc_check$notDetected_alwaysInclude
checkAlwaysInclude %>% select(resource) %>% unique()

checkExclude = desc_check$notDetected_exclude
checkExclude %>% select(resource) %>% unique()


# 3. APPLY OSMTIDY DATA ---------------------------------------------------

# Apply OSMtidy data to USAH
USAH_location = 
  apply_location(
    desc_check = desc_check,
    vInfo_template = USAH_template_baseline_IDN$vIncluded, ## Specify vInfo from scenario you want to make adjustments to (usually template)
    edgelist_template = USAH_template_baseline_IDN$edgelist, ## Specify edgelist from scenario you want to make adjustments to (usually template)
    AH_name = AH_name, 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = AH_scenario)

# Inspect elements
USAH_location$desc_check
USAH_location$vIncluded
USAH_location$vExcluded
USAH_location$adjMat
USAH_location$edgelist
USAH_location$igraph
USAH_location$results
USAH_location$summary


# 4. SAVE OUTPUTS ---------------------------------------------------------

USAH_location %>% 
  export_AHgen(
    type = "USAH", 
    AH_version = AH_version, 
    AH_location = AH_location, 
    AH_scenario = AH_scenario)


# NOTES -------------------------------------------------------------------

# Record of manually included Physical Objects / Resources 
# for all cities analysed to date

# Bristol
manualInclude_vec_Bristol <- 
  c("Animal shelters", # Multiple
    "Botanical gardens", # University of Bristol Botanic Garden
    "Broadband provider offices", # Virgin Media
    "Diplomatic buildings", # Indian Visa and Consular Centre
    "Electricity provider offices", # Multiple e.g. Bristol Energy
    "Ferry terminals", # Bristol Cruise Terminal
    "Fishing clubs", # Bathampton Angline, Lido, etc.
    "Gas provider offices", # Multiple e.g. Bristol Energy
    "Internet cafes", # At the Well etc.
    "Manufacturing (IT hardware)", # Cubik Innovation performs in-house manufacturing
    "Playgrounds (indoor)", # Jungle Mania etc.
    "Sexual health clinics", # Unity Sexual Health @ Central clinic
    "Springs and wells", # St. Edith's Well
    "Vehicle impounds", # Western Distribution Centre: https://www.bristol.gov.uk/parking/if-your-vehicle-has-been-towed-away
    "Water provider offices") # e.g. Bristol Water

# Edinburgh
manualInclude_vec_Edinburgh <- 
  c("Aqueducts and viaducts", # Aqueducts in Slatford/Chesser
    "Camping grounds", # Mortonhall
    "Internet cafes", # Multiple e.g. Here Internet Cafe
    "Prisons and jails", # HMP Edinburgh in Stenhouse/Longstone
    "Tram or ferry stops", # Multiple tram stops
    "Vehicle impounds", # Edinburgh Car Pound in Leith
    "Waste collection service offices") # Multiple

# Glasgow
manualInclude_vec_Glasgow <- 
  c("Blood banks", # Permanent blood donation centre at Nelson Mandela Place
    "Day centres", # Multiple e.g. Muslim Day Care, Newlands Day Centre
    "Dining and catering services", # Multiple e.g. Millar Catering
    "Environmental consulting and quantity surveying firms", # Multiple e.g. Cornerstone Quantity Surveyors
    "Ferry terminals", # Ferry terminal near Glasgow Science Centre"
    "Fishing clubs",# Fishing for Leave FFL
    "Gas provider offices", # Combined with Scottish Power, also Scot Gas Group etc.
    "Harbours", # King George V Dock
    "Internet cafes", # Multiple e.g. Yeeha Internet Hub
    "Manufacturing (healthcare)", #Metix Limited is a medical equipment manufacturer
    "Piers and boardwalks", # Govan Pier
    "Removal services", # Multiple e.g. RG Removals Glasgow
    "Sexual health clinics", # Private Sexual Health Clinic Glasgow
    "Vehicle impounds", # Multiple e.g. at Polmadie
    "Waste collection service offices", # Multiple e.g. AnyJunk Glasgow
    "Water tanks and towers") # Cranhill Water Tower

# London Central
manualInclude_vec_LondonCentral <-
  c("Assisted living (social)", # Multiple rehab accommodation facilities e.g. PCP Rehab London
    "Botanical gardens", # Chelsea Physic Garden
    "Gas provider offices", # Multiple e.g. British Gas
    "Manufacturing (healthcare)", # Multiple e.g. Enteric Healthcare Technology Co-operative
    "Manufacturing (IT hardware)", # Multiple e.g. BOSH Group
    "Sexual health clinics", # Multiple e.g. Burrell St Sexual Health Clinic
    "Tram or ferry stops", # Multiple ferry stops along Thames
    "Waste collection service offices", # Multiple e.g. Cory Environmental Ltd.
    "Water provider offices", # Multiple e.g. Thames Water Heathwall
    "Water pumping stations") # Multiple e.g. Heathwall Pumping Station

# Manchester
manualInclude_vec_Manchester <- 
  c("Animal shelters", # Multiple
    "Citizens Advice Bureaus", #  in city centre at Albert House
    "Dining and catering services", # Multiple e.g. Manchester Flying Chef
    "Driving schools", # Multiple e.g. L Team Driving School
    "Electricity provider offices", # Multiple e.g. ESB Energy
    "Gas provider offices", # Multiple e.g. Gazprom Energy
    "Internet cafes", # Multiple e.g. Princess Internet Cafe
    "Manufacturing (consumer discretionary)", # E.g. Samson Hosiery
    "Manufacturing (energy)", # E.g. Mantec Engineering Ltd.
    "Manufacturing (healthcare)", # E.g. Mantec Engineering Ltd.
    "Manufacturing (materials)", # Multiple e.g. Crane Materials
    "Observatories", # Godlee Observatory
    "Playgrounds (indoor)", # E.g. Wacky Warehouse
    "Political offices", # E.g. North West Liberal Democrats
    "Publishing offices", # Multiple e.g. Phoenix Media and Publishing
    "Registrar offices", # Registry office in city centre
    "Removal services", # Multiple e.g. Removal Services Manchester
    "Security services", # Multiple e.g. Angleside Security Guarding
    "Sexual health clinics", # E.g. The Northern in Cheetham
    "Stables", # E.g. Wythenshawe Park Riding Stables Ltd.
    "Union offices", # Multiple e.g. UNISON in city centre
    "Vehicle impounds", # Manchester Vehicle Pound
    "Waste collection service offices", # Multiple e.g. Skip King Skip Hire
    "Zoos") # Multiple e.g. Wythenshawe Park

manualExclude_vec_Penjaringan <-
  c("Airplanes and helicopters", #No helicopter/planes in penjaringan
    "Food banks", #No NGO for food in area
    "Gas pipelines", #No gas pipelines in Indonesia
    "Homeless shelters", #located in Tanjung Priok, not penjaringan
    "Potable water pipes") #No potable water pipe system in Indonesia, tap water not drinkable

manualInclude_vec_Penjaringan <- 
  c("Beaches", #Pantai Indah Kapuk
    "Blood banks", #Service provided within existing hospitals
    "Accountancy firms", #OFFiCE., CV Cahaya Berkah Solusindo, Justinus A Sidharta CPA Firm
    "Advertising", # ADX Asia, Oval Advertising, Ozzigeno Studio Digital Agency Jakarta
    "Ambulance stations", #Service provided by hospitals
    "Children's clubs", #Within malls and rental offices
    "Couriers", #GOJEK and others
    "Creative spaces", #Studios and language spaces offices rented for extracurriculars
    "Dining and catering services", #Sari rasa Ibu Ngatinah, Evie Seruni Catering, Gogo catering
    "Driving schools", #SDC Satria Driving Center, Kursus Stir Mobil Harinata PIK, Kursus Mengemudi Mobil Marks Pluit
    "EV charging stations", #SPKLU Charging Station
    "Electricity provider offices", #PLN Unit Bandengan, PT PLN Nusantara Power UMRO, PT PLN (PERSERO)
    "Financial advisors", #PT. Mulia Financial Centre, Aegis Finansia
    "Gas terminals", #Multiple gas stations PERTAMINA
    "Harbours", #Marina Bay
    "Internet cafes", #Multiple internet cafes
    "Mental health services", #PERHATI Counseling & Care Center, Bagikan ceritamu, Yayasan Praktek Psikolog PIK
    "Music and theatre venues", #XXI Indonesia
    "Opticians and ophthalmologists", #Karisma Optik, Glasses stores & eye check available in stores in malls
    "Other discretionary services", #Deep spa PIK, KEI SPA
    "Research and consulting services", #dAvinLee Architect, ASAKIWARI,
    "Surveillance cameras", #Present on traffic lights and highways
    "Veterinary practices", #Pet's Choice PIK, DVM. Magda Rumawas & Associates, Jakarta Pet Care Center
    "Waste collection service offices", #Managed by government throughout Indonesia KPKNL Lahat
    "Water provider offices", #PT. Air Alkali Indonesia
    "Wholesale retail", #Exists in multiple shopping malls
    "Zoos") #Muara Angke Wildlife Refuge

