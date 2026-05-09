# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Conceptual R script to reate data snapshot files from 
# DHIS2-WIDP TB data collection system
# Takuya Yamanaka, January 2026
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load packages ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# install.packages("khisr")
library(khisr)
library(dplyr)
library(here)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(httr2)
library(lubridate)

gtb_snapshot_folder <- here(paste0("data/snapshot_", Sys.Date() ))

# Create output folder (only if it doesn't yet exist)
dir.create(gtb_snapshot_folder, showWarnings = FALSE, recursive = TRUE)

# Connection setup ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
url      <- "https://extranet.who.int/dhis2/"  
username <- "yamanakat_RO"    # to be replaced with "tbdata" read only account
password <- ">!x>uih%QN=?d:qU8.uL"

# DEV https://extranet.who.int/dhis2-dev
# PROD https://extranet.who.int/dhis2/
# PREPROD https://portal-uat.who.int/dhis2/
# TRAINING https://extranet.who.int/dhis2-demo/


khis_cred(
  username   = username,
  password   = password,
  server     = url,  
  
  api_version = NULL  
)

khis_base_url()

# Set a few constants ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dcyear   <- year(Sys.Date())
start_year <- 2019 # for testing 

# Extract metadata ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
meta_all <- get_metadata(
  endpoint = "dataElements",
  fields   = "*"      # request ALL fields in data elements metadata
)

# filter “TUB_” in the name
tb_meta <- meta_all |>
  filter(grepl("TUB_", name, ignore.case = TRUE)) |>
  mutate(code = sub("^TUB_", "", code)) |> # remove TUB_ from code
  mutate(code = sub("^TBD_TUB_", "", code)) # remove TBD_TUB_ from code

codebook <- tb_meta |>
  select(dataElement = id, code, name)

# Create country list ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# country list; country OU
countries <- get_metadata(
  endpoint = "organisationUnits",
  fields   = "id,name,shortName,code,parent[id,name]", 
  filter   = "level:eq:3" # level 3 is for countries. 1: global, 2: regions
) |>
  transmute(iso3 = code, country = shortName, g_whoregion = map_chr(parent, \(x) x$name %||% NA), ou_id = id) |>
  mutate(g_whoregion = ifelse(g_whoregion == "SEAR", "SEA", g_whoregion),
         g_whoregion = ifelse(g_whoregion == "NA", NA, g_whoregion))

# Dataset id; this will be used mass data extraction
datasets <- get_metadata(
  endpoint = "dataSets",
  fields   = "id,name"
)

# global OU; used as a parent OU to extract all available data in any OUs
ou_global <-  get_metadata(
  endpoint = "organisationUnits",
  fields   = "id,name,level",
  level %.eq% 1,
  pageSize = 50,
  page     = 1
)

# Category options for questions with disaggregations
coc <- get_metadata(
  endpoint = "categoryOptionCombos",
  fields   = "id,name,code"
) |>
  filter(grepl("TUB_", name, ignore.case = TRUE)) |> # remove TUB_ from COC name
  rename(coc_id = id, coc_name = name, coc_code = code)

coc <- coc |>
  mutate(coc_code = sub("^TUB", "", coc_code)) # remove TUB_ from coc_code

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ---- function and specifications: one year data extraction ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# function to extract data from WIDP
get_dvs_year <- function(year) {
  message("Downloading dataValueSets for year ", year, " ...")
  
  dvs <- request(paste0(base, "/api/dataValueSets")) |>
    req_auth_basic(username, password) |>
    req_url_query(
      dataSet  = dataset_id,
      orgUnit  = parent_ou_id,
      period   = year,
      children = "true"
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  
  # If no data, return empty 
  if (length(dvs$dataValues) == 0) {
    return(tibble(
      dataElement = character(),
      categoryOptionCombo = character(),
      period = character(),
      orgUnit = character(),
      value = character()
    ))
  }
  
  as_tibble(dvs$dataValues)
}

# function to organize and produce a dataframe by section
section_snapshot <- function(codes) {
  stopifnot(
    exists("datavalues_all"),
    exists("countries"),
    exists("codebook"),
    exists("coc")
  )
  
  datavalues_all |>
    left_join(countries, by = "ou_id") |>
    left_join(codebook,  by = "dataElement") |>
    left_join(coc,       by = "coc_id") |>
    mutate(variable = paste0(code, str_replace_na(coc_code, ""))) |>
    filter(code %in% codes) |>
    arrange(year, variable) |>
    select(country, iso3, year, g_whoregion, value, variable) |>
    pivot_wider(names_from = variable, values_from = value)
}

# ---- specifications to run the function ----
base <- khis_base_url()
parent_ou_id <- "H8RixfF8ugH" # set global OU as parent OU
years        <- as.character(start_year:dcyear)
q_seq <- paste0(rep(2020:dcyear, each = 4), "Q", rep(1:4, 7))
m_seq <- paste0(rep(2020:dcyear, each = 12), sprintf("%02d", 1:12))

# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Extract all data values ----
# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
dataset_id <- "XGn6mrO2HU8" # "XGn6mrO2HU8" is for "TUB - Tuberculosis annual data". 
# See Other datasets with View(datasets). Historical DE "UMwGyxafkXG", monthly notifications "aFh9rvR7u8Z", quarterly notifications "mOp64btdbvS"
datavalues_annual <- map_dfr(years, get_dvs_year) |>
  rename(year = period, ou_id = orgUnit, coc_id = categoryOptionCombo) |>
  filter(ou_id %in% countries$ou_id) # filter country level data only

dataset_id <- "UMwGyxafkXG"
datavalues_historical <- map_dfr(years, get_dvs_year) |>
  rename(year = period, ou_id = orgUnit, coc_id = categoryOptionCombo) |>
  filter(ou_id %in% countries$ou_id) # filter country level data only

dataset_id <- "aFh9rvR7u8Z" # 
datavalues_monthly <- map_dfr(m_seq, get_dvs_year) |>
  rename(year = period, ou_id = orgUnit, coc_id = categoryOptionCombo) |>
  filter(ou_id %in% countries$ou_id) # filter country level data only

dataset_id <- "mOp64btdbvS" # 
datavalues_quarterly <- map_dfr(q_seq, get_dvs_year) |>
  rename(year = period, ou_id = orgUnit, coc_id = categoryOptionCombo) |>
  filter(ou_id %in% countries$ou_id) # filter country level data only

datavalues_all <- rbind(datavalues_annual,datavalues_historical,
                        datavalues_monthly,datavalues_quarterly) |>
  distinct()

# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Define variables for all sections ----
# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   
# TB notifications ----
s_tb = c("pulm_labconf",            "pulm_clindx",  "ep", 
         "new_ep","new_labconf",
         "new_clindx",
         "ret_rel_labconf",
         "ret_rel_clindx",
         "ret_rel_ep") 

# Monthly/quarterly provisional notifications ----
s_provisional = c("newinc_prov")

# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Produce section data snapshot ----
# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
section_list <- ls(pattern = "^s_") # list all "s_*"

section_names <- stringr::str_c(stringr::str_remove(section_list, "^s_"))   # "acknowledge", "notifss" etc

section_ss_list <- setNames(lapply(mget(section_list), section_snapshot), section_names) # data snapshot of all "acknowledge", "notifs" etc in a list

list2env(section_ss_list, envir = .GlobalEnv)  # creates section data snapshot


# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# save the data.tables ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
dtlist <- mget(section_names)

clean_df <- function(df) {
  df |>
    dplyr::mutate(
      
      # 1. TRUE/FALSE stored as character → 1/0
      dplyr::across(
        where(is.character),
        ~ if (all(na.omit(.) %in% c("true", "false"))) {
          as.integer(. == "true")
        } else {
          .
        }
      ),
      
      # 2. logical → 1/0 (in case some columns are still logical)
      dplyr::across(where(is.logical), as.integer),
      
      # 3. numeric-looking character → numeric
      dplyr::across(
        where(is.character),
        ~ if (all(grepl("^-?[0-9]+(\\.[0-9]+)?$", na.omit(.)))) {
          as.numeric(.)
        } else {
          .
        }
      )
    )
  
}

dtlist <- lapply(dtlist, clean_df)

dtlist$tb <- dtlist$tb  |>
  select(!g_whoregion)|>
  filter(year>2018)

dtlist$provisional <- dtlist$provisional  |>
  select(iso3, country, everything()) |>
  arrange(iso3)


# - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# save the data.tables ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
invisible(lapply(names(dtlist), function(u) {
  assign(u, dtlist[[u]])
  save(list = u, file = paste0(gtb_snapshot_folder, "/", u, ".rda"))
}))



