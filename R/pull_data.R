library(tidyverse)
library(httr)
library(jsonlite)

# Source and supporting info on data
# All data is pulled from WUE data portal https://wuedata.water.ca.gov/
# UWMP required every five years
# 2020 data comes from this site https://data.cnra.ca.gov/dataset/2020-uwmp-data-export-tables
# 2015 data comes from this site https://wuedata.water.ca.gov/uwmp_export.asp


# Pull data UWMP data from cnra and save in tempfile
# 2020
#
# Make tibble - year, url, file_name, report_name
#
get_uwmp_data <- function() {
  temp <- tempfile()
  download.file("https://wuedata.water.ca.gov/public/uwmp_data_export/uwmp_table_2_1_r_conv_to_af.xls", temp)
  pwsid <- read.table(temp, header = TRUE, sep = "\t", fill = TRUE) |> select(ORG_ID, PUBLIC_WATER_SYSTEM_NUMBER) |> glimpse()
  unlink(temp)


  wue_datasets <- parameters |>
    filter(report_name == "UWMP") |>
    select(year, url, file_name)

  read_table <- function(year, url, file_name){
    print(file_name)
    temp <- tempfile()
    download.file(url, temp)
    data <- read.table(temp, header = TRUE, sep = "\t", fill = TRUE)
    unlink(temp)

    if (file_name %in% c("retail_demand_total", "wholesale_demand_total")) {
      col_af = ifelse(year == 2020 & file_name == "retail_demand_total", paste0("WATER_DEMAND_VOLUME_", year), paste0("WATER_DEMAND_VOLUME_", year, "_AF"))
      reformated_data <- data |>
        left_join(pwsid) |>
        rename(volume_af = col_af) |>
        transmute(report_name = "UWMP",
                  pwsid = PUBLIC_WATER_SYSTEM_NUMBER,
                  supplier_name = WATER_SUPPLIER_NAME,
                  year = year,
                  month = NA,
                  category = "demand total",
                  use_type = WATER_DEMAND_TYPE,
                  volume_af = volume_af)
    }
    if (file_name %in% c("retail_demand", "wholesale_demand")) {
      reformated_data <- data |>
        left_join(pwsid) |>
        transmute(report_name = "UWMP",
                  pwsid = PUBLIC_WATER_SYSTEM_NUMBER,
                  supplier_name = WATER_SUPPLIER_NAME,
                  year = year,
                  month = NA,
                  category = "demand",
                  use_type = tolower(USE_TYPE),
                  volume_af = VOLUME_AF)
    }
    if (file_name %in% c("retail_supply", "wholesale_supply")){
      reformated_data <- data |>
        left_join(pwsid) |>
        transmute(report_name = "UWMP",
                  pwsid = PUBLIC_WATER_SYSTEM_NUMBER,
                  supplier_name = WATER_SUPPLIER_NAME,
                  year = year,
                  month = NA,
                  category = "supply",
                  use_type = tolower(WATER_SUPPLY),
                  volume_af = ACTUAL_VOLUME_AF)
    }
    return(reformated_data)
  }

  uwmp_data <- purrr::pmap(wue_datasets, read_table) |> reduce(bind_rows)
  return(uwmp_data)
}


# WLA
get_wla_data <- function() {

  # download file
  temp <- tempfile()
  download.file("https://wuedata.water.ca.gov/public/awwa_data_export/water_audit_data_conv_to_af.xls",
                destfile = temp)
  wla_data_raw <- read_delim(temp)
  unlink(temp)

  # select columns for supply and demand
  wla_fields <- colnames(wla_data_raw) # all fields (129)

  # variables for supply (use cases)
  supply_fields <- wla_fields[stringr::str_detect(wla_fields, "^WS")] # keep variables with WS
  supply_fields <- supply_fields[!stringr::str_detect(supply_fields, "COMMENT")] # no comments
  supply_fields <- supply_fields[!stringr::str_detect(supply_fields, "UNITS")] # no units
  supply_fields <- supply_fields[stringr::str_detect(supply_fields, "_AF")] # AF (acre feet)
  supply_fields <- supply_fields[!stringr::str_detect(supply_fields, "_ERR_")] # no error

  # variables for demand (use cases)
  demand_fields <- wla_fields[stringr::str_detect(wla_fields, "^AC")] # keep variables with AC (consumption)
  demand_fields <- demand_fields[!stringr::str_detect(demand_fields, "COMMENT")] # no comments
  demand_fields <- demand_fields[stringr::str_detect(demand_fields, "_AF")] # AF (acre feet)
  demand_fields <- demand_fields[!stringr::str_detect(demand_fields, "_ERR_")] # no error

  # other important variables (use cases)
  other_fields <- wla_fields[!stringr::str_detect(wla_fields, "^AC") &
                               !stringr::str_detect(wla_fields, "^WS")] # no WS or AC (supply/demand)
  other_fields <- other_fields[stringr::str_detect(other_fields, "^WL")] # no WL
  other_fields <- other_fields[!stringr::str_detect(other_fields, "COMMENT")] # no comments
  other_fields <- other_fields[stringr::str_detect(other_fields, "AF")] # AF (acre feet)
  other_fields <- other_fields[!stringr::str_detect(other_fields, "ERR")] # no error


  # select supply use cases and supplier name, reporting year, and volume
  wla_supply <- wla_data_raw |>
    select(WATER_SUPPLIER_NAME, REPORTING_YEAR, VOLUME_REPORTING_UNITS, PWS_ID_OR_OTHER_ID, all_of(supply_fields)) |>
    pivot_longer(cols = all_of(supply_fields), names_to = "use_type", values_to = "volume") |>
    transmute("report_name" = "WLR",
              "pwsid" = PWS_ID_OR_OTHER_ID,
              "supplier_name" = WATER_SUPPLIER_NAME,
              "year" = REPORTING_YEAR,
              "month" = NA,
              "category" = ifelse(use_type == "WS_WATER_SUPPLIED_VOL_AF", "supply total", "supply"),
              "use_type" = tolower(use_type),
              "volume_af" = volume)

  # select demand use cases and supplier name, reporting year, and volume
  wla_demand <- wla_data_raw |>
    select(WATER_SUPPLIER_NAME, REPORTING_YEAR, VOLUME_REPORTING_UNITS, PWS_ID_OR_OTHER_ID, all_of(demand_fields)) |>
    pivot_longer(cols = all_of(demand_fields), names_to = "use_type", values_to = "volume") |>
    transmute("report_name" = "WLR",
              "pwsid" = PWS_ID_OR_OTHER_ID,
              "supplier_name" = WATER_SUPPLIER_NAME,
              "year" = REPORTING_YEAR,
              "month" = NA,
              "category" = ifelse(use_type == "AC_AUTH_CONSUMPTION_VOL_AF", "demand total", "demand"),
              "use_type" = tolower(use_type),
              "volume_af" = volume)

  # select other use cases and supplier name, reporting year, and volume
  wla_losses <- wla_data_raw |>
    select(WATER_SUPPLIER_NAME, REPORTING_YEAR, VOLUME_REPORTING_UNITS, PWS_ID_OR_OTHER_ID, all_of(other_fields)) |>
    pivot_longer(cols = all_of(other_fields), names_to = "use_type", values_to = "volume") |>
    transmute("report_name" = "WLR",
              "pwsid" = PWS_ID_OR_OTHER_ID,
              "supplier_name" = WATER_SUPPLIER_NAME,
              "year" = REPORTING_YEAR,
              "month" = NA,
              "category" = "losses",
              "use_type" = tolower(use_type),
              "volume_af" = volume)

  # bind all together
  wla_data <- bind_rows(wla_demand, wla_supply, wla_losses)

  # return
  return(wla_data)

}

# CR
get_cr_data <- function() {

  # Download the whole dataset using a SQL query
  conservation_report_url_sql <- paste0("https://data.ca.gov/api/3/action/",
                                        "datastore_search_sql?",
                                        "sql=",
                                        URLencode("SELECT * from \"0c231d4c-1ea7-43c5-a041-a3a6b02bac5e\""))

  conservation_report_list_sql <- fromJSON(conservation_report_url_sql)
  conservation_report_raw_data <- conservation_report_list_sql$result$records

  # formatting
  use_types <- c("reported_final_commercial_agricultural_water",
                 "reported_final_total_potable_water_production",
                 "calculated_total_potable_water_production_gallons_ag_excluded",
                 "reported_non_revenue_water",
                 "reported_final_commercial_industrial_and_institutional_water",
                 "reported_recycled_water",
                 "final_percent_residential_use")
  conservation_report_data <- conservation_report_raw_data |>
    mutate(reporting_date = as.Date(reporting_month),
           year = lubridate::year(reporting_date),
           month = lubridate::month(reporting_date),
           pwsid = public_water_system_id) |>
    select(-`_full_text`, -reporting_month, -reporting_date,
           supplier_name,
           reported_final_commercial_agricultural_water,
           reported_final_total_potable_water_production,
           calculated_total_potable_water_production_gallons_ag_excluded,
           reported_non_revenue_water, # leaks
           reported_final_commercial_industrial_and_institutional_water,
           reported_recycled_water, final_percent_residential_use, pwsid
    ) |>
    pivot_longer(cols = all_of(use_types), names_to = "use_type", values_to = "volume") |>
    mutate(category = case_when(use_type %in% c("reported_final_total_potable_water_production",
                                                "calculated_total_potable_water_production_gallons_ag_excluded") ~ "supply",
                                use_type == "reported_non_revenue_water" ~ "other",
                                TRUE ~ "demand")) |>
    transmute("report_name" = "monthly_conservation_report",
              "pwsid" = pwsid,
              "supplier_name" = supplier_name,
              "year" = year,
              "month" = month,
              "category" = category,
              "use_type" = tolower(use_type),
              "volume_af" = ifelse(volume == "NaN", NA, as.numeric(volume)))

  return(conservation_report_data)

}

pull_data <-
  function(type = c("supply", "demand", "supply total", "demand total", "losses"),
           year = c(2013:current_year),
           report = c("EAR", "UWMP", "CR", "WLA"),
           pwsid) {
    cr <- get_cr_data()
    wla <- get_wla_data()
    uwmp <- get_uwmp_data()
    data <- bind_rows(ear_data_format, uwmp, wla, cr)
    if (missing(pwsid)) {
      filter(data, year %in% year, report_name %in% report, category %in% type)
    } else{
      filter(data,
             year %in% year,
             report_name %in% report,
             category %in% type,
             pwsid %in% pwsid)
    }
  }

