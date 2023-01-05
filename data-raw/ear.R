# EAR Data Pull #
library(purrr)
library(dplyr)
library(tidyr)
library(vroom)
library(data.table)
library(parallel)
library(doParallel)

# Function to pull all ear water data (separate text file for each year) and
# format to bind years together
get_all_ear_water_data <- function() {
  # Function used to download text file for old format #
  ear_download_txt_pre2020_format <- function(year, parameters) {
    url = parameters[year, ]$url
    file_name = parameters[year, ]$file_name
    temp <- tempfile()
    download.file(url, temp)
    data <- vroom::vroom(unz(temp, file_name), delim = "\t")
    unlink(temp)
    data |>
      dplyr::mutate(
        SectionID = as.character(SectionID),
        SurveyID = as.character(SurveyID),
        QuestionID = as.character(QuestionID)
      ) |>
      dplyr::rename(WSID = PWSID,
                    SurveyName = Survey,
                    WSSurveyID = SurveyID) |>
      dplyr::filter(
        SectionID %in% c("06 Supply-Delivery", "01 Intro", "1", "6") | SectionName %in% c("Intro", "Water Supplied", "Water Rates and Deliveries"))
  }
  # Function used to download text file for new format #
  ear_download_txt_2020_format <- function(year, parameters) {
    url = parameters[year, ]$url
    file_name = parameters[year, ]$file_name
    temp <- tempfile()
    download.file(url, temp)
    data <- vroom::vroom(unz(temp, file_name), delim = "\t")
    unlink(temp)
    data |>
      dplyr::mutate(
        WSSurveyID = as.character(WSSurveyID),
        QuestionID = as.character(QuestionID),
        Order = as.character(Order),
        QuestionName = as.character(QuestionName),
        QuestionResults = as.character(QuestionResults),
        OldShortName_QuestionText = as.character(OldShortName_QuestionText)
      ) |>
      dplyr::filter(
        SectionID %in% c("06 Supply-Delivery", "01 Intro", "1", "6")) |>
      tidyr::separate(SurveyName, c("Year", "Survey"))
  }
  # There are two different download functions: (1) pre 2020 format, (2) 2020+ format
  # Define the parameters needed to run each function
  ear_parameters <- filter(parameters, report_name == "ear")
  parameters_2020 <- ear_parameters |>
    filter(year >= 2020)
  parameters_pre2020 <- ear_parameters |>
    filter(year < 2020)
  # Set up for running in parallel
  no_cores <- detectCores(logical = TRUE)
  cl <- makeCluster(no_cores - 1)
  registerDoParallel(cl)

  clusterExport(cl, list("ear_download_txt_2020_format", "parameters_2020",
                         "ear_download_txt_pre2020_format", "parameters_pre2020"),
                envir=environment())

  ear_raw_2020_format <- parLapply(cl, 1:nrow(parameters_2020),
                                   fun = function(year) {
                                     ear_download_txt_2020_format(year, parameters_2020)
                                   })

  ear_raw_pre2020_format <- parLapply(cl, 1:nrow(parameters_pre2020),
                                      fun = function(year) {
                                        ear_download_txt_pre2020_format(year, parameters_pre2020)
                                      })

  all_ear_raw <- data.table::rbindlist(c(ear_raw_2020_format,
                                         ear_raw_pre2020_format), fill = T)
  all_ear_format <- all_ear_raw |>
    filter(
      Year %in% unique(ear_parameters$year)
    )
  all_ear_format
}

ear_data <- get_all_ear_water_data()

# EAR formatting for integration
# The conversion factors below are used to make sure all volumes are in same units
scale_from_MG_to_AF <- 3.06888
scale_from_CCF_to_AF <- 0.0023
scale_from_G_to_AF <- 1/325851
# agency_lookup finds the supplier name (answer to a question) and creates a
# lookup so the column "supplier_name" can be joined to the dataset
agency_lookup <- ear_data |>
  filter(QuestionName %in% c("PWSName", "Water System Name")) |>
  select(WSID, Year, "supplier_name" = QuestionResults)
# units_lookup_production finds the units used in the WP section and creates a
# lookup so the units can be added to the data and volumes appropriately scaled
units_lookup_production <- ear_data |>
  filter(QuestionName %in% c("WPUnitsofMeasure","WP Units of Measure")) |>
  select(WSID, Year, "units" = QuestionResults)
# units_lookup_delivery finds the units used in the WD section and creates a
# lookup so the units can be added to the data and volumes appropriately scaled
units_lookup_delivery <- ear_data |>
  filter(QuestionName %in% c("WDUnitofMeasure", "WD Unit of Measure")) |>
  select(WSID, Year, "units" = QuestionResults)
# water supply and demand data are defined as fields that are either WP or WD
ear_fields <- unique(ear_data$QuestionName)
ear_supply_fields <- trimws(ear_fields[stringr::str_detect(ear_fields, "^WP")])
ear_demand_fields <- trimws(ear_fields[stringr::str_detect(ear_fields, "^WD")])

# Supply
# Uses data.table to help with performance of formatting
setDT(ear_data)
ear_supply_data <- ear_data[, QuestionName := trimws(QuestionName)
                            ][QuestionName %in% ear_supply_fields,
                              ][, have_month := substr(QuestionName, 3, 5)
                                ][, month := ifelse(have_month %in% month.abb, match(have_month, month.abb), NA)
                                  ][, QuestionName := ifelse(have_month %in% month.abb,
                                                             substr(QuestionName, 6, length(QuestionName)),
                                                             substr(QuestionName, 3, length(QuestionName)))
                                    ][, volume := as.numeric(QuestionResults)] |>
  left_join(agency_lookup) |>
  left_join(units_lookup_delivery) |>
  filter(!is.na(volume)) |>
  transmute("report_name" = "EAR",
            "pwsid" = WSID,
            "supplier_name" = supplier_name,
            "year" = as.numeric(Year),
            "month" = month,
            "category" = ifelse(is.na(month), "supply total", "supply"),
            "use_type" = tolower(QuestionName),
            "volume_af" = case_when(
              units == "MG" ~ volume * scale_from_MG_to_AF,
              units == "G" ~ volume * scale_from_G_to_AF,
              units == "CCF" ~ volume * scale_from_CCF_to_AF,
              units == "-" | is.na(units) | units == "NA" ~ volume, # assume AF
              units == "AF" ~ volume
            ))

# Demand
# Uses data.table to help with performance of formatting
ear_demand_data <- ear_data[, QuestionName := trimws(QuestionName)
                         ][QuestionName %in% ear_demand_fields,
                           ][, have_month := substr(QuestionName, 3, 5)
                             ][, month := ifelse(have_month %in% month.abb, match(have_month, month.abb), NA)
                               ][, QuestionName := ifelse(have_month %in% month.abb,
                                                          substr(QuestionName, 6, length(QuestionName)),
                                                          substr(QuestionName, 3, length(QuestionName)))
                                 ][, volume := as.numeric(QuestionResults)] |>
  left_join(agency_lookup) |>
  left_join(units_lookup_delivery) |>
  filter(!is.na(volume)) |>
  transmute("report_name" = "EAR",
            "supplier_name" = supplier_name,
            "year" = as.numeric(Year),
            "month" = month,
            "category" = ifelse(is.na(month), "demand total", "demand"),
            "use_type" = tolower(QuestionName),
            "volume_af" = case_when(
              units == "MG" ~ volume * scale_from_MG_to_AF,
              units == "G" ~ volume * scale_from_G_to_AF,
              units == "CCF" ~ volume * scale_from_CCF_to_AF,
              units == "AF" ~ volume
            ))

ear_data_format <- bind_rows(ear_demand_data, ear_supply_data)

usethis::use_data(current_year, parameters, ear_data_format, internal = TRUE, overwrite = T)

