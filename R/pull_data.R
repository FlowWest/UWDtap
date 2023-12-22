#' @title Get uwmp data
#' @description This function pulls uwmp data from wue portal. This is a helper function used by the \code{pull_data()} function
#' @source
#' WUE data portal https://wuedata.water.ca.gov/
#' UWMP required every five years
#' 2020 data comes from this site https://data.cnra.ca.gov/dataset/2020-uwmp-data-export-tables
#' 2015 data comes from this site https://wuedata.water.ca.gov/uwmp_export.asp
#' \href{https://wuedata.water.ca.gov/}{Urban Water Management Plan (UWMP)}
#' @export
get_uwmp_data <- function() {
  suppressMessages({
  temp <- tempfile()
  download.file("https://wuedata.water.ca.gov/public/uwmp_data_export/uwmp_table_2_1_r_conv_to_af.xls", temp, quiet = TRUE)
  pwsid <- read.table(temp, header = TRUE, sep = "\t", fill = TRUE) |> select(ORG_ID, PUBLIC_WATER_SYSTEM_NUMBER)
  unlink(temp)

  wue_datasets <- data_source |>
    filter(report_name == "UWMP") |>
    select(year, url, file_name)

  read_table <- function(year, url, file_name){
    print(file_name)
    temp <- tempfile()
    download.file(url, temp, quiet = TRUE)
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
                  use_type = tolower(WATER_DEMAND_TYPE),
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
  })
}

#' @title Get wla data
#' @description This function pulls wla data from wue portal. This is a helper function used by the \code{pull_data()} function
#' @source \href{https://wuedata.water.ca.gov/public/awwa_data_export/water_audit_data_conv_to_af.xls}{Water Loss Audit (WLA)}
#' @export

get_wla_data <- function() {
  # download file
  temp <- tempfile()
  download.file("https://wuedata.water.ca.gov/public/awwa_data_export/water_audit_data_conv_to_af.xls",
                destfile = temp,
                quiet = TRUE)
  wla_data_raw <- read_delim(temp, delim = "\t")
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
  other_fields <- other_fields[!stringr::str_detect(other_fields, "WL_WATER_LOSSES_VOL_AF")]

  # select supply use cases and supplier name, reporting year, and volume
  wla_supply <- wla_data_raw |>
    mutate(WS_OWN_SOURCES_VOL_AF = as.numeric(WS_OWN_SOURCES_VOL_AF)) |>
    select(WATER_SUPPLIER_NAME, WUEDATA_PLAN_REPORT_YEAR, VOLUME_REPORTING_UNITS,
           PWS_ID_OR_OTHER_ID, all_of(supply_fields)) |>
    pivot_longer(cols = all_of(supply_fields), names_to = "use_type", values_to = "volume") |>
    transmute(report_name = "WLA",
              pwsid = PWS_ID_OR_OTHER_ID,
              supplier_name = WATER_SUPPLIER_NAME,
              year = WUEDATA_PLAN_REPORT_YEAR,
              month = NA,
              category = ifelse(use_type == "WS_WATER_SUPPLIED_VOL_AF", "supply total", "supply"),
              use_type = tolower(use_type),
              volume_af = volume)

  # select demand use cases and supplier name, reporting year, and volume
  wla_demand <- wla_data_raw |>
    select(WATER_SUPPLIER_NAME, WUEDATA_PLAN_REPORT_YEAR, VOLUME_REPORTING_UNITS, PWS_ID_OR_OTHER_ID, all_of(demand_fields),
           WL_WATER_LOSSES_VOL_AF) |>
    pivot_longer(cols = c(all_of(demand_fields), WL_WATER_LOSSES_VOL_AF), names_to = "use_type", values_to = "volume") |>
    transmute(report_name = "WLA",
              pwsid = PWS_ID_OR_OTHER_ID,
              supplier_name = WATER_SUPPLIER_NAME,
              year = WUEDATA_PLAN_REPORT_YEAR,
              month = NA,
              category = ifelse(use_type == "AC_AUTH_CONSUMPTION_VOL_AF", "demand total", "demand"),
              use_type = tolower(use_type),
              volume_af = volume)

  # select other use cases and supplier name, reporting year, and volume
  wla_losses <- wla_data_raw |>
    select(WATER_SUPPLIER_NAME, WUEDATA_PLAN_REPORT_YEAR, VOLUME_REPORTING_UNITS, PWS_ID_OR_OTHER_ID, all_of(other_fields)) |>
    pivot_longer(cols = all_of(other_fields), names_to = "use_type", values_to = "volume") |>
    transmute(report_name = "WLA",
              pwsid = PWS_ID_OR_OTHER_ID,
              supplier_name = WATER_SUPPLIER_NAME,
              year = WUEDATA_PLAN_REPORT_YEAR,
              month = NA,
              category = "losses",
              use_type = tolower(use_type),
              volume_af = volume)

  # bind all together
  wla_data <- bind_rows(wla_demand, wla_supply, wla_losses)

  # return
  return(wla_data)
}

#' @title Get CR data
#' @description This function pulls cr data from data.ca.gov. This is a helper function used by the \code{pull_data()} function
#' @source \href{https://data.ca.gov/dataset/drinking-water-public-water-system-operations-monthly-water-production-and-conservation-information}{Conservation Report (CR)}
#' @export

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
  scale_from_MG_to_AF <- 3.06888
  scale_from_CCF_to_AF <- 0.0023
  scale_from_G_to_AF <- 1/325851

  conservation_report_data <- conservation_report_raw_data |>
    mutate(reported_final_total_potable_water_production = ifelse(is.na(reported_final_total_potable_water_production),
                                                                    reported_preliminary_total_potable_water_production,
                                                                    reported_final_total_potable_water_production),
           final_percent_residential_use = ifelse(is.na(final_percent_residential_use),
                                                    preliminary_percent_residential_use,
                                                    final_percent_residential_use),
           reported_final_commercial_agricultural_water = ifelse(is.na(reported_final_commercial_agricultural_water),
                                                                   reported_preliminary_commercial_agricultural_water,
                                                                   reported_final_commercial_agricultural_water),
           reported_final_commercial_industrial_and_institutional_water = ifelse(is.na(reported_final_commercial_industrial_and_institutional_water),
                                                                                 reported_preliminary_commercial_industrial_and_institutional_wa,
                                                                                     reported_final_commercial_industrial_and_institutional_water)) %>%
    select(-c(reported_preliminary_total_potable_water_production,
           preliminary_percent_residential_use,
           reported_preliminary_commercial_agricultural_water,
           reported_preliminary_commercial_industrial_and_institutional_wa,
           county_under_drought_declaration, hydrologic_region, county,
           water_shortage_level_indicator, dwr_state_standard_level_corresponding_to_stage,
           calculated_r_gpcd, calculated_total_potable_water_production_gallons_ag_excluded,
           water_shortage_contingency_stage_invoked, calculated_total_potable_water_production_gallons_2013_ag_exclu,
           climate_zone, calculated_commercial_agricultural_water_gallons_2013, calculated_commercial_agricultural_water_gallons,
           `_full_text`, reference_2014_population, total_population_served, `_id`, qualification)) %>%
    pivot_longer(cols = -c(public_water_system_id, supplier_name, water_production_units, reporting_month, final_percent_residential_use),
                 names_to = "use_type", values_to = "volume") %>%
    transmute(report_name = "CR",
              pwsid = public_water_system_id,
              supplier_name = supplier_name,
              year = lubridate::year(reporting_month),
              month = lubridate::month(reporting_month),
              final_percent_residential_use = final_percent_residential_use,
              category = case_when(use_type %in% c("reported_final_total_potable_water_production",
                                                   "calculated_total_potable_water_production_gallons_ag_excluded") ~ "supply",
                                   use_type == "reported_non_revenue_water" ~ "demand",
                                   TRUE ~ "demand"),
              use_type = gsub("_", " ", tolower(use_type)),
              volume_af = case_when(
                water_production_units == "MG" ~ as.numeric(volume) * scale_from_MG_to_AF,
                water_production_units == "G" ~ as.numeric(volume) * scale_from_G_to_AF,
                water_production_units == "CCF" ~ as.numeric(volume) * scale_from_CCF_to_AF,
                water_production_units == "AF" ~ as.numeric(volume)
              )) |>
    mutate(volume_af = ifelse(is.nan(volume_af), NA, volume_af))

  residential_cr <- conservation_report_data |>
    filter(use_type == "reported final total potable water production") |>
    group_by(report_name, pwsid, supplier_name,
             year, month) |>
    summarize(final_prop_residential = as.numeric(final_percent_residential_use)/100,
              residential = volume_af * final_prop_residential) |>
    mutate(volume_af = ifelse(is.nan(residential), NA, residential),
           use_type = "final residential use",
           category = "demand") |>
    select(report_name, pwsid, supplier_name, year, month, category, use_type, volume_af)

  final_conservation_report_data <- conservation_report_data |>
    bind_rows(residential_cr) |>
    select(-final_percent_residential_use)

  return(final_conservation_report_data)

}


#' @title Get ear data
#' @description This function pulls ear data This is a helper function used by the \code{pull_data()} function
#' @param year_selection Year to filter data to.
#' @source \href{https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/eardata.html}{Electronic Annual Report (EAR)}
#' @export
get_ear_data <- function(year_selection) {
  ear_parameters <- filter(data_source, report_name == "ear", year == year_selection)
  url = ear_parameters$url
  file_name = ear_parameters$file_name
  temp <- tempfile()
  download.file(url, temp, quiet = TRUE)
  data <- vroom::vroom(unz(temp, file_name), delim = "\t")
  unlink(temp)
  if(year_selection < 2020) {
    data <- data |>
      dplyr::filter(
        SectionID %in% c("06 Supply-Delivery", "01 Intro", "1", "6") | SectionName %in% c("Intro", "Water Supplied", "Water Rates and Deliveries")) |>
      dplyr::mutate(
        SectionID = as.character(SectionID),
        SurveyID = as.character(SurveyID),
        QuestionID = as.character(QuestionID)
      ) |>
      dplyr::rename(WSID = PWSID,
                    SurveyName = Survey,
                    WSSurveyID = SurveyID)

  } else {
    if(year_selection == 2021) {
      data <- data |>
        # Column names do not match what the data are
        dplyr::mutate(
          Section = WSSurveyID,
          Question = QuestionID,
          Ord = QuestionName,
          Results = Order,
          Old = SectionID,
          QuesID = QuestionResults,
          SurvID = OldShortName_QuestionText) |>
        dplyr::select(-c(WSSurveyID, QuestionID, SectionID, Order, QuestionName,
                         QuestionResults, OldShortName_QuestionText)) |>
        dplyr::rename(SectionID = Section,
                      QuestionName = Question,
                      Order = Ord,
                      QuestionResults = Results,
                      OldShortName_QuestionText = Old,
                      QuestionID = QuesID,
                      WSSurveyID = SurvID)
    } else {
      data <- data
    }
    data <- data |>
      dplyr::filter(
        SectionID %in% c("06 Supply-Delivery", "01 Intro", "1", "6")) |>
      dplyr::mutate(
        WSSurveyID = as.character(WSSurveyID),
        QuestionID = as.character(QuestionID),
        Order = as.character(Order),
        QuestionName = as.character(QuestionName),
        QuestionResults = as.character(QuestionResults),
        OldShortName_QuestionText = as.character(OldShortName_QuestionText)
      ) |>
      tidyr::separate(SurveyName, c("Year", "Survey"))
  }
  ear_data <- data |>
    mutate(QuestionName = gsub(" ", "", QuestionName))

  # water supply and demand data are defined as fields that are either WP or WD
  ear_fields <- unique(ear_data$QuestionName)
  ear_supply_fields <- ear_fields[stringr::str_detect(ear_fields, "^WP")]
  ear_demand_fields <- ear_fields[stringr::str_detect(ear_fields, "^WD")]
  # The conversion factors below are used to make sure all volumes are in same units
  scale_from_MG_to_AF <- 3.06888
  scale_from_CCF_to_AF <- 0.0023
  scale_from_G_to_AF <- 1/325851

  # agency_lookup finds the supplier name (answer to a question) and creates a
  # lookup so the column "supplier_name" can be joined to the dataset
  agency_lookup <- ear_data |>
    filter(QuestionName %in% c("PWSName", "Water System Name")) |>
    select(WSID, Year, "supplier_name" = QuestionResults) |>
    distinct()
  # units_lookup_production finds the units used in the WP section and creates a
  # lookup so the units can be added to the data and volumes appropriately scaled
  units_lookup_production <- ear_data |>
    filter(QuestionName %in% c("WPUnitsofMeasure","WP Units of Measure")) |>
    select(WSID, Year, "units" = QuestionResults) |>
    distinct()
  # units_lookup_delivery finds the units used in the WD section and creates a
  # lookup so the units can be added to the data and volumes appropriately scaled
  units_lookup_delivery <- ear_data |>
    filter(QuestionName %in% c("WDUnitofMeasure", "WD Unit of Measure")) |>
    select(WSID, Year, "units" = QuestionResults) |>
    distinct()

  ear_supply_data <- filter(ear_data,
                            QuestionName %in% ear_supply_fields) |>
    mutate(have_month = substr(QuestionName, 3, 5),
           month = ifelse(have_month %in% month.abb, match(have_month, month.abb), NA),
           QuestionName = ifelse(have_month %in% month.abb,
                                 substr(QuestionName, 6, length(QuestionName)),
                                 substr(QuestionName, 3, length(QuestionName))),
           volume = as.numeric(QuestionResults)) |>
    left_join(agency_lookup) |>
    left_join(units_lookup_production) |>
    filter(!is.na(volume)) |>
    transmute(report_name = "EAR",
              pwsid = WSID,
              supplier_name = supplier_name,
              year = as.numeric(Year),
              month = month,
              category = ifelse(is.na(month), "supply total", "supply"),
              use_type = tolower(QuestionName),
              volume_af = case_when(
                units == "MG" ~ volume * scale_from_MG_to_AF,
                units == "G" ~ volume * scale_from_G_to_AF,
                units == "CCF" ~ volume * scale_from_CCF_to_AF,
                units == "-" | is.na(units) | units == "NA" ~ volume, # assume AF
                units == "AF" ~ volume
              ))

  ear_demand_data <- filter(ear_data,
                            QuestionName %in% ear_demand_fields) |>
    mutate(have_month = substr(QuestionName, 3, 5),
           month = ifelse(have_month %in% month.abb, match(have_month, month.abb), NA),
           QuestionName = ifelse(have_month %in% month.abb,
                                 substr(QuestionName, 6, length(QuestionName)),
                                 substr(QuestionName, 3, length(QuestionName))),
           volume = as.numeric(QuestionResults)) |>
    left_join(agency_lookup) |>
    left_join(units_lookup_delivery) |>
    filter(!is.na(volume)) |>
    transmute(report_name = "EAR",
              pwsid = WSID,
              supplier_name = supplier_name,
              year = as.numeric(Year),
              month = month,
              category = ifelse(is.na(month), "demand total", "demand"),
              use_type = tolower(QuestionName),
              volume_af = case_when(
                units == "MG" ~ volume * scale_from_MG_to_AF,
                units == "G" ~ volume * scale_from_G_to_AF,
                units == "CCF" ~ volume * scale_from_CCF_to_AF,
                units == "AF" ~ volume
              ))
  ear_data <- bind_rows(ear_demand_data, ear_supply_data)
  return(ear_data)
}

#' @title Pull Data
#' @description Pull one year of data from the CR, EAR, UWMP, and WLA and compiles it into a single format.
#' @param category_selection Describes type of data to pull. Can be \code{"supply"},
#' \code{"supply total"}, \code{"demand"}, \code{"demand total"}, \code{"losses"}, \code{other}
#' @param year_selection One year to filter data to (ex \code{2016}).
#' @param report_selection Specifies which report to pull data from. Can be one of \code{"EAR"}, \code{"UWMP"}
#' \code{"CR"}, or \code{"WLA"}
#' @return A table containing 8 columns: \code{report_name}, \code{pwsid}, \code{supplier_name}, \code{year}, \code{month},
#' \code{category}, \code{volume_af}, \code{use_group}
#' @source
#' Get Summary Data pulls data from the following reports:
#' \href{https://data.ca.gov/dataset/drinking-water-public-water-system-operations-monthly-water-production-and-conservation-information}{Conservation Report (CR)},
#' \href{https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/eardata.html}{Electronic Annual Report (EAR)},
#' \href{https://wuedata.water.ca.gov/}{Urban Water Management Plan (UWMP)},
#' \href{https://wuedata.water.ca.gov/public/awwa_data_export/water_audit_data_conv_to_af.xls}{Water Loss Audit (WLA)}
#' @export

pull_data <-
  function(category_selection = c("supply",
                                  "demand",
                                  "supply total",
                                  "demand total",
                                  "losses",
                                  "other"),
           year_selection,
           report_selection = c("EAR", "UWMP", "CR", "WLA"),
           pwsid,
           ...) {
    if (length(year_selection) > 1) {
      stop("Please select one year. Use pull_data_summary to summarize more than one year.")
    }
    suppressWarnings({
      suppressMessages({
        cr <- get_cr_data() |>
          filter(year == year_selection)
        wla <- get_wla_data() |>
          filter(year == year_selection)
        uwmp <- get_uwmp_data() |>
          filter(year == year_selection)
        ear <- get_ear_data(year_selection, ...)
        data <- bind_rows(uwmp, wla, cr, ear)
        if (missing(pwsid)) {
          all_data <-
            filter(data,
                   report_name %in% report_selection,
                   category %in% category_selection)
        } else{
          all_data <- filter(
            data,
            report_name %in% report_selection,
            category %in% category_selection,
            pwsid %in% pwsid
          )
        }
        all_data_formatted <- all_data |>
          left_join(use_type_lookup) |>
          mutate(category = case_when(use_group == "total volume" & category == "demand" ~ "demand total",
                                      use_group == "total volume" & category == "supply" ~ "supply total",
                                      T ~ category)) |>
          select(-use_type)
        return(all_data_formatted)
      })
    })
  }

#' @title Pull Data Summary
#' @description Summarizes data from the CR, EAR, UWMP, and WLA into summary table.
#' @param category_selection Describes type of data to pull. Can be \code{"supply"},
#' \code{"supply total"}, \code{"demand"}, \code{"demand total"}, \code{"losses"}, \code{other}
#' @param report_selection Specifies which report to pull data from. Can be one of \code{"EAR"}, \code{"UWMP"}
#' \code{"CR"}, or \code{"WLA"}
#' @source
#' Pull summary data pulls data from the following reports:
#' \href{https://data.ca.gov/dataset/drinking-water-public-water-system-operations-monthly-water-production-and-conservation-information}{Conservation Report (CR)},
#' \href{https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/eardata.html}{Electronic Annual Report (EAR)},
#' \href{https://wuedata.water.ca.gov/}{Urban Water Management Plan (UWMP)},
#' \href{https://wuedata.water.ca.gov/public/awwa_data_export/water_audit_data_conv_to_af.xls}{Water Loss Audit (WLA)}
#' @export
pull_data_summary <-
  function(category_selection = c("supply",
                                  "demand",
                                  "supply total",
                                  "demand total",
                                  "losses",
                                  "other"),
           report_selection = c("EAR", "UWMP", "CR", "WLA")) {
    suppressWarnings({
      suppressMessages({
        wla <- get_wla_data()
        cr <- get_cr_data()
        uwmp <- get_uwmp_data()
        data <- bind_rows(wla, cr, uwmp) |>
          filter(report_name %in% report_selection,
                 category %in% category_selection) |>
          group_by(report_name, year, month, category, use_type) |>
          summarise(
            mean = mean(volume_af, na.rm = T),
            median = median(volume_af, na.rm = T),
            q25 = quantile(volume_af, probs = 0.25, na.rm = T),
            q75 = quantile(volume_af, probs = 0.75, na.rm = T),
            sd = sd(volume_af, na.rm = T),
            n = length(unique(pwsid))
          ) |>
          bind_rows(ear_summary) |>
          left_join(use_type_lookup) |>
          pivot_longer(
            cols = c(mean, median, q25, q75, sd),
            names_to = "statistic",
            values_to = "volume_af"
          ) |>
          filter(!is.na(use_group)) |>
          select(-use_type)
      })
    })

  }
