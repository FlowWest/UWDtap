#' @title PWSID Lookup Table
#' @description This lookup table helps a user find a PWSID for a specific agency using the agency's name.
#' @format A table with 2 columns (`water_system_name` & `pwsid`) and 7,898 rows
#' @source (CA open data portal)[https://data.ca.gov/dataset/drinking-water-public-water-system-information/resource/9dca2f92-4630-4bee-a9f9-69d2085b57e3]
#' plot_data(selected_category = "demand", pwsid = "CA3610001")
"pwsid_lookup"

#' @title Parameters
#' @description This table provides links to all the sources that contain UWD.
#' @format A tibble with 4 columns (`year`, `url`, `file_name`, `report_name`) and 23 rows
"parameters"

#' @title Use Type Lookup
#' @description This lookup table helps map use types bewtween reports
#' @format A tibble with 4 columns (`use_group`, `report_name`, `use_type`, `category`) and 141 rows
"use_type_lookup"
