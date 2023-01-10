#' @title PWSID Lookup Table
#' @description This lookup table helps a user find a PWSID for a specific agency using the agency's name.
#' @format A table with 2 columns (\code{water_system_name} & \code{pwsid}) and 7,898 rows
#' @source \href{https://data.ca.gov/dataset/drinking-water-public-water-system-information/resource/9dca2f92-4630-4bee-a9f9-69d2085b57e3}{CA open data portal}
"pwsid_lookup"

#' @title Data Source
#' @description This table provides links to all the web pages used to download urban water data.
#' @format A tibble with 4 columns (\code{year}, \code{url}, \code{file_name}, \code{report_name}) and 23 rows
"data_source"

#' @title Use Type Lookup
#' @description This lookup table helps map use types between reports
#' @format A tibble with 4 columns (\code{use_group}, \code{report_name}, \code{use_type}, \code{category}) and 141 rows
"use_type_lookup"
