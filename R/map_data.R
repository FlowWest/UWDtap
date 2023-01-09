library(tidyverse)
library(jsonlite)
library(tidyverse)
library(RColorBrewer)
library(leaflet)

#' @title Map Urban Water Data
#' @description This function generates a county-level map of Urban Water Data.
#' @param data Data to plot. Must be in the format outputted from \code{pull_data()} function and to avoid
#' summing duplicate rows of data in a county, please only use data with \code{category} "demand_total"
#' or "supply_total".
#' @examples
#' map_data(data)
#' @export
map_county_data <- function(data) {

  # if no PWSID, exit function
  check <- tryCatch(!"pwsid" %in% names(data),
                    error = function(e) print("data does not contain a Public Water System ID (PWSID)"))

  # get counties associated with pwsid
  load(file = here::here("data", "pwsid_lookup.rda"))
  pwsid_data <- pwsid_lookup |>
    mutate(county = tolower(county))

  # join counties to data
  county_level_data <- left_join(data, pwsid_data, by = "pwsid") |>
    group_by(county, year) |>
    summarise(total_county_af = sum(volume_af, na.rm = T),
              no_urban_water_suppliers = n_distinct(supplier_name)) |>
    distinct(county, total_county_af, no_urban_water_suppliers)

  # create palette and labels
  pal_county_af <- colorNumeric(palette = "BuPu", log(county_level_data$total_county_af))

  if(stringr::str_detect(unique(data$category), "demand")) {
    label_suppliers <- function(county, no_suppliers, total_county_af) {
      paste(str_glue("No. of urban water suppliers in {stringr::str_to_title(county)} county: {no_suppliers}"),
            "<br>",
            str_glue("Demand in acre feet: {round(total_county_af,0)}"))
    }
  }
  if(stringr::str_detect(unique(data$category), "supply")) {
    label_suppliers <- function(county, no_suppliers, total_county_af) {
      paste(str_glue("No. of urban water suppliers in {stringr::str_to_title(county)} county: {no_suppliers}"),
            "<br>",
            str_glue("Supply in acre feet: {round(total_county_af,0)}"))
    }
  }


  # get shapefiles and filter to matching counties in CA
  USA <- raster::getData("GADM", country = "usa", level = 2)
  USA$NAME_2 = tolower(USA$NAME_2)
  USA <- USA[USA$NAME_1 == "California",]
  USA <- USA[USA$NAME_2 %in% county_level_data$county,]

  # leaflet plot
  leaflet() |> addTiles() |>
    setView(lat = 36.778259, lng = -119.417931, zoom = 5) |>
    addPolygons(data = USA,
                fillColor = ~pal_county_af(log(county_level_data$total_county_af)),
                fillOpacity = 0.8,
                popup = ~label_suppliers(county_level_data$county,
                                         county_level_data$no_urban_water_suppliers,
                                         county_level_data$total_county_af),
                stroke = T,
                weight = 0.7)
}




