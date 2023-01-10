library(jsonlite)
library(tidyverse)

# Pull pwsid lookup table
pwsid_url_sql <- paste0("https://data.ca.gov/api/3/action/",
                        "datastore_search_sql?",
                        "sql=",
                        URLencode("SELECT * from \"9dca2f92-4630-4bee-a9f9-69d2085b57e3\""))

pwsid_list_sql <- fromJSON(pwsid_url_sql)
pwsid_raw_data <- pwsid_list_sql$result$records

pwsid_lookup <- pwsid_raw_data |>
  janitor::clean_names() |>
  select(water_system_name,
         pwsid = water_system_no,
         county = principal_county_served)

# Parameters Table
# EAR
url_2021 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/2021ry-portalclosed-08302022.zip"
txt_2021 <- "QAResults_2021RY_PublicFinal_08302022.txt"
url_2020 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/2020ry-portalclosed-032822.zip"
txt_2020 <- "2020RY_PortalClosed_032822.txt"
url_2019 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/2019ry_resultset_08192021.zip"
txt_2019 <- "2019RY_Resultset_08192021.txt"
url_2018 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/earsurveyresults_2018ry.zip"
txt_2018 <- "EARSurveyResults_2018RY.txt"
url_2017 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/earsurveyresults_2017ry.zip"
txt_2017 <- "EARSurveyResults_2017RY.txt"
url_2016 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/earsurveyresults_2016ry.zip"
txt_2016 <- "EARSurveyResults_2016RY.txt"
url_2015 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/earsurveyresults_2015ry.zip"
txt_2015 <- "EARSurveyResults_2015RY.txt"
url_2014 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/earsurveyresults_2014ry.zip"
txt_2014 <- "EARSurveyResults_2014RY.txt"
url_2013 <- "https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/ear/earsurveyresults_2013ry.zip"
txt_2013 <- "2013RY_v2.txt"

ear_parameters <- tibble(year = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
                         url = c(url_2013, url_2014, url_2015, url_2016, url_2017, url_2018, url_2019, url_2020, url_2021),
                         file_name = c(txt_2013, txt_2014, txt_2015, txt_2016, txt_2017, txt_2018, txt_2019, txt_2020, txt_2021),
                         report_name = rep("ear", 9))

# UWMP
uwmp_paramters <- read.csv("data-raw/wue_datasets.csv")

# WLA
wla_parameters <- tibble(report_name = "WLA",
                         url = "https://wuedata.water.ca.gov/public/awwa_data_export/water_audit_data_conv_to_af.xls",
                         file_name = NA,
                         year = NA)
# CR
cr_parameters <- tibble(report_name = "CR",
                        url = "https://data.ca.gov/dataset/drinking-water-public-water-system-operations-monthly-water-production-and-conservation-information",
                        file_name = NA,
                        year = NA)

data_source <- bind_rows(ear_parameters, uwmp_paramters, wla_parameters, cr_parameters)

demand_lookup <- read_rds("data-raw/use_type_lookup.rds")
supply_lookup <- read_rds("data-raw/supply_type_lookup.rds")
use_type_lookup <- bind_rows(demand_lookup |>
                               mutate(category = "demand"),
                             supply_lookup |>
                               mutate(category = "supply")) |>
  mutate(report_name = ifelse(report_name == "WLR", "WLA", report_name),
         category = ifelse(report_name == "EAR" & category == "supply", "supply total", category),
         use_group = ifelse(use_group == "recycled water", "recycled", use_group))

new_additions <- tibble(use_group = c("nonpotable", "surface water","imported/purchased",
                                      "total volume", "groundwater", "recycled",
                                      "sold", "total volume", "agricultural irrigation",
                                      "landscape","residential", "other","sales transfers exchanges to other agencies",
                                      "residential","commerical industrial institutional", "landscape", "total volume",
                                      "surface water", "other", "surface water", "groundwater",
                                      "sales transfers exchanges to other agencies", "sales transfers exchanges to other agencies",
                                      "recycled", "other", "commerical industrial institutional", "losses", "total volume"),
                        report_name = c(rep("EAR",17), rep("UWMP", 8), rep("CR",2), rep("WLA",1)),
                        use_type = c("nonpotable", "sw", "purchased", "total",
                                     "gw", "recycled", "sold", "total", "annuala",
                                     "annuali", "annualsf", "annualo","annualop",
                                     "annualmf","annualci","annualli","annualtotal",
                                     "surface water", "stormwater use", "desalinated water",
                                     "groundwater", "transfers to other agencies",
                                     "sales to other agencies", "recycled water demand",
                                     "potable and raw water","reported final commercial industrial and institutional water",
                                     "reported non revenue water", "ws_water_supplied_vol_af"),
                        category = c(rep("supply",7), "demand", rep("demand total", 9),
                                     rep("supply",4), rep("demand",2), rep("demand total",2), rep("demand",2), "supply total"))

use_type_lookup <- use_type_lookup |>
  bind_rows(new_additions)

# Currently takes a few minutes to create the ear summary so using
# static version of ear summary for performance
ear_parameters <- filter(parameters, report_name == "ear")
ear_year_parameters <- tibble(year_selection = ear_parameters$year)

get_ear_summary <- function(year_selection, ...) {
  data <- get_ear_data(year_selection, ...) |>
    group_by(report_name, year, month, category, use_type) |>
    summarise(mean = mean(volume_af, na.rm = T),
              median = median(volume_af, na.rm = T),
              q25 = quantile(volume_af, probs = 0.25, na.rm = T),
              q75 = quantile(volume_af, probs = 0.75, na.rm = T),
              sd = sd(volume_af, na.rm = T),
              n = length(unique(pwsid)))
}

ear_summary <- purrr::map_dfr(pmap(ear_year_parameters, get_ear_summary), bind_rows)

usethis::use_data(pwsid_lookup, data_source, use_type_lookup, ear_summary, overwrite = T)




