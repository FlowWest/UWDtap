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
         pwsid = water_system_no)

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

parameters <- bind_rows(ear_parameters, uwmp_paramters, wla_parameters, cr_parameters)

demand_lookup <- read_rds("data-raw/use_type_lookup.rds")
supply_lookup <- read_rds("data-raw/supply_type_lookup.rds")
use_type_lookup <- bind_rows(demand_lookup |>
                               mutate(category = "demand"),
                             supply_lookup |>
                               mutate(category = "supply"))

usethis::use_data(pwsid_lookup, parameters, use_type_lookup, overwrite = T)




