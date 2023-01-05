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

usethis::use_data(pwsid_lookup)




