# UWDtap <a href='https://FlowWest.github.io/UWDtap'><img src='man/figures/logo.png' align="right" height="200" style="float:right; height:200px;"/></a> 

# What is UWDtap?
UWDtap connects four distinct water reporting datasets to allow users to download and compile urban water data across reporting requirements with a single function call. UWDtap pulls data from the following reports:

* [Conservation Report (CR)](https://data.ca.gov/dataset/drinking-water-public-water-system-operations-monthly-water-production-and-conservation-information)
* [Electronic Annual Report (EAR)](https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/eardata.html)
* [Urban Water Management Plan (UWMP)](https://wuedata.water.ca.gov/)
* [Water Loss Audit (WLA)](https://wuedata.water.ca.gov/public/awwa_data_export/water_audit_data_conv_to_af.xls)

Many retail and wholesale water agencies across California are required to submit these reports to the Department of Water Resources or the State Water Board. Currently a user must dowload many datasets from 4 distinct web portals to preform any analysis on urban water data. The data are not standardized across reporting requirements even through many reporting requirements collect the same data. The goal of this package is to provide a service that pulls data and preforms basic standardization - generating analysis ready data.


# Installation

```r 
remotes::install_github("flowwest/UWDtap")
```

# Basic Usage 

UWDtap exposes several useful functions to pull and visualize data urban water reporting data. 
The main function in the package is `pull_data()`, 

```r 
# download data from all reporting requirements for 2020
urban_water_data <- pull_data(year_selection = 2020)
```

The data returned,

```
urban_water_data |> glimpse()

Rows: 1,120,132
Columns: 9
$ report_name   <chr> "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", "UWMP", …
$ pwsid         <chr> "CA3610001", "CA3610001", "CA3610001", "CA3610001", "CA3610001", "CA3610001", "CA3610001", "CA0110001", …
$ supplier_name <chr> "Adelanto  City Of", "Adelanto  City Of", "Adelanto  City Of", "Adelanto  City Of", "Adelanto  City Of",…
$ year          <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 20…
$ month         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
$ category      <chr> "demand", "demand", "demand", "demand", "demand", "demand", "demand", "demand", "demand", "demand", "dem…
$ use_type      <chr> "multi-family", "commercial", "industrial", "landscape", "other potable", "losses", "single family", "in…
$ volume_af     <dbl> 162, 809, 0, 82, 1, 987, 2606, 2410, 4245, 1064, 5463, 83, 4483, 1569, 0, 7190, 463, 18762, 7813, 2994, …
$ use_group     <chr> "residential", "commerical industrial institutional", "commerical industrial institutional", "landscape"…
```

Visualize these data for a specific category and pwsid,

```r 
urban_water_data |> 
     plot_data(selected_category = "supply",
               pwsid = "", 
               show_subcategories = FALSE)
```
Note: The package does not QA/QC data. It does preform basic mapping of fields so that data can be compared across reports.

  
  
