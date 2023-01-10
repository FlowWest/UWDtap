# UWDtap <a href='https://FlowWest.github.io/UWDtap'><img src='man/figures/logo.png' align="right" height="300" style="float:right; height:300px;"/></a> 

# What is UWDtap?
UWDtap connects four distinct water reporting datasets to allow users to download and compile urban water data across California State Department of Water Resources (DWR) and California State Water Resources Control Board (SWRCB) reporting requirements with a single function call. UWDtap pulls data from the following reports:

* [Conservation Report (CR)](https://data.ca.gov/dataset/drinking-water-public-water-system-operations-monthly-water-production-and-conservation-information)
* [Electronic Annual Report (EAR)](https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/eardata.html)
* [Urban Water Management Plan (UWMP)](https://wuedata.water.ca.gov/)
* [Water Loss Audit (WLA)](https://wuedata.water.ca.gov/public/awwa_data_export/water_audit_data_conv_to_af.xls)

Many retail and wholesale water agencies across California are required to submit these reports to DWR and SWRCB. To access and use data, a user must download many excel or text files from four distinct web portals. UWDtap makes data across these four urban water reports easily accessible and interoperable. UWDtap provides the groundwork for innovative analysis that could be used to support data driven decision making, the development of dashboards and data visualizations, and also provides a helpful tool that could be used for data quality checks.  The goal of this package is to provide a service that pulls data and performs basic standardization for analysis ready data.


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


![](man/figures/pull_data_table.png)


Summarize these data across all years for a specific category and report,

```r
# summarize data from all reporting requirements for total demand
demand_summary <- pull_data_summary(category_selection = "demand total")
```

The data returned,

![](man/figures/pull_data_summary_table.png)

Visualize these data for a specific category and pwsid,

```r 
urban_water_data |> 
    plot_data(data = data, category_selection = "demand", 
          pwsid_selection = "CA0710001")
```

![](man/figures/demand_plot.png)

Note: The package does not QA/QC data. It does preform basic mapping of fields so that data can be compared across reports.

  
  
