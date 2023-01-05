# UWDtap <a href='https://FlowWest.github.io/UWDtap'><img src='man/figures/logo.png' align ="right" height="225" /></a> 

# What is UWDtap?

UWDtap connects to 4 distinct water reporting datasets to allow users to download 
and compile urban water data across reporting requirements with a single function call. 
UWDtap pulls data from the following reports: 

* Conservation Report (CR)
* Electronic Annual Report (EAR)
* Urban Water Management Plan (UWMP)
* Water Loss Audit (WLA)

Many retail and wholesale water agencies across California are required to submit 
these reports to the Department of Water Resources or the State Water Board. 

Currently to analyze these datasets a user must dowload many datasets from 4 distinct 
web portals. The files are not standardized across reporting requirements even 
through many reporting requirements collect the same data. 



# Installation

```r 
remotes::install_github("flowwest/UWDtap")
```

# Basic Usage 

UWDtap exposes several useful functions to pull and visualize data urban water reporting data. 
The main function in the package is `pull_data()`, 

```r 
# download data from all reporting requirements for all years
urban_water_data <- pull_data()
```

The data returned,

```
```

Visualize these datasets,

```r 
plot_data()
```


  
  
