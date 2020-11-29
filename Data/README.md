# COVID-19 Assignement
Maeva Braeckevelt

This analysis aimed at analyzing the pattern of association between registered COVID-19 cases and registered death due to COVID-19 until the 26 October 2020 among countries.

# Analytic plan #

## Importing the data

The data were imported from  https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_daily_reports/10-26-2020.csv
and from the WDI website (I used the WDI package in R).

In the raw data folder there is :
* covid_10_26_2020_raw.csv : the raw data imported from CSSEGI
* pop_WDI_2019.csv : the raw data imported from WDI
* variables_CSSEGI_WDI.csv : two sheets with the detailed variables of these two data tables

## Cleaning the data
In the clean data folder there is :
* covid_pop_10_26_2020_clean.csv : the cleaned data with the two data tables merged
* variables_clean.csv : the detailed variable of the clean data table
