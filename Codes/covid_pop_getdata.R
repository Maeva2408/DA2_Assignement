#######################
## Analysis of       ##
##  Life expectancy  ##
##    and            ##
##  GPD/capita       ##
##                   ##
##      NO. 1        ##
##                   ##
##  Getting the data ##
##                   ##
#######################


# Clear memory
rm(list=ls())

# Call packages
install.packages('WDI')
library(WDI)
library(tidyverse)




#####
my_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/10-25-2020.csv"
read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/10-25-2020.csv")
df <- read_csv( my_url )



# How WDI works - it is an API
# Search for variables which contains GDP
a <- WDIsearch('population')

# Get data
pop_data = WDI(indicator='SP.POP.TOTL', country="all", start=2019, end=2019)


a <- WDIsearch('population, total')
b <- WDIsearch('life expectancy at birth')



# Save the raw data file
my_path <- "C:/Users/mbrae/OneDrive/Bureau/CEU/DA2/DA2_Assignement/Data"
write_csv(pop_data, paste0(my_path,'/raw/WDI_pop_raw.csv'))
my_path <- "C:/Users/mbrae/OneDrive/Bureau/CEU/DA2/DA2_Assignement/Data"
write_csv(df, paste0(my_path,'/raw/Covid_case_raw.csv'))
# I have pushed it to Github, we will use that!

# Note this is only the raw files! I am cleaning them in a separate file and save the results to the clean folder!


