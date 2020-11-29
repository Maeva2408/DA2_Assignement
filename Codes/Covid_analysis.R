#######################
## Assignement for   ##
## DA2 and COD1      ##
## Analysis of       ##
##  number of        ##
## registered death  ##
## and number of     ##
## registered case $ ##
## due to covid on   ##
##  the 25/10/2020   ##
##                   ##
## Analysis of       ##
##   the data        ##
##                   ##
#######################

### Task : Analyse the pattern of association between registered covid-19 
### cases and registered number of death du to covid-19 on the 26 october 2020.

# Clear memory
rm(list=ls())

# Packages to use
library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)
library(tidyverse)
library(geosphere)
library(moments)
library(dplyr)
library(knitr)
library(pander)
install.packages("jtools")
install.packages("huxtable")
# First, I will import ly clean data from github
my_url <- "https://raw.githubusercontent.com/Maeva2408/DA2_Assignement/main/Data/Clean/covid_pop_10_26_2020_clean.csv"
df_covid <- read_csv( my_url )


######

### Selection/drop observation and scaling
# I want to use log transformation so I decided to drop the observation with 0 death, it does not make sens to look for a 
# for a pattern if there are no deaths. I also decided to put the population in million and covid case information in
# 1000 persons

View( df_covid %>% filter( death==0) ) 
# Drop if death is 0
df_covid <- df_covid %>% filter( !(death==0) ) 
#New scale
df_covid <- df_covid %>% transmute( country = country,
                       confirmed = confirmed/1000,
                       deaths    =  death/1000,
                       recovered = recovered/1000,
                       active    = active/1000,
                         population = population/1000000)

# I will do a quick check on x and y variable HISTOGRAMS
df_covid %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= confirmed))+
  geom_histogram(fill= "orangered4", col= "salmon")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Total of registered case (26/10/2020)",y = "count of registered case  (26/10/2020)")

df_covid %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= deaths))+
  geom_histogram(fill= "orangered4", col= "salmon")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Total of registered death (26/10/2020)",y = "Count of registered case  (26/10/2020)")


df_covid_confirmed <- summarise(df_covid,
                                    variable = "Case confirmed",
                                    mean = mean(x = confirmed),
                                    median = median(x = confirmed),
                                    min= min(x = confirmed),
                                    max = max(x = confirmed),
                                    sd = sd(x = confirmed),
                                    skew = skewness(x = confirmed))
df_covid_deaths <- summarise(df_covid,
                                variable = "case of death",
                                mean = mean(x = deaths),
                                median = median(x = deaths),
                                min= min(x = deaths),
                                max = max(x = deaths),
                                sd = sd(x = deaths),
                                skew = skewness(x = deaths))

table_summary <- add_row(df_covid_confirmed,df_covid_deaths)
kable(table_summary)

######
#### Investigation of the transformation of my variable
## I will try the fourth models and choose the best fit.
#
#     my model : number of registered death = alpha + beta * number of registered case
#
# Test of log-transformation :
#
# 1) total of registered case confirmed - total of registered deaths: level-level model without scaling by country
ggplot( df_covid , aes(x = confirmed, y = deaths)) +
  geom_point(color = 'salmon') +
  geom_smooth(method="loess", color = 'orangered4')+
  theme_bw()+
  labs(x = "Total of registered death (26/10/2020)",y = "Total of registered case  (26/10/2020)")

# 2) log(total of registered case confirmed) - total of registered deaths: log -level by country
ggplot( df_covid , aes(x = confirmed, y = deaths)) +
  geom_point(color = 'salmon') +
  geom_smooth(method="loess", color = 'orangered4')+
  theme_bw()+
  labs(x = "Total of registered death (26/10/2020,ln scale )",y = "Total of registered case  (26/10/2020)") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,1000,10000) )

# 3) total of registered case confirmed - log (total of registered deaths) by country
# level - log
ggplot( df_covid , aes(x = confirmed, y = deaths)) +
  geom_point(color = 'salmon') +
  geom_smooth(method="loess",color = 'orangered4' )+
  theme_bw()+
  labs(x = "Total of registered case confirmed (26/10/2020,ln scale )",y = "Total of registered death (26/10/2020)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )


# 4) log (total of registered case confirmed) - log (total of registered deaths) by country
# log - log 
ggplot( df_covid , aes(x = confirmed, y = deaths ))  +
  geom_point(color = 'salmon') +
  geom_smooth(method="loess",color = 'orangered4')+
  theme_bw()+
  labs(x = "Total of registered death (26/10/2020, ln scale )",y = "Total of registered case (26/10/2020, ln scale)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans(),breaks = c(1,2,5,10,20,50,100,200,500,1000,10000 ))

####
# Conclusions:
#   1) taking log of gdptot is needed, but still non-linear pattern in data/need to use 'approximation' interpretation
#     - feasible to check and we do it due to learn how to do it, 
#           but in practice I would skip this -> over-complicates analysis
#   2) using only gdppc is possible, but need to model the non-linearity in data
#       - Substantive: Level changes is harder to interpret and our aim is not to get $ based comparison
#       - Statistical: log transformation is way better approximation make simplification!
#   3) taking log of gdppc is making the association close to linear!
#   4) taking log for life-expectancy does not matter -> use levels!
#       - Substantive: it does not give better interpretation
#       - Statistical: you can compare models with the same y, no better fit
#       - Remember: simplest the better!

### Conclusions :
# I will use the fourth model : use Log transformation for both of my variable :
## substantive reason : both of the variable are affected in multiplicatives ways, it's increased by a certain percentage
## statistical reason : the distribution of the varibales are skewed with a long right tail
## When I compare all the transformation, it make sens to choose the log-log


#### Take Log of the number of case 

df_covid <- df_covid %>% mutate( ln_confirmed = log( confirmed), ln_deaths = log(deaths) )


######
#### Models:

##    Simple linear regression
#     reg1: ln_death = alpha + beta * ln_confirmed

##    Quadratic function
#     reg2: ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2

##    Piecewise linear spline regression
#     reg3: ln_deaths = alpha + beta_1 * ln_confirmed * 1(confirmed < 1) + 
#     beta_2 * ln_confirmed * 1(1 <= confirmed < 200) + 
#     beta_3 * ln_confirmed* 1(confirmed >= 200)

##    Weighted-ols:
#     reg4: ln_death = alpha + beta * ln_confirmed, weights: population

 
#
### I add powers of the variable(s) to the dataframe:
df_covid <- df_covid %>% mutate( ln_confirmed_sq = ln_confirmed^2 )
#
### The regressions
#
# Building the regression
reg_covid <- lm( ln_deaths ~ ln_confirmed , data = df_covid )
reg_covid
summary( reg_covid )


## Simple Linear regression
reg1 <- lm_robust( ln_deaths ~ ln_confirmed , data = df_covid , se_type = "HC2" )
reg1

## Summary statistics
summary( reg1 )
## Vizualition
ggplot( data = df_covid, aes( x = ln_confirmed, y = ln_deaths ) ) + 
  geom_point( color='salmon') +
  theme_bw()+
  geom_smooth( method = lm , color = 'orangered4' )+
  labs(x = "Total of registered death (26/10/2020, ln scale )",y = "Total of registered case (26/10/2020, ln scale)") 

## Quadratic (linear) regression 
reg2 <- lm_robust( ln_deaths ~ ln_confirmed + ln_confirmed_sq , data = df_covid )
summary( reg2 )
ggplot( data = df_covid, aes( x = ln_confirmed, y = ln_deaths ) ) + 
  geom_point( color='salmon') +
  theme_bw()+
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'orangered4' )+
  labs(x = "Total  of registered death (26/10/2020, ln scale )",y = "Total  of registered case (26/10/2020, ln scale)") 


# Piecewise linear spline regression
# cut off and log cut off
cutoff <- c(1,200)
cutoff_ln<- log( cutoff )

reg3 <- lm_robust(ln_deaths ~ lspline( ln_confirmed , cutoff_ln ), data = df_covid )
summary( reg3 )
ggplot( data = df_covid, aes( x = ln_confirmed, y = ln_deaths ) ) + 
  geom_point( color='salmon') +
  theme_bw()+
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'orangered4' )+
  labs(x = "Total  of registered death (26/10/2020, ln scale )",y = "Total  of registered case (26/10/2020, ln scale)") 


# Weighted linear regression : weight with population
reg4 <- lm_robust(ln_deaths ~ ln_confirmed, data = df_covid , weights = population)
summary( reg4 )

ggplot(data = df_covid, aes(x = ln_confirmed, y = ln_deaths)) +
  geom_point(data = df_covid, aes(size=population),  color = 'salmon', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='orangered4')+
  theme_bw()+
  scale_size(range = c(1, 15)) +
  labs(x = "Total  of registered death (26/10/2020, ln scale )",y = "Total  of registered confirmed (26/10/2020, ln scale)") 


#####
# Creating model summary with texreg
df_covid_stat <- export_summs(reg1,reg2,reg3,reg4,
                              model.names = "Linear","Quadratic", "PLS",
                              "weighted linear")
as_hux(df_covid_stat)
data_out <- "C:/Users/mbrae/OneDrive/Bureau/CEU/DA2/DA2_Assignement/out/"
htmlreg( list(reg1 , reg2 , reg3, reg4),
         type = 'html',
         custom.model.names = c("Linear","Quadratic", "PLS",
                                 "weighted linear"),
         caption = "Modelling Covid19 cases vs deaths by country on the 26/10/2020",
         file = paste0( data_out ,'model_comparison_covid.html'), include.ci = FALSE)

######
# Based on model comparison my chosen model is reg4 Weighted Linear
#   Substantive: - log-log interpretation works properly for countries
#                - magnitude of coefficients are meaningful
#   Statistical: - simple model, easy to interpret
#                - R2 is the highest


#################################
## Testing hypothesis
#

##
# 1) Coefficient is equal to 0:
# Implemented by default...
summary( reg4 )

## my t value is above 2 so my CI lower and my CI upper don't cross 0
## So my interval of confidence will be 95%


# 2) Coefficient is equal to your favorite value
library(car)

# Let test: H0: ln_confirmed = 0, HA: ln_confirmed neq 0
## HO = there is no pattern association between the confirmed case and the death
## HA = there is a pattern of association between the confirmed case and death
linearHypothesis( reg4 , "ln_confirmed = 0")

## My p value is < 0,05 so I can reject my HO and therefore there is a pattern
## of association between deaths and confirmed cases on the 26/10/2020


#######################
# Residual analysis.

# I got the predicted y values from the model
df_covid$reg4_y_pred <- reg4$fitted.values

# I Calculate the errors of the model
df_covid$reg4_res <- df_covid$ln_deaths - df_covid$reg4_y_pred 

# Find countries with largest negative errors
df_covid %>% top_n( -5 , reg4_res ) %>% 
  select( country , ln_deaths , reg4_y_pred , reg4_res )
?as.data.frame
## I can see for example, for Burundi, the ln_deaths is equal -6,91, but the
## predicted value for is -3.95 so the predicted value is off by -2,96 so the
## model overestimated the deaths


# Find countries with largest positive errors
df_covid %>% top_n( 5 , reg4_res ) %>% 
  select( country , ln_deaths , reg4_y_pred , reg4_res )

## I can see, for Ecuador, for example, that the ln_deaths is equal at 2,53 
## the predicted value is 1,44 so the predicted value is off by 1,09, so the
## model underestimated the deaths.



