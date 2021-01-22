# install.packages(c("survey", "srvyr"))
# install.packages(c("tidycensus"))


# Libraries, allways the best place to start ------------------------------

library(tidyverse)
library(tidycensus)
library(janitor)
library(survey)


# What variables do we need? ----------------------------------------------

pums_vars_2018 <- pums_variables %>%
  filter(year == 2018, survey == "acs5")

state <- pums_variables %>%
  filter(year == 2018, survey == "acs5") %>%
  filter(var_code == "ST") %>%
  select(val_min, val_label) 


#' We need: 

#' # of adults without a hs diploma
#'            SCHL	Educational attainment	chr	person	15	15	12th grade - no diploma

#' # of adults that don't speak english well
#'            ENG	Ability to speak English	chr	person	1	1	Very well

#' # of adults without a hs diploma who dont speak english well

#' # of adults wihtout a hs diploma who are unemployed
#'            ESR	Employment status recode	chr	person	3	3	Unemployed
            
#' # of adults without a hs diploma who are not in the labor force
#'            ESR	Employment status recode	chr	person	6	6	Not in labor force

#' # of adults without a hs diploma who are on public assistance
#'            FPAP	Public assistance income allocation flag	chr	person	1	1	Yes	

#' # of youth 16-24 without a hs diploma who are not enrolled in school and are unemployed
#' 	          SCH	School enrollment	chr	person	1	1	No, has not attended in the last 3 months

#' % of children living in a household headed by an adult wihtout a high school diploma
#'            NOC	Number of own children in household (unweighted)	num	housing	1	19	Number of own children in household

