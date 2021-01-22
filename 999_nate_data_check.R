library(tidyverse)
library(tidycensus)



# Read in PUMS file for Kentucky ------------------------------------------
x <- Sys.time()
ky_pums <- get_pums(
  variables = c("PUMA", "AGEP", "SCHL", "ESR", "FPAP", "NOC", "ENG", "SCH", "RELP"),
  state = "KY",
  survey = "acs5",
  recode = TRUE
)


# Create an output of chidren living in a household -----------------------
#' with a Head of Household who doesn't have a High School Diploma
#' and those living in a household 
#' with a Head of Household who does have a High School Diploma
#' then finding the percentage.

ky_pums%>%
  #create a flag for all records where Person 1 (head of household)
  #has or does not have a high school diploma
  left_join(ky_pums %>%
              select(SERIALNO, RELP, SCHL) %>%
              filter(RELP == "00") %>%
              filter(SCHL < 16) %>%
              mutate(hoh_less_hs = 1)  %>%  
              select(SERIALNO, hoh_less_hs)) %>%
  #filter to people under the age of 18
  filter(AGEP < 18) %>%
  group_by(ST_label, hoh_less_hs) %>%
  #get counds based around head of household education
  summarize(count = sum(PWGTP))%>%
  ungroup() %>%
  #clean up for descriptive column names
  mutate(hoh_less_hs = ifelse(is.na(hoh_less_hs), "more_than_high_school_diploma", "less_than_high_school_diploma")) %>%
  pivot_wider(id_col = ST_label ,names_prefix = "head_of_household_", names_from = hoh_less_hs, values_from = count) %>%
  #calculate %
  mutate(pct_children_living_with_head_of_household_with_less_than_high_school_diploma = head_of_household_less_than_high_school_diploma/(head_of_household_more_than_high_school_diploma+head_of_household_less_than_high_school_diploma)) %>%
  #make long for viewing below
  pivot_longer(2:4)

Sys.time()-x

# ky_check <- fread("data/psam_p21.csv")
# 
# ky_check_agg <- ky_check %>% aggregate_output()
# ky_pums_agg <- ky_pums %>% aggregate_output()
# 
# ky_check%>%
#   #create a flag for all records where Person 1 (head of household)
#   #has or does not have a high school diploma
#   left_join(ky_check %>%
#               select(SERIALNO, RELP, SCHL) %>%
#               filter(RELP == 00) %>%
#               filter(SCHL < 16) %>%
#               mutate(hoh_less_hs = 1)  %>%  
#               select(SERIALNO, hoh_less_hs)) %>%
#   #filter to people under the age of 18
#   mutate(hoh_less_hs = ifelse(is.na(hoh_less_hs), "more_than_high_school_diploma", "less_than_high_school_diploma")) %>%
#   filter(AGEP < 18) %>%
#   group_by(ST, hoh_less_hs) %>%
#   #get counds based around head of household education
#   summarize(count = sum(PWGTP))%>%
#   ungroup() %>%
#   #clean up for descriptive column names
#   pivot_wider(id_col = ST ,names_prefix = "head_of_household_", names_from = hoh_less_hs, values_from = count)
#   #calculate %
#   #mutate(pct_children_living_with_head_of_household_with_less_than_high_school_diploma = head_of_household_less_than_high_school_diploma/(head_of_household_more_than_high_school_diploma+head_of_household_less_than_high_school_diploma)) %>%
#   #make long for viewing below
#   #pivot_longer(2:4)


# 
# ky_check %>%
#   group_by(ST) %>%
#   filter(AGEP > 17)%>%
#   filter(AGEP < 65) %>%
#   filter(FPAP == 1) %>%
#   filter(SCHL < 16) %>%
#   summarize(FPAP = sum(PWGTP)) %>%
#   left_join(ky_check %>%
#               group_by(ST) %>%
#               filter(AGEP > 17)%>%
#               filter(AGEP < 65) %>%
#               filter(SCHL < 16) %>%
#               summarize(TOTAL = sum(PWGTP)))
