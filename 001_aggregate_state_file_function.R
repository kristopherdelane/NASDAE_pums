options(scipen=999)

aggregate_output <-function(df) { df %>%
  group_by(ST) %>%
  filter(AGEP > 17)%>%
  filter(AGEP < 65) %>%
  filter(SCHL < 16) %>%
  summarize(less_than_high_school_18_64 = sum(PWGTP)) %>%
  left_join(df %>%
              group_by(ST) %>%
              filter(AGEP > 17)%>%
              filter(AGEP < 65) %>%
              filter(ENG != 1) %>%
              summarize(does_not_speak_english_very_well_18_64 = sum(PWGTP)))%>%
  left_join(df %>%
              group_by(ST) %>%
              filter(AGEP > 17)%>%
              filter(AGEP < 65) %>%
              filter(ENG != 1) %>%
              filter(SCHL < 16) %>%
              summarize(less_than_high_school_does_not_speak_english_very_well_18_64 = sum(PWGTP)))%>%
  left_join(df %>%
              group_by(ST) %>%
              filter(AGEP > 17)%>%
              filter(AGEP < 65) %>%
              filter(ESR == 3) %>%
              filter(SCHL < 16) %>%
              summarize(less_than_high_school_unemployed_18_64 = sum(PWGTP)))%>%
  left_join(df %>%
              group_by(ST) %>%
              filter(AGEP > 17)%>%
              filter(AGEP < 65) %>%
              filter(ESR == 6) %>%
              filter(SCHL < 16) %>%
              summarize(less_than_high_school_not_in_labor_force_18_64 = sum(PWGTP)))%>%
  left_join(df %>%
              group_by(ST) %>%
              filter(AGEP > 17)%>%
              filter(AGEP < 65) %>%
              filter(FPAP == 1) %>%
              filter(SCHL < 16) %>%
              summarize(less_than_high_school_on_public_assistance_18_64 = sum(PWGTP)))%>%
  left_join(df %>%
              group_by(ST) %>%
              filter(AGEP > 15)%>%
              filter(AGEP < 24) %>%
              filter(ESR == 3) %>%
              filter(SCHL < 16) %>%
              filter(SCH == 1) %>%
              summarize(less_than_high_school_not_enrolled_unemployed_16_24 = sum(PWGTP)))%>%
  left_join(df %>%
              left_join(df %>%
                          select(SERIALNO, RELP, SCHL) %>%
                          filter(RELP == 00) %>%
                          filter(SCHL < 16) %>%
                          mutate(hoh_less_hs = 1)  %>%  
                          select(SERIALNO, hoh_less_hs)) %>%
              filter(AGEP < 18) %>%
              group_by(ST, hoh_less_hs) %>%
              summarize(count = sum(PWGTP))%>% ungroup() %>%
              mutate(hoh_less_hs = ifelse(is.na(hoh_less_hs), "more_than_high_school_diploma", "less_than_high_school_diploma")) %>%
              pivot_wider(id_col = ST ,names_prefix = "head_of_household_", names_from = hoh_less_hs, values_from = count)) 
              #mutate(pct_children_living_with_head_of_household_with_less_than_high_school_diploma = head_of_household_less_than_high_school_diploma/(head_of_household_more_than_high_school_diploma+head_of_household_less_than_high_school_diploma))) %>%
}

