library(haven)
library(tidyverse)
library(usmap)

getwd()

#Main Variables
depression <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_depression.sas7bdat")

mhlth <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_mhlth.sas7bdat")

#Social Needs
housinsecu <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_housinsecu.sas7bdat")
shututility <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_shututility.sas7bdat")
isolation <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_isolation.sas7bdat")
foodstamp <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_foodstamp.sas7bdat")
emotionspt <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_emotionspt.sas7bdat")
lacktrpt <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_lacktrpt.sas7bdat")
foodinsecu <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_county_crude/final_estimates/sae2022_county_foodinsecu.sas7bdat")


#join
depression <- left_join(depression,emotionspt)
depression <- left_join(depression,housinsecu)
depression <- left_join(depression,shututility)
depression <- left_join(depression,isolation)
depression <- left_join(depression,foodstamp)
depression <- left_join(depression,lacktrpt)
depression1 <- left_join(depression,foodinsecu)


#join
mhlth <- left_join(mhlth,emotionspt)
mhlth <- left_join(mhlth,housinsecu)
mhlth <- left_join(mhlth,shututility)
mhlth <- left_join(mhlth,isolation)
mhlth <- left_join(mhlth,foodstamp)
mhlth <- left_join(mhlth,lacktrpt)
mhlth1 <- left_join(mhlth,foodinsecu)


state_labs <- fips_info(depression1$COUNTY2022)
state_labs$COUNTY2022 <- state_labs$fips


depression1 <- left_join(depression1,state_labs)
mhlth1 <- left_join(mhlth1,state_labs)

########################################
########################################
########################################

depression_pearson_emotionspt <- depression1 %>%
  group_by(abbr)%>%
  summarise(pearson_emotionspt = round(cor(depression,emotionspt, 
           method = "pearson"),3))

depression_pearson_housinsecu <- depression1 %>%
  group_by(abbr)%>%
  summarise(pearson_housinsec = round(cor(depression,housinsecu, 
                          method = "pearson"),3))

depression_pearson_shututility <- depression1 %>%
  group_by(abbr)%>%
  summarise(pearson_shututility = round(cor(depression,shututility, 
                          method = "pearson"),3))

depression_pearson_isolation <- depression1 %>%
  group_by(abbr)%>%
  summarise(pearson_isolation = round(cor(depression,isolation, 
                          method = "pearson"),3))

depression_pearson_foodstamp <- depression1 %>%
  group_by(abbr)%>%
  summarise(pearson_foodstamp = round(cor(depression,foodstamp, 
                          method = "pearson"),3))

depression_pearson_lacktrpt <- depression1 %>%
  group_by(abbr)%>%
  summarise(pearson_lacktrpt = round(cor(depression,lacktrpt, 
                          method = "pearson"),3))

depression_pearson_foodinsecu <- depression1 %>%
  group_by(abbr)%>%
  summarise(pearson_foodinsecu = round(cor(depression,foodinsecu, 
                          method = "pearson"),3))

depression_pearson_states <- purrr::reduce(list(depression_pearson_emotionspt,depression_pearson_housinsecu,depression_pearson_shututility,
              depression_pearson_isolation,depression_pearson_foodstamp,depression_pearson_lacktrpt,depression_pearson_foodinsecu), dplyr::left_join, by='abbr')

write.csv(depression_pearson_states, "Depression Pearson Correlations Within States.csv")
#################################################################################################


depression_spearman_emotionspt <- depression1 %>%
  group_by(abbr)%>%
  summarise(spearman_emotionspt = round(cor(depression,emotionspt, 
                                           method = "spearman"),3))

depression_spearman_housinsecu <- depression1 %>%
  group_by(abbr)%>%
  summarise(spearman_housinsec = round(cor(depression,housinsecu, 
                                          method = "spearman"),3))

depression_spearman_shututility <- depression1 %>%
  group_by(abbr)%>%
  summarise(spearman_shututility = round(cor(depression,shututility, 
                                            method = "spearman"),3))

depression_spearman_isolation <- depression1 %>%
  group_by(abbr)%>%
  summarise(spearman_isolation = round(cor(depression,isolation, 
                                          method = "spearman"),3))

depression_spearman_foodstamp <- depression1 %>%
  group_by(abbr)%>%
  summarise(spearman_foodstamp = round(cor(depression,foodstamp, 
                                          method = "spearman"),3))

depression_spearman_lacktrpt <- depression1 %>%
  group_by(abbr)%>%
  summarise(spearman_lacktrpt = round(cor(depression,lacktrpt, 
                                         method = "spearman"),3))

depression_spearman_foodinsecu <- depression1 %>%
  group_by(abbr)%>%
  summarise(spearman_foodinsecu = round(cor(depression,foodinsecu, 
                                           method = "spearman"),3))

depression_spearman_states <- purrr::reduce(list(depression_spearman_emotionspt,depression_spearman_housinsecu,depression_spearman_shututility,
                                                depression_spearman_isolation,depression_spearman_foodstamp,depression_spearman_lacktrpt,depression_spearman_foodinsecu), dplyr::left_join, by='abbr')

write.csv(depression_spearman_states, "Depression Spearman Correlations Within States.csv")



###############################################################
###############################################################
###############################################################


mhlth_pearson_emotionspt <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(pearson_emotionspt = round(cor(mhlth,emotionspt, 
                                           method = "pearson"),3))

mhlth_pearson_housinsecu <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(pearson_housinsec = round(cor(mhlth,housinsecu, 
                                          method = "pearson"),3))

mhlth_pearson_shututility <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(pearson_shututility = round(cor(mhlth,shututility, 
                                            method = "pearson"),3))

mhlth_pearson_isolation <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(pearson_isolation = round(cor(mhlth,isolation, 
                                          method = "pearson"),3))

mhlth_pearson_foodstamp <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(pearson_foodstamp = round(cor(mhlth,foodstamp, 
                                          method = "pearson"),3))

mhlth_pearson_lacktrpt <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(pearson_lacktrpt = round(cor(mhlth,lacktrpt, 
                                         method = "pearson"),3))

mhlth_pearson_foodinsecu <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(pearson_foodinsecu = round(cor(mhlth,foodinsecu, 
                                           method = "pearson"),3))

mhlth_pearson_states <- purrr::reduce(list(mhlth_pearson_emotionspt,mhlth_pearson_housinsecu,mhlth_pearson_shututility,
                                                mhlth_pearson_isolation,mhlth_pearson_foodstamp,mhlth_pearson_lacktrpt,mhlth_pearson_foodinsecu), dplyr::left_join, by='abbr')

write.csv(mhlth_pearson_states, "Mental Health Pearson Correlations Within States.csv")
#################################################################################################


mhlth_spearman_emotionspt <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(spearman_emotionspt = round(cor(mhlth,emotionspt, 
                                            method = "spearman"),3))

mhlth_spearman_housinsecu <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(spearman_housinsec = round(cor(mhlth,housinsecu, 
                                           method = "spearman"),3))

mhlth_spearman_shututility <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(spearman_shututility = round(cor(mhlth,shututility, 
                                             method = "spearman"),3))

mhlth_spearman_isolation <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(spearman_isolation = round(cor(mhlth,isolation, 
                                           method = "spearman"),3))

mhlth_spearman_foodstamp <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(spearman_foodstamp = round(cor(mhlth,foodstamp, 
                                           method = "spearman"),3))

mhlth_spearman_lacktrpt <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(spearman_lacktrpt = round(cor(mhlth,lacktrpt, 
                                          method = "spearman"),3))

mhlth_spearman_foodinsecu <- mhlth1 %>%
  group_by(abbr)%>%
  summarise(spearman_foodinsecu = round(cor(mhlth,foodinsecu, 
                                            method = "spearman"),3))

mhlth_spearman_states <- purrr::reduce(list(mhlth_spearman_emotionspt,mhlth_spearman_housinsecu,mhlth_spearman_shututility,
                                                 mhlth_spearman_isolation,mhlth_spearman_foodstamp,mhlth_spearman_lacktrpt,mhlth_spearman_foodinsecu), dplyr::left_join, by='abbr')

write.csv(mhlth_spearman_states, "Mental Health Spearman Correlations Within States.csv")

