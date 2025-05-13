library(haven)
library(tidyverse)


getwd()

#Main Variables
depression <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_depression.sas7bdat")

mhlth <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_mhlth.sas7bdat")

#Social Needs
housinsecu <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_housinsecu.sas7bdat")
shututility <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_shututility.sas7bdat")
isolation <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_isolation.sas7bdat")
foodstamp <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_foodstamp.sas7bdat")
emotionspt <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_emotionspt.sas7bdat")
lacktrpt <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_lacktrpt.sas7bdat")
foodinsecu <- read_sas("//cdc.gov/project/NCCD_DPH_GIS4/PLACES/2022/sae_state_crude/final_estimates/sae2022_state_foodinsecu.sas7bdat")


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

#########################################
# DEPRESSION
#########################################

#Overall Pearson
depression_emotionspt <- cor.test(depression1$depression, depression1$emotionspt, 
                                  method = "pearson")
depression_emotionspt$estimate
depression_emotionspt$p.value


depression_isolation <- cor.test(depression1$depression, depression1$isolation, 
                                 method = "pearson")
depression_isolation$estimate

depression_foodinsecu <- cor.test(depression1$depression, depression1$foodinsecu, 
                                  method = "pearson")
depression_foodinsecu$estimate

depression_foodstamp <- cor.test(depression1$depression, depression1$foodstamp, 
                                 method = "pearson")
depression_foodstamp$estimate

depression_housinsecu <- cor.test(depression1$depression, depression1$housinsecu, 
                                  method = "pearson")
depression_housinsecu$estimate

depression_lacktrpt <- cor.test(depression1$depression, depression1$lacktrpt, 
                                method = "pearson")
depression_lacktrpt$estimate

depression_shututility <- cor.test(depression1$depression, depression1$shututility, 
                                   method = "pearson")
depression_shututility$estimate

labels <- rbind("emotionspt","isolation","foodinsecu","foodstamp","housinsecu","lacktrpt","shututility")
estimates <- rbind(depression_emotionspt$estimate,depression_isolation$estimate,depression_foodinsecu$estimate,depression_foodstamp$estimate,
                   depression_housinsecu$estimate,depression_lacktrpt$estimate,depression_shututility$estimate
)
p_values <- rbind(depression_emotionspt$p.value,depression_isolation$p.value,depression_foodinsecu$p.value,depression_foodstamp$p.value,
                   depression_housinsecu$p.value,depression_lacktrpt$p.value,depression_shututility$p.value
)

pearson_depression <- as.data.frame(cbind(labels,estimates,p_values))
pearson_depression <- pearson_depression %>%
  rename("measure"="V1",
         "pearson_estimate"= "cor",
         "p_value1" = "V3")%>%
  mutate(
    pearson_estimate = round(as.numeric(pearson_estimate),3),
    p_value1 = round(as.numeric(p_value1),3)
  )
pearson_depression
#write.csv(pearson_depression, "Depression Pearson Estimates County.csv")

###############################################################################

#Overall Spearman
depression_emotionspt <- cor.test(depression1$depression, depression1$emotionspt, 
                                  method = "spearman")
depression_emotionspt$estimate

depression_isolation <- cor.test(depression1$depression, depression1$isolation, 
                                 method = "spearman")
depression_isolation$estimate

depression_foodinsecu <- cor.test(depression1$depression, depression1$foodinsecu, 
                                  method = "spearman")
depression_foodinsecu$estimate

depression_foodstamp <- cor.test(depression1$depression, depression1$foodstamp, 
                                 method = "spearman")
depression_foodstamp$estimate

depression_housinsecu <- cor.test(depression1$depression, depression1$housinsecu, 
                                  method = "spearman")
depression_housinsecu$estimate

depression_lacktrpt <- cor.test(depression1$depression, depression1$lacktrpt, 
                                method = "spearman")
depression_lacktrpt$estimate

depression_shututility <- cor.test(depression1$depression, depression1$shututility, 
                                   method = "spearman")
depression_shututility$estimate


#labels <- rbind("emotionspt","isolation","foodinsecu","foodstamp","housinsecu","lacktrpt","shututility")
estimates <- rbind(depression_emotionspt$estimate,depression_isolation$estimate,depression_foodinsecu$estimate,depression_foodstamp$estimate,
                   depression_housinsecu$estimate,depression_lacktrpt$estimate,depression_shututility$estimate
)
p_values <- rbind(depression_emotionspt$p.value,depression_isolation$p.value,depression_foodinsecu$p.value,depression_foodstamp$p.value,
                  depression_housinsecu$p.value,depression_lacktrpt$p.value,depression_shututility$p.value
)

spearman_depression <- as.data.frame(cbind(labels,estimates,p_values))
spearman_depression <- spearman_depression %>%
  rename("measure"="V1",
         "spearman_estimate"= "rho",
         "p_value2"="V3")%>%
  mutate(
    spearman_estimate = round(as.numeric(spearman_estimate),3),
    p_value2 = round(as.numeric(p_value2),3)
  )

spearman_depression
#write.csv(spearman_depression, "Depression Spearman Estimates County.csv")



############################################################
############################################################
############################################################


################################
#MHLTH
################################

#Overall Pearson
mhlth_emotionspt <- cor.test(mhlth1$mhlth, mhlth1$emotionspt, 
                             method = "pearson")
mhlth_emotionspt$estimate

mhlth_isolation <- cor.test(mhlth1$mhlth, mhlth1$isolation, 
                            method = "pearson")
mhlth_isolation$estimate

mhlth_foodinsecu <- cor.test(mhlth1$mhlth, mhlth1$foodinsecu, 
                             method = "pearson")
mhlth_foodinsecu$estimate

mhlth_foodstamp <- cor.test(mhlth1$mhlth, mhlth1$foodstamp, 
                            method = "pearson")
mhlth_foodstamp$estimate

mhlth_housinsecu <- cor.test(mhlth1$mhlth, mhlth1$housinsecu, 
                             method = "pearson")
mhlth_housinsecu$estimate

mhlth_lacktrpt <- cor.test(mhlth1$mhlth, mhlth1$lacktrpt, 
                           method = "pearson")
mhlth_lacktrpt$estimate

mhlth_shututility <- cor.test(mhlth1$mhlth, mhlth1$shututility, 
                              method = "pearson")
mhlth_shututility$estimate

labels <- rbind("emotionspt","isolation","foodinsecu","foodstamp","housinsecu","lacktrpt","shututility")
estimates <- rbind(mhlth_emotionspt$estimate,mhlth_isolation$estimate,mhlth_foodinsecu$estimate,mhlth_foodstamp$estimate,
                   mhlth_housinsecu$estimate,mhlth_lacktrpt$estimate,mhlth_shututility$estimate
)
p_values <- rbind(mhlth_emotionspt$p.value,mhlth_isolation$p.value,mhlth_foodinsecu$p.value,mhlth_foodstamp$p.value,
                  mhlth_housinsecu$p.value,mhlth_lacktrpt$p.value,mhlth_shututility$p.value
)

pearson_mhlth <- as.data.frame(cbind(labels,estimates,p_values))
pearson_mhlth <- pearson_mhlth %>%
  rename("measure"="V1",
         "pearson_estimate"= "cor",
         "p_value1"="V3")%>%
  mutate(
    pearson_estimate = round(as.numeric(pearson_estimate),3),
    p_value1 = round(as.numeric(p_value1),3)
  )
pearson_mhlth
#write.csv(pearson_mhlth, "Mental Health Pearson Estimates County.csv")

#Overall Spearman
mhlth_emotionspt <- cor.test(mhlth1$mhlth, mhlth1$emotionspt, 
                             method = "spearman")
mhlth_emotionspt$estimate

mhlth_isolation <- cor.test(mhlth1$mhlth, mhlth1$isolation, 
                            method = "spearman")
mhlth_isolation$estimate

mhlth_foodinsecu <- cor.test(mhlth1$mhlth, mhlth1$foodinsecu, 
                             method = "spearman")
mhlth_foodinsecu$estimate

mhlth_foodstamp <- cor.test(mhlth1$mhlth, mhlth1$foodstamp, 
                            method = "spearman")
mhlth_foodstamp$estimate

mhlth_housinsecu <- cor.test(mhlth1$mhlth, mhlth1$housinsecu, 
                             method = "spearman")
mhlth_housinsecu$estimate

mhlth_lacktrpt <- cor.test(mhlth1$mhlth, mhlth1$lacktrpt, 
                           method = "spearman")
mhlth_lacktrpt$estimate

mhlth_shututility <- cor.test(mhlth1$mhlth, mhlth1$shututility, 
                              method = "spearman")
mhlth_shututility$estimate


#labels <- rbind("emotionspt","isolation","foodinsecu","foodstamp","housinsecu","lacktrpt","shututility")
estimates <- rbind(mhlth_emotionspt$estimate,mhlth_isolation$estimate,mhlth_foodinsecu$estimate,mhlth_foodstamp$estimate,
                   mhlth_housinsecu$estimate,mhlth_lacktrpt$estimate,mhlth_shututility$estimate
)
p_values <- rbind(mhlth_emotionspt$p.value,mhlth_isolation$p.value,mhlth_foodinsecu$p.value,mhlth_foodstamp$p.value,
                  mhlth_housinsecu$p.value,mhlth_lacktrpt$p.value,mhlth_shututility$p.value
)

spearman_mhlth <- as.data.frame(cbind(labels,estimates,p_values))
spearman_mhlth <- spearman_mhlth %>%
  rename("measure"="V1",
         "spearman_estimate"= "rho",
         "p_value2"="V3")%>%
  mutate(
    spearman_estimate = round(as.numeric(spearman_estimate),3),
    p_value2 = round(as.numeric(p_value2),3)
    
  )
spearman_mhlth
#write.csv(spearman_mhlth, "Mental Health Spearman Estimates County.csv")


#Join

depression_output <- left_join(pearson_depression, spearman_depression)
depression_output

mhlth_output <- left_join(pearson_mhlth, spearman_mhlth)
mhlth_output

write.csv(depression_output, "Depression State Estimates.csv")
write.csv(mhlth_output, "Mental Health State Estimates.csv")
