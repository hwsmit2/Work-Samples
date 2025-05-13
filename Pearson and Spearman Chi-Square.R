library(haven)
library(tidyverse)
library(ggpubr)

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

#########################################
# DEPRESSION
#########################################

#Overall Pearson
depression_emotionspt <- cor.test(depression1$depression, depression1$emotionspt, 
                                  method = "pearson")
depression_emotionspt$estimate

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
mhlth_emotionspt$p.value

mhlth_isolation <- cor.test(mhlth1$mhlth, mhlth1$isolation, 
                            method = "spearman")
mhlth_isolation$estimate
mhlth_isolation$p.value

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

write.csv(depression_output, "Depression County Estimates.csv")
write.csv(mhlth_output, "Mental Health County Estimates.csv")



#############################
#Scatter plots
#############################
#Scatterplots
###############
(depression_emotionspt_plot<-ggplot(depression1, aes(emotionspt,depression))+
  geom_point(shape=23,aes(fill=mhlth))+
  geom_smooth(method=lm)+
  labs(title="Depression vs emotionspt by County")+
  theme_classic())

(depression_foodinsecu_plot<-ggplot(depression1, aes(foodinsecu,depression))+
  geom_point(shape=23,aes(fill=mhlth))+
  geom_smooth(method=lm)+
  labs(title="Depression vs foodinsecu by County")+
  theme_classic())

(depression_foodstamp_plot<-ggplot(depression1, aes(foodstamp,depression))+
  geom_point(shape=23,aes(fill=mhlth))+
  geom_smooth(method=lm)+
  labs(title="Depression vs foodstamp by County")+
  theme_classic())

(depression_housinsecu_plot<-ggplot(depression1, aes(housinsecu,depression))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Depression vs housinsecu by County")+
    theme_classic())

(depression_isolation_plot<-ggplot(depression1, aes(depression,isolation))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Depression vs isolation by County")+
    theme_classic())

(depression_lacktrpt_plot<-ggplot(depression1, aes(depression,lacktrpt))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Depression vs lacktrpt by County")+
    theme_classic())

(depression_shututility_plot<-ggplot(depression1, aes(depression,shututility))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Depression vs shututility by County")+
    theme_classic())

library(ggpubr)
(depression_county_scatter <- ggarrange(depression_emotionspt_plot,
    depression_foodinsecu_plot , depression_foodstamp_plot ,
    depression_housinsecu_plot,depression_isolation_plot,
    depression_lacktrpt_plot,depression_shututility_plot, nrows=2, ncols = 3)) #Depression
##############################################
#install.packages("ggpmisc")
library(ggpmisc)
library(viridis)


(mhlth_emotionspt_plot<-ggplot(mhlth1, aes(emotionspt,mhlth))+
   geom_point(shape=23,aes(fill=mhlth))+
   geom_smooth(method=lm)+
   labs(title="Freq. Mental Distress vs Lack of Emotional\nSupport by County, 2022")+
   ylab("FMD (%)")+
    xlab("Lack of Emotional Support (%)")+
    stat_poly_eq() +
   theme_classic()+
  scale_fill_viridis(option = "B", direction = -1, name = "Prevalence\nFMD (%)"))

(mhlth_foodinsecu_plot<-ggplot(mhlth1, aes(foodinsecu,mhlth))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Freq. Mental Distress vs Food Insecurity\nby County, 2022")+
    ylab("FMD (%)")+
    xlab("Food Insecurity (%)")+
    stat_poly_eq() +
    theme_classic()+
  scale_fill_viridis(option = "B", direction = -1, name = "Prevalence\nFMD (%)"))


(mhlth_foodstamp_plot<-ggplot(mhlth1, aes(foodstamp,mhlth))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Freq. Mental Distress vs Receipt of Food\nStamps by County, 2022")+
    ylab("FMD (%)")+
    xlab("Food Stamps (%)")+
    stat_poly_eq() +
    theme_classic()+
  scale_fill_viridis(option = "B", direction = -1, name = "Prevalence\nFMD (%)"))


(mhlth_housinsecu_plot<-ggplot(mhlth1, aes(housinsecu,mhlth))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Freq. Mental Distress vs Housing Insecurity\nby County, 2022")+
    ylab("FMD (%)")+
    xlab("Housing Insecurity (%)")+
    stat_poly_eq() +
    theme_classic()+
  scale_fill_viridis(option = "B", direction = -1, name = "Prevalence\nFMD (%)"))


(mhlth_isolation_plot<-ggplot(mhlth1, aes(isolation,mhlth))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Freq. Mental Distress vs Social Isolation\nby County, 2022")+
    ylab("FMD (%)")+
    xlab("Social Isolation (%)")+
    stat_poly_eq() +
    theme_classic()+
  scale_fill_viridis(option = "B", direction = -1, name = "Prevalence\nFMD (%)"))


(mhlth_lacktrpt_plot<-ggplot(mhlth1, aes(lacktrpt,mhlth))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Freq. Mental Distress vs Lack of\nTransportation by County, 2022")+
    ylab("FMD (%)")+
    xlab("Lack of Transportation (%)")+
    stat_poly_eq() +
    theme_classic()+
  scale_fill_viridis(option = "B", direction = -1, name = "Prevalence\nFMD (%)"))


(mhlth_shututility_plot<-ggplot(mhlth1, aes(shututility,mhlth))+
    geom_point(shape=23,aes(fill=mhlth))+
    geom_smooth(method=lm)+
    labs(title="Freq. Mental Distress vs Utility Services Threat\nby County, 2022" )+
    ylab("FMD (%)")+
    xlab("Utility Services Threat (%)")+
    stat_poly_eq() +
    theme_classic()+
  scale_fill_viridis(option = "B", direction = -1, name = "Prevalence\nFMD (%)"))

(mhlth_county_scatter <- ggarrange(mhlth_emotionspt_plot,mhlth_foodinsecu_plot , mhlth_foodstamp_plot, mhlth_housinsecu_plot,mhlth_isolation_plot, mhlth_lacktrpt_plot,mhlth_shututility_plot, nrows=2, ncols = 3))



