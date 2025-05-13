#Libraries used
library(haven)
library(tidyverse)
library(ggpubr)
library(broom)
library(lme4)
library(performance)
#install.packages("olsrr")
library(corrplot)
library(sjPlot)

library(olsrr)
library(psych)


getwd()



head(mhlth1)

#Unadjusted Linear Models

#emotionspt

lm_emotionspt <- lm(mhlth ~ emotionspt, data = mhlth1)
summary(lm_emotionspt)

df_emotionspt <- augment(lm_emotionspt)

ggplot(df_emotionspt, aes(x=.fitted, y = .resid))+geom_point()

#foodinsecu

lm_foodinsecu <- lm(mhlth ~ foodinsecu, data = mhlth1)
summary(lm_foodinsecu)

df_foodinsecu <- augment(lm_foodinsecu)

ggplot(df_foodinsecu, aes(x=.fitted, y = .resid))+geom_point()

#foodstamp

lm_foodstamp <- lm(mhlth ~ foodstamp, data = mhlth1)
summary(lm_foodstamp)

df_foodstamp <- augment(lm_foodstamp)

ggplot(df_foodstamp, aes(x=.fitted, y = .resid))+geom_point()

#housinsecu

lm_housinsecu <- lm(mhlth ~ housinsecu, data = mhlth1)
summary(lm_housinsecu)

df_housinsecu <- augment(lm_housinsecu)

ggplot(df_housinsecu, aes(x=.fitted, y = .resid))+geom_point()

#isolation

lm_isolation <- lm(mhlth ~ isolation, data = mhlth1)
summary(lm_isolation)

df_isolation <- augment(lm_isolation)

ggplot(df_isolation, aes(x=.fitted, y = .resid))+geom_point()

#lacktrpt

lm_lacktrpt <- lm(mhlth ~ lacktrpt, data = mhlth1)
summary(lm_lacktrpt)

df_lacktrpt <- augment(lm_lacktrpt)

ggplot(df_lacktrpt, aes(x=.fitted, y = .resid))+geom_point()

#shututility

lm_shututility <- lm(mhlth ~ shututility, data = mhlth1)
summary(lm_shututility)

df_shututility <- augment(lm_shututility)

ggplot(df_shututility, aes(x=.fitted, y = .resid))+geom_point()

######################################
######################################
#unadjusted coefficients assembly

lm_output <- function(data){
  
  
  unadjusted <- as.data.frame(coef(data))
  unadjusted <- rownames_to_column(unadjusted,"variable")
  unadjusted <- rename(unadjusted, estimate = `coef(data)`)
  unadjusted <- unadjusted[-1,]
  
  
  p_value <- as.data.frame(summary(data)$coefficients) %>%
    select(4)
  p_value <- p_value[-1,]
  unadjusted <- cbind(unadjusted,p_value)
  unadjusted <- unadjusted %>% 
    mutate(
      estimate = round(estimate,3)
#      p_value = round(p_value,3)
    )
  unadjusted
}

unadjusted_emotionspt <- lm_output(lm_emotionspt)
summary(lm_emotionspt)$r.squared
#R-squared = 0.225
check_model(lm_emotionspt)

unadjusted_foodinsecu <- lm_output(lm_foodinsecu)
summary(lm_foodinsecu)$r.squared
#R-suqared = 0.359
unadjusted_foodstamp <- lm_output(lm_foodstamp)
summary(lm_foodstamp)$r.squared
#R-squared = 0.431
unadjusted_housinsecu <- lm_output(lm_housinsecu)
summary(lm_housinsecu)$r.squared
#R-squared = 0.322
unadjusted_isolation <- lm_output(lm_isolation)
summary(lm_isolation)$r.squared
#R-squared = 0.256
unadjusted_lacktrpt <- lm_output(lm_lacktrpt)
summary(lm_lacktrpt)$r.squared
#R-squared = 0.404
unadjusted_shututility <- lm_output(lm_shututility)
summary(lm_shututility)$r.squared
#R-squared = 0.444



unadjusted <- rbind(unadjusted_emotionspt, unadjusted_foodinsecu,unadjusted_foodstamp, unadjusted_housinsecu, unadjusted_isolation, unadjusted_lacktrpt, unadjusted_shututility)

######################################
######################################

#Adjusted model
#

lm_adjusted <- lm(mhlth ~ emotionspt + foodinsecu + foodstamp + housinsecu + isolation + lacktrpt + shututility, data = mhlth1)
summary(lm_adjusted)

df_adjusted <- augment(lm_adjusted)

ggplot(df_adjusted, aes(x=.fitted, y = .resid))+geom_point()+
  ggtitle("FMD vs HRSNs (7) Adjusted Linear Model Residuals Plot, County Level, 2022")

#plot(lm_adjusted)

#Assembling coefficient estimates:

adjusted <- as.data.frame(coef(lm_adjusted))
adjusted <- rownames_to_column(adjusted,"variable")
adjusted <- rename(adjusted, estimate = `coef(lm_adjusted)`)
adjusted <- adjusted[-1,]


p_value <- as.data.frame(summary(lm_adjusted)$coefficients) %>%
  select(4)
p_value <- p_value[-1,]
adjusted <- cbind(adjusted,p_value)
adjusted <- adjusted %>% 
  mutate(
    estimate = round(estimate,3)
#    p_value = round(p_value,3)
  )
adjusted
summary(lm_adjusted)$r.squared
#R-squared = 0.596


lm_fmd <- cbind(unadjusted, adjusted)
write.csv(lm_fmd,"unadjusted and adjusted linear model results.csv")


#########################################
#Mixed effects

mixed_emotionspt <- lmer(mhlth ~ emotionspt + (1|full), mhlth1)
summary(mixed_emotionspt)
2.482/(2.482+1.559)
#0.614 or ~61%
#Differences between states explain ~61% of the variance that's "left over" after the variance explained by fixed effects.

check_model(mixed_emotionspt)

plot(mixed_emotionspt)
#No descernable pattern in plot. 

############

mixed_foodinsecu <- lmer(mhlth ~ foodinsecu + (1|full), mhlth1)
summary(mixed_foodinsecu)
1.765/(1.765+1.320)
#0.572 or ~57%
#Differences between states explain ~57% of the variance that's "left over" after the variance explained by fixed effects.

plot(mixed_foodinsecu)
#No descernable pattern in plot. 

############

mixed_foodstamp <- lmer(mhlth ~ foodstamp + (1|full), mhlth1)
summary(mixed_foodstamp)
1.542/(1.542+1.273)
#0.548 or ~55%
#Differences between states explain ~55% of the variance that's "left over" after the variance explained by fixed effects.

plot(mixed_foodstamp)
#No descernable pattern in plot. 

############

mixed_housinsecu <- lmer(mhlth ~ housinsecu + (1|full), mhlth1)
summary(mixed_housinsecu)
1.967/(1.967+1.358)
#0.0.592 or ~59%
#Differences between states explain ~59% of the variance that's "left over" after the variance explained by fixed effects.

plot(mixed_housinsecu)
#No descernable pattern in plot. 


mixed_isolation <- lmer(mhlth ~ isolation + (1|full), mhlth1)
summary(mixed_isolation)
2.627/(2.627+1.383)
#0.655 or ~66%
#Differences between states explain ~66% of the variance that's "left over" after the variance explained by fixed effects.
check_model(mixed_isolation)
plot(mixed_isolation)
#No descernable pattern in plot. 

mixed_lacktrpt <- lmer(mhlth ~ lacktrpt + (1|full), mhlth1)
summary(mixed_lacktrpt)
1.736/(1.736+1.139)
#0.604 or ~60%
#Differences between states explain ~60% of the variance that's "left over" after the variance explained by fixed effects.

plot(mixed_lacktrpt)
#No descernable pattern in plot. 


mixed_shututility <- lmer(mhlth ~ shututility + (1|full), mhlth1)
summary(mixed_shututility)
1.439/(1.439+1.277)
#0.530 or ~53%
#Differences between states explain ~53% of the variance that's "left over" after the variance explained by fixed effects.

plot(mixed_shututility)
#No descernable pattern in plot. 


mixed_adjusted <- lmer(mhlth ~ emotionspt +isolation+foodinsecu + foodstamp + housinsecu  + lacktrpt + shututility + (1|full), mhlth1)
summary(mixed_adjusted)
2.7336/(2.7336+0.8254)
#Variance explained by Random Effect (state) ~77%
check_model(mixed_emotionspt)
check_model(lm_adjusted)
check_model(mixed_adjusted)


plot(mixed_adjusted)


(hrsn_corr_matrix <- mhlth1 %>%
  as.data.frame() %>%
  select(
    emotionspt,isolation,foodinsecu,foodstamp,housinsecu,lacktrpt,shututility
  )%>%
  as.matrix() %>%
  cor(use="complete.obs"))


corrplot(hrsn_corr_matrix, method = "color", 
         type = 'lower', diag = FALSE,
         addCoef.col = "white",
         title = "\nHRSN Prevalence Correlation Matrix County level, 2022")


tab_model(mixed_emotionspt)
tab_model(mixed_isolation)
tab_model(mixed_foodinsecu)
tab_model(mixed_foodstamp)
tab_model(mixed_housinsecu)
tab_model(mixed_lacktrpt)
tab_model(mixed_shututility)

tab_model(mixed_adjusted)



mixed_adjusted2 <- lmer(mhlth ~ emotionspt +isolation+foodinsecu + foodstamp +  lacktrpt + (1|full), mhlth1)
summary(mixed_adjusted2)
tab_model(mixed_adjusted2)
check_model(mixed_adjusted2)

#################
#Stepwise 
lm_forward <- step(lm_adjusted, direction="forward", scope = formula(mhlth ~ emotionspt + foodinsecu + foodstamp + housinsecu + isolation + lacktrpt + shututility))

summary(lm_forward)

mixed_step <- lmer(mhlth ~ emotionspt + (1|full), mhlth1)
summary(mixed_step)

mixed_step1 <- lmer(mhlth ~ emotionspt + foodinsecu +  (1|full), mhlth1)
summary(mixed_step1)

mixed_step2 <- lmer(mhlth ~ emotionspt + foodinsecu + foodstamp + (1|full), mhlth1)
summary(mixed_step2)

mixed_step3 <- lmer(mhlth ~ emotionspt + foodinsecu + foodstamp + housinsecu + (1|full), mhlth1)
summary(mixed_step3)

mixed_step4 <- lmer(mhlth ~ emotionspt + foodinsecu + foodstamp +  isolation + (1|full), mhlth1)
summary(mixed_step4)

mixed_step4 <- lmer(mhlth ~ emotionspt + foodinsecu + isolation + lacktrpt  +(1|full), mhlth1)
summary(mixed_step4)


##########################
##########################
#COLLINEARITY EXPLORATION

#Original Adjusted Model
summary(lm_adjusted)
ols_plot_obs_fit(lm_adjusted)
#Actual vs fitted values to observe model fit

#Diagnostics panels (Click arrows to go through panels)
ols_plot_diagnostics(lm_adjusted)
check_model(lm_adjusted)

ols_plot_comp_plus_resid(lm_adjusted)


#Tolerance and VIF
ols_vif_tol(lm_adjusted)
#Tolerance = the percent of variance in the predictor that 
# cannot be accounted for by other predictors.

#VIF numbers above 5 indciate problematic multicollinearity. All but "isolation" >5

#EIGENVALUES
adjusted_eigen <- ols_eigen_cindex(lm_adjusted)
write.csv(adjusted_eigen,"FMD vs ALL HRSN Eigenvalues.csv")

#Collinearity diagnostics
ols_coll_diag(lm_adjusted)


#RESIDUAL FIT SPREAD PLOT
ols_plot_resid_fit_spread(lm_adjusted)


#additional linear modeling

lm_adjusted1 <- lm(mhlth ~ emotionspt + foodstamp + housinsecu + isolation + lacktrpt + shututility, data = mhlth1)
summary(lm_adjusted1)
ols_vif_tol(lm_adjusted1)
ols_plot_obs_fit(lm_adjusted1)

lm_adjusted2 <- lm(mhlth ~ emotionspt + foodstamp +  isolation + lacktrpt + shututility, data = mhlth1)
summary(lm_adjusted2)
ols_vif_tol(lm_adjusted2)
ols_plot_obs_fit(lm_adjusted2)

lm_adjusted3 <- lm(mhlth ~ emotionspt + foodstamp +  isolation + shututility, data = mhlth1)
summary(lm_adjusted3)
ols_vif_tol(lm_adjusted3)
ols_plot_obs_fit(lm_adjusted3)

#ALL VIF Below 5 in this model
lm_adjusted4 <- lm(mhlth ~ emotionspt +  isolation + shututility, data = mhlth1)
summary(lm_adjusted4)
ols_vif_tol(lm_adjusted4)
ols_plot_obs_fit(lm_adjusted4)
check_model(lm_adjusted4)

adjusted4_eigen <- ols_eigen_cindex(lm_adjusted4)
adjusted4_eigen
write.csv(adjusted4_eigen,"FMD vs Emotion + Isolation + Utility Eigenvalues.csv")



##################################
