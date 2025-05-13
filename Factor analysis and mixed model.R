#library(psych)
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



head(mhlth2)
head(hrsn)

#KMO Test

kmo_results <- KMO(hrsn)
kmo_results

#KMO value above 0.6 indicates that data is suitable for factor analysis

#Bartlett's Test

bartlett_test <- cortest.bartlett(hrsn)
bartlett_test


#factor analysis

fa_results <- fa(hrsn, nfactors = 2, rotate = "varimax")
fa_results

fa_results <- fa(hrsn, nfactors = 3)
fa_results


#How much does each variable contribute to each factor
fa.diagram(fa_results)

#factor loading, indicate how much each variable contributes to each factor
#Communalities, show how much variance in each variable is explained by the factors


#Determine number of factors:

#parallel analysis:

fa.parallel(hrsn, fa="fa",n.iter=100)

scree(hrsn)
#"ELBOW" suggests number of factors needed where eigenvalues level off
#2 factors


factor_scores <- fa_results$scores

factor_scores <- as.data.frame(factor_scores)

factor_scores <- factor_scores %>%
  rename(
    "ses" = MR1,
    "emotion" = MR2
  )

mhlth1 <- cbind(mhlth1,factor_scores)


model_ses <- lm(mhlth ~ ses , data = mhlth1)
summary(model_ses)

model_emotion <- lm(mhlth ~ emotion , data = mhlth1)
summary(model_emotion)

model <- lm(mhlth ~ ses + emotion, data = mhlth1)
summary(model)
tab_model(model,
          show.intercept = FALSE,
          show.se = TRUE,
          show.aic = TRUE,
          title = "Linear Model of Prevalence of Frequent Mental Distress on Factored Health-related Social Needs, 2022"
)
check_model(model)

##################################
#############
#mixed models
#############
head(mhlth1)

#unadjusted SES 
mixed_model_ses <- lmer(mhlth ~ ses + (1|full), mhlth1)
summary(mixed_model_ses)
tab_model(mixed_model_ses,
          show.intercept = FALSE,
          show.se = TRUE,
          show.aic = TRUE,
          title = "Unadjusted Mixed Effects Model of Prevalence of Frequent Mental Distress on Factored Health-related Social Needs, 2022"
)

#unadjusted emotion 
mixed_model_emotion <- lmer(mhlth ~ emotion + (1|full), mhlth1)
summary(mixed_model_emotion)
tab_model(mixed_model_emotion,
          show.intercept = FALSE,
          show.se = TRUE,
          show.aic = TRUE,
          title = "Unadjusted Mixed Effects Model of Prevalence of Frequent Mental Distress on Factored Health-related Social Needs, 2022"
)



#adjusted

mixed_model <- lmer(mhlth ~ ses + emotion + (1|full), mhlth1)
summary(mixed_model)
tab_model(mixed_model,
          show.intercept = FALSE,
          show.se = TRUE,
          show.aic = TRUE,
          title = "Mixed Effects Model of Prevalence of Frequent Mental Distress on Factored Health-related Social Needs, 2022"
          )

check_model(mixed_model)

#Correlation matrix of factors

(factor_corr_matrix <- mhlth1 %>%
    as.data.frame() %>%
    select(
      ses, emotion
    )%>%
    as.matrix() %>%
    cor(use="complete.obs"))


corrplot(factor_corr_matrix, method = "color", 
         type = 'lower', diag = FALSE,
         addCoef.col = "black",
         title = "\nTwo Factor HRSN Prevalence Correlation Matrix County level, 2022")


#install.packages("lavaan")
library(lavaan)

library(foreign)




#Plots (not used)
theme_set(theme_bw())
ggplot(mhlth1, aes(x = ses, y=mhlth))+
  geom_point(alpha=0.5 , size =1, color = "red")+
  geom_point(aes(x=emotion), alpha=0.5, size = 1,color= "blue")+
  labs(x = "Factor Estimates", y = "Mean FMD", title = )
  

#Factor 1
mhlth1 %>%
  ggplot(aes(x = ses, y=mhlth, col = full))+
  geom_point()+
  geom_smooth(method= "lm", se = F)

#FACTOR 2
mhlth1 %>%
  ggplot(aes(x = emotion, y=mhlth, col = full))+
  geom_point()+
  geom_smooth(method= "lm", se = F)

write.csv(mhlth1,"fmd_hrsn_factors.csv")
