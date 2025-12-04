
#conditional logit estimates 
install.packages("survival")
install.packages("marginaleffects")
install.packages("modelsummary")

install.packages("pandoc")


#library(AER) # Applied Econometrics with R
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(gmnl) # Multinomial Logit Models with Random Parameters
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics
library(stargazer) #format regression tables 
library(mlogit) # Multinomial Logit Models
#library(stargazer) # Well-Formatted Regression and Summary Statistics Tables
library(tibble) # Simple Data Frames
library(tidyr) # Tidy Messy Data
library(haven)
library(purrr)
library(cowplot)
library(marginaleffects)
#library(patchwork)
library(modelsummary)



setwd("C:/Users/donghuiwang/SynologyDrive/实验/data") #office 
#setwd("C:/Users/Donghui/SynologyDrive/实验/data") #home 

#================================================
#no need to run this chunk every time 
marriage<-read_dta("marriage_mixlogit.dta")%>%
  select(id, oid, r1, female,fmedu_max,own_onlychild,type_u, option, asc,weights, 
         xage1, xage2,xage3, iage, 
         xedu2, xedu3,xedu4,edu,
         beautyc, xhousing2, xhousing3, housing, 
         xdanwei1,xdanwei2,xdanwei3, danwei,
         xonlychild, onlychild, isalary, c10, age, a2)%>%
  #create alternative specific individual vars 
  mutate(fmedu_max_optout=if_else(oid == 3, fmedu_max, 0),
         own_onlychild_optout=if_else(oid==3, own_onlychild, 0),
         type_u1_optout = if_else(oid == 3 & type_u == 1, 1, 0),  # 退出且type_u=1时为1
         type_u2_optout = if_else(oid == 3 & type_u == 2, 1, 0),  # 退出且type_u=2时为1
         want_marry_optout=if_else(oid==3 & c10==1, 1, 0) , #plan to marry
         ownage_optout=if_else(oid==3, age, 0), 
         owngrade_optout=if_else(oid==3, a2,0) ,
         
         # create interactions between family background and attributes 
         
         xage1_fmedu_max=xage1*fmedu_max,
         xage2_fmedu_max=xage2*fmedu_max,
         xage3_fmedu_max=xage3*fmedu_max,
         
         xedu2_fmedu_max=xedu2*fmedu_max,
         xedu3_fmedu_max=xedu3*fmedu_max,
         xedu4_fmedu_max=xedu4*fmedu_max,
         
         xdanwei1_fmedu_max=xdanwei1*fmedu_max,
         xdanwei2_fmedu_max=xdanwei2*fmedu_max,
         xdanwei3_fmedu_max=xdanwei3*fmedu_max,
         
         xhousing2_fmedu_max= xhousing2*fmedu_max,
         xhousing3_fmedu_max= xhousing3*fmedu_max,
         
         beautyc_fmedu_max=beautyc*fmedu_max,
         xonlychild_fmedu_max=xonlychild*fmedu_max,
         
         #female interactions 
         asc_female=asc*female,
         xage1_female=xage1*female,
         xage2_female=xage2*female,
         xage3_female=xage3*female,
         beautyc_female=beautyc*female,
         xdanwei1_female=xdanwei1*female,
         xdanwei2_female=xdanwei2*female,
         xdanwei3_female=xdanwei3*female,
         xedu2_female=xedu2*female,
         xedu3_female=xedu3*female,
         xedu4_female=xedu4*female,
         xhousing2_female=xhousing2*female,
         xhousing3_female=xhousing3*female,
         xonlychild_female=xonlychild*female,
         isalary_female=isalary*female
         
  )


marriage_mixlogit<-mlogit.data(marriage,
                               choice = "option",
                               shape = "long",
                               chid.var="id",
                               alt.var="oid")

marriage_mixlogit_female<-subset(marriage_mixlogit, female==1)
marriage_mixlogit_male<-subset(marriage_mixlogit, female==0)

save(marriage_mixlogit_female, file="marriage_mixlogit_female.RData")
save(marriage_mixlogit_male, file="marriage_mixlogit_male.RData")
save (marriage_mixlogit, file = "marriage_mixlogit.RData")


#===================================
#conditional logit 

load("marriage_mixlogit_female.RData")
load("marriage_mixlogit_male.RData")
load("marriage_mixlogit.RData")

iv_formula <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2+xhousing3 + xonlychild + isalary |0


iv_formula_inter <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2+xhousing3 + xonlychild + isalary +
  xage1_fmedu_max+xage2_fmedu_max+xage3_fmedu_max+
  xedu2_fmedu_max+ xedu3_fmedu_max+xedu4_fmedu_max+
  xdanwei1_fmedu_max+xdanwei2_fmedu_max+xdanwei3_fmedu_max+
  xhousing2_fmedu_max+xhousing3_fmedu_max+
  beautyc_fmedu_max+xonlychild_fmedu_max | 0

#vars predict ouput-out
iv_formula_asc <- option ~  asc+xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2+xhousing3 + xonlychild + isalary+
  ownage_optout+fmedu_max_optout+own_onlychild_optout+type_u1_optout+type_u2_optout+want_marry_optout |  # Active choice variables
  0  # Opt-out specific



iv_formula_both <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2+xhousing3 + xonlychild + isalary +
  xage1_fmedu_max+xage2_fmedu_max+xage3_fmedu_max+
  xedu2_fmedu_max+ xedu3_fmedu_max+xedu4_fmedu_max+
  xdanwei1_fmedu_max+xdanwei2_fmedu_max+xdanwei3_fmedu_max+
  xhousing2_fmedu_max+xhousing3_fmedu_max+
  beautyc_fmedu_max+xonlychild_fmedu_max +
  fmedu_max_optout+own_onlychild_optout+type_u1_optout+type_u2_optout| 0

m1_clogit_female <- mlogit(
  iv_formula,
  #iv_formula_inter ,
  #iv_formula_asc,
  #iv_formula_both,
  data = marriage_mixlogit_female,
  #weights = marriage_mixlogit_female$weights
)

summary(m1_clogit_female) # same as stata cmlogit result 

#male 
m1_clogit_male <- mlogit(
  iv_formula,
  #iv_formula_inter,
  #iv_formula_both,
  #iv_formula_asc,
  data = marriage_mixlogit_male,
  #weights = marriage_mixlogit_male$weights
)

summary(m1_clogit_male) # same as stata cmlogit result 

stargazer(
  m1_clogit_female,
  m1_clogit_male,  
  
  type = "html",
  title = "full(Female Vs Male)",
  align = TRUE,
  digit.separator = "",
  #out = "m1_clogit_asc.rtf"
  out = "m1_clogit_m1.rtf"
)


#test gender differences 
iv_gender <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2+xhousing3 + xonlychild + isalary +
  asc_female + xage1_female + xage2_female + xage3_female + beautyc_female + 
  xdanwei1_female + xdanwei2_female + xdanwei3_female + 
  xedu2_female + xedu3_female + xedu4_female + 
  xhousing2_female+xhousing3_female + xonlychild_female + isalary_female|0


m1_clogit_gender <- mlogit(
  iv_gender,
  data = marriage_mixlogit,
)

summary(m1_clogit_gender)

modelsummary(m1_clogit_gender, output = "m1_clogit_gender.docx")

#----------------------------------------------------------
#calculate average marginal effect 

coef_summary <- summary(m1_clogit_female)$coefficients
isalary_coef <- coef_summary["isalary", ]
odds_ratio <- exp(isalary_coef)



#---------------------------------------------------------
#predicted probabilities 
fitted_probs_wide  <- fitted(m1_clogit_female, outcome = FALSE, type = "probabilities") 
#in-sample fitted probabilities; outcome= FALSE for prob of all alternatives. only apply to female sample 

fitted_probs_long <- data.frame(
  id = rep(rownames(fitted_probs_wide), each = 3),
  oid = rep(1:3, nrow(fitted_probs_wide)),
  prob = as.vector(t(fitted_probs_wide))
)

# Merge with original data
clogit_female_pred <- merge(
  marriage_mixlogit_female,
  fitted_probs_long,
  by = c("id", "oid"),
)%>%
  tibble()

#predcited shares at the aggregate level 
share<-clogit_female_pred%>%
  group_by(oid)%>%
  summarise(share=sum(prob*weights)/sum(weights)) #0.294 

#as it is 
#----------------------------------
#calculate share based on the unadjusted  model 
total_weights <- clogit_female_pred %>%
  summarise(total = sum(weights)) %>%
  pull(total)

#calculate unconditional share 
group_vars <-c("edu", "iage" , "beautyc", "housing", "danwei", "onlychild", "isalary")

uncon_shares<-function(var){
  clogit_female_pred%>%
    group_by(across(all_of(var)))%>%
    summarise(nchoose=sum(prob*weights),
              ntotal=total_weights/3)%>%
    ungroup()%>%
    mutate(share=nchoose/ntotal)
}

results_list_female_original<-map(group_vars, uncon_shares)

names(results_list_female_original) <- group_vars

#====================================
#run iterations simulate different scenario 
#run iteration_asc.R first 






#============================================
#do the same thing for male 

#predicted probabilities 
fitted_probs_wide  <- fitted(m1_clogit_male, outcome = FALSE, type = "probabilities") #in-sample fitted probabilities; outcome= FALSE for prob of all alternatives.
fitted_probs_long <- data.frame(
  id = rep(rownames(fitted_probs_wide), each = 3),
  oid = rep(1:3, nrow(fitted_probs_wide)),
  prob = as.vector(t(fitted_probs_wide))
)

# Merge with original data
clogit_male_pred <- merge(
  marriage_mixlogit_male,
  fitted_probs_long,
  by = c("id", "oid"),
)%>%
  tibble()

#predicted shares at the aggregate level 
share<-clogit_male_pred%>%
  group_by(oid)%>%
  summarise(share=sum(prob*weights)/sum(weights)) #0.232 # the same 


#As it is 
#calculate share based on the calibarated model 
total_weights <- clogit_male_pred %>%
  summarise(total = sum(weights)) %>%
  pull(total)

#calculate unconditional share 
group_vars <-c("edu", "iage" , "beautyc", "housing", "danwei", "onlychild", "isalary")

uncon_shares<-function(var){
  clogit_male_pred%>%
    group_by(across(all_of(var)))%>%
    summarise(nchoose=sum(prob*weights),
              ntotal=total_weights/3)%>%
    ungroup()%>%
    mutate(share=nchoose/ntotal)
}

results_list_male_original<-map(group_vars, uncon_shares)

names(results_list_male_original) <- group_vars


#=====================================================================
#additional analysis of relative importance using partial log-likelihood analysis 
#Lancsar, Emily, Jordan Louviere, and Terry Flynn. "Several methods to investigate relative attribute impact in stated preference experiments." Social science & medicine 64.8 (2007): 1738-1753.
# ！ this version is not presented in the paper ! 
#Leave one out and measure 

# Full model (iv0)
iv0 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0

# Models leaving out one variable each
iv1 <- option ~ asc + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xage1

iv2 <- option ~ asc + xage1 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xage2

iv3 <- option ~ asc + xage1 + xage2 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xage3

iv4 <- option ~ asc + xage1 + xage2 + xage3 + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out beautyc

iv5 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xdanwei1

iv6 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xdanwei2

iv7 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xdanwei3

iv8 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xedu2

iv9 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xedu3

iv10 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  # leaves out xedu4

iv11 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing3 + xonlychild + isalary | 0  # leaves out xhousing2

iv12 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xonlychild + isalary | 0  # leaves out xhousing3

iv13 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + isalary | 0  # leaves out xonlychild

iv14 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild | 0  # leaves out isalary


results_female <- data.frame(model = paste0("iv", 0:14), loglik = NA)

# Run all models one by one
results_female$loglik[1] <- as.numeric(logLik(mlogit(iv0, data = marriage_mixlogit_female)))
results_female$loglik[2] <- as.numeric(logLik(mlogit(iv1, data = marriage_mixlogit_female)))
results_female$loglik[3] <- as.numeric(logLik(mlogit(iv2, data = marriage_mixlogit_female)))
results_female$loglik[4] <- as.numeric(logLik(mlogit(iv3, data = marriage_mixlogit_female)))
results_female$loglik[5] <- as.numeric(logLik(mlogit(iv4, data = marriage_mixlogit_female)))
results_female$loglik[6] <- as.numeric(logLik(mlogit(iv5, data = marriage_mixlogit_female)))
results_female$loglik[7] <- as.numeric(logLik(mlogit(iv6, data = marriage_mixlogit_female)))
results_female$loglik[8] <- as.numeric(logLik(mlogit(iv7, data = marriage_mixlogit_female)))
results_female$loglik[9] <- as.numeric(logLik(mlogit(iv8, data = marriage_mixlogit_female)))
results_female$loglik[10] <- as.numeric(logLik(mlogit(iv9, data = marriage_mixlogit_female)))
results_female$loglik[11] <- as.numeric(logLik(mlogit(iv10, data = marriage_mixlogit_female)))
results_female$loglik[12] <- as.numeric(logLik(mlogit(iv11, data = marriage_mixlogit_female)))
results_female$loglik[13] <- as.numeric(logLik(mlogit(iv12, data = marriage_mixlogit_female)))
results_female$loglik[14] <- as.numeric(logLik(mlogit(iv13, data = marriage_mixlogit_female)))
results_female$loglik[15] <- as.numeric(logLik(mlogit(iv14, data = marriage_mixlogit_female)))

# Show results
print(results_female)

results_female$loglik_diff<-results_female$loglik - results_female$loglik[1]
total_loglik_diff <- sum(results_female$loglik_diff[-1])

results_female$pct_contribution <- (results_female$loglik_diff / total_loglik_diff) * 100

#rank the rlative contribution
results_female$rank <- NA
results_female$rank[-1] <- rank(-results_female$pct_contribution[-1]) 



#do the same thing for male 
results_male <- data.frame(model = paste0("iv", 0:14), loglik = NA)

results_male$loglik[1] <- as.numeric(logLik(mlogit(iv0, data = marriage_mixlogit_male)))
results_male$loglik[2] <- as.numeric(logLik(mlogit(iv1, data = marriage_mixlogit_male)))
results_male$loglik[3] <- as.numeric(logLik(mlogit(iv2, data = marriage_mixlogit_male)))
results_male$loglik[4] <- as.numeric(logLik(mlogit(iv3, data = marriage_mixlogit_male)))
results_male$loglik[5] <- as.numeric(logLik(mlogit(iv4, data = marriage_mixlogit_male)))
results_male$loglik[6] <- as.numeric(logLik(mlogit(iv5, data = marriage_mixlogit_male)))
results_male$loglik[7] <- as.numeric(logLik(mlogit(iv6, data = marriage_mixlogit_male)))
results_male$loglik[8] <- as.numeric(logLik(mlogit(iv7, data = marriage_mixlogit_male)))
results_male$loglik[9] <- as.numeric(logLik(mlogit(iv8, data = marriage_mixlogit_male)))
results_male$loglik[10] <- as.numeric(logLik(mlogit(iv9, data = marriage_mixlogit_male)))
results_male$loglik[11] <- as.numeric(logLik(mlogit(iv10, data = marriage_mixlogit_male)))
results_male$loglik[12] <- as.numeric(logLik(mlogit(iv11, data = marriage_mixlogit_male)))
results_male$loglik[13] <- as.numeric(logLik(mlogit(iv12, data = marriage_mixlogit_male)))
results_male$loglik[14] <- as.numeric(logLik(mlogit(iv13, data = marriage_mixlogit_male)))
results_male$loglik[15] <- as.numeric(logLik(mlogit(iv14, data = marriage_mixlogit_male)))

# Show results
print(results_male)


#do the same thing for male 

results_male$loglik_diff<-results_male$loglik - results_male$loglik[1]
total_loglik_diff <- sum(results_male$loglik_diff[-1])

results_male$pct_contribution <- (results_male$loglik_diff / total_loglik_diff) * 100

results_male$rank <- NA
results_male$rank[-1] <- rank(-results_male$pct_contribution[-1]) 


#=================================================================================
#version 2: systematically leave out one type of attribute, not one variable : use version 2 in the paper 

# Full model (iv0)
iva0 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0


iva1 <- option ~ asc + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0   #noage 


iva2 <- option ~ asc + xage1 + xage2 + xage3 + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  #no beauty 



iva3 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0  #nodanwei 



iva4 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xhousing2 + xhousing3 + xonlychild + isalary | 0   #noedu 


iva5 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
    xonlychild + isalary | 0  #no housing 


iva6 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3  + isalary | 0  #no onlychild


iva7 <- option ~ asc + xage1 + xage2 + xage3 + beautyc + 
  xdanwei1 + xdanwei2 + xdanwei3 + 
  xedu2 + xedu3 + xedu4 + 
  xhousing2 + xhousing3 + xonlychild  | 0  #no salary 



results_female <- data.frame(model = paste0("iv", 0:7), loglik = NA)

# Run all models one by one
results_female$loglik[1] <- as.numeric(logLik(mlogit(iva0, data = marriage_mixlogit_female)))
results_female$loglik[2] <- as.numeric(logLik(mlogit(iva1, data = marriage_mixlogit_female)))
results_female$loglik[3] <- as.numeric(logLik(mlogit(iva2, data = marriage_mixlogit_female)))
results_female$loglik[4] <- as.numeric(logLik(mlogit(iva3, data = marriage_mixlogit_female)))
results_female$loglik[5] <- as.numeric(logLik(mlogit(iva4, data = marriage_mixlogit_female)))
results_female$loglik[6] <- as.numeric(logLik(mlogit(iva5, data = marriage_mixlogit_female)))
results_female$loglik[7] <- as.numeric(logLik(mlogit(iva6, data = marriage_mixlogit_female)))
results_female$loglik[8] <- as.numeric(logLik(mlogit(iva7, data = marriage_mixlogit_female)))

# Show results
print(results_female)

results_female$loglik_diff<-results_female$loglik - results_female$loglik[1]
total_loglik_diff <- sum(results_female$loglik_diff[-1])

results_female$pct_contribution <- (results_female$loglik_diff / total_loglik_diff) * 100

#rank the rlative contribution
results_female$rank <- NA
results_female$rank[-1] <- rank(-results_female$pct_contribution[-1]) 
print(results_female)

# Table S3
# model    loglik loglik_diff pct_contribution rank
# 1   iv0 -4874.168    0.000000         0.000000   NA
# 2   iv1 -5100.755 -226.586409        32.193741    1
# 3   iv2 -4988.335 -114.167197        16.221049    3
# 4   iv3 -4883.270   -9.101404         1.293141    7
# 5   iv4 -4976.914 -102.745760        14.598274    5
# 6   iv5 -4994.979 -120.810662        17.164962    2
# 7   iv6 -4892.472  -18.303766         2.600627    6
# 8   iv7 -4986.274 -112.106102        15.928205    4


#do the same thing for male 

results_male <- data.frame(model = paste0("iv", 0:7), loglik = NA)

# Run all models one by one
results_male$loglik[1] <- as.numeric(logLik(mlogit(iva0, data = marriage_mixlogit_male)))
results_male$loglik[2] <- as.numeric(logLik(mlogit(iva1, data = marriage_mixlogit_male)))
results_male$loglik[3] <- as.numeric(logLik(mlogit(iva2, data = marriage_mixlogit_male)))
results_male$loglik[4] <- as.numeric(logLik(mlogit(iva3, data = marriage_mixlogit_male)))
results_male$loglik[5] <- as.numeric(logLik(mlogit(iva4, data = marriage_mixlogit_male)))
results_male$loglik[6] <- as.numeric(logLik(mlogit(iva5, data = marriage_mixlogit_male)))
results_male$loglik[7] <- as.numeric(logLik(mlogit(iva6, data = marriage_mixlogit_male)))
results_male$loglik[8] <- as.numeric(logLik(mlogit(iva7, data = marriage_mixlogit_male)))

# Show results
print(results_male)

results_male$loglik_diff<-results_male$loglik - results_male$loglik[1]
total_loglik_diff <- sum(results_male$loglik_diff[-1])

results_male$pct_contribution <- (results_male$loglik_diff / total_loglik_diff) * 100

#rank the rlative contribution
results_male$rank <- NA
results_male$rank[-1] <- rank(-results_male$pct_contribution[-1]) 


print(results_male)


# model    loglik loglik_diff pct_contribution rank
# 1   iv0 -3627.140    0.000000         0.000000   NA
# 2   iv1 -3783.868 -156.727455        53.068697    1
# 3   iv2 -3692.024  -64.883805        21.969980    2
# 4   iv3 -3630.295   -3.154538         1.068142    7
# 5   iv4 -3643.599  -16.458817         5.573037    5
# 6   iv5 -3656.323  -29.182752         9.881425    3
# 7   iv6 -3631.447   -4.306306         1.458137    6
# 8   iv7 -3647.756  -20.615708         6.980581    4

#===================================================================
#==================================================================

