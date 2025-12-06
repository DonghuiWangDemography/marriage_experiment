#import the census data 
#I have tried two version, the first version is use the entire population of single, the second version is to use single w some college & above degree.
# The deficit pattern is more or less the same. might just stick with simpler version: singles with some college degree & above


library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(tibble) # Simple Data Frames
library(tidyr) # Tidy Messy Data
library(haven)
library(readxl)

setwd("C:/Users/donghuiwang/SynologyDrive/实验/data")


#no limit on age category 
#keep only zhuanke and above
census <- read_excel("2020census_marriage_collegeabove.xlsx",
                         sheet = "whole_coded")%>%
  filter(#category != "total",
         #zhuanke and above
         category %in% c("zhuanke", "ba", "master", "phd")) %>%
  mutate(#category_broad=if_else(category %in% c("noschool", "preschool","primary","middle","high", "zhuanke"), "zhuankebelow", category),
         #below 25, 25-29,30-34,35 and above

           age_cat= case_when(
           agegp==1 | agegp ==2 ~ 1 ,
           agegp==3 ~ 2 ,
           agegp==4 ~ 3 ,
           agegp >4 ~ 4),
         edu_broad=case_when(
           category =="zhuanke" ~ 1,
           category =="ba"  ~ 2,
           category =="master" ~ 3,
           category =="phd" ~ 4
         ))




# version 2 : no education restriction 
# census <- read_excel("2020census_marriage_collegeabove.xlsx", 
#                      sheet = "whole_coded")%>%
#   filter(category != "total" )%>%
#   mutate(
#     category_broad=if_else(category %in% c("noschool", "preschool","primary","middle","high"), "belowzhuanke", category),
#     #below 25, 25-29,30-34,35 and above 
#     age_cat= case_when(
#       agegp==1 | agegp ==2 ~ 1 ,
#       agegp==3 ~ 2 ,
#       agegp==4 ~ 3 ,
#       agegp >4 ~ 4),
#     
#     edu_broad=case_when(
#       category_broad =="belowzhuanke" ~ 0.5,
#       category_broad =="zhuanke" ~ 1,
#       category_broad =="ba"  ~ 2, 
#       category_broad =="master" ~ 3,
#       category_broad =="phd" ~ 4
#     ))





#age distributions of female single   
census_agegp_female<-census%>%
  mutate(total=sum(female))%>%
  group_by(age_cat )%>%
  summarise(female_tgp=sum(female))%>%
  ungroup()%>%
  mutate(share=female_tgp/sum(female_tgp),
         type = "census")%>%
  select(-female_tgp)

save(census_agegp_female, file = "census_agegp_female.RData")  

census_agegp_male<-census%>%
  mutate(total=sum(male))%>%
  group_by(age_cat )%>%
  summarise(male_tgp=sum(male))%>%
  ungroup()%>%
  mutate(share=male_tgp/sum(male_tgp),
         type="census")%>%
  select(-male_tgp )

save(census_agegp_male, file = "census_agegp_male.RData")  

#education distribution of female single 
census_edu_female<-census %>%
  mutate(total=sum(female))%>%
  group_by(edu_broad)%>%
  summarise(female_tgp=sum(female))%>%
  ungroup()%>%
  mutate(share=female_tgp/sum(female_tgp),
         type="census")%>%
  select(-female_tgp)%>%
  rename(edu= edu_broad)

save(census_edu_female, file="census_edu_female.RData")

census_edu_male<-census %>%
  mutate(total=sum(male))%>%
  group_by(edu_broad)%>%
  summarise(male_tgp=sum(male))%>%
  ungroup()%>%
  mutate(share=male_tgp/sum(male_tgp),
         type= "census")%>%
  select(-male_tgp)%>%
  rename(edu= edu_broad)
save(census_edu_male, file="census_edu_male.RData")
