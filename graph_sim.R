#graphing the simulated results 
library(ggplot2)
library(tidyverse)
library(writexl)
library(haven)
library(patchwork) 

setwd("C:/Users/donghuiwang/SynologyDrive/实验/data") #office 
#setwd("C:/Users/Donghui/SynologyDrive/实验/data") #home 
sim_results_female <- readRDS("sim_results_female.rds")
sim_results_male <- readRDS("sim_results_male.rds")



#adjust 
female_edu<-sim_results_female$edu%>%
  #not include senario 3 
  select(-senario3)%>%
  mutate(across(where(haven::is.labelled), as.numeric))%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(edu)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(
    edu_label = case_when(
    edu == 0 ~ "Opt-out",
    edu == 1 ~ "2yr-College",
    edu == 2 ~ "Bachelor",
    edu == 3 ~ "Master",
    edu == 4 ~ "PhD"
  ))%>%
  mutate(edu_label = factor(edu_label, levels = c("Opt-out", "2yr-College", "Bachelor", "Master", "PhD")),
         gender="female")



#age 
female_iage<-sim_results_female$iage%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(iage)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(iage_label = case_when(
    iage == 0 ~ "Opt-out",
    iage == 1 ~ "25 and below",
    iage == 2 ~ "25～29",
    iage == 3 ~ "30～34",
    iage == 4 ~ "35+"
  ))%>%
  mutate(iage_label = factor(iage_label, levels = c("Opt-out", "25 and below", "25～29", "30～34", "35+")),
         gender="female")


# beautyc
female_beautyc<-sim_results_female$beautyc%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(beautyc)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(beautyc_label = case_when(
    beautyc == 0 ~ "Opt-out",
    beautyc == 4 ~ "4",
    beautyc == 6 ~ "6",
    beautyc == 8 ~ "8",
    beautyc == 10 ~ "10"
  ))%>%
  mutate(beautyc_label = factor(beautyc_label, levels = c("Opt-out", "4", "6", "8", "10")),
         gender="female")


#housing
female_housing<-sim_results_female$housing%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(housing)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(housing_label = case_when(
    housing == 0 ~ "Opt-out",
    housing == 1 ~ "No housing",
    housing == 2 ~ "1 apt",
    housing == 3 ~ "2 apts",
  ))%>%
  mutate(housing_label = factor(housing_label, levels = c("Opt-out", "No housing", "1 apt", "2 apts")),
         gender="female")


#danwei 
female_danwei<-sim_results_female$danwei%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(danwei)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(danwei_label = case_when(
    danwei == 0 ~ "Opt-out",
    danwei == 1 ~ "Goverment",
    danwei == 2 ~ "Foreign company",
    danwei == 3 ~ "Private company",
    danwei == 4 ~ "Freelance"

  ))%>%
  mutate(danwei_label = factor(danwei_label, levels = c("Opt-out","Goverment", "Foreign company" , "Private company","Freelance")),
         gender="female")


#only child 
female_onlychild<-sim_results_female$onlychild%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(onlychild)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(onlychild_label=case_when(
    onlychild == 0 ~ "Opt-out",
    onlychild == 1 ~ "Only child" ,
    onlychild == 2 ~ "has siblings"
   ))%>%
  mutate(onlychild_label = factor(onlychild_label, levels = c("Opt-out","Only child" , "has siblings")),
         gender="female")

  

#salary 
female_isalary<-sim_results_female$isalary%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(isalary)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(isalary_label = case_when(
    isalary == 0 ~ "Opt-out",
    isalary == 0.5 ~ "0.5k",
    isalary == 1 ~ "10k",
    isalary == 1.5 ~ "10.5k",
    isalary == 2 ~ "20k"
    
  ))%>%
  mutate(isalary_label = factor(isalary_label, levels = c("Opt-out","0.5k","10k","10.5k","20k")),
         gender="female")


#--------------------------
#graph female only 
# 
# sim_colors<-c("#0039A6", "#97CAEB" , "#CC0000")
# 
# female_edu_sim<-ggplot(female_edu, aes(y = edu_label, x = percent, color = senarios_ordered)) +
#   geom_linerange(aes(xmin = 0, xmax = percent), 
#                  position = position_dodge(width = 0.7),
#                  linewidth = 1.2) +
#   geom_point(position = position_dodge(width = 0.7), size = 3.5) +
#   scale_color_manual(values=sim_colors)+
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(y= "",  x = "Percentage", color = "Scenarios",
#        title = "Education") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10),
#         legend.position = "bottom")
# 
# 
# 
# 
# female_iage_sim<-ggplot(female_iage, aes(y = iage_label, x = percent, color = senarios_ordered)) +
#   geom_linerange(aes(xmin = 0, xmax = percent), 
#                  position = position_dodge(width = 0.7),
#                  linewidth = 1.2) +
#   geom_point(position = position_dodge(width = 0.7), size = 3.5) +
#   scale_color_manual(values=sim_colors)+
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(y= "",  x = "Percentage", color = "Scenarios",
#        title = "Age") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10),
#         legend.position = "bottom")
# 
# 
# female_beautyc_sim<-ggplot(female_beautyc, aes(y = beautyc_label, x = percent, color = senarios_ordered)) +
#   geom_linerange(aes(xmin = 0, xmax = percent), 
#                  position = position_dodge(width = 0.7),
#                  linewidth = 1.2) +
#   geom_point(position = position_dodge(width = 0.7), size = 3.5) +
#   scale_color_manual(values=sim_colors)+
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(y= "",  x = "Percentage", color = "Scenarios",
#        title = "Physical attractivenes") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10),
#         legend.position = "bottom")
# 
# 
# #housing
# female_housing_sim<-ggplot(female_housing, aes(y = housing_label, x = percent, color = senarios_ordered)) +
#   geom_linerange(aes(xmin = 0, xmax = percent), 
#                  position = position_dodge(width = 0.7),
#                  linewidth = 1.2) +
#   geom_point(position = position_dodge(width = 0.7), size = 3.5) +
#   scale_color_manual(values=sim_colors)+
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(y= "",  x = "Percentage", color = "Scenarios",
#        title = "Family Housing") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10),
#         legend.position = "bottom")
# 
# 
# #danwei : bit strange 
# female_danwei_sim<-ggplot(female_danwei, aes(y = danwei_label, x = percent, color = senarios_ordered)) +
#   geom_linerange(aes(xmin = 0, xmax = percent), 
#                  position = position_dodge(width = 0.7),
#                  linewidth = 1.2) +
#   geom_point(position = position_dodge(width = 0.7), size = 3.5) +
#   scale_color_manual(values=sim_colors)+
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(y= "",  x = "Percentage", color = "Scenarios",
#        title = "Work Sector") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10),
#         legend.position = "bottom")
# 
# 
# #onlychild
# female_onlychild_sim<-ggplot(female_onlychild, aes(y = onlychild_label, x = percent, color = senarios_ordered)) +
#   geom_linerange(aes(xmin = 0, xmax = percent), 
#                  position = position_dodge(width = 0.7),
#                  linewidth = 1.2) +
#   geom_point(position = position_dodge(width = 0.7), size = 3.5) +
#   scale_color_manual(values=sim_colors)+
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(y= "",  x = "Percentage", color = "Scenarios",
#        title = "Sibship status") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10),
#         legend.position = "bottom")
# 
# #salary 
# female_salary_sim<-ggplot(female_isalary, aes(y = isalary_label, x = percent, color = senarios_ordered)) +
#   geom_linerange(aes(xmin = 0, xmax = percent), 
#                  position = position_dodge(width = 0.7),
#                  linewidth = 1.2) +
#   geom_point(position = position_dodge(width = 0.7), size = 3.5) +
#   scale_color_manual(values=sim_colors)+
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(y= "",  x = "Percentage", color = "Scenarios",
#        title = "Salary ") +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 10),
#         legend.position = "bottom")
# 
# 
# 
# plot_grid_female_sim <- (female_iage_sim + female_edu_sim + female_danwei_sim + 
#                        female_salary_sim + female_onlychild_sim + female_housing_sim + 
#                        female_beautyc_sim + 
#                        plot_layout(ncol = 3, guides = "collect")) &
#   theme(legend.position = "bottom")
# 
# ggsave("plot_grid_female_sim.png", plot=plot_grid_female_sim )
# 


#-----------------------------------------------
#do the same thing for men : men percentage as negative 


#adjust 
male_edu<-sim_results_male$edu%>%
  #not include scenario 3 
  select(-senario3)%>%
  mutate(across(where(haven::is.labelled), as.numeric))%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(edu)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(
    edu_label = case_when(
      edu == 0 ~ "Opt-out",
      edu == 1 ~ "2yr-College",
      edu == 2 ~ "Bachelor",
      edu == 3 ~ "Master",
      edu == 4 ~ "PhD"
    ))%>%
  mutate(edu_label = factor(edu_label, levels = c("Opt-out", "2yr-College", "Bachelor", "Master", "PhD")),
         gender="male")  


#age 
male_iage<-sim_results_male$iage%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(iage)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(iage_label = case_when(
    iage == 0 ~ "Opt-out",
    iage == 1 ~ "25 and below",
    iage == 2 ~ "25～29",
    iage == 3 ~ "30～34",
    iage == 4 ~ "35+"
  ))%>%
  mutate(iage_label = factor(iage_label, levels = c("Opt-out", "25 and below", "25～29", "30～34", "35+")),
         gender="male")


# beautyc
male_beautyc<-sim_results_male$beautyc%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(beautyc)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(beautyc_label = case_when(
    beautyc == 0 ~ "Opt-out",
    beautyc == 4 ~ "4",
    beautyc == 6 ~ "6",
    beautyc == 8 ~ "8",
    beautyc == 10 ~ "10"
  ))%>%
  mutate(beautyc_label = factor(beautyc_label, levels = c("Opt-out", "4", "6", "8", "10")),
         gender="male")


#housing
male_housing<-sim_results_male$housing%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(housing)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(housing_label = case_when(
    housing == 0 ~ "Opt-out",
    housing == 1 ~ "No housing",
    housing == 2 ~ "1 apt",
    housing == 3 ~ "2 apts",
  ))%>%
  mutate(housing_label = factor(housing_label, levels = c("Opt-out", "No housing", "1 apt", "2 apts")),
         gender="male")


#danwei 
male_danwei<-sim_results_male$danwei%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(danwei)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(danwei_label = case_when(
    danwei == 0 ~ "Opt-out",
    danwei == 1 ~ "Goverment",
    danwei == 2 ~ "Foreign company",
    danwei == 3 ~ "Private company",
    danwei == 4 ~ "Freelance"
    
  ))%>%
  mutate(danwei_label = factor(danwei_label, levels = c("Opt-out","Goverment", "Foreign company" , "Private company","Freelance")),
         gender="male")




#only child 
male_onlychild<-sim_results_male$onlychild%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(onlychild)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(onlychild_label=case_when(
    onlychild == 0 ~ "Opt-out",
    onlychild == 1 ~ "Only child" ,
    onlychild == 2 ~ "has siblings"
  ))%>%
  mutate(onlychild_label = factor(onlychild_label, levels = c("Opt-out","Only child" , "has siblings")),
         gender="male")



#salary 
male_isalary<-sim_results_male$isalary%>%
  #not include scenario 3 
  select(-senario3)%>%
  gather(2:4, key="senarios", value="percent")%>%
  group_by(isalary)%>%
  mutate(senarios_ordered=factor(senarios,
                                 levels = senarios[order(percent)]))%>%
  ungroup()%>%
  mutate(isalary_label = case_when(
    isalary == 0 ~ "Opt-out",
    isalary == 0.5 ~ "0.5k",
    isalary == 1 ~ "10k",
    isalary == 1.5 ~ "10.5k",
    isalary == 2 ~ "20k"
    
  ))%>%
  mutate(isalary_label = factor(isalary_label, levels = c("Opt-out","0.5k","10k","10.5k","20k")),
         gender="male")


#merge together 
edu_both_sim<-rbind(female_edu,male_edu)%>%
  mutate(percent=if_else(gender == "male", -1*percent, percent))

iage_both_sim<-rbind(female_iage,male_iage)%>%
  mutate(percent=if_else(gender == "male", -1*percent, percent))

danwei_both_sim<-rbind(female_danwei,male_danwei)%>%
  mutate(percent=if_else(gender == "male", -1*percent, percent))

isalary_both_sim<-rbind(female_isalary,male_isalary)%>%
  mutate(percent=if_else(gender == "male", -1*percent, percent))

onlychild_both_sim<-rbind(female_onlychild,male_onlychild)%>%
  mutate(percent=if_else(gender == "male", -1*percent, percent))

housing_both_sim<-rbind(female_housing,male_housing)%>%
  mutate(percent=if_else(gender == "male", -1*percent, percent))

beautyc_both_sim<-rbind(female_beautyc,male_beautyc)%>%
  mutate(percent=if_else(gender == "male", -1*percent, percent))


#export
sim <- list(
  "Education" = edu_both_sim,
  "Age" = iage_both_sim,
  "Work Unit" = danwei_both_sim,
  "Salary" = isalary_both_sim,
  "Only Child" = onlychild_both_sim,
  "Housing" = housing_both_sim,
  "Beauty" = beautyc_both_sim
)


write_xlsx(sim, path = "sim_results.xlsx")




#--------------------------
#graph both 
edu_both_sim_graph <- ggplot(edu_both_sim, aes(y = edu_label, x = percent, 
                                               color = senarios_ordered, linetype = gender,
                                               group = interaction(senarios_ordered, edu_label))) +
  geom_linerange(aes(xmin = 0, xmax = percent),  
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values = sim_colors) +
  # Show absolute values (positive) for labels
  scale_x_continuous(labels = function(x) paste0(abs(x)*100, "%")) +
  labs(y = "", x = "Percentage", color = "Scenarios",
       title = "Education") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom")

ggsave("edu_both_sim_graph.png", plot=edu_both_sim_graph )



iage_both_sim_graph <- ggplot(iage_both_sim, aes(y = iage_label, x = percent, 
                                               color = senarios_ordered, linetype = gender,
                                               group = interaction(senarios_ordered, iage_label))) +
  geom_linerange(aes(xmin = 0, xmax = percent),  
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values = sim_colors) +
  # Show absolute values (positive) for labels
  scale_x_continuous(labels = function(x) paste0(abs(x)*100, "%")) +
  labs(y = "", x = "Percentage", color = "Scenarios",
       title = "Age") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom")

ggsave("iage_both_sim_graph.png", plot=iage_both_sim_graph )


danwei_both_sim_graph <- ggplot(danwei_both_sim, aes(y = danwei_label, x = percent, 
                                                 color = senarios_ordered, linetype = gender,
                                                 group = interaction(senarios_ordered, danwei_label))) +
  geom_linerange(aes(xmin = 0, xmax = percent),  
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values = sim_colors) +
  # Show absolute values (positive) for labels
  scale_x_continuous(labels = function(x) paste0(abs(x)*100, "%")) +
  labs(y = "", x = "Percentage", color = "Scenarios",
       title = "Job sector") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom")



isalary_both_sim_graph <- ggplot(isalary_both_sim, aes(y = isalary_label, x = percent, 
                                                     color = senarios_ordered, linetype = gender,
                                                     group = interaction(senarios_ordered, isalary_label))) +
  geom_linerange(aes(xmin = 0, xmax = percent),  
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values = sim_colors) +
  # Show absolute values (positive) for labels
  scale_x_continuous(labels = function(x) paste0(abs(x)*100, "%")) +
  labs(y = "", x = "Percentage", color = "Scenarios",
       title = "Salary") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom")


onlychild_both_sim_graph <- ggplot(onlychild_both_sim, aes(y = onlychild_label, x = percent, 
                                                       color = senarios_ordered, linetype = gender,
                                                       group = interaction(senarios_ordered, onlychild_label))) +
  geom_linerange(aes(xmin = 0, xmax = percent),  
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values = sim_colors) +
  # Show absolute values (positive) for labels
  scale_x_continuous(labels = function(x) paste0(abs(x)*100, "%")) +
  labs(y = "", x = "Percentage", color = "Scenarios",
       title = "Sibship status") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom")



housing_both_sim_graph <- ggplot(housing_both_sim, aes(y = housing_label, x = percent, 
                                                           color = senarios_ordered, linetype = gender,
                                                           group = interaction(senarios_ordered, housing_label))) +
  geom_linerange(aes(xmin = 0, xmax = percent),  
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values = sim_colors) +
  # Show absolute values (positive) for labels
  scale_x_continuous(labels = function(x) paste0(abs(x)*100, "%")) +
  labs(y = "", x = "Percentage", color = "Scenarios",
       title = "Family Property") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom")



beautyc_both_sim_graph <- ggplot(beautyc_both_sim, aes(y = beautyc_label, x = percent, 
                                                       color = senarios_ordered, linetype = gender,
                                                       group = interaction(senarios_ordered, beautyc_label))) +
  geom_linerange(aes(xmin = 0, xmax = percent),  
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values = sim_colors) +
  # Show absolute values (positive) for labels
  scale_x_continuous(labels = function(x) paste0(abs(x)*100, "%")) +
  labs(y = "", x = "Percentage", color = "Scenarios",
       title = "Physical attractiveness") +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "bottom")




plot_grid_both_sim <- (iage_both_sim_graph + edu_both_sim_graph + danwei_both_sim_graph + 
                           isalary_both_sim_graph + onlychild_both_sim_graph + housing_both_sim_graph + 
                           beautyc_both_sim_graph + 
                           plot_layout(ncol = 3, guides = "collect")) &
  theme(legend.position = "bottom")

ggsave("plot_grid_both_sim.png", plot=plot_grid_both_sim )



#==========================================================================

#graph male only 

sim_colors<-c("#0039A6","#374057", "#97CAEB" ,   "#CC0000")

male_edu_sim<-ggplot(male_edu, aes(y = edu_label, x = percent, color = senarios_ordered)) +
  geom_linerange(aes(xmin = 0, xmax = percent), 
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values=sim_colors)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "",  x = "Percentage", color = "Scenarios",
       title = "Education") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "bottom")




male_iage_sim<-ggplot(male_iage, aes(y = iage_label, x = percent, color = senarios_ordered)) +
  geom_linerange(aes(xmin = 0, xmax = percent), 
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values=sim_colors)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "",  x = "Percentage", color = "Scenarios",
       title = "Age") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "bottom")


male_beautyc_sim<-ggplot(male_beautyc, aes(y = beautyc_label, x = percent, color = senarios_ordered)) +
  geom_linerange(aes(xmin = 0, xmax = percent), 
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values=sim_colors)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "",  x = "Percentage", color = "Scenarios",
       title = "Physical attractivenes") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "bottom")


#housing
male_housing_sim<-ggplot(male_housing, aes(y = housing_label, x = percent, color = senarios_ordered)) +
  geom_linerange(aes(xmin = 0, xmax = percent), 
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values=sim_colors)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "",  x = "Percentage", color = "Scenarios",
       title = "Family Housing") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "bottom")


#danwei : bit strange 
male_danwei_sim<-ggplot(male_danwei, aes(y = danwei_label, x = percent, color = senarios_ordered)) +
  geom_linerange(aes(xmin = 0, xmax = percent), 
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values=sim_colors)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "",  x = "Percentage", color = "Scenarios",
       title = "Work Sector") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "bottom")


#onlychild
male_onlychild_sim<-ggplot(male_onlychild, aes(y = onlychild_label, x = percent, color = senarios_ordered)) +
  geom_linerange(aes(xmin = 0, xmax = percent), 
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values=sim_colors)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "",  x = "Percentage", color = "Scenarios",
       title = "Sibship status") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "bottom")

#salary 
male_salary_sim<-ggplot(male_isalary, aes(y = isalary_label, x = percent, color = senarios_ordered)) +
  geom_linerange(aes(xmin = 0, xmax = percent), 
                 position = position_dodge(width = 0.7),
                 linewidth = 1.2) +
  geom_point(position = position_dodge(width = 0.7), size = 3.5) +
  scale_color_manual(values=sim_colors)+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "",  x = "Percentage", color = "Scenarios",
       title = "Salary ") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "bottom")



plot_grid_male_sim <- (male_iage_sim + male_edu_sim + male_danwei_sim + 
                           male_salary_sim + male_onlychild_sim + male_housing_sim + 
                           male_beautyc_sim + 
                           plot_layout(ncol = 3, guides = "collect")) &
  theme(legend.position = "bottom")

ggsave("plot_grid_male_sim.png", plot=plot_grid_male_sim )

#=============================================================================
#=============================================================================


#bring structure. use marriage aspiration as opt-out 
# supply  Vs predicted distribution 

#female 
load("census_edu_male.RData")
cgss_pool<-read_dta("marriagepool_cgss.dta")
css_pool<-read_dta("marriagepool_css.dta")


female_edu_senario2<-female_edu%>%
  filter(senarios=="senario2")%>%
  select(edu,percent)%>%
  mutate(type = if_else(edu == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


edu<-rbind(female_edu_senario2, census_edu_male)%>%
  # Ensure age categories are properly ordered
  mutate(
    edu = factor(edu, levels = c(0, 1, 2, 3, 4),
                 labels = c("Opt Out", "2yr college", "BA", "Master's", "Ph.D." ))
  )


#calculate difference 
edu_female_dif<-female_edu_senario2%>%
  filter(edu>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(census_edu_male, by="edu")%>%
  mutate(dif=share-demand_share)



#graph 
edu_female<- ggplot(edu, aes(x = edu, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "census" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "census" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    ))+
  # Add black outline for census bars only
  geom_col(
    data = subset(edu, type == "census"),
    aes(x = edu, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Education",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))

print(edu_female)

#-------------------------------------------------

#age 
#census age 
load("census_agegp_male.RData")

female_iage_senario2<-female_iage%>%
  filter(senarios=="senario2")%>%
  select(iage,percent)%>%
  mutate(type = if_else(iage == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent,
         age_cat=iage)


age<-rbind(census_agegp_male, female_iage_senario2)%>%
  mutate(
    age_cat = factor(age_cat, levels = c(0, 1, 2, 3, 4),
                     labels = c("Opt Out", "<=25", "25-29", "30-34", "35+" ))) 



#calculate difference 
age_female_dif<-female_iage_senario2%>%
  filter(age_cat>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(census_agegp_male, by="age_cat")%>%
  mutate(dif=share-demand_share)%>%
  select(age_cat,dif)



age_female<- ggplot(age, aes(x = age_cat, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "census" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "census" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(age, type == "census"),
    aes(x = age_cat, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Age",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))

print(age_female)

#---------------------------------------------------
#danwei : 1 gover 2.company (public proviate combined ) 3. freelance 4. ag employment or unemployed 


female_danwei_senario2<-female_danwei%>%
  filter(senarios=="senario2")%>%
  select(danwei,percent)%>%
  rename(share=percent)%>%
  mutate(occup= case_when(
    danwei ==0 ~ 0 ,
    danwei ==1 ~ 1 ,
    danwei ==2 ~ 2 ,
    danwei ==3 ~ 2 ,
    danwei ==4 ~ 3 ))%>%
  group_by(occup)%>%
  summarise(share=sum(share))%>%
  mutate(type = if_else(occup == 0, "estimated_optout", "estimated"))

#cgss : female danwei 
danwei_cgss<-cgss_pool%>%
  filter(female==0)%>% #male only 
  group_by(danwei)%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="cgss", 
         occup=danwei)%>%
  select(occup, type, share)

danwei <-rbind(female_danwei_senario2, danwei_cgss)%>%  
  mutate(
    occup = factor(occup, levels = c(0, 1, 2, 3, 4),
                   labels = c("Opt Out", "Gov/Inst", "Company", "Freelance", "Ag/unemployed" ))
  )


#calculate difference 
danwei_female_dif<-female_danwei_senario2%>%
  filter(occup>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(danwei_cgss, by="occup")%>%
  mutate(dif=share-demand_share)%>%
  select(occup,dif)



danwei_female<- ggplot(danwei, aes(x = occup, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    ))+
  # Add black outline for cgss bars only
  geom_col(
    data = subset(danwei, type == "cgss"),
    aes(x = occup, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Occupation sector",
    y = "Share ",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


print(danwei_female)


#---------------------------------------------
#only child = 1 , 0 otherwise 
css_pool<-read_dta("marriagepool_css.dta")

css_only<-css_pool%>%
  filter(female ==0) %>%  #male only
  group_by(only_child)%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="css",
         onlychild= 2-only_child
  )%>%
  select(-only_child, -w_n)

female_onlychild_senario2<-female_onlychild%>%
  filter(senarios=="senario2")%>%
  select(onlychild,percent)%>%
  mutate(type = if_else(onlychild == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


onlychild<-rbind(female_onlychild_senario2, css_only)%>%
  mutate(onlychild=factor(onlychild, levels=c(0,1,2),
                          labels=c("opt out", "only child", "has siblings")))


#calculate difference 
onlychild_female_dif<-female_onlychild_senario2%>%
  filter(onlychild>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(css_only, by="onlychild")%>%
  mutate(dif=share-demand_share)%>%
  select(onlychild,dif)




onlychild_female<- ggplot(onlychild, aes(x = onlychild, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "css" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "css" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(onlychild, type == "css"),
    aes(x = onlychild, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Sibship ",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


#--------------------------------------------
#housing 

female_housing_senario2<-female_housing%>%
  filter(senarios=="senario2")%>%
  select(housing,percent)%>%
  mutate(type = if_else(housing == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


housing_cgss<-cgss_pool%>%
  filter(female==0)%>% #male only 
  group_by(family_housing )%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="cgss",
         housing = family_housing + 1)%>%
  select(housing, type, share)



housing<-rbind(female_housing_senario2, housing_cgss)%>%
  mutate(housing=factor(housing, levels=c(0,1,2,3),
                        labels=c("opt out", "no housing", "one apt" , "two or more apts")))


#calculate difference 
housing_female_dif<-female_housing_senario2%>%
  filter(housing>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(housing_cgss, by="housing")%>%
  mutate(dif=share-demand_share)%>%
  select(housing,dif)

view(housing_female_dif)


housing_female<- ggplot(housing, aes(x = housing, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(housing, type == "cgss"),
    aes(x = housing, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Family Property",
    y = "Share ",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


#-----------------------------------------

# beauty 
female_beautyc

female_beautyc_senario2<-female_beautyc%>%
  filter(senarios=="senario2")%>%
  select(beautyc,percent)%>%
  mutate(type = if_else(beautyc == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)%>%
  mutate(beautyc=factor(beautyc, levels=c(0,4,6,8,10),
                        labels=c("opt out", "level 4", "level 6" , "level 8" , "level 10")))




color_mapping <- c(
  "estimated_optout"    = "#d4d4d4",           # No color (transparent)
  "estimated"  = alpha("steelblue", 0.5)     # Steel blue for education
)


beautyc_female<- ggplot(female_beautyc_senario2, aes(x = beautyc  , y = share,fill = type)) +
  geom_col(width=0.5)+
  scale_fill_manual(
    values=color_mapping,
    guide= "none"
  )+
  labs(
    title = "Physical Attractiveness",
    x = "",
    y = "share",
  ) +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Add space for labels 

print(beautyc_female)


#--------------------------------------------------------------------------
#salary 


female_isalary_senario2<-female_isalary%>%
  filter(senarios=="senario2")%>%
  select(isalary,percent)%>%
  rename(share=percent)%>%
  mutate(type= if_else(isalary==0,"estimated_optout", "estimated" ),
         salary= case_when(
           isalary ==0 ~ 0,
           isalary ==0.5 ~ 2,  #keep consistent with cgss
           isalary ==1  ~ 3 ,
           isalary ==1.5 ~ 4,
           isalary ==2 ~ 5 
         ))%>%
  select(share, type, salary)


salary_cgss<- cgss_pool%>%
  filter(female==0)%>%
  group_by(monincome_cat)%>%
  summarise(w_n = sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type = "cgss",
         salary = monincome_cat)%>%
  select(share, salary, type)


salary<-rbind(salary_cgss,female_isalary_senario2)%>%
  mutate(salary=factor(salary, levels= c(0, 1, 2, 3, 4, 5),
                       labels=c("opt out",
                                "0-5k",
                                "5k-10k",
                                "10k-15k",
                                "15k-20k",
                                "20k+")))



#calculate difference 
salary_female_dif<-female_isalary_senario2%>%
  filter(salary>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(salary_cgss, by="salary")%>%
  mutate(dif=share-demand_share)%>%
  select(salary,dif)


view(salary_female_dif)

salary_female<- ggplot(salary, aes(x = salary, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(salary, type == "cgss"),
    aes(x = salary, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Monthly salary",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))



plot_grid_female <- (age_female + edu_female + danwei_female + 
                       salary_female + onlychild_female + housing_female + 
                       beautyc_female + 
                       plot_layout(ncol = 3, guides = "collect")) &
  theme(legend.position = "bottom")

ggsave("plot_grid_female_senario2.png", plot=plot_grid_female )




#==========================================================
#==========================================================
# do the same thing for men 
# !! make sure the run the entire chunck, bx have to restrict cgss & css to female singles only 

#male 
cgss_pool<-read_dta("marriagepool_cgss.dta")
load("census_edu_female.RData") #load female distribution 


male_edu_senario2<-male_edu%>%
  filter(senarios=="senario2")%>%
  select(edu,percent)%>%
  mutate(type = if_else(edu == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


edu<-rbind(male_edu_senario2, census_edu_female)%>%
  # Ensure age categories are properly ordered
  mutate(
    edu = factor(edu, levels = c(0, 1, 2, 3, 4),
                 labels = c("Opt Out", "2yr college", "BA", "Master's", "Ph.D." ))
  )


edu_male<- ggplot(edu, aes(x = edu, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "census" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "census" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    ))+
  # Add black outline for census bars only
  geom_col(
    data = subset(edu, type == "census"),
    aes(x = edu, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Education",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))

print(edu_male)

#-------------------------------------------------

#age 
#census age 
load("census_agegp_female.RData") #load female data for comparison 


male_iage_senario2<-male_iage%>%
  filter(senarios=="senario2")%>%
  select(iage,percent)%>%
  mutate(type = if_else(iage == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent,
         age_cat=iage)



age<-rbind(census_agegp_female, male_iage_senario2)%>%
  mutate(
    age_cat = factor(age_cat, levels = c(0, 1, 2, 3, 4),
                     labels = c("Opt Out", "<=25", "25-29", "30-34", "35+" ))) 


age_male<- ggplot(age, aes(x = age_cat, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "census" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "census" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(age, type == "census"),
    aes(x = age_cat, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Age",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))

print(age_male)

#---------------------------------------------------
#danwei : 1 gover 2.company (public proviate combined ) 3. freelance 4. ag employment or unemployed 


male_danwei_senario2<-male_danwei%>%
  filter(senarios=="senario2")%>%
  select(danwei,percent)%>%
  rename(share=percent)%>%
  mutate(occup= case_when(
    danwei ==0 ~ 0 ,
    danwei ==1 ~ 1 ,
    danwei ==2 ~ 2 ,
    danwei ==3 ~ 2 ,
    danwei ==4 ~ 3 ))%>%
  group_by(occup)%>%
  summarise(share=sum(share))%>%
  mutate(type = if_else(occup == 0, "estimated_optout", "estimated"))

#cgss : female danwei 
danwei_cgss_male<-cgss_pool%>%
  filter(female==1)%>% #female
  group_by(danwei)%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="cgss", 
         occup=danwei)%>%
  select(occup, type, share)

danwei <-rbind(male_danwei_senario2, danwei_cgss)%>%  
  mutate(
    occup = factor(occup, levels = c(0, 1, 2, 3, 4),
                   labels = c("Opt Out", "Gov/Inst", "Company", "Freelance", "Ag/unemployed" ))
  )



danwei_male<- ggplot(danwei, aes(x = occup, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    ))+
  # Add black outline for cgss bars only
  geom_col(
    data = subset(danwei, type == "cgss"),
    aes(x = occup, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Occupation sector",
    y = "Share ",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


print(danwei_male)


#---------------------------------------------
#only child 
css_pool<-read_dta("marriagepool_css.dta")

css_only<-css_pool%>%
  filter(female ==1) %>%  #female only
  group_by(only_child)%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="css",
         onlychild= 2-only_child
  )%>%
  select(-only_child, -w_n)

male_onlychild_senario2<-male_onlychild%>%
  filter(senarios=="senario2")%>%
  select(onlychild,percent)%>%
  mutate(type = if_else(onlychild == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


onlychild<-rbind(male_onlychild_senario2, css_only)%>%
  mutate(onlychild=factor(onlychild, levels=c(0,1,2),
                          labels=c("opt out", "only child", "has siblings")))



onlychild_male<- ggplot(onlychild, aes(x = onlychild, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "css" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "css" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(onlychild, type == "css"),
    aes(x = onlychild, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Sibship ",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


#--------------------------------------------
#housing 

male_housing_senario2<-male_housing%>%
  filter(senarios=="senario2")%>%
  select(housing,percent)%>%
  mutate(type = if_else(housing == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


housing_cgss<-cgss_pool%>%
  filter(female==1)%>% #female only 
  group_by(family_housing )%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="cgss",
         housing = family_housing + 1)%>%
  select(housing, type, share)



housing<-rbind(male_housing_senario2, housing_cgss)%>%
  mutate(housing=factor(housing, levels=c(0,1,2,3),
                        labels=c("opt out", "no housing", "one apt" , "two or more apts")))




housing_male<- ggplot(housing, aes(x = housing, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(housing, type == "cgss"),
    aes(x = housing, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Family Property",
    y = "Share ",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


#-----------------------------------------

# beauty 


male_beautyc_senario2<-male_beautyc%>%
  filter(senarios=="senario2")%>%
  select(beautyc,percent)%>%
  mutate(type = if_else(beautyc == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)%>%
  mutate(beautyc=factor(beautyc, levels=c(0,4,6,8,10),
                        labels=c("opt out", "level 4", "level 6" , "level 8" , "level 10")))




color_mapping <- c(
  "estimated_optout"    = "#d4d4d4",           # No color (transparent)
  "estimated"  = alpha("firebrick4", 0.5)     # Steel blue for education
)


beautyc_male<- ggplot(male_beautyc_senario2, aes(x = beautyc  , y = share,fill = type)) +
  geom_col(width=0.5)+
  scale_fill_manual(
    values=color_mapping,
    guide= "none"
  )+
  labs(
    title = "Physical Attractiveness",
    x = "",
    y = "share",
  ) +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Add space for labels 

print(beautyc_male)


#--------------------------------------------------------------------------
#salary 


male_isalary_senario2<-male_isalary%>%
  filter(senarios=="senario2")%>%
  select(isalary,percent)%>%
  rename(share=percent)%>%
  mutate(type= if_else(isalary==0,"estimated_optout", "estimated" ),
         salary= case_when(
           isalary ==0 ~ 0,
           isalary ==0.5 ~ 2,  #keep consistent with cgss
           isalary ==1  ~ 3 ,
           isalary ==1.5 ~ 4,
           isalary ==2 ~ 5 
         ))%>%
  select(share, type, salary)


salary_cgss<- cgss_pool%>%
  filter(female==1)%>% #female only 
  group_by(monincome_cat)%>%
  summarise(w_n = sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type = "cgss",
         salary = monincome_cat)%>%
  select(share, salary, type)


salary<-rbind(salary_cgss,male_isalary_senario2)%>%
  mutate(salary=factor(salary, levels= c(0, 1, 2, 3, 4, 5),
                       labels=c("opt out",
                                "0-5k",
                                "5k-10k",
                                "10k-15k",
                                "15k-20k",
                                "20k+")))


salary_male<- ggplot(salary, aes(x = salary, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(salary, type == "cgss"),
    aes(x = salary, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Monthly salary",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))



plot_grid_male <- (age_male + edu_male + danwei_male + 
                       salary_male + onlychild_male + housing_male + 
                       beautyc_male + 
                       plot_layout(ncol = 3, guides = "collect")) &
  theme(legend.position = "bottom")

ggsave("plot_grid_male_senario2.png", plot=plot_grid_male )


#-----------------
#calculate difference between supply and demand 
edu_male_dif<-male_edu_senario2%>%
  filter(edu>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(census_edu_male, by="edu")%>%
  mutate(dif=share-demand_share)%>%
  select(edu, dif)


age_male_dif<-male_iage_senario2%>%
  filter(age_cat>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(census_agegp_male, by="age_cat")%>%
  mutate(dif=share-demand_share)%>%
  select(age_cat,dif)


danwei_male_dif<-male_danwei_senario2%>%
  filter(occup>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(danwei_cgss, by="occup")%>%
  mutate(dif=share-demand_share)%>%
  select(occup,dif)

view(danwei_male_dif)

onlychild_male_dif<-male_onlychild_senario2%>%
  filter(onlychild>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(css_only, by="onlychild")%>%
  mutate(dif=share-demand_share)%>%
  select(onlychild,dif)

view(onlychild_male_dif)

housing_male_dif<-male_housing_senario2%>%
  filter(housing>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(housing_cgss, by="housing")%>%
  mutate(dif=share-demand_share)%>%
  select(housing,dif)


view(housing_male_dif)


salary_male_dif<-male_isalary_senario2%>%
  filter(salary>0)%>%
  rename(demand_share=share, demand_type=type)%>%
  left_join(salary_cgss, by="salary")%>%
  mutate(dif=share-demand_share)%>%
  select(salary,dif)

view(salary_male_dif)

#======================================================
#======================================================
#baseline scenario 


#female 
load("census_edu_male.RData")
cgss_pool<-read_dta("marriagepool_cgss.dta")
css_pool<-read_dta("marriagepool_css.dta")


female_edu_original<-female_edu%>%
  filter(senarios=="original")%>%
  select(edu,percent)%>%
  mutate(type = if_else(edu == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


edu<-rbind(female_edu_original, census_edu_male)%>%
  # Ensure age categories are properly ordered
  mutate(
    edu = factor(edu, levels = c(0, 1, 2, 3, 4),
                 labels = c("Opt Out", "2yr college", "BA", "Master's", "Ph.D." ))
  )


edu_female<- ggplot(edu, aes(x = edu, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "census" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "census" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    ))+
  # Add black outline for census bars only
  geom_col(
    data = subset(edu, type == "census"),
    aes(x = edu, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Education",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))

print(edu_female)

#-------------------------------------------------

#age 
#census age 
load("census_agegp_male.RData")

female_iage_original<-female_iage%>%
  filter(senarios=="original")%>%
  select(iage,percent)%>%
  mutate(type = if_else(iage == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent,
         age_cat=iage)


age<-rbind(census_agegp_male, female_iage_original)%>%
  mutate(
    age_cat = factor(age_cat, levels = c(0, 1, 2, 3, 4),
                     labels = c("Opt Out", "<=25", "25-29", "30-34", "35+" ))) 


age_female<- ggplot(age, aes(x = age_cat, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "census" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "census" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(age, type == "census"),
    aes(x = age_cat, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Age",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))

print(age_female)

#---------------------------------------------------
#danwei : 1 gover 2.company (public proviate combined ) 3. freelance 4. ag employment or unemployed 


female_danwei_original<-female_danwei%>%
  filter(senarios=="original")%>%
  select(danwei,percent)%>%
  rename(share=percent)%>%
  mutate(occup= case_when(
    danwei ==0 ~ 0 ,
    danwei ==1 ~ 1 ,
    danwei ==2 ~ 2 ,
    danwei ==3 ~ 2 ,
    danwei ==4 ~ 3 ))%>%
  group_by(occup)%>%
  summarise(share=sum(share))%>%
  mutate(type = if_else(occup == 0, "estimated_optout", "estimated"))

#cgss : female danwei 
danwei_cgss<-cgss_pool%>%
  filter(female==0)%>% #male only 
  group_by(danwei)%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="cgss", 
         occup=danwei)%>%
  select(occup, type, share)

danwei <-rbind(female_danwei_original, danwei_cgss)%>%  
  mutate(
    occup = factor(occup, levels = c(0, 1, 2, 3, 4),
                   labels = c("Opt Out", "Gov/Inst", "Company", "Freelance", "Ag/unemployed" ))
  )



danwei_female<- ggplot(danwei, aes(x = occup, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    ))+
  # Add black outline for cgss bars only
  geom_col(
    data = subset(danwei, type == "cgss"),
    aes(x = occup, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Occupation sector",
    y = "Share ",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


print(danwei_female)


#---------------------------------------------
#only child 
css_pool<-read_dta("marriagepool_css.dta")

css_only<-css_pool%>%
  filter(female ==0) %>%  #male only
  group_by(only_child)%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="css",
         onlychild= 2-only_child
  )%>%
  select(-only_child, -w_n)

female_onlychild_original<-female_onlychild%>%
  filter(senarios=="original")%>%
  select(onlychild,percent)%>%
  mutate(type = if_else(onlychild == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


onlychild<-rbind(female_onlychild_original, css_only)%>%
  mutate(onlychild=factor(onlychild, levels=c(0,1,2),
                          labels=c("opt out", "only child", "has siblings")))



onlychild_female<- ggplot(onlychild, aes(x = onlychild, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "css" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "css" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(onlychild, type == "css"),
    aes(x = onlychild, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Sibship ",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


#--------------------------------------------
#housing 

female_housing_original<-female_housing%>%
  filter(senarios=="original")%>%
  select(housing,percent)%>%
  mutate(type = if_else(housing == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


housing_cgss<-cgss_pool%>%
  filter(female==0)%>% #male only 
  group_by(family_housing )%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="cgss",
         housing = family_housing + 1)%>%
  select(housing, type, share)



housing<-rbind(female_housing_original, housing_cgss)%>%
  mutate(housing=factor(housing, levels=c(0,1,2,3),
                        labels=c("opt out", "no housing", "one apt" , "two or more apts")))




housing_female<- ggplot(housing, aes(x = housing, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(housing, type == "cgss"),
    aes(x = housing, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Family Property",
    y = "Share ",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


#-----------------------------------------

# beauty 
female_beautyc

female_beautyc_original<-female_beautyc%>%
  filter(senarios=="original")%>%
  select(beautyc,percent)%>%
  mutate(type = if_else(beautyc == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)%>%
  mutate(beautyc=factor(beautyc, levels=c(0,4,6,8,10),
                        labels=c("opt out", "level 4", "level 6" , "level 8" , "level 10")))




color_mapping <- c(
  "estimated_optout"    = "#d4d4d4",           # No color (transparent)
  "estimated"  = alpha("steelblue", 0.5)     # Steel blue for education
)


beautyc_female<- ggplot(female_beautyc_original, aes(x = beautyc  , y = share,fill = type)) +
  geom_col(width=0.5)+
  scale_fill_manual(
    values=color_mapping,
    guide= "none"
  )+
  labs(
    title = "Physical Attractiveness",
    x = "",
    y = "share",
  ) +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Add space for labels 

print(beautyc_female)


#--------------------------------------------------------------------------
#salary 


female_isalary_original<-female_isalary%>%
  filter(senarios=="original")%>%
  select(isalary,percent)%>%
  rename(share=percent)%>%
  mutate(type= if_else(isalary==0,"estimated_optout", "estimated" ),
         salary= case_when(
           isalary ==0 ~ 0,
           isalary ==0.5 ~ 2,  #keep consistent with cgss
           isalary ==1  ~ 3 ,
           isalary ==1.5 ~ 4,
           isalary ==2 ~ 5 
         ))%>%
  select(share, type, salary)


salary_cgss<- cgss_pool%>%
  filter(female==0)%>%
  group_by(monincome_cat)%>%
  summarise(w_n = sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type = "cgss",
         salary = monincome_cat)%>%
  select(share, salary, type)


salary<-rbind(salary_cgss,female_isalary_original)%>%
  mutate(salary=factor(salary, levels= c(0, 1, 2, 3, 4, 5),
                       labels=c("opt out",
                                "0-5k",
                                "5k-10k",
                                "10k-15k",
                                "15k-20k",
                                "20k+")))


salary_female<- ggplot(salary, aes(x = salary, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("steelblue", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(salary, type == "cgss"),
    aes(x = salary, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Monthly salary",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))



plot_grid_female <- (age_female + edu_female + danwei_female + 
                       salary_female + onlychild_female + housing_female + 
                       beautyc_female + 
                       plot_layout(ncol = 3, guides = "collect")) &
  theme(legend.position = "bottom")

ggsave("plot_grid_female_original.png", plot=plot_grid_female )




#==========================================================
#==========================================================
# do the same thing for men 

#male 
cgss_pool<-read_dta("marriagepool_cgss.dta")
load("census_edu_female.RData")



male_edu_original<-male_edu%>%
  filter(senarios=="original")%>%
  select(edu,percent)%>%
  mutate(type = if_else(edu == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


edu<-rbind(male_edu_original, census_edu_female)%>%
  # Ensure age categories are properly ordered
  mutate(
    edu = factor(edu, levels = c(0, 1, 2, 3, 4),
                 labels = c("Opt Out", "2yr college", "BA", "Master's", "Ph.D." ))
  )


edu_male<- ggplot(edu, aes(x = edu, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "census" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "census" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    ))+
  # Add black outline for census bars only
  geom_col(
    data = subset(edu, type == "census"),
    aes(x = edu, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Education",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))

print(edu_male)

#-------------------------------------------------

#age 
#census age 
load("census_agegp_female.RData") #load female data for comparison 


male_iage_original<-male_iage%>%
  filter(senarios=="original")%>%
  select(iage,percent)%>%
  mutate(type = if_else(iage == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent,
         age_cat=iage)



age<-rbind(census_agegp_female, male_iage_original)%>%
  mutate(
    age_cat = factor(age_cat, levels = c(0, 1, 2, 3, 4),
                     labels = c("Opt Out", "<=25", "25-29", "30-34", "35+" ))) 


age_male<- ggplot(age, aes(x = age_cat, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "census" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "census" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(age, type == "census"),
    aes(x = age_cat, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Age",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))

print(age_male)

#---------------------------------------------------
#danwei : 1 gover 2.company (public proviate combined ) 3. freelance 4. ag employment or unemployed 


male_danwei_original<-male_danwei%>%
  filter(senarios=="original")%>%
  select(danwei,percent)%>%
  rename(share=percent)%>%
  mutate(occup= case_when(
    danwei ==0 ~ 0 ,
    danwei ==1 ~ 1 ,
    danwei ==2 ~ 2 ,
    danwei ==3 ~ 2 ,
    danwei ==4 ~ 3 ))%>%
  group_by(occup)%>%
  summarise(share=sum(share))%>%
  mutate(type = if_else(occup == 0, "estimated_optout", "estimated"))

#cgss : female danwei 
danwei_cgss_male<-cgss_pool%>%
  filter(female==1)%>% #female
  group_by(danwei)%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="cgss", 
         occup=danwei)%>%
  select(occup, type, share)

danwei <-rbind(male_danwei_original, danwei_cgss)%>%  
  mutate(
    occup = factor(occup, levels = c(0, 1, 2, 3, 4),
                   labels = c("Opt Out", "Gov/Inst", "Company", "Freelance", "Ag/unemployed" ))
  )



danwei_male<- ggplot(danwei, aes(x = occup, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    ))+
  # Add black outline for cgss bars only
  geom_col(
    data = subset(danwei, type == "cgss"),
    aes(x = occup, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Occupation sector",
    y = "Share ",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


print(danwei_male)


#---------------------------------------------
#only child 
css_pool<-read_dta("marriagepool_css.dta")

css_only<-css_pool%>%
  filter(female ==1) %>%  #female only
  group_by(only_child)%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="css",
         onlychild= 2-only_child
  )%>%
  select(-only_child, -w_n)

male_onlychild_original<-male_onlychild%>%
  filter(senarios=="original")%>%
  select(onlychild,percent)%>%
  mutate(type = if_else(onlychild == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


onlychild<-rbind(male_onlychild_original, css_only)%>%
  mutate(onlychild=factor(onlychild, levels=c(0,1,2),
                          labels=c("opt out", "only child", "has siblings")))



onlychild_male<- ggplot(onlychild, aes(x = onlychild, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "css" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "css" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(onlychild, type == "css"),
    aes(x = onlychild, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Sibship ",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


#--------------------------------------------
#housing 

male_housing_original<-male_housing%>%
  filter(senarios=="original")%>%
  select(housing,percent)%>%
  mutate(type = if_else(housing == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)


housing_cgss<-cgss_pool%>%
  filter(female==1)%>% #female only 
  group_by(family_housing )%>%
  summarise(w_n=sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type="cgss",
         housing = family_housing + 1)%>%
  select(housing, type, share)



housing<-rbind(male_housing_original, housing_cgss)%>%
  mutate(housing=factor(housing, levels=c(0,1,2,3),
                        labels=c("opt out", "no housing", "one apt" , "two or more apts")))




housing_male<- ggplot(housing, aes(x = housing, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(housing, type == "cgss"),
    aes(x = housing, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Family Property",
    y = "Share ",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    #axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))


#-----------------------------------------

# beauty 


male_beautyc_original<-male_beautyc%>%
  filter(senarios=="original")%>%
  select(beautyc,percent)%>%
  mutate(type = if_else(beautyc == 0, "estimated_optout", "estimated"))%>%
  rename(share=percent)%>%
  mutate(beautyc=factor(beautyc, levels=c(0,4,6,8,10),
                        labels=c("opt out", "level 4", "level 6" , "level 8" , "level 10")))




color_mapping <- c(
  "estimated_optout"    = "#d4d4d4",           # No color (transparent)
  "estimated"  = alpha("firebrick4", 0.5)     # Steel blue for education
)


beautyc_male<- ggplot(male_beautyc_original, aes(x = beautyc  , y = share,fill = type)) +
  geom_col(width=0.5)+
  scale_fill_manual(
    values=color_mapping,
    guide= "none"
  )+
  labs(
    title = "Physical Attractiveness",
    x = "",
    y = "share",
  ) +
  theme_minimal() +
  
  theme(
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Add space for labels 

print(beautyc_male)


#--------------------------------------------------------------------------
#salary 


male_isalary_original<-male_isalary%>%
  filter(senarios=="original")%>%
  select(isalary,percent)%>%
  rename(share=percent)%>%
  mutate(type= if_else(isalary==0,"estimated_optout", "estimated" ),
         salary= case_when(
           isalary ==0 ~ 0,
           isalary ==0.5 ~ 2,  #keep consistent with cgss
           isalary ==1  ~ 3 ,
           isalary ==1.5 ~ 4,
           isalary ==2 ~ 5 
         ))%>%
  select(share, type, salary)


salary_cgss<- cgss_pool%>%
  filter(female==1)%>% #female only 
  group_by(monincome_cat)%>%
  summarise(w_n = sum(weight))%>%
  mutate(share=w_n/sum(w_n),
         type = "cgss",
         salary = monincome_cat)%>%
  select(share, salary, type)


salary<-rbind(salary_cgss,male_isalary_original)%>%
  mutate(salary=factor(salary, levels= c(0, 1, 2, 3, 4, 5),
                       labels=c("opt out",
                                "0-5k",
                                "5k-10k",
                                "10k-15k",
                                "15k-20k",
                                "20k+")))


salary_male<- ggplot(salary, aes(x = salary, y = share, fill = type)) +
  geom_col(position = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "cgss" = "transparent",
      "estimated" = alpha("firebrick4", 0.5) ,
      "estimated_optout" = "#d4d4d4"
    ),  
    labels = c(
      "cgss" = "Single",
      "estimated" = "Estimated",
      "estimated_optout" = "Opt Out"
    )
  ) +
  # Add black outline for CGSS bars only
  geom_col(
    data = subset(salary, type == "cgss"),
    aes(x = salary, y = share),
    fill = "transparent",
    color = "black",
    size = 0.8,  # Outline thickness
    width = 0.7
  )+
  labs(
    title = "Monthly salary",
    y = "Share",
    x = "" ,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    # axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  guides(fill = guide_legend(
    override.aes = list(
      colour = c("black", "transparent", "transparent"),
      linetype = c(1, 0, 0),
      size = c(0.8, 0, 0)
    )
  ))



plot_grid_male <- (age_male + edu_male + danwei_male + 
                     salary_male + onlychild_male + housing_male + 
                     beautyc_male + 
                     plot_layout(ncol = 3, guides = "collect")) &
  theme(legend.position = "bottom")

ggsave("plot_grid_male_original.png", plot=plot_grid_male )

#end of graphing 
#=========================================================
#additional graphs
#compare age and gender distribution between CGSS and CSS 
css_pool<-read_dta("marriagepool_css.dta")

cgss_pool<-read_dta("marriagepool_cgss.dta")
load("census_edu_female.RData") #load female distribution 
load("census_edu_male.RData") #load male distribution 
load("census_agegp_female.RData")


css_agegp<-css_pool%>%
  group_by(age_cat, female)%>%
  summarise(n_w=sum(weight))%>%
  ungroup()%>%
  group_by(female)%>%
  mutate(agegp_share=n_w/sum(n_w),
         source="css")

cgss_agegp<-cgss_pool%>%
  group_by(age_cat, female)%>%
  summarise(n_w=sum(weight))%>%
  ungroup()%>%
  group_by(female)%>%
  mutate(agegp_share=n_w/sum(n_w),
         source="cgss")

compare_age<-rbind(cgss_agegp,css_agegp)

ggplot(compare_age, aes(x=age_cat,y=agegp_share, fill = source))+
  geom_col(position = "dodge") +
  facet_wrap(~ female) +
  labs(x = "Age Category", y = "Age Group Share") +
  theme_minimal()

census_agegp_female