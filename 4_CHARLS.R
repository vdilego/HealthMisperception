#-----------------------------------------------------------#
# Data prep for CHARLS   --------------------------------------#
# Author: Vanessa di Lego  #
#-----------------------------------------------------------#

library(purrr)
library(tidyverse)
library(labelled)
library(ipumsr)
library(haven)
library(dplyr)
library(splitstackshape)
library(survey)
library(here)
library(ggthemes)
library(AMR)
library(metan)
library(gtsummary)
library(corrplot)
library(likert)
library(flextable)
library(naniar)

#The China Health and Retirement Longitudinal Study (CHARLS) is a longitudinal study of 
#individuals over age 45 in China. Version D incorporates the latest released version of CHARLS data,
#and adds variables for Wave 4. It contains 25,586 observations or rows. 
#It is a Respondent-level file so each row represents a unique Respondent;
#The sample population was selected as part of a stratified, multistage probability design. 
#We will use Wave 3, for year 2014/2015, with the eligible interviews amounting to a sample size of 20,281.
# sample size 8,814, after excluding the cases for which biomarkers were not collected and wave 1,
# since there were problems with the question of walking 100m.



# Loading useful functions into environment
source(here("0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Manuscript","Figures")
dat.folder <- here("Manuscript","Data")
figs.app.folder <- here("Appendix","Figures")

# CHARLS

charls <- read_dta(here("Data","walk_charls.dta")) %>% 
  filter(!is.na(rwtrespb)) %>% 
  filter(rwtrespb>0) 


charls<-as_factor(charls)                         # transform all using as_factor from haven    
charls$walkcomp<-as.character(charls$walkcomp)
charls$walkaid<-as.character(charls$walkaid)
charls$wave<-as.character(charls$wave)
charls$wave<-as.numeric(charls$wave)

df1<-charls %>% 
  filter(!is.na(walkspeed1)) %>% 
  unlabelled() %>% 
  filter(ragey>=65 & wave%in%c(2,3))

df2<-cSplit(df1, splitCols = c(6:8), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  mutate(rwspeed1=as.numeric(rwspeed1),
         rwspeed2=as.numeric(rwspeed2),
         speed_avg=(rwspeed1+rwspeed2)/2,
         walk_speed=(2.5/speed_avg)) %>% 
  arrange(ID, wave) %>% 
  mutate(age_cat=cut(ragey, breaks = c(seq(65,80,5), 85, Inf),
                     labels = c("65-70","70-75", "75-80","80-85","85+"), right=F)) %>% 
  droplevels() #%>% 



# let´s have a look at the missing values. The doctor diagnosed disease is actually pretty robust,
# when thinking in terms of missing values. 

plot_missy<-df3 %>%
  # Select the survey items
  select(c("speed_avg",  "rwalk100a_1")) %>%
  # Create an UpSet plot
  gg_miss_upset(., nsets = 10 )




# check all variables  by gender:
#pdf(here(figs.app.folder,"missing_share_gender.pdf"))
gg_miss_var(df3, ragender_2, show_pct = T)
#dev.off()

# -----------------------------------------------------------------------------------------------------
# Construct a new variable for the cutoff difficulties in walking: 0.4 m/s, see Studenski et al 2011 and
# also Middleton et al. 2015
# -----------------------------------------------------------------------------------------------------

# we are considering impaired also those who do not complete their walking test or do not feel safe.
# We also consider imparied those who the interviewer said could not perform the walking test. 

# first considering all the missings as impaired:

wlk_charls<-df3 %>% 
  mutate(wlk_impair=case_when( (walk_speed >= 0.4) ~ 0,
                               (walk_speed <0.4 |walkspeed1 == -444 ) ~ 1,
                               TRUE ~ NA_real_ ),
         
      #   wlk_agree_room= case_when((rwalkra_1 == 0 & wlk_impair ==0) ~ 0,
      #                             (rwalkra_1 == 1 & wlk_impair ==1) ~ 0,
      #                             (rwalkra_1 == 0 & wlk_impair ==1) ~ 1,
      #                             (rwalkra_1 == 1 & wlk_impair ==0) ~ 2,
      #                             TRUE ~ NA_real_ ),
         
         wlk_agree_100m= case_when((rwalk100a_1 == 0 & wlk_impair ==0) ~ 0,
                                   (rwalk100a_1 == 1 & wlk_impair ==1) ~ 0,
                                   (rwalk100a_1 == 0 & wlk_impair ==1) ~ 1,
                                   (rwalk100a_1 == 1 & wlk_impair ==0) ~ 2,
                                   TRUE ~ NA_real_ ))

wlk_charls$ragender_2<-as.factor(wlk_charls$ragender_2)

# converting to survey data to account for weights. CHARLS provides individual-level or household weights 
# that account for non-response adjustment (rwtrespb), and no weights to account for complex
# survey design .

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")


speed_charls_surv <- svydesign( 
  ids = ~ID,  
  weights = ~rwtrespb,                          
  nest = T, 
  data = subset(wlk_charls, rwtrespb > 0 & ragey>=65))            


#All selected respondents who are not age-eligible are assigned 0 as the weight.



summary(speed_charls_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(wlk_charls$ragender_2))                        #non-weighted
prop.table(table(wlk_charls$age_cat))      
prop.table(svytable(~ragender_2, design=speed_charls_surv))    #weighted
prop.table(svytable(~age_cat, design=speed_charls_surv))

# Now accounting for more than one variable
svytable(~wlk_impair+age_cat+ragender_2, design=speed_charls_surv)


# table with summary statistics

speed_charls_surv %>% 
  tbl_svysummary(
    # Use include to select variables
    by= age_cat,
    include = c(ragey, raeducl_2,rwalk100a_2, wlk_impair,wlk_agree_100m, ragender_2,
                walkaid),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list( all_categorical() ~ c(0, 1)),
    missing="no") %>%
  modify_header(label = "**Variable**") %>%
  add_p() %>% # comparing values by "both" column
  add_overall() %>%
  modify_caption("Weighted descriptive statistics") %>%
bold_labels()  %>% # uncomment here to save in the file the table. 
  as_flex_table() %>%
  YesSiR::exportxlsx(path = here("decriptive_charls.xlsx"))

#remotes::install_github("Sebastien-Le/YesSiR")
library(YesSiR)


#-----------------------------------------------------------------------------------#
# Pooled countries analysis
#-----------------------------------------------------------------------------------#

# Getting the proportions of conditions
# proportions by age category with standard errors- here for all countries pooled together
# proportions by age category with standard errors. Here, we take the mean because taking the mean of a 
# variable coded 0/1 gives the proportions of 1s, so the mean of this variable is the estimated proportion
# of the population that is health or unhealthy. 


prop_healthy_wlk<-svyby(formula = ~wlk_impair, by = ~age_cat+ragender_2, 
                        design = speed_charls_surv, FUN = svymean,vartype=c("ci"),
                        na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_wlk<-as.data.frame(prop_healthy_wlk)

colnames(prop_healthy_wlk)<-c("age","gender","Impair","CI_low",
                              "CI_up")



# Grouping information and checking
# first age groups
prop_healthy_wlk<-prop_healthy_wlk %>% 
  #  select(1:4) %>% 
  arrange(gender) 

rownames(prop_healthy_wlk) <- NULL

# Simple graph to check gender with CI
fig1<-ggplot(prop_healthy_wlk,
             aes(age, Impair, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_wlk,aes(age, CI_low, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_wlk,aes(age, CI_up, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  #  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#009988','#882255')) 

fig1


# now if it is the reported

prop_healthy_wlk2<-svyby(formula = ~rwalk100a_1, by = ~age_cat+ragender_2, 
                         design = speed_charls_surv, FUN = svymean,vartype=c("ci"),
                         na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_wlk2<-as.data.frame(prop_healthy_wlk2)

colnames(prop_healthy_wlk2)<-c("age","gender","Impair","CI_low",
                               "CI_up")



# Grouping information and checking
# first age groups
prop_healthy_wlk2<-prop_healthy_wlk2 %>% 
  #  select(1:4) %>% 
  arrange(gender) 

rownames(prop_healthy_wlk2) <- NULL

# Simple graph to check gender with CI
fig2<-ggplot(prop_healthy_wlk2,
             aes(age, Impair, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_wlk2,aes(age, CI_low, 
                                       group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_wlk2,aes(age, CI_up, 
                                       group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  #  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#009988','#882255')) 

fig2




# now if it is the walk across the room - variable does not exist for CHARLS...

prop_healthy_wlk2$type<-"reported_100"
prop_healthy_wlk$type<-"tested"

p_all<-rbind(prop_healthy_wlk,prop_healthy_wlk2)


# Simple graph to check gender with CI
fig4<-ggplot(p_all,
             aes(age, Impair, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=p_all,aes(age, CI_low, 
                           group=gender,color=gender, shape=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=p_all,aes(age, CI_up, 
                           group=gender,color=gender, shape=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_grid(.~type)+
  #  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#009988','#882255')) 

fig4


# saving this data as .csv file but you can change it if you prefer later

write.table(p_all, here("Data","CHARLS","tested_reported_all_charls.csv"), sep = ",", col.names = NA,
            qmethod = "double")




fig5<-ggplot(p_all,
             aes(age, Impair, group=type,color=type, shape=type))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=p_all,aes(age, CI_low, 
                           group=type,color=type, shape=type), size=0.3, linetype="dashed")+
  
  geom_line(data=p_all,aes(age, CI_up, 
                           group=type,color=type, shape=type), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_grid(.~gender)
#  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
#scale_color_manual(values = c("brown","darkblue"))+
# scale_color_manual(values = c('#009988','#882255')) 
X11()
fig5

# some models
library(ggeffects)
library(svyVGAM)

mod1<-svyglm(as.numeric(wlk_impair)~age_cat+ragender_2, design= speed_charls_surv)
summary(mod1)


mod_1<-ggeffect(mod1, terms = c("age_cat", "ragender_2"))  %>%
  plot() 

mod2<-svyglm(as.numeric(wlk_impair)~age_cat+ragender_2+raeducl_2, design= speed_charls_surv, family = binomial)
summary(mod2)


ggeffect(mod2, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 



mod4<-svyglm(as.numeric(rwalk100a_1)~age_cat+ragender_2, design= speed_charls_surv)
summary(mod4)


ggeffect(mod4, terms = c("age_cat", "ragender_2"))  %>%
  plot() 


# multinomial models

library(kableExtra)
library(broom)       
library(srvyr)       
library(ggalluvial)  
library(dotwhisker)  

mod1.mult<-svy_vglm( wlk_agree_100m ~ age_cat+ragender_2, design = speed_charls_surv, family = multinomial(refLevel = "0"))

tidy_pma_model <- tidy.svyVGAM(mod1.mult, exponentiate = TRUE, conf.int = TRUE)

tidy_pma_model %>% 
  select(-y.level) %>% 
  rename(Variables = term) %>% 
  mutate(sig = case_when(p.value < 0.001 ~ "***",
                         p.value < 0.01 ~ "**",
                         p.value < 0.05 ~ "*",
                         T ~ "")) %>% 
  kbl(digits = 3) %>% 
  pack_rows("Overestimate", 1, 6) %>% 
  pack_rows("Underestimate", 7, 12)

# dot whisker plot

tidy_pma_model %>% 
  mutate(
    model = if_else(
      y.level == 1, 
      "Overestimate Concordance",
      "Underestimate Concordance", 
    ),
    sig = gtools::stars.pval(p.value)
  ) %>%
  relabel_predictors(c(
    `age_cat70-75` = "Age 70-75",
    `age_cat75-80`= "Age 75-80",
    `age_cat80-85`= "Age 80-85",
    `age_cat85+`= "Age > 85",
    `ragender_2woman` = "Woman")) %>% 
  dotwhisker::dwplot(
    dodge_size = 0.1,
    vline = geom_vline(xintercept = 1, colour = "grey60", linetype = 2),
    line_args = list(size = 4, lwd=8)
  ) + 
  guides(color = guide_legend(reverse = TRUE)) + 
  theme_minimal(base_size = 18) +
  theme(legend.position="top", 
        legend.title  = element_blank()) +
  scale_color_manual(values=c("darkblue", "firebrick")) 
