#-----------------------------------------------------------#
# Data prep for MHAS   --------------------------------------#
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

# We are using the Haronized VERSION B.4 (2001-2015), FEBRUARY 2022, for the MHAS data. 
#The Mexican Health and Aging Study (MHAS) is a longitudinal household survey dataset 
# for the study of health, economic position, and quality of life among the elderly.
# MHAS datasets as of September 2020. The MHAS (Mexican Health and Aging Study)
# Version B.4 incorporates the latest released version of MHAS data, and adds several new variables.
# It contains 22,016 observations or rows. It is a Respondent-level file so each row represents a unique Respondent.
# We will only use WAVE 3, because this was the only wave with the test, so an intial sample of 16,886, 
# when considering only elligible questionaires and people not in institutions. After only considering those
# whose walking tests were performed, we are left with a sample of 1,904 individuals. And only for people after
# age 65 : 753



# Loading useful functions into environment
source(here("0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Manuscript","Figures")
dat.folder <- here("Manuscript","Data")
figs.app.folder <- here("Appendix","Figures")

# We are using the Harmonized version C HRS: 42,233 observations. This is a new updated version C,
# until 2019 that was updated now in 2022. 
# It is a Respondent level file so each row represents a unique Respondent. 
#
# Reading HRS for all years
# Here the number after the HRS data call is the latest date where data was retrived and updated from mycomputer


mhas <- read_dta(here("Data","walk_mhas.dta")) %>% 
  filter(!is.na(rwtresp)) %>% 
  filter(rwtresp>0) 


mhas<-as_factor(mhas)                         # transform all using as_factor from haven    
mhas$walkcomp<-as.character(mhas$walkcomp)
mhas$walkaid<-as.character(mhas$walkaid)
mhas$wave<-as.character(mhas$wave)
mhas$wave<-as.numeric(mhas$wave)

df1<-mhas %>% 
  #filter(!is.na(walkspeed1)) %>% 
  unlabelled() %>% 
  filter(ragey>=65)

df2<-cSplit(df1, splitCols = c(6:9,12:13), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  mutate(rwspeed1=as.numeric(rwspeed1),
         rwspeed2=as.numeric(rwspeed2),
         speed_avg=(rwspeed1+rwspeed2)/2,
         walk_speed=(3/speed_avg)) %>% 
  arrange(unhhidnp, wave) %>% 
  mutate(age_cat=cut(ragey, breaks = c(seq(65,80,5), 85, Inf),
                     labels = c("65-70","70-75", "75-80","80-85","85+"), right=F)) %>% 
  droplevels() #%>% 
  filter(walk_speed<3)


# let´s have a look at the missing values. The doctor diagnosed disease is actually pretty robust,
# when thinking in terms of missing values. 

plot_missy<-df3 %>%
  # Select the survey items
  select(c("speed_avg",  "rwalk1a_1","rwalkra_1")) %>%
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

wlk_mhas<-df3 %>% 
  mutate(wlk_impair=case_when( (walk_speed >= 0.4) ~ 0,
                               (walk_speed <0.4 |walkspeed1 == -444 ) ~ 1,
                               TRUE ~ NA_real_ ),
         
         wlk_agree_room= case_when((rwalkra_1 == 0 & wlk_impair ==0) ~ 0,
                                   (rwalkra_1 == 1 & wlk_impair ==1) ~ 0,
                                   (rwalkra_1 == 0 & wlk_impair ==1) ~ 1,
                                   (rwalkra_1 == 1 & wlk_impair ==0) ~ 2,
                                   TRUE ~ NA_real_ ),
         
         wlk_agree_100m= case_when((rwalk1a_1 == 0 & wlk_impair ==0) ~ 0,
                                   (rwalk1a_1 == 1 & wlk_impair ==1) ~ 0,
                                   (rwalk1a_1 == 0 & wlk_impair ==1) ~ 1,
                                   (rwalk1a_1 == 1 & wlk_impair ==0) ~ 2,
                                   TRUE ~ NA_real_ ))


# converting to survey data to account for weights. SHARE provides individual-level or household weights 
# that account for non-response adjustment (rwtresp) and sample design weights.
# design_wgt is the sample design weight, to guarantee representiveness and
# account for non-response issues. rwtresp is the person-level analysis weight. SHARE uses a multistage
# stratified sample. We stratify by country - even though we can do pooled analysis as well.

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")


speed_mhas_surv <- svydesign( 
  ids =  ~unhhidnp, 
 # strata= ~raestrat,
  weights = ~rwtresp,
  nest = T, 
  data = subset(wlk_mhas, rwtresp > 0 & ragey>=65))            


#All selected respondents who are not age-eligible are assigned 0 as the weight.



summary(speed_mhas_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(wlk_mhas$ragender_2))                        #non-weighted
prop.table(table(wlk_mhas$age_cat))      
prop.table(svytable(~ragender_2, design=speed_mhas_surv))    #weighted
prop.table(svytable(~age_cat, design=speed_mhas_surv))

# Now accounting for more than one variable
svytable(~wlk_impair+age_cat+ragender_2, design=speed_mhas_surv)


# table with summary statistics

speed_mhas_surv %>% 
  tbl_svysummary(
    # Use include to select variables
    by= age_cat,
    include = c(ragey, raeducl_2,rwalk1a_2,rwalkra_2, wlk_impair,wlk_agree_room,wlk_agree_100m, ragender_2,
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
  YesSiR::exportxlsx(path = here("decriptive_mhas.xlsx"))

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
                        design = speed_mhas_surv, FUN = svymean,vartype=c("ci"),
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

prop_healthy_wlk2<-svyby(formula = ~rwalk1a_1, by = ~age_cat+ragender_2, 
                         design = speed_mhas_surv, FUN = svymean,vartype=c("ci"),
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




# now if it is the walk across the room

prop_healthy_wlk3<-svyby(formula = ~rwalkra_1, by = ~age_cat+ragender_2, 
                         design = speed_mhas_surv, FUN = svymean,vartype=c("ci"),
                         na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_wlk3<-as.data.frame(prop_healthy_wlk3)

colnames(prop_healthy_wlk3)<-c("age","gender","Impair","CI_low",
                               "CI_up")





# Grouping information and checking
# first age groups
prop_healthy_wlk3<-prop_healthy_wlk3 %>% 
  #  select(1:4) %>% 
  arrange(gender) 

rownames(prop_healthy_wlk3) <- NULL

# Simple graph to check gender with CI
fig3<-ggplot(prop_healthy_wlk3,
             aes(age, Impair, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_wlk3,aes(age, CI_low, 
                                       group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_wlk3,aes(age, CI_up, 
                                       group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  #  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#009988','#882255')) 

fig3


prop_healthy_wlk3$type<-"reported_across"
prop_healthy_wlk2$type<-"reported_100"
prop_healthy_wlk$type<-"tested"

p_all<-rbind(prop_healthy_wlk,prop_healthy_wlk2,prop_healthy_wlk3)


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

write.table(p_all, here("Data","MHAS","tested_reported_all_mhas.csv"), sep = ",", col.names = NA,
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

mod1<-svyglm(as.numeric(wlk_impair)~age_cat+ragender_2, design= speed_mhas_surv)
summary(mod1)


mod_1<-ggeffect(mod1, terms = c("age_cat", "ragender_2"))  %>%
  plot() 

mod2<-svyglm(as.numeric(wlk_impair)~age_cat+ragender_2+raeducl_2, design= speed_mhas_surv, family = binomial)
summary(mod2)


ggeffect(mod2, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


mod3<-svyglm(as.numeric(rwalkra_1)~age_cat+ragender_2, design= speed_mhas_surv)

ggeffect(mod3, terms = c("age_cat", "ragender_2"))  %>%
  plot() 
summary(mod3)


mod4<-svyglm(as.numeric(rwalk1a_1)~age_cat+ragender_2, design= speed_mhas_surv)
summary(mod4)


ggeffect(mod4, terms = c("age_cat", "ragender_2"))  %>%
  plot() 


# multinomial models

library(kableExtra)
library(broom)       
library(srvyr)       
library(ggalluvial)  
library(dotwhisker)  

mod1.mult<-svy_vglm( wlk_agree_room ~ age_cat+ragender_2, design = speed_mhas_surv, family = multinomial(refLevel = "0"))

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
    `ragender_2male` = "Male")) %>% 
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