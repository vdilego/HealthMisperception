#-----------------------------------------------------------#
# Data prep for SHARE   --------------------------------------#
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

# Loading useful functions into environment
source(here("0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Manuscript","Figures")
dat.folder <- here("Manuscript","Data")
figs.app.folder <- here("Appendix","Figures")

#This is Version F in the harmonized files and incorporates the latest released version of SHARE data, 
# release 8.0.0, and adds observations from Wave 8. It contains 139,620 observations or rows. 
# It is a Respondent-level file so each row represents a unique Respondent. We will however use Wave 6,
# for year 2014, with 66,877 observations. 
#It also adds new variables and makes adjustments and corrections. We focus on data from SHARE Wave 6,
#with the release 8.0.0 as of February 2022. SHARE uses a multistage stratified sample.
#Its weighting variables make its data representative of the target populations in constituent countries.
# Wave 6 does not still have full coverage of European countries, with the following countries 
# only added in Wave 7: Finland, Lithuania, Latvia, Slovakia, Romania, Bulgaria, Malta and Cyprus.


# Reading SHARE for all years  -  read-in stata file from merge procedure

share <- read_dta(here("Data","walk_share.dta")) 


share<-as_factor(share)                         # transform all using as_factor from haven    
share$walkcomp<-as.character(share$walkcomp)
share$rwalkaid<-as.character(share$rwalkaid)
share$wave<-as.character(share$wave)
share$wave<-as.numeric(share$wave)

df1<-share %>% 
  mutate(year = case_when(wave ==1 ~ '2004',
                          wave ==2 ~ '2007',
                          TRUE ~ 'NA'))%>% 
 # mutate(across(everything(), ~replace(., . ==  -999 , NA))) %>%
 # mutate(across( .cols  = c(12:15),.fns =  ~replace(., . ==  -444 , "unable")))%>% 
  #mutate(across( .cols  = c(12:15),.fns =  ~replace(., . ==  -99 , "unsafe"))) %>% 
  unlabelled() %>% 
  filter(wave%in%c(1,2))


df2<-cSplit(df1, splitCols = c(8:11), ".")  # split the cols because of stata labels 

df3_wave1<-df2 %>% 
  filter(!is.na(rwtresp)) %>%                   # filtering only the eligible cases: non-eligibiles have a missing id
  filter(ragey >= 76 & wave %in%1)

df3_wave2<-df2 %>% 
  filter(!is.na(rwtresp)) %>%                   # filtering only the eligible cases: non-eligibiles have a missing id
  filter(ragey >= 75 & wave %in%2)

df3<-full_join(df3_wave1,df3_wave2) %>% 
# mutate(rwspeed_flag1= case_when(rwspeed1 == "unable" ~ 'unable',
#                                  rwspeed1 == "unsafe" ~ 'unsafe',
#                                  is.na(rwspeed1) ~ "NA",
#                                 TRUE ~ "completed"),
#         rwspeed_flag2= case_when(rwspeed2 == "unable" ~ 'unable',
#                                  rwspeed2 == "unsafe" ~ 'unsafe',
#                                  is.na(rwspeed2) ~ "NA",
#                                  TRUE ~ "completed"), 
  mutate(rwspeed1=as.numeric(rwspeed1),
         rwspeed2=as.numeric(rwspeed2),
         speed_avg=(rwspeed1+rwspeed2)/2,
         walk_speed=(2.5/speed_avg)) %>% 
  arrange(mergeid, wave) %>% 
  mutate(age_cat=cut(ragey, breaks = c(seq(75,80,5), 85, Inf),
                     labels = c( "75-80","80-85","85+"), right=F)) %>% 
  droplevels()


# let´s have a look at the missing values. The doctor diagnosed disease is actually pretty robust,
# when thinking in terms of missing values. 

plot_missy<-df3 %>%
  # Select the survey items
  select(c("speed_avg",  "rwalk100a_1" ,"rwalkra_1")) %>%
  # Create an UpSet plot
  gg_miss_upset(., nsets = 10 )


pdf(here(figs.app.folder,"missing_share_walk.pdf"))
plot_missy
dev.off()


# check all variables  by gender:
pdf(here(figs.app.folder,"missing_share_gender.pdf"))
gg_miss_var(df3, ragender_2, show_pct = T)
dev.off()

# -----------------------------------------------------------------------------------------------------
# Construct a new variable for the cutoff difficulties in walking: 0.4 m/s, see Studenski et al 2011 and
# also Middleton et al. 2015
# -----------------------------------------------------------------------------------------------------

# we are considering impaired also those who do not complete their walking test or do not feel safe.
# We also consider imparied those who the interviewer said could not perform the walking test. 

# first considering all the missings as impaired:

wlk_share<-df3 %>% 
  mutate(wlk_impair=case_when( (walk_speed > 0.4) ~ 0,
                               (walk_speed <=0.4 |walkspeed1 == -444 |walkspeed1 == -99)  ~ 1,
                               TRUE ~ NA_real_ ),
                      
         
         wlk_agree_room= case_when((rwalkra_1 == 0 & wlk_impair ==0) ~ 0,
                                     (rwalkra_1 == 1 & wlk_impair ==1) ~ 0,
                                     (rwalkra_1 == 0 & wlk_impair ==1) ~ 1,
                                     (rwalkra_1 == 1 & wlk_impair ==0) ~ 2,
                                     TRUE ~ NA_real_ ),
         
         wlk_agree_100m= case_when((rwalk100a_1 == 0 & wlk_impair ==0) ~ 0,
                                     (rwalk100a_1 == 1 & wlk_impair ==1) ~ 0,
                                     (rwalk100a_1 == 0 & wlk_impair ==1) ~ 1,
                                     (rwalk100a_1 == 1 & wlk_impair ==0) ~ 2,
                                     TRUE ~ NA_real_ ))

## all missings are missings
wlk_share2<-df3 %>% 
  mutate(wlk_impair=case_when( (walk_speed > 0.4) ~ 0,
                               (walk_speed <=0.4) ~ 1,
                               TRUE ~ NA_real_ ),
         
         
         wlk_agree_room= case_when((rwalkra_1 == 0 & wlk_impair ==0) ~ 0,
                                   (rwalkra_1 == 1 & wlk_impair ==1) ~ 0,
                                   (rwalkra_1 == 0 & wlk_impair ==1) ~ 1,
                                   (rwalkra_1 == 1 & wlk_impair ==0) ~ 2,
                                   TRUE ~ NA_real_ ),
         
         wlk_agree_100m= case_when((rwalk100a_1 == 0 & wlk_impair ==0) ~ 0,
                                   (rwalk100a_1 == 1 & wlk_impair ==1) ~ 0,
                                   (rwalk100a_1 == 0 & wlk_impair ==1) ~ 1,
                                   (rwalk100a_1 == 1 & wlk_impair ==0) ~ 2,
                                   TRUE ~ NA_real_ ))




# converting to survey data to account for weights. SHARE provides individual-level or household weights 
# that account for non-response adjustment (rwtresp) and sample design weights.
# design_wgt is the sample design weight, to guarantee representiveness and
# account for non-response issues. rwtresp is the person-level analysis weight. SHARE uses a multistage
# stratified sample. We stratify by country - even though we can do pooled analysis as well.

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

speed_share_surv <- svydesign( 
  ids = ~rwtsamp, 
  strata= ~country,
  weights = ~rwtresp,
  nest = T, 
  data = subset(wlk_share, rwtresp > 0))            

speed_share_surv2 <- svydesign( 
  ids = ~rwtsamp, 
  strata= ~country,
  weights = ~rwtresp,
  nest = T, 
  data = subset(wlk_share2, rwtresp > 0))   

#All selected respondents who are not age-eligible are assigned 0 as the weight.



summary(speed_share_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(wlk_share$ragender_2))                        #non-weighted
prop.table(table(wlk_share$age_cat))      
prop.table(svytable(~ragender_2, design=speed_share_surv))    #weighted
prop.table(svytable(~age_cat, design=speed_share_surv))

# Now accounting for more than one variable
svytable(~wlk_impair+age_cat+ragender_2, design=speed_share_surv)


# table with summary statistics

speed_share_surv %>% 
  tbl_svysummary(
    # Use include to select variables
    by= age_cat,
    include = c(ragey, raeducl_2,rwalk100a_2,rwalkra_2, wlk_impair,wlk_agree_room,wlk_agree_100m, ragender_2,
                walkaid),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list( all_categorical() ~ c(0, 1)),
    missing="no") %>%
  modify_header(label = "**Variable**") %>%
  add_p() %>% # comparing values by "both" column
  add_overall() %>%
  modify_caption("Weighted descriptive statistics") %>%
  bold_labels()#  %>% # uncomment here to save in the file the table. 
as_flex_table() %>%
  flextable::save_as_docx(path=here("Gender_health","Countries","EUROPE","Descriptive","decriptive_share_pooled.docx"))


# with the missings

speed_share_surv2 %>% 
  tbl_svysummary(
    # Use include to select variables
    by= age_cat,
    include = c(ragey, raeducl_2,rwalk100a_2,rwalkra_2, wlk_impair,wlk_agree_room,wlk_agree_100m, ragender_2,
                walkaid),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list( all_categorical() ~ c(0, 1)),
    missing="no") %>%
  modify_header(label = "**Variable**") %>%
  add_p() %>% # comparing values by "both" column
  add_overall() %>%
  modify_caption("Weighted descriptive statistics") %>%
  bold_labels()#  %>% # uncomment here to save in the file the table. 



# now the same table for each country

speed_share_surv2  %>% 
  tbl_svysummary(
    # Use include to select variables
    by= age_cat,
    include = c(ragey, raeducl_2,rwalk100a_2,rwalkra_2, wlk_impair,wlk_agree_room,wlk_agree_100m, ragender_2,
                walkaid, country),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list( all_categorical() ~ c(0, 1)),
    missing="no") %>%
  modify_header(label = "**Variable**") %>%
  add_p() %>% # comparing values by "both" column
  add_overall() %>%
  modify_caption("Weighted descriptive statistics") %>%
  bold_labels() # %>% # uncomment here to save in the file the table. 
as_flex_table() %>%
  flextable::save_as_docx(path=here("Gender_health","Countries","EUROPE","Descriptive","decriptive_share_countries.docx"))



#-----------------------------------------------------------------------------------#
# Pooled countries analysis
#-----------------------------------------------------------------------------------#

# Getting the proportions of conditions
# proportions by age category with standard errors- here for all countries pooled together
# proportions by age category with standard errors. Here, we take the mean because taking the mean of a 
# variable coded 0/1 gives the proportions of 1s, so the mean of this variable is the estimated proportion
# of the population that is health or unhealthy. 


prop_healthy_wlk<-svyby(formula = ~wlk_impair, by = ~age_cat+ragender_2, 
                        design = speed_share_surv, FUN = svymean,vartype=c("ci"),
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



# saving this data as .csv file but you can change it if you prefer later

write.table(prop_healthy_wlk, here("Data","SHARE","share_walk_pooled.csv"), sep = ",", col.names = NA,
            qmethod = "double")



#  Now for countries

#-----------------------------------------------------------------------------------#
# country specific analysis
#-----------------------------------------------------------------------------------#

# proportions by age category with standard errors by country

prop_healthy_wlk_country<-svyby(formula = ~wlk_impair, by = ~age_cat+ragender_2+country, 
                        design = speed_share_surv, FUN = svymean,vartype=c("ci"),
                        na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_wlk_country<-as.data.frame(prop_healthy_wlk_country)

colnames(prop_healthy_wlk_country)<-c("age","gender","country","Impair","CI_low",
                              "CI_up")



rownames(prop_healthy_wlk_country) <- NULL


X11()
# Simple graph to check gender with CI
fig2<-ggplot(prop_healthy_wlk_country,
                             aes(age, country, group=country, fill=Impair))+
  geom_raster(interpolate = T) +
  theme_clean(base_size = 14)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=10),
        legend.text = element_text(size=10))+
  facet_grid(gender~.)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy", n.breaks=4)

#pdf(here("Gender_health", "Countries","EUROPE","Descriptive","fig_prev_adl_country.pdf"), width = 8, height=10)
fig2
#dev.off()

# saving this data as .csv file but you can change it if you prefer later

write.table(prop_healthy_wlk_country, here("Data","SHARE", "share_walk_country.csv"), sep = ",", col.names = NA,
            qmethod = "double")


X11()
fig2.1<-ggplot(prop_healthy_wlk_country,
             aes(age, Impair, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  #geom_line(data=prop_healthy_wlk_country,aes(age, CI_low, 
#                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  
  #geom_line(data=prop_healthy_wlk_country,aes(age, CI_up, 
  #                                    group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
   facet_grid(.~country)+
  #  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#009988','#882255')) 

fig2.1




# some models
library(ggeffects)

mod1<-svyglm(as.numeric(adl)~age_cat+ragender_2, design= df_share_surv, family = binomial)
summary(mod1)


mod_1<-ggeffect(mod1, terms = c("age_cat", "ragender_2"))  %>%
  plot() 

mod2<-svyglm(as.numeric(adl)~age_cat+ragender_2+raeducl_2, design= df_share_surv, family = binomial)
summary(mod2)


ggeffect(mod2, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


mod3<-svyglm(as.numeric(chronic_free)~age_cat+ragender_2, design= df_share_surv, family = binomial)

ggeffect(mod3, terms = c("age_cat", "ragender_2"))  %>%
  plot() 
summary(mod3)


mod4<-svyglm(as.numeric(chronic_free_heart)~age_cat+ragender_2, design= df_share_surv, family = binomial)
summary(mod4)


ggeffect(mod4, terms = c("age_cat", "ragender_2"))  %>%
  plot() 


mod5<-svyglm(as.numeric(chronic_free_heart)~age_cat+ragender_2+raeducl_2, design= df_share_surv, family = binomial)
summary(mod5)

ggeffect(mod5, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


mod6<-svyglm(as.numeric(chronic_free_arthre)~age_cat+ragender_2, design= df_share_surv, family = binomial)
summary(mod6)

ggeffect(mod6, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


#mod7<-svy_vglm(as.numeric(chronic_severe)~age_cat+ragender_2, design= df_hrs_surv, 
#               family = multinomial)
#summary(mod7)


mod7<-svyglm(as.numeric(chronic_sum)~age_cat+ragender_2, design= df_share_surv)
summary(mod7)

ggeffect(mod7, terms = c("age_cat", "ragender_2"))  %>%
  plot() 




