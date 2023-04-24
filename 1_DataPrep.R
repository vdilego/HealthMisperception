#-----------------------------------------------------------#
# Data prep for all countries   ----------------------------#
# Author: Vanessa di Lego #
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
library(foreign)
#install.packages("readstata13")
library(readstata13)

# Loading useful functions into environment
source(here("0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Manuscript","Figures")
dat.folder <- here("Manuscript","Data")
figs.app.folder <- here("Appendix","Figures")

#dir.create(figs.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(dat.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(figs.app.folder, showWarnings = FALSE, recursive = TRUE)

# HRS#

# We are using the Harmonized version C HRS: 42,233 observations. This is a new updated version C,
# until 2019 that was updated now in 2022. 
# It is a Respondent level file so each row represents a unique Respondent. 
# We focus only on wave 12 (year 2014) of HRS. This leaves us with 18,747 observations

# Reading HRS for all years
# Here the number after the HRS data call is the latest date where data was retrived and updated from mycomputer

hrs <- read_dta(here("Data","walk_hrs13.dta") ) # read-in stata file from merge procedure


df1<-as_factor(hrs) # transform all using as_factor from haven    

df1<-df1 %>% 
  mutate(year = case_when(wave ==1 ~ '1992',
                          wave ==2 ~ '1994',
                          wave ==3 ~ '1996',
                          wave ==4 ~ '1998',
                          wave ==5 ~ '2000',
                          wave ==6 ~ '2002',
                          wave ==7 ~ '2004',
                          wave ==8 ~ '2006',
                          wave ==9 ~ '2008',
                          wave ==10 ~ '2010',
                          wave ==11 ~ '2012',
                          wave ==12 ~ '2014',
                          wave ==13 ~ '2016',
                          wave ==14 ~ '2018'  ,
                          TRUE ~ 'NA')) %>% 
  filter(wave>7 & ragey_e>=64)



df1<-unlabelled(df1)

df2<-cSplit(df1, splitCols = c(3,9:11,14,15), ".")  # split the cols because of stata labels 

df3<-df2 %>% 
  filter(!is.na(rwtresp) )%>%                   # filtering only the eligible cases: non-eligibiles have a missing id
  select(- c(10))                               # select only the variables for use in prevalence estimation 


# Uncomment down below to rename columns (check columns first). I have decided to keep the original names to help further
# checks with data updates

#colnames(df3)<-c("id","wave","birth_year","age","strata_wgt","sample_wgt","adl", "diabete_age","heart_age",
#                 "ind_wgt", "last_cancer_age","last_heart_age","gender_1","gender_2","diabetes_1", "diabetes_2",
#                 "cancer_1","cancer_2",  "lung_1","lung_2", "heart_1", "heart_2", "stroke_1","stroke_2",
#                 "arthritis_1","arthritis_2",  "educ_1","educ_2")
                
                
#table((df3 %>% filter(rwtresp%in%0 & ragey_e>=50))$ragender_2)

df3<-df3 %>% 
  filter(wave%in%12) %>% 
  mutate(age_cat=cut(ragey_e, breaks = c(seq(0,75,5), 80, Inf),
                     labels = c("0-5","5-10", "10-15",  
                                "15-20","20-25","25-30",
                                "30-35","35-40","40-45","45-50",
                                "50-55", "55-60", "60-65","65-70",
                                "70-75",  "75-80","80+"), right=F)) %>% 
  droplevels()

# let´s have a look at the missing values. The doctor diagnosed disease is actually pretty robust,
# when thinking in terms of missing values. 

plot_missy<-df3 %>%
  # Select the survey items
  select(c("rdiabe_1", "rcancre_1","rlunge_1","rhearte_1","rstroke_1","rarthre_1","radlfive")) %>%
  # Create an UpSet plot
  gg_miss_upset(., nsets = 10 )


pdf(here("Gender_health", "Countries","US","Descriptive","missing_hrs.pdf"))
plot_missy
dev.off()


# check all variables  by gender:
pdf(here("Gender_health", "Countries","US","Descriptive","missing_hrs_gender.pdf"))
gg_miss_var(df3, ragender_2, show_pct = T)
dev.off()

# lots of missing. We cannot use any of the age variables. However, it is important to see that 
# there is no pattern by gender in terms of missing. Which shows it is pretty robust.

df3 <-df3 %>% 
  select(-c(7))

# -----------------------------------------------------------------------------------------------------
# Construct a new variable for the chronic conditions/diseases
# We have constructed three new variables that capture prevalence of chronic conditions. 
# “chronic” refers to having at least one of the conditions cited above. “chronic_sum” refers to 
# the total number of conditions ever diagnosed. “chronic_severe” is a dummy variable that measures 
# where an individual is diagnosed with ever having had three or more of those conditions and we
# are associating this to a measure of severity of chronic disease or having experienced more than one
# condition through a lifetime.
# -----------------------------------------------------------------------------------------------------

# Since the number of missings is really small and around 0.1% for each conditions we need, we excluded those cases.
# this totals 194 cases/persons. There was also no pattern by gender or specific pattern for the missing cases.
# the total sample missing for Wave 12 is around 1.03%, of the total 18,747.


df3_na<-df3 %>% 
  drop_na()

df3_var<-df3_na %>% 
  group_by(hhidpn) %>% 
  mutate(chronic_free = case_when(rdiabe_1 ==0 & rcancre_1 == 0 &
                              rlunge_1==0   & rhearte_1 ==  0 &
                              rstroke_1==0 & rarthre_1 == 0  ~ 0, TRUE ~ 1),
         chronic_free_heart = case_when(rhearte_1 ==  0  ~ 0, TRUE ~ 1),
         chronic_free_diabe = case_when(rdiabe_1 ==0 ~ 0, TRUE ~ 1),
         chronic_free_cancre = case_when(rcancre_1 ==0 ~ 0, TRUE ~ 1),
         chronic_free_stroke = case_when(rstroke_1 ==0 ~ 0, TRUE ~ 1),
         chronic_free_arthre = case_when(rarthre_1 ==0 ~ 0, TRUE ~ 1),
         chronic_free_lunge = case_when(rlunge_1 ==0 ~ 0, TRUE ~ 1)) %>% 
  mutate(chronic_sum=sum(rdiabe_1,rcancre_1,rlunge_1,rhearte_1,rstroke_1,rarthre_1)) %>% 
  ungroup() %>% 
  group_by(hhidpn) %>% 
         mutate(adl=case_when(radlfive ==0 ~ 0, radlfive >= 1 ~ 1),
                chronic_severe=case_when((chronic_sum == 0)  ~ 0,
                                         (chronic_sum >= 1 & chronic_sum <3) ~ 1,
                                         (chronic_sum >= 3 & chronic_sum <5) ~ 2,
                                         (chronic_sum >= 5 ~ 3)))

# now let us take make it a common dataframe

df3_var<-as.data.frame(df3_var)

        
df3_var$chronic_free<-as.character(df3_var$chronic_free)
df3_var$chronic_free_heart<-as.character(df3_var$chronic_free_heart)
df3_var$chronic_free_diabe<-as.character(df3_var$chronic_free_diabe)
df3_var$chronic_free_cancre<-as.character(df3_var$chronic_free_cancre)
df3_var$chronic_free_stroke<-as.character(df3_var$chronic_free_stroke)
df3_var$chronic_free_arthre<-as.character(df3_var$chronic_free_arthre)
df3_var$chronic_free_lunge<-as.character(df3_var$chronic_free_lunge)



df3_var$chronic_severe<-as.character(df3_var$chronic_severe)
df3_var$adl<-as.character(df3_var$adl)


df3_var$chronic_severe_2<-as.factor(df3_var$chronic_severe)
df3_var$chronic_severe_2<-factor(df3_var$chronic_severe, levels = c("0","1","2","3"), labels=c("no chronic",
                                                                                             ">=1 & <3 ",
                                                                                             ">= 3 & <5",
                                                                                             ">= 5") )

df3_var$ragender_2<-as.factor(df3_var$ragender_2)
df3_var$ragender_2<-factor(df3_var$ragender_2, levels = c("female","male"), labels=c("woman","man") )

# converting to survey data to account for weights. HRS provides individual-level or household weights 
# that account for non-response adjustment (rwtresp), and strata and sample weights to account for complex
# survey design ("raestrat","raehsamp").
# Like all datasets, I left it in wide format and with both numeric and categorical dummy variables,
# so you can choose what you prefer or find more useful.


# we have to use this survey package to account for the standard errors and sample design.
# non-institutionalized

df_hrs_surv <- svydesign( 
  ids = ~raehsamp,  
  strata = ~raestrat,
  weights = ~rwtresp,                          
  nest = T, 
  data = subset(df3_var, rwtresp > 0 & ragey_e>=50))            

# weights greater than 0 because 0: people in institutions. Also, From Wave 5 to Wave 12, HRS provides weights for 
# individuals living in a nursing home. These weights are provided in RwWTR_NH. For those not living in a 
# nursing home, these weights are zero. From Wave 13, there are no nursing home weights.

# ages equal or above 50 because we only want the main individual and no spouses

summary(df_hrs_surv)

# differences between weighted and unweighted when estimating proportions 

prop.table(table(df3_var$ragender_1))                        #non-weighted
prop.table(table(df3_var$age_cat))      
prop.table(svytable(~ragender_2, design=df_hrs_surv))    #weighted
prop.table(svytable(~age_cat, design=df_hrs_surv))

prop.table(svytable(~adl, design=df_hrs_surv))

# Now accounting for more than one variable
svytable(~radlfive+ragey_e, design=df_hrs_surv)

svytable(~radlfive+age_cat+ragender_2, design=df_hrs_surv)

svytable(~chronic_free+age_cat+ragender_2, design=df_hrs_surv)

svytable(~chronic_sum+age_cat+ragender_2, design=df_hrs_surv)

svytable(~chronic_severe+age_cat+ragender_2, design=df_hrs_surv)

# table with summary statistics

df_hrs_surv %>% 
  tbl_svysummary(
    # Use include to select variables
    by=ragender_2,
    include = c(ragender_2,adl,ragey_e, raeducl_2,chronic_free,chronic_free_heart,chronic_free_diabe,
                chronic_free_arthre,chronic_free_lunge,chronic_free_stroke,chronic_free_cancre, chronic_severe_2),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list( all_categorical() ~ c(0, 1)),
    missing="no") %>%
  modify_header(label = "**Variable**") %>%
  add_p() %>% # comparing values by "both" column
  add_overall() %>%
  modify_caption("Weighted descriptive statistics") %>%
  bold_labels() # uncomment here to save in the file the table.%>% 
as_flex_table() %>%
flextable::save_as_docx(path=here("Gender_health","Countries","US","Descriptive","decriptive_hrs.docx"))

# Getting the proportions by ADLs


# proportions by age category with standard errors. Here, we take the mean because taking the mean of a 
# variable coded 0/1 gives the proportions of 1s, so the mean of this variable is the estimated proportion
# of the population that is health or unhealthy. 

prop_healthy_adl<-svyby(formula = ~adl, by = ~age_cat+year+ragender_2, 
                    design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_adl<-as.data.frame(prop_healthy_adl)

colnames(prop_healthy_adl)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                          "CI_up_unhealthy")
prop_healthy_adl<-prop_healthy_adl[, c(1:5,6,8,7,9)]


# proportions by single age with standard errors.
#prop_healthy_single<-svyby(formula = ~adl, by = ~age+wave+ragender_2, 
#                           design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
#                           na.rm = TRUE,prop_method = c("likelihood"))


#prop_healthy_single<-as.data.frame(prop_healthy_single)

# colnames(prop_healthy_single)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
#                          "CI_up_unhealthy")

# prop_healthy_single<-prop_healthy_single[, c(1:5,6,8,7,9)]



# Grouping information and checking
# first age groups
prop_healthy_adl<-prop_healthy_adl %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_adl) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_adl,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_adl,aes(age, CI_low_unhealthy, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_adl,aes(age, CI_up_unhealthy, 
                                      group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

# saving this data as .csv file but you can change it if you prefer later

write.table(prop_healthy_adl, here("Gender_health","Data","HRS","Prevalence", "prev_hrs_adl_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")



# now for single ages

#prev_unhealthy_single_adl<-prop_healthy_single %>% 
#  #  select(1:4) %>% 
#  arrange(gender) %>% 
#  mutate(year=as.factor(year))

#rownames(prev_unhealthy_single_adl) <- NULL

# saving this data as .csv file but you can change it if you prefer later

#write.table(prev_unhealthy_single_adl, here("Gender_health","Data","HRS", "Prevalence","prev_hrs_adl.csv"), sep = ",", col.names = NA,
#            qmethod = "double")





# Now for Chronic Conditions
# proportions by age category with standard errors.
# here for at least one chronic condition

prop_healthy_chron<-svyby(formula = ~chronic_free, by = ~age_cat+year+ragender_2, 
                    design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                    na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron<-as.data.frame(prop_healthy_chron)


colnames(prop_healthy_chron)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                          "CI_up_unhealthy")
prop_healthy_chron<-prop_healthy_chron[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron<-prop_healthy_chron %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron,
       aes(age, unhealthy, group=gender,color=gender, shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_healthy_chron,aes(age, CI_low_unhealthy, 
                                        group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron,aes(age, CI_up_unhealthy, 
                                        group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  # facet_grid(.~gender)+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 

# saving this data as .csv file but you can change it if you prefer later

write.table(prop_healthy_chron, here("Gender_health","Data","HRS","Prevalence", "prev_hrs_cron_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")


# now doing some sensitivity for different conditions

# here for being free from heart conditions, but not any other.

prop_healthy_chron_heart<-svyby(formula = ~chronic_free_heart, by = ~age_cat+year+ragender_2, 
                          design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                          na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_heart<-as.data.frame(prop_healthy_chron_heart)


colnames(prop_healthy_chron_heart)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                "CI_up_unhealthy")
prop_healthy_chron_heart<-prop_healthy_chron_heart[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_heart<-prop_healthy_chron_heart %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_heart) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_heart,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_heart,aes(age, CI_low_unhealthy, 
                                            group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_heart,aes(age, CI_up_unhealthy, 
                                            group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))

## gradient reverses.

# let us see arthrites: being free from arthiritis, but not any other.

prop_healthy_chron_arthre<-svyby(formula = ~chronic_free_arthre, by = ~age_cat+year+ragender_2, 
                                design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_arthre<-as.data.frame(prop_healthy_chron_arthre)


colnames(prop_healthy_chron_arthre)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                      "CI_up_unhealthy")
prop_healthy_chron_arthre<-prop_healthy_chron_arthre[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_arthre<-prop_healthy_chron_arthre %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_arthre) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_arthre,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_arthre,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_arthre,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))

# gradient goes back to female disadvantage


# cancer
# let us see cancer: being free from cancer, but not any other.

prop_healthy_chron_cancer<-svyby(formula = ~chronic_free_cancre, by = ~age_cat+year+ragender_2, 
                                 design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_cancer<-as.data.frame(prop_healthy_chron_cancer)


colnames(prop_healthy_chron_cancer)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_cancer<-prop_healthy_chron_cancer[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_cancer<-prop_healthy_chron_cancer %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_cancer) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_cancer,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_cancer,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_cancer,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))



#  Stroke
# being free from stroke, but not any other.

prop_healthy_chron_stroke<-svyby(formula = ~chronic_free_stroke, by = ~age_cat+year+ragender_2, 
                                 design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_stroke<-as.data.frame(prop_healthy_chron_stroke)


colnames(prop_healthy_chron_stroke)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_stroke<-prop_healthy_chron_stroke[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_stroke<-prop_healthy_chron_stroke %>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_stroke) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_stroke,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_stroke,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_stroke,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))



#  lung disease
# being free from lung disease, but not any other.

prop_healthy_chron_lunge<-svyby(formula = ~chronic_free_lunge, by = ~age_cat+year+ragender_2, 
                                 design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                                 na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_lunge<-as.data.frame(prop_healthy_chron_lunge)


colnames(prop_healthy_chron_lunge)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                       "CI_up_unhealthy")
prop_healthy_chron_lunge<-prop_healthy_chron_lunge[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_lunge<-prop_healthy_chron_lunge%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_lunge) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_lunge,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_lunge,aes(age, CI_low_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_lunge,aes(age, CI_up_unhealthy, 
                                               group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))


#  diabetes
# being free from diabetes, but not any other.

prop_healthy_chron_diabe<-svyby(formula = ~chronic_free_diabe, by = ~age_cat+year+ragender_2, 
                                design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_diabe<-as.data.frame(prop_healthy_chron_diabe)


colnames(prop_healthy_chron_diabe)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                      "CI_up_unhealthy")
prop_healthy_chron_diabe<-prop_healthy_chron_diabe[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_diabe<-prop_healthy_chron_diabe%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_diabe) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_diabe,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_diabe,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_diabe,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))



# dummy of severity of conditions - 
# >=1 & <3 conditions

prop_healthy_chron_2<-svyby(formula = ~chronic_severe==1, by = ~age_cat+year+ragender_2, 
                                design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                                na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_2<-as.data.frame(prop_healthy_chron_2)


colnames(prop_healthy_chron_2)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                      "CI_up_unhealthy")
prop_healthy_chron_2<-prop_healthy_chron_2[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_2<-prop_healthy_chron_2%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_2) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_2,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_2,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_2,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))


# having >= 3 & <5

prop_healthy_chron_3<-svyby(formula = ~chronic_severe==2, by = ~age_cat+year+ragender_2, 
                            design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_3<-as.data.frame(prop_healthy_chron_3)


colnames(prop_healthy_chron_3)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                  "CI_up_unhealthy")
prop_healthy_chron_3<-prop_healthy_chron_3[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_3<-prop_healthy_chron_3%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_3) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_3,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_3,aes(age, CI_low_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_3,aes(age, CI_up_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))



# having 	>= 5

prop_healthy_chron_4<-svyby(formula = ~chronic_severe==3, by = ~age_cat+year+ragender_2, 
                            design = df_hrs_surv, FUN = svymean,vartype=c("ci"),
                            na.rm = TRUE, prop_method = c("likelihood"))

prop_healthy_chron_4<-as.data.frame(prop_healthy_chron_4)


colnames(prop_healthy_chron_4)<-c("age","year","gender","healthy","unhealthy","CI_low_healthy","CI_low_unhealthy","CI_up_healthy",
                                  "CI_up_unhealthy")
prop_healthy_chron_4<-prop_healthy_chron_4[, c(1:5,6,8,7,9)]


# Grouping information and checking
# first age groups
prop_healthy_chron_4<-prop_healthy_chron_4%>% 
  #  select(1:4) %>% 
  arrange(gender) %>% 
  mutate(year=as.factor(year))

rownames(prop_healthy_chron_4) <- NULL

# Simple graph to check gender with CI
ggplot(prop_healthy_chron_4,
       aes(age, unhealthy, group=gender,color=gender))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_line(data=prop_healthy_chron_4,aes(age, CI_low_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_healthy_chron_4,aes(age, CI_up_unhealthy, 
                                          group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  # facet_grid(.~gender)+
  scale_color_manual(values = c("brown","darkblue"))



# joining them all

prop_healthy_chron$type<-"at least one"
prop_healthy_chron_arthre$type<-"arthritis"
prop_healthy_chron_cancer$type<-"cancer"
prop_healthy_chron_heart$type<-"heart"
prop_healthy_chron_lunge$type<-"lung"
prop_healthy_chron_stroke$type<-"stroke"
prop_healthy_chron_diabe$type<-"diabetes"
prop_healthy_chron_diabe$type<-"diabetes"
prop_healthy_chron_2$type<-"one or two any condition"
prop_healthy_adl$type<-"adl"


prop_all_chronic<- rbind(prop_healthy_chron, prop_healthy_adl,prop_healthy_chron_arthre, prop_healthy_chron_cancer, prop_healthy_chron_diabe,
                         prop_healthy_chron_heart, prop_healthy_chron_lunge, prop_healthy_chron_stroke,prop_healthy_chron_2)


fig_prev_all<- ggplot(prop_all_chronic,
       aes(age, unhealthy, group=gender,color=gender,shape=gender))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=prop_all_chronic,aes(age, CI_low_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  
  geom_line(data=prop_all_chronic,aes(age, CI_up_unhealthy, 
                                              group=gender,color=gender), size=0.3, linetype="dashed")+
  theme_clean(base_size = 14)+
   facet_wrap(.~type)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#882255','#009988')) 


pdf(here("Gender_health", "Countries","US","Descriptive","fig_prev_hrs.pdf"), width = 10, height=10)
fig_prev_all
dev.off()



# save this dataset as well.

write.table(prop_all_chronic, here("Gender_health","Data","HRS","Prevalence", "prev_hrs_cron_type_agecat.csv"), sep = ",", col.names = NA,
            qmethod = "double")


# some models
library(ggeffects)

mod1<-svyglm(as.numeric(adl)~age_cat+ragender_2, design= df_hrs_surv, family = binomial)
summary(mod1)


mod_1<-ggeffect(mod1, terms = c("age_cat", "ragender_2"))  %>%
  plot() 

mod2<-svyglm(as.numeric(adl)~age_cat+ragender_2+raeducl_2, design= df_hrs_surv, family = binomial)
summary(mod2)


ggeffect(mod2, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


mod3<-svyglm(as.numeric(chronic_free)~age_cat+ragender_2, design= df_hrs_surv, family = binomial)

ggeffect(mod3, terms = c("age_cat", "ragender_2"))  %>%
  plot() 
summary(mod3)


mod4<-svyglm(as.numeric(chronic_free_heart)~age_cat+ragender_2, design= df_hrs_surv, family = binomial)
summary(mod4)


ggeffect(mod4, terms = c("age_cat", "ragender_2"))  %>%
  plot() 


mod5<-svyglm(as.numeric(chronic_free_heart)~age_cat+ragender_2+raeducl_2, design= df_hrs_surv, family = binomial)
summary(mod5)

ggeffect(mod5, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


mod6<-svyglm(as.numeric(chronic_free_arthre)~age_cat+ragender_2, design= df_hrs_surv, family = binomial)
summary(mod6)

ggeffect(mod6, terms = c("age_cat", "ragender_2","raeducl_2"))  %>%
  plot() 


#mod7<-svy_vglm(as.numeric(chronic_severe)~age_cat+ragender_2, design= df_hrs_surv, 
#               family = multinomial)
#summary(mod7)


mod7<-svyglm(as.numeric(chronic_sum)~age_cat+ragender_2, design= df_hrs_surv)
summary(mod7)

ggeffect(mod7, terms = c("age_cat", "ragender_2"))  %>%
  plot() 


