# -----------------------------------------------------------------------------------------------------#
# Estimating new HLY with "adjusted" values of walking disability
# author: Vanessa di Lego
# -----------------------------------------------------------------------------------------------------#

## cleaning the workspace
rm(list=ls(all=TRUE))

# loading necessary packages
library(MortalityLaws)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(purrr)
library(styler)
library(broom)
library(here)
library(ggpubr)
library(ggExtra)
library(ggthemes)
library(ggrepel)
library(Hmisc)
library(suffrager)
library(forcats)
library(ggthemes)
library(colorspace)
library(cowplot)
library(grid)
library(DemoDecomp) 
library(gridExtra)


# Loading useful functions into environment
source(here("0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Manuscript","Figures")
figs.app.folder <- here("Appendix","Figures")
dat.folder <- here("Data")
mort.folder <- here("Data","LT")

# reading countries one by one 

dis<-fread(here(dat.folder,"all_prev_wlk.csv")) %>%  
  rename(sex=gender) %>% 
  select(-1)

#------------------------------------------------------------------------#
# US
#------------------------------------------------------------------------#

us_mort_f<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  filter(country%in%"US" & age>=65 & sex%in%"woman" )

us_mort_m<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  filter(country%in%"US" & age>=65 & sex%in%"man" )


us_dis_f<-dis %>% 
  filter(country%in%"US" & age>=65 & sex%in%"woman" )

us_dis_m<-dis %>%
  filter(country%in%"US" & age>=65 & sex%in%"man" )


# USA
mxwx.f.US <- c(us_mort_f$nMx,us_dis_f$Impair)

mxwx.m.US <- c(us_mort_m$nMx,us_dis_m$Impair)


# men
HL.m.US = Sullivan.fun(rates=mxwx.m.US)
HL.m.US

# woman
HL.f.US = Sullivan.fun(rates=mxwx.f.US)
HL.f.US

# The gender gap in CDFLE was:
gap.US = HL.f.US - HL.m.US
gap.US

# the gender gap in le at age 60 is

us_mort_f$ex[1]-us_mort_m$ex[1]


HE_Decomp_Cont.US <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.US, 
                              pars2 = mxwx.f.US,
                              N=20)
sum(HE_Decomp_Cont.US [1:5])
sum(HE_Decomp_Cont.US [6:10])


#------------------------------------------------------------------------#
# China
#------------------------------------------------------------------------#

china_mort_f<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  filter(country%in%"China" & age>=60 & sex%in%"woman" )

china_mort_m<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  filter(country%in%"China" & age>=60 & sex%in%"man" )



china_cron_f<-fread(here(dis.folder,"all_prev_cron.csv")) %>%  
  rename(sex=gender) %>% 
  filter(country%in%"China" & age>=60 & sex%in%"woman" )

china_cron_m<-fread(here(dis.folder,"all_prev_cron.csv")) %>% 
  rename(sex=gender) %>% 
  filter(country%in%"China" & age>=60 & sex%in%"man" )


# China
mxwx.f.china <- c(china_mort_f$nMx,china_cron_f$unhealthy)

mxwx.m.china <- c(china_mort_m$nMx,china_cron_m$unhealthy)


# men
HL.m.china = Sullivan.fun(rates=mxwx.m.china)
HL.m.china

# woman
HL.f.china = Sullivan.fun(rates=mxwx.f.china)
HL.f.china

# The gender gap in CDFLE was:
gap.china = HL.f.china - HL.m.china
gap.china

# the gender gap in le at age 60 is

china_mort_f$ex[1]-china_mort_m$ex[1]


HE_Decomp_Cont.china <- horiuchi(func=Sullivan.fun,
                                 pars1 = mxwx.m.china, 
                                 pars2 = mxwx.f.china,
                                 N=20)
sum(HE_Decomp_Cont.china [1:5])
sum(HE_Decomp_Cont.china [6:10])


#------------------------------------------------------------------------#
# Mexico
#------------------------------------------------------------------------#

mex_mort_f<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"woman" )

mex_mort_m<-fread(here(mort.folder,"lt_un_abridged.csv")) %>%  
  mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"man" )


mex_cron_f<-fread(here(dis.folder,"all_prev_cron.csv")) %>%  
  rename(sex=gender) %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"woman" )

mex_cron_m<-fread(here(dis.folder,"all_prev_cron.csv")) %>% 
  rename(sex=gender) %>% 
  filter(country%in%"Mexico" & age>=60 & sex%in%"man" )


# Mexico
mxwx.f.mex <- c(mex_mort_f$nMx,mex_cron_f$unhealthy)

mxwx.m.mex <- c(mex_mort_m$nMx,mex_cron_m$unhealthy)


# men
HL.m.mex = Sullivan.fun(rates=mxwx.m.mex)
HL.m.mex

# woman
HL.f.mex = Sullivan.fun(rates=mxwx.f.mex)
HL.f.mex

# The gender gap in CDFLE was :
gap.mex = HL.f.mex- HL.m.mex
gap.mex

# quite amazing how this result is much more striking for Mexico
# than only looking into DFLE. Is this reasonable or something with
# data/my calculations?

# the gender gap in le at age 60 is

mex_mort_f$ex[1]-mex_mort_m$ex[1]


HE_Decomp_Cont.mex <- horiuchi(func=Sullivan.fun,
                               pars1 = mxwx.m.mex, 
                               pars2 = mxwx.f.mex,
                               N=20)
sum(HE_Decomp_Cont.mex [1:5])
sum(HE_Decomp_Cont.mex [6:10])


# here for England as well since the life table is from ONS

#------------------------------------------------------------------------#
# England
#------------------------------------------------------------------------#

# life table from england comes from the ONS so here we have to use a 
# different one

en_mort_f<-fread(here(mort.folder,"lt_eng_abridged.csv")) %>%  
  # mutate(sex = fct_recode(sex, "woman" = "Female")) %>% 
  mutate(country = "England") %>% 
  filter(country%in%"England" & age>=60 & sex%in%"woman" )

en_mort_m<-fread(here(mort.folder,"lt_eng_abridged.csv")) %>%  
  #mutate(sex = fct_recode(sex, "man" = "Male")) %>% 
  mutate(country = "England") %>% 
  filter(country%in%"England" & age>=60 & sex%in%"man" )

en_cron_f<-fread(here(dis.folder,"all_prev_cron.csv")) %>%  
  rename(sex=gender) %>% 
  filter(country%in%"England" & age>=60 & sex%in%"woman" )

en_cron_m<-fread(here(dis.folder,"all_prev_cron.csv")) %>% 
  rename(sex=gender) %>% 
  filter(country%in%"England" & age>=60 & sex%in%"man" )


# England
mxwx.f.en <- c(en_mort_f$nMx,en_cron_f$unhealthy)

mxwx.m.en <- c(en_mort_m$nMx,en_cron_m$unhealthy)


# men
HL.m.en = Sullivan.fun(rates=mxwx.m.en)
HL.m.en

# woman
HL.f.en = Sullivan.fun(rates=mxwx.f.en)
HL.f.en

# The gender gap in CDFLE was:
gap.en = HL.f.en - HL.m.en
gap.en

# the gender gap in le at age 60 is

en_mort_f$ex[1]-en_mort_m$ex[1]


HE_Decomp_Cont.en <- horiuchi(func=Sullivan.fun,
                              pars1 = mxwx.m.en, 
                              pars2 = mxwx.f.en,
                              N=20)
sum(HE_Decomp_Cont.en [1:5])
sum(HE_Decomp_Cont.en [6:10])


# continue for other countries...
