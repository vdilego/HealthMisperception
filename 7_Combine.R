#-----------------------------------------------------------#
# Combining all surveys   ----------------------------------#
# Author: Vanessa di Lego  #
#-----------------------------------------------------------#


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
library(forcats)
library(broom)
library(here)
library(ggpubr)
library(ggExtra)
library(ggthemes)
library(ggrepel)
library(Hmisc)
library(devtools)
library(forcats)
library(wesanderson)
library(ggthemes)
library(colorspace)
library(cowplot)
library(grid)
library(DemoDecomp) 
library(gridExtra)
library(DemoTools)
library(DemoToolsData)
require(openxlsx)
library(readxl)
library(rio)

# reading and combining all prevalences of  into one file

#China

charls_dis<-fread(here("Data","CHARLS","tested_reported_all_charls.csv")) %>% 
  mutate(country= "China",
         gender=as.factor(gender))

#USA

hrs_dis<-fread(here("Data","HRS","tested_reported_all_hrs.csv")) %>% 
  mutate(country="US",
         gender=as.factor(gender)) 

  hrs_dis$gender<- fct_recode( hrs_dis$gender, "woman" = "female", "man" = "male")
  hrs_dis$gender<-fct_relevel(hrs_dis$gender, "man")

#England

elsa_dis<-fread(here("Data","ELSA","tested_reported_all_elsa.csv")) %>% 
  mutate(country="England",
         gender=as.factor(gender))


#India
lasi_dis<-fread(here("Data","LASI","tested_reported_all_lasi.csv")) %>% 
  mutate(country="India",
         gender=as.factor(gender))


#Mexico

mhas_dis<-fread(here("Data","MHAS","tested_reported_all_mhas.csv")) %>% 
  mutate(country="Mexico",
         gender=as.factor(gender)) 

mhas_dis$gender<- fct_recode( mhas_dis$gender, "man" = "Man", "woman" = "Woman")






#Europe

#all countries and then pooled countries

share_dis_p<-fread(here("Data","SHARE","share_walk_pooled.csv")) %>% 
  mutate(country="Europe")


share_dis_c<-fread(here("Data","SHARE","share_walk_country.csv"))

share_dis_all<- full_join(share_dis_c,share_dis_p)



# joining all countries impair walking

all_prev_wlk<-rbind(charls_dis,hrs_dis,elsa_dis,lasi_dis, mhas_dis)

write.table(all_prev_wlk, here("Data", "all_prev_wlk.csv"),sep = ",",row.names = F) 

