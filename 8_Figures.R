#-----------------------------------------------------------#
# Figures                 ----------------------------------#
# Author: Vanessa di Lego  #
#-----------------------------------------------------------#

# Calculation and decomposition of DFLE


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
library(ggthemes)
library(colorspace)
library(cowplot)
library(grid)
library(DemoDecomp) 
library(gridExtra)
library(forcats)


# Loading useful functions into environment
source(here("0_Functions.R"))
options(scipen=999)

# creating directory folders where outputs are saved
figs.folder <- here("Manuscript","Figures")
dat.folder <- here("Data")
figs.app.folder <- here("Appendix","Figures")


#dir.create(figs.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(dat.folder, showWarnings = FALSE, recursive = TRUE)
#dir.create(figs.app.folder, showWarnings = FALSE, recursive = TRUE)

# reading walking impairment data for all countries at the same time

dis<-fread(here(dat.folder,"all_prev_wlk.csv")) %>%  
  rename(sex=gender) %>% 
  select(-1)


fig1<-ggplot(dis, aes(age, country, group=country, fill=Impair))+
  geom_raster(interpolate = T) +
  # geom_tile (color="grey80") +
  theme_clean(base_size = 26)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90))+
  facet_grid(sex~type)+
  scale_fill_distiller(palette = "Spectral", name="%Unhealthy", n.breaks=4)


pdf(here(figs.folder,"fig_prev_country.pdf"), width = 18, height=8)
fig1
dev.off()

X11()




 ggplot(dis %>% filter(!country%in%"Mexico"),aes(age, Impair, group=country, color=country), size=1.5)+
  geom_line(size=2)+
  theme_clean(base_size =26)+
  theme(legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        legend.title  = element_text(size=18),
        legend.text = element_text(size=18),
        axis.text.x = element_text(angle = 90),
        panel.border = element_blank(),
        plot.background = element_blank())+
  facet_grid(sex~type)+
  #scale_color_brewer(palette = "PuOr")
  scale_color_manual(values=c("#ab864a","#5d7dc3","#0d3173",
                              "#ba1e68", "#6ba772"))

pdf(here(figs.folder,"fig_3.pdf"), width = 10, height=10)
fig_selected
dev.off()

# by country


X11()
dis<-dis %>% filter(!country%in%"Mexico")
dis_uk<-dis %>% filter(country%in%"England")
  
ggplot(dis_uk ,
       aes(age, Impair, group=sex,color=sex, shape=sex))+
  geom_line(size=1)+
  geom_point(size=2.7, alpha=0.7)+
  geom_line(data=dis_uk,aes(age, CI_low, 
                           group=sex,color=sex, shape=sex), size=0.3, linetype="dashed")+
  
  geom_line(data=dis_uk,aes(age, CI_up, 
                           group=sex,color=sex, shape=sex), size=0.3, linetype="dashed")+
  theme_clean(base_size = 16)+
  theme(legend.position = "bottom", legend.background = element_rect(color = NA))+
  ylab("Prevalence Unhealthy")+
  facet_grid(.~type)+
  #  scale_x_discrete(labels = c(50,55,60,65,70,75,80,85,90))+
  #scale_color_manual(values = c("brown","darkblue"))+
  scale_color_manual(values = c('#009988',"#882255")) 



ggplot() +
  geom_bar(data= dis %>%
             arrange(Impair) %>% 
             group_by(type) %>% 
             mutate(country=fct_reorder(country, Impair,.desc = T)),
           aes(x=country, y=Impair, fill=factor(sex)),
           stat = "identity", position = "dodge")+
  #  ggtitle(bquote(~'Germany (SHARE)' ))+
  xlab("Age") +ylab(" ")+
  theme (plot.title = element_text(size = 10))+
  # geom_bar(stat = "identity", position = "stack")+ 
  #geom_errorbar(data= outgap.ci,
  #              aes(x=Country, ymin=l, ymax=u, 
  #                  color=factor(type, levels=c("Mortality","Disability"))),
  #              width=0.5, alpha=0.5, size=1.2, show.legend = F, position=position_dodge(width=0.5))+
  scale_fill_manual(values=alpha(c( "blue", "darkred"),0.5))+
  scale_color_manual(values=alpha(c("blue","darkred"),0.7))+
  #scale_fill_manual(values=alpha(c( "#A50026", "#4575B4")))+
  
  # ylim(-1.2, 1.7)+
  geom_hline(yintercept=0, linetype="dashed",  color = "black", size=0.5)+
  labs(fill = "Sex")+
  theme_minimal(base_size = 12) +
  # facet_wrap(.~Country, ncol = 4)+
  theme(legend.text=element_text(size=9),
        legend.title=element_text(size=10),
        axis.title =  element_text(size=12),title =  element_text(size=12),
        legend.position = "bottom", 
        legend.background = element_rect(color = NA),
        axis.text.x = element_text( vjust = 0.3, hjust = 1))+
  facet_grid(type~age)#+
  coord_flip()




library(ggalt)
library(ggtext)
library(extrafont)

# making  a dumbbell to check
dis_cron_bell<-dis %>% 
  pivot_wider(id_cols = c(age,country,sex), names_from = type, values_from = Impair)


ggplot(dis_cron_bell %>%
         arrange(tested) %>%
         #  filter(!location%in%"Canada") %>% 
         mutate(country=fct_reorder(country, tested,.desc = T)))+ 
  geom_dumbbell(size=1, color="black",
                aes(y = country, x=tested, xend=reported_100, color=type),
                size_x = 2.5, size_xend = 2.5, colour_x = '#B6407D', colour_xend = '#11718A') +
  facet_grid(sex~age)+
  theme_clean()+
  theme(axis.text.x = element_text( vjust = 0.3, hjust = 1, angle = 90))



# Helper function for string wrapping because names are too large in the facets
# Default 20 character target width.
swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}

swr = Vectorize(swr)

prev.all$type = swr(prev.all$type)


# bump chart


# doing a rank grphs
# first ranking according to different metrics


decomp.rankings.le <- decomp.all %>% 
  #group_by(Country) %>% 
  arrange((GAP_LE), Country) %>% 
  mutate(ranking.le = row_number()) %>% 
  as.data.frame()

decomp.rankings.dfle <- decomp.rankings.le  %>% 
  #group_by(Country) %>% 
  arrange((GAP_DFLE), Country) %>% 
  mutate(ranking.dfle = row_number()) %>% 
  as.data.frame()


decomp.rankings.dis <- decomp.rankings.dfle  %>% 
  #group_by(Country) %>% 
  arrange(desc(Disability), Country) %>% 
  mutate(ranking.disability = row_number()) %>% 
  as.data.frame()

decomp.rankings.cron <- decomp.rankings.dis  %>% 
  #group_by(Country) %>% 
  arrange(desc(Chronic), Country) %>% 
  mutate(ranking.chronic = row_number()) %>% 
  as.data.frame()

decomp.rankings.cron.gap <- decomp.rankings.cron %>% 
  #group_by(Country) %>% 
  arrange(desc(GAP_CFLE), Country) %>% 
  mutate(ranking.cfle = row_number()) %>% 
  as.data.frame()

rank.values<-decomp.rankings.cron.gap  %>% 
  select(1, 12:16) %>% 
  pivot_longer(2:6,names_to ="Type",values_to = "Rank" )

rank.values$tag<-factor(rank.values$Type, 
                        levels = c("ranking.cfle","ranking.chronic","ranking.dfle", 
                                   "ranking.disability", "ranking.le"),
                        labels=c("GAP CFLE","CHRONIC","GAP DFLE","DISABILITY","GAP LE"))


rank.values<-rank.values %>% 
  mutate(tag=fct_relevel(tag,c("GAP DFLE","DISABILITY","GAP CFLE","CHRONIC","GAP LE"))) %>%
  arrange(tag)



library(ggbump)
# then plot basic bump chart
X11()

r.bump<-ggplot(rank.values,
               aes(x = tag, y = Rank, color = Country, group=Country)) +
  geom_bump(smooth = 15, size = 2, alpha = 0.2, color="grey70")


rank_dis<-r.bump + 
  geom_bump(data= rank.values%>% 
              filter(Country%in%c("Portugal","Korea","Denmark","US", "India")) ,
            aes(x = tag, y = Rank, color = Country, group=Country),
            smooth = 15, size = 2, inherit.aes = F)+
  
  geom_point(data= rank.values,
             aes(x = tag, y = Rank),
             size = 5,  alpha = 0.5,color="grey70") +
  geom_point(data= rank.values %>% 
               filter(Country%in%c("Portugal","Korea","Denmark","US", "India")),
             aes(x = tag, y = Rank),
             size = 5) +
  geom_segment(data =rank.values,
               aes(x = tag , xend = tag , y = Rank, yend = Rank),
               size = 5,  alpha = 0.5,color="grey70",
               lineend = "round")+
  
  geom_segment(data =rank.values %>% 
                 filter(Country%in%c("Portugal","Korea","Denmark","US", "India")),
               aes(x = tag , xend = tag , y = Rank, yend = Rank),
               size = 2.5,alpha = 0.5,
               lineend = "round")+
  
  geom_text(data = rank.values %>% filter(!Country%in%c("Portugal","Korea","Denmark","US", "India")),
            aes(label = Country, x = tag),
            size = 3,  alpha = 0.5,color="grey70",
            #  color = "white",
            nudge_y = .43,
            nudge_x = -.05,
            size = 3.5,
            fontface = 2,
            hjust = 0) +
  
  geom_text(data = rank.values %>% filter(Country%in%c("Portugal","Korea","Denmark","US", "India")),
            aes(label = Country, x = tag),
            #  color = "white",
            nudge_y = .5,
            nudge_x = -.07,
            size = 5,
            fontface = 2,
            hjust = 0) +
  scale_color_manual(values=c("#ab864a","#5d7dc3","#0d3173",
                              "#ba1e68","#6ba772","black"))+
  # scale_color_manual(values =c("#CA0020", "#F4A582", "#F7F7F7", "#92C5DE", "#0571B0","grey90"))+
  scale_y_reverse(breaks = 1:nrow(rank.values))+
  labs(  y = "Rank")+
  labs(  x = "")+
  my_theme()+
  theme(legend.position = "none")


rank_dis

pdf(here("Manuscript","Figures","fig4.pdf"), width = 15, height=10)
rank_dis
dev.off()


# basic version

ggplot(data = rank.values, aes(x = tag, y = Rank, group = Country)) +
  geom_line(aes(color = Country, alpha = 1), size = 2) +
  geom_point(aes(color = Country, alpha = 1), size = 4) +
  scale_y_reverse(breaks = 1:nrow(rank.values))

my_theme <- function() {
  # Colors
  color.background = "white"
  color.text = "#22211d"
  # Begin construction of chart
  theme_bw(base_size=15) +
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    # Format the legend
    theme(legend.position = "none") +
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=18, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=18, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=18, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=18, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}



rank.values <- rank.values %>%
  filter(tag%in%c("GAP DFLE","DISABILITY")) %>% 
  mutate(flag = ifelse(Country %in%c("Portugal","Korea","Denmark","US", "China"), TRUE, FALSE),
         country_col = if_else(flag == TRUE, Country, "zzz"))

ggplot(data = rank.values, aes(x = tag, y = Rank, group = Country)) +
  geom_line(aes(color = country_col, alpha = 1), size = 3) +
  geom_point(color = "#FFFFFF", size = 4) +
  geom_point(aes(color = country_col, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:nrow(rank.values))+
  # scale_x_continuous(breaks = 1:16, minor_breaks = 1:16, expand = c(.05, .05)) +
  geom_text(data = rank.values %>% filter(tag == "GAP DFLE"),
            aes(label = Country, x = 0.8) , fontface = "bold", color = "grey90", size = 6) +
  geom_text(data = rank.values %>% filter(tag == "DISABILITY"),
            aes(label = Country, x = 2.2) , fontface = "bold", color = "grey90", size = 6) +
  
  geom_text(data = rank.values %>% filter(tag == "GAP DFLE" & Country %in%c("Portugal","Korea","Denmark","US", "China")),
            aes(label = Country, x = 0.8, color=Country) , fontface = "bold", size = 6) +
  geom_text(data = rank.values %>% filter(tag == "DISABILITY" &Country %in%c("Portugal","Korea","Denmark","US", "China")),
            aes(label = Country,  x = 2.2, color=Country) , fontface = "bold", size = 6) +
  # coord_cartesian(ylim = c(1,show.top.n)) + 
  theme(legend.position = "none") +
  labs(  y = "Rank")+
  my_theme()+
  
  scale_color_manual(values = c("#CA0020", "#F4A582", "black", "#92C5DE", "#0571B0","grey90"))

 