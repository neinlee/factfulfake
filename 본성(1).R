library(dplyr)
library(ggplot2)
library(ggthemes)



################################
#그림1##########################
################################
#Brecke - Conflict Catalog (1999); Population based on various sources (2023) – with major processing by Our World in Data. “Global death rate in violent political conflicts over the long-run” [dataset]. Brecke, “Conflict Catalog”; Various sources, “Population” [original data]. Retrieved February 14, 2024 from https://ourworldindata.org/grapher/global-death-rate-in-violent-political-conflicts-over-the-long-run


owid_conflict <- read.csv('owid_deaths-in-conflicts-by-source.csv')
conflict <- owid_conflict%>%filter(Entity=='death rate (per 100,000)')
conflict <- conflict%>%mutate(century=Year-Year%%100, years=Year-century)

fig5.1 <- 
  ggplot(data=conflict%>%filter(!Conflict.Catalogue...Total==0), 
       aes(x=Year, y=Conflict.Catalogue...Total))+
  geom_line()+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100),
                labels=c('','', '', '', ''))+
  labs(title='\n', 
       x='', y='\n',caption='')+
  theme_few()+
  stat_smooth(formula = y~x, method = 'lm', se=F, 
              linetype='dashed', color='black', size=0.5)+
  theme(plot.caption = element_text(size=12),
        plot.subtitle = element_text(size=8),
        axis.text.x = element_blank()) ############The long Graph (1400-2000)


ggsave("5장 1.png", fig5.1, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림2##########################
################################

fig5.2 <- 
  ggplot(data=conflict%>%filter(!Conflict.Catalogue...Total==0), 
         aes(x=Year, y=Conflict.Catalogue...Total))+
  geom_line()+
  scale_y_continuous(breaks=c(0, 100, 200, 300),
                labels=c('','', '', ''))+
  labs(title='\n', 
       x='', y='\n',caption='')+
  theme_few()+
  stat_smooth(formula = y~x, method = 'lm', se=F, 
              linetype='dashed', color='black', size=0.5)+
  theme(plot.caption = element_text(size=12),
        plot.subtitle = element_text(size=8),
        axis.text.x = element_blank()) ############The long Graph (1400-2000)


ggsave("5장 2.png", fig5.2, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림3##########################
################################
fig5.3 <- 
  ggplot()+
  geom_line(data=conflict%>%filter(century==1900),
            aes(x=years, y=Conflict.Catalogue...Total), size=1)+
  geom_line(data=conflict%>%filter(century<1600),
            aes(x=years, y=Conflict.Catalogue...Total, 
                discrete=factor(century), linetype=factor(century),
                color=factor(century)), size=0.2)+
  scale_linetype_stata(limits= c('1900','1400','1500'), breaks=c(1900,1400,1500),
                       labels=c('20c', '15c', '16c'))+
  scale_color_grey(limits = c('1900','1400','1500'), breaks=c(1900,1400,1500), 
                   labels=c('20c', '15c', '16c'),
                   start = 0, end = 0.2)+
  scale_y_log10(breaks=c(0.1,1,10,100), 
                labels=c('','','',''))+
  xlim(c(0,100))+
  labs(title='\n', x='', y='\n',caption='')+
  theme_few()+theme(plot.caption = element_text(size=12),
                    legend.title = element_blank(),
                    legend.position = c(0.875,0.78),
                    legend.text = element_blank(),
                    axis.text.x = element_blank()) #######15c, 16c, 20c

ggsave("5장 3.png", fig5.3, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림4##########################
################################
fig5.4 <- 
  ggplot()+
  geom_line(data=conflict%>%filter(century==1900),
            aes(x=years, y=Conflict.Catalogue...Total), size=1)+
  geom_line(data=conflict%>%filter(century>=1600, century!=1900, !Conflict.Catalogue...Total==0),
            aes(x=years, y=Conflict.Catalogue...Total, 
                linetype=factor(century), color=factor(century)), 
            size=.2)+
  scale_y_log10(breaks=c(0.1,1,10,100),
                labels=c('','','',''))+
  scale_linetype_stata(limits= c('1900','1600','1700','1800'), breaks=c(1900,1600,1700,1800),
                       labels=c('', '', '', ''))+
  scale_color_grey(limits= c('1900','1600','1700','1800'), breaks=c(1900,1600,1700,1800),
                   labels=c('', '', '', ''),
                   start = 0, end = 0.5)+
  xlim(c(0,100))+
  annotate('segment', x=89, xend=89, y=0, yend=60)+
  annotate('text', x=89, y=120, label='')+
  labs(title='\n', x='', y='\n',caption='')+
  theme_few()+theme(plot.caption = element_text(size=12),
                    legend.title = element_blank(),
                    legend.position = 'bottom',
                    axis.text.x = element_blank()) ####### 17~20c linear

ggsave("5장 4.png", fig5.4, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림5##########################
################################

fig5.5 <- ggplot()+
  geom_line(data=conflict%>%filter(century==1900),
            aes(x=years, y=Conflict.Catalogue...Total), size=1)+
  geom_line(data=conflict%>%filter(century!=1900),
            aes(x=years, y=Conflict.Catalogue...Total, 
                discrete=factor(century), linetype=factor(century),
                color=factor(century)), size=.2)+
  scale_linetype_stata(limits=c('1900','1400','1500','1600','1700','1800'),
                       labels=c('', '', '', '', '', ''))+
  scale_color_grey(limits=c('1900','1400','1500','1600','1700','1800'),
                   labels=c('', '', '', '', '', ''),
                   start = 0, end = 0.2)+
  xlim(c(0,100))+
  labs(title='\n', x='', y='\n',
       caption='')+
  theme_few()+theme(plot.caption = element_text(size=12),
                    legend.title = element_blank(),
                    legend.position = c(0.625,0.6),
                    legend.text = element_blank(),
                    axis.text = element_blank()) #######15c ~ 20c linear

ggsave("5장 5.png", fig5.5, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림6##########################
################################
fig5.6 <- 
  ggplot(data=conflict%>%filter(Year>=1500, !Conflict.Catalogue...Military.only==0), 
       aes(x=Year, y=Conflict.Catalogue...Military.only))+
  geom_line()+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100),
                labels=c('','', '', '', ''))+
  labs(title='\n',
       x='', y='\n',
       caption='')+
  theme_few()+
  theme(plot.caption = element_text(size=12),
        plot.subtitle = element_text(size=8),
        axis.text.x = element_blank()) +
  stat_smooth(formula = y~x, method = 'lm', se=F, 
              linetype='dashed', color='black', size=0.5) ############The long Graph military only (1500-2000)

ggsave("5장 6.png", fig5.6, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림7##########################
################################
#CoW CC Graph (1815-2000)##########################################################################
cowcclog <-
  ggplot()+
  geom_line(data=conflict%>%filter(Year>=1800, !Conflict.Catalogue...Military.only==0),
            aes(x=Year, y=Conflict.Catalogue...Military.only, linetype='solid'),
            size=.2, alpha=.5)+
  geom_line(data=conflict%>%filter(Year>=1800), 
            aes(x=Year, y=Correlates.of.War, linetype='longdash'),
            size=.2, alpha=1)+
  
  scale_y_log10(breaks=c(0.01,0.1,1,10,100),
                labels=c('','', '', '', ''))+
  labs(title='',
       x='', y='\n',caption='')+
  theme_few()+
  scale_linetype_manual(values = c('solid','dashed'),
                        labels = c('CoW', 'CC'))+
  annotate('segment', x=1989, xend=1989, y=0, yend=30, size=0.1)+
  annotate('text', x=1989, y=60, label='')+
  theme(plot.caption = element_text(size=12),
        legend.position = c(0.2,0.85),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text = element_blank()) ############log Graph (1815-2000)

#linear##
cowcclinear <- 
  ggplot()+
  geom_line(data=conflict%>%filter(Year>=1800, !Conflict.Catalogue...Military.only==0),
            aes(x=Year, y=Conflict.Catalogue...Military.only, linetype='solid'),
            size=.2, alpha=.5)+
  geom_line(data=conflict%>%filter(Year>=1800), 
            aes(x=Year, y=Correlates.of.War, linetype='longdash'),
            size=.2, alpha=1)+
  labs(title='',
       x='', y='')+
  theme_few()+
  scale_linetype_manual(values = c('solid','dashed'),
                        labels = c('CoW', 'CC'))+
  theme(plot.caption = element_text(size=12),
        legend.position = c(0.9,0.825),
        legend.title = element_blank(),
        legend.text = element_blank(),
        axis.text = element_blank()) ############The long Graph linear (1815-2000)

fig5.7 <- cowcclinear/cowcclog
ggsave("5장 7.png", fig5.7, dpi = 300, width = 6, height = 8, units = "in")



################################
#그림8##########################
################################

#비국가 사회 폭력 사망률(전체 인구 십만명당)
#Bowles, S. (2009), Gat, A. (2008), Knauft, B. M. et al (1987), Keeley, L. H. (1996), Pinker, S. (2011), and Walker, R. S., & Bailey, D. H. (2013) – processed by Our World in Data. “Rate of violent deaths (non-state societies)” [dataset]. Bowles, S. (2009), Gat, A. (2008), Knauft, B. M. et al (1987), Keeley, L. H. (1996), Pinker, S. (2011), and Walker, R. S., & Bailey, D. H. (2013) [original data].
owid_rate.ns <- read.csv('rate-of-violent-deaths-non-state-societies.csv')
colnames(owid_rate.ns)[4] <- 'death_rate'
owid_rate.ns <- owid_rate.ns%>%mutate(group='group_2(non_state)')
owid_rate.ns[1, "Entity"] <- "!Kung Bushmen (Kalahari), with a state authority"
owid_rate.ns[2, "Entity"] <- "!Kung Bushmen (Kalahari), without a state authority"



#국가 사회 폭력 사망률(전체 인구 십만명당)
#Bowles, S. (2009), Gat, A. (2008), Knauft, B. M. et al (1987), Keeley, L. H. (1996), Pinker, S. (2011), and Walker, R. S., & Bailey, D. H. (2013) – processed by Our World in Data. “Rate of violent deaths (state societies)” [dataset]. Bowles, S. (2009), Gat, A. (2008), Knauft, B. M. et al (1987), Keeley, L. H. (1996), Pinker, S. (2011), and Walker, R. S., & Bailey, D. H. (2013) [original data].
owid_rate.s <- read.csv('rate-of-violent-deaths-state-societies.csv')
colnames(owid_rate.s)[4] <- 'death_rate'
owid_rate.s <- owid_rate.s%>%mutate(group='group_1(state)')
owid_rate.s[7, "Entity"] <- "Tepoztlan (Mexico); 1922 - 1955 CE"


#################################################
# owid_ns_s <- read.csv('owid_violence_ns_vs_s.csv')
owid_ns_s <- rbind(owid_rate.ns, owid_rate.s)
View(owid_ns_s)

library(ggpubr)

fig5.8 <- 
  ggbarplot(owid_ns_s, x = "Entity", y = "death_rate",
            fill = "group",               # change fill color by group
            palette = "grey",             # grey color palette
            sort.val = "asc",             # Sort the value in ascending order
            sort.by.groups = TRUE,        # Sort inside each group
            levels.order = c("non-state", "state")) +  # Specify the order of levels
  coord_flip() +
  labs(title = '\n', y = '', x = '', caption = '') +
  annotate('text', x = 5.5, y = 750, label = '', size = 7) +
  annotate('text', x = 26, y = 780, label = '', size = 7) +
  theme_few() +
  theme(plot.caption = element_text(size = 18),
        legend.position = '',
        plot.title = element_text(hjust = 1, size = 27),
        axis.title.x = element_text(size = 17),
        axis.text.x = element_blank())

ggsave("5장 8.png", fig5.8, dpi = 300, width = 6, height = 8, units = "in")



################################
#그림13#########################
################################
# V-Dem (2023) – with major processing by Our World in Data. “Central estimate” [dataset]. V-Dem, “Democracy and Human rights, OWID based on Varieties of Democracy (v13) and Regimes of the World v13” [original data].
owid_libdem <- read.csv('liberal-democracy-index.csv')

inddm <-
  ggplot(owid_libdem%>%filter(Code=='IND',Year>=1990),
         aes(x=Year, y=libdem_vdem_owid))+
  geom_ribbon(aes(ymin=0.3, ymax=libdem_vdem_owid), 
              color='grey', alpha=.25)+
  geom_line(size=1.2)+  
  ylim(c(0.3,0.6))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020),
                     labels = c('','','',''))+
  labs(title='', x='', y='\n')+
  annotate('segment', x=2013, xend=2013, y=0.3, yend=0.57, 
           linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

phldm <- 
  ggplot(owid_libdem%>%filter(Code=='PHL',Year>=1990), 
         aes(x=Year, y=libdem_vdem_owid))+
  geom_ribbon(aes(ymin=0.2, ymax=libdem_vdem_owid), 
              color='grey', alpha=.25)+
  geom_line(size=1.2)+  
  ylim(c(0.2,0.5))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020),
                     labels = c('','','',''))+
  labs(title='', x='', y='\n')+
  annotate('segment', x=2015, xend=2015, y=0.2, yend=0.455, linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

bradm <- 
  ggplot(owid_libdem%>%filter(Code=='BRA',Year>=1990), aes(x=Year, y=libdem_vdem_owid))+
  geom_ribbon(aes(ymin=0.475, ymax=libdem_vdem_owid), 
              color='grey', alpha=.25)+
  geom_line(size=1.2)+
  ylim(c(0.475,0.85))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020),
                     labels = c('','','',''))+
  labs(title='', x='', y='')+
  annotate('segment', x=2015, xend=2015, y=0.475, yend=0.815, linetype='dashed')+
  annotate('segment', x=2018, xend=2018, y=0.475, yend=0.675, linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

hundm <- 
  ggplot(owid_libdem%>%filter(Code=='HUN',Year>=1990), aes(x=Year, y=libdem_vdem_owid))+
  geom_ribbon(aes(ymin=0.3, ymax=libdem_vdem_owid), 
              color='grey', alpha=.25)+
  geom_line(size=1.2)+
  scale_y_continuous(breaks = c(0.4,0.6,0.8),
                     labels = c('','',''))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020),
                     labels = c('','','',''))+
  labs(title='', x='', y='\n')+
  annotate('segment', x=2009, xend=2009, y=0.3, yend=0.805, linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

poldm <- 
  ggplot(owid_libdem%>%filter(Code=='POL',Year>=1990),
                aes(x=Year, y=libdem_vdem_owid))+
  geom_ribbon(aes(ymin=0.35, ymax=libdem_vdem_owid), 
              color='grey', alpha=.25)+
  geom_line(size=1.2)+
  ylim(c(0.35,1))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020),
                     labels = c('','','',''))+
  labs(title='', x='', y='\n',
       caption='')+
  annotate('segment', x=2015, xend=2015, y=0.35, yend=0.87, linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

rusdm <- 
  ggplot(owid_libdem%>%filter(Code=='RUS',Year>=1990), 
         aes(x=Year, y=libdem_vdem_owid))+
  geom_ribbon(aes(ymin=0.05, ymax=libdem_vdem_owid), 
              color='grey', alpha=.25)+
  geom_line(size=1.2)+
  ylim(c(0.05,0.35))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020),
                     labels = c('','','',''))+
  labs(title='', x='', y='\n')+
  annotate('segment', x=1999, xend=1999, y=0.05, yend=0.317, linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

usadm <- ggplot(owid_libdem%>%filter(Code=='USA',Year>=1990),
                aes(x=Year, y=libdem_vdem_owid))+
  geom_ribbon(aes(ymin=0.65, ymax=libdem_vdem_owid), 
              color='grey', alpha=.25)+
  geom_line(size=1.2)+
  ylim(c(0.65,0.95))+
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020),
                     labels = c('','','',''))+
  labs(title='', x='', y='\n')+
  annotate('segment', x=2015, xend=2015, y=0.65, yend=0.885, linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

fig5.13 <- (usadm+rusdm)/(inddm+phldm)/(bradm+poldm)

ggsave("5장 13.png", fig5.13, dpi = 300, width = 6, height = 9, units = "in")



################################
#그림14#########################
################################
fig5.14 <- 
  ggplot(libdem%>%filter(Entity=='World',Year>=1950), 
       aes(x=Year, y=libdem))+
  coord_cartesian(ylim = c(0.2, 0.5))+
  geom_ribbon(aes(ymin=0.2, ymax=libdem), 
              color='grey', alpha=.25)+
  geom_line(size=1.2)+ylim(c(0.2,0.5))+
  labs(title='', subtitle = '', x='', y='\n',
       caption='')+
  annotate('point', x=1989, y=0.3037815, size=2.5)+
  annotate('point', x=2012, y=0.3845459, size=2.5)+
  annotate('point', x=2022, y=0.2991115, size=2.5)+
  annotate('segment', x=1989, xend=1989, y=0.2, yend=0.425, 
           linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

ggsave("5장 14.png", fig5.14, dpi = 300, width = 6, height = 4, units = "in")
