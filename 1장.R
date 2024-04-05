library(readxl)
library(wbstats)
library(WDI)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)

##################
#우울장애 유병률##
##################
# IHME, Global Burden of Disease (2020) – processed by Our World in Data. “Depressive disorders (share of population) - Sex: Both - Age: Age-standardized” [dataset]. IHME, Global Burden of Disease (2020) [original data].

depression <- read.csv('depressive-disorders-prevalence-ihme.csv')
colnames(depression)[4] <- 'prevalence'

fig1.1 <-
  ggplot(depression%>%filter(Entity=='World', Year>=2000), 
                 aes(x=Year, y=prevalence*1000))+
    geom_area(fill = 'grey', alpha=0.5)+
    geom_line(linewidth=1)+geom_point()+
    labs(title='\n', y='\n\n',
         caption='')+
    xlab('')+
    coord_cartesian(xlim = c(2000, 2019),
                    ylim = c(2000, 5000))+
    theme_few()+
    scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
                     labels = c('','','','',''))+
    scale_y_continuous(breaks = c(2000, 3000,4000,5000),
                     labels = c('','','',''))

ggsave("1장 우울증 유병률.png", 
       fig1.1, dpi = 300, width = 3, height = 3, units = "in")



##################
#성인 비만률######
##################
# WHO, Global Health Observatory (2022) – processed by Our World in Data. “Indicator:Prevalence of obesity among adults, BMI &GreaterEqual; 30 (crude estimate) (%) - Sex:Both sexes” [dataset]. WHO, Global Health Observatory (2022) [original data].
obesity <- read.csv('share-of-adults-defined-as-obese.csv')
colnames(obesity)[4] <- 'prevalence'
tail(obesity)

fig1.2 <-
  ggplot(obesity%>%filter(Entity=='World', Year>=2000), 
         aes(x=Year, y=prevalence))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1.2)+geom_point()+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(xlim = c(2000, 2016),
                  ylim = c(4, 18))+
  theme_few()+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015),
                     labels = c('','','',''))+
  scale_y_continuous(breaks = c(5, 10, 15),
                     labels = c('','',''))

ggsave("1장 성인 비만률.png", 
       fig1.2, dpi = 300, width = 3, height = 3, units = "in")




##################
#민주주의 인구####
##################

popdem <- read.csv('people-living-in-democracies-autocracies.csv')
colnames(popdem)[4:8] <- c('missreg', 'closedaut', 'electaut',
                           'electdem', 'libdem')
popdem <- popdem%>%mutate(rate=(electdem+libdem)/(missreg+closedaut+electaut+electdem+libdem))
tail(popdem)

fig1.5 <-
  ggplot(popdem%>%filter(Entity == 'World', Year>=2000), 
         aes(x=Year, y=rate*100))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1)+geom_point()+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(xlim = c(2000, 2022),
                  ylim = c(0, 80))+
  theme_few()+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
                     labels = c('','','','', ''))+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80),
                     labels = c('','','','',''))

ggsave("1장 민주주의 체제에 사는 인구.png", 
       fig1.5, dpi = 300, width = 3, height = 3, units = "in")



##################
#자유민주주의#####
##################

libdem <- read.csv('liberal-democracy-index-popw-vdem.csv')
colnames(libdem)[5] <- c('libdem')
tail(libdem)

fig1.6 <-
  ggplot(libdem%>%filter(Entity=='World', Year>=2000), 
         aes(x=Year, y=libdem))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1)+
  geom_point()+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(xlim = c(2000, 2022),
                  ylim = c(0.2, 0.5))+
  theme_few()+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
                     labels = c('','','','', ''))+
  scale_y_continuous(breaks = c(0.2, 0.3, 0.4, 0.5),
                     labels = c('','','',''))

ggsave("1장 자유민주주의.png", 
       fig1.6, dpi = 300, width = 3, height = 3, units = "in")



##################
#생물다양성#######
##################
#https://ourworldindata.org/grapher/global-living-planet-index
biodiv <- read.csv('global-living-planet-index.csv')
tail(biodiv)

fig1.9 <-
  ggplot(biodiv%>%filter(Entity == 'World', Year>=2000), 
         aes(x=Year, y=Living.Planet.Index/100))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1)+
  geom_point()+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(xlim = c(2000, 2018),
                  ylim = c(0.3, 0.5))+
  theme_few()+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
                     labels = c('','','','', ''))+
  scale_y_continuous(breaks = c(0.3, 0.35, 0.4, 0.45, 0.5),
                     labels = c('','','','',''))

ggsave("1장 생물다양성.png", 
       fig1.9, dpi = 300, width = 3, height = 3, units = "in")



##################
#멸종위기종#######
##################

redlist <- read.csv('red-list-index.csv')
colnames(redlist)[4] <- 'redlist'
tail(redlist)

fig1.10 <-
  ggplot(redlist%>%filter(Entity == 'World', Year>=2000), 
         aes(x=Year, y=1-redlist))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1)+
  geom_point()+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(xlim = c(2000, 2023),
                  ylim = c(0.05, 0.35))+
  theme_few()+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
                     labels = c('','','','',''))+
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3))

ggsave("1장 멸종위기.png", 
       fig1.10, dpi = 300, width = 3, height = 3, units = "in")



##################
#긍정 경험########
##################

gallup.emotions <- read.csv('data-dA2RK.csv')
gallup.emotions <- rbind(c(2022, 70, 33), gallup.emotions)
colnames(gallup.emotions)[1] <- 'year'

fig1.3 <-
  ggplot(gallup.emotions, 
         aes(x=year, y=Positive.Experience))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1)+
  geom_point()+
  labs(title='긍정 경험 지수', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(xlim = c(2005, 2023),
                  ylim = c(50, 80))+
  theme_few()+
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020))+
  scale_y_continuous(breaks = c(50, 60, 70, 80))

ggsave("1장 긍정경험.png", 
       fig1.3, dpi = 300, width = 3, height = 3, units = "in")



# #################
# 부정 경험########
# #################

fig1.4 <-
  ggplot(gallup.emotions, 
         aes(x=year, y=Negative.Experience))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1)+
  geom_point()+
  labs(title='부정 경험 지수', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(xlim = c(2005, 2023),
                  ylim = c(10, 40))+
  theme_few()+
  scale_x_continuous(breaks = c(2005, 2010, 2015, 2020))+
  scale_y_continuous(breaks = c(10, 20, 30, 40))

ggsave("1장 부정경험.png", 
       fig1.4, dpi = 300, width = 3, height = 3, units = "in")



# #################
# 세계 난민########
# #################

wbrefugee <- WDI(country = 'WLD', 
                 indicator = c('SM.POP.REFG.OR', 
                               'SP.POP.TOTL'),
                 start = 2000)
refugee <- wbrefugee%>%mutate(ref.ratio=SM.POP.REFG.OR/SP.POP.TOTL)

fig1.7 <-
  ggplot(refugee, 
         aes(x=year, y=ref.ratio*100000))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1)+
  geom_point()+
  labs(title='\n', y='\n',
       caption='출처: 세계 은행')+
  xlab('')+
  coord_cartesian(xlim = c(2000, 2022),
                  ylim = c(150, 500))+
  theme_few()+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020),
                     labels = c('','','','',''))+
  scale_y_continuous(breaks = c(200, 300, 400, 500))

ggsave("1장 세계 난민 수.png", 
       fig1.7, dpi = 300, width = 3, height = 3, units = "in")



# #################
# 무장 분쟁########
# #################
#자료 출처: Uppsala Conflict Data Program (2023) – processed by Our World in Data. “Deaths in armed conflicts by region” [dataset]. Uppsala Conflict Data Program, “Georeferenced Event Dataset v23.1” [original data]. Retrieved January 17, 2024 from https://ourworldindata.org/grapher/deaths-in-armed-conflicts-by-region


conflict <- read.csv('deaths-in-armed-conflicts.csv')
colnames(conflict)[4:6] <- c('high.estimate', 'best.estimate',
                             'low.estimate')
conflict <- conflict%>%filter(Entity=='World')%>%
  left_join(wbpop%>%filter(iso3c=='WLD'),
                                 by = c('Year'='year'))%>%
  mutate(ratio=best.estimate/SP.POP.TOTL)


fig1.8 <-
  ggplot(conflict%>%filter(Year>=2000), 
         aes(x=Year, y=ratio*100000))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth=1)+
  geom_point()+
  labs(title='무장 분쟁 사망자 수',
       y='십만명 당 사망자 수',
       caption='출처: Our World In Data, 세계 은행')+
  xlab('')+
  coord_cartesian(xlim = c(2000, 2022), ylim = c(0,3))+
  theme_few()

ggsave("1장 무장 분쟁.png", 
       fig1.8, dpi = 300, width = 3, height = 3, units = "in")
