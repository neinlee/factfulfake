#install.packages('readstata13')
# install.packages('lubridate')
library(readstata13)
library(lubridate)

library(readxl)
library(wbstats)
library(WDI)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggthemes)
################################
#그림7##########################
################################
#자료 출처: Kentikelenis, A., & Stubbs, T. (2023). A Thousand Cuts: Social Protection in the Age of Austerity. Oxford University Press.
#(IMF Monitor)
#링크: https://imfmonitor.org/conditionality/
imf <- read.dta13('IMFMonitor_Conditions_Main.dta')

imf <- imf %>%
  mutate(year = year(as.Date(year)))

fig2.7 <-
  ggplot(data = imf %>% group_by(year) %>% 
           summarise(total_conditions = sum(BA1TOT, na.rm = TRUE)),
         aes(x=year, y=total_conditions))+
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(title='\n', y='\n\n',  caption='')+
  xlab('')+
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c('','','','',''))+
  scale_y_continuous(breaks = c(0, 1000, 2000,3000),
                     labels = c('','','',''))+
  theme_few()

ggsave("2장 7.png", fig2.7, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림8##########################
################################
scimf <- imf%>%filter(!(ccode %in% c('CHN', 'IND')))%>%
  select(ccode, year, SCsTOT)%>%
  mutate(SCsTOT=as.numeric(ifelse(SCsTOT>0, 1, 0)))%>%
  left_join(wbpop%>%select(country, iso3c, year, SP.POP.TOTL), 
            by = c('ccode'='iso3c', 'year'='year'))%>%
  group_by(year)%>%summarize(SCs=sum(SCsTOT*SP.POP.TOTL, na.rm = T),
                             pop=sum(SP.POP.TOTL, na.rm = T),
                             ratio=SCs/pop)

fig2.8 <-
  ggplot(data = scimf, aes(x=year, y=ratio*100))+
  geom_area(fill = 'grey', alpha=0.75)+
  geom_line(linewidth=1.2)+
  labs(title='\n\n', y='\n\n',  caption='')+
  xlab('')+
  coord_cartesian(ylim = c(0, 47))+ 
  theme_few()+
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c('','','','',''))+
  scale_y_continuous(breaks = c(0, 10, 20,30, 40),
                     labels = c('','','','',''))

ggsave("2장 8.png", fig2.8, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림9##########################
################################
su <- read_excel('072-uwf1gq-uwf1gq.xlsx', skip = 19)
colnames(su)[1] <- 'year'

fig2.9 <-
  ggplot(data=su%>%filter(year>=1950), 
         aes(x=year, y=CBN*100))+
  geom_area(fill = 'grey', alpha=0.75)+
  geom_line(linewidth=1.2)+
  labs(title='\n', y='\n\n',  caption='')+
  xlab('')+
  theme_few()+
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020),
                     labels = c('','','',''))+
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20),
                     labels = c('','','','',''))


ggsave("2장 9.png", fig2.9, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림10#########################
################################
chn <- read_excel('079-v1hey0-v1hey0.xlsx', skip = 19)
colnames(chn)[5] <- 'cbn'
colnames(chn)[1] <- 'year'
chn <- chn %>% 
  mutate(country.name = 'China', ccode = 156, 
         wbcode = 'CHN') %>%  
  filter(year >= 1950)%>%
  select(ccode, country.name, year, cbn)
chn <- rbind(chn%>%filter(year<1995),
             cbn%>%filter(year>=1995 & ccode == 156))

fig2.10 <-
  ggplot(data=chn%>%filter(year>1950), 
         aes(x=year, y=cbn*100))+
    geom_area(fill = 'grey', alpha=0.75)+
    geom_line(linewidth=1.2)+
    geom_point(data = filter(chn, year %in% c(1968, 1995, 2000, 2018)),
               aes(x = year, y = cbn*100),
               color = "black", size = 2.5) +
    labs(title='\n', y='\n\n',  caption='')+
    xlab('')+
    coord_cartesian(ylim = c(0, 80))+ 
    theme_few()+
    annotate("segment", x = 1978, xend = 1978, y = 0, yend = 65,
             linetype = "dashed") +
    scale_x_continuous(breaks = c(1960, 1980, 2000, 2020),
                     labels = c('','','',''))+
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80),
                     labels = c('','','','',''))

ggsave("2장 10.png", fig2.10, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림11#########################
################################
crle <- WDI(country = c('CHN', 'RUS'),
            indicator = c("SP.DYN.LE00.IN"),
            start = 1960, extra = T)

fig2.11 <-
  ggplot(crle%>%filter(year>=1980, year<=2020), 
         aes(x=year, y=SP.DYN.LE00.IN, 
             linetype = iso3c))+
    geom_line()+
    theme_few()+
    labs(title = '\n', x='\n', y='\n\n',
       caption = '')+
    theme(legend.position = c(0.1,0.75),
          legend.title = element_blank(),
          legend.text = element_blank())

ggsave("2장 11.png", fig2.11, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림12#########################
################################
oecdcbn <- read_xlsx('067-6q543k-6q543k.xlsx', skip = 20)

fig2.12 <-
  ggplot(data=oecdcbn, 
         aes(x=Year, y=CBN*100))+
  geom_area(fill = 'grey', alpha=0.75)+
  geom_line(linewidth=1.2)+
  geom_point(data = filter(oecdcbn, Year %in% c(1820, 2018)),
             aes(x = Year, y = CBN*100),
             color = "black", size = 2.5) +
  labs(title='\n', y='\n\n',  caption='')+
  xlab('')+
  coord_cartesian(ylim = c(0, 100))+ 
  theme_few()+
  scale_x_continuous(breaks = c(1850, 1900, 1950, 2020),
                     labels = c('','','',''))+
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     labels = c('','','','',''))

ggsave("2장 12.png", fig2.12, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림13#########################
################################
fig2.13 <-
  ggplot(data=cbn%>%filter(ccode==356), 
         aes(x=year, y=cbn*100))+
  geom_line(linewidth=1.2, color='darkgrey')+
  geom_point(data = filter(cbn%>%filter(ccode==356), 
                           year %in% c(1950, 2018)),
             aes(x = year, y = cbn*100),
             color = "black", size = 2.5) +
  labs(title='\n', y='\n\n',  caption='')+
  xlab('')+
  coord_cartesian(xlim = c(1800, 2020), ylim = c(0, 80))+ 
  theme_few()+
  annotate("point", x=1810, y=23, color="black", size=2.5)+
  annotate("segment", x=1810, xend=1950, y=23, yend=0.6070169*100,
           linetype = "dashed", color = "black", linewidth=0.75)+
  scale_x_continuous(breaks = c(1800, 1850, 1900, 1950, 2020),
                     labels = c('','','','',''))+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80),
                     labels = c('','','','',''))

ggsave("2장 13.png", fig2.13, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림14#########################
################################
#자료 출처: UN WPP (2022); HMD (2023); Zijdeman et al. (2015); Riley (2005) – with minor processing by Our World in Data. “Life expectancy at birth – Various sources – period tables” [dataset]. Human Mortality Database, “Human Mortality Database”; United Nations, “World Population Prospects 2022”; United Nations, “World Population Prospects”; Zijdeman et al., “Life Expectancy at birth 2”; James C. Riley, “Estimates of Regional and Global Life Expectancy, 1800-2001” [original data]. Retrieved January 15, 2024 from https://ourworldindata.org/grapher/life-expectancy

owid_le <- read.csv('life-expectancy.csv')
colnames(owid_le)[4] <- 'le'

fig2.14 <-
  ggplot(owid_le%>%filter(Year>=1880, Year<2020, 
                          Entity%in%c('China','India')), 
         aes(x=Year, y=le, 
             linetype = Entity))+
  geom_line(linewidth=1)+
  geom_point(data = filter(owid_le%>%filter(Entity=='India'), 
                           Year %in% c(1881, 1911)),
             aes(x = Year, y = le),
             color = "black", size = 2.5)+
  geom_point(data = filter(owid_le%>%filter(Entity=='China'), 
                           Year == 1930),
             aes(x = Year, y = le),
             color = "black", size = 2.5)+
  theme_few()+
  labs(title = '\n', x='\n', y='\n\n',
       caption = '')+
  scale_x_continuous(breaks = c(1880, 1920, 1960, 2000),
                     labels = c('','','',''))+
  scale_y_continuous(breaks = c(40, 60, 80),
                     labels = c('','',''))+
  theme(legend.position = c(0.1,0.75),
        legend.title = element_blank(),
        legend.text = element_blank())

ggsave("2장 14.png", fig2.14, dpi = 300, width = 6, height = 4, units = "in")
