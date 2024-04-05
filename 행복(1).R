library(readxl)
library(pdftables)
library(WDI)
library(wbstats)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(tidyr)


################################
#그림1##########################
################################
#자료출처: Helliwell, John F., Richard Layard, Jeffrey Sachs, and Jan-Emmanuel De Neve, eds. 2020. World Happiness Report 2020. New York: Sustainable Development Solutions Network
#링크: https://worldhappiness.report/data/

whr1 <- read_xlsx('WHR20_DataForFigures2.2_2.3_Appendix2.xlsx',
                  sheet = 1, skip = 1)
colnames(whr1)[1:10] <- c('year', 'ladder', 'ladder.exc5', 'ladder.nw',
                          'positive', 'positive.exc5', 'positive.nw',
                          'negative', 'negative.exc5', 'negative.nw')

fig4.1 <-
  ggplot(whr1, aes(x=year, y=ladder))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth = 1.2)+geom_point()+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(ylim = c(4, 7))+
  theme_few()+
  scale_x_continuous(breaks = c(2006, 2009, 2012, 2015, 2018),
                     labels = c('','','','',''))

ggsave("4장 1.png", fig4.1, dpi = 300, width = 6, height = 4, units = "in")


  
################################
#그림2##########################
################################

left4.2 <-
  ggplot(whr1, aes(x=year, y=positive))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_point(size = 2)+
  geom_line(linewidth = 1.2)+
  labs(title='\n', y='',
       caption='')+
  xlab('')+
  coord_cartesian(ylim = c(0.675, 0.825))+
  theme_few()+
  scale_y_continuous(breaks = c(0.7, 0.75, 0.80),
                     labels = c('','',''))+
  scale_x_continuous(breaks = c(2006, 2009, 2012, 2015, 2018))

right4.2 <-
  ggplot(whr1, aes(x=year, y=negative))+
  geom_point(size = 2)+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth = 1.2)+
  labs(title='\n', y='',
       caption='')+
  xlab('')+
  coord_cartesian(ylim = c(0.175, 0.325))+
  theme_few()+
  scale_y_continuous(breaks = c(0.2, 0.25, 0.3),
                     labels = c('','',''))+
  scale_x_continuous(breaks = c(2006, 2009, 2012, 2015, 2018))

fig4.2 <- left4.2+right4.2
ggsave("4장 2.png", fig4.2, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림3##########################
################################
whr2 <- read_xlsx('WHR20_DataForFigures2.2_2.3_Appendix2.xlsx',
                  sheet = 2, skip = 1)
whr3 <- read_xlsx('WHR20_DataForFigures2.2_2.3_Appendix2.xlsx',
                  sheet = 3, skip = 1)
colnames(whr2)[1:10] <- c('year', 'worry', 'worry.exc5', 'worry.nw',
                          'sadness', 'sadness.exc5', 'sadness.nw',
                          'anger', 'anger.exc5', 'anger.nw')
colnames(whr3)[1:4] <- c('year', 'stress', 'stress.exc5', 'stress.nw')
whr2 <- whr2%>%left_join(whr3, by = 'year')

# fig4.3 <-
  ggplot(whr2, aes(x=year))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth = 1.2)+geom_point()+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(ylim = c(4, 7))+
  theme_few()+
  scale_x_continuous(breaks = c(2006, 2009, 2012, 2015, 2018),
                     labels = c('','','','',''))

ggsave("4장 3.png", fig4.1, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림3##########################
################################

income.swb <- data.frame(time = c(1, 2, 3, 4, 5, 6, 7),
                         income = c(4, 3, 5, 4, 6, 5, 7),
                         happiness = c(2, 1, 2, 1, 2, 1, 2))

fig4.3 <-
    ggplot(income.swb, aes(x = time)) +
    geom_line(aes(y = income), color = 'black', linewidth = 1.2) +
    geom_line(aes(y = happiness), color = 'grey', linewidth = 1.2) +
    geom_smooth(aes(y = income), method = 'lm', color = 'black', se = FALSE) +
    geom_smooth(aes(y = happiness), method = 'lm', color = 'grey', se = FALSE) +
    labs(title = '\n', y = '', caption = '') +
    xlab('') +
    coord_cartesian(ylim = c(0, 7.5)) +
    theme_bw()+
    theme(axis.text.x = element_blank(),  
          axis.text.y = element_blank())+
    geom_vline(xintercept = seq(1, 7, by = 1), 
               color = 'black', alpha = 0.25, linewidth = 0.5)



ggsave("4장 3.png", fig4.3, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림4##########################
################################
fsu.countries <- c('Russia', 'Ukraine', 'Uzbekistan',
                   'Kazakhstan', 'Belarus', 'Azerbaijan',
                   'Kyrgyzstan', 'Tajikistan', 'Turkmenistan',
                   'Armenia', 'Georgia', 'Moldova',
                   'Lithuania', 'Latvia', 'Estonia')
fsu.wbcode <- countrycode(fsu.countries, origin = 'country.name',
                          destination = 'wb')
fsu.gapmind <- gapmind%>%filter(wbcode%in%fsu.wbcode)
View(fsu.gapmind%>%filter(year==2015))

fig4.4 <-
  ggplot(fsu.gapmind%>%filter(year>=1980, year<=2020,
                              wbcode%in%c('RUS','UKR',
                                          'UZB', 'KAZ', 
                                          'AZE')),
         aes(x = year, y = gdppc, 
             group = name, linetype = name, shape = name)) +
  geom_line(linewidth = 1) + geom_point(size = 2)+
  labs(title = '', y = '1인당 GDP\n(PPP, constant 2017 $)', caption = '') +
  xlab('') +
  theme_few()+theme(legend.title = element_blank(),
                    legend.position = 'top')+
  scale_linetype_manual(values = c("Russia" = "solid", 
                                   "Ukraine" = "dashed",
                                   "Uzbekistan" = "dotted",
                                   "Kazakhstan" = "dotdash",
                                   "Azerbaijan" = 'longdash'),
                        guide = 'none') +
  scale_shape_manual(values = c("Russia" = 1,
                                "Ukraine" = 2,
                                "Uzbekistan" = 3,
                                "Kazakhstan" = 4,
                                "Azerbaijan" = 5),
                     limits = c("Russia", 
                                "Ukraine", 
                                "Uzbekistan", 
                                "Kazakhstan", 
                                "Azerbaijan"))
  

ggsave("4장 4.png", fig4.4, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림5##########################
################################



################################
#그림6##########################
################################
chn.ls <- data.frame(year = c(1990, 1995, 2001, 2007, 2012.5, 2018),
                     ls = c(7.29, 6.83, 6.53, 6.76, 6.85, 7.37))
fig4.6 <-
  ggplot(chn.ls, aes(x=year, y=ls))+
  geom_line(linewidth = 1.2)+
  geom_point(size=2)+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(ylim = c(5, 9), xlim = c(1990, 2020))+
  theme_few()
  
ggsave("4장 6.png", fig4.6, dpi = 300, width = 6, height = 4, units = "in")
  


################################
#그림7##########################
################################
ind.ls <- data.frame(year = c(1990, 1995, 2001, 2006.5, 2012),
                     ls = c(6.70, 6.53, 5.14, 5.80, 6.52))
fig4.ind <-
  ggplot(ind.ls, aes(x=year, y=ls))+
  geom_line(linewidth = 1.2)+
  geom_point(size=2)+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(ylim = c(3.5, 8))+
  theme_few()

ggsave("4장 인도.png", fig4.ind, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림8##########################
################################
#자료 출처: Helliwell, John F., Richard Layard, Jeffrey Sachs, and Jan-Emmanuel De Neve, eds. 2021. World Happiness Report 2021. New York: Sustainable Development Solutions Network.
#링크: https://worldhappiness.report/data/
whr2021 <- readxl::read_excel("DataPanelWHR2021C2.xls")
colnames(whr2021)[1] <- 'country'
colnames(whr2021)[3] <- 'ladder'
colnames(whr2021)[10:11] <- c('positive', 'negative')

left4.8 <-
  ggplot(whr2021%>%filter(country%in%c('United States',
                                       'South Korea',
                                       'Costa Rica')),
         aes(x = year, y = ladder, discrete = country,
             shape = country, linetype = country))+
    geom_line(linewidth = 0.6) + geom_point(size = 1.5)+
    labs(title = '', y = '\n', caption = '') +
    xlab('') +
    coord_cartesian(ylim = c(4, 8))+
    theme_few()+
    theme(legend.title = element_blank(),
                      legend.position = c(0.7, 0.2))+
    scale_linetype_manual(values = c("Costa Rica" = "solid",
                                     "United States" = "dotted", 
                                     "South Korea" = "dashed"),
                          guide = 'none')+
    scale_x_continuous(breaks = c(2010, 2015, 2020),
                       labels = c('','',''))+
    scale_y_continuous(breaks = c(4, 5, 6, 7, 8),
                       labels = c('','','','',''))
  
right4.8 <-
  ggplot(whr2021%>%filter(country%in%c('United States',
                                       'South Korea',
                                       'Costa Rica')),
         aes(x = year, y = positive, discrete = country,
             shape = country, linetype = country))+
  geom_line(linewidth = 0.6) + geom_point(size = 1.5)+
  labs(title = '', y = '\n', caption = '') +
  xlab('') +
  coord_cartesian(ylim = c(0.6, 0.9))+
  theme_few()+
  theme(legend.position = 'none')+
  scale_linetype_manual(values = c("Costa Rica" = "solid",
                                   "United States" = "dotted", 
                                   "South Korea" = "dashed"),
                        guide = 'none')+
  scale_x_continuous(breaks = c(2010, 2015, 2020),
                     labels = c('','',''))+
  scale_y_continuous(breaks = c(0.7, 0.8, 0.9),
                     labels = c('','',''))

fig4.8 <- left4.8+right4.8  

ggsave("4장 8.png", fig4.8, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림5##########################
################################
#자료 출처: EASTERLIN, Richard A.; O'CONNOR, Kelsey J. The Easterlin Paradox. IZA Discussion Papers, 2020.
#링크: https://www.econstor.eu/handle/10419/232675
easterlin.wvs <- read.table('easterlin_wvs_ivs.txt', header = T,
                            sep = " ")
easterlin.wvs <- easterlin.wvs %>%
  mutate(transition = ifelse(row_number() > 55, 1, 0))

easterlin.gwp <- read.table('easterlin_gwp.txt', header = T,
                            sep = " ")
easterlin.gwp <- easterlin.gwp %>%
  mutate(transition = ifelse(row_number() >= 26 & row_number() <=53,
                             1, 0))

left4.5 <-
  ggplot(easterlin.wvs, aes(x = gdppc, y = adjls, 
                            discrete = factor(transition),
                            shape = factor(transition)))+
  geom_abline(slope = 0.001242, intercept = 0.012920, 
              color = 'darkgrey')+
  geom_abline(slope = 0.003196, intercept = 0.015173, 
              color = 'darkgrey', linetype = 'dashed')+
  geom_point(size = 1.5)+
  labs(title = '', y = '\n', caption = '')+
  xlab('\n\n') +
  coord_cartesian(xlim = c(0.5, 10), ylim = c(-0.05, 0.25))+
  theme_few()+
  theme(legend.position = 'none')+
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10),
                     labels = c('','','','',''))
    # geom_smooth(data = filter(easterlin.wvs, transition != 1), 
    #             method = 'lm', se = FALSE, color = 'darkgrey', 
    #             linewidth = .5)+
    
    
right4.5 <-
  ggplot(easterlin.gwp%>%filter(yrs>=12), 
         aes(x = gdppc, y = ladder, 
             discrete = factor(transition),
             shape = factor(transition)))+
  geom_abline(slope = 0.002411, intercept = -0.004216,
              color = 'darkgrey')+
  geom_abline(slope = 0.006133, intercept = -0.005587,
              linetype = 'dashed', color = 'darkgrey')+
  geom_point(size = 1.5)+
  labs(title = '', y = '\n', caption = '') +
  xlab('\n\n') +
  coord_cartesian(xlim = c(-5, 8.5), ylim = c(-0.2, 0.2))+
  theme_few()+
  theme(legend.position = 'none')+
  scale_x_continuous(breaks = c(-5, 0, 5),
                     labels = c('','',''))
  # geom_smooth(data = filter(easterlin.gwp, transition != 1, yrs>=12), 
  #             method = 'lm', se = FALSE, color = 'black')+
 

fig4.5 <- left4.5+right4.5

ggsave("4장 5.png", fig4.5, dpi = 300, width = 6, height = 4, units = "in")

summary(lm(data = easterlin.wvs,
           adjls~gdppc))
