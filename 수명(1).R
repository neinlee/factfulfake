library(readxl)
library(wbstats)
library(WDI)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(patchwork)

################################
#그림1##########################
################################
choco <- read.csv('choco_nobel.txt', sep = ",")
colnames(choco)[2:4] <- c('chocolate',
                          'coffee',
                          'nobel')
choco <- choco%>%mutate(wbcode=countrycode(Country, 
                                           origin = 'country.name',
                                           destination = 'wb'))%>%
  left_join(wbpop%>%filter(year==2018), by = c('wbcode'='iso3c'))%>%
  mutate(ratio=nobel/SP.POP.TOTL)

# fig3.1 <- 
  ggplot(choco, aes(x=chocolate, y=ratio*10000000))+
  geom_point()+
  # geom_text(aes(label = country), vjust = .1, check_overlap = T)+
  geom_text_repel(aes(label = country))+
  labs(title = '\n', y = '\n', x='')+
  stat_smooth(formula = y~x, method = 'lm', se=F, color='black')+
  # geom_abline(intercept = -0.3214312, slope = 1.9857661, 
  #             color = "red") +
  coord_cartesian(xlim = c(0,12), ylim = c(-1, 35))+
  scale_x_continuous(breaks = c(3,6,9),
                     labels = c('','',''))+
  scale_y_continuous(breaks = c(0, 10, 20, 30),
                     labels = c('','','',''))+
  theme_few()

ggsave('3장 1.png', fig3.1, dpi = 300, width = 6, height = 4, units = 'in')

lmchoco <- lm(ratio*10000000 ~ chocolate, data = choco)
lmchoco <- summary(lmchoco)

lmchoco$coefficients
lmchoco$residuals



################################
#그림2##########################
################################

wb_le.ppp <- WDI(country = 'all', 
                 indicator = c('NY.GDP.PCAP.PP.KD',
                               'SP.DYN.LE00.IN',
                               'SP.POP.TOTL'),
                 start = 2019, end = 2019, extra = T)
#1/15

fig3.2 <-
  ggplot(wb_le.ppp%>%filter(region!='Aggregate'), 
         aes(x=NY.GDP.PCAP.PP.KD, 
                      y=SP.DYN.LE00.IN,
                      size=SP.POP.TOTL))+
    geom_point(alpha=.4)+
    scale_size(range = c(.2,30))+
    scale_x_continuous(trans = 'log',
                     breaks = c(1000, 2000, 4000, 
                                8000, 16000, 32000,
                                64000, 128000),
                     labels = c('','','','','','','',''))+ 
    scale_y_continuous(breaks = c(60, 70, 80), 
                     labels = c('','',''))+
    theme_few()+
    labs(title = '\n', x='\n', y='\n\n',
       caption = '')+
    theme(legend.position = 'none')

ggsave('3장 2.png', fig3.2, dpi = 300, width = 6, height = 4, units = 'in')



################################
#그림3##########################
################################

fig3.3 <-
  ggplot(data = owid_le%>%filter(Entity == 'World'), 
         aes(x=Year, y=le))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth = 1)+
  geom_point(data= filter(owid_le%>%filter(Entity == 'World'), 
                          Year %in% c(1770, 2019)),
             aes(x = Year, y = le),
             color = "black", size = 2.5)+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(ylim = c(0, 77))+ 
  theme_few()+
  scale_x_continuous(breaks = c(1800, 1850, 1900, 1950, 2000),
                     labels = c('','','','',''))+
  scale_y_continuous(breaks = c(0, 20, 40, 60),
                     labels = c('','','',''))

ggsave("3장 3.png", fig3.3, dpi = 300, width = 6, height = 4, units = "in")


################################
#그림4##########################
################################
#자료 출처: Maddison Project Database 2020 (Bolt and van Zanden, 2020) – with minor processing by Our World in Data. “GDP per capita – Maddison Project Database – Historical data” [dataset]. Bolt and van Zanden, “Maddison Project Database” [original data]. Retrieved January 15, 2024 from https://ourworldindata.org/grapher/gdp-per-capita-maddison
owid_wrldgdp <- read.csv('gdp-per-capita-maddison.csv')
colnames(owid_wrldgdp)[4] <- 'gdppc'

fig3.4 <-
  ggplot(data = owid_wrldgdp%>%filter(Year>=1820,
                                      Entity == 'World'), 
         aes(x=Year, y=gdppc))+
    geom_area(fill = 'grey', alpha=0.5)+
    geom_line(linewidth = 1)+
    geom_point(data= filter(owid_wrldgdp%>%filter(Year>=1820,
                                                  Entity == 'World'), Year %in% c(1820, 2018)),
               aes(x = Year, y = gdppc),
               color = "black", size = 2.5)+
    labs(title='\n', y='\n',
         caption='')+
    xlab('')+
    theme_few()+
    scale_x_continuous(breaks = c(1850, 1900, 1950, 2000),
                       labels = c('','','',''))+
    scale_y_continuous(breaks = c(0, 5000, 10000, 15000),
                     labels = c('','','',''))

ggsave("3장 4.png", fig3.4, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림5##########################
################################
fig3.5 <- 
  ggplot(data=owid_le%>%filter(Entity=='United Kingdom'), 
                 aes(x=Year, y=le))+
  geom_ribbon(aes(ymin=0, ymax=le), 
              color='grey', alpha=.25)+
  geom_line(linewidth=1)+
  labs(title='\n', y='\n\n',caption='')+
  theme_few()+
  annotate('segment', x=1820, xend=1820, y=0, yend=72, linetype='dashed')+
  annotate('segment', x=1870, xend=1870, y=0, yend=72, linetype='dashed')+
  scale_x_continuous(breaks = c(1600, 1700, 1800, 1900),
                   labels = c('','','',''))+
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80),
                     labels = c('','','','',''))

ggsave("3장 5.png", fig3.5, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림6##########################
################################

owid <- owid_le%>%select(Entity, Year, le)%>%
  left_join(owid_wrldgdp, 
            by = c('Entity'='Entity', 'Year'='Year'))

gbrindgdp <-
  ggplot(data=owid%>%filter(Code=='IND'|Code=='GBR', 
                            Year>=1670),
                    aes(x=Year, y=gdppc, discrete=Code, 
                        linetype=Code, shape=Code))+
  geom_rect(xmin=1830, xmax=1950, 
            ymin=0, ymax=32000, alpha=0.0017)+
  geom_line()+
  labs(title = "\n", x='', y='\n\n')+
  scale_y_log10(breaks=c(1000,4000, 16000),
                  label=c('','',''))+
  theme_few()+
  scale_x_continuous(breaks = c(1700, 1800, 1900, 2000),
                       labels = c('','','',''))+
  geom_vline(xintercept = 1830)+
  geom_vline(xintercept = 1950, linetype='dashed')+
  theme(legend.position = c(0.15,0.75),
          legend.title = element_blank(),
          legend.text = element_blank())

gbrindle <- 
  ggplot(data=owid%>%filter(Entity=='United Kingdom'|Entity=='India',
                                      Year>=1670),
                   aes(x=Year, y=le, discrete=Code, 
                       linetype=Code, shape=Code))+
  geom_rect(xmin=1870, xmax=1921, 
            ymin=0, ymax=85, alpha=0.0017)+
  geom_line()+
  labs(title = "", x='', y='\n\n',
       caption = '')+
  theme_few()+
  scale_x_continuous(breaks = c(1700, 1800, 1900, 2000),
                     labels = c('','','',''))+
  scale_y_continuous(breaks = c(20, 40, 60, 80),
                     labels = c('','','',''))+
  geom_vline(xintercept = 1870)+
  geom_vline(xintercept = 1921, linetype='dashed')+
  theme(legend.position = c(0.15,0.67),
        legend.title = element_blank(),
        legend.text = element_blank())
fig3.6 <- gbrindgdp/gbrindle

ggsave("3장 6.png", fig3.6, dpi = 300, width = 6, height = 9, units = "in")



