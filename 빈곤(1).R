#install.packages("installr")
#installr::updateR()

library(httr)
#devtools::install_github('worldbank/pipr')

library(readxl)
library(pipr)
library(wbstats)
library(WDI)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggthemes)

################################
#그림1##########################
################################

#자료 출처
#링크: https://ourworldindata.org/poverty#all-charts 
#World population living in extreme povertyWorld Bank & Bourguignon and Morrisson
#파일명: 'world-population-in-extreme-poverty-absolute.csv'
owid_pov <- read.csv('world-population-in-extreme-poverty-absolute.csv')
colnames(owid_pov) <- c('Entity', 'Code', 'Year', 'No_Poverty', 'Poverty')
owid_pov <- owid_pov%>%mutate(rate = Poverty/(Poverty+No_Poverty))

fig2.1 <-
  ggplot(data = owid_pov, aes(x=Year, y=rate*100))+
  geom_area(fill = 'grey', alpha=0.5)+
  geom_line(linewidth = 1.2)+
  labs(title='\n', y='\n',
       caption='')+
  xlab('')+
  coord_cartesian(ylim = c(0, 100))+ 
  annotate('point', x=1820, y=89.15, size=2.5)+
  annotate('point', x=1980, y=43.25, size=2.5)+
  annotate('point', x=2015, y=9.98, size=2.5)+
  theme_few()+
  scale_x_continuous(breaks = c(1850, 1900, 1950, 2000),
                     labels = c('','','',''))+
  scale_y_continuous(breaks = c(0, 25,50,75,100),
                     labels = c('','','','',''))

ggsave("2장 1.png", fig2.1, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림2##########################
################################
reddy <- as.data.frame(cbind(c(rep('Food ($5.04)',5),rep('GC ($1.90)',5)),
                             c(68.9, 69.5, 70, 65.8, 56.6, 45.5, 39.6, 30.8, 25.1, 15.9),
                             c(1980, 1990, 2000, 2005, 2012, 1980, 1990, 2000, 2005, 2012)))
colnames(reddy) <- c('PPP', 'rate', 'year')
reddy <- reddy%>%mutate(year=as.numeric(year), rate=as.numeric(rate))

fig2.2 <-
  ggplot(data=reddy, aes(x=year, y=rate, discrete=PPP, color=PPP))+
    geom_line(linewidth=1.2)+geom_point(size=2.5)+
    labs(title = '\n', x="", y="\n", 
         caption = "")+
    coord_cartesian(ylim = c(10, 85), xlim = c(1980, 2015))+ 
    scale_color_grey(name = '') +
    theme_few()+
    scale_x_continuous(breaks = c(1980, 1990, 2000, 2010),
                       labels = c('','','',''))+
    scale_y_continuous(breaks = c(20, 40, 60, 80),
                       labels = c('','','',''))+
    theme(legend.position = 'none')

ggsave("2장 2.png", fig2.2, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림3##########################
################################
#자료 출처 :   Tony Fujs, Aleksander Eilertsen, Ronak Shah and R. Andrés Castañeda (2022). pipr: Client for the PIP API. https://github.com/worldbank/pipr, https://worldbank.github.io/pipr/.

pip <- get_stats(country = 'all', ppp_version = '2011', reporting_level = 'national')
pipwrld <- get_wb(ppp_version = '2011')

p1.9 <- pipwrld%>%filter(region_code=='WLD')%>%
  left_join(pip%>%filter(country_code=='CHN')%>%mutate(headcount.chn=headcount, pop.chn=pop)%>%
              select(year, headcount.chn, pop.chn))%>%
  mutate(exc.chn=(headcount*pop-headcount.chn*pop.chn)/(pop-pop.chn))

p5.0 <- get_wb(ppp_version = '2011', povline = 5.0)%>%filter(region_code=='WLD')
p5.0 <- p5.0%>%left_join(get_stats(country = 'all', ppp_version = '2011', reporting_level = 'national', povline = 5.0)%>%
              filter(country_code=='CHN')%>%mutate(headcount.chn=headcount, pop.chn=pop)%>%
              select(year, headcount.chn, pop.chn))%>%
  mutate(exc.chn=(headcount*pop-headcount.chn*pop.chn)/(pop-pop.chn))

pp5.0 <- p1.9%>%select(year, headcount)%>%left_join(p5.0%>%select(year, exc.chn), by ='year')

fig2.3 <- 
  ggplot(data = pp5.0)+
  geom_line(aes(x=year, y=headcount*100, color='black'), size=1, alpha=.6)+
  geom_line(data=pp5.0%>%filter(is.na(exc.chn)==F), 
            aes(x=year, y=exc.chn*100, color='darkgrey'), size=1)+
  geom_point(aes(x=year, y=headcount*100))+
  geom_point(data=pp5.0%>%filter(is.na(exc.chn)==F),
             aes(x=year, y=exc.chn*100), alpha=.5)+
  labs(x='\n', y='\n\n', title = '\n',
       caption='')+
  theme_few()+
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c('','','','',''))+
  ylim(c(0,70))+
  scale_color_manual(values = c('black','darkgrey'),
                     labels = c('', ''))+
  theme(plot.caption = element_text(size=12),
        legend.position = c(0.2,0.25),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=12))

ggsave("2장 3.png", fig2.3, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림4##########################
################################
p3.2 <- get_wb(ppp_version = '2011', povline = 3.2)%>%filter(region_code=='WLD')
p3.2 <- p3.2%>%
  left_join(get_stats(country = 'all', ppp_version = '2011', reporting_level = 'national', povline = 3.2)%>%
              filter(country_code=='CHN')%>%mutate(headcount.chn=headcount, pop.chn=pop)%>%
              select(year, headcount.chn, pop.chn))%>%
  mutate(exc.chn=(headcount*pop-headcount.chn*pop.chn)/(pop-pop.chn))

fig2.4 <-
ggplot(data = p3.2)+
  geom_line(aes(x=year, y=headcount*100, color='black'), size=1, alpha=.6)+
  geom_line(data=p3.2%>%filter(is.na(exc.chn)==F), 
            aes(x=year, y=exc.chn*100, color='darkgrey'), size=1)+
  geom_point(aes(x=year, y=headcount*100))+
  geom_point(data=p3.2%>%filter(is.na(exc.chn)==F),
             aes(x=year, y=exc.chn*100), alpha=.5)+
  labs(x='\n', y='\n\n', title = '\n',
       caption='')+
  theme_few()+
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c('','','','',''))+
  ylim(c(0,65))+
  scale_color_manual(values = c('black','darkgrey'),
                     labels = c('', ''))+
  theme(plot.caption = element_text(size=12),
        legend.position = c(0.2,0.25),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=12))

ggsave("2장 4.png", fig2.4, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림5##########################
################################
p5.5 <- get_wb(ppp_version = '2011', povline = 5.5)%>%filter(region_code=='WLD')
p5.5 <- p5.5%>%
  left_join(get_stats(country = 'all', ppp_version = '2011', 
                      reporting_level = 'national', povline = 5.5)%>%
              filter(country_code=='CHN')%>%mutate(headcount.chn=headcount, pop.chn=pop)%>%
              select(year, headcount.chn, pop.chn))%>%
  mutate(exc.chn=(headcount*pop-headcount.chn*pop.chn)/(pop-pop.chn))

fig2.5 <-
  ggplot(data = p5.5)+
  geom_line(aes(x=year, y=headcount*100, color='black'),
            size=1, alpha=.6)+
  geom_line(data=p5.5%>%filter(is.na(exc.chn)==F), 
            aes(x=year, y=exc.chn*100, color='darkgrey'),
            size=1)+
  geom_point(aes(x=year, y=headcount*100))+
  geom_point(data=p5.5%>%filter(is.na(exc.chn)==F),
             aes(x=year, y=exc.chn*100), alpha=.5)+
  labs(x='\n', y='\n\n', title = '\n',
       caption='')+
  theme_few()+
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020),
                     labels = c('','','','',''))+
  ylim(c(5,75))+
  scale_color_manual(values = c('black','darkgrey'),
                     labels = c('', ''))+
  theme(plot.caption = element_text(size=12),
        legend.position = c(0.2,0.25),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=11),
        legend.text = element_text(size=12))

ggsave("2장 5.png", fig2.5, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림6##########################
################################
#자료 출처 : Moatsos, M. (2021). Global extreme poverty: Present and past since 1820.
#링크: https://clio-infra.eu/Indicators/GlobalExtremePovertyDollaraDay.html
#링크: https://clio-infra.eu/Indicators/GlobalExtremePovertyCostofBasicNeeds.html
#링크: https://www.oecd-ilibrary.org/sites/3d96efc5-en/1/3/9/index.html?itemId=/content/publication/3d96efc5-en&_csp_=2c2e680562193998e9d20ed6a45a9242&itemIGO=oecd&itemContentType=book
#
#자료 출처: Free data from Gapminder.org
#링크: http://gapm.io/dpop
#
#자료 출처 : World Bank. (2022). World Development Indicators: Population, total. https://data.worldbank.org/indicator/SP.POP.TOTL

cbn <- read_xlsx('GlobalExtremePovertyCostofBasicNeeds_Compact.xlsx',
                 sheet = 2)
colnames(cbn)[4] <- 'cbn'

dad <- read_xlsx('GlobalExtremePovertyDollaraDay_Compact.xlsx',
                 sheet = 2)
colnames(dad)[4] <- 'dad'

# wbpop <- WDI(country = 'all', indicator = c("SP.POP.TOTL"),
#            start = 1960, extra = T)
# wbpop <- wbpop%>%mutate(country=ifelse(country=='Turkiye', 'Turkey', country))

gap.pop <- read_xlsx('GM-Population - Dataset - v7.xlsx', 
                     sheet = 4)%>%
  mutate(wbcode = countrycode(name, 
                              origin = 'country.name',
                              destination = 'wb'))

pov <- cbn %>%left_join(dad%>%select(-country.name),
                        by = c('ccode'='ccode',
                               'year'='year'))%>%
  mutate(wbcode = countrycode(country.name, 
                              origin = 'country.name',
                              destination = 'wb'))%>%
  left_join(gap.pop%>%select(time, wbcode, Population),
            by = c('wbcode'='wbcode', 'year'='time'))%>%
  filter(year>=1960)

wrlddad <- 
  pov %>% group_by(year) %>%
  summarize(dad = weighted.mean(dad, w = Population, na.rm = TRUE))


chn <- read_excel('079-v1hey0-v1hey0.xlsx', skip = 19)
colnames(chn)[5] <- 'cbn'
colnames(chn)[1] <- 'year'
chn <- chn %>% 
  mutate(country.name = 'China', ccode = 156, 
         wbcode = 'CHN', dad=DAD) %>%  
  filter(year >= 1960)%>%
  left_join(gap.pop %>% select(wbcode, time, Population), 
            by = c('wbcode' = 'wbcode', 'year' = 'time'))%>%
  select(ccode, country.name, year, cbn, dad, wbcode, Population)

pov2 <- rbind(chn%>%filter(year<1995),
              pov%>%filter(!(year < 1995 & wbcode == 'CHN')))

wrldcbn <- 
  pov2 %>% group_by(year) %>%
  summarize(cbn = weighted.mean(cbn, w = Population, na.rm = TRUE))

wrldcbn.exc.chn <- pov2 %>% filter(wbcode!='CHN')%>%
  group_by(year) %>%
  summarize(cbn = weighted.mean(cbn, w = Population, na.rm = TRUE))
colnames(wrldcbn.exc.chn)[2] <- 'cbn.exc.chn'

wrldpov <- wrldcbn%>%left_join(wrlddad, by ='year')%>%
  left_join(wrldcbn.exc.chn, by = 'year')


fig2.6 <- 
  ggplot(wrldpov, aes(x=year))+
  geom_rect(xmin = 1980, xmax = 2000,
            ymin = -Inf, ymax = Inf,
            fill = 'gray', alpha = 0.01,
            inherit.aes = FALSE)+
  geom_line(aes(y=cbn*100, linetype='cbn'))+
  geom_line(aes(y=dad*100, linetype='dad'))+
  #geom_line(aes(y=cbn.exc.chn*100, linetype='cbn.exc.chn'))+
  theme_few()+
  ylim(c(0, 60))+
  labs(title = '\n', x='\n', y='\n\n',
       caption = '')+
  theme(plot.caption = element_text(size=12),
        legend.position = c(0.2,0.25),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size=11),
        legend.text = element_blank())

ggsave("2장 6.png", fig2.6, dpi = 300, width = 6, height = 4, units = "in")

