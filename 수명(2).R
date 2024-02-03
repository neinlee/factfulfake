library(readxl)
library(wbstats)
library(WDI)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(tidyr)

################################
#그림7##########################
################################
#기대수명 자료출처: Free data from Gapminder.org
#링크: http://gapm.io/dlex
#1인당 GDP (2017 PPP) 자료출처: Free data from Gapminder.org
#링크: http://gapm.io/dgdpcap_cppp

gap.le <- read_xlsx('_GM-Life Expectancy- Dataset - v12.xlsx',
                    sheet = 4)
colnames(gap.le)[4] <- 'le'

gap.gdp <- read_xlsx('GM-GDP per capita - Dataset - v28.xlsx', 
                     sheet = 4)
colnames(gap.gdp)[4] <- 'gdppc'

gapmind <- gap.le%>%left_join(gap.pop, 
                              by = c('geo'='geo', 
                                     'time'='time'))%>%
  left_join(gap.gdp, by = c('geo'='geo', 'time'='time'))%>%
  mutate(pop=Population, year=time)%>%
  select(name, wbcode, year, le, gdppc, pop)


# widegapmind <- pivot_wider(gapmind%>%filter(is.na(gdppc)==F, 
#                                             is.na(le)==F)%>%
#                              select(name, year, le), 
#                            names_from = year, values_from = le)
# write.csv(widegapmind, 'widegapmind.csv')
exc.list <- c('Andorra', 'Dominica', 'St. Kitts and Nevis',
              'Monaco', 'Marshall Islands', 'Nauru',
              'Palau', 'San Marino', 'Tuvalu')  


fig3.7 <- 
  ggplot(gapmind%>%filter(year==1900|year==1960|year==2019, !name%in%exc.list),
       aes(x=gdppc, y=le, 
           discrete=factor(year), shape=factor(year), linetype=factor(year)))+
  geom_point(size=2, alpha=.5)+
  labs(x='\n', y='\n\n', title = '\n',
       caption = '')+
  scale_x_continuous(trans = 'log', breaks = c(500,1000,2000,4000,
                                               8000,16000,32000,64000),
                     labels = c('','','','','','','',''))+
  scale_y_continuous(breaks = c(20,40,60,80),
                     labels = c('','','',''))+
  stat_smooth(formula = y~x, method = 'lm', se=F, color='black')+
  theme_few()+theme(legend.position = c(0.8,0.25),
                    legend.title = element_blank(),
                    legend.text = element_blank(),
                    plot.caption = element_blank())

ggsave('3장 7.png', fig3.7, dpi = 300, width = 6, height = 4, units = 'in')



################################
#그림8##########################
################################

lm1900 <- summary(lm(data = gapmind%>%filter(year==1900, !name%in%exc.list),
                     le~log(gdppc)))
lm1900$coefficients
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) -5.265472  4.0304043 -1.306438 1.930344e-01
# log(gdppc)   5.375056  0.5464413  9.836474 1.296791e-18
#
# Call:
#   lm(formula = le ~ log(gdppc), data = gapmind %>% filter(year == 
#                                                             1900, !name %in% exc.list))
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -12.375  -3.209   0.313   3.176  14.553 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -5.2655     4.0304  -1.306    0.193    
# log(gdppc)    5.3751     0.5464   9.836   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.028 on 184 degrees of freedom
# Multiple R-squared:  0.3446,	Adjusted R-squared:  0.3411 
# F-statistic: 96.76 on 1 and 184 DF,  p-value: < 2.2e-16

lm1960 <- summary(lm(data = gapmind%>%filter(year==1960, !name%in%exc.list),
                     le~log(gdppc)))
lm1960$coefficients
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) -8.858897  4.7711289 -1.856772 6.494219e-02
# log(gdppc)   7.996841  0.5800551 13.786347 3.681425e-30

# Call:
#   lm(formula = le ~ log(gdppc), data = gapmind %>% filter(year == 
#                                                             1960, !name %in% exc.list))
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -26.3958  -5.2070   0.2927   5.9271  16.0756 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -8.8589     4.7711  -1.857   0.0649 .  
# log(gdppc)    7.9968     0.5801  13.786   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.591 on 184 degrees of freedom
# Multiple R-squared:  0.5081,	Adjusted R-squared:  0.5054 
# F-statistic: 190.1 on 1 and 184 DF,  p-value: < 2.2e-16

left <-
  ggplot(gapmind%>%filter(year==1900|year==1960, !name%in%exc.list),
         aes(x=gdppc, y=le, 
             discrete=factor(year), 
             shape=factor(year), linetype=factor(year)))+
  labs(x='', y='\n', title = '',
       caption = '')+
  scale_x_continuous(trans = 'log', breaks = c(1000,4000,16000,64000),
                     labels = c('','','',''))+
  scale_y_continuous(breaks = c(20,40,60,80),
                     labels = c('','','',''))+
  stat_smooth(formula = y~x, method = 'lm', se=F, color='black')+
  annotate('point', x= 2500, y=5.375056*log(2500)-5.265472,
           size = 3)+
  annotate('point', x=7400, y=7.996841*log(7400)-8.858897,
           size = 3)+
  theme_few()+theme(legend.position = c(0.2,0.85),
                    legend.title = element_blank(),
                    legend.text = element_blank(),
                    plot.caption = element_blank())

right <-
  ggplot(gapmind%>%filter(year==1900|year==1960, !name%in%exc.list),
         aes(x=gdppc, y=le, 
             discrete=factor(year), 
             shape=factor(year), linetype=factor(year)))+
  labs(x='', y='\n', title = '',
       caption = '')+
  scale_x_continuous(trans = 'log', breaks = c(1000,4000,16000,64000),
                     labels = c('','','',''))+
  scale_y_continuous(breaks = c(20,40,60,80),
                     labels = c('','','',''))+
  stat_smooth(formula = y~x, method = 'lm', se=F, color='black')+
  annotate('point', x= 2500, y=5.375056*log(2500)-5.265472,
           size = 3)+
  annotate('point', x=7400, y=7.996841*log(7400)-8.858897,
           size = 3)+
  theme_few()+theme(legend.position = 'none',
                    plot.caption = element_blank())

fig3.8 <- left+right

ggsave('3장 8.png', fig3.8, dpi = 300, width = 6, height = 4, units = 'in')



################################
#그림9##########################
################################
#영아사망률 자료출처: Free dataset from Gapminder.org
#링크: https://www.gapminder.org/data/documentation/gd002/ (Version 4)
gap.imr <- read_xlsx('indicator gapminder infant_mortality.xlsx', sheet = 1)
colnames(gap.imr)[1] <- 'country'
colnames(gap.imr)[-1] <- as.numeric(sub("\\.0$", "",
                                        colnames(gap.imr)[-1]))
gap.imr <- gap.imr %>% mutate_at(vars(-1), as.numeric)
gap.imr <- gap.imr%>%
  pivot_longer(cols = -country, names_to = 'year', values_to = 'imr')
gap.imr <- gap.imr%>%mutate(wbcode=countrycode(country, 
                                               origin = 'country.name',
                                               destination = 'wb'))
gap.imr <- gap.imr%>%filter_all(all_vars(!is.na(.)))%>%
  mutate(year=as.numeric(year))

gap.imrgdp <- gap.imr%>%
  left_join(gap.gdp%>%mutate(wbcode=countrycode(name, 
                                                origin='country.name',
                                                destination='wb')),
            by = c('wbcode'='wbcode', 'year'='time'))%>%
  filter(year>=1960)


result <- gap.imrgdp %>% filter_all(all_vars(!is.na(.)))%>%
  group_by(wbcode) %>%
  summarize(min_year = min(year),
            max_year = max(year),
            diff_yrs = max_year - min_year) %>%
  filter(diff_yrs >= 25) %>%
  select(wbcode)

filt.gap.imrgdp <- gap.imrgdp %>%
  filter(wbcode %in% result$wbcode)


avg.gap <- filt.gap.imrgdp %>%
  group_by(wbcode) %>%
  summarize(diffyrs = last(year)-first(year),
            avg_imr = ( (last(imr)-first(imr)) / diffyrs ),
            cagr = ((last(gdppc) / first(gdppc))^(1/diffyrs) - 1) * 100,
            )

right3.9 <-
  ggplot(avg.gap, aes(x=cagr, y=avg_imr))+
  geom_point()+
  labs(x='\n', y='', 
       caption = '')+
  scale_y_continuous(breaks = c(-4,-3,-2,-1,0),
                     labels = c('','','','',''))+
  scale_x_continuous(breaks = c(-2.5,0.0,2.5,5.0, 7.5),
                     labels = c('','','','',''))+
  stat_smooth(formula = y~x, method = 'lm', se=F, color='black')+
  theme_few()+theme(axis.title.y = element_text(size=25))

left3.9 <-
  ggplot(gap.imrgdp%>%filter(year==2015),
         aes(x=gdppc, y=imr))+
    geom_point()+
    labs(x='\n', y='', 
       caption = '')+
    coord_cartesian(ylim = c(0, 100))+ 
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     labels = c('','','','',''))+
    scale_x_continuous(trans = 'log', 
                     breaks = c(1000, 10000, 100000),
                     labels = c('','',''))+
    stat_smooth(formula = y~x, method = 'lm', se=F, color='black')+
    theme_few()+theme(axis.title.y = element_text(size=25))
  

fig3.9 <- left3.9+right3.9  

ggsave('3장 9.png', fig3.9, dpi = 300, width = 6, height = 4, units = 'in')



################################
#그림10#########################
################################
avg.mind <- gapmind %>% filter(year==1960 | year == 2019,
                               !name%in%exc.list)%>%
  group_by(wbcode) %>%
  summarize(diffyrs = last(year)-first(year),
            avg_le = ( (last(le)-first(le)) / diffyrs ),
            cagr = ((last(gdppc) / first(gdppc))^(1/diffyrs) - 1) * 100)

right3.10 <-
  ggplot(avg.mind, aes(x=cagr, y=avg_le))+
    geom_point()+
    labs(x='\n', y='', 
       caption = '')+
    coord_cartesian(xlim = c(-1.5, 6))+ 
    scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8),
                     labels = c('','','','',''))+
    scale_x_continuous(breaks = c(0,2,4,6),
                     labels = c('','','',''))+
    stat_smooth(formula = y~x, method = 'lm', se=F, color='black')+
    theme_few()+theme(axis.title.y = element_text(size=25))
  
left3.10 <-
  ggplot(gapmind%>%filter(year==2019, !name%in%exc.list),
         aes(x=gdppc, y=le))+
  geom_point()+
  labs(x='\n', y='', 
       caption = '')+
  # coord_cartesian(ylim = c(0, 100))+ 
  scale_y_continuous(breaks = c(50, 60, 70, 80),
                     labels = c('','','',''))+
  scale_x_continuous(trans = 'log', 
                     breaks = c(1000, 5000, 25000, 125000),
                     labels = c('','','',''))+
  stat_smooth(formula = y~x, method = 'lm', se=F, color='black')+
  theme_few()+theme(axis.title.y = element_text(size=25))

fig3.10 <- left3.10+right3.10

ggsave('3장 10.png', fig3.10, dpi = 300, width = 6, height = 4, units = 'in')



################################
#그림11#########################
################################
wb.chnle <- WDI(country = 'CHN',
                indicator = 'SP.DYN.LE00.IN',
                start = 1960, end = 2020)
fig3.11 <-
  ggplot(wb.chnle%>%filter(year>=1962), 
         aes(x=year, y=SP.DYN.LE00.IN))+
    geom_line(linewidth = 1.2)+
    geom_point(data= filter(wb.chnle, 
                          year %in% c(1962, 1981, 2000, 2019)),
               aes(x = year, y = SP.DYN.LE00.IN),
               color = "black", size = 2.5)+
    labs(title='\n', y='\n\n', x='',
       caption='')+
    xlab('')+
    coord_cartesian(ylim = c(50, 80))+ 
    annotate('segment', x=1981, xend=1981, y=40, yend=100,
             linetype='dashed')+
    theme_few()+
    scale_x_continuous(breaks = c(1960, 1980, 2000, 2020),
                     labels = c('','','',''))+
    scale_y_continuous(breaks = c(50, 60, 70),
                     labels = c('','',''))

ggsave('3장 11.png', fig3.11, dpi = 300, width = 6, height = 4, units = 'in')



################################
#그림12#########################
################################

fig3.12 <-
  ggplot(owid_le%>%filter(Code%in%c('CHN','IND'),
                          Year>=1930),
         aes(x=Year, y=le, linetype=Code))+
  geom_line(linewidth=1)+
  theme_few()+
  labs(title = '', x='', y='\n\n',
       caption = '')+
  coord_cartesian(xlim = c(1930, 2020))+ 
  scale_x_continuous(breaks = c(1930, 1950, 1970, 1990, 2010),
                     labels = c('','','','',''))+
  scale_y_continuous(breaks = c(30, 40, 50, 60, 70, 80),
                     labels = c('','','','','',''))+
  theme(legend.position = c(0.1,0.75),
        legend.title = element_blank(),
        legend.text = element_blank())
  
ggsave('3장 12.png', fig3.12, dpi = 300, width = 6, height = 4, units = 'in')
