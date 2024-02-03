library(readxl)
library(WDI)
library(wbstats)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(tidyr)
library(zoo)



catalogue <- read_xlsx('Conflict-Catalog-18-vars.xlsx')
View(catalogue)

########################

tot.catalogue <- catalogue %>%
  filter(!is.na(TotalFatalities))


# Replace NA values in EndYear with 1999
tot.catalogue <- 
  tot.catalogue %>%
  mutate(
    EndYear = ifelse(is.na(EndYear), 1999, EndYear),
    EndYear = ifelse(row_number() == 1177, 1984, EndYear) #india rebellion against Sikhs
  )


tot_result <- tot.catalogue %>%
  rowwise() %>%
  do(data.frame(
    Name = rep(.$Name, length(seq(.$StartYear, .$EndYear))),
    year = seq(.$StartYear, .$EndYear),
    avg_fatalities = .$TotalFatalities / length(seq(.$StartYear, .$EndYear))
  )) %>%
  ungroup()

tot_aggregated <- tot_result %>%
  group_by(year) %>%
  summarise(total_fatalities = sum(avg_fatalities))
View(tot_aggregated)


######################################################


wrld.pop <- read.csv('population.csv')
# Gapminder - Population v7 (2022); Gapminder - Systema Globalis (2022); HYDE (2017); United Nations - World Population Prospects (2022) – with major processing by Our World in Data. “Population” [dataset]. Gapminder, “Population v7”; Gapminder, “Systema Globalis”; PBL Netherlands Environmental Assessment Agency, “HYDE 3.2”; United Nations, “World Population Prospects” [original data]. Retrieved February 2, 2024 from https://ourworldindata.org/grapher/population
wrld.pop <- wrld.pop%>%filter(Year>=1400, Entity=='World')
wrld.pop <- data.frame(Year = seq(min(wrld.pop$Year), max(wrld.pop$Year)))%>%
  left_join(wrld.pop, by = 'Year')
colnames(wrld.pop)[4] <- 'population'

# Use the approx function to perform linear interpolation
wrld.pop <- approx(wrld.pop$Year, wrld.pop$population,
                   method = "linear", n = nrow(wrld.pop))
wrld.pop <- data.frame(year = wrld.pop$x, population = wrld.pop$y)

tot_aggregated <- tot_aggregated%>%left_join(wrld.pop, by = 'year')%>%
  mutate(rate=(total_fatalities/population)*100000,
         rolling_average = rollapply(rate, width = 15, FUN = mean, align = "center", fill = NA))

################################
#그림1##########################
################################
fig5.1.last <-
  ggplot(data=tot_aggregated, 
         aes(x=year, y=rate))+
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


ggsave("5장 1(최종).png", fig5.1.last, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림2##########################
################################

fig5.2.last <-
  ggplot(data=tot_aggregated, 
         aes(x=year, y=rate))+
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


ggsave("5장 2(최종).png", fig5.2.last, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림3##########################
################################
tot_aggregated <- tot_aggregated%>%mutate(century=year-year%%100,
                                          years=year-century)

fig5.3.last <- 
  ggplot()+
  geom_line(data=tot_aggregated%>%filter(century==1900),
            aes(x=years, y=rate), size=1)+
  geom_line(data=tot_aggregated%>%filter(century<1600),
            aes(x=years, y=rate, 
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

ggsave("5장 3(최종).png", fig5.3.last, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림4##########################
################################
fig5.4.last <- 
  ggplot()+
  geom_line(data=tot_aggregated%>%filter(century==1900),
            aes(x=years, y=rate), size=1)+
  geom_line(data=tot_aggregated%>%filter(century>=1600, century!=1900, !rate==0),
            aes(x=years, y=rate, 
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

ggsave("5장 4(최종).png", fig5.4.last, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림5##########################
################################

fig5.5.last <- ggplot()+
  geom_line(data=tot_aggregated%>%filter(century==1900),
            aes(x=years, y=rate), size=1)+
  geom_line(data=tot_aggregated%>%filter(century!=1900),
            aes(x=years, y=rate, 
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

ggsave("5장 5(최종).png", fig5.5.last, dpi = 300, width = 6, height = 4, units = "in")



############################################################

mil.catalogue <- catalogue %>%
  filter(!is.na(MilFatalities))


# Replace NA values in EndYear with 1999
mil.catalogue <- 
  mil.catalogue %>%
  mutate(
    EndYear = ifelse(is.na(EndYear), 1999, EndYear),
    EndYear = ifelse(row_number() == 411, 1984, EndYear) #india rebellion against Sikhs
  )


mil_result <- mil.catalogue %>%
  rowwise() %>%
  do(data.frame(
    Name = rep(.$Name, length(seq(.$StartYear, .$EndYear))),
    year = seq(.$StartYear, .$EndYear),
    avg_fatalities = .$MilFatalities / length(seq(.$StartYear, .$EndYear))
  )) %>%
  ungroup()

mil_aggregated <- mil_result %>%
  group_by(year) %>%
  summarise(mil_fatalities = sum(avg_fatalities))


mil_aggregated <- mil_aggregated%>%left_join(wrld.pop, by = 'year')%>%
  mutate(rate=(mil_fatalities/population)*100000,
         rolling_average = rollapply(rate, width = 5, FUN = mean, align = "center", fill = NA))

View(mil_aggregated)

################################
#그림6##########################
################################

fig5.6.last <-
  ggplot(data=mil_aggregated%>%filter(year>=1500, !rate==0), 
         aes(x=year, y=rate))+
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

ggsave("5장 6(최종).png", fig5.6.last, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림7##########################
################################
# Correlates of War - Wars (2020); Population based on various sources (2023) – with minor processing by Our World in Data. “Interstate wars” [dataset]. Correlates of War, “War Data v4.0”; Correlates of War, “War Data v5.1”; Various sources, “Population” [original data].
owid_correlate <- read.csv('conflict-data-source.csv')
owid_correlate <- owid_correlate%>%
  mutate(sum=Interstate.wars+Extrastate.wars+Non.state.wars+Internationalized.intrastate+Non.internationalized.intrastate)


#CoW CC Graph (1815-2000)##########################################################################
cowcclog.last <-
  ggplot()+
  geom_line(data=mil_aggregated%>%filter(year>=1800, !rate==0),
            aes(x=year, y=rate, linetype='dashed'),
            size=.2, alpha=.5)+
  geom_line(data=owid_correlate%>%filter(Year>=1800), 
            aes(x=Year, y=sum, linetype='solid'),
            size=.2, alpha=1)+
  
  scale_y_log10(breaks=c(0.01,0.1,1,10,100),
                labels=c('','', '', '', ''))+
  labs(title='',
       x='', y='\n',caption='')+
  theme_few()+
  scale_linetype_manual(values = c('dashed','solid'),
                        labels = c('CC', 'CoW'))+
  annotate('segment', x=1989, xend=1989, y=0, yend=30, size=0.1)+
  annotate('text', x=1989, y=60, label='')+
  theme(plot.caption = element_text(size=12),
        legend.position = c(0.2,0.85),
        legend.title = element_blank(),
        # legend.text = element_blank(),
        legend.background = element_rect(fill='transparent'),
        axis.text = element_blank()) ############log Graph (1815-2000)

#linear##
cowcclinear.last <-
  ggplot()+
  geom_line(data=mil_aggregated%>%filter(year>=1800, !rate==0),
            aes(x=year, y=rate, linetype='dashed'),
            size=.2, alpha=.5)+
  geom_line(data=owid_correlate%>%filter(Year>=1800), 
            aes(x=Year, y=sum, linetype='solid'),
            size=.2, alpha=1)+
  labs(title='',
       x='', y='')+
  theme_few()+
  scale_linetype_manual(values = c('dashed','solid'),
                        labels = c('CC', 'CoW'))+
  theme(plot.caption = element_text(size=12),
        legend.position = c(0.9,0.825),
        legend.title = element_blank(),
        # legend.text = element_blank(),
        axis.text = element_blank()) ############The long Graph linear (1815-2000)

fig5.7.last <- cowcclinear.last/cowcclog.last
ggsave("5장 7(최종).png", fig5.7.last, dpi = 300, width = 6, height = 8, units = "in")
