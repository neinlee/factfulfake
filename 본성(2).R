library(dplyr)
library(ggplot2)
library(ggthemes)



################################
#그림10#########################
################################
# Conflict initiations, PRIO data, 1945-2014

load(file="Annual-prio-international.RData")  # Annual count of 25-or-more-death conflicts from UCDP/PRIO Armed Conflict Dataset Version 4-2014a
prio <- annual.prio.international 
prio <- prio%>%mutate(rate=conflicts/relevance.BC.sum)

fig5.10 <- 
  ggplot(prio, aes(x=Year, y=rate))+
  geom_line()+
  coord_cartesian(ylim = c(0, 0.012))+
  labs(title='', subtitle = '', x='', y='\n\n',
       caption='')+
  annotate('segment', x=1991, xend=1991, y=0, yend=0.008, 
           linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

ggsave("5장 10.png", fig5.10, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림11#########################
################################
load(file="Annual.RData")
annual <- annual%>%mutate(force.ratio=force.sum/relevance.BC.sum,
                          war.ratio=war.sum/relevance.BC.sum)

fig5.11 <-
  ggplot(annual, aes(x=year, y=war.ratio))+
  geom_line()+
  coord_cartesian(ylim = c(0, 0.08))+
  labs(title='', subtitle = '', x='', y='\n\n',
       caption='')+
  annotate('segment', x=1991, xend=1991, y=-10, yend=0.05, 
           linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

ggsave("5장 11.png", fig5.11, dpi = 300, width = 6, height = 4, units = "in")



################################
#그림12#########################
################################
fig5.12 <-
  ggplot(annual, aes(x=year, y=force.ratio))+
  geom_line()+
  coord_cartesian(ylim = c(0, 0.06))+
  labs(title='', subtitle = '', x='', y='\n\n',
       caption='')+
  annotate('segment', x=1991, xend=1991, y=-10, yend=0.045, 
           linetype='dashed')+
  theme_few()+theme(axis.text = element_blank())

ggsave("5장 12.png", fig5.12, dpi = 300, width = 6, height = 4, units = "in")
