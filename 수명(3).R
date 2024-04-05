library(readxl)
library(WDI)
library(countrycode)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(tidyr)
library(fixest)

################################
#표 1###########################
################################

lm1900 <- summary(lm(data = gapmind%>%filter(year==1900, !name%in%exc.list),
                     le~log(gdppc)))
lm1900$coefficients
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) -5.265472  4.0304043 -1.306438 1.930344e-01
# log(gdppc)   5.375056  0.5464413  9.836474 1.296791e-18

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

gapmind%>%filter(year==1960, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc)%>%
  left_join(gapmind%>%filter(year==1900)%>%
              select(wbcode, pop), 
            by = c('wbcode'))%>%
  mutate(le60bygdp=5.375056*log(gdppc)-5.265472)%>%
  summarise(wrldle60bygdp = weighted.mean(le60bygdp, pop))

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

gapmind%>%filter(year==1900, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc, pop)%>%
  mutate(le00base=5.375056*log(gdppc)-5.265472)%>%
  summarise(formula00gdp00 = weighted.mean(le00base, pop))
# formula00gdp00
#           34.9

gapmind%>%filter(year==1960, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc)%>%
  left_join(gapmind%>%filter(year==1900)%>%
              select(wbcode, pop), 
            by = c('wbcode'))%>%
  mutate(le60bygdp=5.375056*log(gdppc)-5.265472)%>%
  summarise(formula00gdp60 = weighted.mean(le60bygdp, pop))
# formula00gdp60
#           37.6

gapmind%>%filter(year==1960, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc)%>%
  left_join(gapmind%>%filter(year==1900)%>%
              select(wbcode, pop), 
            by = c('wbcode'))%>%
  mutate(le60=7.996841*log(gdppc)-8.858897)%>%
  summarise(formula60gdp60 = weighted.mean(le60, pop))
# formula60gdp60
#           54.9


gapmind%>%filter(year==1900, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc, pop)%>%
  mutate(le60byshift=7.996841*log(gdppc)-8.858897)%>%
  summarise(formula60gdp00 = weighted.mean(le60byshift, pop))
# formula60gdp00
#           50.9

gapmind%>%filter(year==1960, !name%in%exc.list)%>%
  summarise(wrldle60 = weighted.mean(le, pop))
# wrldle60
#      50.8

gapmind%>%filter(year==1900, !name%in%exc.list)%>%
  summarise(wrldle00 = weighted.mean(le, pop))
# wrldle00
# <dbl>
#     32.7

################################
#표 2###########################
################################

lm2019 <- summary(lm(data = gapmind%>%filter(year==2019, 
                                             !name%in%exc.list),
                     le~log(gdppc)))
lm2019$coefficients
#              Estimate Std. Error  t value     Pr(>|t|)
# (Intercept) 26.009118  2.2899646 11.35787 5.384638e-23
# log(gdppc)   5.017849  0.2433272 20.62182 1.017292e-49

# Call:
#   lm(formula = le ~ log(gdppc), data = gapmind %>% filter(year == 
#                                                             2019, !name %in% exc.list)) Residuals:
#   Min       1Q   Median       3Q      Max 
# -13.6648  -2.4853   0.6868   2.9680   9.5210 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  26.0091     2.2900   11.36   <2e-16 ***
# log(gdppc)    5.0178     0.2433   20.62   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.866 on 184 degrees of freedom
# Multiple R-squared:  0.698,	Adjusted R-squared:  0.6964 
# F-statistic: 425.3 on 1 and 184 DF,  p-value: < 2.2e-16


gapmind%>%filter(year==1960, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc, pop)%>%
  mutate(le60base=7.996841*log(gdppc)-8.858897)%>%
  summarise(formula60gdp60 = weighted.mean(le60base, pop))
# formula60gdp60
#           55.0

gapmind%>%filter(year==2019, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc)%>%
  left_join(gapmind%>%filter(year==1960)%>%
              select(wbcode, pop), 
            by = c('wbcode'))%>%
  mutate(le19bygdp=7.996841*log(gdppc)-8.858897)%>%
  summarise(formula60gdp19 = weighted.mean(le19bygdp, pop))
# formula60gdp19
#           68.0

gapmind%>%filter(year==2019, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc)%>%
  left_join(gapmind%>%filter(year==1960)%>%
              select(wbcode, pop), 
            by = c('wbcode'))%>%
  mutate(le19=5.017849*log(gdppc)+26.009118)%>%
  summarise(formula19gdp19 = weighted.mean(le19, pop))
# formula19gdp19
#           74.3


gapmind%>%filter(year==1960, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc, pop)%>%
  mutate(le19=5.017849*log(gdppc)+26.009118)%>%
  summarise(formula19gdp60 = weighted.mean(le19, pop))
# formula19gdp60
#           66.1

gapmind%>%filter(year==2019, !name%in%exc.list)%>%
  summarise(wrldle19 = weighted.mean(le, pop))
# wrldle19
#      73.6

gapmind%>%filter(year==1900, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc, pop)%>%
  mutate(le19=5.017849*log(gdppc)+26.009118)%>%
  summarise(formula19gdp00 = weighted.mean(le19, pop))
# formula19gdp00
#           63.5


################
gapmind%>%filter(year==2019, !name%in%exc.list)%>%
  select(name, wbcode, year, le, gdppc)%>%
  left_join(gapmind%>%filter(year==1900)%>%
              select(wbcode, pop), 
            by = c('wbcode'))%>%
  mutate(le19=5.375056*log(gdppc)-5.265472)%>%
  summarise(formula19gdp19 = weighted.mean(le19, pop))
# formula00gdp19
#           46.5
                #27.8
# formula19gdp19
#           74.3
                #10.8
# formula19gdp00
#           63.5
                #28.6
# formula00gdp00
#           34.9
                #11.6
# > (11.6+10.8)/2
# [1] 11.2
# > (28.6+27.8)/2
# [1] 28.2
# > 28.2/(28.2+11.2)
# [1] 0.715736

################################
#미주36#########################
################################

lm.avg <- summary(lm(data = avg.mind, 
                     avg_le~cagr))
lm.avg$coefficients
#                Estimate Std. Error    t value     Pr(>|t|)
# (Intercept) 0.267303558 0.01753947 15.2401172 1.885305e-34
# cagr        0.005309781 0.00732252  0.7251304 4.692932e-01
# lm(formula = avg_le ~ cagr, data = avg.mind)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.30674 -0.09237 -0.02557  0.08821  0.55317 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.267304   0.017539  15.240   <2e-16 ***
# cagr        0.005310   0.007323   0.725    0.469    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1301 on 184 degrees of freedom
# Multiple R-squared:  0.00285,	Adjusted R-squared:  -0.00257 
# F-statistic: 0.5258 on 1 and 184 DF,  p-value: 0.4693

gap60.avg.shift <- avg.mind%>%
  left_join(gapmind%>%filter(year==1960)%>%select(wbcode, le, pop),
            by = c('wbcode'))
gap60.avg.shift%>%mutate(exp.leshift = cagr*0.005309781*59,
                         exp.le = le+exp.leshift)%>%
  summarise(weighted.mean(exp.le, pop))
# `weighted.mean(exp.le, pop)`
# <dbl>
#                         51.7
# 실제 1960 le:           50.8


gapmind$
################################
#식 4###########################
################################

fixest::feols(data=gapmind%>%filter(year>=1900, year<=1960, 
                                    year%%5==0, !name%in%exc.list), 
              le~log(gdppc)|wbcode+year)
# OLS estimation, Dep. Var.: le
# Observations: 2,418 
# Fixed-effects: wbcode: 186,  year: 13
# Standard-errors: Clustered (wbcode) 
#            Estimate Std. Error t value   Pr(>|t|)    
# log(gdppc)  4.45394   0.851336 5.23171 4.5216e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# RMSE: 4.14537     Adj. R2: 0.864409
# Within R2: 0.058066


fe6019 <- fixest::feols(data=gapmind%>%filter(year>=1960, 
                                              year<2020, 
                                              year%%5==0, 
                                              !name%in%exc.list), 
              le~log(gdppc)|wbcode+year,
              panel.id = ~wbcode + year)
# OLS estimation, Dep. Var.: le
# Observations: 2,232 
# Fixed-effects: wbcode: 186,  year: 19
# Standard-errors: Clustered (wbcode) 
#            Estimate Std. Error t value Pr(>|t|)    
# log(gdppc)  1.76796   0.476695 3.70877 0.0002752 ***  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# RMSE: 3.12063     Adj. R2: 0.898698
# Within R2: 0.031922

fe1 <- fixef(fe6019)
country.fe <- as.data.frame(fe1$wbcode)
colnames(country.fe)[1] <- 'country.fe'
year.fe <- as.data.frame(fe1$year)

country.fe$wbcode <- rownames(country.fe)
country.fe <- country.fe%>%
  left_join(gapmind%>%filter(year==1960)%>%
              mutate(gdp1960=gdppc)%>%
              select(wbcode, gdp1960, pop),
            by = c('wbcode'='wbcode'))
country.fe <- country.fe%>%
  left_join(gapmind%>%filter(year==2015)%>%
              mutate(gdp2015 = gdppc)%>%select(wbcode, gdp2015),
            by = c('wbcode'='wbcode'))%>%
  mutate(yr.fe2015=13.441382, gdpcoef=1.76796)
country.fe%>%
  mutate(exp.le=country.fe+log(gdp1960)*gdpcoef)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 1 57.13752
country.fe%>%
  mutate(exp.le=country.fe+log(gdp2015)*gdpcoef+yr.fe2015)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 1 59.78626

(59.78626-57.13752)/(73.22764-57.13752)


################################################################################
#linear trend control###########################################################
################################################################################
oneway6019 <- fixest::feols(data=gapmind%>%filter(year>=1960, 
                                              year<2020, 
                                              year%%5==0, 
                                              !name%in%exc.list), 
                            le~log(gdppc)+year|wbcode,
                            panel.id = ~wbcode + year)
oneway0060 <- fixest::feols(data=gapmind%>%filter(year>=1900, 
                                                  year<=1960, 
                                                  year%%5==0, 
                                                  !name%in%exc.list), 
                            le~log(gdppc)+year|wbcode,
                            panel.id = ~wbcode + year)


fe2 <- fixef(oneway0060)
country.fe.linear <- as.data.frame(fe2$wbcode)
colnames(country.fe.linear)[1] <- 'country.fe'

country.fe.linear$wbcode <- rownames(country.fe.linear)
country.fe.linear <- country.fe.linear%>%
  left_join(gapmind%>%filter(year==1900)%>%
              mutate(gdp1900=gdppc)%>%
              select(wbcode, gdp1900, pop),
            by = c('wbcode'='wbcode'))
country.fe.linear <- country.fe.linear%>%
  left_join(gapmind%>%filter(year==1960)%>%
              mutate(gdp1960 = gdppc)%>%select(wbcode, gdp1960),
            by = c('wbcode'='wbcode'))%>%
  mutate(gdpcoef=6.519767)

country.fe.linear%>%
  mutate(exp.le=country.fe+log(gdp1900)*gdpcoef+1900*0.290096)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 30.94173
country.fe.linear%>%
  mutate(exp.le=country.fe+log(gdp1960)*gdpcoef+1900*0.290096)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 34.1626
country.fe.linear%>%
  mutate(exp.le=country.fe+log(gdp1900)*gdpcoef+1960*0.290096)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 48.34749
country.fe.linear%>%
  mutate(exp.le=country.fe+log(gdp1960)*gdpcoef+1960*0.290096)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 51.56836

(34.1626-30.94173)/(51.56836-30.94173)
#0.1561511
(48.34749-30.94173)/(51.56836-30.94173)
#0.8438489

#############
#1960-2019###
#############

fe3 <- fixef(oneway6019)
country.fe.linear2 <- as.data.frame(fe3$wbcode)
colnames(country.fe.linear2)[1] <- 'country.fe'

country.fe.linear2$wbcode <- rownames(country.fe.linear2)
country.fe.linear2 <- country.fe.linear2%>%
  left_join(gapmind%>%filter(year==1960)%>%
              mutate(gdp1960=gdppc)%>%
              select(wbcode, gdp1960, pop),
            by = c('wbcode'='wbcode'))
country.fe.linear2 <- country.fe.linear2%>%
  left_join(gapmind%>%filter(year==2019)%>%
              mutate(gdp2019 = gdppc)%>%select(wbcode, gdp2019),
            by = c('wbcode'='wbcode'))%>%
  mutate(gdpcoef=1.974374)

country.fe.linear2%>%
  mutate(exp.le=country.fe+log(gdp1960)*gdpcoef+1960*0.225355)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 57.9912
country.fe.linear2%>%
  mutate(exp.le=country.fe+log(gdp2019)*gdpcoef+1960*0.225355)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 61.20245
country.fe.linear2%>%
  mutate(exp.le=country.fe+log(gdp1960)*gdpcoef+2019*0.225355)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 71.28714
country.fe.linear2%>%
  mutate(exp.le=country.fe+log(gdp2019)*gdpcoef+2019*0.225355)%>%
  summarise(wrld.le = weighted.mean(exp.le, pop))
# wrld.le
# 74.49839

(61.20245-57.9912)/(74.49839-57.9912)
#0.1945364
(71.28714-57.9912)/(74.49839-57.9912)
#0.8054636

