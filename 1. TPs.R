
#################################################################################################
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer, purrr, broom)   

#################################################################################################
load("data_80_80_80/base_rates.Rda") #data too large for github (from 80-80-80 paper)
load("data_NCDC/wpp_age_20.Rda")

#update with wpp2021 estimates for incoming 20-year old cohorts
b_rates<-b_rates%>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))%>%
  left_join(., wpp_20%>%select(iso3, year, sex, Population))%>%
  mutate(Nx = ifelse(age==20, Population, Nx),
         pop = ifelse(age==20, Population, pop))%>%
  select(-Population, -iso3)

#inspect
ggplot(b_rates%>%filter(location=="India", year==2050, cause=="ihd"),
       aes(x=age, y=(CF)))+
  geom_line()+
  facet_wrap(sex~cause)

ggplot(b_rates%>%filter(location=="India", year==2050, cause=="ihd"),
       aes(x=age, y=(IR)))+
  geom_line()+
  facet_wrap(sex~cause)

#address odd trends
#fit everything exponentially
#assumes age 60-85 most stable estimates
#assumes risk increases with age

# https://stackoverflow.com/questions/31851936/exponential-curve-fitting-in-r
# https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
# https://stats.stackexchange.com/questions/216023/suitable-non-linear-equation-to-capture-a-j-shaped-relationship-between-x-and


reg<-b_rates%>%
  filter(age>=60 & age<=85)%>%
  nest(-year, -sex, -cause, -location)%>%
  mutate(ir_slope = map(data, ~coef(lm(log(IR)~age, data=.x))[[2]]),
         ir_int = map(data, ~coef(lm(log(IR)~age, data=.x))[[1]]),
         cf_slope = map(data, ~coef(lm(log(CF)~age, data=.x))[[2]]),
         cf_int = map(data, ~coef(lm(log(CF)~age, data=.x))[[1]]))%>%
  select(-data)#takes 1-2 minutes #warnings ok

reg$ir_slope<-unlist(reg$ir_slope)
reg$cf_slope<-unlist(reg$cf_slope)
reg$ir_int<-unlist(reg$ir_int)
reg$cf_int<-unlist(reg$cf_int)

new_rates<-left_join(b_rates, reg)%>%
  mutate(CF_new = exp(age*cf_slope+cf_int),
         IR_new = exp(age*ir_slope+ir_int))%>%
  mutate(CF2 = ifelse(age<55, CF_new, CF),
         IR2 = ifelse(age<55, IR_new, IR)) #dont use this

##plot again
ggplot(new_rates%>%filter(location=="India", year==2050, cause=="ihd"),
       aes(x=age, y=CF2))+
  geom_line()+
  facet_wrap(sex~cause)

#https://datascienceplus.com/cubic-and-smoothing-splines-in-r/
##Then fit with a cubic spline**
library(splines)
test<-new_rates%>%filter(location=="India", year==2050, cause=="ihd", sex=="Female")

agelims<-range(test$age)
age.grid<-seq(from=20, to = 95)

plot(test$age,test$CF2,col="grey",xlab="Age",ylab="Case fatality")
fit<-lm(test$CF2 ~ bs(test$age,knots = c(70))) #cutpoints(30,60,80)
points(age.grid, predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(70),lty=2,col="darkgreen")


test2<-new_rates%>%
  group_by(location, year, cause, sex)%>%
  mutate(pred = fitted(lm(CF2 ~ bs(age, knots = c(70)))))%>%
  ungroup()%>%
  select(location, year, age, sex, cause, CF, CF2, pred)%>%
  gather(model, CF, -location, -year, - age, -sex, -cause)

test2<-test2%>%
  mutate(model = ifelse(model=="CF", "Original",
                        ifelse(model=="CF2", "Adjusted", "Fit exponentially")))


ggplot(test2%>%filter(location=="India", year==2050, cause=="ihd", model!="Adjusted"),
       aes(x=age, y=CF, color=model))+
  geom_line(size=1)+
  facet_wrap(~sex)+
  theme_bw()+
  ylab("Sick to dead transition probabiliy")+
  xlab("Age")+
  labs(color="")

ggsave("outputs/FigureA2.jpeg", height=4, width=8)


#for incidence
test3<-new_rates%>%
  group_by(location, year, cause, sex)%>%
  mutate(pred = fitted(lm(IR ~ bs(age, knots = c(70)))))%>%
  ungroup()%>%
  select(location, year, age, sex, cause, IR, pred)%>%
  gather(model, IR, -location, -year, - age, -sex, -cause)

test3<-test3%>%
  mutate(model = ifelse(model=="IR", "Original", "Fit exponentially"))%>%
  mutate(IR = ifelse(IR<0, 0 , IR))


ggplot(test3%>%filter(location=="India", year==2050, cause=="ihd"),
       aes(x=age, y=IR, color=model))+
  geom_line(size=1)+
  facet_wrap(~sex)+
  theme_bw()+
  ylab("Well to sick transition probabiliy")+
  xlab("Age")+
  labs(color="")

ggsave("outputs/FigureA2_2.jpeg", height=4, width=8)


b_rates<-left_join(test2%>%filter(model=="Fit exponentially")%>%select(-model), 
                     test3%>%filter(model=="Fit exponentially")%>%select(-model))%>%
  mutate(IR = ifelse(IR<0, 0, IR),
         IR = ifelse(IR>0.9, 0.9, IR),
         CF = ifelse(CF<0, 0 , CF),
         CF = ifelse(CF>0.9, 0.9, CF))%>%
  left_join(., b_rates%>%select(year, location, sex, age, cause, ALL.mx,
                                BG.mx.all, BG.mx, PREVt0, DIS.mx.t0, Nx,
                                sick, dead, well, pop, all.mx, covid.mx, newcases))

#Save new rates
save(b_rates, file='new_rates.Rda')
save(countrylist, file="set-up.Rda")

##coverage and effects
inc<-read.csv("coverage_data/new_scale_up.csv", stringsAsFactors = F)

unique(inc$cause)
unique(inc$intervention)

load("new_rates.Rda")
b_rates<-data.table(b_rates)
any(is.na(b_rates))

#updating TPs with new intervention scale-up fxn for those 80+
#we only want to apply treatment effects to those age 55-79
#but there will be a continuation of protection for those aging in to the 80+ group
#So we want to just project the expected slope for this group

#run regression for age 80-94, grouped by year, sex, cause, intervention
library(purrr)
library(broom)

reg<-b_rates%>%
  filter(age>=80 & age<=90)%>%
  nest(-year, -sex, -cause, -location)%>%
  mutate(ir_slope = map(data, ~coef(lm(IR~age, data=.x))[["age"]]),
         cf_slope = map(data, ~coef(lm(CF~age, data=.x))[["age"]]))%>%
  select(-data)#takes 1-2 minutes #warnings ok

reg$ir_slope<-unlist(reg$ir_slope)
reg$cf_slope<-unlist(reg$cf_slope)

b_rates<-left_join(b_rates, inc)

#control
b_rates[, PP:=1-(PP_eff*pp.cov.inc)]
b_rates[, SP:=1-(SP_eff*sp.cov.inc)]
b_rates[is.na(PP),PP:=1]
b_rates[is.na(SP),SP:=1]

#treatment
b_rates[, PP_trt:=1-(0.5*PP_eff*(pp.trt.inc-pp.cov.inc))]
b_rates[, SP_trt:=1-(0.5*SP_eff*(sp.trt.inc-sp.cov.inc))]
b_rates[is.na(PP_trt),PP_trt:=1]
b_rates[is.na(SP_trt),SP_trt:=1]

b_rates<-b_rates[age>=55 & age<80, IR:=IR*PP*PP_trt]
b_rates<-b_rates[age>=55 & age<80, CF:=CF*SP*SP_trt]

b_rates<-b_rates%>%select(-pp.cov.inc, -sp.cov.inc,
                          -pp_cov, -sp_cov, -PP_eff, -SP_eff,
                          -PP, -SP, -pp.trt.inc, -sp.trt.inc,
                          -PP_trt, -SP_trt)

intervention_rates<-left_join(b_rates, reg)

  for(i in 1:16){
    temp<-intervention_rates[age<=79+i & age>=79+i-1]
    temp<-temp[, IR_new:=shift(IR)+ir_slope,by=.(year, sex, cause, intervention, location)]
    temp<-temp[, CF_new:=shift(CF)+cf_slope,by=.(year, sex, cause, intervention, location)]
    temp<-temp[age==79+i]
    
    intervention_rates[age==79+i, IR:=temp[, IR_new]]
    intervention_rates[age==79+i, CF:=temp[, CF_new]]
  }

unique(intervention_rates$intervention)

plot<-intervention_rates%>%
  filter(location=="India",
         year %in% c(2020,2035,2050),
         cause=="ihd",
         sex=="Female")%>%
  mutate(Intervention = ifelse(intervention=="Baseline",
                               "Current care", intervention))%>%
  mutate(Intervention = factor(Intervention, levels = c("Current care",
                                                        "Worst case, targeted",
                                                        "Worst case, population",
                                                        "Base case, targeted", 
                                                        "Best case, targeted",
                                                        "Base case, population-wide",
                                                        "Best case, population-wide")))%>%
  filter(!is.na(Intervention))

#plot
ggplot(plot, aes(x=age, y=IR, color=Intervention))+
  geom_line(size=1)+
  facet_wrap(~year)+
  theme_bw()+
  ylab("Well to sick transition probabiliy")+
  xlab("Age")

ggsave("outputs/FigureA4.jpeg", height=4, width=8)

ggplot(plot, aes(x=age, y=CF, color=Intervention))+
  geom_line(size=1)+
  facet_wrap(~year)+
  theme_bw()+
  ylab("Sick to dead transition probabiliy")+
  xlab("Age")

ggsave("outputs/FigureA4_2.jpeg", height=4, width=8)
