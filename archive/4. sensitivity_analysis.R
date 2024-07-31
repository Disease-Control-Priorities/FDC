
#################################################################################################
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer)   

#################################################################################################
#load TPs
load("new_rates.Rda")
load("set-up.Rda")

##coverage and effects
inc<-read.csv("scale-up-new-aspirin.csv", stringsAsFactors = F)%>%
  filter(intervention %in% c("Baseline", "Scenario 4"))

#updating TPs with new scale-up fxn for those 80+
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
b_rates<-data.table(b_rates)

#control
b_rates[, PP:=1-(PP_eff*pp.cov.inc*(5/8))]
b_rates[, SP:=1-(SP_eff*sp.cov.inc*(5/8))]
b_rates[is.na(PP),PP:=1]
b_rates[is.na(SP),SP:=1]

#treatment
b_rates[, PP_trt:=1-(0.5*PP_eff*(pp.trt.inc-pp.cov.inc)*(5/8))]
b_rates[, SP_trt:=1-(0.5*SP_eff*(sp.trt.inc-sp.cov.inc)*(5/8))]
b_rates[is.na(PP_trt),PP_trt:=1]
b_rates[is.na(SP_trt),SP_trt:=1]

b_rates<-b_rates[age>=55 & age<80, IR:=IR*PP*PP_trt]
b_rates<-b_rates[age>=55 & age<80, CF:=CF*SP*SP_trt]

b_rates<-b_rates%>%select(-pp.cov.inc, -sp.cov.inc,
                          -pp_cov, -sp_cov, -PP_eff, -SP_eff,
                          -PP, -SP, -pp.trt.inc, -sp.trt.inc,
                          -PP_trt, -SP_trt)


#################################################################################################
# As a function
#################################################################################################
project.all <- function(Country){
  #################################################################################################
  #################################################################################################
  intervention_rates<-b_rates[location==Country]%>%
    left_join(., reg%>%filter(location==Country))%>%
    arrange(age, year)
  
  for(i in 1:16){
  temp<-intervention_rates[age<=79+i & age>=79+i-1]
  temp<-temp[, IR_new:=shift(IR)+ir_slope,by=.(year, sex, cause, intervention, location)]
  temp<-temp[, CF_new:=shift(CF)+cf_slope,by=.(year, sex, cause, intervention, location)]
  temp<-temp[age==79+i]
  
  intervention_rates[age==79+i, IR:=temp[, IR_new]]
  intervention_rates[age==79+i, CF:=temp[, CF_new]]
  }

  ##########################################################################################

  ## calculate initial states for the incoming year 2000 and all years for age 20 population
  intervention_rates[year==2017 | age==20, sick:=Nx*PREVt0]
  intervention_rates[year==2017 | age==20, dead:=Nx*DIS.mx.t0]
  intervention_rates[year==2017 | age==20, well:=Nx*(1-(PREVt0+ALL.mx))]
  
  
  #base_rates<-base_rates[location %in% countrylist]
  intervention_rates[age==20 | year==2017, pop:=Nx]
  intervention_rates[age==20 | year==2017, all.mx:=Nx*ALL.mx]
  
  intervention_rates[CF>0.99, CF:=0.99]
  intervention_rates[IR>0.99, IR:=0.99]
  
  #STATE TRANSITIONS#
  for(i in 1:41){
    
    b2<-intervention_rates[year<=2017+i & year>=2017+i-1]
    b2[,age2:=age+1]
    
    #newcases
    b2[, newcases2:=shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    
    #sick
    b2[, sick2:=shift(sick)*(1-(CF+BG.mx+covid.mx)) + shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    b2[sick2<0, sick2:=0]
    
    #dead
    b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age, intervention)]
    b2[dead2<0, dead2:=0]
    
    #pop
    b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age, intervention)]
    b2[pop2<0, pop2:=0] #prevent negatives
    
    #all dead envelope
    b2[,all.mx2:=sum(dead2), by=.(sex, location, year, age, intervention)]
    b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)+(pop2*covid.mx)] #UPDATE w/ covid data
    b2[all.mx2<0, all.mx:=0]
    
    #well
    b2[, well2:=pop2-all.mx2-sick2]
    b2[well2<0, well2:=0] #prevent negatives
    
    #re-combined into original data.table
    b2<-b2[year==2017+i & age2<96, c("age2", "newcases2", "sick2", "dead2", "well2", "pop2", 
                                     "all.mx2", "sex", "location", "cause", "intervention")]
    setnames(b2, "age2", "age")
    intervention_rates[year==2017+i & age>20, newcases:=b2[, newcases2]]
    intervention_rates[year==2017+i & age>20, sick:=b2[,sick2]]
    intervention_rates[year==2017+i & age>20, dead:=b2[,dead2]]
    intervention_rates[year==2017+i & age>20, well:=b2[,well2]]
    intervention_rates[year==2017+i & age>20, pop:=b2[,pop2]]
    intervention_rates[year==2017+i & age>20, all.mx:=b2[,all.mx2]]
    
  }
  
  out.df<-intervention_rates[, c("age", "cause", "sex", "year", "well", "sick", "newcases",
                         "dead", "pop", "all.mx", "intervention", "location")]
  
  return(out.df)
  
  
}#as a fxn

#test
#test
test<-project.all("India")%>%
  bind_rows(., project.all("China"))%>%
  bind_rows(., project.all("Ethiopia")) %>%
  bind_rows(., project.all("United States"))  

any(is.na(test%>%filter(cause!="hhd")))

p<-test%>%filter(cause!="hhd")%>%
  group_by(year, intervention, location)%>%
  summarise(dead = sum(dead),
            sick=sum(sick),
            pop = sum(pop))

#inspect
ggplot(p, aes(x=year, y=dead, color=intervention))+
  geom_point()+
  facet_wrap(~location, scales = "free")

#####################################################################
#loop
output2<-project.all(countrylist[1])

time1<-Sys.time()

for(i in 2:182){
  output2<-bind_rows(output2, project.all(countrylist[i]))
}

time2<-Sys.time()
time2-time1 # ~8 mins for 182 countries

drops <- c("all", "b_rates", "cfr", "df", "inc", "names", "p", "project.all",
           "pop20", "test", "wpp.adj", "time1", "time2", "i", "countrylist")
rm(list = c(drops,"drops"))

save.image(file = "output_aspirin_sens1.Rda")

#####################################################################
# Sensitivity analysis 2
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer)   

#load TPs
load("new_rates.Rda")
load("set-up.Rda")

##coverage and effects
inc<-read.csv("scale-up-new-aspirin.csv", stringsAsFactors = F)%>%
  filter(intervention %in% c("Baseline", "Scenario 4"))

avg<-inc%>%group_by(intervention, year)%>%
  summarise(pp = mean(pp_cov), sp = mean(sp_cov))%>%
  arrange(intervention, year)%>%
  mutate(diff = pp - shift(pp))%>%
  group_by(intervention)%>%
  summarise(inc = mean(diff, na.rm=T))

ad_scale<-read.csv("adherence_scale.csv")

#updating TPs with new scale-up fxn for those 80+
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
b_rates<-left_join(b_rates, ad_scale)
b_rates<-data.table(b_rates)

#control
b_rates[, PP:=1-(PP_eff*pp.cov.inc*cov_adj)]
b_rates[, SP:=1-(SP_eff*sp.cov.inc*cov_adj)]
b_rates[is.na(PP),PP:=1]
b_rates[is.na(SP),SP:=1]

#treatment
b_rates[, PP_trt:=1-(0.5*PP_eff*(pp.trt.inc-pp.cov.inc)*cov_adj)]
b_rates[, SP_trt:=1-(0.5*SP_eff*(sp.trt.inc-sp.cov.inc)*cov_adj)]
b_rates[is.na(PP_trt),PP_trt:=1]
b_rates[is.na(SP_trt),SP_trt:=1]

b_rates<-b_rates[age>=55 & age<80, IR:=IR*PP*PP_trt]
b_rates<-b_rates[age>=55 & age<80, CF:=CF*SP*SP_trt]

b_rates<-b_rates%>%select(-pp.cov.inc, -sp.cov.inc,
                          -pp_cov, -sp_cov, -PP_eff, -SP_eff,
                          -PP, -SP, -pp.trt.inc, -sp.trt.inc,
                          -PP_trt, -SP_trt)

#test
test<-project.all("India")%>%
  bind_rows(., project.all("China"))%>%
  bind_rows(., project.all("Ethiopia")) %>%
  bind_rows(., project.all("United States"))  

any(is.na(test%>%filter(cause!="hhd")))

p<-test%>%filter(cause!="hhd")%>%
  group_by(year, intervention, location)%>%
  summarise(dead = sum(dead),
            sick=sum(sick),
            pop = sum(pop))

#inspect
ggplot(p, aes(x=year, y=dead, color=intervention))+
  geom_point()+
  facet_wrap(~location, scales = "free")

#####################################################################
#loop
output3<-project.all(countrylist[1])

time1<-Sys.time()

for(i in 2:182){
  output3<-bind_rows(output3, project.all(countrylist[i]))
}

time2<-Sys.time()
time2-time1 # ~8 mins for 182 countries

drops <- c("all", "b_rates", "cfr", "df", "inc", "names", "p", "project.all",
           "pop20", "test", "wpp.adj", "time1", "time2", "i", "countrylist")
rm(list = c(drops,"drops"))

save.image(file = "output_aspirin_sens2.Rda")


#####################################################################
## Tables and figures ##
#####################################################################

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer)   

load("output_aspirin2_new.Rda")
output1<-output2
load("output_aspirin_sens1.Rda")
load("output_aspirin_sens2.Rda")

groups<-read.csv("data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_gbd)%>%
  rename(location = location_gbd)

inc_adjust<-read.csv("data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_wb, location_gbd)%>%
  rename(Country = location_gbd)%>%
  right_join(., read.csv("new_events.csv", stringsAsFactors = F))%>%
  group_by(wb2021)%>%
  summarise(MI_ratio = mean(MI_ratio),
            stroke_ratio = mean(stroke_ratio),
            HF_ratio = mean(HF_ratio))%>%
  na.omit()

inc_adjust<-inc_adjust%>%
  bind_rows(., inc_adjust%>%filter(wb2021 == "LMIC")%>%
              mutate(wb2021 = "LIC"))%>%
  left_join(., groups)


# DEATHS AND EVENT AVERTED #
df<-bind_rows(output1%>%mutate(Results = "Main analysis")%>%filter(intervention %in% c("Baseline", "Scenario 4")),
              output2%>%mutate(Results = "Sensitivity analysis 1"),
              output3%>%mutate(Results = "Sensitivity analysis 2"))%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(intervention, Results, year)%>%
  summarise(Incidence = sum(newcases),
            Prevalence = sum(sick),
            CVD_deaths = sum(dead),
            All_deaths = sum(all.mx)/3)

df2<-bind_rows(output1%>%mutate(Results = "Main analysis")%>%filter(intervention %in% c("Baseline", "Scenario 4")),
              output2%>%mutate(Results = "Sensitivity analysis 1")%>%filter(intervention != "Baseline"),
              output3%>%mutate(Results = "Sensitivity analysis 2")%>%filter(intervention != "Baseline"))%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(intervention, Results, year)%>%
  summarise(Incidence = sum(newcases),
            Prevalence = sum(sick),
            CVD_deaths = sum(dead),
            All_deaths = sum(all.mx)/3)

DA<-df%>%select(year, intervention, Results, CVD_deaths)%>%
  spread(intervention, CVD_deaths)%>%
  mutate(diff = Baseline - `Scenario 4`)

ggplot(DA, aes(x=year, y=diff, color=Results))+
  geom_line()

DA2<-left_join(df2%>%filter(intervention!="Baseline")%>%select(year, Results, CVD_deaths)%>%
                 spread(Results, CVD_deaths),
               df2%>%filter(intervention=="Baseline")%>%ungroup()%>%select(year, Baseline=CVD_deaths))%>%
  ungroup()%>%
  select(-intervention)%>%
  gather(Results, "Scenario 4", -Baseline, -year)%>%
  mutate(diff = Baseline - `Scenario 4`)

ggplot(DA2, aes(x=year, y=diff, color=Results))+
  geom_line()


CA2<-left_join(df2%>%filter(intervention!="Baseline")%>%select(year, Results, Incidence)%>%
                 spread(Results, Incidence),
               df2%>%filter(intervention=="Baseline")%>%ungroup()%>%select(year, Baseline=Incidence))%>%
  ungroup()%>%
  select(-intervention)%>%
  gather(Results, "Scenario 4", -Baseline, -year)%>%
  mutate(diff = Baseline - `Scenario 4`)

ggplot(CA2, aes(x=year, y=diff, color=Results))+
  geom_line()

cdf<-left_join(DA2%>%rename(deaths = diff)%>%select(-Baseline, -`Scenario 4`),
               CA2%>%rename(cases = diff)%>%select(-Baseline, -`Scenario 4`))%>%
  arrange(Results, year)%>%
  group_by(Results)%>%
  mutate(`Cumulative deaths averted` = cumsum(deaths),
         `Cumulative cases averted` = cumsum(cases))%>%
  select(-deaths, -cases)%>%
  gather(metric, diff, -Results, -year)


ggplot(cdf, aes(x=year, y=diff/1e6, color=Results))+
  geom_line(size=1)+
  facet_wrap(~metric)+
  theme_bw()+
  ylab("Cumulative events averted (millions)")+
  xlab("Year")

ggsave("Sensitivity_figure.jpeg", height = 6, width =9)

write.csv(cdf%>%filter(year==2050), "sensitivity_results.csv")
