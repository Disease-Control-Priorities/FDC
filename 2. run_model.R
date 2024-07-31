
#################################################################################################
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer)   

#################################################################################################
#load TPs
load("new_rates.Rda")
load("set-up.Rda")

##coverage and effects
inc<-read.csv("coverage_data/new_scale_up.csv", stringsAsFactors = F)

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
output<-project.all(countrylist[1])

time1<-Sys.time()

for(i in 2:182){
  output<-bind_rows(output, project.all(countrylist[i]))
}

time2<-Sys.time()
time2-time1 #40 mins for 182 countries

drops <- c("all", "b_rates", "cfr", "df", "inc", "names", "p", "project.all",
           "pop20", "test", "wpp.adj", "time1", "time2", "i", "countrylist")
rm(list = c(drops,"drops"))

save.image(file = "output3.Rda")


######################################

load("output3.Rda")

pop<-output%>%filter(year==2019, intervention=="Current care")%>%
  group_by(location)%>%summarise(Nx = sum(pop, na.rm = T))


#########################################################################
#Tables#
#########################################################################

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

table1<-output%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(intervention, year)%>%
  summarise(Incidence = sum(newcases),
            Prevalence = sum(sick),
            CVD_deaths = sum(dead),
            All_deaths = sum(all.mx)/3)%>%
  filter(year==2020 & intervention=="Current care" | year==2050 | year == 2045 | year ==2040)

write.csv(table1, "outputs/TABLE2_072027.csv")

#for app#

table1b<-output%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(intervention, year, location)%>%
  summarise(Incidence = sum(newcases),
            Prevalence = sum(sick),
            CVD_deaths = sum(dead),
            All_deaths = sum(all.mx)/3)%>%
  filter(year==2020 & intervention=="Current care" | year==2050 | year == 2045 | year ==2040)

write.csv(table1b, "shiny/FDC/TABLE1.csv")

##


table2<-output%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(intervention)%>%
  summarise(Deaths=sum(dead),
            Cases=sum(newcases))

base<-table2%>%filter(intervention=="Current care")%>%
  rename(Base_deaths = Deaths,
         Base_cases = Cases)%>%
  select(-intervention )

table2<-merge(table2, base)%>%
  mutate(deaths.averted = Base_deaths - Deaths,
         cases.averted = Base_cases-Cases)

write.csv(table2, "outputs/CUMULATIVE_072027.csv")

table3<-output%>%
  filter(cause!="hhd")%>%
  group_by(intervention)%>%
  summarise(All.cause.deaths = sum(all.mx)/3)

write.csv(table3, "outputs/allcauseMX_072027.csv")




######### Format tables for paper ##############

unique(table1$intervention)

tab_base<-table1%>%ungroup()%>%
  filter(intervention %in% c("Current care", "Base case, population-wide", "Base case, targeted"))%>%
  arrange(year)%>%
  mutate(header = paste(intervention, year))%>%
  select(-intervention, -year)%>%
  gather(measure, value, -header)%>%
  filter(measure != "All_deaths")%>%
  spread(header, value)%>%
  select("Measure" = "measure",
         "Current care (2020)" = "Current care 2020",
         "Current care (2040)" = "Current care 2040",
         "Targeted (2040)" = "Base case, targeted 2040",
         "Population (2040)" = "Base case, population-wide 2040",
         "Current care (2045)" = "Current care 2045",
         "Targeted (2045)" = "Base case, targeted 2045",
         "Population (2045)" = "Base case, population-wide 2045",
         "Current care (2050)" = "Current care 2050",
         "Targeted (2050)" = "Base case, targeted 2050",
         "Population (2050)" = "Base case, population-wide 2050")%>%
  bind_rows(., data.frame(Measure = c("CVD events averted (millions)", "Change in CVD events (%)",
                                      "CVD prev averted",
                                      "Change in CVD prevalence (%)",
                                      "CVD deaths averted (millions)", "Change in CVD deaths (%)")))


#CVD events averted#
tab_base[4,4]<-tab_base[2,3] - tab_base[2,4]
tab_base[4,5]<-tab_base[2,3] - tab_base[2,5]

tab_base[4,7]<-tab_base[2,6] - tab_base[2,7]
tab_base[4,8]<-tab_base[2,6] - tab_base[2,8]

tab_base[4,10]<-tab_base[2,9] - tab_base[2,10]
tab_base[4,11]<-tab_base[2,9] - tab_base[2,11]

#CVD prev#
tab_base[6,4]<-tab_base[3,3] - tab_base[3,4]
tab_base[6,5]<-tab_base[3,3] - tab_base[3,5]

tab_base[6,7]<-tab_base[3,6] - tab_base[3,7]
tab_base[6,8]<-tab_base[3,6] - tab_base[3,8]

tab_base[6,10]<-tab_base[3,9] - tab_base[3,10]
tab_base[6,11]<-tab_base[3,9] - tab_base[3,11]

#CVD deaths averted#
tab_base[8,4]<-tab_base[1,3] - tab_base[1,4]
tab_base[8,5]<-tab_base[1,3] - tab_base[1,5]

tab_base[8,7]<-tab_base[1,6] - tab_base[1,7]
tab_base[8,8]<-tab_base[1,6] - tab_base[1,8]

tab_base[8,10]<-tab_base[1,9] - tab_base[1,10]
tab_base[8,11]<-tab_base[1,9] - tab_base[1,11]

tab_base[,2:11]<-tab_base[,2:11]/1e6


## percentages
tab_base[5,2:11]<-100*(tab_base[4,2:11]/tab_base[2,2:11])
tab_base[7,2:11]<-100*(tab_base[6,2:11]/tab_base[3,2:11])
tab_base[9,2:11]<-100*(tab_base[8,2:11]/tab_base[1,2:11])

## 2 sig figs ##
tab_base[,2:11]<-signif(tab_base[,2:11], 2)

tab_base<-tab_base%>%
  mutate(Measure = ifelse(Measure=="CVD_deaths", "CVD deaths (millions)", Measure),
         Measure= ifelse(Measure=="Incidence", "New CVD events (millions)", Measure),
         Measure=ifelse(Measure=="Prevalence", "Prevalent CVD cases (millions)", Measure))%>%
  filter(Measure!="CVD prev averted")


tab_base<-tab_base%>%slice(2,4,5,3,6,1,7,8)

write.csv(tab_base, "outputs/Table1_base_case.csv", row.names = F)


## Best case ##

tab_best<-table1%>%ungroup()%>%
  filter(intervention %in% c("Current care", "Best case, population-wide", "Best case, targeted"))%>%
  arrange(year)%>%
  mutate(header = paste(intervention, year))%>%
  select(-intervention, -year)%>%
  gather(measure, value, -header)%>%
  filter(measure != "All_deaths")%>%
  spread(header, value)%>%
  select("Measure" = "measure",
         "Current care (2020)" = "Current care 2020",
         "Current care (2040)" = "Current care 2040",
         "Targeted (2040)" = "Best case, targeted 2040",
         "Population (2040)" = "Best case, population-wide 2040",
         "Current care (2045)" = "Current care 2045",
         "Targeted (2045)" = "Best case, targeted 2045",
         "Population (2045)" = "Best case, population-wide 2045",
         "Current care (2050)" = "Current care 2050",
         "Targeted (2050)" = "Best case, targeted 2050",
         "Population (2050)" = "Best case, population-wide 2050")%>%
  bind_rows(., data.frame(Measure = c("CVD events averted (millions)", "Change in CVD events (%)",
                                      "CVD prev averted",
                                      "Change in CVD prevalence (%)",
                                      "CVD deaths averted (millions)", "Change in CVD deaths (%)")))


#CVD events averted#
tab_best[4,4]<-tab_best[2,3] - tab_best[2,4]
tab_best[4,5]<-tab_best[2,3] - tab_best[2,5]

tab_best[4,7]<-tab_best[2,6] - tab_best[2,7]
tab_best[4,8]<-tab_best[2,6] - tab_best[2,8]

tab_best[4,10]<-tab_best[2,9] - tab_best[2,10]
tab_best[4,11]<-tab_best[2,9] - tab_best[2,11]

#CVD prev#
tab_best[6,4]<-tab_best[3,3] - tab_best[3,4]
tab_best[6,5]<-tab_best[3,3] - tab_best[3,5]

tab_best[6,7]<-tab_best[3,6] - tab_best[3,7]
tab_best[6,8]<-tab_best[3,6] - tab_best[3,8]

tab_best[6,10]<-tab_best[3,9] - tab_best[3,10]
tab_best[6,11]<-tab_best[3,9] - tab_best[3,11]

#CVD deaths averted#
tab_best[8,4]<-tab_best[1,3] - tab_best[1,4]
tab_best[8,5]<-tab_best[1,3] - tab_best[1,5]

tab_best[8,7]<-tab_best[1,6] - tab_best[1,7]
tab_best[8,8]<-tab_best[1,6] - tab_best[1,8]

tab_best[8,10]<-tab_best[1,9] - tab_best[1,10]
tab_best[8,11]<-tab_best[1,9] - tab_best[1,11]

tab_best[,2:11]<-tab_best[,2:11]/1e6


## percentages
tab_best[5,2:11]<-100*(tab_best[4,2:11]/tab_best[2,2:11])
tab_best[7,2:11]<-100*(tab_best[6,2:11]/tab_best[3,2:11])
tab_best[9,2:11]<-100*(tab_best[8,2:11]/tab_best[1,2:11])

## 2 sig figs ##
tab_best[,2:11]<-signif(tab_best[,2:11], 2)

tab_best<-tab_best%>%
  mutate(Measure = ifelse(Measure=="CVD_deaths", "CVD deaths (millions)", Measure),
         Measure= ifelse(Measure=="Incidence", "New CVD events (millions)", Measure),
         Measure=ifelse(Measure=="Prevalence", "Prevalent CVD cases (millions)", Measure))%>%
  filter(Measure!="CVD prev averted")


tab_best<-tab_best%>%slice(2,4,5,3,6,1,7,8)

write.csv(tab_best, "outputs/eTable2_best_case.csv", row.names = F)


## Worst case ##

tab_worst<-table1%>%ungroup()%>%
  filter(intervention %in% c("Current care", "Worst case, population-wide", "Worst case, targeted"))%>%
  arrange(year)%>%
  mutate(header = paste(intervention, year))%>%
  select(-intervention, -year)%>%
  gather(measure, value, -header)%>%
  filter(measure != "All_deaths")%>%
  spread(header, value)%>%
  select("Measure" = "measure",
         "Current care (2020)" = "Current care 2020",
         "Current care (2040)" = "Current care 2040",
         "Targeted (2040)" = "Worst case, targeted 2040",
         "Population (2040)" = "Worst case, population-wide 2040",
         "Current care (2045)" = "Current care 2045",
         "Targeted (2045)" = "Worst case, targeted 2045",
         "Population (2045)" = "Worst case, population-wide 2045",
         "Current care (2050)" = "Current care 2050",
         "Targeted (2050)" = "Worst case, targeted 2050",
         "Population (2050)" = "Worst case, population-wide 2050")%>%
  bind_rows(., data.frame(Measure = c("CVD events averted (millions)", "Change in CVD events (%)",
                                      "CVD prev averted",
                                      "Change in CVD prevalence (%)",
                                      "CVD deaths averted (millions)", "Change in CVD deaths (%)")))


#CVD events averted#
tab_worst[4,4]<-tab_worst[2,3] - tab_worst[2,4]
tab_worst[4,5]<-tab_worst[2,3] - tab_worst[2,5]

tab_worst[4,7]<-tab_worst[2,6] - tab_worst[2,7]
tab_worst[4,8]<-tab_worst[2,6] - tab_worst[2,8]

tab_worst[4,10]<-tab_worst[2,9] - tab_worst[2,10]
tab_worst[4,11]<-tab_worst[2,9] - tab_worst[2,11]

#CVD prev#
tab_worst[6,4]<-tab_worst[3,3] - tab_worst[3,4]
tab_worst[6,5]<-tab_worst[3,3] - tab_worst[3,5]

tab_worst[6,7]<-tab_worst[3,6] - tab_worst[3,7]
tab_worst[6,8]<-tab_worst[3,6] - tab_worst[3,8]

tab_worst[6,10]<-tab_worst[3,9] - tab_worst[3,10]
tab_worst[6,11]<-tab_worst[3,9] - tab_worst[3,11]

#CVD deaths averted#
tab_worst[8,4]<-tab_worst[1,3] - tab_worst[1,4]
tab_worst[8,5]<-tab_worst[1,3] - tab_worst[1,5]

tab_worst[8,7]<-tab_worst[1,6] - tab_worst[1,7]
tab_worst[8,8]<-tab_worst[1,6] - tab_worst[1,8]

tab_worst[8,10]<-tab_worst[1,9] - tab_worst[1,10]
tab_worst[8,11]<-tab_worst[1,9] - tab_worst[1,11]

tab_worst[,2:11]<-tab_worst[,2:11]/1e6


## percentages
tab_worst[5,2:11]<-100*(tab_worst[4,2:11]/tab_worst[2,2:11])
tab_worst[7,2:11]<-100*(tab_worst[6,2:11]/tab_worst[3,2:11])
tab_worst[9,2:11]<-100*(tab_worst[8,2:11]/tab_worst[1,2:11])

## 2 sig figs ##
tab_worst[,2:11]<-signif(tab_worst[,2:11], 2)

tab_worst<-tab_worst%>%
  mutate(Measure = ifelse(Measure=="CVD_deaths", "CVD deaths (millions)", Measure),
         Measure= ifelse(Measure=="Incidence", "New CVD events (millions)", Measure),
         Measure=ifelse(Measure=="Prevalence", "Prevalent CVD cases (millions)", Measure))%>%
  filter(Measure!="CVD prev averted")


tab_worst<-tab_worst%>%slice(2,4,5,3,6,1,7,8)

write.csv(tab_worst, "outputs/eTable1_worst_case.csv", row.names = F)


##########################
# eTable 3 #
##########################

unique(output$cause)

pop<-output%>%filter(age>=55 & age<80)%>%
  group_by(sex, year, location, intervention)%>%
  summarise(pop= sum(pop)/4,
            cvd = sum(sick))%>%
  na.omit()

inc<-read.csv("coverage_data/new_scale_up.csv", stringsAsFactors = F)%>%
  group_by(intervention, year, location)%>%
  summarise(cov=mean(pp.trt.inc))

#PP adverse effects
adv_pp<-left_join(pop, inc)%>%
  mutate(no_cvd = pop-cvd)%>%
  mutate(dizzy = no_cvd*cov*0.025,
         gi_bleed = no_cvd*cov*0.002)%>%
  group_by(intervention, year)%>%
  summarise(dizzy = sum(dizzy),
            gi_bleed = sum(gi_bleed))%>%
  mutate(total_events = dizzy+gi_bleed)%>%
  filter(intervention %in% c("Best case, population-wide", "Base case, population-wide",
                             "Worst case, population-wide"))%>%
  arrange(year)%>%
  group_by(intervention)%>%
  mutate(Dizziness = cumsum(dizzy),
         `Nonfatal GI bleeding` = cumsum(gi_bleed))%>%
  select(-dizzy, -gi_bleed, - total_events)%>%
  filter(year %in% c(2040,2045,2050))%>%
  mutate(`Total events` = Dizziness + `Nonfatal GI bleeding`)

write.csv(adv_pp, "outputs/eTable3.csv", row.names = F)



##########################
# eTable 4 #
##########################

tab4<-table2%>%filter(intervention!="Current care")%>%
  select(intervention, deaths.averted)%>%
  mutate(deaths.averted = signif(deaths.averted/1e6, 2))

tab4<-tab4 %>% separate(intervention, c("Scenario","Intervention"), sep = ',')%>%
  spread(Intervention, deaths.averted)

tab4<-cbind(tab4, data.frame(`Tobacco control` = c(20,20,20)))%>%
  cbind(., data.frame(`Hypertension treatment` = c(53,53,53)))%>%
  select(Scenario, Tobacco.control, Hypertension.treatment, Targeted=" targeted", Population=" population-wide")

write.csv(tab4, "outputs/eTable4.csv", row.names = F)





