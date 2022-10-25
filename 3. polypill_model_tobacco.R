
#################################################################################################
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer)   

#################################################################################################
countrylist <- read.csv("data_80_80_80/super_regions.csv", stringsAsFactors=FALSE)%>%filter(location!="Global", 
                                                                              location!="American Samoa",
                                                                              location!="Andorra",
                                                                              location!= "Bermuda",
                                                                              location!= "Dominica",
                                                                              location!="Greenland",
                                                                              location!="Marshall Islands",
                                                                              location!="Northern Mariana Islands",
                                                                              location!="Palestine",
                                                                              location!="Taiwan (Province of China)",
                                                                              location!="Guam",
                                                                              location!="Puerto Rico",
                                                                              location!="South Sudan",
                                                                              location!="Virgin Islands, U.S.")%>%pull(location)


names<-read.csv("data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(location_gbd, gbd2019, iso3)%>%
  rename(location = location_gbd)

#add covid mx data
load("data_80_80_80/wpp.adj.Rda")

#Covid mx ~= excess mortality
b_rates<-fread("data_80_80_80/base_rates_2022.csv")
b_rates<-left_join(b_rates, wpp.adj%>%
                     rename(location = location_name)%>%
                     select( -Nx, -mx, -iso3))

#add location-specific CFAARC estimates
df<-read.csv("data_80_80_80/IHME-CFR.csv", stringsAsFactors = F)
cfr<-df%>%select(-c(lower, upper, measure, age, metric))%>%
  filter(cause!="All causes")%>%
  spread(year, val)%>%
  mutate(CFAARC = 1+log(`2019`/`2009`)/10)

cfr$cause[cfr$cause == "Ischemic heart disease"]<-"ihd"
cfr$cause[cfr$cause == "Hypertensive heart disease"]<-"hhd"
cfr$cause[cfr$cause == "Ischemic stroke"]<-"istroke"
cfr$cause[cfr$cause == "Intracerebral hemorrhage"]<-"hstroke"

cfr<-left_join(cfr%>%rename(gbd2019 = location), names)%>%
  filter(location %in% countrylist)%>%
  select(-c(gbd2019, iso3, `2019`, `2009`))

##add reduction in bg.mx over time
all<-df%>%select(-c(lower, upper, measure, age, metric))%>%
  filter(cause=="All causes")%>%
  spread(year, val)%>%
  mutate(BGAARC = 1+log(`2019`/`2009`)/10)

all<-left_join(all%>%rename(gbd2019 = location), names)%>%
  filter(location %in% countrylist)%>%
  select(-c(gbd2019, iso3, `2019`, `2009`, cause))

b_rates<-left_join(b_rates, all)%>%select(-c(CFAARC))
b_rates<-left_join(b_rates, cfr)

#adjust rates
#b_rates[year>=2020, CF :=CF*(CFAARC^(year-2019))]
b_rates[year>=2020, BG.mx :=BG.mx*(BGAARC^(year-2019))]
b_rates[year>=2020, BG.mx.all :=BG.mx.all*(BGAARC^(year-2019))]

#add age-20 cohort projections from demographic model
pop20<-read.csv("data_80_80_80/PopulationsAge20_2050.csv", stringsAsFactors = F)

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=ifelse(is.na(Nx2), pop, Nx2))%>%
  select(-c(Nx2))

#check rates
b_rates[is.na(covid.mx), covid.mx:=0]
b_rates[covid.mx>=1, covid.mx:=0.9]

#rebalance TPs w/ covid such that they sum to less than 1
#especially @ old ages where covid deaths are high
b_rates[,check_well := BG.mx+covid.mx+IR]
b_rates[,check_sick := BG.mx+covid.mx+CF]

#first ensure that background mortality + covid <1
b_rates[check_well>1 | check_sick>1, covid.mx:=ifelse(1-BG.mx<covid.mx, 1-BG.mx, covid.mx)]
#then proportionally reduce rates by check_well
b_rates[check_well>1, covid.mx:= covid.mx - covid.mx*(check_well-1)/(covid.mx+BG.mx+IR)]
b_rates[check_well>1, BG.mx   := BG.mx    - BG.mx*   (check_well-1)/(covid.mx+BG.mx+IR)]
b_rates[check_well>1, IR      := IR       - IR*      (check_well-1)/(covid.mx+BG.mx+IR)]

b_rates[,check_well := BG.mx+covid.mx+IR]
b_rates[check_well>1]

#same process for check_sick
b_rates[check_sick>1, covid.mx:= covid.mx - covid.mx*(check_sick-1)/(covid.mx+BG.mx+CF)]
b_rates[check_sick>1, BG.mx   := BG.mx    - BG.mx*   (check_sick-1)/(covid.mx+BG.mx+CF)]
b_rates[check_sick>1, CF      := CF       - CF*      (check_sick-1)/(covid.mx+BG.mx+CF)]

b_rates[,check_sick := BG.mx+covid.mx+CF]
b_rates[check_sick>1]

#check that no BG.mx.all+covid>1
b_rates[covid.mx+BG.mx.all>1]
b_rates[, newcases:=0]

##coverage and effects
base_inc<-read.csv("scale-up_withaspirin.csv", stringsAsFactors = F)%>%
  filter(intervention=="Baseline")%>%
  select(location, cause, year, pp.cov.inc,
         sp.cov.inc, PP_eff, SP_eff)%>%
  mutate(base_PP=PP_eff*pp.cov.inc,
         base_SP=SP_eff*sp.cov.inc)%>%
  select(-c(pp.cov.inc, sp.cov.inc, PP_eff, SP_eff))

inc<-read.csv("data_NCDC/tobaccoandalcohol_efficacy6.csv", stringsAsFactors = F)%>%
  filter(Risk=="Smoking", Outcome %in% c("Ischemic heart disease", "Ischemic stroke", "Intracerebral hemorrhage"))%>%
  mutate(SP_eff = 1-((1-Mortality.reduction.policy)*(1-Mortality.reduction.tax)),
         cause = ifelse(Outcome=="Ischemic heart disease", "ihd",
                        ifelse(Outcome =="Ischemic stroke", "istroke", "hstroke")))%>%
  rename(location = Country)%>%
  select(location, SP_eff, cause)


  a<-merge(data.frame(location=unique(inc$location)),
           data.frame(cause=c("ihd", "hstroke", "istroke")))
  
inc<-inc%>%
  full_join(.,a)%>%
  merge(., data.frame(year=2017:2050))%>%
  left_join(., read.csv("data_NCDC/delays.csv"))%>%
  mutate(sp.cov.inc = ifelse(year<2023, 0*delay,
                             ifelse(year==2023, (1/3)*delay,
                                    ifelse(year==2024, (2/3)*delay, 1*delay))))%>%
  mutate(intervention = "Tobacco policies and taxes")%>%
  left_join(., base_inc)%>%
  bind_rows(., base_inc%>%mutate(intervention="Baseline"))%>%
  mutate(SP_eff = ifelse(is.na(SP_eff),0,SP_eff),
         sp.cov.inc = ifelse(is.na(sp.cov.inc),0,sp.cov.inc),
         SP_eff = SP_eff*sp.cov.inc,
         new_sp = 1-((1-base_SP)*(1-SP_eff)),
         new_sp = ifelse(intervention=="Baseline", base_SP, new_sp))


#################################################################################################
# As a function
#################################################################################################
project.all <- function(Country){
  #################################################################################################
  #################################################################################################
  intervention_rates<-b_rates[location==Country & cause!="hhd"]
  
  intervention_rates<-left_join(intervention_rates, inc)
  
  intervention_rates[,SP:=1-(new_sp)]
  intervention_rates[is.na(SP),SP:=1]
  intervention_rates[,PP:=1-(base_PP)]
  intervention_rates[is.na(PP),PP:=1]
  
  #intervention_rates<-unique(intervention_rates[,-c("PP_ef", "PP_af", "PP_cov", "PP_base", 
  #                                                  "SP_ef", "SP_af", "SP_cov", "SP_base",
  #                                                  "reach", "wb2021", "check_well", "check_sick")])
  
  intervention_rates[, CF:=CF*SP]
  intervention_rates[, IR:=IR*PP]
  
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
test<-project.all("India")

any(is.na(test))

p<-test%>%
  filter(cause!="hhd")%>%
  group_by(year, intervention)%>%
  summarise(dead = sum(dead),
            sick=sum(sick))

#inspect
ggplot(p, aes(x=year, y=dead, color=intervention))+
  geom_point()

ggplot(p, aes(x=year, y=sick, color=intervention))+
  geom_point()

#####################################################################
#loop
output3<-project.all(countrylist[1])

time1<-Sys.time()

for(i in 2:182){
  output3<-bind_rows(output3, project.all(countrylist[i]))
}

time2<-Sys.time()
time2-time1 #6-7 mins for 182 countries

drops <- c("a", "all", "b_rates", "base_inc", "cfr", "df", "inc", "names", "p", "project.all",
           "pop20", "test", "wpp.adj", "time1", "time2", "i", "countrylist")
rm(list = c(drops,"drops"))

save.image(file = "output_tobacco.Rda")
