
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


#inspect
ggplot(b_rates%>%filter(location=="India", year==2019, cause!="hhd"), 
       aes(x=age, y=CF, color=sex))+
  geom_line()+
  facet_wrap(~cause)


#where is the case fatality higher for age 20 than 55?
probs<-b_rates%>%filter(cause=="ihd", age%in%c(20,55))%>%
  select(age, CF, location, year, sex, cause)%>%
  spread(age, CF)%>%
  filter(`55`<`20`)

fix<-unique(probs$location)
write.csv(fix, "list_cf_countries.csv", row.names = F)
#impose exponential fit to estimates IHD case fatality rate#
#take data on ages 55+ and fit for younger ages#
library(purrr)

test<-b_rates%>%
  filter(location%in%fix, cause=="ihd", age>55)%>%
  mutate(new.cf = log(CF))%>%
  select(year, age, new.cf, sex, location)%>%
  nest(-year, -sex, -location)%>%
  mutate(cf_slope = map(data, ~coef(lm(new.cf~age, data=.x))[["age"]]),
         cf_intercept = map(data, ~coef(lm(new.cf~age, data=.x))[["(Intercept)"]]))%>%
  select(-data) #takes a minute, warnings are fine

test$cf_slope<-unlist(test$cf_slope)
test$cf_intercept<-unlist(test$cf_intercept)


u60<-b_rates%>%
  filter(location%in%fix, cause=="ihd")%>%
  mutate(CF = log(CF))%>%
  left_join(., test)%>%
  group_by(year, sex, location)%>%arrange(age)%>%
  mutate(CF=ifelse(age<=55, age*cf_slope + cf_intercept, CF),
         CF = exp(CF))%>%
  mutate(data = "Fit exponentially")

og<-b_rates%>%filter(location%in%fix, cause=="ihd")%>%
  mutate(data = "Original")

plot<-bind_rows(og, u60)

ggplot(plot%>%filter(location=="India"), aes(x=age, y=CF, color=sex))+
  geom_line()+
  facet_wrap(~data)+
  ggtitle("IHD 'case fatality' for India")+
  ylab("Sick to dead transition probability")+
  xlab("Age")

ggsave("cf_model.jpeg", width=8, height=4)

#recombine and save
b_rates<-left_join(b_rates, u60%>%select(year, age, sex, cause, CF, location)%>%
                         rename(new_CF=CF))%>%
  mutate(new_CF = ifelse(is.na(new_CF), CF, new_CF),
         CF = new_CF)%>%
  select(-new_CF)

ggplot(b_rates%>%filter(location=="India", year==2019, cause!="hhd"), 
       aes(x=age, y=CF, color=sex))+
  geom_line()+
  facet_wrap(~cause)

rm(list = c("probs", "fix", "test", "u60", "og", "plot"))
save.image(file = "base_rates.Rda")

