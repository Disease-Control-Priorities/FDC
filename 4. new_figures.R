rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer)   

load("output.Rda")
load("output_aspirin.Rda")

#########

plot<-bind_rows(output%>%
                  filter(intervention== "Scenario 4", cause!="hhd")%>%
                  mutate(intervention = "Polypill"),
                output2%>%
                  filter(intervention%in%c("Baseline", "Scenario 4"),
                         cause!="hhd")%>%
                  mutate(intervention = ifelse(intervention=="Scenario 4", "Polypill w/ aspirin", "Baseline"))
                )%>%
  group_by(year, intervention)%>%
  summarise(Deaths = sum(dead),
            `New cases` = sum(newcases))

rm(output)
rm(output2)

plot2<-plot%>%gather(metric, val, -year, -intervention)
#change name from Baseline to Business as usual
#plot2<-read.csv("outputs/plot_data/dpc_data.csv", stringsAsFactors = F)
plot2$intervention[plot2$intervention=="Baseline"]<-"Business as usual"

ggplot(plot2%>%filter(intervention != "Polypill w/ aspirin"), 
       aes(x=year, y=val/1e6, color=intervention))+
  geom_line(aes(linetype=metric), size=1)+
  theme_bw()+
  xlab("Year")+
  ylab("Counts (millions)")+
  scale_linetype_discrete(name = "Metric") +
  scale_color_discrete(name = "Scenario") +
  xlim(2020,2050)

ggsave("outputs/death_prev_counts.jpeg", height=6, width=6)

ggplot(plot2, 
       aes(x=year, y=val/1e6, color=intervention))+
  geom_line(aes(linetype=metric), size=1)+
  theme_bw()+
  xlab("Year")+
  ylab("Counts (millions)")+
  scale_linetype_discrete(name = "Metric") +
  scale_color_discrete(name = "Scenario") +
  xlim(2020,2050)

ggsave("outputs/death_prev_counts_aspirin.jpeg", height=6, width=6)

write.csv(plot2, "outputs/plot_data/dpc_data.csv", row.names=F)

#########

load("output_tobacco.Rda")

output3<-output3%>%
  filter(cause!="hhd")%>%
  group_by(year, intervention)%>%
  summarise(Deaths = sum(dead))%>%
  spread(intervention, Deaths)%>%
  mutate(`Tobacco taxes and policies` = Baseline - `Tobacco policies and taxes`)%>%
  select(-Baseline, -`Tobacco policies and taxes`)
  
any(is.na(output3))

output<-plot%>%
  select(-`New cases`)%>%
  spread(intervention, Deaths)%>%
  mutate(Polypill = Baseline - Polypill,
        `Polypill w/ aspirin` = Baseline - `Polypill w/ aspirin`)%>%
  select(-Baseline)%>%
  left_join(., output3)%>%
  gather(intervention, DA, -year)

any(is.na(output))

rm(output3)

load("data_80_80_80/model_output_updated.Rda")
rm(aspirational)

output4<-progress%>%filter(intervention%in%c("b.a.u", "Antihypertensive therapy"))%>%
  filter(cause!="hhd")%>%
  group_by(year, intervention)%>%
  summarise(Deaths = sum(dead))%>%
  spread(intervention, Deaths)%>%
  mutate(DA = `b.a.u` - `Antihypertensive therapy`)%>%
  mutate(intervention = "Progress (80-80-80)")%>%
  select(year, DA, intervention)

rm(progress)

## Cumulative deaths averted


plot4<-bind_rows(output, output4)%>%
  group_by(intervention)%>%
  arrange(year)%>%
  mutate(DA = cumsum(DA))%>%
  mutate(intervention = factor(intervention, 
    levels = c("Polypill w/ aspirin", "Polypill", "Progress (80-80-80)", "Tobacco taxes and policies")))

any(is.na(plot4))
unique(plot4$intervention)

ggplot(plot4, aes(x=year, y=DA/1e6, color=intervention))+
  geom_line(size=1)+
  theme_bw()+
  xlim(2020,2050)+
  scale_color_discrete(name = "Scenario")+
  xlab("Year")+
  ylab("Cumulative deaths averted (millions)")+
  ylim(0,150)

ggsave("outputs/death_averted_cumulative.jpeg", height=6, width=6)

write.csv(plot4, "outputs/plot_data/dac_data.csv", row.names=F)

#########################################################

plot4<-read.csv("outputs/plot_data/dac_data.csv", stringsAsFactors = F)

plot4%>%filter(year==2050)

#########################################################
# https://data.worldbank.org/indicator/SP.POP.TOTL
plot5<-read.csv("outputs/plot_data/Fig1_data.csv", stringsAsFactors = F)%>%
  mutate(pop = ifelse(wb2021=="HIC", 1241374.28,
                      ifelse(wb2021=="UMIC", 2501427.94,
                             ifelse(wb2021=="LMIC",3363196.66, 701926.97))))%>%
  filter(year==2050)%>%
  group_by(intervention)%>%
  summarise(x50q30 = weighted.mean(x50q30, pop))

write.csv(plot5, "outputs/world_50q30.csv")

plot5b<-read.csv("outputs/plot_data/Fig1_data_aspririn.csv", stringsAsFactors = F)%>%
  mutate(pop = ifelse(wb2021=="HIC", 1241374.28,
                      ifelse(wb2021=="UMIC", 2501427.94,
                             ifelse(wb2021=="LMIC",3363196.66, 701926.97))))%>%
  filter(year==2050)%>%
  group_by(intervention)%>%
  summarise(x50q30 = weighted.mean(x50q30, pop))

write.csv(plot5b, "outputs/world_50q30_aspirin.csv")

### Appendix figure (new) ###
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
inc<-read.csv("scale-up_withaspirin.csv", stringsAsFactors = F)

#updating TPs with new scale-up fxn for those 80+
#run regression for age 80-94, grouped by year, sex, cause, intervention
library(purrr)
library(broom)

reg<-b_rates%>%
  filter(age>=80 & age<=90)%>%
  nest(-year, -sex, -cause, -location)%>%
  mutate(ir_slope = map(data, ~coef(lm(IR~age, data=.x))[["age"]]),
         cf_slope = map(data, ~coef(lm(CF~age, data=.x))[["age"]]))%>%
  select(-data)#takes 1-2 minutes

reg$ir_slope<-unlist(reg$ir_slope)
reg$cf_slope<-unlist(reg$cf_slope)

b_rates<-left_join(b_rates, inc)

b_rates[, PP:=1-(PP_eff*pp.cov.inc)]
b_rates[, SP:=1-(SP_eff*sp.cov.inc)]
b_rates[is.na(PP),PP:=1]
b_rates[is.na(SP),SP:=1]

b_rates<-b_rates[age>=55 & age<80, IR:=IR*PP]
b_rates<-b_rates[age>=55 & age<80, CF:=CF*SP]

b_rates<-b_rates%>%select(-IRadjust, -CFadjust, -pp.cov.inc, -sp.cov.inc,
                          -pp_cov, -sp_cov, -PP_eff, -SP_eff,
                          -PP, -SP)

ggplot(b_rates%>%filter(year==2030, sex=="Female", cause=="ihd", location=="China"), 
       aes(x=age, y=CF, color=intervention))+
  geom_line(size=0.8)




Country<-"India"

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
  
  
  
ggplot(intervention_rates%>%filter(year%in%c(2020,2035,2050), sex=="Female", cause=="ihd"), 
       aes(x=age, y=CF*1e5, color=intervention))+
  geom_line(size=0.8)+
  facet_wrap(~year)+
  ylab("Case fatality rate (per 100,000)")+
  xlab("Age")+
  theme_bw()+ 
  scale_y_sqrt()
  
ggsave("outputs/CF_plot_India.jpeg", height=6, width=8)
  
  ggplot(intervention_rates%>%filter(year%in%c(2020,2035,2050), sex=="Female", cause=="ihd"), 
         aes(x=age, y=IR*1e5, color=intervention))+
    geom_line(size=0.8)+
    facet_wrap(~year)+
    ylab("Incidence rate (per 100,000)")+
    xlab("Age")+
    theme_bw()+ scale_y_sqrt()
  
ggsave("outputs/IR_plot_India.jpeg", height=6, width=8)

  
