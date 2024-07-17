rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer, countrycode)   

#coverage, events
inc<-read.csv("cascade_data_trt.csv", stringsAsFactors = F)%>%
  filter(scenario %in% c("Scenario 2", "Scenario 3", "Scenario 4"))%>%
  select(-Treated, -Control)%>%
  spread(scenario, Aware)%>%
  mutate(`Scenario 3` = `Scenario 3` - `Scenario 2`,
         `Scenario 4` = `Scenario 4` - `Scenario 2`)%>%
  select(-`Scenario 2`)%>%
  gather(intervention, cov, -location_name, -year)%>%
  mutate(iso3 = countrycode::countrycode(location_name, "country.name", "iso3c"))

any(is.na(inc))

locs<-read.csv("coverage_data/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(gbd2019, iso3, wb2021, location_gbd, location_ncdrisc, wbregion)%>%
  rename(location = location_gbd)

inc<-left_join(inc, locs%>%select(iso3, location))

isos<-unique(locs$iso3)

#PIN, age 55-79
load("output2.Rda")
load("output_aspirin2.Rda")

pop<-output%>%filter(age>=55 & age<80, intervention %in% c("Scenario 3", "Scenario 4"))%>%
  group_by(sex, year, location, intervention)%>%
  summarise(pop= sum(pop)/4,
            cvd = sum(sick))%>%
  mutate(regimen = "No aspirin")%>%bind_rows(., output2%>%
  filter(age>=55 & age<80, intervention %in% c("Scenario 3", "Scenario 4"))%>%
  group_by(sex, year, location, intervention)%>%
  summarise(pop= sum(pop)/4,
            cvd = sum(sick))%>%
    mutate(regimen = "Aspirin"))%>%
  na.omit()

############
# 70q0 all cause
#############

pop<-read_xlsx("WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", skip=16, 
               col_types = c("numeric", "text", "text", 
                             "text", "text", "text", "text",
                             "text", "text", "text", "numeric",
                             rep("numeric", 101)))%>% #WPP pop estimates for age 0-20
  select(-Index , -Variant, -Notes, -`Location code`, -`ISO2 Alpha-code`, 
         -`SDMX code**`, -Type, -`Parent code`, -`Region, subregion, country or area *`)%>%
  gather(age, pop, -Year, -`ISO3 Alpha-code`)%>%
  rename(iso3 = `ISO3 Alpha-code`, year=Year)%>%
  filter(iso3 %in% isos)%>%
  left_join(., locs%>%select(iso3, location, wbregion))%>%
  select(-iso3)%>%
  filter(year<=2050 & year>=2020)%>%
  mutate(age = as.numeric(age))%>%
  filter(age<20)

mx1<-read.csv("WPP2024_DeathsBySingleAgeSex_Medium_1950-2023.csv", stringsAsFactors = F)%>%
  filter(Time %in% c(2020:2023), AgeGrpStart<20, ISO3_code %in% isos)%>%
  select(iso3 = ISO3_code, dead= DeathTotal, year=Time, age = AgeGrpStart)

mx2<-read.csv("WPP2024_DeathsBySingleAgeSex_Medium_1950-2023.csv", stringsAsFactors = F)%>%
  filter(Time %in% c(2024:2050), AgeGrpStart<20, ISO3_code %in% isos)%>%
  select(iso3 = ISO3_code, dead= DeathTotal, year=Time, age = AgeGrpStart)

add<-bind_rows(mx1, mx2)%>%
  left_join(., pop)%>%
  left_join(., locs%>%select(iso3, location, wbregion))%>%
  select(-iso3)%>%
  merge(., data.frame(intervention = c("Current care", "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4")))%>%
  merge(., data.frame(regimen = c("No aspirin", "Aspirin")))

any(is.na(add))

comb<-bind_rows(output%>%mutate(regimen = "No aspirin"), output2%>%mutate(regimen = "Aspirin"))%>%
  bind_rows(., output2%>%filter(intervention=="Baseline")%>%mutate(regimen = "No aspirin"))%>%
  left_join(., locs%>%select(location, wbregion))%>%
  mutate(intervention = ifelse(intervention=="Baseline", "Current care", intervention))

CVD<-comb%>%filter(cause=="ihd")%>% #doesn't matter which
  group_by(age, wbregion, year, intervention, regimen)%>%
  filter(age<70)%>%summarise(pop=sum(pop), dead=sum(all.mx))%>% 
  bind_rows(., add)

CVD$age.group<-NA
CVD$age.group[CVD$age>=0 & CVD$age<5]<-"0-4"
CVD$age.group[CVD$age>=5 & CVD$age<10]<-"5-9"
CVD$age.group[CVD$age>=10 & CVD$age<15]<-"10-14"
CVD$age.group[CVD$age>=15 & CVD$age<20]<-"15-19"
CVD$age.group[CVD$age>=20 & CVD$age<25]<-"20-24"
CVD$age.group[CVD$age>=25 & CVD$age<30]<-"25-29"
CVD$age.group[CVD$age>=30 & CVD$age<35]<-"30-34"
CVD$age.group[CVD$age>=35 & CVD$age<40]<-"35-39"
CVD$age.group[CVD$age>=40 & CVD$age<45]<-"40-44"
CVD$age.group[CVD$age>=45 & CVD$age<50]<-"45-49"
CVD$age.group[CVD$age>=50 & CVD$age<55]<-"50-54"
CVD$age.group[CVD$age>=55 & CVD$age<60]<-"55-59"
CVD$age.group[CVD$age>=60 & CVD$age<65]<-"60-64"
CVD$age.group[CVD$age>=65 & CVD$age<70]<-"65-69"

WB_50q30<-CVD%>%group_by(age.group,  wbregion, year, intervention, regimen)%>%
  summarise(pop=sum(pop), dead=sum(dead))
WB_50q30$mx<-WB_50q30$dead/WB_50q30$pop
any(is.na(WB_50q30))

WB_50q30<-WB_50q30%>%group_by(wbregion, year, intervention, regimen)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))


ggplot(WB_50q30%>%filter(intervention!="Baseline", year>=2020), 
       aes(x=year, y=x50q30, color=intervention, linetype=regimen))+
  #geom_line()+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  facet_wrap(~wbregion, nrow=1)+
  labs(color="Scenario", linetype="Regimen")+
  xlim(2020,2050)+
  ylab("70q0")+
  xlab("Year")+
  theme_bw()+
  ylim(0,0.5)+
  scale_linetype_manual(values=c("solid", "twodash"))+
  theme(axis.text.x = element_text(angle=45))+
  scale_color_viridis(discrete = T) 

ggsave("plot_70q0.jpeg", height=6, width=12)
write.csv(WB_50q30%>%filter(intervention!="Baseline"), "data_70q0.csv", row.names = F)


### 50q30 ####
CVD<-comb%>%filter(cause=="ihd")%>%
  group_by(age, sex, location, wbregion, year, intervention, regimen)%>%
  filter(age>=30 & age<80)%>%summarise(pop=sum(pop), dead=sum(all.mx)) #divide pop by 3 to avoid over counting for each cause

CVD$age.group<-NA
CVD$age.group[CVD$age>=30 & CVD$age<35]<-"30-34"
CVD$age.group[CVD$age>=35 & CVD$age<40]<-"35-39"
CVD$age.group[CVD$age>=40 & CVD$age<45]<-"40-44"
CVD$age.group[CVD$age>=45 & CVD$age<50]<-"45-49"
CVD$age.group[CVD$age>=50 & CVD$age<55]<-"50-54"
CVD$age.group[CVD$age>=55 & CVD$age<60]<-"55-59"
CVD$age.group[CVD$age>=60 & CVD$age<65]<-"60-64"
CVD$age.group[CVD$age>=65 & CVD$age<70]<-"65-69"
CVD$age.group[CVD$age>=70 & CVD$age<75]<-"70-74"
CVD$age.group[CVD$age>=75 & CVD$age<80]<-"75-79"

WB_50q30<-CVD%>%group_by(age.group,  wbregion, year, intervention, regimen)%>%
  summarise(pop=sum(pop), dead=sum(dead))
WB_50q30$mx<-WB_50q30$dead/WB_50q30$pop
any(is.na(WB_50q30))

WB_50q30<-WB_50q30%>%group_by(wbregion, year, intervention, regimen)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))


ggplot(WB_50q30%>%filter(intervention!="Baseline", year>=2020), 
       aes(x=year, y=x50q30, color=intervention, linetype=regimen))+
  #geom_line()+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  facet_wrap(~wbregion, nrow=1)+
  labs(color="Scenario", linetype="Regimen")+
  xlim(2020,2050)+
  ylab("50q30")+
  xlab("Year")+
  ylim(0,0.7)+
  theme_bw()+
  scale_linetype_manual(values=c("solid", "twodash"))+
  theme(axis.text.x = element_text(angle=45))+
  scale_color_viridis(discrete = T) 

ggsave("plot_50q30.jpeg", height=6, width=12)
write.csv(WB_50q30%>%filter(intervention!="Baseline"), "data_50q30.csv", row.names = F)


rm(data.in)
rm(output)
rm(output2)
rm(comb)
rm(add)

#PP adverse effects
adv_pp<-left_join(pop, inc)%>%
  mutate(no_cvd = pop-cvd)%>%
  filter(year>=2019)%>%
  mutate(dizzy = no_cvd*cov*0.025,
         gi_bleed = no_cvd*cov*0.002)%>%
  group_by(intervention, regimen)%>%
  summarise(dizzy = sum(dizzy),
            gi_bleed = sum(gi_bleed))

write.csv(adv_pp%>%mutate(gi_bleed = ifelse(regimen=="No aspirin", 0, gi_bleed)), "adverse_effects.csv", row.names = F)

#combine plots

plot<-read.csv("outputs/plot_data/Fig2_data_aspririn_new_new.csv", stringsAsFactors = F)%>%
  mutate(Scenario = "FDC with aspirin")
plot2<-read.csv("outputs/plot_data/Fig2_data_new_new.csv", stringsAsFactors = F)%>%
  mutate(Scenario = "FDC without aspirin")

fig1<-bind_rows(plot, plot2)%>%
  mutate(metric = ifelse(metric=="Cumulative cases averted", "Cumulative events averted", metric),
         Intervention = factor(Intervention, levels=c("Scenario 4", "Scenario 3", "Scenario 2", "Scenario 1")))

library(viridis)
ggplot(fig1%>%filter(metric %in% c("Cumulative events averted", "Cumulative deaths averted"), Intervention!="Scenario 5"), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.7 , size=.5, colour="white") +
  facet_grid(metric~Scenario, scales = "free_y")+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative CVD events/deaths averted (millions)")+
  scale_fill_viridis(discrete = T) 

#ggsave("outputs/new_Figure1.jpeg", height=6, width=8)

##

ggplot(fig1%>%filter(metric %in% c("Cumulative events averted", "Cumulative deaths averted"), Intervention!="Scenario 5"), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.7 , size=.5, colour="white") +
  facet_grid(metric~Scenario)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative CVD events/deaths averted (millions)")+
  scale_fill_viridis(discrete = T) 

ggsave("outputs/new_Figure1_same_scale.jpeg", height=6, width=8)

ggplot(fig1%>%filter(metric %in% c("Cumulative events averted", "Cumulative deaths averted")), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.7 , size=.5, colour="white") +
  facet_grid(metric~Scenario)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative CVD events/deaths averted (millions)")+
  scale_fill_viridis(discrete = T) 

#ggsave("outputs/new_Figue1_same_scale_appendix.jpeg", height=6, width=8)
##

ggplot(fig1%>%filter(metric %in% c("MI averted", "Stroke averted", "Heart failure averted")), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.7 , size=.5, colour="white") +
  facet_grid(metric~Scenario)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative CVD events averted (millions)")+
  scale_fill_viridis(discrete = T) 

#ggsave("outputs/new_Figue1_events_appendix.jpeg", height=6, width=8)

ggplot(fig1%>%filter(metric %in% c("MI averted", "Stroke averted", "Heart failure averted"), Intervention!="Scenario 5"), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.7 , size=.5, colour="white") +
  facet_grid(metric~Scenario)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative CVD events averted (millions)")+
  scale_fill_viridis(discrete = T) 

ggsave("outputs/new_Figue1_events.jpeg", height=6, width=8)

##

ggplot(fig1%>%filter(metric %in% c("Cumulative deaths averted")), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.7 , size=.5, colour="white") +
  facet_wrap(~Scenario)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative deaths averted (millions)")+
  scale_fill_viridis(discrete = T) 

#ggsave("outputs/new_Figue1_deaths_appendix.jpeg", height=4, width=8)

ggplot(fig1%>%filter(metric %in% c("Cumulative deaths averted"), Intervention!="Scenario 5"), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.7 , size=.5, colour="white") +
  facet_wrap(~Scenario)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative deaths averted (millions)")+
  scale_fill_viridis(discrete = T) 

#ggsave("outputs/new_Figue1_deaths.jpeg", height=4, width=8)

###

plot<-read.csv("outputs/plot_data/Fig1_data_new_new.csv", stringsAsFactors = F)%>%
  mutate(Scenario = "FDC without aspirin")
plot2<-read.csv("outputs/plot_data/Fig1_data_aspririn_new_new.csv", stringsAsFactors = F)%>%
  mutate(Scenario = "FDC with aspirin")

fig2<-bind_rows(plot, plot2)
fig2$wb2021<-factor(fig2$wb2021, levels=c("LIC", "LMIC", "UMIC", "HIC"))
fig2<-fig2%>%mutate(intervention = ifelse(intervention=="Business as usual" ,"Current care", intervention))

ggplot(fig2%>%filter(intervention!="Alt Scenario 1", intervention!="Scenario 5"), 
       aes(x=year, y=x50q30, color=wb2021, linetype=Scenario))+
  #geom_line()+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  facet_wrap(~intervention, nrow=1)+
  labs(color="Country Income Group")+
  xlim(2020,2050)+
  ylim(0,0.33)+
  ylab("50q30")+
  xlab("Year")+
  theme_bw()+
  scale_linetype_manual(values=c("twodash", "solid"))+
  theme(axis.text.x = element_text(angle=45))+
  scale_color_viridis(discrete = T) 

ggsave("outputs/new_Figure2.jpeg", height=5, width=9)


## numbers for paper

diffq30<-read.csv("outputs/plot_data/appendix_50q30_new_new.csv", stringsAsFactors = F)%>%
  select(-X)%>%mutate(Scenario = "FDC without aspirin")%>%
  bind_rows(., read.csv("outputs/plot_data/appendix_50q30_aspirin_new_new.csv", stringsAsFactors = F)%>%
              select(-X)%>%mutate(Scenario = "FDC with aspirin"))%>%
  mutate(intervention = ifelse(intervention=="Business as usual", "Current care", intervention),
         wb2021 = "World")%>%
  bind_rows(., fig2)%>%filter(year %in% c(2020,2050), intervention %in% c("Current care", "Scenario 4"))%>%
  spread(intervention, x50q30)%>%
  mutate(diff = (`Current care` - `Scenario 4`)/`Current care`)

write.csv(diffq30, "outputs/x50q30_new_new.csv", row.names = F)

#

plot3<-read.csv("outputs/plot_data/appendix_50q30_new_new.csv", stringsAsFactors = F)%>%
  select(-X)%>%mutate(FDC = "Without aspirin")%>%
  bind_rows(., read.csv("outputs/plot_data/appendix_50q30_aspirin_new_new.csv", stringsAsFactors = F)%>%
              select(-X)%>%mutate(FDC = "With aspirin"))%>%
  mutate(intervention = factor(intervention, levels = c("Business as usual", "Alt Scenario 1", "Scenario 4", "Scenario 5")))


ggplot(plot3, aes(x=year, y=x50q30, color=intervention, linetype=FDC))+
  #geom_line()+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  labs(color="Intervention Scenario")+
  xlim(2020,2050)+
  ylim(0,0.33)+
  ylab("50q30")+
  xlab("Year")+
  theme_bw()+
  scale_linetype_manual(values=c("twodash", "solid"))+
  theme(axis.text.x = element_text(angle=45))+
  scale_color_viridis(discrete = T) 


#ggsave("outputs/appendix_50q30fig.jpeg", height=4, width=6)


## new bubble figure ???
load("output2.Rda") #output
load("output_aspirin2.Rda") #output2

plot_new<-output%>%filter(year ==2020 | year==2050)%>%
  left_join(., locs%>%select(location, wbregion))%>%
  group_by(year, wbregion, intervention)%>%
  summarise(dead = sum(dead))%>%
  mutate(year = ifelse(year==2020, "base", "int"))%>%
  spread(year, dead)%>%
  mutate(change = 100*(int-base)/base)

plot_new2<-output2%>%filter(year ==2020 | year==2050)%>%
  left_join(., locs%>%select(location, wbregion))%>%
  group_by(year, wbregion, intervention)%>%
  summarise(dead = sum(dead))%>%
  mutate(year = ifelse(year==2020, "base", "int"))%>%
  spread(year, dead)%>%
  mutate(change = 100*(int-base)/base)

plot_new<-bind_rows(plot_new, plot_new2%>%filter(intervention=="Baseline"))%>%
  mutate(wbregion = factor(wbregion, levels = c("Europe and Central Asia", "North America",
                                                "Latin America and the Caribbean", "South Asia",
                                                "East Asia and Pacific", "Middle East and North Africa",
                                                "Sub-Saharan Africa")))
  

ggplot(plot_new, aes(x=wbregion, y=change, color=intervention))+
  geom_point()+
  ylab("Percent change in CVD deaths (%)")+
  xlab("")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(color = "Intervention")+
  theme_bw()

ggsave("bubble_plot.jpeg", height=6, width=9)


plot_new2<-plot_new2%>%mutate(wbregion = factor(wbregion, levels = c("Europe and Central Asia", "North America",
                                                                     "Latin America and the Caribbean", "South Asia",
                                                                     "East Asia and Pacific", "Middle East and North Africa",
                                                                     "Sub-Saharan Africa")))


ggplot(plot_new2, aes(x=wbregion, y=change, color=intervention))+
  geom_point()+
  ylab("Percent change in CVD deaths (%)")+
  xlab("")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(color = "Intervention")+
  theme_bw()

ggsave("bubble_plot_aspirin.jpeg", height=6, width=9)
