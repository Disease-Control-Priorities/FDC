rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)

load("../../output_aspirin2_new.Rda") #output2
load("../../output2_new.Rda") #output

output<-output%>%filter(intervention!="Baseline")%>%
  bind_rows(.,output2%>%filter(intervention=="Baseline"))

pop<-output2%>%filter(year==2019, intervention=="Baseline")%>%
  group_by(location)%>%summarise(Nx = sum(pop, na.rm = T))

groups<-read.csv("../../data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_gbd)%>%
  rename(location = location_gbd)

inc_adjust<-read.csv("../../data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_wb, location_gbd)%>%
  rename(Country = location_gbd)%>%
  right_join(., read.csv("../../new_events.csv", stringsAsFactors = F))%>%
  group_by(wb2021)%>%
  summarise(MI_ratio = mean(MI_ratio),
            stroke_ratio = mean(stroke_ratio),
            HF_ratio = mean(HF_ratio))%>%
  na.omit()

inc_adjust<-inc_adjust%>%
  bind_rows(., inc_adjust%>%filter(wb2021 == "LMIC")%>%
              mutate(wb2021 = "LIC"))%>%
  left_join(., groups)

download1<-output%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF,
         age.group = ifelse(age<30, "20-29 years", 
                            ifelse(age>=30 & age<70, "30-69 years", "70+ years")))%>%
  group_by(intervention, year, age.group, sex, location)%>%
  summarise(MI = sum(MI),
            Stroke = sum(stroke),
            HF = sum(HF),
            Incidence = sum(newcases),
            Prevalent_CVD = sum(sick),
            CVD_deaths = sum(dead),
            All_deaths = sum(all.mx)/3)%>%
  filter(year>=2020)%>%
  mutate(intervention = ifelse(intervention=="Baseline", "Current care", intervention),
         Scenario = "FDC without aspirin")

download2<-output2%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF,
         age.group = ifelse(age<30, "20-29 years", 
                            ifelse(age>=30 & age<70, "30-69 years", "70+ years")))%>%
  group_by(intervention, year, age.group, sex, location)%>%
  summarise(MI = sum(MI),
            Stroke = sum(stroke),
            HF = sum(HF),
            Incidence = sum(newcases),
            Prevalent_CVD = sum(sick),
            CVD_deaths = sum(dead),
            All_deaths = sum(all.mx)/3)%>%
  filter(year>=2020)%>%
  mutate(intervention = ifelse(intervention=="Baseline", "Current care", intervention),
         Scenario = "FDC with aspirin")

rm(output)
rm(output2)

download<-bind_rows(download1, download2)%>%filter(intervention!="Alt Scenario 1", intervention!="Scenario 5")
unique(download$intervention)
#reorder columns
download <- download[, c(13, 1,2,3,4,5,6,7,8,9,10,11,12)]
write.csv(download, "for_download.csv", row.names = F)

rm(download1)
rm(download2)

download<-read.csv("for_download.csv", stringsAsFactors = F)

###################################
## Figure 1 ##
###################################

base<-download%>%filter(intervention=="Current care", Scenario=="FDC with aspirin")%>%
  group_by(year, location)%>%
  summarise("Cumulative events averted" = sum(Incidence),
            "Cumulative deaths averted" = sum(CVD_deaths))%>%
  gather(metric, base, -year, -location)

plot<-download%>%filter(intervention!="Current care")%>%
  group_by(year, intervention, Scenario, location)%>%
  summarise("Cumulative events averted" = sum(Incidence),
            "Cumulative deaths averted" = sum(CVD_deaths))%>%
  gather(metric, val, -year, -location, -intervention, -Scenario)%>%
  left_join(., base)%>%
  mutate(diff = base-val)%>%
  arrange(year)%>%
  group_by(location, intervention, Scenario, metric)%>%
  mutate(diff2 = cumsum(diff))%>%
  select(-diff, -base, -val)%>%
  spread(intervention, diff2)%>%
  group_by(year)%>%
  mutate(`Scenario 2` = `Scenario 2` - `Scenario 1`,
         `Scenario 3` = `Scenario 3` - `Scenario 2` - `Scenario 1`,
         `Scenario 4` = `Scenario 4` - `Scenario 3` - `Scenario 2` - `Scenario 1`)%>%
  gather(Intervention, value, -year, -metric, -location, -Scenario)%>%
  mutate(Intervention = factor(Intervention, levels=c("Scenario 4", "Scenario 3", "Scenario 2", "Scenario 1")))%>%
  arrange(desc(Intervention))

library(viridis)
all<-plot%>%
  group_by(year, metric, Scenario, Intervention)%>%
  summarise(value = sum(value))

ggplot(all, 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.6 , size=.5, colour="white") +
  facet_grid(metric~Scenario)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative cases/deaths averted (millions)")+
  scale_fill_viridis(discrete = T)   

plot1<-bind_rows(plot, all%>%mutate(location="All locations"))

write.csv(plot1, "plot_data.csv", row.names = F)

###################################
##Table 2 ##
###################################

fix<-download%>%
  filter(year==2050)%>%
  group_by(location, year, intervention, Scenario)%>%
  summarise(deaths = sum(CVD_deaths))%>%
  spread(intervention, deaths)%>%
  mutate(`Scenario 1` = ifelse(`Scenario 1`>`Current care`, `Current care`, `Scenario 1`),
         `Scenario 2` = ifelse(`Scenario 2`>`Current care`, `Current care`, `Scenario 2`),
         `Scenario 3` = ifelse(`Scenario 3`>`Current care`, `Current care`, `Scenario 3`),
         `Scenario 4` = ifelse(`Scenario 4`>`Current care`, `Current care`, `Scenario 4`))%>%
  gather(intervention, deaths, -Scenario, -year, -location)%>%
  group_by(intervention, Scenario)%>%
  summarise(deaths = sum(deaths)/1e6)
                               

table<-download%>%
  filter(year==2050 & intervention!="Current care")%>%
  bind_rows(., download%>%filter(year %in% c(2020,2050)
                                 & intervention=="Current care" 
                                 & Scenario == "FDC with aspirin"))%>%
  group_by(location, year, intervention, Scenario)%>%
  summarise(deaths = sum(CVD_deaths),
            events = sum(Incidence),
            prev = sum(Prevalent_CVD))



all<-table%>%
  group_by(year, intervention, Scenario)%>%
  summarise("CVD deaths" = sum(deaths),
            "New CVD events"  = sum(events),
            "Prevalent CVD cases" = sum(prev))%>%
  rename(Intervention = intervention,
         Year = year)
  
base_deaths<-as.numeric(all[2,4])
base_inc<-as.numeric(all[2,5])
base_prev<-as.numeric(all[2,6])

all<-all%>%
  mutate("Deaths averted" = base_deaths- `CVD deaths`,
         "CVD events averted" = base_inc - `New CVD events`,
         "Change in CVD prevalence" = 100*(`Prevalent CVD cases`-base_prev)/base_prev,
         `CVD deaths` = `CVD deaths`,
         `New CVD events` = `New CVD events`,
         `Prevalent CVD cases` = `Prevalent CVD cases`,
         Intervention = ifelse(Intervention!="Current care",
                               paste0(Intervention,", ", Scenario),
                               Intervention)
         )%>%
  select(-Scenario)%>%
  rename(Scenario = Intervention)

#reorder and clean up
all<-all[,c(1,2,4,7,5,8,3,6)]
all<-all[c(1,2,4,3,6,5,8,7,10,9),]

#significant digit formatter function
so_formatter <- function(x) {
  dplyr::case_when(
    x < 1e3 ~ as.character(x),
    x < 1e6 ~ paste0(as.character(x/1e3), " thousand"),
    x < 1e9 ~ paste0(as.character(x/1e6), " million"),
  )
} #end of function

all$`New CVD events`<-so_formatter(signif(all$`New CVD events`, digits=2))
all$`CVD events averted`<-so_formatter(signif(all$`CVD events averted`, digits=2))
all$`Prevalent CVD cases`<-so_formatter(signif(all$`Prevalent CVD cases`, digits=2))
all$`Change in CVD prevalence`<-paste0(signif(all$`Change in CVD prevalence`, digits=2), "%")
all$`CVD deaths`<-so_formatter(signif(all$`CVD deaths`, digits=2))
all$`Deaths averted`<-so_formatter(signif(all$`Deaths averted`, digits=2))

all[1:2,c(4,6,8)]<-NA

#fin

check<-download%>%
  group_by(location, Scenario, intervention)%>%
  summarize(dead=sum(CVD_deaths))%>%
  spread(intervention, dead)%>%
  mutate(`Scenario 1` = `Current care` - `Scenario 1`,
         `Scenario 2` = `Current care` - `Scenario 2`,
         `Scenario 3` = `Current care` - `Scenario 3`,
         `Scenario 4` = `Current care` - `Scenario 4`)%>%
  select(-`Current care`)%>%
  gather(intervention, death_averted, -location, -Scenario)%>%
  mutate(death_averted = ifelse(death_averted<0,0,death_averted))%>%
  group_by(intervention, Scenario)%>%
  summarise(DA = sum(death_averted)/1e6)


