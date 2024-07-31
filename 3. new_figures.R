rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer, countrycode, stringr)   

locs<-read.csv("coverage_data/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(iso3, location_gbd, wbregion, wb2021)%>%
  rename(location = location_gbd)

load("output3.Rda")

rm(reg)

groups<-read.csv("data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_gbd)%>%
  rename(location = location_gbd)


####################################
#Figure 1
#################################

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

unique(output$intervention)

plot1<-output%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  left_join(., locs)%>%
  group_by(wbregion, year, intervention)%>%
  summarise(MI = sum(MI),
            HF = sum(HF),
            Stroke = sum(stroke))%>%
  filter(intervention %in% c("Current care", "Base case, targeted", "Base case, population-wide"))%>%
  gather(measure, val, -intervention, -year, -wbregion)%>%
  spread(intervention, val)%>%
  mutate(target = `Current care` - `Base case, targeted`,
         pop = `Current care` - `Base case, population-wide`)%>%
  arrange(year)%>%
  group_by(wbregion, measure)%>%
  mutate(Targeted = cumsum(target),
         Population = cumsum(pop),
         Population = Population - Targeted)%>%
  select(-target,-pop, -`Current care`, -`Base case, targeted`, -`Base case, population-wide`)%>%
  gather(scenario, val, -year, -wbregion, -measure)%>%
  mutate(Measure = paste0(measure, " (", scenario, ")"))%>%
  filter(year %in% c(2040,2045,2050))

#format text
plot1b<-plot1%>%
  mutate(Measure = factor(Measure, levels=c("MI (Population)",
                                            "MI (Targeted)",
                                            "Stroke (Population)",
                                            "Stroke (Targeted)",
                                            "HF (Population)",
                                            "HF (Targeted)")))


colors<-c( "#e9d8a6","#ee9b00","#94d2bd", "#0a9396",  "#a69cac",  "#474973")


plot1b$newx = str_wrap(plot1b$wbregion, width = 20)


#cumulative cases averted bar chart..?
ggplot(plot1b, aes(x=year, y=val/1e6, fill=Measure))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(~newx, nrow = 1)+
  ylab("Cumulative cases averted (millions)")+
  xlab("Year")+
  theme_bw()+
  scale_fill_manual(values=colors)

ggsave("outputs/Figure1.jpeg", height=6, width=12)


# Data for r shiny #
write.csv(plot1b, "shiny/FDC/plot1_base.csv", row.names = F)



####################################
#Figure 2
#################################

plot2<-left_join(output, locs)%>%
  filter(cause!="hhd")

#pull data for year 2019
output2<-plot2%>%
  filter(year==2019 | year==2050)%>%
  group_by(intervention, year, wbregion)%>%
  summarise(dead = sum(dead))%>%
  spread(year, dead)%>%
  mutate(change = 100*((`2050`-`2019`)/`2019`))%>%
  rename("deaths2050" = `2050`, "deaths2019" = `2019`)

##Scenario 1: use 2050 total pop w/ 2019 pop age and death rates
pop2050<-plot2%>%filter(cause=="ihd", year==2050)%>%
  group_by(intervention, wbregion)%>%summarise(totalpop2050=sum(pop))

pop2019<-plot2%>%filter(cause=="ihd", year==2019)%>%
  group_by(intervention, wbregion)%>%summarise(totalpop2019=sum(pop))

deathrt2019<-plot2%>%filter(year==2019)%>%
  group_by(wbregion, intervention, age)%>%
  summarise(dead2019 = sum(dead),
            pop2019 = sum(pop)/3)

growth<-left_join(deathrt2019, pop2019)%>%
  mutate(age_prop = pop2019/totalpop2019)%>%
  left_join(., pop2050)%>%
  mutate(newpop = age_prop * totalpop2050,
         deathsexpgrowth = newpop * (dead2019/pop2019))%>%
  group_by(intervention, wbregion)%>%
  summarise(growthdeaths=sum(deathsexpgrowth))

output2<-left_join(output2, growth)%>%
  mutate(change_growth = 100*((growthdeaths - deaths2019)/deaths2019))

##Scenario 2: use 2050 pop and age structure with 2019 death rates

aging<-plot2%>%filter(year==2050)%>%
  group_by(intervention, wbregion, age)%>%
  summarise(pop2050 = sum(pop)/3)%>%
  left_join(., deathrt2019)%>%
  mutate(agedeaths = pop2050*(dead2019/pop2019))%>%
  group_by(intervention, wbregion)%>%
  summarise(agedeaths = sum(agedeaths))

output2<-left_join(output2, aging)%>%
  mutate(change_age = 100*((agedeaths-deaths2019)/deaths2019)-change_growth,
         change_epi = change - change_age - change_growth)

plot2<-output2%>%filter(intervention %in% c("Current care", "Base case, targeted",
                                            "Base case, population-wide"))%>%
  mutate(intervention = ifelse(intervention=="Base case, targeted", "Targeted", intervention),
         intervention = ifelse(intervention=="Base case, population-wide", "Population", intervention),
         Intervention = factor(intervention, levels=c("Current care", "Targeted", "Population")))%>%
  ungroup()%>%
  select(Intervention, wbregion, change_growth, change_age, change_epi, netchange=change)%>%
  gather(metric, change, -Intervention, -wbregion, -netchange)%>%
  mutate(Metric = ifelse(metric == "change_age", "Change due to population aging", metric),
         Metric = ifelse(metric == "change_epi", "Change due to age-specific CVD mortality rates", Metric),
         Metric = ifelse(metric == "change_growth", "Change due to population growth", Metric),
         Metric = factor(Metric, levels= c("Change due to population aging", 
                                           "Change due to population growth", 
                                           "Change due to age-specific CVD mortality rates")))

library(ggthemes)

ggplot(plot2, aes(x=Intervention))+
  geom_bar(stat='identity', aes(y=change, fill=Metric), position="stack")+
  geom_point(aes(y=netchange, shape = "Net change"))+
  facet_grid(~wbregion, labeller = labeller(wbregion= label_wrap_gen(width=16)))+
  theme_calc()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="bottom", legend.box = "horizontal", legend.direction = "horizontal")+
  scale_fill_manual(values = c("#56B4E9","#E69F00","#009E73"))+
  xlab("")+
  ylab("Change in CVD Deaths 2019 to 2050 (%)")+
  labs(fill="", shape=NULL)
  #ylim(c(-175,300))


ggsave("outputs/Figure2.jpeg", width=11, height=6, dpi=1200)

#ggsave("../output/fig_A1_updated.pdf", width=11, height=6, dpi=600)
#ggsave(p, "../output/fig_A1_updated.eps", device='eps', width=11, height=6)
#tiff("../output/fig_A1_updated.tiff")
#ggsave( "../output/fig_A1_updated.tiff", device='tiff', width=11, height=6)
#dev.off()

############
# Figure 3
#############

library(readxl)

isos<-unique(locs$iso3)

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
  merge(., data.frame(intervention = unique(output$intervention)))

any(is.na(add))

comb<-output%>%
  left_join(., locs%>%select(location, wbregion, iso3))

CVD<-comb%>%filter(cause=="ihd")%>% #doesn't matter which cause
  group_by(age, iso3, year, intervention)%>%
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

WB_50q30<-CVD%>%group_by(age.group,  iso3, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))

WB_50q30$mx<-WB_50q30$dead/WB_50q30$pop
any(is.na(WB_50q30))

WB_50q30<-WB_50q30%>%group_by(iso3, year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

base19<-WB_50q30%>%filter(intervention=="Current care", year==2019)%>%
  ungroup()%>%
  select(iso3, base19 = x50q30)

base<-WB_50q30%>%filter(intervention=="Current care", year==2050)%>%
  select(iso3, base = x50q30)%>%
  left_join(., base19)


target<-WB_50q30%>%filter(intervention=="Base case, targeted", year==2050)%>%
  left_join(., base)%>%
  mutate(base_diff = 1-(base/base19),
         int_dff = 1-(x50q30/base19),
         reduction = (int_dff/base_diff)-1)%>%
  mutate(redux = ifelse(reduction<0.1, "Less than 10%", NA),
         redux = ifelse(reduction>=0.1 & reduction<=0.15, "10-15%", redux),
         redux = ifelse(reduction>0.15 & reduction<=0.2, "16-20%", redux),
         redux = ifelse(reduction>0.2 & reduction<=0.25, "21-25%", redux),
         redux = ifelse(reduction>0.25, "Greater than 25%", redux))%>%
  mutate(redux = factor(redux, levels=c("Less than 10%", "10-15%", "16-20%",
                                        "21-25%", "Greater than 25%")))
population<-WB_50q30%>%filter(intervention=="Base case, population-wide", year==2050)%>%
  left_join(., base)%>%
  mutate(base_diff = 1-(base/base19),
         int_dff = 1-(x50q30/base19),
         reduction = (int_dff/base_diff)-1)%>%
  mutate(redux = ifelse(reduction<0.1, "Less than 10%", NA),
         redux = ifelse(reduction>=0.1 & reduction<=0.15, "10-15%", redux),
         redux = ifelse(reduction>0.15 & reduction<=0.2, "16-20%", redux),
         redux = ifelse(reduction>0.2 & reduction<=0.25, "21-25%", redux),
         redux = ifelse(reduction>0.25, "Greater than 25%", redux))%>%
  mutate(redux = factor(redux, levels=c("Less than 10%", "10-15%", "16-20%",
                                        "21-25%", "Greater than 25%")))


#assume same results for south sudan as sudan
target<-bind_rows(target, target%>%filter(iso3=="SDN")%>%mutate(iso3="SSD"))
population<-bind_rows(population, population%>%filter(iso3=="SDN")%>%mutate(iso3="SSD"))

# map 1  
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
world<-left_join(world, target%>%rename(iso_a3 = iso3))

e<-ggplot(data = world) +
  geom_sf(aes(fill = redux)) +
  theme_bw()+
  scale_fill_manual(values = c("#2F635A","#b9d780", "#feea83",
                               "#faa175", "#f8696b","#9B2226"), 
                    name= "Relative reduction in 70q0 \ncompared to current care")+ 
  theme(legend.position = "right")

e

ggsave("outputs/Figure3_target.jpeg", width=11, height=6, dpi=1200)


# map 2

world2 <- ne_countries(scale = "medium", returnclass = "sf")
world2<-left_join(world2, population%>%rename(iso_a3 = iso3))

f<-ggplot(data = world2) +
  geom_sf(aes(fill = redux)) +
  theme_bw()+
  scale_fill_manual(values = c("#2F635A","#b9d780", "#feea83",
                               "#faa175", "#f8696b","#9B2226"), 
                    name= "Relative reduction in 70q0 \ncompared to current care")+ 
  theme(legend.position = "right")

f

ggsave("outputs/Figure3_population.jpeg", width=11, height=6, dpi=1200)

