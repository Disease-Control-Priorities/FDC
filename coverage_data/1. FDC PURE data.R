
############ NEW COVERAGE THOUGHTS ###############

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dplyr, tidyr, ggplot2, boot, data.table, tidyverse)   

locs<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(gbd2019, iso3, wb2021, location_gbd, location_ncdrisc)%>%rename(location_name = gbd2019)

cascade2<-read.csv("../shiny/FDC/cascade.csv", stringsAsFactors = F)
cascade3<-read.csv("../shiny/FDC/cascade_aspirin.csv", stringsAsFactors = F)

#average rates of change over the last 10 years (2009-2019)
#assume rate applies to SP and PP

aroc<-read.csv("NCD-RisC_Lancet_2021_Hypertension_crude_countries.csv", stringsAsFactors = F)%>%
  rename(iso3 = ISO, year=Year)%>%
  left_join(., locs)%>%
  group_by(year,location_name, iso3)%>%
  summarise(Treated = mean(Proportion.of.treated.hypertension.among.all.hypertension),
            Aware = mean(Proportion.of.diagnosed.hypertension.among.all.hypertension),
            Control = mean(Proportion.of.controlled.hypertension.among.all.hypertension))%>%
  na.omit()%>%
  filter(year>=2009)%>%
  mutate(Adherence = Control/Treated,
         Initiation = Treated/Aware)%>%
  nest(-location_name, -iso3)%>%
  mutate(aware_aroc = map(data, ~coef(lm(Aware ~ year, data=.x))[["year"]]),
         adherence_aroc = map(data, ~coef(lm(Adherence ~ year, data=.x))[["year"]]),
         initiation_aroc = map(data, ~coef(lm(Initiation ~ year, data=.x))[["year"]]),
         treated_aroc = map(data, ~coef(lm(Treated ~ year, data=.x))[["year"]]),
         control_aroc = map(data, ~coef(lm(Control ~ year, data=.x))[["year"]]))%>%
  select(-data)%>%
  unnest()%>%
  mutate(aware_aroc = ifelse(aware_aroc<0, 0, aware_aroc),
         adherence_aroc = ifelse(adherence_aroc<0, 0, adherence_aroc),
         initiation_aroc = ifelse(initiation_aroc<0, 0, initiation_aroc),
         treated_aroc = ifelse(treated_aroc<0, 0 , treated_aroc),
         control_aroc = ifelse(control_aroc<0, 0 , control_aroc))

#warning ok
htn<-read.csv("NCD-RisC_Lancet_2021_Hypertension_crude_countries.csv", stringsAsFactors = F)%>%
  rename(iso3 = ISO, year=Year)%>%
  left_join(., locs)%>%
  group_by(year,location_name, iso3)%>%
  summarise(Treated = mean(Proportion.of.treated.hypertension.among.all.hypertension),
            Aware = mean(Proportion.of.diagnosed.hypertension.among.all.hypertension),
            Control = mean(Proportion.of.controlled.hypertension.among.all.hypertension))%>%
  na.omit()

# fitting a logarithmic curve: 
# https://www.graphpad.com/guides/prism/latest/curve-fitting/REG_Exponential-plateau.htm

baselinePP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*initiation_aroc + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence,((year-2022)*adherence_aroc + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

baselineSP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*initiation_aroc + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence,((year-2022)*adherence_aroc + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

#
baselinePP_asp<-cascade3%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*initiation_aroc + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence,((year-2022)*adherence_aroc + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

baselineSP_asp<-cascade3%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*initiation_aroc + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence,((year-2022)*adherence_aroc + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

plot<-bind_rows(htn%>%filter(year<2017), baselinePP%>%group_by(location_name, iso3, year)%>%
                  summarise(Aware = mean(Aware),
                            Treated = mean(Treated),
                            Control = mean(Control)))

ggplot(plot, aes(x=year, y=Aware, groups=location_name))+
  geom_line()+
  xlim(2009,2050)

ggplot(baselinePP, aes(x=year, y=Initiation, groups=location_name))+
  geom_line()+
  facet_wrap(~cause)

ggplot(plot, aes(x=year, y=Treated, groups=location_name))+
  geom_line()

ggplot(baselinePP, aes(x=year, y=Adherence, groups=location_name))+
  geom_line()+
  facet_wrap(~cause)

ggplot(plot, aes(x=year, y=Control, groups=location_name))+
  geom_line()


#Average increase in baseline control rates?
#need to do population weighting**
pop<-read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_5795797.csv", stringsAsFactors = F, skip=4)%>%
  select(iso3 = Country.Code, pop = X2019)

baselinePP%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))

baselinePP_asp%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))

baselineSP%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))


baselineSP_asp%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))

#construct intervention scenarios

## Scenario 1
scenario1_PP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*(initiation_aroc+0.01) + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence, ((year-2022)*(adherence_aroc+0.01) + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

scenario1_SP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*(initiation_aroc+0.01) + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence, ((year-2022)*(adherence_aroc+0.01) + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

#
scenario1_PP_asp<-cascade3%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*(initiation_aroc+0.01) + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence, ((year-2022)*(adherence_aroc+0.01) + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

scenario1_SP_asp<-cascade3%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*(initiation_aroc+0.01) + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence, ((year-2022)*(adherence_aroc+0.01) + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)


plot3<-bind_rows(baselinePP%>%mutate(scenario="baseline"),
                 scenario1_PP%>%mutate(scenario="scenario 1"))

ggplot(plot3, aes(x=year, y=Control, groups=location_name))+
  geom_line()+
  facet_grid(cause~scenario)

#average rate of scale up
scenario1_PP%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))

scenario1_PP_asp%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))


## Scenario 2
scenario2_PP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*(initiation_aroc+0.01) + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence, ((year-2022)*(adherence_aroc+0.01) + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

scenario2_SP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*(initiation_aroc+0.01) + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence, ((year-2022)*(adherence_aroc+0.01) + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

#
scenario2_PP_asp<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*(initiation_aroc+0.01) + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence, ((year-2022)*(adherence_aroc+0.02) + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

scenario2_SP_asp<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*aware_aroc),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*(initiation_aroc+0.01) + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence, ((year-2022)*(adherence_aroc+0.02) + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)


plot3<-bind_rows(baselinePP%>%mutate(scenario="baseline"),
                 scenario2_PP%>%mutate(scenario="scenario 2"))

ggplot(plot3, aes(x=year, y=Control, groups=location_name))+
  geom_line()+
  facet_grid(cause~scenario)

#average rate of scale up
scenario2_SP%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))


## Scenario 3
scenario3_PP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*(aware_aroc+0.01)),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*(initiation_aroc+0.01) + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence, ((year-2022)*(adherence_aroc+0.02) + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

scenario3_SP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*(aware_aroc+0.01)),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*(initiation_aroc+0.01) + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence, ((year-2022)*(adherence_aroc+0.02) + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)


#
scenario3_PP_asp<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*(aware_aroc+0.01)),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*(initiation_aroc+0.01) + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence, ((year-2022)*(adherence_aroc+0.02) + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

scenario3_SP_asp<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))),
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*(aware_aroc+0.01)),
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*(initiation_aroc+0.01) + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence, ((year-2022)*(adherence_aroc+0.02) + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)



plot3<-bind_rows(baselinePP%>%mutate(scenario="baseline"),
                 scenario3_PP%>%mutate(scenario="scenario 3"))

ggplot(plot3, aes(x=year, y=Control, groups=location_name))+
  geom_line()+
  facet_grid(cause~scenario)

#average rate of scale up
scenario3_PP%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))


## Scenario 4
scenario4_PP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))), #log scale
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*(aware_aroc+0.02)), #linear scale
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*(initiation_aroc+0.02) + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence, ((year-2022)*(adherence_aroc+0.02) + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

scenario4_SP<-cascade2%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))), #log scale
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*(aware_aroc+0.02)), #linear scale
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*(initiation_aroc+0.02) + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence, ((year-2022)*(adherence_aroc+0.02) + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

#

scenario4_PP_asp<-cascade3%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, pp_aware, 0.95 - (0.95-pp_aware)*exp(-aware_aroc*(year-2022))), #log scale
         Aware = ifelse(year<2023, pp_aware, Aware + (year-2022)*(aware_aroc+0.02)), #linear scale
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, pp_initiation, ((year-2022)*(initiation_aroc+0.02) + pp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, pp_adherence, ((year-2022)*(adherence_aroc+0.02) + pp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)

scenario4_SP_asp<-cascade3%>%
  left_join(.,aroc)%>%
  merge(., data.frame(year=2017:2050))%>%
  mutate(initiation_aroc = ifelse(initiation_aroc>0.005, 0.005, initiation_aroc),
         adherence_aroc = ifelse(adherence_aroc>0.005, 0.005, adherence_aroc))%>%
  mutate(Aware = ifelse(year<2023, sp_aware, 0.95 - (0.95-sp_aware)*exp(-aware_aroc*(year-2022))), #log scale
         Aware = ifelse(year<2023, sp_aware, Aware + (year-2022)*(aware_aroc+0.02)), #linear scale
         Aware = ifelse(Aware>0.95, 0.95, Aware),
         Initiation = ifelse(year<2023, sp_initiation, ((year-2022)*(initiation_aroc+0.02) + sp_initiation)),
         Initiation = ifelse(Initiation>0.95 ,0.95, Initiation),
         Treated = Aware * Initiation,
         Adherence = ifelse(year<2023, sp_adherence, ((year-2022)*(adherence_aroc+0.02) + sp_adherence)),
         Adherence = ifelse(Adherence>0.95 ,0.95, Adherence),
         Control = Treated*Adherence)%>%
  select(location_name, iso3, cause, year, Aware, Treated, Control, Initiation, Adherence)


plot3<-bind_rows(baselinePP%>%mutate(scenario="baseline"),
                 scenario4_PP%>%mutate(scenario="scenario 4"))

ggplot(plot3, aes(x=year, y=Control, groups=location_name))+
  geom_line()+
  facet_grid(cause~scenario)

#average rate of scale up
scenario4_SP%>%filter(year %in% c(2022,2050))%>%
  select(year, iso3, Control, cause)%>%
  spread(year, Control)%>%
  left_join(., pop)%>%
  mutate(slope = (`2050` - `2022`)/(2050-2022))%>%
  na.omit()%>%
  summarise(rate = weighted.mean(slope, pop))


#replicate original plot:

plot<-bind_rows(baselinePP%>%mutate(scenario="Baseline"),
                scenario4_PP%>%mutate(scenario="Scenario 4"),
                scenario1_PP%>%mutate(scenario="Scenario 1"),
                scenario2_PP%>%mutate(scenario="Scenario 2"),
                scenario3_PP%>%mutate(scenario="Scenario 3"))%>%
  group_by(location_name, year, scenario)%>%
  summarise(Control = mean(Control),
            Treated = mean(Treated),
            Aware = mean(Aware))

ggplot(plot, aes(x=year, y=Control, groups=location_name))+
  geom_line()+
  facet_wrap(~scenario, nrow=1)+
  ylim(0,1)+
  xlab("Year")+
  ylab("Primary prevention effective coverage")

#ggsave("../outputs/new_coverage.jpeg", width=10, height=4)

ggplot(plot, aes(x=year, y=Treated, groups=location_name))+
  geom_line()+
  facet_wrap(~scenario, nrow=1)+
  ylim(0,1)+
  xlab("Year")+
  ylab("Primary prevention treatment coverage")


#ggsave("../outputs/new_coverage_treated.jpeg", width=10, height=4)

ggplot(plot, aes(x=year, y=Aware, groups=location_name))+
  geom_line()+
  facet_wrap(~scenario, nrow=1)+
  ylim(0.2,1)+
  xlab("Year")+
  ylab("Primary prevention awareness")


#ggsave("../outputs/new_coverage_aware.jpeg", width=10, height=4)

#cascade line plot:
plot2<-plot%>%group_by(year,scenario)%>%
  summarise(Control = mean(Control),
            Treated = mean(Treated),
            Aware = mean(Aware))%>%
  gather(Metric, val, -year, -scenario)%>%
  mutate(Metric = ifelse(Metric=="Treated", "On treatment", Metric),
         Metric = ifelse(Metric=="Control", "Optimized", Metric),
         scenario = ifelse(scenario=="Baseline", "Current care", scenario))%>%
  mutate(Metric = factor(Metric, levels=c("Aware", "On treatment", "Optimized")),
         scenario = factor(scenario, levels=c("Scenario 4", "Scenario 3", "Scenario 2", "Scenario 1", "Current care")))


ggplot(plot2, aes(x=year, y=val, color=scenario))+
  geom_line(size=1)+
  facet_wrap(~Metric)+
  ylab("Proportion of all primary prevention patients")+
  xlab("Year")+
  ylim(0,1)+
  labs(color = "Scenario")+
  theme_bw()

#ggsave("../outputs/cascade.jpeg", height=4, width=8)
write.csv(plot, "../cascade_data_trt.csv", row.names = F)
write.csv(plot, "../shiny/FDC/cascade_data_trt.csv", row.names = F)


plot<-bind_rows(baselinePP%>%mutate(scenario="Baseline"),
                scenario4_PP%>%mutate(scenario="Scenario 4"),
                scenario1_PP%>%mutate(scenario="Scenario 1"),
                scenario2_PP%>%mutate(scenario="Scenario 2"),
                scenario3_PP%>%mutate(scenario="Scenario 3"))%>%
  group_by(location_name, year, scenario)%>%
  summarise(Control = mean(Control),
            Treated = mean(Treated),
            Aware = mean(Aware))%>%
  mutate(group = "no aspirin, pp")%>%
  bind_rows(.,
            bind_rows(baselinePP_asp%>%mutate(scenario="Baseline"),
                      scenario4_PP_asp%>%mutate(scenario="Scenario 4"),
                      scenario1_PP_asp%>%mutate(scenario="Scenario 1"),
                      scenario2_PP_asp%>%mutate(scenario="Scenario 2"),
                      scenario3_PP_asp%>%mutate(scenario="Scenario 3"))%>%
              group_by(location_name, year, scenario)%>%
              summarise(Control = mean(Control),
                        Treated = mean(Treated),
                        Aware = mean(Aware))%>%
              mutate(group = "with aspirin, pp")
            )%>%
  bind_rows(.,
            bind_rows(baselineSP_asp%>%mutate(scenario="Baseline"),
                      scenario4_SP_asp%>%mutate(scenario="Scenario 4"),
                      scenario1_SP_asp%>%mutate(scenario="Scenario 1"),
                      scenario2_SP_asp%>%mutate(scenario="Scenario 2"),
                      scenario3_SP_asp%>%mutate(scenario="Scenario 3"))%>%
              group_by(location_name, year, scenario)%>%
              summarise(Control = mean(Control),
                        Treated = mean(Treated),
                        Aware = mean(Aware))%>%
              mutate(group = "with aspirin, sp")
  )%>%
  bind_rows(.,
            bind_rows(baselineSP_asp%>%mutate(scenario="Baseline"),
                      scenario4_SP_asp%>%mutate(scenario="Scenario 4"),
                      scenario1_SP_asp%>%mutate(scenario="Scenario 1"),
                      scenario2_SP_asp%>%mutate(scenario="Scenario 2"),
                      scenario3_SP_asp%>%mutate(scenario="Scenario 3"))%>%
              group_by(location_name, year, scenario)%>%
              summarise(Control = mean(Control),
                        Treated = mean(Treated),
                        Aware = mean(Aware))%>%
              mutate(group = "with aspirin, sp")
  )

write.csv(plot, "../cascade_data_all.csv", row.names = F)

#SP
plot<-bind_rows(baselineSP%>%mutate(scenario="Baseline"),
                scenario4_SP%>%mutate(scenario="Scenario 4"),
                scenario1_SP%>%mutate(scenario="Scenario 1"),
                scenario2_SP%>%mutate(scenario="Scenario 2"),
                scenario3_SP%>%mutate(scenario="Scenario 3"))%>%
  group_by(location_name, year, scenario)%>%
  summarise(Control = mean(Control),
            Treated = mean(Treated),
            Aware = mean(Aware))

ggplot(plot, aes(x=year, y=Control, groups=location_name))+
  geom_line()+
  facet_wrap(~scenario, nrow=1)+
  ylim(0,1)+
  xlab("Year")+
  ylab("Secondary prevention effective coverage")

write.csv(plot, "../shiny/FDC/cascade_data_SP.csv", row.names = F)
#ggsave("../outputs/new_coverage_SP.jpeg", width=10, height=4)


#put it in scale-up format
sup<-read.csv("../scale-up.csv", stringsAsFactors = F)
sup_aspirin<-read.csv("../scale-up_withaspirin.csv", stringsAsFactors = F)

cov<-bind_rows(baselinePP%>%mutate(intervention="Baseline"),
              scenario4_PP%>%mutate(intervention="Scenario 4"),
              scenario1_PP%>%mutate(intervention="Scenario 1"),
              scenario2_PP%>%mutate(intervention="Scenario 2"),
              scenario3_PP%>%mutate(intervention="Scenario 3"))%>%
  select(location_name, iso3, cause, year, intervention, 
         pp_cov = Control, pp_trt = Treated)%>%
  left_join(., bind_rows(baselineSP%>%mutate(intervention="Baseline"),
                         scenario4_SP%>%mutate(intervention="Scenario 4"),
                         scenario1_SP%>%mutate(intervention="Scenario 1"),
                         scenario2_SP%>%mutate(intervention="Scenario 2"),
                         scenario3_SP%>%mutate(intervention="Scenario 3"))%>%
              select(location_name, iso3, cause, year, intervention, 
                     sp_cov = Control, sp_trt = Treated)
  )%>%left_join(., sup%>%select(-pp.cov.inc, -sp.cov.inc, -pp_cov, -sp_cov))%>%
  group_by(intervention, location_name, cause)%>%
  arrange(year, .by_group=TRUE)%>%
  mutate(pp.cov.inc = pp_cov - shift(pp_cov),
         pp.cov.inc = ifelse(is.na(pp.cov.inc), 0, pp.cov.inc),
         pp.cov.inc = cumsum(pp.cov.inc),
         sp.cov.inc = sp_cov - shift(sp_cov),
         sp.cov.inc = ifelse(is.na(sp.cov.inc), 0, sp.cov.inc),
         sp.cov.inc = cumsum(sp.cov.inc),
         pp.trt.inc = pp_trt - shift(pp_trt),
         pp.trt.inc = ifelse(is.na(pp.trt.inc), 0, pp.trt.inc),
         pp.trt.inc = cumsum(pp.trt.inc),
         sp.trt.inc = sp_trt - shift(sp_trt),
         sp.trt.inc = ifelse(is.na(sp.trt.inc), 0, sp.trt.inc),
         sp.trt.inc = cumsum(sp.trt.inc))

write.csv(cov, "../scale-up-new.csv", row.names = F)

#

cov2<-bind_rows(baselinePP_asp%>%mutate(intervention="Baseline"),
               scenario4_PP_asp%>%mutate(intervention="Scenario 4"),
               scenario1_PP_asp%>%mutate(intervention="Scenario 1"),
               scenario2_PP_asp%>%mutate(intervention="Scenario 2"),
               scenario3_PP_asp%>%mutate(intervention="Scenario 3"))%>%
  select(location_name, iso3, cause, year, intervention, 
         pp_cov = Control, pp_trt = Treated)%>%
  left_join(., bind_rows(baselineSP_asp%>%mutate(intervention="Baseline"),
                         scenario4_SP_asp%>%mutate(intervention="Scenario 4"),
                         scenario1_SP_asp%>%mutate(intervention="Scenario 1"),
                         scenario2_SP_asp%>%mutate(intervention="Scenario 2"),
                         scenario3_SP_asp%>%mutate(intervention="Scenario 3"))%>%
              select(location_name, iso3, cause, year, intervention, 
                     sp_cov = Control, sp_trt = Treated)
  )%>%left_join(., sup_aspirin%>%select(-pp.cov.inc, -sp.cov.inc, -pp_cov, -sp_cov))%>%
  group_by(intervention, location_name, cause)%>%
  arrange(year, .by_group=TRUE)%>%
  mutate(pp.cov.inc = pp_cov - shift(pp_cov),
         pp.cov.inc = ifelse(is.na(pp.cov.inc), 0, pp.cov.inc),
         pp.cov.inc = cumsum(pp.cov.inc),
         sp.cov.inc = sp_cov - shift(sp_cov),
         sp.cov.inc = ifelse(is.na(sp.cov.inc), 0, sp.cov.inc),
         sp.cov.inc = cumsum(sp.cov.inc),
         pp.trt.inc = pp_trt - shift(pp_trt),
         pp.trt.inc = ifelse(is.na(pp.trt.inc), 0, pp.trt.inc),
         pp.trt.inc = cumsum(pp.trt.inc),
         sp.trt.inc = sp_trt - shift(sp_trt),
         sp.trt.inc = ifelse(is.na(sp.trt.inc), 0, sp.trt.inc),
         sp.trt.inc = cumsum(sp.trt.inc))

write.csv(cov2, "../scale-up-new-aspirin.csv", row.names = F)


