
#################################################################################################
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer)   

#################################################################################################
load("data_80_80_80/base_rates.Rda") #data too large for github

#update with wpp2021 estimates for incoming 20-year old cohorts
load("data_NCDC/wpp_age_20.Rda")

b_rates<-b_rates%>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))%>%
  left_join(., wpp_20%>%select(iso3, year, sex, Population))%>%
  mutate(Nx = ifelse(age==20, Population, Nx),
         pop = ifelse(age==20, Population, pop))%>%
  select(-Population, -iso3)

##coverage and effects
inc<-read.csv("scale-up-new.csv", stringsAsFactors = F)

any(is.na(inc))
unique(inc$cause)

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
#warnings ok

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

save.image(file = "output2_new.Rda")


######################################

load("output2.Rda")
load("output_aspirin2.Rda")

output<-bind_rows(output, output2%>%filter(intervention=="Baseline"))

pop<-output2%>%filter(year==2019, intervention=="Baseline")%>%
  group_by(location)%>%summarise(Nx = sum(pop, na.rm = T))
rm(output2)

#########################################################################
#Tables and figures
#########################################################################

inc_adjust<-read.csv("data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb_region, location_wb, location_gbd)%>%
  rename(location = location_gbd)%>%
  right_join(., read.csv("cvd_events2.csv", stringsAsFactors = F))%>%
  filter(CV.death!=0)%>%
  mutate(MI_ratio = MI/CV.death,
         stroke_ratio = Stroke/CV.death,
         HF_ratio = Heart.Failure/CV.death)%>%
  left_join(., pop)%>%
  na.omit()%>%
  group_by(wb_region)%>%
  summarise(MI_ratio = weighted.mean(MI_ratio, Nx),
            stroke_ratio = weighted.mean(stroke_ratio, Nx),
            HF_ratio = weighted.mean(HF_ratio, Nx))%>%
  right_join(., read.csv("data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
               select(wb_region, location_gbd)%>%
               rename(location = location_gbd))%>%
  select(-wb_region)

table1<-output%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(intervention,year)%>%
  summarise(Incidence =  sum(newcases),
            Prevalence = sum(sick),
            CVD_deaths=  sum(dead),
            All_deaths = sum(all.mx)/3)%>%
  filter(year==2020 & intervention=="Baseline" | year==2050)

write.csv(table1, "outputs/Table2_new.csv")

table1_b<-output%>%
  filter(cause!="hhd", year==2050)%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(intervention)%>%
  summarise(Incidence = sum(newcases),
            Prevalence = sum(sick),
            CVD_deaths = sum(dead))

write.csv(table1_b, "outputs/Table2_b_new.csv")

table2<-output%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(intervention)%>%
  summarise(Deaths.averted=sum(dead),
            Cases.averted=sum(newcases))%>%
  gather(metric, value, -intervention)%>%
  spread(intervention,value)%>%
  mutate(`Scenario 1` = signif((Baseline- `Scenario 1`), 2),
         `Scenario 2` = signif((Baseline- `Scenario 2`), 2),
         `Scenario 3` = signif((Baseline- `Scenario 3`), 2),
         `Scenario 4` = signif((Baseline- `Scenario 4`), 2),
         `Scenario 5` = signif((Baseline- `Scenario 5`), 2),
         `Alt Scenario 1` = signif((Baseline- `Alt Scenario 1`), 2))%>%
  select(-Baseline)

write.csv(table2, "outputs/cumulative_resuts_new.csv")

table3<-output%>%
  filter(cause!="hhd")%>%
  group_by(intervention)%>%
  summarise(All.cause.deaths = sum(all.mx)/3)

write.csv(table3, "outputs/allcauseMX_new.csv")

#############################
#Calculate 50q30
groups<-read.csv("data_80_80_80/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_gbd)%>%rename(location = location_gbd)

comb<-left_join(output, groups, by="location")
any(is.na(comb$wb2021))

CVD<-comb%>%filter(cause!="hhd")%>%
  group_by(age, sex, location, wb2021, year, intervention)%>%
  filter(age>=30 & age<80)%>%summarise(pop=sum(pop)/3, dead=sum(dead)) #divide pop by 3 to avoid over counting for each cause
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

#by region
WB_50q30<-CVD%>%group_by(age.group,  wb2021, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
WB_50q30$mx<-WB_50q30$dead/WB_50q30$pop
any(is.na(WB_50q30))

WB_50q30<-WB_50q30%>%group_by(wb2021, year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

WB_50q30$wb2021<-factor(WB_50q30$wb2021, levels=c("LIC", "LMIC", "UMIC", "HIC"))

library(viridis)

#all countries by scenario
all_50q30<-CVD%>%group_by(age.group, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
all_50q30$mx<-all_50q30$dead/all_50q30$pop
any(is.na(all_50q30))

all_50q30<-all_50q30%>%group_by(year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

all_50q30<-all_50q30%>%mutate(intervention = ifelse(intervention=="Baseline", "Business as usual", intervention))

all_50q30<-all_50q30%>%
  filter(intervention%in%c("Business as usual", "Alt Scenario 1", "Scenario 4", "Scenario 5"))%>%
  mutate(intervention = factor(intervention, levels = c("Business as usual", "Alt Scenario 1", "Scenario 4", "Scenario 5")))

ggplot(all_50q30, 
       aes(x=year, y=x50q30, color=intervention))+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  labs(color="Scenario")+
  xlim(2020,2050)+
  ylim(0,0.25)+
  ylab("50q30")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45))+
  scale_color_viridis(discrete = T) 

ggsave("outputs/alt_Figure1_appendix.jpeg", height=4, width=6)

write.csv(all_50q30, "outputs/plot_data/appendix_50q30.csv")


#WB_50q30<-read.csv("outputs/plot_data/Fig1_data.csv", stringsAsFactors = F)
WB_50q30<-WB_50q30%>%mutate(intervention = ifelse(intervention=="Baseline", "Business as usual", intervention))

ggplot(WB_50q30%>%filter(intervention!="Scenario 5", intervention!="Alt Scenario 1"), aes(x=year, y=x50q30, color=wb2021))+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  facet_wrap(~intervention, nrow=1)+
  labs(color="Country Income Group")+
  xlim(2020,2050)+
  ylim(0,0.33)+
  ylab("50q30")+
  xlab("Year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45))+
  scale_color_viridis(discrete = T) 

ggsave("outputs/Figure1_new.jpeg", height=4, width=9)

write.csv(WB_50q30, "outputs/plot_data/Fig1_data.csv", row.names=F)


##################figure 2
plot2<-output%>%filter(intervention!="Alt Scenario 1")%>%
  filter(cause!="hhd")%>%
  left_join(., inc_adjust)%>%
  mutate(MI = dead*MI_ratio,
         stroke = dead*stroke_ratio,
         HF = dead*HF_ratio,
         newcases = MI + stroke + HF)%>%
  group_by(year, intervention)%>%
  summarise(`Cumulative deaths averted`=sum(dead),
            `Cumulative cases averted` = sum(newcases),
            `MI averted` = sum(MI),
            `Stroke averted` = sum(stroke),
            `Heart failure averted` = sum(HF))%>%
  gather(metric, value, -intervention, -year)%>%
  spread(intervention, value)%>%
  mutate(`Scenario 1` = Baseline - `Scenario 1`,
         `Scenario 2` = Baseline - `Scenario 2`,
         `Scenario 3` = Baseline - `Scenario 3`,
         `Scenario 4` = Baseline - `Scenario 4`,
         `Scenario 5` = Baseline - `Scenario 5`)%>%
  select(-Baseline)%>%
  arrange(metric, year)%>%
  group_by(metric)%>%
  mutate(`Scenario 1` = cumsum(`Scenario 1`),
         `Scenario 2` = cumsum(`Scenario 2`),
         `Scenario 3` = cumsum(`Scenario 3`),
         `Scenario 4` = cumsum(`Scenario 4`),
         `Scenario 5` = cumsum(`Scenario 5`))%>%
  gather(Intervention, value, -year, -metric)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "black")

#re-order scenarios as factors
#subtract values so makes sense
#make nicer colors

plot<-plot2%>%
  spread(Intervention, value)%>%
  group_by(year)%>%
  mutate(`Scenario 2` = `Scenario 2` - `Scenario 1`,
         `Scenario 3` = `Scenario 3` - `Scenario 2` - `Scenario 1`,
         `Scenario 4` = `Scenario 4` - `Scenario 3` - `Scenario 2` - `Scenario 1`,
         `Scenario 5` = `Scenario 5` - `Scenario 4` - `Scenario 3` - `Scenario 2` - `Scenario 1`)%>%
  gather(Intervention, value, -year, -metric)%>%
  mutate(Intervention = factor(Intervention, levels=c("Scenario 5", "Scenario 4", "Scenario 3", "Scenario 2", "Scenario 1")))%>%
  arrange(desc(Intervention))

ggplot(plot%>%filter(metric %in% c("Cumulative cases averted", "Cumulative deaths averted"), Intervention!="Scenario 5"), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.6 , size=.5, colour="white") +
  facet_wrap(~metric, nrow=1)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative cases/deaths averted (millions)")+
  scale_fill_viridis(discrete = T) 

#ggsave("outputs/Figure2_area_new.jpeg", height=4, width=9)
write.csv(plot, "outputs/plot_data/Fig2_data.csv", row.names = F)

ggplot(plot%>%filter(metric %in% c("Cumulative cases averted", "Cumulative deaths averted")), 
       aes(x=year, y=value/1e6))+
  geom_area(aes(fill=Intervention), position = 'stack', alpha=0.6 , size=.5, colour="white") +
  facet_wrap(~metric, nrow=1)+
  theme_bw()+
  xlim(2023,2050)+
  xlab("Year")+
  ylab("Cumulative cases/deaths averted (millions)")+
  scale_fill_viridis(discrete = T) 
