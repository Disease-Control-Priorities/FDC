rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer)   

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


#########
#old
#########

load("output2.Rda")
load("output_aspirin2.Rda")

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
load("base_rates.Rda")

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
  
  
intervention_rates<-intervention_rates%>%
  mutate(intervention = ifelse(intervention=="Baseline", "Current care", intervention))
  
ggplot(intervention_rates%>%filter(year%in%c(2020,2035,2050), sex=="Female", cause=="ihd"), 
       aes(x=age, y=CF, color=intervention))+
  geom_line(size=0.8)+
  facet_wrap(~year)+
  ylab("Transition probability \n(IHD to death)")+
  xlab("Age")+
  theme_bw()+ 
  scale_y_sqrt()+
  labs(color = "Intervention")
  
ggsave("outputs/CF_plot_India.jpeg", height=6, width=8)
  
  ggplot(intervention_rates%>%filter(year%in%c(2020,2035,2050), sex=="Female", cause=="ihd"), 
         aes(x=age, y=IR, color=intervention))+
    geom_line(size=0.8)+
    facet_wrap(~year)+
    ylab("Transition probablity \n(Well to IHD)")+
    xlab("Age")+
    theme_bw()+ scale_y_sqrt()+
    labs(color = "Intervention")
  
ggsave("outputs/IR_plot_India.jpeg", height=6, width=8)


  
