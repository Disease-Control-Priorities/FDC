rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dplyr, tidyr, ggplot2, boot, data.table, tidyverse)   

locs<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(gbd2019, iso3, wb2021, location_gbd, location_ncdrisc)%>%rename(location_name = gbd2019)

df<-read.csv("../cascade_data_all.csv", stringsAsFactors = F)

adjust<-read.csv("../adherence_scale.csv")%>%
  filter(intervention=="Scenario 4")%>%
  select(-intervention)

unique(df$year)
unique(adjust$year)

plot<-left_join(df, adjust)%>%
  mutate(cov_adj = ifelse(is.na(cov_adj), 1, cov_adj))%>%
  mutate(new_name = ifelse(group == "with aspirin, pp" & scenario == "Baseline", "Current care", NA),
         new_name = ifelse(group == "no aspirin, pp" & scenario == "Scenario 1", "Base case, targeted", new_name),
         new_name = ifelse(group == "no aspirin, pp" & scenario == "Scenario 2", "Worst case, targeted", new_name),
         new_name = ifelse(group == "with aspirin, pp" & scenario == "Scenario 2", "Best case, targeted", new_name),
         new_name = ifelse(group == "no aspirin, pp" & scenario == "Scenario 3", "Base case, population-wide", new_name),
         new_name = ifelse(group == "with aspirin, pp" & scenario == "Scenario 3", "Worst case, population-wide", new_name), #doesn't actually contain aspirin, just using this temp file to distinguish
         new_name = ifelse(group == "with aspirin, pp" & scenario == "Scenario 4", "Best case, population-wide", new_name))

plot<-plot%>%
  mutate(Treated = ifelse(new_name %in% c("Worst case, targeted", "Worst case, population-wide"), Treated*cov_adj, Treated),
         Control = ifelse(new_name %in% c("Worst case, targeted", "Worst case, population-wide"), Control*cov_adj, Control))%>%
  na.omit()%>%
  select(-scenario, -cov_adj, -group)

#plot<-plot%>%na.omit()%>%select(-scenario, -cov_adj, -group)

  
#cascade line plot:
plot2<-plot%>%group_by(year, new_name)%>%
  summarise(Control = mean(Control),
            Treated = mean(Treated),
            Aware = mean(Aware))%>%
  gather(Metric, val, -year, -new_name)%>%
  mutate(Metric = ifelse(Metric=="Treated", "On treatment", Metric),
         Metric = ifelse(Metric=="Control", "Optimized", Metric))%>%
  mutate(Metric = factor(Metric, levels=c("Aware", "On treatment", "Optimized")))%>%
  mutate(new_name = factor(new_name, levels=c("Best case, population-wide",
                                              "Base case, population-wide",
                                              "Best case, targeted",
                                              "Base case, targeted",
                                              "Worst case, population-wide",
                                              "Worst case, targeted",
                                              "Current care")))

ggplot(plot2, aes(x=year, y=val, color=new_name))+
  geom_line(size=1)+
  facet_wrap(~Metric)+
  ylab("Proportion of all primary prevention patients")+
  xlab("Year")+
  ylim(0,1)+
  labs(color = "Scenario")+
  theme_bw()

ggsave("../outputs/cascade_0727.jpeg", height=4, width=8)


##

inc<-read.csv("../scale-up-new-aspirin.csv", stringsAsFactors = F)%>%
  mutate(group = "with aspirin")
inc2<-read.csv("../scale-up-new.csv", stringsAsFactors = F)%>%
  mutate(group = "no aspirin")

any(is.na(inc))

cov_inc<-bind_rows(inc, inc2)%>%
  left_join(., adjust)%>%
  mutate(cov_adj = ifelse(is.na(cov_adj), 1, cov_adj))%>%
  mutate(new_name = ifelse(group == "with aspirin" & intervention == "Baseline", "Current care", NA),
         new_name = ifelse(group == "no aspirin" & intervention == "Scenario 1", "Base case, targeted", new_name),
         new_name = ifelse(group == "no aspirin" & intervention == "Scenario 2", "Worst case, targeted", new_name),
         new_name = ifelse(group == "with aspirin" & intervention == "Scenario 2", "Best case, targeted", new_name),
         new_name = ifelse(group == "no aspirin" & intervention == "Scenario 3", "Worst case, population-wide", new_name),
         new_name = ifelse(group == "with aspirin" & intervention == "Scenario 4", "Best case, population-wide", new_name))

cov_inc<-bind_rows(cov_inc, cov_inc%>%filter(new_name=="Worst case, population-wide")%>%mutate(new_name = "Base case, population-wide"))%>%  
  mutate(pp.cov.inc = ifelse(new_name %in% c("Worst case, targeted", "Worst case, population-wide"), pp.cov.inc*cov_adj, pp.cov.inc),
         sp.cov.inc = ifelse(new_name %in% c("Worst case, targeted", "Worst case, population-wide"), sp.cov.inc*cov_adj, sp.cov.inc),
         pp.trt.inc = ifelse(new_name %in% c("Worst case, targeted", "Worst case, population-wide"), pp.trt.inc*cov_adj, pp.trt.inc),
         sp.trt.inc = ifelse(new_name %in% c("Worst case, targeted", "Worst case, population-wide"), sp.trt.inc*cov_adj, sp.trt.inc))%>%
  na.omit()%>%
  select(-cov_adj, -group, -intervention)%>%
  rename(intervention = new_name)

unique(cov_inc$intervention)

write.csv(cov_inc, "new_scale_up.csv", row.names = F)
