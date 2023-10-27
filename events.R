############################################################################
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, RColorBrewer, xlsx)   
############################################################################

df<-read_xlsx("events.xlsx", sheet=1)%>%
  mutate(deaths = as.numeric(sub("\\(.*", "", `CV-death`)))%>%
  select(Country, deaths)%>%
  left_join(., read_xlsx("events.xlsx", sheet=2)%>%
  mutate(NonFatalStroke = as.numeric(sub("\\(.*", "", NonFatalStroke)),
         NonFatalHF = as.numeric(sub("\\(.*", "", NonFatalHF)),
         NonFatalMI = as.numeric(sub("\\(.*", "", NonFatalMI)))
  )%>%
  mutate(
    MI_ratio = NonFatalMI/deaths,
    stroke_ratio = NonFatalStroke/deaths,
    HF_ratio = NonFatalHF/deaths
  )%>%
  filter(deaths != 0)%>%
  select(Country, MI_ratio, stroke_ratio, HF_ratio)

write.csv(df, "new_events.csv", row.names = F)


