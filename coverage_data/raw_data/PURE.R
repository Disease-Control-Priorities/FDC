rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stringr)
library(openxlsx)
### PURE survey data###

df1<-read_excel("pure_data.xlsx", sheet=1)
df2<-read_excel("pure_data.xlsx", sheet=2)
df3<-read_excel("pure_data.xlsx", sheet=3)
df4<-read_excel("pure_data.xlsx", sheet=4)
df5<-read_excel("pure_data.xlsx", sheet=5)

#remove (prop %)

df1[,3:5]<-sapply(df1[,3:5], function(x) as.numeric(gsub("\\(.*", "", x)))
df2[,3:5]<-sapply(df2[,3:5], function(x) as.numeric(gsub("\\(.*", "", x)))
df3[,3:8]<-sapply(df3[,3:8], function(x) as.numeric(gsub("\\(.*", "", x)))
df4[,3:8]<-sapply(df4[,3:8], function(x) as.numeric(gsub("\\(.*", "", x)))
df5[,3:8]<-sapply(df5[,3:8], function(x) as.numeric(gsub("\\(.*", "", x)))

#default to GBD names/codes
df1<-df1%>%rename(location_name = `Country name`)%>%
  mutate(location_name = ifelse(location_name=="UAE", "United Arab Emirates",
                                ifelse(location_name=="Iran", "Iran (Islamic Republic of)",
                                       ifelse(location_name=="Tanzania", "United Republic of Tanzania",
                                              ifelse(location_name=="Russia", "Russian Federation", location_name)))))
df2<-df2%>%rename(location_name = `Country name`)%>%
  mutate(location_name = ifelse(location_name=="UAE", "United Arab Emirates",
                                ifelse(location_name=="Iran", "Iran (Islamic Republic of)",
                                       ifelse(location_name=="Tanzania", "United Republic of Tanzania",
                                              ifelse(location_name=="Russia", "Russian Federation", location_name)))))
df3<-df3%>%rename(location_name = `Country name`)%>%
  mutate(location_name = ifelse(location_name=="UAE", "United Arab Emirates",
                                ifelse(location_name=="Iran", "Iran (Islamic Republic of)",
                                       ifelse(location_name=="Tanzania", "United Republic of Tanzania",
                                              ifelse(location_name=="Russia", "Russian Federation", location_name)))))
df4<-df4%>%rename(location_name = `Country name`)%>%
  mutate(location_name = ifelse(location_name=="UAE", "United Arab Emirates",
                                ifelse(location_name=="Iran", "Iran (Islamic Republic of)",
                                       ifelse(location_name=="Tanzania", "United Republic of Tanzania",
                                              ifelse(location_name=="Russia", "Russian Federation", location_name)))))
df5<-df5%>%rename(location_name = `Country name`)%>%
  mutate(location_name = ifelse(location_name=="UAE", "United Arab Emirates",
                                ifelse(location_name=="Iran", "Iran (Islamic Republic of)",
                                       ifelse(location_name=="Tanzania", "United Republic of Tanzania",
                                              ifelse(location_name=="Russia", "Russian Federation", location_name)))))

#grab list of country names
locs<-unique(df1$location_name)

### GBD Covariates ###
haqi<-read.csv("IHME_GBD_2019_COV_1980_2019_HAQI_Y2020M07D31.csv", stringsAsFactors = F)%>%
  filter(sex_label=="Both", year_id==2008, location_name %in% locs)%>%
  select(location_name, val)%>%
  rename(haqi=val)

ldi_pc<-read.csv("IHME_GBD_2019_COV_1980_2019_LDI_PC_Y2020M07D31.csv", stringsAsFactors = F)%>%
  filter(sex_label=="Both", year_id==2008, location_name %in% locs)%>%
  select(location_name, val)%>%
  rename(ldi_pc=val)

sev_ihd<-read.csv("IHME_GBD_2019_COV_1980_2019_SEV_SCALAR_AGESTD_CVD_IHD_Y2020M07D31.csv", stringsAsFactors = F)%>%
  filter(location_name %in% locs, year_id==2008)%>%
  select(location_name, val, sex_label)%>%
  spread(sex_label, val)%>%
  rename(sev_ihd_male = Male,
         sev_ihd_female = Female)

sev_stroke<-read.csv("IHME_GBD_2019_COV_1980_2019_SEV_SCALAR_AGESTD_CVD_STROKE_Y2020M07D31.csv", stringsAsFactors = F)%>%
  filter(location_name %in% locs, year_id==2008)%>%
  select(location_name, val, sex_label)%>%
  spread(sex_label, val)%>%
  rename(sev_stroke_male = Male,
         sev_stroke_female = Female)

covs<-left_join(sev_ihd, sev_stroke)
covs<-left_join(covs, haqi)
covs<-left_join(covs, ldi_pc)


### write out data ###

xl_lst <- list('Table1' = left_join(df1, covs), 
               'Table2' = left_join(df2, covs),
               'Table3' = left_join(df3, covs),
               'Table4' = left_join(df4, covs),
               'Table5' = left_join(df5, covs))

write.xlsx(xl_lst, file = "formatted_pure_data.xlsx")
