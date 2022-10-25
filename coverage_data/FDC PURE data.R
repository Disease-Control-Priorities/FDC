rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dplyr, tidyr, ggplot2, boot)   

### tl;dr for Sarah: see models on lines listed below for coefficients to use in predictions
### predictions will give expected value in logit space, need to convert to % by using inverse logit!
### see line 143 for example prediction; use GBD covariates for ~190 countries FOR YEAR 2019 in predictions

### primary prevention - BP medication use -- extract directly from NCD-RiSC (% treated in 2019): line 60
### primary prevention - statin use best model: line 136
### primary prevention - aspirin use best model: line 169
### secondary prevention of IHD - statin use: line 202
### secondary prevention of IHD - aspirin use: line 208
### secondary prevention of IHD - beta blocker use: line 214
### secondary prevention of IHD - ACEi/ARB use: line 220
### secondary prevention of stroke - statin use: line 247
### secondary prevention of stroke - aspirin use: line 253
### secondary prevention of stroke - ACEi/ARB use (proxy for any BP medication): line 259
### model predictions : line 265
### coverage calculations: line 286

## HTN treatment coverage by country obtained from NCD-RiSC and used for this ##

##################################################
#Housekeeping

locs<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(gbd2019, iso3, wb2021, location_gbd, location_ncdrisc)%>%rename(location_name = gbd2019)

##################################################
#GBD Covariates 

#Georgia the state LocID = 533

haqi<-read.csv("IHME_GBD_2019_COV_1980_2019_HAQI_Y2020M07D31.csv", stringsAsFactors = F)%>%
  filter(location_id !=533, sex_label=="Both")%>%
  select(location_name, val, year_id)%>%
  rename(haqi=val, year=year_id)

ldi_pc<-read.csv("IHME_GBD_2019_COV_1980_2019_LDI_PC_Y2020M07D31.csv", stringsAsFactors = F)%>%
  filter(location_id !=533, sex_label=="Both")%>%
  select(location_name, val, year_id)%>%
  rename(ldi_pc=val, year=year_id)

sev_ihd<-read.csv("IHME_GBD_2019_COV_1980_2019_SEV_SCALAR_AGESTD_CVD_IHD_Y2020M07D31.csv", stringsAsFactors = F)%>%
  filter(location_id !=533)%>%
  select(location_name, val, sex_label, year_id)%>%
  spread(sex_label, val)%>%
  rename(sev_ihd_male = Male, sev_ihd_female = Female, year=year_id)

sev_stroke<-read.csv("IHME_GBD_2019_COV_1980_2019_SEV_SCALAR_AGESTD_CVD_STROKE_Y2020M07D31.csv", stringsAsFactors = F)%>%
  filter(location_id !=533)%>%
  select(location_name, val, sex_label, year_id)%>%
  spread(sex_label, val)%>%
  rename(sev_stroke_male = Male, sev_stroke_female = Female, year=year_id)

gbd.covs<-left_join(sev_ihd, sev_stroke)%>%
  left_join(., haqi)%>%left_join(., ldi_pc)

##################################################
#Proportion w/ hypertension treated (NCD-RisC)

#htn <- read.csv("HTN_PURE.csv", header = T, fileEncoding = 'UTF-8-BOM') 
#htn$pr_trt <- htn$Hyp.Treated/htn$Hypertension

htn<-read.csv("NCD-RisC_Lancet_2021_Hypertension_crude_countries.csv", stringsAsFactors = F)%>%
  rename(iso3 = ISO, year=Year)%>%
  left_join(., locs)%>%
  group_by(year,location_name, iso3)%>%
  summarise(pr_trt = mean(Proportion.of.treated.hypertension.among.all.hypertension)) #average of females & males

##################################################
# Statin and aspirin use, primary prevention data

HPACC<-read.csv("ST_ASA_HPACC_SP.csv", stringsAsFactors = F)%>%
  filter(location_name!="Burkina Faso",
         location_name!="Iran",
         location_name!="St. Vincent & the Grenadines")%>%
  mutate(location_name = ifelse(location_name=="Moldova", "Republic of Moldova", location_name),
         location_name = ifelse(location_name=="Vietnam", "Viet Nam", location_name),
         location_name = ifelse(location_name=="Timor Leste", "Timor-Leste", location_name),
         source="HPACC")

oth <- read.csv("ST-ASA_PURE.csv", header = T, fileEncoding = 'UTF-8-BOM') 
#oth$pr_sta <- oth$Statin.Use/oth$Hypertension.or.Diabetes.or.LDL.4.5.mmol.L
#oth$pr_asa <- oth$Aspirin.Use/oth$Hypertension.or.Diabetes.or.LDL.4.5.mmol.L
oth<-oth%>%mutate(pr_sta_pp = Statin.Use/Hypertension.or.Diabetes.or.LDL.4.5.mmol.L,
                  pr_asa_pp = Aspirin.Use/Hypertension.or.Diabetes.or.LDL.4.5.mmol.L,
                  year = 2008,
                  source = "PURE")%>%
  select(location_name, year, source, Sample.Size, pr_sta_pp, pr_asa_pp)

#Combine datasets PURE + HPACC
#oth <- merge(oth, htn, by = "location_name")
oth<-bind_rows(oth, HPACC%>%select(location_name, year, Sample.Size, source, pr_sta_pp, pr_asa_pp))%>%
  left_join(., htn, by=c("location_name", "year"))%>%
  left_join(., gbd.covs, by=c("location_name", "year")) #a few NAs for small island nations

##################################################
# Statin

summary(oth$pr_sta_pp)

ggplot(oth, aes(x=log(ldi_pc), y=pr_sta_pp, color=source))+
  geom_point(size=3) + 
  theme_bw()
#plot(log(oth$ldi_pc), oth$pr_sta_pp)
ggplot(oth, aes(x=haqi, y=pr_sta_pp, color=source))+
  geom_point(size=3) + 
  theme_bw()
#plot(oth$haqi, oth$pr_sta_pp)
ggplot(oth, aes(x=pr_trt, y=pr_sta_pp, color=source))+
  geom_point(size=3) + 
  theme_bw()
#plot(oth$pr_trt, oth$pr_sta_pp)
ggplot(oth, aes(x=sev_ihd_female, y=pr_sta_pp, color=source))+
  geom_point(size=3) + 
  theme_bw()
#plot(oth$sev_ihd_female, oth$pr_sta_pp) # not useful (nor is male)
ggplot(oth, aes(x=sev_stroke_female, y=pr_sta_pp, color=source))+
  geom_point(size=3) + 
  theme_bw()
#plot(oth$sev_stroke_female, oth$pr_sta_pp) # not useful (nor is male)

library(boot)
oth$pr_sta_pp <- replace(oth$pr_sta_pp, oth$pr_sta_pp==0, 0.001) # fix ZIM and TZA data

m1_sta <- lm(logit(pr_sta_pp) ~ log(ldi_pc), data = oth)
summary(m1_sta)
m2_sta <- lm(logit(pr_sta_pp) ~ haqi, data = oth)
summary(m2_sta)
m3_sta <- lm(logit(pr_sta_pp) ~ log(ldi_pc) + haqi, data = oth)
summary(m3_sta) # most of HAQI effect explained by income
m4_sta <- lm(logit(pr_sta_pp) ~ logit(pr_trt), data = oth)
summary(m4_sta)
m5_sta <- lm(logit(pr_sta_pp) ~ log(ldi_pc) + logit(pr_trt), data = oth)
summary(m5_sta) 
m6_sta <- lm(logit(pr_sta_pp) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = oth)
summary(m6_sta) # probably the best we can do
m7_sta <- lm(logit(pr_sta_pp) ~ log(ldi_pc) + logit(pr_trt) + year, weights = Sample.Size, data = oth)
summary(m7_sta) #SJP: adding year doesn't seem to help, but I haven't tinkered much with it.

# example prediction:
ex <- predict(m6_sta, newdata = data.frame(ldi_pc = 10000, pr_trt = 0.5))
paste0(signif(inv.logit(ex)*100, digits=3), "%") 
# country with ldi (GDP) of $10k and 50% HTN treated would have 4.3% on statin (only PURE data)
# with updated data, the prediction is now %4.69 (with HPACC)

##################################################
# Aspirin
summary(oth$pr_asa_pp)

plot(log(oth$ldi_pc), oth$pr_asa_pp)
plot(oth$haqi, oth$pr_asa_pp)
plot(oth$sev_ihd_female, oth$pr_asa_pp) # not useful (nor is male)
plot(oth$sev_stroke_female, oth$pr_asa_pp) # not useful (nor is male)
plot(oth$pr_trt, oth$pr_asa_pp)

oth$pr_asa_pp <- replace(oth$pr_asa_pp, oth$pr_asa_pp==0, 0.001)

m1_asa <- lm(logit(pr_asa_pp) ~ log(ldi_pc), data = oth)
summary(m1_asa)
m2_asa <- lm(logit(pr_asa_pp) ~ haqi, data = oth)
summary(m2_asa)
m3_asa <- lm(logit(pr_asa_pp) ~ log(ldi_pc) + haqi, data = oth)
summary(m3_asa) # as before, not helpful
m4_asa <- lm(logit(pr_asa_pp) ~ logit(pr_trt), data = oth)
summary(m4_asa)
m5_asa <- lm(logit(pr_asa_pp) ~ log(ldi_pc) + logit(pr_trt), data = oth)
summary(m5_asa)
m6_asa <- lm(logit(pr_asa_pp) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = oth)
summary(m6_asa) #best

plot(oth$pr_sta_pp, oth$pr_asa_pp) # correlated, as expected



##################################################
## Combo therapy use, IHD secondary prevention ##

ihd <- read.csv("IHD-SP_PURE.csv", header = T, fileEncoding = 'UTF-8-BOM')

ihd<-ihd%>%mutate(pr_sta_sp_ihd = Statin.Use/CHD,
                  pr_asa_sp_ihd = Aspirin.Use/CHD,
                  pr_bbl_ihd = Beta.Blocker/CHD,
                  pr_ras_ihd = ACEI.or.ARB/CHD,
                  year = 2008,
                  source = "PURE")%>%
  select(location_name, year, source, Sample.Size, pr_sta_sp_ihd, pr_asa_sp_ihd,pr_bbl_ihd,pr_ras_ihd)%>%
  bind_rows(., HPACC%>%
              select(location_name, year, Sample.Size, source, pr_sta_sp, pr_asa_sp)%>%
              rename(pr_sta_sp_ihd = pr_sta_sp, pr_asa_sp_ihd = pr_asa_sp))%>%
  left_join(., htn, by=c("location_name", "year"))%>%
  left_join(., gbd.covs, by=c("location_name", "year"))


#Statin
ihd$pr_sta_sp_ihd <- replace(ihd$pr_sta_sp_ihd, ihd$pr_sta_sp_ihd==0, 0.001)

ggplot(ihd, aes(x=log(ldi_pc), y=pr_sta_sp_ihd, color=source))+
  geom_point(size=3) + 
  theme_bw()

m1_sta_ihd <- lm(logit(pr_sta_sp_ihd) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = ihd)
summary(m1_sta_ihd)

# Aspirin
ihd$pr_asa_sp_ihd <- replace(ihd$pr_asa_sp_ihd, ihd$pr_asa_sp_ihd==0, 0.001)

m1_asa_ihd <- lm(logit(pr_asa_sp_ihd) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = ihd)
summary(m1_asa_ihd)

# Beta blocker
ihd$pr_bbl_ihd <- replace(ihd$pr_bbl_ihd, ihd$pr_bbl_ihd==0, 0.001)

m1_bbl_ihd <- lm(logit(pr_bbl_ihd) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = ihd)
summary(m1_bbl_ihd)

# ACEi or ARB
ihd$pr_ras_ihd <- replace(ihd$pr_ras_ihd, ihd$pr_ras_ihd==0, 0.001)

m1_ras_ihd <- lm(logit(pr_ras_ihd) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = ihd)
summary(m1_ras_ihd)


plot1<-oth%>%select(ldi_pc, pr_trt, pr_asa_pp, pr_sta_pp, source)%>%
  gather(treatment, prop, -ldi_pc, -pr_trt, -source)%>%
  mutate(treatment = ifelse(treatment=="pr_asa_pp", "Aspirin", "Statin"))

##Some plots###
ggplot(plot1, aes(x=ldi_pc, y=prop, color=source ,shape=treatment))+
  geom_point(size=2)+
  theme_bw()+
  ylab("Proportion on treatment for primary prevention")+
  xlab("Gross national income per capita  ($USD)")+
  labs(color="Data source", shape="Primary prevention \ntreatment")

ggsave("pp_vs_GNI.jpeg", height=4, width=8)

ggplot(plot1, aes(x=pr_trt, y=prop, color=source ,shape=treatment))+
  geom_point(size=2)+
  theme_bw()+
  ylab("Proportion on treatment for primary prevention")+
  xlab("Proportion of hypertensive patients on treatment")+
  labs(color="Data source", shape="Primary prevention \ntreatment")

ggsave("pp_vs_HTN.jpeg", height=4, width=8)

##################################################
## Combo therapy use, stroke secondary prevention ##

cva <- read.csv("CVA-SP_PURE.csv", header = T, fileEncoding = 'UTF-8-BOM')

cva<-cva%>%mutate(pr_sta_sp_cva = Statin.Use/Stroke.Only,
                  pr_asa_sp_cva = Aspirin.Use/Stroke.Only,
                  pr_ras_cva = ACEI.or.ARB/Stroke.Only,
                  year = 2008,
                  source = "PURE")%>%
  select(location_name, year, source, Sample.Size, pr_sta_sp_cva, pr_asa_sp_cva, pr_ras_cva)%>%
  bind_rows(., HPACC%>%
              select(location_name, year, Sample.Size, source, pr_sta_sp, pr_asa_sp)%>%
              rename(pr_sta_sp_cva = pr_sta_sp, pr_asa_sp_cva = pr_asa_sp))%>%
  left_join(., htn, by=c("location_name", "year"))%>%
  left_join(., gbd.covs, by=c("location_name", "year"))

# Statin
cva$pr_sta_sp_cva <- replace(cva$pr_sta_sp_cva, cva$pr_sta_sp_cva==0, 0.001)

ggplot(cva, aes(x=log(ldi_pc), y=pr_sta_sp_cva, color=source))+
  geom_point(size=3) + 
  theme_bw()

m1_sta_cva <- lm(logit(pr_sta_sp_cva) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = cva)
summary(m1_sta_cva)

# Aspirin
cva$pr_asa_sp_cva <- replace(cva$pr_asa_sp_cva, cva$pr_asa_sp_cva==0, 0.001)

m1_asa_cva <- lm(logit(pr_asa_sp_cva) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = cva)
summary(m1_asa_cva)

# ACEi or ARB
cva$pr_ras_cva <- replace(cva$pr_ras_cva, cva$pr_ras_cva==0, 0.001)

m1_ras_cva <- lm(logit(pr_ras_cva) ~ log(ldi_pc) + logit(pr_trt), weights = Sample.Size, data = cva)
summary(m1_ras_cva) # makes sense; correlated w/ hypertension


##################################################
#Model predictions for all countries (2019)

covariates<-htn%>%
  filter(year==2019)%>%
  left_join(.,gbd.covs%>%filter(year==2019))%>%
  ungroup()%>%select(-year)%>%na.omit()

baseline.coverage<-data.frame(location_name = covariates$location_name,
                              statin.pp = inv.logit(predict(m6_sta, newdata=covariates)),
                              aspirin.pp = inv.logit(predict(m6_asa, newdata=covariates)),
                              statin.sp.ihd = inv.logit(predict(m1_sta_ihd, newdata=covariates)),
                              aspirin.sp.ihd = inv.logit(predict(m1_asa_ihd, newdata=covariates)),
                              bbl.sp.ihd = inv.logit(predict(m1_bbl_ihd, newdata=covariates)),
                              ras.sp.ihd = inv.logit(predict(m1_ras_ihd, newdata=covariates)),
                              statin.sp.cva = inv.logit(predict(m1_sta_cva, newdata=covariates)),
                              aspirin.sp.cva = inv.logit(predict(m1_asa_cva, newdata=covariates)),
                              ras.sp.cva = inv.logit(predict(m1_ras_cva, newdata=covariates))
                              )
                              
##################################################
#plots for appendix
plot2<-baseline.coverage%>%
  left_join(., htn%>%filter(year==2019))%>%
  select(-year)%>%
  rename(`Statin (PP)` = statin.pp,
         `Aspirin (PP)` = aspirin.pp,
         `Statin (SP for IHD)` = statin.sp.ihd,
         `Statin (SP for stroke)` = statin.sp.cva,
         `Aspirin (SP for IHD)` = aspirin.sp.ihd,
         `Aspirin (SP for stroke)` = aspirin.sp.cva,
         `ACEi/ARB (SP for IHD)` = ras.sp.ihd,
         `ACEi/ARB (SP for stroke)` = ras.sp.cva,
         `Beta blocker (SP for IHD)` = bbl.sp.ihd,
         `Hypertension treatment` = pr_trt)%>%
  gather(medication, prop, -location_name, -iso3)%>%
  left_join(., locs%>%select(iso3, wb2021))%>%
  mutate(wb2021 = factor(wb2021, levels=c("HIC", "UMIC", "LMIC", "LIC")),
         medication = factor(medication, levels=c("Statin (PP)", "Aspirin (PP)",
                             "Statin (SP for stroke)", "Statin (SP for IHD)",
                             "Aspirin (SP for stroke)", "Aspirin (SP for IHD)",
                             "Beta blocker (SP for IHD)",
                             "ACEi/ARB (SP for stroke)", "ACEi/ARB (SP for IHD)",
                              "Hypertension treatment")))

ggplot(plot2, aes(y=medication, x=prop*100, color=wb2021))+
  geom_jitter()+
  theme_bw()+
  ylab("Medication")+
  xlab("Proportion on treatment (%)")+
  labs(color="Income Group")

ggsave("jitter_plot.jpeg", height = 4, width=8)


#observed vs. predicted
plot3<-oth%>%select(location_name, year, pr_sta_pp, pr_asa_pp)%>%
  left_join(., ihd%>%select(location_name, year, pr_sta_sp_ihd, pr_asa_sp_ihd,
                            pr_bbl_ihd, pr_ras_ihd))%>%
  left_join(., cva%>%select(location_name, year, pr_sta_sp_cva,
                            pr_asa_sp_cva, pr_ras_cva))%>%
  rename(`Statin (PP)` = pr_sta_pp,
         `Aspirin (PP)` = pr_asa_pp,
         `Statin (SP for IHD)` = pr_sta_sp_ihd,
         `Statin (SP for stroke)` = pr_sta_sp_cva,
         `Aspirin (SP for IHD)` = pr_asa_sp_ihd,
         `Aspirin (SP for stroke)` = pr_asa_sp_cva,
         `ACEi/ARB (SP for IHD)` = pr_ras_ihd,
         `ACEi/ARB (SP for stroke)` = pr_ras_cva,
         `Beta blocker (SP for IHD)` = pr_bbl_ihd)%>%
  gather(medication, prop_observed, -location_name, -year)


covariates_plot<-htn%>%
  filter(year%in%unique(plot3$year))%>%
  left_join(.,gbd.covs)%>%
  ungroup()%>%na.omit()

baseline.coverage.plot<-data.frame(location_name = covariates$location_name,
                                   pr_sta_pp = inv.logit(predict(m6_sta, newdata=covariates)),
                                   pr_asa_pp = inv.logit(predict(m6_asa, newdata=covariates)),
                                   pr_sta_sp_ihd = inv.logit(predict(m1_sta_ihd, newdata=covariates)),
                                   pr_asa_sp_ihd = inv.logit(predict(m1_asa_ihd, newdata=covariates)),
                                   pr_bbl_ihd = inv.logit(predict(m1_bbl_ihd, newdata=covariates)),
                                   pr_ras_ihd = inv.logit(predict(m1_ras_ihd, newdata=covariates)),
                                   pr_sta_sp_cva = inv.logit(predict(m1_sta_cva, newdata=covariates)),
                                   pr_asa_sp_cva = inv.logit(predict(m1_asa_cva, newdata=covariates)),
                                   pr_ras_cva = inv.logit(predict(m1_ras_cva, newdata=covariates)))%>%
  rename(`Statin (PP)` = pr_sta_pp,
         `Aspirin (PP)` = pr_asa_pp,
         `Statin (SP for IHD)` = pr_sta_sp_ihd,
         `Statin (SP for stroke)` = pr_sta_sp_cva,
         `Aspirin (SP for IHD)` = pr_asa_sp_ihd,
         `Aspirin (SP for stroke)` = pr_asa_sp_cva,
         `ACEi/ARB (SP for IHD)` = pr_ras_ihd,
         `ACEi/ARB (SP for stroke)` = pr_ras_cva,
         `Beta blocker (SP for IHD)` = pr_bbl_ihd)%>%
  gather(medication, prop_predicted, -location_name)

plot3<-left_join(plot3, baseline.coverage.plot)

ggplot(plot3, aes(x=prop_observed*100, y=prop_predicted*100, color=medication))+
  geom_point(size=2)+
  theme_bw()+
  xlab("Observed proportion on medications (%)")+
  ylab("Predicted proportion on medications (%)")+
  geom_abline(slope=1, intercept=0, size=1, color="grey")+
  xlim(0,100)+
  ylim(0,100)+
  labs(color="Medication")

ggsave("obs_vs_pred.jpeg", height=6, width=8)

ggplot(plot3, aes(x=prop_observed*100, y=prop_predicted*100))+
  geom_point()+
  facet_wrap(~medication)+
  geom_abline(slope=1, intercept=0, size=1, color="red", alpha=0.5)+
  theme_bw()+
  xlab("Observed proportion on medications (%)")+
  ylab("Predicted proportion on medications (%)")+
  xlim(0,100)+
  ylim(0,100)

ggsave("obs_vs_pred_panel.jpeg", height=6, width=6)
  
##################################################
#Coverage calculations

#We start by defining the ratio of the proportion of persons with hypertension who are controlled to 
#the proportion of persons with hypertension who are on treatment as the effective coverage factor r_HTN 
#that is used to adjust estimates of statin and aspirin use for primary prevention. 

cascade <- read.csv("NCD-RisC_Lancet_2021_Hypertension_crude_countries.csv", stringsAsFactors = F)%>%
  rename(iso3 = ISO, year=Year)%>%
  filter(year==2019)%>%
  left_join(., locs)%>%
  group_by(location_name, iso3)%>%  #average of females & males
  summarise(pr_ddx = mean(Proportion.of.diagnosed.hypertension.among.all.hypertension),
            pr_trt = mean(Proportion.of.treated.hypertension.among.all.hypertension),
            pr_ctrl = mean(Proportion.of.controlled.hypertension.among.all.hypertension))%>%
  mutate(r_HTN_pp = pr_ctrl/pr_trt)

#For secondary prevention, the effective coverage factor is the ratio of the proportion of persons with 
#IHD or stroke who are on all indicated medications to the proportion of persons with IHD or stroke who 
#are on at least one indicated medication, giving two additional measures, r_IHD and  r_stroke.)

#Use estimation from https://www-sciencedirect-com.offcampus.lib.washington.edu/science/article/pii/S0140673611612154
#Figure 2

sp_cascade<-data.frame(wb2021=c("HIC", "UMIC", "LMIC", "LIC"),
                       r_IHD = c(50/88, 15/55, 5/30, 5/20),
                       r_stroke = c(35/80, 15/48, 5/54, 1/82))%>%
  right_join(., locs)

baseline.coverage<-left_join(baseline.coverage, cascade%>%select(location_name, iso3, pr_trt, r_HTN_pp))%>%
  left_join(., sp_cascade)%>%
  merge(.,  data.frame(cause=c("ihd", "istroke", "hstroke")))%>%
  mutate(pp_cov = ifelse(cause=="ihd", r_HTN_pp*(0.56*statin.pp + 0.44*pr_trt),
                     ifelse(cause=="istroke", r_HTN_pp*(0.44*statin.pp + 0.56*pr_trt),
                            r_HTN_pp*(0.44*statin.pp + 0.56*pr_trt))),
         sp_cov = ifelse(cause=="ihd", r_IHD*(0.25*aspirin.sp.ihd + 0.15*statin.sp.ihd + 0.28*bbl.sp.ihd + 0.32*ras.sp.ihd),
                         ifelse(cause=="istroke", r_stroke*(0.40*aspirin.sp.ihd + 0.19*statin.sp.ihd + 0.41*ras.sp.ihd),
                                r_stroke*(0.32*statin.sp.ihd + 0.68*ras.sp.ihd))) 
         )

#plot for sanity check
plot<-left_join(baseline.coverage, covariates)

ggplot(plot, aes(x=ldi_pc, y=pp_cov, color=wb2021))+
  geom_point()+
  facet_wrap(~cause)+
  ggtitle("Primary prevention coverage, no aspirin")

ggsave("PP_coverage.jpeg", height=4, width=8)

ggplot(plot, aes(x=ldi_pc, y=sp_cov, color=wb2021))+
  geom_point()+
  facet_wrap(~cause)+
  ggtitle("Secondary prevention coverage, no aspirin")
  
ggsave("SP_coverage.jpeg", height=4, width=8)

write.csv(baseline.coverage%>%select(location_name, iso3, pp_cov, sp_cov), 
          "coverage.csv", row.names = F)

#Scenarios targets

#Scenario 1 PP: Already on treatment but not optimized (step-up or substitution therapy)
#Scenario 1 SP: Already on some but not all medications (step-up or substitution therapy)
#Scenario 2 PP: Aware of high CVD risk but not on any treatment (initiation of therapy)
#Scenario 2 SP: 70% (aware assumption)
#Scenarios 3+4: 80%

#baseline rate of increase
b_inc<-read.csv("baseline_increase.csv", stringsAsFactors = F)%>%
  left_join(., locs)%>%
  select(location_name, base_inc)

#on 1-2 drugs medication
sp_treated<-data.frame(wb2021=c("HIC", "UMIC", "LMIC", "LIC"),
                       pr_trt_IHD = c(0.40, 0.40, 0.25, 0.15),
                       pr_trt_stroke = c(0.50, 0.40, 0.55, 0.20))%>%
  right_join(., locs%>%select(-location_gbd, -location_ncdrisc))%>%
  gather(cause, pr_trt_SP, -wb2021, -iso3, -location_name)%>%
  mutate(cause = ifelse(cause=="pr_trt_IHD", "ihd", "istroke"))

prop.cov<-left_join(cascade%>%merge(., data.frame(cause=c("ihd", "hhd", "istroke", "hstroke"))), sp_treated)%>%
  select(location_name, iso3, cause, pr_ddx, pr_trt, pr_trt_SP)

targets<-merge(x = c("Baseline", "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"),
               y = c("ihd", "hhd", "istroke", "hstroke"))%>%
  rename(intervention = x, cause =y)%>%
  mutate(PP_eff = ifelse(cause == "ihd", 1-0.59,
                         ifelse(cause=="istroke", 1-0.55,
                                ifelse(cause=="hstroke", 1-0.66,0))),
         SP_eff = ifelse(cause == "ihd", 1-0.576,
                         ifelse(cause=="istroke", 1-0.576,0)),
         cov_inc = ifelse(intervention == "Baseline", 0,
                          ifelse(intervention=="Scenario 4", 0.045,
                                 ifelse(intervention=="Scenario 3", 0.025, 
                                        ifelse(intervention=="Scenario 2", 0.015, 0.015))))
  )

#these are the countries in which the historic rates are faster than 1.5% per year
hps<-c("Republic of Korea", "Costa Rica", "Portugal", "Germany", "Iceland", "Canada")

#any countries w/ pr_ddx or pr_trt >80% ?
unique(prop.cov%>%filter(pr_ddx>0.8)%>%pull(location_name))
prop.cov<-prop.cov%>%mutate(pr_ddx = ifelse(pr_ddx>0.8,0.8, pr_ddx))

unique(prop.cov%>%filter(pr_trt>0.8)%>%pull(location_name))
unique(prop.cov%>%filter(pr_trt_SP>0.8)%>%pull(location_name))

df_out<-left_join(targets, prop.cov)%>%
  left_join(., b_inc)%>%
  mutate(cov_inc = ifelse(intervention=="Baseline", base_inc, cov_inc),
         cov_inc = ifelse(intervention=="Scenario 1" & location_name%in%hps, base_inc, cov_inc))%>%
  merge(., data.frame(year=2017:2050))%>%
  left_join(., baseline.coverage)%>%
  mutate(pp.cov.inc = ifelse(year>=2023, cov_inc*(year-2022), 0),
         sp.cov.inc = ifelse(year>=2023, cov_inc*(year-2022), 0),
         pp.max = ifelse(intervention == "Scenario 4", 0.9, 
                         ifelse(intervention == "Scenario 3", 0.8,
                                ifelse(intervention =="Scenario 2", pr_ddx, pr_trt))), #cap at Scenario-specific targets
         sp.max = ifelse(intervention == "Scenario 4", 0.9, 
                         ifelse(intervention == "Scenario 3", 0.8,
                                ifelse(intervention == "Scenario 2", 0.8, pr_trt_SP))),
         pp.cov.inc = ifelse(pp_cov+pp.cov.inc>=pp.max, (pp.max-pp_cov), pp.cov.inc),
         sp.cov.inc = ifelse(sp_cov+sp.cov.inc>=sp.max, (sp.max-sp_cov), sp.cov.inc)
         )%>%
  select(location_name, iso3, cause, year, intervention, 
         pp.cov.inc, sp.cov.inc, pp_cov, sp_cov,
         PP_eff, SP_eff)%>%
  mutate(pp.cov.inc = ifelse(pp.cov.inc<0, 0, pp.cov.inc),
         sp.cov.inc = ifelse(sp.cov.inc<0, 0, sp.cov.inc))


#sanity check
ggplot(df_out%>%filter(cause=="ihd"), aes(x=year, y=pp.cov.inc+pp_cov, group=location_name))+
  facet_wrap(~intervention, nrow=1)+
  geom_line()+
  ylab("Primary prevention coverage")+
  xlab("Year")+
  ggtitle("Primary prevention coverage scale-up, no aspirin: IHD")+
  ylim(0,1)

ggsave("../outputs/PP_coverage_noaspirin.jpeg", height=4, width=12)

ggplot(df_out%>%filter(cause=="ihd"), aes(x=year, y=sp.cov.inc+sp_cov, group=location_name))+
  facet_wrap(~intervention, nrow=1)+
  geom_line()+
  ylab("Secondary prevention coverage")+
  xlab("Year")+
  ggtitle("Secondary prevention coverage scale-up: IHD")+
  ylim(0,1)

ggsave("../outputs/SP_coverage.jpeg", height=4, width=12)

#change to old gbd names
df_out<-left_join(df_out, locs)%>%
  rename(location=location_gbd)

write.csv(df_out, "../scale-up.csv", row.names = F)
#df_out<-read.csv("../scale-up.csv", stringsAsFactors = F)

#####################################################################
## With aspirin ##
#####################################################################

covariates<-htn%>%
  filter(year==2019)%>%
  left_join(.,gbd.covs%>%filter(year==2019))%>%
  ungroup()%>%select(-year)%>%na.omit()

baseline.coverage<-data.frame(location_name = covariates$location_name,
                              statin.pp = inv.logit(predict(m6_sta, newdata=covariates)),
                              aspirin.pp = inv.logit(predict(m6_asa, newdata=covariates)),
                              statin.sp.ihd = inv.logit(predict(m1_sta_ihd, newdata=covariates)),
                              aspirin.sp.ihd = inv.logit(predict(m1_asa_ihd, newdata=covariates)),
                              bbl.sp.ihd = inv.logit(predict(m1_bbl_ihd, newdata=covariates)),
                              ras.sp.ihd = inv.logit(predict(m1_ras_ihd, newdata=covariates)),
                              statin.sp.cva = inv.logit(predict(m1_sta_cva, newdata=covariates)),
                              aspirin.sp.cva = inv.logit(predict(m1_asa_cva, newdata=covariates)),
                              ras.sp.cva = inv.logit(predict(m1_ras_cva, newdata=covariates))
)


##################################################
#Coverage calculations

#We start by defining the ratio of the proportion of persons with hypertension who are controlled to 
#the proportion of persons with hypertension who are on treatment as the effective coverage factor r_HTN 
#that is used to adjust estimates of statin and aspirin use for primary prevention. 

cascade <- read.csv("NCD-RisC_Lancet_2021_Hypertension_crude_countries.csv", stringsAsFactors = F)%>%
  rename(iso3 = ISO, year=Year)%>%
  filter(year==2019)%>%
  left_join(., locs)%>%
  group_by(location_name, iso3)%>%  #average of females & males
  summarise(pr_ddx = mean(Proportion.of.diagnosed.hypertension.among.all.hypertension),
            pr_trt = mean(Proportion.of.treated.hypertension.among.all.hypertension),
            pr_ctrl = mean(Proportion.of.controlled.hypertension.among.all.hypertension))%>%
  mutate(r_HTN_pp = pr_ctrl/pr_trt)

#For secondary prevention, the effective coverage factor is the ratio of the proportion of persons with 
#IHD or stroke who are on all indicated medications to the proportion of persons with IHD or stroke who 
#are on at least one indicated medication, giving two additional measures, r_IHD and  r_stroke.)

#Use estimation from https://www-sciencedirect-com.offcampus.lib.washington.edu/science/article/pii/S0140673611612154
#Figure 2

sp_cascade<-data.frame(wb2021=c("HIC", "UMIC", "LMIC", "LIC"),
                       r_IHD = c(50/88, 15/55, 5/30, 5/20),
                       r_stroke = c(35/80, 15/48, 5/54, 1/82))%>%
  right_join(., locs)

baseline.coverage<-left_join(baseline.coverage, cascade%>%select(location_name, iso3, pr_trt, r_HTN_pp))%>%
  left_join(., sp_cascade)%>%
  merge(.,  data.frame(cause=c("ihd", "istroke", "hstroke")))%>%
  mutate(pp_cov = ifelse(cause=="ihd", r_HTN_pp*(0.28*aspirin.pp+ 0.40*statin.pp + 0.31*pr_trt),
                         ifelse(cause=="istroke", r_HTN_pp*(0.29*aspirin.pp+ 0.31*statin.pp + 0.39*pr_trt),
                                r_HTN_pp*(0.44*statin.pp + 0.56*pr_trt))),
         sp_cov = ifelse(cause=="ihd", r_IHD*(0.25*aspirin.sp.ihd + 0.15*statin.sp.ihd + 0.28*bbl.sp.ihd + 0.32*ras.sp.ihd),
                         ifelse(cause=="istroke", r_stroke*(0.40*aspirin.sp.ihd + 0.19*statin.sp.ihd + 0.41*ras.sp.ihd),
                                r_stroke*(0.32*statin.sp.ihd + 0.68*ras.sp.ihd))) 
  )

#plot for sanity check
plot<-left_join(baseline.coverage, covariates)

ggplot(plot, aes(x=ldi_pc, y=pp_cov, color=wb2021))+
  geom_point()+
  facet_wrap(~cause)+
  ggtitle("Primary prevention coverage, with aspirin")

ggsave("PP_coverage_aspirin.jpeg", height=4, width=8)

ggplot(plot, aes(x=ldi_pc, y=sp_cov, color=wb2021))+
  geom_point()+
  facet_wrap(~cause)+
  ggtitle("Secondary prevention coverage, with aspirin")

ggsave("SP_coverage_aspirin.jpeg", height=4, width=8)

write.csv(baseline.coverage%>%select(location_name, iso3, pp_cov, sp_cov), 
          "coverage_aspirin.csv", row.names = F)

#Scenarios targets

#Scenario 1 PP: Already on treatment but not optimized (step-up or substitution therapy)
#Scenario 1 SP: Already on some but not all medications (step-up or substitution therapy)
#Scenario 2 PP: Aware of high CVD risk but not on any treatment (initiation of therapy)
#Scenario 2 SP: 70% (aware assumption)
#Scenarios 3+4: 80%

#baseline rate of increase
b_inc<-read.csv("baseline_increase.csv", stringsAsFactors = F)%>%
  left_join(., locs)%>%
  select(location_name, base_inc)

#on 1-2 drugs medication
sp_treated<-data.frame(wb2021=c("HIC", "UMIC", "LMIC", "LIC"),
                       pr_trt_IHD = c(0.40, 0.40, 0.25, 0.15),
                       pr_trt_stroke = c(0.50, 0.40, 0.55, 0.20))%>%
  right_join(., locs%>%select(-location_gbd, -location_ncdrisc))%>%
  gather(cause, pr_trt_SP, -wb2021, -iso3, -location_name)%>%
  mutate(cause = ifelse(cause=="pr_trt_IHD", "ihd", "istroke"))

prop.cov<-left_join(cascade%>%merge(., data.frame(cause=c("ihd", "hhd", "istroke", "hstroke"))), sp_treated)%>%
  select(location_name, iso3, cause, pr_ddx, pr_trt, pr_trt_SP)

#any countries w/ pr_ddx or pr_trt >80% ?
unique(prop.cov%>%filter(pr_ddx>0.8)%>%pull(location_name))
prop.cov<-prop.cov%>%mutate(pr_ddx = ifelse(pr_ddx>0.8,0.8, pr_ddx))

unique(prop.cov%>%filter(pr_trt>0.8)%>%pull(location_name))
unique(prop.cov%>%filter(pr_trt_SP>0.8)%>%pull(location_name))


targets<-merge(x = c("Baseline", "Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"),
               y = c("ihd", "hhd", "istroke", "hstroke"))%>%
  rename(intervention = x, cause =y)%>%
  mutate(PP_eff = ifelse(cause == "ihd", 1-0.47,
                         ifelse(cause=="istroke", 1-0.45,
                                ifelse(cause=="hstroke", 1-0.63,0))),
         SP_eff = ifelse(cause == "ihd", 1-0.51,
                         ifelse(cause=="istroke", 1-0.51,0)),
         cov_inc = ifelse(intervention == "Baseline", 0,
                          ifelse(intervention=="Scenario 4", 0.045,
                                 ifelse(intervention=="Scenario 3", 0.045, 
                                        ifelse(intervention=="Scenario 2", 0.015, 0.015))))
  )

df_out<-left_join(targets, prop.cov)%>%
  left_join(., b_inc)%>%
  mutate(cov_inc = ifelse(intervention=="Baseline", base_inc, cov_inc),
         cov_inc = ifelse(intervention=="Scenario 1" & location_name%in%hps, base_inc, cov_inc))%>%
  merge(., data.frame(year=2017:2050))%>%
  left_join(., baseline.coverage)%>%
  mutate(pp.cov.inc = ifelse(year>=2023, cov_inc*(year-2022), 0),
         sp.cov.inc = ifelse(year>=2023, cov_inc*(year-2022), 0),
         pp.max = ifelse(intervention == "Scenario 4", 0.9, 
                         ifelse(intervention == "Scenario 3", 0.8,
                                ifelse(intervention =="Scenario 2", pr_ddx, pr_trt))), #cap at Scenario-specific targets
         sp.max = ifelse(intervention == "Scenario 4", 0.9, 
                         ifelse(intervention == "Scenario 3", 0.8,
                                ifelse(intervention == "Scenario 2", 0.8, pr_trt_SP))),
         pp.cov.inc = ifelse(pp_cov+pp.cov.inc>=pp.max, (pp.max-pp_cov), pp.cov.inc),
         sp.cov.inc = ifelse(sp_cov+sp.cov.inc>=sp.max, (sp.max-sp_cov), sp.cov.inc)
  )%>%
  select(location_name, iso3, cause, year, intervention, 
         pp.cov.inc, sp.cov.inc, pp_cov, sp_cov,
         PP_eff, SP_eff)%>%
  mutate(pp.cov.inc = ifelse(pp.cov.inc<0, 0, pp.cov.inc),
         sp.cov.inc = ifelse(sp.cov.inc<0, 0, sp.cov.inc))

#change to old gbd names
df_out<-left_join(df_out, locs)%>%
  rename(location=location_gbd)

write.csv(df_out, "../scale-up_withaspirin.csv", row.names = F)


#sanity check
ggplot(df_out%>%filter(cause=="ihd"), aes(x=year, y=pp.cov.inc+pp_cov, group=location_name))+
  facet_wrap(~intervention, nrow=1)+
  geom_line()+
  ylab("Primary prevention coverage")+
  xlab("Year")+
  ggtitle("Primary prevention coverage scale-up, with aspirin: IHD")+
  ylim(0,1)

ggsave("../outputs/PP_coverage_aspirin.jpeg", height=4, width=12)

