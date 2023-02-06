#################################################################################################
#################################################################################################
###########################################
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#################################################################################################

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

#################################################################################################
load("../not_on_github/PreppedData.Rda")

#Set coverage to 2.5% linear increase til 64% cap
inc<-data.in%>%select(htncov2, sex, iso3)%>%unique()%>%
  group_by(iso3)%>%
  summarise(htncov2 = mean(htncov2))%>%
  left_join(., inc)%>%
  mutate(cov.inc = ifelse(Year>=2023, (Year-2022)*0.025, 0),
         new.cov = htncov2+cov.inc,
         cov.inc = ifelse(new.cov>0.64, 0.64-htncov2, cov.inc),
         cov.inc = ifelse(cov.inc<aroc2, aroc2, cov.inc),
         new.cov = htncov2+cov.inc,
         a_change2 = cov.inc,
         p_change2 = cov.inc)

#################################################################################################
# As a function
#################################################################################################
project.all <- function(Country, saltmet, salteff, saltyear2, drugcov){
  #################################################################################################
  base_rates<-b_rates[location==Country]#[, -c("year")]
  #################################################################################################
  #################################################################################################
  #intervention scenarios
  DT<-unique(data.in[location==Country][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  
  bp_prob_drug<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_base<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 0, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_bau<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, "baseline")
  
  bp_prob_drug[,intervention:="Antihypertensive therapy"]
  bp_prob_bau[,intervention:="b.a.u"]

  setnames(bp_prob_base, "prob", "prob_0")
  
  bp_probs<-bind_rows(bp_prob_drug, bp_prob_bau)
  bp_probs<-merge(bp_probs, bp_prob_base, by=c("age","sex", "bp_cat", "Year", "location")) #change to "Year"
  
  #duplicating data to be age-specific
  bp_probs[, age:=as.numeric(substr(age, 1,2))]
  bp_probs<-bp_probs[rep(seq_len(nrow(bp_probs)), each=5)]
  bp_probs[,age2:=rep(1:5, nrow(bp_probs)/5)][,age:=age+age2-1]
  
  over90<-bp_probs[age==89]
  
  over90<-over90[rep(seq_len(nrow(over90)), each=6)]
  over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]
  
  #bind  
  bp_probs<-rbindlist(list(bp_probs, over90))
  
  ##add RRis##
  addRR<-function(RR, bp){
    if(bp=="<120"){1}
    else if (bp=="120-129"){1/RR}
    else if (bp=="130-139"){1/RR^2}
    else if (bp=="140-149"){1/RR^3}
    else if (bp=="150-159"){1/RR^4}
    else if (bp=="160-169"){1/RR^5}
    else if (bp=="170-179"){1/RR^6}
    else {1/RR^7}
  }
  
  bp_probs[, RRi_IHD:=sapply(bp_cat, addRR, RR=0.83)]
  bp_probs[, RRi_HHD:=sapply(bp_cat, addRR, RR=0.72)]
  bp_probs[, RRi_stroke:=sapply(bp_cat, addRR, RR=0.73)]
 
  
  ##add alphas##
  alphas<-bp_probs[,.(ihd=sum(prob_0*RRi_IHD), istroke=sum(prob_0*RRi_stroke), 
                  hstroke=sum(prob_0*RRi_stroke), hhd=sum(prob_0*RRi_HHD)), 
                  by=.(age, sex, location, intervention, Year)] #change to "Year"
  
  alphas<-melt(alphas, id.vars=c("age", "sex", "location", "intervention", "Year"), measure.vars=c(), variable.name = "cause",
       value.name="alpha")#change to "Year"
  
  
  rris<-bp_probs[,list(age, sex, Year, location, intervention, bp_cat, prob, RRi_IHD, RRi_HHD, RRi_stroke)]#change to "Year"
  rris[,hstroke:=RRi_stroke]
  
  setnames(rris, c("RRi_IHD", "RRi_HHD", "RRi_stroke"), c("ihd", "hhd","istroke"))
  rris<-melt(rris, id.vars=c("age", "sex", "location", "intervention", "bp_cat", "prob", "Year"), measure.vars=c(), variable.name = "cause",
               value.name="RRi")#change to "Year"
  
  bp_probs<-merge(rris, alphas, by=c("age", "sex", "location", "intervention","cause", "Year"))#change to "Year"
  setnames(bp_probs, "Year", "year")
  
  ####adding baseline_rates
  intervention_rates<-merge(bp_probs, base_rates, by=c("age", "sex", "location", "cause", "year"))

  #calculating yi*pi
  intervention_rates[, yixpi:=(RRi*IR/alpha)*prob]
  intervention_rates[, IR:=sum(yixpi), by=.(age, sex, location, intervention, cause, CF, 
                                            BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, year, ALL.mx)]#change to "Year"

  intervention_rates<-unique(intervention_rates[,-c("prob", "bp_cat", "yixpi", "RRi", "alpha")])
  
  ##add CF effects##
  #this is ugly code
  
  intervention_rates<-as.data.table(left_join(intervention_rates, inc%>%rename(year=Year), by=c("location","year")))
  
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="ihd" & age<80,     CF:=CF*(1-0.24*aroc2)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="istroke" & age<80, CF:=CF*(1-0.36*aroc2)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hstroke" & age<80, CF:=CF*(1-0.76*aroc2)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hhd" & age<80,     CF:=CF*(1-0.20*aroc2)]
  
  if(drugcov=="p75"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd" & age<80,     CF:=CF*(1-0.24*p_change2)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke" & age<80, CF:=CF*(1-0.36*p_change2)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke" & age<80, CF:=CF*(1-0.76*p_change2)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd" & age<80,     CF:=CF*(1-0.20*p_change2)]
  }
  
  if(drugcov=="p975"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd" & age<80,     CF:=CF*(1-0.24*a_change2)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke" & age<80, CF:=CF*(1-0.36*a_change2)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke" & age<80, CF:=CF*(1-0.76*a_change2)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd" & age<80,     CF:=CF*(1-0.20*a_change2)]
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


#test that it doesn't crash
test<-project.all("China","app", 0, 2027, "p975")
p<-test%>%group_by(year, intervention)%>%
  summarise(dead = sum(dead),
            sick=sum(sick))

ggplot(p, aes(x=year, y=dead, color=intervention))+
  geom_point()

ggplot(p, aes(x=year, y=sick, color=intervention))+
  geom_point()

#loop for all 182 countries
df<-project.all(countrylist[1],"app", 0, 2027, "p975")%>%group_by(year, intervention, location, age, cause)%>%
  summarise(dead = sum(dead), sick=sum(sick))

time1<-Sys.time()

for(i in countrylist[2:182]){
  temp<-project.all(countrylist[1],"app", 0, 2027, "p975")%>%group_by(year, intervention, location, age, cause)%>%
    summarise(dead = sum(dead), sick=sum(sick))
  df<-bind_rows(df, temp)
}

time2<-Sys.time()
time2-time1 #77 mins

write.csv(df, "htn_out.csv", row.names = F)


##deaths averted##

test<-df%>%select(-sick)%>%
  filter(cause!="hhd")%>%
  group_by(intervention)%>%
  summarise(dead = sum(dead))

((test$dead[2]-test$dead[1])/1e6)-(67.4139-60.83708)

(67.4139-60.83708)*(59.19009/67.41393)

59.19009-5.774512
59.19009-6.57682
