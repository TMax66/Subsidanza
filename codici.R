library(readxl)
library(tidyverse)

dt <- read_excel("subsidenza.xlsx")  

#dt<-dt %>% 
#  mutate(diffvet=op1-op2)

dt<-dt %>% 
  pivot_longer(3:4, names_to = "Operatore", values_to = "grade") %>% 
  mutate(Operatore=factor(Operatore), 
         tecnica=factor(tecnica),
         id=factor(id))

summary(lme4::lmer(grade~tecnica+Operatore+(1|id), data=dt))




#####guarigione#####  

d<-tibble(treatment=c(rep("PD", 12),rep("DS",13)), 
          outcome=c(c(rep("M",5 ),rep("S",3), rep("P",4)),
                    c(rep("M",11),rep("S",1), rep("P",1))))
                    
library(nnet)         

m<-multinom(outcome~treatment, data=d)          

library(brms)
library(bayesplot)
fit <- brm(
  formula= outcome~treatment, data=d,
  family= categorical (link="logit"))

x<-posterior_samples(fit)
mcmc_intervals(x)
mcmc_areas(
 x, 
 pars = c("b_muP_Intercept", "b_muS_Intercept", "b_muP_treatmentPD",
          "b_muS_treatmentPD"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)


mcmc_areas_ridges(x)
