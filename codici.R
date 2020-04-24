library(readxl)
library(tidyverse)
library(nnet)   
library(brms)
library(bayesplot)
library(sjPlot)
library(lme4)
library(VCA)

dati <- read_excel("subsidenza.xlsx")  


dt<-dati %>% 
  pivot_longer(3:4, names_to = "Operatore", values_to = "grade") %>% 
  mutate(Operatore=factor(Operatore), 
         tecnica=factor(tecnica, levels = c("PD","DS")),
         id=factor(id)) %>% 
  data.frame()

dt %>% 
  group_by(tecnica) %>% 
  summarise(m=mean(grade))
  

mod<-lmer(grade~tecnica+Operatore+(1|id), data=dt)
anova(mod)
###scomposizione varianza=518+127+2.63+13.77###


p<-plot_model(mod, type="re", colors = c("blue", "blue"))

pp<-p[["data"]]

dati %>%
  full_join(pp, by = c("id" = "term")) %>% 
  mutate(dogs= factor(id)) %>% 
  ggplot(aes(x = estimate, y=dogs, label=p.label))+
  geom_point(aes(color=tecnica), size=2.8)+ scale_color_manual(values=c("blue", "red"))+
  geom_segment(aes(x = conf.low, xend = conf.high, y=dogs, yend=dogs,
                   color=tecnica))+theme_sjplot()+
  vline_0(color="grey3")+labs(title="random effect", x="diff", y="")+
  geom_text(vjust=-1)

tab_model(mod, file="m.html")


plot_model(mod, type = "resid")

#####Bayes mixed model###
# library(rstanarm)
# 
# bmod <- stan_lmer(formula = grade~tecnica+Operatore+(1|id),
#                          data = dt,
#                          seed = 349)
##############################

#####Guarigione  a 30 gg#####  

d30<-tibble(treatment=c(rep("PD", 12),rep("DS",13)), 
          outcome=c(c(rep("M",5 ),rep("S",3), rep("P",4)),
                    c(rep("M",11),rep("S",1), rep("P",1))))

write.table(d30, file="d30.csv")
                    
library(nnet)         

m<-multinom(outcome~treatment, data=d30)          


####Guarigione a 90 giorni###
d90<-tibble(treatment=c(rep("PD", 12),rep("DS",13)), 
          outcome=c(c(rep("M",4 ),rep("S",5), rep("P",3)),
                    c(rep("M",8),rep("S",4), rep("P",1))))

library(nnet)         

m<-multinom(outcome~treatment, data=d90)      


####bayes model####
# fit <- brm(
#   formula= outcome~treatment, data=d,
#   family= categorical (link="logit"))
# 
# x<-posterior_samples(fit)
# mcmc_intervals(x)
# mcmc_areas(
#  x, 
#  pars = c("b_muP_Intercept", "b_muS_Intercept", "b_muP_treatmentPD",
#           "b_muS_treatmentPD"),
#   prob = 0.8, # 80% intervals
#   prob_outer = 0.99, # 99%
#   point_est = "mean"
# )
# 
# 
# mcmc_areas_ridges(x)
######################################