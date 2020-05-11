library(readxl)
library(tidyverse)
library(nnet)   
library(brms)
library(bayesplot)
library(sjPlot)
library(lme4)
library(broom)
library(afex)
library(patchwork)
library(ggstatsplot)
library(margins)
library(ggeffects)

dati <- read_excel("subsidenza.xlsx")  


dt<-dati %>% 
  pivot_longer(3:4, names_to = "Observer", values_to = "Subsidence") %>% 
  mutate(Observer=factor(Observer), 
         Treatment=factor(tecnica, levels = c("PD","DS")),
         IDdog=factor(id)) %>% 
  select(-tecnica, -id) %>% 
  data.frame()

# dt %>% 
#   group_by(tecnica) %>% 
#   summarise(m=mean(grade))
  

mod<-lmer(Subsidence~Treatment+Observer+(1|IDdog), data=dt)
mod<-mixed(Subsidence~Treatment+Observer+(1|IDdog), data=dt)

anova(mod)
###scomposizione varianza=518+127+2.63+13.77###

tab_model(mod, show.intercept = FALSE, file="sub.html")

tidy(mod)

p<-plot_model(mod, type="re", colors = c("steelblue2", "steelblue2"))
p2<-plot_model(mod, type="est", colors=c("steelblue2", "steelblue2"))

p+p2


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
          outcome=c(c(rep("Improved",5 ),rep("Stable",3), rep("Worse",4)),
                    c(rep("Improved",11),rep("Stable",1), rep("Worse",1))))

####Guarigione a >30giorni< 12 mesi###
d90<-tibble(treatment=c(rep("PD", 12),rep("DS",13)), 
            outcome=c(c(rep("Improved",3),rep("Stable",6), rep("Worse",3)),
                      c(rep("Improved",8),rep("Stable",2), rep("Worse",3))))

  
####Guarigione a >12 mesi###
d12m<-tibble(treatment=c(rep("PD", 12),rep("DS",13)), 
            outcome=c(c(rep("Improved",4),rep("Stable",5), rep("Worse",3)),
                      c(rep("Improved",8),rep("Stable",4), rep("Worse",1))))




    
# ref. improv
m30<-multinom(outcome~treatment, data=d30) 
m90<-multinom(outcome~treatment, data=d90)
m12m<-multinom(outcome~treatment, data=d12m) 



###ref worse###
d30$outcome<-relevel(as.factor(d30$outcome), ref = "Worse")
d90$outcome<-relevel(as.factor(d90$outcome), ref = "Worse")
d12m$outcome<-relevel(as.factor(d12m$outcome), ref = "Worse")

m30<-multinom(outcome~treatment, data=d30)
m90<-multinom(outcome~treatment, data=d90) 
m12m<-multinom(outcome~treatment, data=d12m) 

#grafici
eff<-ggemmeans(m12m, "treatment")
plot(eff)+labs(y="95%CI Outcome Probability", x="Treatment", title="")







  tab_model(m30,emph.p=FALSE, show.intercept = FALSE, file="30dw.html")
  tab_model(m90,emph.p=FALSE, show.intercept = FALSE, file="90dw.html")
  tab_model(m12m,emph.p=FALSE, show.intercept = FALSE, file="m12dw.html")

  # 
# 
# 
# 
# 
# p<-plot_model(m)
# 
# library(ggstance)
# tt <- broom::tidy(m,conf.int=TRUE)
# tt <- dplyr::filter(tt, term!="(Intercept)")
# ggplot(tt, aes(x=estimate,y=term,colour=y.level))+
#   geom_pointrangeh(aes(xmin=conf.low,
#                        xmax=conf.high),
#                    position=position_dodgev(height=0.75))
# 
# library(MASS)
# utils::example(topic = birthwt, echo = FALSE)
# 
# # model
# bwt.mu <-
#   nnet::multinom(
#     formula = low ~ .,
#     data = bwt,
#     trace = FALSE
#   )






####Guarigione a 90 giorni###
d90<-tibble(treatment=c(rep("PD", 12),rep("DS",13)), 
          outcome=c(c(rep("M",4 ),rep("S",5), rep("P",3)),
                    c(rep("M",8),rep("S",4), rep("P",1))))

library(nnet)         

m<-multinom(outcome~treatment, data=d90)      


####bayes model####
fit <- brm(
  formula= outcome~treatment, data=d30,
  family= categorical (link="logit"))
# 
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

# 
# mcmc_areas_ridges(x)
######################################