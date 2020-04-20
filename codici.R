library(readxl)
library(tidyverse)

dt <- read_excel("subsidenza.xlsx")  

dt %>% 
  pivot_longer(3:4, names_to = "Operatore", values_to = "grade")


#####guarigione#####  

d<-tibble(treatment=c(rep("PD",3), rep("DS",3)),outcome=rep(c("M","S","P"),2),
          freq=c(5,3,4,11,1,1))
