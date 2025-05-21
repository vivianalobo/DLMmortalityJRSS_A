

library(dplyr)
library(ggplot2)
library(BayesMortalityPlus)
library(patchwork)
#library(tidyverse)
require(stringr)
library(prodlim)
library(purrr)
library(tidyr)
library(scales)
require(gridExtra)

grad <- seq(0,110,20)
grad[(grad/5)%%2 != 0] <- ""
point <- format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE)


#####==========#####
##### dataset  #####
#####==========#####
dx <- read.csv("deaths_MYB2.csv")
Ex <- read.csv("MYB2_mais_OLDAGE.csv")

dx.all = dx %>%
  dplyr::select(!X) %>% 
  pivot_longer(cols = starts_with("X"), values_to = "dx", names_to = "year") %>%
  mutate(year = substring(year, 2)) %>%
  filter(Age != '105+' & Age != '0') %>%
  mutate(age = as.numeric(Age)) %>%
  dplyr::select(age, sex, year, dx)

Ex.all = Ex %>%
  dplyr::select(!X) %>% 
  pivot_longer(cols = starts_with("population_"), values_to = "Ex", names_to = "year") %>%
  mutate(year = str_remove(year, "population_")) %>%
  filter(age != 105 & age !=0 )

mx = dx.all %>% 
  left_join(Ex.all, by = c("sex", "age", "year")) %>%
  mutate(mx = dx/Ex) %>%
  mutate(sex = as.character(sex))


### Usando ano de 2010 para construir cenarios
ytime=2010

### Dados do cenario 1
df.mx<- mx %>%
  mutate(log.mx=log(mx))

y.2010m<- df.mx %>%
  filter(year=='2010', sex=='1') %>%
  dplyr:: select(age,log.mx)

y.2010m<-y.2010m$log.mx
mx.m2010<- mx %>%
  filter(year=='2010', sex=='1') %>%
  dplyr::  select(age,mx)

fit1= dlm(y.2010m, delta = 0.75)
#fit2= dlm(y.2010m, delta = 0.90)
#fit3= dlm(y.m, delta = 0.9)
fit4= dlm(y.2010m, delta = 0.9999)
fit5= dlm(y.2010m, delta = rep(c(0.99, 0.85, 0.85, 0.99), c(5, 30, 50, 19)))




d1<- fitted(fit1)
#d2<- fitted(fit2)
#d3<- fitted(fit3)
d4<- fitted(fit4)
d5<- fitted(fit5)

qxall<- bind_rows(d1,d4,d5, .id="id")
qxall



mx.m2010. <- mx.m2010 %>%
  filter(age<=60)

qxall. = qxall %>% 
  filter(age <=60)



p1<-ggplot(NULL,aes(x = 0:60)) + 
  geom_point(data =mx.m2010., aes(x = age, y = mx), col = "steelblue") +
  geom_line(data=qxall., aes(x=age, y =  -log(1-qx.fitted), color = id),col = "steelblue") +
    theme_classic(base_size = 20) + 
  geom_ribbon(data = qxall., aes(x = age, ymin =  -log(1-qx.lower), ymax =  -log(1-qx.upper), fill="id"), alpha = 0.25) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.01),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 60, by = 10)) +  
  scale_fill_manual(values=c("steelblue","steelblue","steelblue","steelblue","steelblue"), labels=c("1", "2", "3", "4","5"), guide="none") + 
  
  theme(legend.position = c(0.05, 0.86),
        strip.background=element_rect(colour="black",
                                      fill="gray87"), panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title=element_blank(),
        legend.key.height = unit(.6, "cm") ,
        legend.text=element_text(color="black", size=16),
        axis.title = element_text(color = "black", size = 16),
        axis.text = element_text(color="black",size=16))+
  facet_wrap(~id, ncol=5, labeller = labeller(id = 
                                                c("1" = "0.75",
                                              
                                                   "2" = "0.999",
                                                    "3" = "different per age"
                                                  )))

p1

pdf("FigAgeVarying1.pdf", width=12, height=5)
p1
graphics.off()
