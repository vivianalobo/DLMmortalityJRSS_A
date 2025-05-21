#################################################################################
#### output: exploraratory analysis
### data: England + Wales, male and female, 2010-2012
### authors: Luiz Figueiredo, Viviana Lobo, Mariane Alves and Thais Fonseca
#################################################################################



library(dplyr)
library(ggplot2)
library(BayesMortalityPlus)
library(patchwork)
library(tidyverse)
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


### analise exploratoria dos dados
pdf("section2/Fig1a.pdf", width=12, height=5)
ggplot(mx, aes(age, y = log(mx), col = sex)) + 
  geom_point() + 
  theme_classic(base_size = 15) + 
  scale_y_continuous(expression("log mortality rate"), limits = -c(10,0),labels = point)+
  scale_x_continuous("Age", breaks = seq(0, 110, by = 20), labels = grad) +
  scale_color_manual(name="sex",
                     labels=c("male","female"),
                     values=c("steelblue","tomato"))+
  theme(legend.position = c(0.07,0.87), strip.background=element_rect(colour="black",
                                                                      fill="gray87"), panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title=element_blank(),
        legend.text=element_text(size=rel(1)),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color="black",size=14))+
  facet_wrap(~year)
graphics.off()


## zoom
mx..<- mx %>%
  filter(age >=80)
pdf("section2/Fig1b.pdf", width=12, height=5)
ggplot(mx.., aes(age, y = log(mx), col = sex)) + 
  geom_point() + 
  theme_classic(base_size = 15) + 
  scale_y_continuous(expression("log mortality rate"), limits = -c(5,0),labels = point)+
  scale_x_continuous("Age", breaks = seq(0, 110, by = 10))+ #, labels = grad) +
  scale_color_manual(name="sex",
                     labels=c("male","female"),
                     values=c("steelblue","tomato"), guide="none")+
  theme(legend.position = c(0.87,0.07), strip.background=element_rect(colour="black",
                                                                      fill="gray87"), panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title=element_blank(),
        legend.text=element_text(size=rel(1)),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color="black",size=14))+
  facet_wrap(~year)
graphics.off()



mx..<- mx %>%
  filter(age <=30)
pdf("section2/Fig1c.pdf", width=12, height=5)
ggplot(mx.., aes(age, y = log(mx), col = sex)) + 
  geom_point() + 
  theme_classic(base_size = 15) + 
  scale_y_continuous(expression("log mortality rate"), limits = -c(10.5,6),labels = point)+
  scale_x_continuous("Age", breaks = seq(0, 30, by = 10))+ #, labels = grad) +
  scale_color_manual(name="sex",
                     labels=c("male","female"),
                     values=c("steelblue","tomato"), guide="none")+
  theme(legend.position = c(0.87,0.07), strip.background=element_rect(colour="black",
                                                                      fill="gray87"), panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.title=element_blank(),
        legend.text=element_text(size=rel(1)),
        axis.title = element_text(color = "black", size = 14),
        axis.text = element_text(color="black",size=14))+
  facet_wrap(~year)
graphics.off()

