setwd("~/Dropbox/Semestre 2024.1 UFRJ/Projeto Multivariado (viviana)")

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
df.mx<- mx %>% mutate(log.mx=log(mx))
y.m<- df.mx %>%
  filter(year==!!ytime, sex=='1') %>%
  select(log.mx) 
y.f<- df.mx %>% 
  filter(year==!!ytime, sex=='2') %>% 
  select(log.mx) 
y<- as.matrix(cbind(y.m,y.f)) 
yt= y


#----------------------------------------------
qx_fitted = function(fit){
  n = dim(fit$mu)[1]
  # samples = fit$mu[seq(bn+1, n, by = thin),,]
  # V.samples = fit$V[seq(bn+1, n, by = thin),,]
  samples = fit$mu
  V.samples = fit$V
  fitted <- array(NA, dim = dim(samples))
  
  for(i in 1:dim(samples)[1]){
    for(t in 1:dim(samples)[2])
      fitted[i,t,] = exp(MASS::mvrnorm(1, mu = samples[i,t,], Sigma = V.samples[i,,]))
  }
  
  qx_fitted = apply(fitted, 2:3, quantile, probs = c(0.5, 0.025, 0.975))
  qx_fitted_m = t(qx_fitted[,,1])
  qx_fitted_f = t(qx_fitted[,,2])
  
  colnames(qx_fitted_m) = colnames(qx_fitted_f) = c("qx.fitted", "qx.lower", "qx.upper")
  
  return(list(qx_fitted_m, qx_fitted_f))
  
}