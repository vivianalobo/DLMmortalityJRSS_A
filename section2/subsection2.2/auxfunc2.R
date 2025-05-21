
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


### Usando ano de 2010 e 2011 para construir cenarios
df.mx<- mx %>% mutate(log.mx=log(mx))
y.m2010<- df.mx %>%
  filter(year==!!2010, sex=='1') %>%
  select(log.mx) 
y.f2010<- df.mx %>% 
  filter(year==!!2010, sex=='2') %>% 
  select(log.mx) 
y.f2011<- df.mx %>% 
  filter(year==!!2011, sex=='2') %>% 
  select(log.mx) 
y.f2012<- df.mx %>% 
  filter(year==!!2012, sex=='2') %>% 
  select(log.mx) 
y<- as.matrix(cbind(y.m2010,y.f2010, y.f2012)) 
yt= y


#----------------------------------------------
qx_fitted = function(fit){
  n = dim(fit$mu)[1]
  samples = fit$mu
  V.samples = fit$V
  fitted <- array(NA, dim = dim(samples))
  
  for(i in 1:dim(samples)[1]){
    for(t in 1:dim(samples)[2])
      fitted[i,t,] = exp(MASS::mvrnorm(1, mu = samples[i,t,], Sigma = V.samples[i,,]))
  }
  
  qx_fitted = apply(fitted, 2:3, quantile, probs = c(0.5, 0.025, 0.975))
  qx_fitted_m2010 = t(qx_fitted[,,1])
  qx_fitted_f2010 = t(qx_fitted[,,2])
  qx_fitted_f2012 = t(qx_fitted[,,3])
  
  colnames(qx_fitted_m2010) = colnames(qx_fitted_f2010) =  colnames(qx_fitted_f2012)= c("qx.fitted", "qx.lower", "qx.upper")
  
  return(list(qx_fitted_m2010, qx_fitted_f2010,qx_fitted_f2012))
  
}



