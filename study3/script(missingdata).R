######################################################
#### output: missing data modelling 
#### Missing Data - scenario
### data: England + Wales, male and female, 2010
######################################################

source("auxfunc.R")
V=cov(y)
#--------------------------------------------------------------------------

##### rodando todos os dados (comparadando modelos: usual, PCross-Over, Commom term)
#save.image("study3/outStudy3.RData")
load("study3/outStudy3.RData")    ### a partir da linha 46 deste script tem 
## os ajustes dos dados, sem missing

#### Criando cenarios com dados faltantes
### cenario 1 : dado faltante em feminino (inicio da tabua)
y[c(4:10,15:17),2] = NA
V = cov(y[-c(4:10,15:17),])

load("study3/outScenario1.RData")

### cenario 2: dado faltante em feminino (inicio da tabua, mais extremo)
y[c(3:16),2]= NA
V = cov(y[-c(3:16),])

#load("study3/outScenario2.RData")

## cenario 3 : dado faltante em masculino e feminino (inicio da tabua)
y[c(5:17),2] = NA
y[c(1:4), 1] = NA
V = cov(y[-c(1:4, 5:17),])

#load("study3/outScenario3.RData")

## cenario 4: dado faltante em masculino (final da tabua)
y[c(80:104),1] = NA
V= cov(y[-c(80:104),])

#load("study3/outScenario4.RData")

## cenario 5 : percentual de missing variando
### no cenario 5, ver script "script(missingdataS2).R"


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
### Modelos estudados
### bivariado usual
source("ffbs_-_bivar.R")
source('gibbs_missing_data.R')
### modelo bivariado usual
Ft = matrix(c(1, 0, 0, 0,
              0, 1, 0, 0), nrow = 2, ncol = 4, byrow = T)
Gt = matrix(c(1,0,1,0,
              0,1,0,1,
              0,0,1,0,
              0,0,0,1), nrow = 4, ncol = 4, byrow = T)

m0 = rep(0, nrow(Gt))
C0 = diag(100, nrow(Gt))
s0 <- diag(2)*0.01  ###priori do petris vaga
v0 <- 3

d12 <- rep(c(0.99, 0.85, 0.99), c(5, 85, 14)) ## Ate 104
# ### sem dados faltantes
fitB<- gibbsV_corrigido(y = yt, delta = d12, m0 = m0, C0 = C0,
                            Ft0 = Ft, Gt = Gt, nit = 5000, v0 = v0, s0 = s0, V = V)
### com dados faltantes
fitB.miss<- gibbs_missing_data(y = y, Ft = Ft, Gt = Gt, nit = 5000, bn = 3000, thin = 1,
                               v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)

## com dados faltantes - bivariado com termo comum
Gt = matrix(c(1,0,1,0,0,
              0,1,0,1,0,
              0,0,1,0,0,
              0,0,0,1,0,
              1,1,0,0,1), nrow = 5, ncol = 5, byrow = T)
Ft = matrix(c(1, 0, 0, 0, 1,
              0, 1, 0, 0, 1), nrow = 2, ncol = 5, byrow = T)
m0 = rep(0, nrow(Gt))
C0 = diag(100, nrow(Gt))

## sem dados faltantes
fitB.TC<- gibbsV_corrigido(y = yt, delta = d12, m0 = m0, C0 = C0,
                           Ft0 = Ft, Gt = Gt, nit = 5000, v0 = v0, s0 = s0, V = V)

## com dados faltantes
fitB.TCmiss<- gibbs_missing_data(y = y, Ft = Ft, Gt = Gt, nit = 5000, bn = 3000, thin = 1,
                               v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)


#--------------------------------------
#### ajustes

## no missing
# qxB.m = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[1]]))
# qxB.f = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[2]]))
# qxBTC.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[1]]))
# qxBTC.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[2]]))



### missing
qxBmiss.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.miss)[[1]]))
qxBmiss.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.miss)[[2]]))
qxBTCmiss.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCmiss)[[1]]))
qxBTCmiss.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCmiss)[[2]]))

qxBmiss.all<- bind_rows(qxBmiss.m,qxBmiss.f, .id="id") 
qxBTCmiss.all<- bind_rows(qxBTCmiss.m,qxBTCmiss.f, .id="id") 


out<-bind_rows(qxBmiss.all,qxBTCmiss.all,
               .id="id1")
out = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 

 out. = out %>% 
   mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
   filter(age <=35)
 
 
 ## crude mortality rate 
 mx.<- mx %>% 
   filter(year==!!ytime) %>%
   dplyr::select(age,mx,sex) 
 mx.<- mx. %>%
   mutate(sex = ifelse(sex == 1, 'male', 'female'))
 
 mx.. = mx. %>% 
     filter(age <= 30)
 
 mx..f<- mx. %>%
   filter(sex=="female", age<=30)
 mx..m<- mx. %>%
   filter(sex=="male", age<=30)
 
 # qxB.m. = qxB.m %>%
 #   filter(age<=30)
 # qxB.f. = qxB.f %>%
 #   filter(age<=30)
 # 
 out..<- out. %>%
   filter(sex=="female")
 
 #save.image("study3/outScenario1.Rdata")
 
 #save.image("study3/outScenario2.Rdata")
 
 #save.image("study3/outScenario3.Rdata")
 
 #save.image("study3/outScenario4.Rdata")

