

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

## com dados faltantes
fitB.TCmiss<- gibbs_missing_data(y = y, Ft = Ft, Gt = Gt, nit = 5000, bn = 3000, thin = 1,
                                 v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)

#---------------------------------------------------------------
### Modelo bivariado preventing cross-over
Ft = matrix(c(1, 1, 0, 0,
              1, 0, 0, 0), nrow = 2, ncol = 4, byrow = T)
Gt = matrix(c(1,0,1,0,
              0,1,0,1,
              0,0,1,0,
              0,0,0,1), nrow = 4, ncol = 4, byrow = T)
w<-104
m0 = rep(0, nrow(Gt))
C0 = diag(100, nrow(Gt))
a= 0

### truncando
z= (a-m0[2])/sqrt(C0[2,2])
aux1= C0[,2]%*%solve(C0[2,2]) ; cst= (dnorm(z)/(1-pnorm(z)))
b0 = aux1%*%(sqrt(C0[2,2])*cst)

V.mu1= C0[2,2]%*%(1 + z*cst - (cst)^2)
M0= - aux1%*%C0[2,] + aux1%*%V.mu1%*%t(aux1)
C0= C0+M0
m0= m0+b0
m0;C0

source("ffbs-trunc.R")
### com dados faltantes
fitB.PCOmiss<- gibbs_missing_data_trunc(y = y, Ft = Ft, Gt = Gt, a=a, nit = 5000, bn = 3000, thin = 1,
                                        v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)



## com dados faltantes - bivariado com termo comum + prevent cross-over
Gt = matrix(c(1,0,1,0,0,
              0,1,0,1,0,
              0,0,1,0,0,
              0,0,0,1,0,
              1,1,0,0,1), nrow = 5, ncol = 5, byrow = T)
Ft = matrix(c(1, 1, 0, 0, 1,
              1, 0, 0, 0, 1), nrow = 2, ncol = 5, byrow = T)
m0 = rep(0, nrow(Gt))
C0 = diag(100, nrow(Gt))
a= 0

### truncando
z= (a-m0[2])/sqrt(C0[2,2])
aux1= C0[,2]%*%solve(C0[2,2]) ; cst= (dnorm(z)/(1-pnorm(z)))
b0 = aux1%*%(sqrt(C0[2,2])*cst)

V.mu1= C0[2,2]%*%(1 + z*cst - (cst)^2)
M0= - aux1%*%C0[2,] + aux1%*%V.mu1%*%t(aux1)
C0= C0+M0
m0= m0+b0
m0;C0

### com dados faltantes
fitB.TCPCOmiss<- gibbs_missing_data_trunc(y = y, Ft = Ft, Gt = Gt, a=a,nit = 5000, bn = 3000, thin = 1,
                                          v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)



### missing
qxBmiss.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.miss)[[1]]))
qxBmiss.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.miss)[[2]]))
qxBTCmiss.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCmiss)[[1]]))
qxBTCmiss.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCmiss)[[2]]))
qxBPCOmiss.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.PCOmiss)[[1]]))
qxBPCOmiss.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.PCOmiss)[[2]]))
qxBTCPCOmiss.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCPCOmiss)[[1]]))
qxBTCPCOmiss.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCPCOmiss)[[2]]))


### missing 
qxBmiss.all<- bind_rows(qxBmiss.m,qxBmiss.f, .id="id") 
qxBTCmiss.all<- bind_rows(qxBTCmiss.m,qxBTCmiss.f, .id="id") 
qxBPCOmiss.all<- bind_rows(qxBPCOmiss.m,qxBPCOmiss.f, .id="id") 
qxBTCPCOmiss.all<- bind_rows(qxBTCPCOmiss.m,qxBTCPCOmiss.f, .id="id") 


out<-bind_rows(qxBmiss.all,qxBTCmiss.all,qxBPCOmiss.all,qxBTCPCOmiss.all,
               .id="id1")
out = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 

out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)
