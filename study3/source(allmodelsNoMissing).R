

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
### sem dados faltantes
fitB.PCO<- gibbsV_corrigido_trunc(y = y, Ft = Ft, Gt = Gt,a=a, nit=5000 , bn=3000, thin=1, v0 = v0, s0 = s0,
                                  V = cov(y), m0 = m0, C0 = C0, delta = d12)



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

# ### sem dados faltantes
fitB.TCPCO<- gibbsV_corrigido_trunc(y = y, Ft = Ft, Gt = Gt,a=a, nit=5000 , bn=3000, thin=1, v0 = v0, s0 = s0,
                                    V = cov(y), m0 = m0, C0 = C0, delta = d12)



#--------------------------------------
#### ajustes

## no missing
qxB.m = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[1]]))
qxB.f = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[2]]))
qxBPCO.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.PCO)[[1]]))
qxBPCO.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.PCO)[[2]]))
qxBTC.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[1]]))
qxBTC.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[2]]))
qxBTCPCO.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCPCO)[[1]]))
qxBTCPCO.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCPCO)[[2]]))


outAll<-bind_rows(qxB.all,qxBTC.all,qxBPCO.all,qxBTCPCO.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 

outAll. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)

