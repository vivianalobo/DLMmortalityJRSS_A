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

# ### sem dados faltantes
fitB.TCPCO<- gibbsV_corrigido_trunc(y = y, Ft = Ft, Gt = Gt,a=a, nit=5000 , bn=3000, thin=1, v0 = v0, s0 = s0,
                                    V = cov(y), m0 = m0, C0 = C0, delta = d12)


### com dados faltantes
fitB.TCPCOmiss<- gibbs_missing_data_trunc(y = y, Ft = Ft, Gt = Gt, a=a,nit = 5000, bn = 3000, thin = 1,
                                 v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)


#--------------------------------------
#### ajustes

## no missing
# qxB.m = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[1]]))
# qxB.f = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[2]]))
# qxBPCO.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.PCO)[[1]]))
# qxBPCO.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.PCO)[[2]]))
# qxBTC.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[1]]))
# qxBTC.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[2]]))
# qxBTCPCO.m = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCPCO)[[1]]))
# qxBTCPCO.f = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCPCO)[[2]]))


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
#----------------------------------------------------------
##### Plotando os resultados do ajustes com cenarios
 
 ## caso geral sem missing - model comparison
 
 load("study3/outStudy3.RData")
 ## no missing 
 qxB.all<- bind_rows(qxB.m,qxB.f, .id="id") 
 qxBTC.all<- bind_rows(qxBTC.m,qxBTC.f, .id="id") 
 qxBPCO.all<- bind_rows(qxBPCO.m,qxBPCO.f, .id="id") 
 qxBTCPCO.all<- bind_rows(qxBTCPCO.m,qxBTCPCO.f, .id="id") 
 
 ## caso geral - no missing 
 outAll<-bind_rows(qxB.all,qxBTC.all,qxBPCO.all,qxBTCPCO.all,
                   .id="id1")
 outAll = outAll %>% 
   mutate(sex = ifelse(id == 1, 'male', 'female')) 
 
 outAll. = outAll %>% 
   mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
   filter(age <=35)
 
pdf("Fig10a.pdf",width=17, height=6)
 ggplot(NULL, aes(x = 1:w)) + 
   geom_point(data = mx., aes(x = age, y = mx, color=sex)) +
   geom_line(data=outAll, aes(x=age, y = qx.fitted, color = sex)) + 
   geom_ribbon(data = outAll, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) + 
   theme_classic(base_size = 20) + 
   scale_y_continuous(expression(m[x]), limits = 10^-c(NA,NA), 
                      trans = 'log10', labels = scales::comma) + 
   scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) + 
   guides( fill = "none") +
   scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male"))+
   scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
   theme(legend.position = c(0.93,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
         panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
         legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
         legend.text=element_text(color="black", size=16), 
         axis.title = element_text(color = "black", size = 14), 
         axis.text = element_text(color="black",size=14))+ 
   facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual",
                                                        "2" = "Common term", 
                                                        "3" = "Prevent cross-over",
                                                        "4" = "Common term + PCO")))
graphics.off()
 
#----------------------------------------------------------
### cenario 1   - 4-10 anos e 15-17 anos
load("study3/outScenario1.Rdata")

load("study3/outStudy3.RData")


pdf("Fig11a.pdf", width=17, height=6)
ggplot(NULL, aes(x = 1:30)) + 
 # geom_point(data = mx.., aes(x = age, y = mx, color=sex)) +
  geom_point(data = filter(mx..f, (age %in% c(4:10, 15:17))), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(4:10, 15:17))), aes(x = age, y = mx), color="tomato") +
    geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
   geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0015), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male"), guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.93,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual", 
                                                       "2" = "Common term", 
                                                       "3" = "Prevent cross-over",
                                                       "4" = "Common term + PCO")))
graphics.off()
#-------------------------------------------------
### cenario 2 - 3- 16 anos
load("study3/outScenario2.Rdata")

pdf("Fig11b.pdf", width=17, height=6)
ggplot(NULL, aes(x = 1:30)) + 
  # geom_point(data = mx.., aes(x = age, y = mx, color=sex)) +
  geom_point(data = filter(mx..f, (age %in% 3:16)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 3:16)), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
#  geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0015), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male"))+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.93,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual", 
                                                       "2" = "Common term", 
                                                       "3" = "Prevent cross-over",
                                                       "4" = "Common term + PCO")))
graphics.off()

#-------------------------------------------------
### cenario 3 - priori vaga. -2-4 anos (masculino) ; 5-10 e 15-17 (feminino)
load("study3/outScenario3.Rdata")



outAll.f <- outAll %>%
  filter(sex=="female", age <=35)

outAll.m <- outAll %>%
  filter(sex=="male", age <=35)

pdf("Fig11c.pdf", width=17, height=6)
ggplot(NULL, aes(x = 1:30)) + 
  geom_point(data = filter(mx..f, age %in% 5:17), aes(x = age, y = mx,color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..m, age %in% 1:4), aes(x = age, y = mx, color="male"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 5:17)), aes(x = age, y = mx, color="female"), size=1.5) +
  geom_point(data = filter(mx..m, !(age %in% 1:4)), aes(x = age, y = mx, color="male"), size=1.5) +
  geom_line(data=outAll.f, aes(x=age, y = qx.fitted, color = "no missing"), linetype="dashed", linewidth=0.7) + 
  geom_line(data=outAll.m, aes(x=age, y = qx.fitted, color = "no missing"), linetype="dashed", linewidth=0.7) + 
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_ribbon(data = out., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(q[x]), limits = c(0.00005, 0.0015), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue", "black"), labels=c("female", "male", "no missing"))+
  scale_fill_manual(values=c("tomato","steelblue", "black"), labels=c("female", "male", "no missing")) + 
  theme(legend.position = c(0.93,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual", 
                                                       "2" = "Common term", 
                                                       "3" = "Prevent cross-over",
                                                       "4" = "Common term + PCO")))
graphics.off()
# pdf("Fig11c.pdf", width=17, height=6)
# ggplot(NULL, aes(x = 1:30)) + 
# #  geom_point(data = mx., aes(x = age, y = mx, color=sex), shape=1) +
#   geom_point(data = filter(mx..f, age %in% 5:17), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
#   geom_point(data = filter(mx..m, age %in% 1:4), aes(x = age, y = mx), color="steelblue", size=1.7, shape=1) +
#   geom_point(data = filter(mx..f, !(age %in% 5:17)), aes(x = age, y = mx), color="tomato", size=1.5) +
#   geom_point(data = filter(mx..m, !(age %in% 1:4)), aes(x = age, y = mx), color="steelblue", size=1.5) +
#   geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
#     geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
#   geom_ribbon(data = out., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) + 
#   theme_classic(base_size = 20) + 
#   scale_y_continuous(expression(q[x]), limits = c(0.00005, 0.0015), 
#                      trans = 'log10', labels = scales::comma) + 
#   scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
#   guides( fill = "none") +
#   scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male"))+
#   scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
#   theme(legend.position = c(0.93,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
#         legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
#         legend.text=element_text(color="black", size=16), 
#         axis.title = element_text(color = "black", size = 14), 
#         axis.text = element_text(color="black",size=14))+ 
#   facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual", 
#                                                        "2" = "Common term", 
#                                                        "3" = "Prevent cross-over",
#                                                        "4" = "Common term + PCO")))
# graphics.off()


### cenario 4
load("study3/outScenario4.Rdata")
out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age >=70)
out..<- out. %>%
  filter(sex=="male")

outAll. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age >=70)

outAll.m = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=="male",age >=70)

mx.. = mx. %>% 
  filter(age >= 70)

mx..f<- mx. %>%
  filter(sex=="female", age>=70)
mx..m<- mx. %>%
  filter(sex=="male", age>=70)

pdf("Fig11d.pdf", width=17, height=6)
ggplot(NULL, aes(x = 70:104)) + 
  geom_point(data = filter(mx..m, age %in% 80:104), aes(x = age, y = mx, color="male"), size=1.7, shape=1) +
  geom_point(data = filter(mx..m, !(age %in% 80:104)), aes(x = age, y = mx, color="male")) +
  geom_point(data = mx..f, aes(x = age, y = mx, color="female")) +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll.m, aes(x=age, y = qx.fitted, color = "no missing"), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.. , aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="steelblue"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(70, 110, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue", "black"), labels=c("female", "male", "no missing"))+
  scale_fill_manual(values=c("steelblue","steelblue", "black"), labels=c("female", "male", "no missing")) + 
  theme(legend.position = c(0.93,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual", 
                                                       "2" = "Common term", 
                                                       "3" = "Prevent cross-over",
                                                       "4" = "Common term + PCO")))

graphics.off()

