######################################################
#### Motivation missing data
#### With pooling and without pooling
#### considering scenario in study 3
### data: England + Wales, male and female, 2010
######################################################

### diretorio viviana
setwd("~/Dropbox/ProjetoMultivariadoMortality/Codes/ResultadosPaper/section2/subsection2.2")


## j=1 male, 2010 ; j=2 female, 2010 ;  j=3 female, 2012
source("auxfunc2.R")
#--------------------------------------------------------------------------

y_orig = y
### cenario c 3-16 
y[c(3:16),2] = NA
V = cov(y[-c(3:16),])
V
#load("outcenD_3tables.RData")
#load("study3/outcenD_3tables.RData")
load("section6/study3/output/outcenD_3tables.RData") 




source("dlm_missing.R")
source("ffbs_dlm_missing_alt.R")
fit.miss <- dlm(y[,2], delta = d12, ages = 1:104)   ### DLM missing fem 2010
plot(fit.miss)

qx.miss.2010 <- fitted(fit.miss) %>%
  mutate(qx.fitted = -log(1 - qx.fitted),
         qx.lower = -log(1 - qx.lower),
         qx.upper = -log(1 - qx.upper))

qxmiss.all <- bind_rows(qx.miss.2010, .id="id")

fit <- dlm(y_orig[,2], delta = d12, ages = 1:104)
plot(fit)

qx.2010 <- fitted(fit) %>%
  mutate(qx.fitted = -log(1 - qx.fitted),
         qx.lower = -log(1 - qx.lower),
         qx.upper = -log(1 - qx.upper))

qx.all <- bind_rows(qx.2010, .id="id")
qxBTC.all<- bind_rows(qxBTC.f2010, .id="id")

outAll<-bind_rows(qx.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = case_when(
    id == 1 ~ 'female'))

qxBTCmiss.all <- bind_rows(qxBTCmiss.f2010, .id="id")

out.missAll<-bind_rows(qxmiss.all,qxBTCmiss.all,
                  .id="id1")
out.missAll = out.missAll %>% 
  mutate(sex = case_when(
    id == 1 ~ 'female'))

out.missAll. <- out.missAll %>%
  filter(age <=35)

outAll. <- outAll %>%
  filter(age <=35)

mx.. = mxtot. %>% 
  filter(age <= 35)

mx..f<- mxtot. %>%
  filter(sex=="female", age<=35)
#mx..m<- mxtot. %>%
#  filter(sex=="male", age<=35)
#mx..f2<- mxtot. %>%
#  filter(sex=="female2", age<=35)

out..missf<- out.missAll %>%
  filter(sex=="female", age <=35)


## ler aqui!
outAll.f <- outAll %>%
  filter(sex=="female", age <=35)

pdf("Fig_study3_motiv_c.pdf",  width=10, height=6) 
ggplot(NULL, aes(x = 1:35)) + 
  #geom_point(data = filter(mx..f, (age %in% c(3:16))), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(3:16))), aes(x = age, y = mx, color="missing")) +#, color="tomato") +
  #geom_point(data = mx..m, aes(x = age, y = mx, color="male")) + #, color="steelblue") +
  #geom_point(data = mx..f2, aes(x = age, y = mx, color= "female2")) + #, color="orange") +
  geom_line(data = out.missAll., aes(x=age, y = qx.fitted, color = "missing")) +
  geom_line(data = outAll.f, aes(x = age, y = qx.fitted,color = "no missing" ),linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out..missf, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="missing"), alpha = 0.25) +
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), #limits = c(0.00005, 0.0015), 
                     trans = 'log10', labels = scales::comma) +
  coord_cartesian(ylim = c(c(0.00005, 0.0015))) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato", "black"), labels=c("missing", "no missing"))+
  scale_fill_manual(values=c("tomato", "black"), labels=c("missing", "no missing")) +
  scale_linetype_manual(values = c("missing" = "solid", "no missing" = "dashed"), guide="none") +
  
  theme(legend.position = c(0.88,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Without pooling",
                                                       "2" = "With pooling")))
graphics.off()

##################################################

## falta fazer cenario (a), 4-8 missing
d12 <- rep(c(0.9995, 0.80, 0.85, 0.99), c(5, 30, 50, 19))

y = y_orig
y[4:8,2] <- NA

source("dlm_missing.R")
#source("ffbs_dlm_missing.R")
source("ffbs_dlm_missing_alt.R")
ff.miss <- ff(m0 = rep(0, 2), C0 = diag(100, 2),y = y[,2],
              alpha0 = 0, beta0 = 0, Ft = matrix(c(1,0), nrow = 1),
              Gt = matrix(c(1,0,1,1), 2), delta = d12)
plot(ff.miss$m[,1], type = "l", lwd = 2)
ff.miss$C[24,,]; ff.miss$C[25,,]; ff.miss$C[27,,]

bs.miss <- bs(m = ff.miss$m, C = ff.miss$C,
              a = ff.miss$a, R = ff.miss$R,
              Gt = matrix(c(1,0,1,1), 2),
              alpha = ff.miss$alpha, beta = ff.miss$beta)

lines(bs.miss$as[,1], col = 2, lwd = 2)

fit.miss <- dlm(y[,2], delta = d12, ages = 1:104)   ### DLM missing fem 2010
plot(fit.miss)

qx.miss.2010 <- fitted(fit.miss) %>%
  mutate(qx.fitted = -log(1 - qx.fitted),
         qx.lower = -log(1 - qx.lower),
         qx.upper = -log(1 - qx.upper))

qxmiss.all <- bind_rows(qx.miss.2010, .id="id")

###vou reaproveitar o fit sem missing la de cima, tanto pro univar qto pro BTC


## com dados faltantes - bivariado com termo comum
Ft = matrix(c(1, 0, 0, 0, 0, 0, 1,
              0, 1, 0, 0, 0, 0, 1, 
              0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 7, byrow = T)
Gt = matrix(c(1, 0, 0, 1, 0, 0, 0,
              0, 1, 0, 0, 1, 0, 0, 
              0, 0, 1, 0, 0, 1, 0, 
              0, 0, 0, 1, 0, 0, 0, 
              0, 0, 0, 0, 1, 0, 0,
              0, 0, 0, 0, 0, 1, 0,
              1, 1, 1, 0, 0, 0, 1), nrow = 7, ncol = 7, byrow = T)
m0 = rep(0, nrow(Gt))
C0 = diag(100, nrow(Gt))
s0 <- diag(3)*0.01  ###priori do petris vaga
v0 <- 3
V = cov(y[-c(4:8),])

## com dados faltantes
source("ffbs_-_bivar.R")
source('gibbs_missing_data.R')
fitB.TCmiss<- gibbs_missing_data(y = y, Ft = Ft, Gt = Gt, nit = 5000, bn = 3000, thin = 1,
                                 v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)

qxBTCmiss.f2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCmiss)[[2]]))

qxBTCmiss.all<- bind_rows(qxBTCmiss.f2010, .id="id")

outAll<-bind_rows(qx.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = case_when(
    id == 1 ~ 'female'))

out.missAll<-bind_rows(qxmiss.all,qxBTCmiss.all,
                       .id="id1")
out.missAll = out.missAll %>% 
  mutate(sex = case_when(
    id == 1 ~ 'female'))

out.missAll. <- out.missAll %>%
  filter(age <=35)

outAll. <- outAll %>%
  filter(age <=35)

mx.. = mxtot. %>% 
  filter(age <= 35)

mx..f<- mxtot. %>%
  filter(sex=="female", age<=35)
#mx..m<- mxtot. %>%
#  filter(sex=="male", age<=35)
#mx..f2<- mxtot. %>%
#  filter(sex=="female2", age<=35)

out..missf<- out.missAll %>%
  filter(sex=="female", age <=35)


## ler aqui!
outAll.f <- outAll %>%
  filter(sex=="female", age <=35)


pdf("Fig_study3_motiv_a.pdf",  width=10, height=6) 
ggplot(NULL, aes(x = 1:35)) + 
  #geom_point(data = filter(mx..f, (age %in% c(3:16))), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(4:8))), aes(x = age, y = mx, color="missing")) +#, color="tomato") +
  #geom_point(data = mx..m, aes(x = age, y = mx, color="male")) + #, color="steelblue") +
  #geom_point(data = mx..f2, aes(x = age, y = mx, color= "female2")) + #, color="orange") +
  geom_line(data = out.missAll., aes(x=age, y = qx.fitted, color = "missing")) +
  geom_line(data = outAll.f, aes(x = age, y = qx.fitted,color = "no missing" ),linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out..missf, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="missing"), alpha = 0.25) +
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), #limits = c(0.00005, 0.0015), 
                     trans = 'log10', labels = scales::comma) +
  coord_cartesian(ylim = c(c(0.00005, 0.0015))) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato", "black"), labels=c("missing", "no missing"))+
  scale_fill_manual(values=c("tomato", "black"), labels=c("missing", "no missing")) +
  scale_linetype_manual(values = c("missing" = "solid", "no missing" = "dashed"), guide="none") +
  
  theme(legend.position = c(0.88,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Without pooling",
                                                       "2" = "With pooling")))
graphics.off()
