######################################################
#### output: missing data modelling 
#### Missing Data - scenario - 3 populacoes
### data: England + Wales, male and female, 2010
######################################################

## j=1 male, 2010 ; j=2 female, 2010 ;  j=3 female, 2012
source("auxfunc2.R")
source("ffbs_-_bivar.R")
source('gibbs_missing_data.R')
#--------------------------------------------------------------------------

### cenario c 3-16 
y[c(3:16),2] = NA
V = cov(y[-c(3:16),])
V
load("study3/outcenD_3tables.RData")
#save.image("study3/outcenD_3tables.RData")


### cenario f do artigo (1-45 anos female 2010) ~ 43%
y[c(1:45),2] = NA
V = cov(y[-c(1:45),])
load("study3/outcenF_3tables.RData")
#save.image("study3/outcenF_3tables.RData")


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------


### Modelos estudados
### bivariado usual

### modelo bivariado usual
Ft = matrix(c(1, 0, 0, 0, 0, 0,
              0, 1, 0, 0, 0, 0,
              0, 0, 1, 0, 0, 0), nrow = 3, ncol = 6, byrow = T)
Gt = matrix(c(1, 0, 0, 1, 0, 0,
              0, 1, 0, 0, 1, 0,
              0, 0, 1, 0, 0, 1,
              0, 0, 0, 1, 0, 0,
              0, 0, 0, 0, 1, 0,
              0, 0, 0, 0, 0, 1), nrow = 6, ncol = 6, byrow = T)

m0 = rep(0, nrow(Gt))
C0 = diag(100, nrow(Gt))
s0 <- diag(3)*0.01  ###priori do petris vaga
v0 <- 3

d12 <- rep(c(0.9995, 0.80, 0.85, 0.99), c(5, 30, 50, 19)) ## Ate 104

############# considerando male 2010 e female 2012 pooling para female 2010
# ### sem dados faltantes
fitB<- gibbsV_corrigido(y = yt, delta = d12, m0 = m0, C0 = C0,
                        Ft0 = Ft, Gt = Gt, nit = 5000, v0 = v0, s0 = s0, V = V)
### com dados faltantes
fitB.miss<- gibbs_missing_data(y = y, Ft = Ft, Gt = Gt, nit = 5000, bn = 3000, thin = 1,
                               v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)

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

## sem dados faltantes
fitB.TC<- gibbsV_corrigido(y = yt, delta = d12, m0 = m0, C0 = C0,
                           Ft0 = Ft, Gt = Gt, nit = 5000, v0 = v0, s0 = s0, V = V)

## com dados faltantes
fitB.TCmiss<- gibbs_missing_data(y = y, Ft = Ft, Gt = Gt, nit = 5000, bn = 3000, thin = 1,
                                 v0 = v0, s0 = s0, m0 = m0, C0 = C0, delta = d12, V = V)


## no missing
w=104
qxB.m2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[1]]))
qxB.f2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[2]]))
qxB.f2012 = data.frame(age=1:(w), rbind(qx_fitted(fitB)[[3]]))

qxBTC.m2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[1]]))
qxBTC.f2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[2]]))
qxBTC.f2012 = data.frame(age=1:(w), rbind(qx_fitted(fitB.TC)[[3]]))

qxB.all<- bind_rows(qxB.m2010,qxB.f2010, qxB.f2012,.id="id")
qxBTC.all<- bind_rows(qxBTC.m2010,qxBTC.f2010,qxBTC.f2012, .id="id")


## caso geral - no missing 
outAll<-bind_rows(qxB.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = case_when(
    id == 1 ~ 'male',
    id == 2 ~ 'female',
    id == 3 ~ 'female2'))

## crude mortality rate 
mx2010 <- mx %>%
  filter(year == 2010) %>%
  select(age, mx, sex) %>%
  mutate(sex = case_when(sex == 1 ~ "male",sex == 2 ~ "female"))

mx2012 <- mx %>%
  filter(year == 2012 & sex == 2) %>%
  select(age, mx, sex) %>%
  mutate(sex = case_when(sex == 2 ~ 'female2'))

mxtot. <- bind_rows(mx2010, mx2012)

pdf("Fig12a.pdf",  width=10, height=6)
ggplot(NULL, aes(x = 1:w)) + 
  geom_point(data = mxtot., aes(x = age, y = mx, color=sex)) +
  geom_line(data=outAll, aes(x=age, y = qx.fitted, color = sex)) + 
  geom_ribbon(data = outAll, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato", "orange","steelblue"), labels=c("female (2010)", "female (2012)","male (2010)"))+
  scale_fill_manual(values=c("tomato","orange","steelblue"), labels=c("female (2010)","female (2012)", "male (2010)")) +
  theme(legend.position = c(0.88,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual",
                                                       "2" = "Common term")))
graphics.off()

### missing
qxBmiss.m2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB.miss)[[1]]))
qxBmiss.f2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB.miss)[[2]]))
qxBmiss.f2012 = data.frame(age=1:(w), rbind(qx_fitted(fitB.miss)[[3]]))


qxBTCmiss.m2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCmiss)[[1]]))
qxBTCmiss.f2010 = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCmiss)[[2]]))
qxBTCmiss.f2012 = data.frame(age=1:(w), rbind(qx_fitted(fitB.TCmiss)[[3]]))

# ### missing 
qxBmiss.all<- bind_rows(qxBmiss.m2010,qxBmiss.f2010,qxBmiss.f2012, .id="id")
qxBTCmiss.all<- bind_rows(qxBTCmiss.m2010,qxBTCmiss.f2010,qxBTCmiss.f2012, .id="id")
 

out.missAll<-bind_rows(qxBmiss.all,qxBTCmiss.all,
                  .id="id1")
out.missAll = out.missAll %>% 
  mutate(sex = case_when(
    id == 1 ~ 'male',
    id == 2 ~ 'female',
    id == 3 ~ 'female2'
  ))

out.missAll. <- out.missAll %>%
  filter(age <=35)

outAll. <- outAll %>%
  filter(age <=35)

mx.. = mxtot. %>% 
  filter(age <= 35)

mx..f<- mxtot. %>%
  filter(sex=="female", age<=35)
mx..m<- mxtot. %>%
  filter(sex=="male", age<=35)
mx..f2<- mxtot. %>%
  filter(sex=="female2", age<=35)

out..missf<- out.missAll %>%
  filter(sex=="female", age <=35)

# pdf("Fig12b.pdf",  width=10, height=6) 
# ggplot(NULL, aes(x = 1:35)) + 
# geom_point(data = filter(mx..f, (age %in% c(3:16))), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
#  geom_point(data = filter(mx..f, !(age %in% c(3:16))), aes(x = age, y = mx), color="tomato") +
# geom_point(data = mx..m, aes(x = age, y = mx), color="steelblue") +
# geom_point(data = mx..f2, aes(x = age, y = mx), color="orange") +
#  geom_line(data=out.missAll., aes(x=age, y = qx.fitted, color = sex)) +
#    geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) +
#  geom_ribbon(data = out..missf, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) +
#   theme_classic(base_size = 20) + 
#     scale_y_continuous(expression(q[x]), limits = c(0.00005, 0.0015), 
#                        trans = 'log10', labels = scales::comma) + 
#     scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
#   guides( fill = "none") +
#   scale_color_manual(values=c( "tomato", "orange","steelblue"), labels=c("female (2010)", "female (2012)","male (2010)"))+
#   scale_fill_manual(values=c("tomato","orange","steelblue"), labels=c("female (2010)","female (2012)", "male (2010)")) +
#    theme(legend.position = c(0.88,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
#         legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
#         legend.text=element_text(color="black", size=16), 
#         axis.title = element_text(color = "black", size = 14), 
#         axis.text = element_text(color="black",size=14))+ 
#   facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual",
#                                                        "2" = "Common term")))
# graphics.off()

## ler aqui!
outAll.f <- outAll %>%
  filter(sex=="female", age <=35)

pdf("Fig12b.pdf",  width=10, height=6) 
ggplot(NULL, aes(x = 1:35)) + 
  geom_point(data = filter(mx..f, (age %in% c(3:16))), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(3:16))), aes(x = age, y = mx, color="female")) +#, color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="male")) + #, color="steelblue") +
  geom_point(data = mx..f2, aes(x = age, y = mx, color= "female2")) + #, color="orange") +
  geom_line(data=out.missAll., aes(x=age, y = qx.fitted, color = sex)) +
  geom_line(data = outAll.f, aes(x = age, y = qx.fitted,color = "no missing" ),linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out..missf, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) +
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0015), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato", "orange","steelblue", "black"), labels=c("female (2010)", "female (2012)","male (2010)", "no missing"))+
  scale_fill_manual(values=c("tomato","orange","steelblue", "black"), labels=c("female (2010)","female (2012)", "male (2010)", "no missing")) +
  scale_linetype_manual(values = c("female (2010)" = "solid", "female (2012)" = "solid", "male (2010)" = "solid", "no missing" = "dashed"), guide="none") +
  
  theme(legend.position = c(0.88,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual",
                                                       "2" = "Common term")))
graphics.off()
##################################################
### cenario mais extremo 1-45 anos (female, 2010)


out.missAll. <- out.missAll %>%
  filter(age <=60)

outAll. <- outAll %>%
  filter(age <=60)

mx.. = mxtot. %>% 
  filter(age <= 60)

mx..f<- mxtot. %>%
  filter(sex=="female", age<=60)
mx..m<- mxtot. %>%
  filter(sex=="male", age<=60)
mx..f2<- mxtot. %>%
  filter(sex=="female2", age<=60)

out..missf<- out.missAll %>%
  filter(sex=="female", age <=60)



# pdf("Fig12c.pdf",  width=10, height=6)
# ggplot(NULL, aes(x = 1:60)) + 
#   geom_point(data = filter(mx..f, (age %in% c(1:45))), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
#   geom_point(data = filter(mx..f, !(age %in% c(1:45))), aes(x = age, y = mx), color="tomato") +
#   geom_point(data = mx..m, aes(x = age, y = mx), color="steelblue") +
#   geom_point(data = mx..f2, aes(x = age, y = mx), color="orange") +
#   geom_line(data=out.missAll., aes(x=age, y = qx.fitted, color = sex)) +
#   geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) +
#   geom_ribbon(data = out..missf, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) +
#   theme_classic(base_size = 20) + 
#   scale_y_continuous(expression(q[x]), limits = c(0.00001, 0.015), 
#                      trans = 'log10', labels = scales::comma) + 
#   scale_x_continuous("Age", breaks = seq(0, 60, by = 10)) + 
#   guides( fill = "none") +
#   scale_color_manual(values=c( "tomato", "orange","steelblue"), labels=c("female (2010)", "female (2012)","male (2010)"))+
#   scale_fill_manual(values=c("tomato","orange","steelblue"), labels=c("female (2010)","female (2012)", "male (2010)")) + 
#     theme(legend.position = c(0.88,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
#         legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
#         legend.text=element_text(color="black", size=16), 
#         axis.title = element_text(color = "black", size = 14), 
#         axis.text = element_text(color="black",size=14))+ 
# facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual",
#                                                      "2" = "Common term")))
# 
# graphics.off()

#### rodar aqui
outAll.f <- outAll %>%
  filter(sex=="female", age <=60)



pdf("Fig12c.pdf",  width=10, height=6)
ggplot(NULL, aes(x = 1:60)) + 
  geom_point(data = filter(mx..f, (age %in% c(1:45))), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(1:45))), aes(x = age, y = mx, color="female")) +
  geom_point(data = mx..m, aes(x = age, y = mx, color="male")) + #, color="steelblue") +
  geom_point(data = mx..f2, aes(x = age, y = mx, color="female2"))+ #, color="orange") +
  geom_line(data=out.missAll., aes(x=age, y = qx.fitted, color = sex)) +
  geom_line(data=outAll.f, aes(x=age, y = qx.fitted, color = "no missing"), linetype="dashed", linewidth=0.7) +
  geom_ribbon(data = out..missf, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) +
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00001, 0.015), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 60, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato", "orange","steelblue", "black"), labels=c("female (2010)", "female (2012)","male (2010)", "no missing"))+
  scale_fill_manual(values=c("tomato","orange","steelblue", "black"), labels=c("female (2010)","female (2012)", "male (2010)", "no missing")) + 
  #scale_linetype_manual(values = c("female (2010)" = "solid", "female (2012)" = "solid", "male (2010)" = "solid", "no missing" = "dashed"), guide="none") +
  
  theme(legend.position = c(0.88,0.18), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual",
                                                       "2" = "Common term")))
graphics.off()
