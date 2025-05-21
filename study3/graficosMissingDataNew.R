#----------------------------------------------------------
##### Plotando os resultados do ajustes com cenarios

## caso geral sem missing - model comparison
load("study3/outStudy3.RData")
## no missing 
qxB.all<- bind_rows(qxB.m,qxB.f, .id="id") 
qxBTC.all<- bind_rows(qxBTC.m,qxBTC.f, .id="id") 

## caso geral - no missing 
outAll<-bind_rows(qxB.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 

outAll. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)

pdf("Fig10aNew.pdf",width=10, height=6)
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
  theme(legend.position = c(0.88,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual",
                                                       "2" = "Common term")))
graphics.off()




####-------------------------------------------------------------------------
### cenario 1 (4-8 age female)
### 5% missing data

load("study3/outcen1s2.RData")

### vai precisar rodar o primeiro cenario sem o modelo de prevencao 
# para ficar o id1 ==1, e id1==2 somente


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


out<-bind_rows(qxBmiss.all,qxBTCmiss.all,
               .id="id1")
out = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)
out..<- out. %>%
  filter(sex=="female")

## caso geral - no missing 
outAll<-bind_rows(qxB.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
outAll.. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=='female', age <=35)

pdf("Fig11fNew.pdf", width=10, height=6)
ggplot(NULL, aes(x = 1:30)) +
  geom_point(data = filter(mx..f, (age %in% 4:8)), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 4:8)), aes(x = age, y = mx, color="female")) +
  geom_point(data = mx..m, aes(x = age, y = mx, color = "male"))+ #, linetype = "male")) +
  geom_line(data = out., aes(x = age, y = qx.fitted, color = sex)) +#, linetype = sex)) +
  geom_line(data = outAll.., aes(x = age, y = qx.fitted, color = "no missing"), linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper), fill = "tomato", alpha = 0.25, inherit.aes = FALSE) +
  
  theme_classic(base_size = 20) +
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0015), trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) +
  scale_color_manual(values = c("female" = "tomato", "male" = "steelblue", "no missing" = "black")) +
  scale_linetype_manual(values = c("female" = "solid", "male" = "solid", "no missing" = "dashed"), guide="none") +
  
  theme( legend.position = c(0.88, 0.2),
         strip.background = element_rect(colour = "black", fill = "gray87"),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.title = element_blank(),
         legend.key.height = unit(.6, "cm"),
         legend.text = element_text(color = "black", size = 16),
         axis.title = element_text(color = "black", size = 14),
         axis.text = element_text(color = "black", size = 14)) +
  facet_wrap(~id1, ncol = 4, labeller = labeller(id1 = c(
    "1" = "Usual",
    "2" = "Common term")))
graphics.off()


#-------------------------------------------------------------------------
### cenario 2 (4-10 anos e 15-17 anos)
### 10% missing data
load("study3/outScenario1.Rdata") ## missing
#load("study3/outStudy3.RData") ### no missing



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

out<-bind_rows(qxBmiss.all,qxBTCmiss.all,
               .id="id1")
out = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)
out..<- out. %>%
  filter(sex=="female")


## caso geral - no missing 
outAll<-bind_rows(qxB.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
outAll.. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=='female', age <=35)


pdf("Fig11aNew.pdf", width=10, height=6)
ggplot(NULL, aes(x = 1:30)) +
  # Pontos para o grupo feminino com diferentes tamanhos e cores
  geom_point(data = filter(mx..f, (age %in% c(4:10, 15:17))), aes(x = age, y = mx, color = "female"), size = 1.7, shape = 1) +
  geom_point(data = filter(mx..f, !(age %in% c(4:10, 15:17))), aes(x = age, y = mx, color = "female")) +
  # Pontos para o grupo masculino
  geom_point(data = mx..m, aes(x = age, y = mx, color = "male"))+ #, linetype = "male")) +
  # Linhas para modelos com `sex` no `aes` para unificar a legenda de cor e linetype
  geom_line(data = out., aes(x = age, y = qx.fitted, color = sex)) +#, linetype = sex)) +
  # Linha preta para a série completa (no missing)
  geom_line(data = outAll.., aes(x = age, y = qx.fitted, color = "no missing"), linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper), fill = "tomato", alpha = 0.25, inherit.aes = FALSE) +
  theme_classic(base_size = 20) +
  # Eixos e transformações
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0015), trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) +
  # Ajuste das cores e tipos de linha para consolidar legenda
  scale_color_manual(values = c("female" = "tomato", "male" = "steelblue", "no missing" = "black")) +
  scale_linetype_manual(values = c("female" = "solid", "male" = "solid", "no missing" = "dashed"), guide="none") +
  # Configuração da posição e estilo da legenda
  theme( legend.position = c(0.88, 0.2),
         strip.background = element_rect(colour = "black", fill = "gray87"),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.title = element_blank(),
         legend.key.height = unit(.6, "cm"),
         legend.text = element_text(color = "black", size = 16),
         axis.title = element_text(color = "black", size = 14),
         axis.text = element_text(color = "black", size = 14)) +
  facet_wrap(~id1, ncol = 4, labeller = labeller(id1 = c(
    "1" = "Usual",
    "2" = "Common term" )))
graphics.off()

#-------------------------------------------------
### cenario 3 - 3- 16 anos
### 15% missing data
load("study3/outScenario2.Rdata")

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

out<-bind_rows(qxBmiss.all,qxBTCmiss.all,
               .id="id1")
out = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)
out..<- out. %>%
  filter(sex=="female")


## caso geral - no missing 
outAll<-bind_rows(qxB.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
outAll.. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=='female', age <=35)


pdf("Fig11bNew.pdf", width=10, height=6)
ggplot(NULL, aes(x = 1:30)) +
  geom_point(data = filter(mx..f, (age %in% 3:16)), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 3:16)), aes(x = age, y = mx, color="female")) +
  geom_point(data = mx..m, aes(x = age, y = mx, color = "male"))+ #, linetype = "male")) +
  geom_line(data = out., aes(x = age, y = qx.fitted, color = sex)) +#, linetype = sex)) +
  geom_line(data = outAll.., aes(x = age, y = qx.fitted, color = "no missing"), linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper), fill = "tomato", alpha = 0.25, inherit.aes = FALSE) +
  
  theme_classic(base_size = 20) +
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0015), trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) +
  scale_color_manual(values = c("female" = "tomato", "male" = "steelblue", "no missing" = "black")) +
  scale_linetype_manual(values = c("female" = "solid", "male" = "solid", "no missing" = "dashed"), guide="none") +
  
  theme( legend.position = c(0.88, 0.18),
         strip.background = element_rect(colour = "black", fill = "gray87"),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.title = element_blank(),
         legend.key.height = unit(.6, "cm"),
         legend.text = element_text(color = "black", size = 16),
         axis.title = element_text(color = "black", size = 14),
         axis.text = element_text(color = "black", size = 14)) +
  facet_wrap(~id1, ncol = 4, labeller = labeller(id1 = c(
    "1" = "Usual",
    "2" = "Common term" )))
graphics.off()


###--------------------------------------------
### cenario 4 (1-15 age female)
### 25% missing data

load("study3/outcen7s2.RData")

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

out<-bind_rows(qxBmiss.all,qxBTCmiss.all,
               .id="id1")
out = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)
out..<- out. %>%
  filter(sex=="female")


## caso geral - no missing 
outAll<-bind_rows(qxB.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
outAll.. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=='female', age <=35)



pdf("Fig11iNew.pdf", width=10, height=6)
ggplot(NULL, aes(x = 1:30)) +
  geom_point(data = filter(mx..f, (age %in% 1:25)), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 1:25)), aes(x = age, y = mx, color="female")) +
  geom_point(data = mx..m, aes(x = age, y = mx, color = "male"))+ #, linetype = "male")) +
  geom_line(data = out., aes(x = age, y = qx.fitted, color = sex)) +#, linetype = sex)) +
  geom_line(data = outAll.., aes(x = age, y = qx.fitted, color = "no missing"), linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper), fill = "tomato", alpha = 0.25, inherit.aes = FALSE) +
  
  theme_classic(base_size = 20) +
  scale_y_continuous(expression(m[x]), limits = c(0.00001, 0.0015), trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) +
  scale_color_manual(values = c("female" = "tomato", "male" = "steelblue", "no missing" = "black")) +
  scale_linetype_manual(values = c("female" = "solid", "male" = "solid", "no missing" = "dashed"), guide="none") +
  
  theme( legend.position = c(0.88, 0.18),
         strip.background = element_rect(colour = "black", fill = "gray87"),
         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
         legend.title = element_blank(),
         legend.key.height = unit(.6, "cm"),
         legend.text = element_text(color = "black", size = 16),
         axis.title = element_text(color = "black", size = 14),
         axis.text = element_text(color = "black", size = 14)) +
  facet_wrap(~id1, ncol = 4, labeller = labeller(id1 = c(
    "1" = "Usual",
    "2" = "Common term")))
graphics.off()

###-------------------------------
### cenario 5 (1-16, 23-41 age female)
### 33% missing data

load("study3/outcen5s2.RData")

## crude mortality rate 
mx.<- mx %>% 
  filter(year==!!ytime) %>%
  dplyr::select(age,mx,sex) 
mx.<- mx. %>%
  mutate(sex = ifelse(sex == 1, 'male', 'female'))

mx.. = mx. %>% 
  filter(age <= 45)

mx..f<- mx. %>%
  filter(sex=="female", age<=45)
mx..m<- mx. %>%
  filter(sex=="male", age<45)

out<-bind_rows(qxBmiss.all,qxBTCmiss.all,
               .id="id1")
out = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=45)
out..<- out. %>%
  filter(sex=="female")


## caso geral - no missing 
outAll<-bind_rows(qxB.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
outAll.. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=='female', age <=45)


pdf("Fig11jNew.pdf", width=10, height=6)
ggplot(NULL,aes(x = 1:50)) + 
  geom_point(data = filter(mx..f, (age %in% c(1:16,23:41))), aes(x = age, y = mx, color = "female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(1:16,23:41))), aes(x = age, y = mx, color = "female")) +
  geom_point(data = mx..m, aes(x = age, y = mx, color = "male"))+ #, linetype = "male")) +
  geom_line(data = out., aes(x = age, y = qx.fitted, color = sex)) +#, linetype = sex)) +
  geom_line(data = outAll.., aes(x = age, y = qx.fitted, color = "no missing"), linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper), fill = "tomato", alpha = 0.25, inherit.aes = FALSE) +
  
  
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00003, 0.0020),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 50, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values = c("female" = "tomato", "male" = "steelblue", "no missing" = "black")) +
  scale_linetype_manual(values = c("female" = "solid", "male" = "solid", "no missing" = "dashed"), guide="none") +
  theme(legend.position = c(0.88,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual", 
                                                       "2" = "Common term")))
graphics.off()


#--------------------------------------------
### cenario 6 (1-45 age female)
### 45% missing data

load("study3/outcen8s2.RData")


## crude mortality rate 
mx.<- mx %>% 
  filter(year==!!ytime) %>%
  dplyr::select(age,mx,sex) 
mx.<- mx. %>%
  mutate(sex = ifelse(sex == 1, 'male', 'female'))

mx.. = mx. %>% 
  filter(age <= 55)
mx..f<- mx. %>%
  filter(sex=="female", age<=55)
mx..m<- mx. %>%
  filter(sex=="male", age<55)


out<-bind_rows(qxBmiss.all,qxBTCmiss.all,
               .id="id1")
out = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=55)
out..<- out. %>%
  filter(sex=="female")


## caso geral - no missing 
outAll<-bind_rows(qxB.all,qxBTC.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 
outAll.. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=='female', age <=55)


pdf("Fig11kNew.pdf", width=10, height=6)
ggplot(NULL,aes(x = 1:55)) + 
  geom_point(data = filter(mx..f, (age %in% c(1:45))), aes(x = age, y = mx, color = "female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(1:45))), aes(x = age, y = mx, color = "female")) +
  geom_point(data = mx..m, aes(x = age, y = mx, color = "male"))+ #, linetype = "male")) +
  geom_line(data = out., aes(x = age, y = qx.fitted, color = sex)) +#, linetype = sex)) +
  geom_line(data = outAll.., aes(x = age, y = qx.fitted, color = "no missing"), linetype = "dashed",linewidth = 0.7) +
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper), fill = "tomato", alpha = 0.25, inherit.aes = FALSE) +
  
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00002, 0.002),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 60, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values = c("female" = "tomato", "male" = "steelblue", "no missing" = "black")) +
  scale_linetype_manual(values = c("female" = "solid", "male" = "solid", "no missing" = "dashed"), guide="none") +
  theme(legend.position = c(0.88,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14))+ 
  facet_wrap(~id1, ncol=4, labeller = labeller(id1 = c("1" = "Usual", 
                                                       "2" = "Common term")))
graphics.off()


##------------------------------------------------------------
## cenario 7 -2-4 anos (masculino) ; 5-10 e 15-17 (feminino)

load("study3/outScenario3.Rdata")

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

out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)
out..<- out. %>%
  filter(sex=="female")
outAll.f = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=='female', age <=35)
outAll.m = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(sex=='male', age <=35)


pdf("Fig11c.pdf", width=17, height=6)
ggplot(NULL, aes(x = 1:30)) + 
  geom_point(data = filter(mx..f, age %in% 5:17), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..m, age %in% 1:4), aes(x = age, y = mx), color="steelblue", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 5:17)), aes(x = age, y = mx), color="tomato", size=1.5) +
  geom_point(data = filter(mx..m, !(age %in% 1:4)), aes(x = age, y = mx), color="steelblue", size=1.5) +
  geom_line(data = outAll.f, aes(x = age, y = qx.fitted, color = "no missing"), linetype = "dashed",linewidth = 0.7) +
  geom_line(data = outAll.m, aes(x = age, y = qx.fitted, color = "no missing"), linetype = "dashed",linewidth = 0.7) +
  
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_ribbon(data = out., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill=sex), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0015), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values = c("female" = "tomato", "male" = "steelblue", "no missing" = "black")) +
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  scale_linetype_manual(values = c("female" = "solid", "male" = "solid", "no missing" = "dashed"), guide="none") +
  
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


##------------------------------------------------------------
## cenario 8 

load("study3/outScenario4.Rdata")

out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age >=70)
out..<- out. %>%
  filter(sex=="male")

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
  # geom_point(data = mx.., aes(x = age, y = mx, color=sex), shape=1) +
  geom_point(data = filter(mx..m, age %in% 80:104), aes(x = age, y = mx), color="steelblue", size=1.7, shape=1) +
  geom_point(data = filter(mx..m, !(age %in% 80:104)), aes(x = age, y = mx), color="steelblue") +
  geom_point(data = mx..f, aes(x = age, y = mx), color="tomato") +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll.m, aes(x=age, y = qx.fitted), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.. , aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="steelblue"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = 10^-c(NA,NA), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(70, 110, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male"))+
  scale_fill_manual(values=c("steelblue","steelblue"), labels=c("female", "male")) + 
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
