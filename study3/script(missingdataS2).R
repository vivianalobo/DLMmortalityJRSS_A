######################################################
#### output: missing data modelling 
#### Missing Data - percentual de censura
### data: England + Wales, male and female, 2010
######################################################

### Percentual de censura - evoluindo

## caso geral NO MISSING - model comparison
load("study3/outStudy3.RData")
## no missing 
qxB.all<- bind_rows(qxB.m,qxB.f, .id="id") 
qxBTC.all<- bind_rows(qxBTC.m,qxBTC.f, .id="id") 
qxBPCO.all<- bind_rows(qxBPCO.m,qxBPCO.f, .id="id") 
qxBTCPCO.all<- bind_rows(qxBTCPCO.m,qxBTCPCO.f, .id="id") 

outAll<-bind_rows(qxB.all,qxBTC.all,qxBPCO.all,qxBTCPCO.all,
                  .id="id1")
outAll = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) 

outAll. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)

#pdf("Fig10a.pdf",width=17, height=6)
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
#graphics.off()

#-------------------------------------------------------
### leitura dos dados
source("auxfunc.R")

#--------------------------
###  5/104 = 0.0480
y[c(4:8),2] = NA
V = cov(y[-c(4:8),])

#save.image("study3/outcen1s2.RData")
load("study3/outcen1s2.RData")

out..<- out %>%
  filter(sex=="female")

mx..f<- mx. %>%
  filter(sex=="female")

mx..f<- mx. %>%
  filter(sex=="female", age<=30)
mx..m<- mx. %>%
  filter(sex=="male", age<=30)


pdf("Fig11f.pdf", width=17, height=6)
ggplot(NULL,aes(x = 1:30)) + 
  geom_point(data = filter(mx..f, (age %in% 4:8)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 4:8)), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
   geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 

  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0015),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male")) +#, guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.95,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
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

#----------------------------
### percentual de missing 
### 11/104 = 0.105
y[c(3:13),2] = NA
V = cov(y[-c(3:13),])
#save.image("study3/outcen2s2.RData")
load("study3/outcen2s2.RData")

out..<- out %>%
  filter(sex=="female")

mx..f<- mx. %>%
  filter(sex=="female")

mx..f<- mx. %>%
  filter(sex=="female", age<=30)
mx..m<- mx. %>%
  filter(sex=="male", age<=30)

pdf("Fig14b.pdf", width=17, height=6)
ggplot(NULL,aes(x = 1:w)) + 
  # geom_point(data = mx.., aes(x = age, y = mx, color=sex)) +
  geom_point(data = filter(mx..f, (age %in% 3:13)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 3:13)), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
  
    geom_line(data=out, aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll, aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) + 
  # scale_y_continuous(expression(q[x]), limits = c(0.00005, 0.0015), 
  #                    trans = 'log10', labels = scales::comma) + 
  # scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male")) +#, guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.95,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
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

#------
## 20/104 =0.20
y[c(5:25),2] = NA
V = cov(y[-c(5:25),])
#save.image("study3/outcen3s2.RData")
load("study3/outcen3s2.RData")

out..<- out %>%
  filter(sex=="female")

mx..f<- mx. %>%
  filter(sex=="female")

out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)

outAll. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)

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


pdf("Fig11e.pdf", width=17, height=6)
ggplot(NULL,aes(x = 1:30)) + 
  # geom_point(data = mx.., aes(x = age, y = mx, color=sex)) +
  geom_point(data = filter(mx..f, (age %in% 5:25)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 5:25)), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
   geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  # scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA), 
  #                    trans = 'log10', labels = scales::comma) + 
  # scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) + 
  scale_y_continuous(expression(q[x]), limits = c(0.00005, 0.0015),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male")) +#, guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.95,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
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

#----------------------------------------------------
#### --
### cenario 3:35 anos (feminino) ~ %
y[c(5:35),2] = NA
V = cov(y[-c(5:35),])

source("source(allmodelsMissing.R")

#save.image("study3/outcen4s2.RData")
load("study3/outcen4s2.RData")


out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=45)


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
  filter(sex=="male", age<=45)

# qxB.m. = qxB.m %>%
#   filter(age<=30)
# qxB.f. = qxB.f %>%
#   filter(age<=30)
# 
out..<- out. %>%
  filter(sex=="female")



#pdf("Fig14a.pdf", width=17, height=6)
pdf("Fig11g.pdf", width=17, height=6)
ggplot(NULL,aes(x = 1:45)) + 
  # geom_point(data = mx.., aes(x = age, y = mx, color=sex)) +
  geom_point(data = filter(mx..f, (age %in% 5:35)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 5:35)), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
    geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
    geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  # scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA),
  #                    trans = 'log10', labels = scales::comma) +
  # scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) +
  scale_y_continuous(expression(q[x]), limits = c(0.00005, 0.0015),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 50, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male")) +#, guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.95,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
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





#----------------------------------------------------
#### --
### cenario 
y[c(1:16,23:41),2] = NA
V = cov(y[-c(1:16,23:41),])

source("source(allmodelsMissing).R")

#save.image("study3/outcen5s2.RData")
load("study3/outcen5s2.RData")

out..<- out %>%
  filter(sex=="female")


#pdf("Fig14a.pdf", width=17, height=6)
pdf("Fig11j.pdf", width=17, height=6)
ggplot(NULL,aes(x = 1:50)) + 
  geom_point(data = filter(mx..f, (age %in% c(1:16,23:41))), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(1:16,23:41))), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  # scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA),
  #                    trans = 'log10', labels = scales::comma) +
  # scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) +
  scale_y_continuous(expression(q[x]), limits = c(0.00003, 0.0020),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 50, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male")) +#, guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.93,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
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




### -----

## cenario 1:25 injusto esse cenario.


y[c(3:45),2] = NA
V = cov(y[-c(3:45),])

source("source(allmodelsMissing).R")

#save.image("study3/outcen6s2.RData")
load("study3/outcen6s2.RData")


out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=55)


## crude mortality rate 
mx.<- mx %>% 
  filter(year==!!ytime) %>%
  dplyr::select(age,mx,sex) 
mx.<- mx. %>%
  mutate(sex = ifelse(sex == 1, 'male', 'female'))

mx.. = mx. %>% 
  filter(age <= 50)

mx..f<- mx. %>%
  filter(sex=="female", age<=50)
mx..m<- mx. %>%
  filter(sex=="male", age<=50)


out..<- out. %>%
  filter(sex=="female")

outAll. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=55)


#pdf("Fig14a.pdf", width=17, height=6)
pdf("Fig11h.pdf", width=17, height=6)
ggplot(NULL,aes(x = 1:50)) + 
  # geom_point(data = mx.., aes(x = age, y = mx, color=sex)) +
  geom_point(data = filter(mx..f, (age %in% 3:45)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 3:45)), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  # scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA),
  #                    trans = 'log10', labels = scales::comma) +
  # scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) +
  scale_y_continuous(expression(q[x]), limits = c(0.00004, 0.002),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 50, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male")) +#, guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.95,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
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




#-------------
y[c(1:25),2] = NA
V = cov(y[-c(1:25),])

source("source(allmodelsMissing).R")

#save.image("study3/outcen7s2.RData")
load("study3/outcen7s2.RData")

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


out..<- out. %>%
  filter(sex=="female")

outAll. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)


pdf("Fig11i.pdf", width=17, height=6)
ggplot(NULL,aes(x = 1:30)) + 
  # geom_point(data = mx.., aes(x = age, y = mx, color=sex)) +
  geom_point(data = filter(mx..f, (age %in% 1:25)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 1:25)), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  # scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA),
  #                    trans = 'log10', labels = scales::comma) +
  # scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) +
  scale_y_continuous(expression(q[x]), limits = c(0.00001, 0.0015),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male")) +#, guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.95,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
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


#-------------
y[c(1:45),2] = NA
V = cov(y[-c(1:45),])

source("source(allmodelsMissing).R")

#save.image("study3/outcen8s2.RData")
load("study3/outcen8s2.RData")



out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=55)


## crude mortality rate 
mx.<- mx %>% 
  filter(year==!!ytime) %>%
  dplyr::select(age,mx,sex) 
mx.<- mx. %>%
  mutate(sex = ifelse(sex == 1, 'male', 'female'))

mx.. = mx. %>% 
  filter(age <= 50)

mx..f<- mx. %>%
  filter(sex=="female", age<=50)
mx..m<- mx. %>%
  filter(sex=="male", age<=50)


out..<- out. %>%
  filter(sex=="female")

outAll. = outAll %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=55)


#pdf("Fig14a.pdf", width=17, height=6)
pdf("Fig11k.pdf", width=17, height=6)
ggplot(NULL,aes(x = 1:50)) + 
  # geom_point(data = mx.., aes(x = age, y = mx, color=sex)) +
  geom_point(data = filter(mx..f, (age %in% 1:45)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 1:45)), aes(x = age, y = mx), color="tomato") +
  geom_point(data = mx..m, aes(x = age, y = mx, color="sex"), color="steelblue") +
  geom_line(data=out., aes(x=age, y = qx.fitted, color = sex)) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color = sex), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  # scale_y_continuous(expression(q[x]), limits = 10^-c(NA,NA),
  #                    trans = 'log10', labels = scales::comma) +
  # scale_x_continuous("Age", breaks = seq(0, 120, by = 20)) +
  scale_y_continuous(expression(q[x]), limits = c(0.00002, 0.002),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 50, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male")) +#, guide="none")+
  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) + 
  theme(legend.position = c(0.95,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
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


