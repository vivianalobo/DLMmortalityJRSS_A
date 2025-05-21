
### Fig12f ; Fig12g

#### Scenario 43% of missing data
###### pooling with two populations
load("study3/outcen8s2.RData")

out. = out %>% 
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=60)

## crude mortality rate 
mx.<- mx %>% 
  filter(year==!!ytime) %>%
  dplyr::select(age,mx,sex) 
mx.<- mx. %>%
  mutate(sex = ifelse(sex == 1, 'male', 'female'))
mx.. = mx. %>% 
  filter(age <= 60)
mx..f<- mx. %>%
  filter(sex=="female", age<=60)

out..<- out. %>%   ### no missing
  filter(sex=="female", age<=60)

outAll. = outAll %>%   ### missing
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=60)

### filtrando modelo termo comum
out.2010femalePop1<- out. %>%
  filter(sex=="female", id1 == 2, age<=60)
out..<- out. %>%
  filter(sex=="female", id1==2, age<=60)
outAll.<- outAll %>%
  filter(sex=="female", id1==2, age<=60)
outAll.

### pooling with three populations
load("study3/outcenF_3tables.RData")

outAll. <- outAll %>%
  filter(sex=="female", id1==2, age<=60)

out.2010femalePop2<- out.missAll %>%
  filter(sex=="female", age <=60, id1==2)

teste <- bind_rows(out.2010femalePop1, out.2010femalePop2, 
                   .id="id2")
teste

# pdf("Fig12g.pdf",  width=10, height=6)
# ggplot(NULL,aes(x = 1:50)) + 
#   geom_point(data = filter(mx..f, (age %in% 1:45)), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
#   geom_point(data = filter(mx..f, !(age %in% 1:45)), aes(x = age, y = mx), color="tomato") +
#   geom_line(data=teste, aes(x=age, y = qx.fitted,color=sex)) + 
#   geom_line(data = outAll., aes(x = age, y = qx.fitted, linetype = "no missing"), color = "black", linewidth = 0.7) + 
#     geom_line(data=outAll., aes(x=age, y = qx.fitted), color=" black", linetype="dashed", linewidth=0.7) + 
#     geom_ribbon(data = teste, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
#   theme_classic(base_size = 20) + 
#   scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.005),
#                      trans = 'log10', labels = scales::comma) +
#   scale_x_continuous("Age", breaks = seq(0, 60, by = 10)) +
#  guides( fill = "none") +
#    scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male"), guide="none")+
#   scale_linetype_manual(name = "Legend", values = c("no missing" = "dashed")) +
#    theme(legend.position = c(0.88,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
#         legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
#         legend.text=element_text(color="black", size=16), 
#         axis.title = element_text(color = "black", size = 14), 
#         axis.text = element_text(color="black",size=14)) + 
#   facet_wrap(~id2, ncol=4, labeller = labeller(id2 = c("1" = "pooling with 2 populations",
#                                                        "2" = "pooling with 3 populations")))
# graphics.off()


pdf("Fig12g.pdf",  width=10, height=6)
ggplot(NULL,aes(x = 1:50)) + 
  geom_point(data = filter(mx..f, (age %in% 1:45)), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% 1:45)), aes(x = age, y = mx, color="female")) +
  geom_line(data=teste, aes(x=age, y = qx.fitted,color="female")) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color="no missing"), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = teste, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.005),
                     trans = 'log10', labels = scales::comma) +
  scale_x_continuous("Age", breaks = seq(0, 60, by = 10)) +
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","black"), labels=c("female (2010)", "no missing"))+
  scale_linetype_manual(name = "Legend", values = c("no missing" = "dashed")) +
  theme(legend.position = c(0.88,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14)) + 
  facet_wrap(~id2, ncol=4, labeller = labeller(id2 = c("1" = "pooling with 2 populations",
                                                       "2" = "pooling with 3 populations")))

graphics.off()


###########################################################################################33
#### Scenario 15% of missing data


### pooling with two populations
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
  filter(age <= 35)
mx..f<- mx. %>%
  filter(sex=="female", age<=35)

out..<- out. %>%   ### no missing
  filter(sex=="female", age<=35)

outAll. = outAll %>%   ### missing
  mutate(sex = ifelse(id == 1, 'male', 'female')) %>%
  filter(age <=35)

### filtrando modelo termo comum
out.2010femalePop1<- out. %>%
  filter(sex=="female", id1 == 2, age<=35)
out..<- out. %>%
  filter(sex=="female", id1==2, age<=35)
outAll.<- outAll %>%
  filter(sex=="female", id1==2, age<=35)



# ggplot(NULL, aes(x = 1:35)) +
#   geom_point(data = filter(mx..f, (age %in% c(3:16))), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
#   geom_point(data = filter(mx..f, !(age %in% c(3:16))), aes(x = age, y = mx), color="tomato") +
#   geom_line(data=out.2010femalePop1, aes(x=age, y = qx.fitted,color=sex)) +
# #  geom_line(data=outAll., aes(x=age, y = qx.fitted, color=sex), linetype="dashed", linewidth=0.7) +
#   geom_line(data=outAll., aes(x=age, y = qx.fitted), linetype="dashed", linewidth=0.7) +
# 
#     geom_ribbon(data = out.., aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) +
#   theme_classic(base_size = 20) +
#   scale_y_continuous(expression(q[x]), limits = c(0.00005, 0.0015),
#                      trans = 'log10', labels = scales::comma) +
#   scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) +
#   guides( fill = "none") +
#   scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male"), guide="none")+
#   #  scale_fill_manual(values=c("tomato","steelblue"), labels=c("female", "male")) +
#   theme(legend.position = c(0.95,0.2), strip.background=element_rect(colour="black", fill="gray87"),
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#         legend.title=element_blank(), legend.key.height = unit(.6, "cm") ,
#         legend.text=element_text(color="black", size=16),
#         axis.title = element_text(color = "black", size = 14),
#         axis.text = element_text(color="black",size=14))
# 
# ### 
### pooling with three popultions
load("study3/outcenD_3tables.RData")

outAll. <- outAll %>%
  filter(sex=="female", id1==2, age<=35)

out.2010femalePop2<- out.missAll %>%
  filter(sex=="female", age <=35, id1==2)


teste <- bind_rows(out.2010femalePop1, out.2010femalePop2, 
                   .id="id2")
teste

# pdf("Fig12f.pdf",  width=10, height=6)
# ggplot(NULL, aes(x = 1:35)) + 
#   geom_point(data = filter(mx..f, (age %in% c(3:16))), aes(x = age, y = mx), color="tomato", size=1.7, shape=1) +
#   geom_point(data = filter(mx..f, !(age %in% c(3:16))), aes(x = age, y = mx), color="tomato") +
#   geom_line(data=teste, aes(x=age, y = qx.fitted,color=sex)) + 
# #geom_line(data=outAll., aes(x=age, y = qx.fitted), linetype="no missing", linewidth=0.7) + 
#   geom_line(data = outAll., aes(x = age, y = qx.fitted, linetype = "no missing"), color = "black", linewidth = 0.7) + 
#   geom_line(data=outAll., aes(x=age, y = qx.fitted), color=" black", linetype="dashed", linewidth=0.7) + 
#   geom_ribbon(data = teste, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
#   theme_classic(base_size = 20) + 
#   scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0010), 
#                      trans = 'log10', labels = scales::comma) + 
#   scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
#   guides( fill = "none") +
#   scale_color_manual(values=c( "tomato","steelblue"), labels=c("female", "male"), guide="none")+
#  scale_linetype_manual(name = "Legend", values = c("no missing" = "dashed")) +
#   theme(legend.position = c(0.88,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
#         legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
#         legend.text=element_text(color="black", size=16), 
#         axis.title = element_text(color = "black", size = 14), 
#         axis.text = element_text(color="black",size=14)) + 
#      facet_wrap(~id2, ncol=4, labeller = labeller(id2 = c("1" = "pooling with 2 populations",
#                                                        "2" = "pooling with 3 populations")))
# graphics.off()


pdf("Fig12f.pdf",  width=10, height=6)
ggplot(NULL, aes(x = 1:35)) + 
  geom_point(data = filter(mx..f, (age %in% c(3:16))), aes(x = age, y = mx, color="female"), size=1.7, shape=1) +
  geom_point(data = filter(mx..f, !(age %in% c(3:16))), aes(x = age, y = mx, color="female")) +
  geom_line(data=teste, aes(x=age, y = qx.fitted,color="female")) + 
  geom_line(data=outAll., aes(x=age, y = qx.fitted, color="no missing"), linetype="dashed", linewidth=0.7) + 
  geom_ribbon(data = teste, aes(x = age, ymin = qx.lower, ymax = qx.upper, fill="tomato"), alpha = 0.25) + 
  theme_classic(base_size = 20) + 
  scale_y_continuous(expression(m[x]), limits = c(0.00005, 0.0010), 
                     trans = 'log10', labels = scales::comma) + 
  scale_x_continuous("Age", breaks = seq(0, 40, by = 10)) + 
  guides( fill = "none") +
  scale_color_manual(values=c( "tomato","black"), labels=c("female (2010)", "no missing"))+
  scale_linetype_manual(name = "Legend", values = c("no missing" = "dashed")) +
  theme(legend.position = c(0.88,0.2), strip.background=element_rect(colour="black", fill="gray87"), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), legend.key.height = unit(.6, "cm") , 
        legend.text=element_text(color="black", size=16), 
        axis.title = element_text(color = "black", size = 14), 
        axis.text = element_text(color="black",size=14)) + 
  facet_wrap(~id2, ncol=4, labeller = labeller(id2 = c("1" = "pooling with 2 populations",
                                                       "2" = "pooling with 3 populations")))
graphics.off()

