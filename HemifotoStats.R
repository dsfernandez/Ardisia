# estadisticas hemifotos Johann
library(ggplot2)
library(data.table)
library(car)
library(dplyr)
library(ggpubr)

# cargando datos
Hemifoto <- data.frame(Matriz_Hemifotos)
head(Hemifoto)

HemifotoDT <- data.table(Hemifoto)
HemifotoDT

## normalidad
# Vis.Sky
hist(Hemifoto$Vis.Sky,probability=T, 
     main="Histograma Hemifoto",xlab="Visible Sky (fraction)", 
     xlim = c(0,0.15), ylim = c(0,40), breaks = 10)
lines(density(Hemifoto$Vis.Sky),col=2)

ggqqplot(Hemifoto$Vis.Sky)

shapiro.test(Hemifoto$Vis.Sky)

# LAI
hist(Hemifoto$LAI,probability=T, 
     main="Histograma Hemifoto",xlab="LAI", 
     xlim = c(1,5), ylim = c(0,1.5), breaks = 10)
lines(density(Hemifoto$LAI),col=2)

ggqqplot(Hemifoto$LAI)

shapiro.test(Hemifoto$LAI)

## homocedasticidad
# Levene's test por la falta de normalidad (Ho: var1 = var2 = var3)
Hemifoto$FechaF = factor(Hemifoto$Fecha)
Hemifoto$AreaF = factor(Hemifoto$Area)
Hemifoto$PosiciónF = factor(Hemifoto$Posición)

# Vis.Sky
leveneTest(Vis.Sky ~ AreaF, data = Hemifoto)
leveneTest(Vis.Sky ~ PosiciónF, data = Hemifoto)
leveneTest(Vis.Sky ~ FechaF, data = Hemifoto)

# LAI
leveneTest(LAI ~ AreaF, data = Hemifoto)
leveneTest(LAI ~ PosiciónF, data = Hemifoto)
leveneTest(LAI ~ FechaF, data = Hemifoto)


## box-plot
# Vis.Sky
ggplot(Hemifoto, aes(x=Hemifoto$AreaF, y=Hemifoto$Vis.Sky, fill=Hemifoto$PosiciónF)) +
  geom_boxplot() + ylim(0.025, 0.11) + 
  labs(x="Localidad", y="Visible Sky", fill="Posición") +
  scale_fill_manual(label = c("1 m", "0.5 m"), values = c("yellow","red")) +
  facet_grid(.~Hemifoto$FechaF)

# LAI
ggplot(Hemifoto, aes(x=Hemifoto$AreaF, y=Hemifoto$LAI, fill=Hemifoto$PosiciónF)) +
  geom_boxplot() + ylim(2,4) + 
  labs(x="Localidad", y="Visible Sky", fill="Posición") +
  scale_fill_manual(label = c("1 m", "0.5 m"), values = c("yellow","red")) +
  facet_grid(.~Hemifoto$FechaF)

## ANOVA
#Vis.Sky
Anova(lm(Vis.Sky ~ Fecha * Area * Posición, data=HemifotoDT, type=2))
Anova(lm(Vis.Sky ~ Fecha * Area, data=HemifotoDT, type=2))
Anova(lm(Vis.Sky ~ Area, data=HemifotoDT, type=2))

#LAI
Anova(lm(LAI ~ Fecha * Area * Posición, data=HemifotoDT, type=2))
Anova(lm(LAI ~ Fecha * Area, data=HemifotoDT, type=2))
Anova(lm(LAI ~ Area, data=HemifotoDT, type=2))

## post-hoc
# Vis.Sky
a1 <- aov(Hemifoto$Vis.Sky ~ Hemifoto$AreaF + Hemifoto$Fecha)
posthoc <- TukeyHSD(x=a1, 'Hemifoto$AreaF', conf.level=0.95)
posthoc

#LAI
a1 <- aov(Hemifoto$LAI ~ Hemifoto$AreaF + Hemifoto$Fecha)
posthoc <- TukeyHSD(x=a1, 'Hemifoto$AreaF', conf.level=0.95)
posthoc

