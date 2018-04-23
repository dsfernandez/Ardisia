# estadisticas herbivoria Johann
library(ggplot2)
library(data.table)
library(car)
library(dplyr)
library(ggpubr)

# dataframe
herb <- data.frame(Herbivory)
head(herb)

# data.table
herbDT <- data.table(herb)
herbDT

# histograma y normalidad
hist(herb$Herbiv,probability=T, 
     main="Histograma Herbivoría",xlab="% Herbivoría", 
     xlim = c(0,10), ylim = c(0,1), breaks = 40)
lines(density(herb$Herbiv),col=2)

ggqqplot(herb$Herbiv)

shapiro.test(herb$Herbiv)

# homocedasticidad
# Levene's test por la falta de normalidad (Ho: var1 = var2 = var3)
herb$AreaF = factor(herb$Area)
herb$PosicF = factor(herb$Posic)

leveneTest(Herbiv ~ AreaF, data = herb)
leveneTest(Herbiv ~ PosicF, data = herb)
leveneTest(Herbiv ~ FechaF, data = herb)

# factores para analisis o keys
setkey(herbDT, Fecha, Area, Posic)

# estadísticas descriptivas con data.table
herbDT[, list(Mean = mean(Herbiv), SD = sd(Herbiv)), by = list(Fecha, Area, Posic)]

# box-plots
herb$FechaF <- factor(herb$Fecha)
levels(herb$FechaF)[levels(herb$FechaF)=="1"] <- "Dic_16"
levels(herb$FechaF)[levels(herb$FechaF)=="2"] <- "Abr_17"
levels(herb$FechaF)[levels(herb$FechaF)=="3"] <- "Ago_17"

ggplot(herb, aes(x=herb$AreaF, y=herb$Herbiv, fill=herb$PosicF)) +
  geom_boxplot() + ylim(0, 5) + 
  labs(x="Localidad", y="Herbivoría (%)", fill="Posición") +
  scale_fill_manual(label = c("superior", "inferior"), values = c("red","green")) +
  facet_grid(.~herb$FechaF)

 
# para probar efectos
# Pruebas Paramétricas 
# ANOVA
Anova(lm(Herbiv ~ Fecha * Area * Posic, data=herbDT, type=2))
Anova(lm(Herbiv ~ Fecha * Area, data=herbDT, type=2))
Anova(lm(Herbiv ~ Area, data=herbDT, type=2))
# para cada Fecha
Anova(lm(Herbiv[1:180] ~ Area[1:180], data=herbDT, type=2))
Anova(lm(Herbiv[181:360] ~ Area[181:360], data=herbDT, type=2))
Anova(lm(Herbiv[361:540] ~ Area[361:540], data=herbDT, type=2))
# to check
anovaFecha <- aov(Herbiv[361:540] ~ Area[361:540], data=herbDT)
summary(anovaFecha)

# Pruebas paramétricas
# Herbivoría de cada Fecha por Posición
t.test(Herbiv[1:180] ~ Posic[1:180], data=herbDT)
t.test(Herbiv[181:360] ~ Posic[181:360], data=herbDT)
t.test(Herbiv[361:540] ~ Posic[361:540], data=herbDT)

# Pruebas no-paramétricas
# Herbivoría de cada Fecha por Posición
wilcox.test(Herbiv[1:180] ~ Posic[1:180], data=herbDT)
wilcox.test(Herbiv[181:360] ~ Posic[181:360], data=herbDT)
wilcox.test(Herbiv[361:540] ~ Posic[361:540], data=herbDT)




