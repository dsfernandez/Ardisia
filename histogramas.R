# caracteristicas datos
class(Ardisia_demografia_censo_nov2016)
dim(Ardisia_demografia_censo_nov2016)
head(Ardisia_demografia_censo_nov2016)
tail(Ardisia_demografia_censo_nov2016)

# separar por sitio
sitioC <- Ardisia_demografia_censo_nov2016[1:86, ]
head(sitioC)
sitioB <- Ardisia_demografia_censo_nov2016[117:167, ]
head(sitioB)
sitioA <- Ardisia_demografia_censo_nov2016[87:116, ]
head(sitioA)

# estadísticas descriptivas
summary(sitioA$altura)

# histograma
hist(sitioA$altura, seq(0, 300, 25), xlab = "Altura")
