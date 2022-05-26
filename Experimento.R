### EXPERIMENTO ###
# Para aleatorizar los datos 
sample(24)

# Lectura de datos
library(readxl)
d <- read_excel("~/Susana/Universidad/Regresión/Trabajo DOE/d.xlsx")
str(d)

### -------------------------- ANALISIS DESCRIPTIVO -------------------------- ###
attach(d)
tamaño <- as.factor(tamaño)
papel <- as.factor(papel)
forma <- as.factor(forma)

## GRAFICO DE CAJAS POR FACTOR
# Distancia vs Tamaño
par(mfrow=c(1,3))
boxplot(distancia ~ tamaño, xlab="Tamaño del avión", ylab="Distancia recorrida", col=palette("Pastel 2"))
points(by(distancia, tamaño, mean), col="red", pch=16)
lines(by(distancia, tamaño, mean), col ="red", pch=16)
abline(h=mean(distancia), col="red", lty= 2)

# Distancia vs Papel
boxplot(distancia ~ papel, xlab="Tipo de papel", ylab="Distancia recorrida", col=palette("Pastel 2"))
points(by(distancia, papel, mean), col="red", pch=16)
lines(by(distancia, papel, mean), col="red", pch=16)
abline(h=mean(distancia), col="red", lty=2)

# Distancia vs Forma
boxplot(distancia ~ forma, xlab="Forma del avión", ylab="Distancia recorrida", col=palette("Pastel 2"))
points(by(distancia, forma, mean), col="red", pch=16)
lines(by(distancia, forma, mean), col="red", pch=16)
abline(h=mean(distancia), col="red", lty=2)

## GRAFICO DE MEDIAS
require(gplots)
  par(mfrow=c(1,3))
plotmeans(distancia ~ tamaño, xlab="Tamaño del avión", ylab="Promedio de Distancia recorrida")
plotmeans(distancia ~ papel, xlab="Tipo de papel", ylab="Promedio de Distancia recorrida")
plotmeans(distancia ~ forma, xlab="Forma del avión", ylab="Promedio de Distancia recorrida")

## GRAFICO DEL DISEÑO
require(graphics)
par(mfrow=c(1,1))
plot.design(distancia ~ tamaño+papel+forma, col="magenta", xlab="Efectos", ylab="Promedio de Distancia recorrida", main="Grafico de Diseño")


## GRAFICOS DE INTERACCION
# Interaccion doble
par(mfrow=c(1,3))
interaction.plot(tamaño, papel, distancia, xlab="Tamaño", ylab="Promedio de Distancia recorrida", col="black")
interaction.plot(tamaño, forma, distancia, xlab="Tamaño", ylab="Promedio de Distancia recorrida", col="black")
interaction.plot(papel, forma, distancia, xlab="Papel", ylab="Promedio de Distancia recorrida", col="black")

# Todas las interaccione
par(mfrow=c(1,1))
boxplot(distancia ~ tamaño+papel+forma, cex.axis=0.8, ylab="Distancia recorrida", xlab= "", las=2, col=palette("Pastel 2"))
par(mfrow=c(1,1))
plot.design(distancia ~ tamaño+papel+forma, col="magenta", ylab="Distancia recorrida", main="Gráfico de Diseño")

### ---------------------------- MODELOS --------------------------- ###
## MODELO DE REGRESION 
m1 <- lm(distancia ~ tamaño*papel*forma, data=d) 
summary(m1)
anova(m1)

## MODELO DE EFCTOS ASOCIADOS
m <- aov(distancia ~ tamaño*papel*forma)
summary(m)

# Modelo de efectos
model.tables(m, type="effects")

# Modelo de medias
model.tables(m, type="means")

### --------------------- VALIDACION DEL MODELO --------------------- ###
residuales <- rstandard(m)
summary(residuales)

library(car)
## NORMALIDAD
# Graficos
par(mfrow=c(1,3))
qqPlot(residuales, xlab="Cuantiles teoricos", ylab="Cuantiles muestrales", main="Grafico cuantil-cuantil", col="black")
hist(residuales, xlab="Residuales", ylab="Frecuencia", main="Histograma", col=palette("Pastel 2"))
boxplot(residuales, xlab="Residuales", main="Grafico de cajas", col=palette("Pastel 2"))

# Pruebas estadisticas
require(nortest)
shapiro.test(residuales)
ad.test(residuales)
cvm.test(residuales)
require(tseries)
jarque.bera.test(residuales)
lillie.test(residuales)

## HOMOCEDASTICIDAD O VARIANZA CONSTANTE
# Graficos
valores_ajustados<-fitted(m)
par(mfrow=c(1,4))
plot(valores_ajustados, residuales, xlab="Valores ajustados", ylab="Residuales", main="Valores Ajustados vs e_i")
abline(h=0, col="red")
plot(tamaño, residuales, xlab="Tamaño", ylab="Residuales", main="Boxplot Tamaño vs ei", col=palette("Pastel 2"))
abline(h=0, col="red")
plot(papel, residuales, xlab="Papel", ylab="Residuales", main="Boxplot Papel vs ei", col=palette("Pastel 2"))
abline(h=0, col="red")
plot(forma, residuales, xlab="Forma", ylab="Residuales", main="Boxplot Forma vs ei", col=palette("Pastel 2"))
abline(h=0, col="red")

# Pruebas estadisticas
require(lmtest)
bptest(m)

leveneTest(residuales ~ tamaño)
leveneTest(residuales ~ papel)
leveneTest(residuales ~ forma)
leveneTest(residuales ~ tamaño*papel)
leveneTest(residuales ~ tamaño*forma)
leveneTest(residuales ~ papel*forma)
leveneTest(residuales ~ tamaño*papel*forma)

## INDEPENDENCIA
# Graficos 
par(mfrow=c(1,3))
plot(residuales, pch=16, xlab="Orden", ylab="Residuales", main="Gráfico de Orden vs e_i")
abline(h=0, col="red")
acf(residuales, ylim=c(-1,1), main="ACF")
pacf(residuales, ylim=c(-1,1), main="PACF")

# Pruebas estadisticas
dwtest(m, alternative="two.sided")
bgtest(m)
Box.test(residuales, type="Ljung")

  ## COMPARACIONES MULTIPLES
require(agricolae)
LSD.test(m, "tamaño", console=TRUE, group=FALSE)
LSD.test(m, "papel", console=TRUE, group=FALSE)
LSD.test(m, "forma", console=TRUE, group=FALSE)

# Metodo Tukey
par(mfrow=c(1,1))
TukeyHSD(m)
plot(TukeyHSD(m))

### --------------------- SUPERFICIE DE RESPUESTA --------------------- ###
library(readxl)
d1 <- read_excel("~/Susana/Universidad/Regresión/Trabajo DOE/d1.xlsx", sheet = "Codificada")

# Modelo 
m2 <- lm(distancia ~ tamaño*papel*forma, data=d1) 
summary(m2)

#Superficie de respuesta:
par(mfrow=c(1,2))
x     <-  seq(-1, 1, 0.1) # x=tamaño
y     <-  seq(-1, 1, 0.1) # y=papel
model <-  function (x, y){267.9875 + 14.0958*x -22.6625*y + 11.4625*x*y -7.3708*(1)+  5.2625*y*(-1)}
z     <-  outer(x, y ,model)

#Superficie respuesta:
persp(x,y,z, phi=30, theta=130, xlab="tamaño", ylab="papel",main = "Grafico de superfice de respuesta", 
      zlab="distancia", col = "lightblue", expand=0.9, ticktype = "detailed")

#Grafico de contornos:
contour(x,y,z,nlevels=30, main="Grafico de contornos")

### ------------------------ NUMERO DE REPLICAS ------------------------ ###
install.packages("pwr2")
require(pwr2)
sd(d1$distancia)

## TAMAÑO DE LA MUESTRA 
#Factor A Tamaño, Factor B Papel
#f.a <- sqrt((a*D^2)/(2*b*sigma^2))
f.a <- sqrt((2*0.1^2)/(2*2*(35.14435)^2))

#f.b <- sqrt((b*D^2)/(2*a*sigma^2))
f.b <- sqrt((2*0.4^2)/(2*2*(35.14435)^2))
P1 <- pwr.2way(a=2, b=2, alpha =0.05, size.A = 3, size.B = 3, f.A = f.a, f.B = f.b)
P1

#Factor A Tamaño, Factor B Forma
#f.a <- sqrt((a*D^2)/(2*b*sigma^2))
f.a <- sqrt((2*0.1^2)/(2*2*(35.14435)^2))

#f.b <- sqrt((b*D^2)/(2*a*sigma^2))
f.b <- sqrt((2*5.4^2)/(2*2*(35.14435)^2))
P2 <- pwr.2way(a=2, b=2, alpha =0.05, size.A = 3, size.B = 3, f.A = f.a, f.B = f.b)
P2

#Factor A Papel, Factor B Forma
#f.a <- sqrt((a*D^2)/(2*b*sigma^2))
f.a <- sqrt((2*0.4^2)/(2*2*(35.14435)^2))

#f.b <- sqrt((b*D^2)/(2*a*sigma^2))
f.b <- sqrt((2*5.4^2)/(2*2*(35.14435)^2))
P3 <- pwr.2way(a=2, b=2, alpha =0.05, size.A = 3, size.B = 3, f.A = f.a, f.B = f.b)
P3

## NUMERO DE REPLICAS
#Factor A Tamaño, Factor B Papel
#f.a <- sqrt((a*D^2)/(2*b*sigma^2))
f.a <- sqrt((2*0.1^2)/(2*2*(35.14435)^2))

#f.b <- sqrt((b*D^2)/(2*a*sigma^2))
f.b <- sqrt((2*0.4^2)/(2*2*(35.14435)^2))
n1 <- ss.2way(a=2, b=2, alpha = 0.1, beta = 0.1, f.A = f.a, f.B = f.b, B=100)
n1

#Factor A Tamaño, Factor B Forma
#f.a <- sqrt((a*D^2)/(2*b*sigma^2))
f.a <- sqrt((2*0.1^2)/(2*2*(35.14435)^2))

#f.b <- sqrt((b*D^2)/(2*a*sigma^2))
f.b <- sqrt((2*5.4^2)/(2*2*(35.14435)^2))
n2 <- ss.2way(a=2, b=2, alpha = 0.1, beta = 0.1, f.A = f.a, f.B = f.b, B=100)
n2

#Factor A Papel, Factor B Forma
#f.a <- sqrt((a*D^2)/(2*b*sigma^2))
f.a <- sqrt((2*0.4^2)/(2*2*(35.14435)^2))

#f.b <- sqrt((b*D^2)/(2*a*sigma^2))
f.b <- sqrt((2*5.4^2)/(2*2*(35.14435)^2))
n3 <- ss.2way(a=2, b=2, alpha = 0.1, beta = 0.1, f.A = f.a, f.B = f.b, B=100)
n3

