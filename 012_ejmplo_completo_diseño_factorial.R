

#############Instalar paquetes

###########Cargar paquetes

library(agricolae) ### para comparación de tratamientos
library(pwr) ### para tamaño de muestra
library(gplots) ### para gráfica de medias
library(graphics) ### para gráfico de interacción
library(car) #### para validación de supuestos
library(tseries) #### para validación de supuestos
library(nortest) #### para validación de supuestos
library(goftest)#### para validación de supuestos
library(lmtest) #### para validación de supuestos

################# 3. leer datos #############
url_menu='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/tipos_menu.csv'
ventas<-read.csv(url_menu,sep=";",stringsAsFactors = T)
attach(ventas)
ventas$Menu=as.factor(ventas$Menu)



############### Evaluar tamaño muestral


################# 4. Tamaño muestral #####

mean(ventas$ventas)

sigma=sd(ventas$ventas)
delta=5 ## definido previamente
beta=0.10 ## error tipo II, falso negativo, ho es falsa y acepto
sig=0.05 ## error tipo I, falso positivo, ho es verdadero y rechazo
power=1-beta

## ¿cuál de los dos errores subirían primero?

#########


power.t.test(d=delta, sd=sigma, sig.level = sig,power=power)


#### Modificar argumentos
delta=17 ## definido previamente
beta=0.10 ## error tipo II, falso negativo, ho es falsa y acepto
sig=0.18 ## error tipo I, falso positivo, ho es verdadero y rechazo
power=1-beta


power.t.test(d=delta, sd=sigma, sig.level = sig,power=power)

############## 5. Análsis descriptivo ####
  ######5.1 Efectos principales #########

  par(mfrow=c(1,2))
  
  boxplot(ventas$ventas~Costa,main="Ventas por costa",ylab="ventas semanales miles de dólares")
  plotmeans(ventas$ventas~Costa,main="Ventas por costa",ylab="ventas semanales miles de dólares")
  

  boxplot(ventas$ventas~Menu,main="Ventas por Menu",ylab="ventas semanales miles de dólares")
  plotmeans(ventas$ventas~Menu,main="Ventas por Menu",ylab="ventas semanales miles de dólares")
  
  par(mfrow=c(1,1))
  
  formula=ventas$ventas~ventas$Menu +ventas$Costa
  plot.design(formula,  xlab="Efectos", ylab="Ventas semanales", main="Comparación efectos principales")
  
  
  
  ######5.2 Efectos de interaccion #########

  interaction.plot(x.factor =   ventas$Costa,trace.factor=ventas$Menu,
                   response= ventas$ventas,
                   trace.label = "Menu",xlab = "Costa", 
                   ylab="Ventas semanales",
                   main="Gráfico de interacciones")
  
  interaction.plot(x.factor =   ventas$Menu,trace.factor=ventas$Costa,
                   response= ventas$ventas,
                   trace.label = "Costa",xlab = "Menu", 
                   ylab="",
                   main="Gráfico de interacciones")
  
  
############## 6. Ajustar modelos  de medias #############
  

anova_franc=aov(ventas$ventas~ventas$Costa*ventas$Menu)

summary(anova_franc)

### tabla de medias

medias<-model.tables(x=anova_franc,type="means")
medias                 


################### 7. modelos de efecto #####

efectos<-model.tables(x=anova_franc,type="effects")
efectos  


############ 9. Evaluación de pruebas de hipotesis ######

summary(anova_franc)


############ 10. Comparación de tratamientos ####

thsd<-TukeyHSD(anova_franc)
plot(thsd,cex.axis=0.7,las=1)


hsd<-HSD.test(y=anova_franc, trt=c("ventas$Costa","ventas$Menu"), console=T, group=F)


lsd<-LSD.test(y=anova_franc, trt=c("ventas$Costa","ventas$Menu"), console=T, group=F)




############ Evaluación de supuestos #####

residuales=residuals(anova_franc)
#### 11.1 Supuesto de normalidad -Gráficos  ####
#Histograma
par(mfrow=c(1,3))
hist(residuales)



#qqplot fancy

qqPlot(residuales) # Si se salen de las bandas generalmente no cumple normalidad

# boxplot 

boxplot(residuales, horizontal = T)


#residuales=rnorm(1000)
#### 11.1 Supuesto de normalidad -pruebas estadísticas #########


shapiro.test(residuales) ##shapiro wilk

jarque.bera.test(residuales) ###jarque bera test

ks.test(x=residuales,pnorm,mean=0,sd=sd(residuales)) ####kolmogorov smirnov

ad.test(residuales,pnorm,mean=0,sd=sd(residuales))  ###Anderson darling

cvm.test(residuales,pnorm,mean=0,sd=sd(residuales)) ###Cramer von mises


#### 11.2 supuesto Varianza constante - graficos #####



plot(predict(anova_franc),residuales)
abline(h=0)


#### 11.2 supuesto Varianza constante - prueba estadísticas ####


bptest(anova_franc) ##breusch pagan test


###### 11.3 supuestos independencia de errores -graficos ####
par(mfrow=c(1,3))
#orden vs residuales
plot(residuales)


#acf y pacf

acf(residuales)
pacf(residuales)


#### 11.3 supuestos independencia de errores pruebas de independencia ####

# Prueba de durbin watson

dwtest(anova_franc) 
# Prueba de Breusch y godfrey

bgtest(anova_franc)

install.packages("gamlss.data")
install.packages("gamlss")

fitDist()

gamlss.dist::ZAPIG()

library(gamlss)
y <- rt(100, df=1)
m1<-fitDist(y, type="realline")
m1$fits
m1$failed
summary(m1)

m1$mu
m1$sigma
m1$sigma.coefficients
m2$nu

set.seed(123)
data <- r(100, mu = 5, nu = 2)

# Fit the ZAPIG distribution to the data
fit <- gamlss(data ~ 1, family = ZAPIG.family)

# Print the summary of the fitted distribution
summary(fit)

# Extract the estimated parameters of the ZAPIG distribution
best_params <- coef(fit)

# Generate new data from the ZAPIG distribution using the estimated parameters
new_data <- rzapig(100, mu = best_params["mu"], nu = best_params["nu"])

# Plot the original data and the newly generated data
par(mfrow = c(1, 2))
hist(data, main = "Original Data", xlab = "Value")
hist(new_data, main = "Generated Data", xlab = "Value")



datos=rZAPIG(100, mu=1, nu=0.4, sigma=1)

plot(datos, type="l")

m=fitDist(datos,type="counts")

#### solo esta parte cambia, en lugar de escribirlos con el .coefficients , se escriben solos, el .coefficient tiene una transformación, y porbablemente poreso salía un error cuando generabamos la distribución con los parámetros calculados.

mu_f=m$mu
sig_f=m$sigma
nu_f=m$nu

datos_nuevo=rZAPIG(n=720,mu =mu_f, sigma=sig_f, nu=nu_f )






