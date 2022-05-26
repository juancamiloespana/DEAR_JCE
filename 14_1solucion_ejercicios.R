
#### 1.Ajsutar modelo, entrenar modelo explicar salida #####

modelo1<-lm(base_modelo$y~base_modelo$x)
summary(modelo1)












##### 2.Calcular predichos y desempeño mape, mae, mse, rmse #####
AIC(modelo1)
BIC(modelo1)



predichos<-predict(modelo1)
predichos<-modelo1$fitted.values

mae(base_modelo$y,predichos)
mape(base_modelo$y,predichos)

mse(base_modelo$y,predichos)
rmse(base_modelo$y,predichos)








##### 3.ELiminar atípico ####


lim_sup<- quantile(y,0.75) +(IQR(y)*1.5 ) 
lim_inf<- quantile(y,0.25) - (IQR(y)*1.5 ) 
base_modelo2=subset(base_modelo, y<=lim_sup & y>lim_inf)

modelo2<-lm(base_modelo2$y~base_modelo2$x)
predichos2<-predict(modelo2)

mae(base_modelo2$y,predichos2)
mae(base_modelo$y,predichos)


mape(base_modelo2$y,predichos2)
mape(base_modelo$y,predichos)

mse(base_modelo2$y,predichos2)
mse(base_modelo$y,predichos)

rmse(base_modelo2$y,predichos2)
rmse(base_modelo$y,predichos)

summary(modelo2)
summary(modelo1)

AIC(modelo2)
AIC(modelo1)

BIC(modelo2)
BIC(modelo1)









###### 4. agregar variable #####

modelo3<-lm(base_modelo2$y~base_modelo2$x +base_modelo2$x2)
predichos3<-predict(modelo3)

mae(base_modelo2$y,predichos3)
mae(base_modelo2$y,predichos2)
mae(base_modelo$y,predichos)

mape(base_modelo2$y,predichos3)
mape(base_modelo2$y,predichos2)
mape(base_modelo$y,predichos)

mse(base_modelo2$y,predichos3)
mse(base_modelo2$y,predichos2)
mse(base_modelo$y,predichos)

rmse(base_modelo2$y,predichos3)
rmse(base_modelo2$y,predichos2)
rmse(base_modelo$y,predichos)

summary(modelo3)
summary(modelo2)
summary(modelo1)

AIC(modelo3)
AIC(modelo2)
AIC(modelo1)

BIC(modelo3)
BIC(modelo2)
BIC(modelo1)









##### 5. formula para mape #####

mape<-sum(abs(predichos-base_modelo$y)/base_modelo$y)/length(base_modelo$y)
rmse<-sqrt(sum((predichos-base_modelo$y)^2)/length(base_modelo$y))










##### 6. Agregar tercera variable #####

modelo4<-lm(base_modelo2$y~base_modelo2$x +base_modelo2$x2+base_modelo2$x3)
predichos5<-predict(modelo4)

mae(base_modelo2$y,predichos5)
mae(base_modelo2$y,predichos3)

mape(base_modelo2$y,predichos5)
mape(base_modelo2$y,predichos3)

mse(base_modelo2$y,predichos5)
mse(base_modelo2$y,predichos3)

rmse(base_modelo2$y,predichos5)
rmse(base_modelo2$y,predichos3)


summary(modelo3)
summary(modelo4)

AIC(modelo4)
AIC(modelo3)

BIC(modelo4)
BIC(modelo3)








########7. utilizar stepAIC e importancia de variables #######



modelo_definitivo<-stepAIC(modelo4)
summary(modelo_definitivo)



bosques<-randomForest(base_modelo2$y~base_modelo2$x +base_modelo2$x2+base_modelo2$x3)
bosques$importance

arbol_decision<-rpart(base_modelo2$y~base_modelo2$x +base_modelo2$x2+base_modelo2$x3)

arbol_decision$variable.importance











######## 8. validar supuestos #######


residuales<-modelo3$residuals


################### 1. prueba normalidad ####
#####1.1 Normalidad - Pruebas gráficas ####

#Histograma

hist(residuales)

#qqplot sencillo

qqnorm(residuales)
qqline(residuales)

#qqplot fancy
qqPlot(residuales)


boxplot(residuales,horizontal=T)

#### 1.2 Normalidad -pruebas estadísticas #########


shapiro.test(residuales) ##shapiro wilk

jarque.bera.test(residuales) ###jarque bera test

ks.test(x=residuales,pnorm,0,sd(residuales)) ####kolmogorov smirnov

ad.test(residuales, pnorm, 0, sd(residuales))  ###Anderson darling

cvm.test(residuales,pnorm, 0, sd(residuales)) ###Cramer von mises

################# 2. prueba varianza constante - Homocedasticidad ######

########2.1 Homocedasticidad -Pruebas gráficas #####


plot(modelo3$fitted.values,residuales)
abline(h=0, col="red")

plot(y,residuales)
abline(h=0)

########2.2 Homocedasticidad -Pruebas estadísticas #####

bptest(modelo3) 

################ 3. prueba independencia  #####


########3.1 Independencia-Pruebas GRáficas #####

plot(residuales)

par(mfrow=c(1,2))
acf(residuales)
pacf(residuales)

########3.2 Independencia-Pruebas estadísticas #####

dwtest(modelo3) 

bgtest(modelo3)



confint(modelo3)
