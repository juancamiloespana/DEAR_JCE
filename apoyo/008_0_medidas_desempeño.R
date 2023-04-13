
library(Metrics) ##Para calcular MAPE, RMSE MAE
library(car) ### para gráfico qqplot


################## Crear una base de datos ################
################## y es el precio de una vivienda en millones de pesos #########
################## x son los metros cuadrados
################ X2 número de rutas de transporte público #####
###### X3 número de habitaciones


set.seed(100)
x=runif(n=1000,min=20,max=80) #### Genera variable aleatoria 'x' con distribución uniforme de 20 a 80
x2=runif(n=1000,min=5,max=20)
x3= rnorm(n=1000,50,1)
x4=runif(1000, 5, 30)

  


e=rnorm(1000,0,2) ### Se genera un error aleatorio normal
y= 30 + 0.5*x + 0.3*x2+ e  #### Genera la variable respuesta con base en ecuación de recta
y[901:1000]= runif(10,150,200) #### Genera atípicos para la variable respuesta

base_modelo=data.frame(y,x,x2,x3, x4)
colnames(base_modelo)=c("precio",'m2','n_rutas_t','n_hab','anos')

write.csv(base_modelo, 'data\\p_viviendas_ind.csv')



########## 1. Crear modelo de regresión con metros cuadrados
######### 1.1 explicar la salida.

modelo1<-lm(y~x,data=base_modelo)
summary(modelo1)

mean(base_modelo$y)


##### 2. completar estas funciones
AIC(modelo1)
BIC(modelo1)



predichos<-predict(modelo1)
predichos<-modelo1$fitted.values

mae(base_modelo$y,predichos)
mape(base_modelo$y,predichos)

mse(base_modelo$y,predichos)
rmse(base_modelo$y,predichos)




##### 3. ELiminar atípicos y comparar modelos


lim_sup<- quantile(y,0.75) +(IQR(y)*1.5 ) 
lim_inf<- quantile(y,0.25) - (IQR(y)*1.5 ) 

base_modelo2=subset(base_modelo, y<=lim_sup & y>lim_inf)



modelo2<-lm(y~x,data=base_modelo2)


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







##### 4. agregar segunda variable y comparar modelos


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


#### 5 agregar 3 variable ###

modelo4<-lm(base_modelo2$y~base_modelo2$x +base_modelo2$x2+base_modelo2$x3)
predichos4<-predict(modelo4)

mae(base_modelo2$y,predichos4)
mae(base_modelo2$y,predichos3)

mape(base_modelo2$y,predichos4)
mape(base_modelo2$y,predichos3)


mse(base_modelo2$y,predichos4)
mse(base_modelo2$y,predichos3)

rmse(base_modelo2$y,predichos4)
rmse(base_modelo2$y,predichos2)

summary(modelo4)
summary(modelo3)

AIC(modelo4)
AIC(modelo3)


BIC(modelo4)
BIC(modelo3)



######Seleccion de variables

library(randomForest) ### para ver importancia de variables
library(MASS) ## para stepAIC


modelo_reducido=stepAIC(modelo4,k=10) 

summary(modelo_reducido)
## esta función crea el modelo con la menor cantidad de variables posibles sin deteriorar el AIC


rf_model=randomForest(y~.,data=base_modelo2)
rf_model$importance
  
summary(modelo4)



#### Separación de Muestras###

n_obs=length(base_modelo2$y) ## número de observaciones en la base
perc_eval=0.2 # definir el porcentaje de datos a evaluación

eval20= round(n_obs*perc_eval,0) #Calcular el número de observaciones que representa el porcentaje definido

filas_test=sample(x=c(1:n_obs),size=eval20,replace=F) ## generar una lista aleatorias de filas para seleccionar como evaluación


data_train=base_modelo2[-filas_test,]
data_test=base_modelo2[filas_test,c(2:4)]
data_test_y=base_modelo2[filas_test,1]


modelo4=lm(y~.,data=data_train)
predict_train=predict(modelo4)
predict_test=predict(modelo4,newdata = data_test)

mape(data_train$y,predict_train)
mape(data_test_y,predict_test)

######## Crear otro modelo para comparar ####

modelo3=lm(y~x2,data=data_train)

predict_train=predict(modelo3)
predict_test=predict(modelo3,newdata = data_test)

mape(data_train$y,predict_train)
mape(data_test_y,predict_test)
