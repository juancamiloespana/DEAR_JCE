
library(Metrics) ##Para calcular MAPE, RMSE MAE
library(car) ### para gráfico qqplot
library(MASS) ###STEPAIC
##### utilice la base de datos 'p_viviendas_ind.csv' para realizar los siguientes puntos###

#### 1. separar base en entrenamiento y evaluacion ####
### 2. Crear modelo de regresión con metros cuadrados
#### 2.1 explicar la salida.
##### 4. agregar segunda variable y comparar modelos
#### 5 agregar 3 variable ###
##### 6. Seleccion de variables automáticas
##### 7. Eliminar atípicos y comparar modelos

#### Ejercicio ajustar un modelo para la base 'los_microsoft.csv'
### predecir la variable lengthofstay con base en las otras variables ##



ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/p_viviendas_ind.csv'
precio=read.csv(ruta)

boxplot(precio$precio) ## tiene atipicos pero es mejor separar muestra de evaluacion y 
#entrenamiento y dejar atipicos en evalucion y eliminarlos de entrenamiento
set.seed(123)
list_filas=c(1:length(precio$precio)) ### lista con las filas en tabla
n_fila_test=round(length(precio$precio)*0.2,0) ### calculculo de cuantas filas son el 20%
sam=sample(list_filas,size=n_fila_test)

precio_train=precio[-sam,] ##dataframe observaciones entrenamiento
precio_test=precio[sam,]##dataframe observaciones evaluacion

#####ajustar modelo solo con m2

mod1=lm(precio~m2, data=precio_train)
summary(mod1)

#### ajustar modelo2 incluyendo una variables

mod2=lm(precio~m2+n_rutas_t, data=precio_train)
summary(mod2)

##### comparar mod1 y mod2 con diferentes indicadores

AIC(mod1)
AIC(mod2)

BIC(mod1)
BIC(mod2)

mae(precio_train$precio, mod1$fitted.values)
mae(precio_train$precio, mod2$fitted.values)
mape(precio_train$precio, mod1$fitted.values)
mape(precio_train$precio, mod2$fitted.values)
rmse(precio_train$precio, mod1$fitted.values)
rmse(precio_train$precio, mod2$fitted.values)

######### modelo con todas las variables #####

mod3=lm(precio~.-X, data= precio_train)
summary(mod3)

AIC(mod1)
AIC(mod3)

mape(precio_train$precio, mod1$fitted.values)
mape(precio_train$precio, mod3$fitted.values)

##### muestra de evaluacion ######

pred1=predict(mod1, newdata = precio_test)
mape(precio_test$precio, pred1)

pred3=predict(mod3, newdata = precio_test)

mape(precio_test$precio, pred3)


#####
mod_red= stepAIC(mod3)
summary(mod_red)


