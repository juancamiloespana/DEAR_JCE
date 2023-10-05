install.packages("Metrics")
library(Metrics)

url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/los_microsoft.csv'

df=read.csv(url, stringsAsFactors = T)

mod=lm(lengthofstay~., data=df)
length(mod$coefficients)
summary(mod)


### modelo dos eliminando las variables de fechas


mod2=lm(lengthofstay~.-vdate-discharged, data=df)
length(mod2$coefficients)

summary(mod2)

mape(df$lengthofstay, mod2$fitted.values)*100 ## para que de en porcentaje
rmse(df$lengthofstay, mod2$fitted.values)
mae(df$lengthofstay, mod2$fitted.values)
AIC(mod2)
BIC(mod2)

####
library(MASS)

modelo_reducido= stepAIC(mod2)
length(modelo_reducido$coefficients)

mape(df$lengthofstay, modelo_reducido$fitted.values)*100


####### ajustar modelo y medir erorr de entrenamiento y evaluación ###

library(Metrics)

url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/los_microsoft.csv'

df_micro=read.csv(url, stringsAsFactors = T)

filas_df= c(1:nrow(df_micro)) ## vector con todas las filas
n_filas=nrow(df_micro) ## cantidad de filas

set.seed(235) ### sirve para que una seleccion aleatoria se pueda replicar
ind_train= sample(filas_df, size=round(n_filas*0.8), replace=F) ###extrae las filas para el entrenamiento
df_train=df_micro[ind_train,] ### dataframe de entrenamiento
df_test=df_micro[-ind_train,] ### dataframe de evaluación
mean(df_train$lengthofstay) ### verificar semilla

modelo=lm(lengthofstay~.-vdate-discharged, data=df_train) ### se ajusta en entrenamiento

####medir indicadores en entrenamiento

mape(df_train$lengthofstay, modelo$fitted.values)
mae(df_train$lengthofstay, modelo$fitted.values)
rmse(df_train$lengthofstay, modelo$fitted.values)

#### medir en evaluacion

pred=predict(modelo, newdata=df_test)

mape(df_test$lengthofstay, pred)
mae(df_test$lengthofstay, pred)
rmse(df_test$lengthofstay, pred)


######## ### calcular evaluacióno y entrenamiento para nueva base prod_pib_sa


url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/prod_bib_sa.csv'

df_pib=read.csv(url, stringsAsFactors = T)

filas_df= c(1:nrow(df_pib)) ## vector con todas las filas
n_filas=nrow(df_pib) ## cantidad de filas

set.seed(235) ### sirve para que una seleccion aleatoria se pueda replicar
ind_train= sample(filas_df, size=round(n_filas*0.8), replace=F) ###extrae las filas para el entrenamiento
df_train=df_pib[ind_train,] ### dataframe de entrenamiento
df_test=df_pib[-ind_train,] ### dataframe de evaluación
mean(df_train$prod) ### verificar semilla

modelo=lm(prod~., data=df_train) ### se ajusta en entrenamiento

####medir indicadores en entrenamiento

mape(df_train$prod, modelo$fitted.values)*100
mae(df_train$prod, modelo$fitted.values)
rmse(df_train$prod, modelo$fitted.values)

#### medir en evaluacion

pred=predict(modelo, newdata=df_test)

mape(df_test$prod, pred)*100
mae(df_test$prod, pred)
rmse(df_test$prod, pred)


plot(prod~pib, data=df_pib)
lines(df_train$pib, modelo$fitted.values, col='red')
