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



####

