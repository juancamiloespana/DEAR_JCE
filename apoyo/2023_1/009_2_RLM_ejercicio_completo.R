
url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/base_supermercado2.csv'


df=read.csv(url, stringsAsFactors = T)

df$fecha_ultima_compra2=as.Date(df$fecha_ultima_compra,format="%d/%m/%Y")

df$fu_m=as.numeric(format(df$fecha_ultima_compra2,format="%u"))

mod=lm(y~.-id-fecha_ultima_compra-producto_frecuente+usa_tc*reportado, data=df)
summary(mod)

library(plot)

mape(mod$model$y, mod$fitted.values)


df$y=df$y+(df$usa_tc*df$reportado)*500000

max(df$y)

write.csv(df, 'data\\base_supermercado2.csv', row.names=F)

df$y=y

library(graphics)


interaction.plot(x.factor=df$reportado, trace.factor=df$usa_tc, response = df$y, data=df)

mod_red=stepAIC(mod)
summary(mod_red)
mape(mod_red$model$y, mod_red$fitted.values)
mape(mod$model$y, mod$fitted.values)
mae(mod_red$model$y, mod_red$fitted.values)
mae(mod$model$y, mod$fitted.values)

library(dplyr)
options(scipen = 2)
mod_red$coefficients[ order(mod_red$coefficients, decreasing=T)]
length(mod_red$coefficients)

df=data.frame(mod_red$coefficients)

arrange(df,desc(mod_red.coefficients))
df$mod_red.coefficients=as.integer(df$mod_red.coefficients)

mod_red$xlevels

