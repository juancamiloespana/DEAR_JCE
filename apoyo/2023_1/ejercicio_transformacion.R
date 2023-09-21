###ejercicio


pib=runif(200, 1,  15)

plot(pib)

r_prod= 50 +4.5*pib +rnorm(100,5,3)

plot(pib~sqrt(r_prod))

min(r_prod)



filas=sample(c(1:200),30)

r_prod[filas[1:30]] =pib[filas[1:30]]*50+rnorm(30,200,70)


max(r_prod)
plot(r_prod~pib)



prod=r_prod**4
plot(prod~pib)

mean(prod)

max(prod)

p=prod/1000000

plot(p~pib)
min(p)



df=data.frame(prod=p, pib=pib)


write.csv(df, "data\\prod_pib.csv", row.names=F)



df=read.csv("data\\prod_pib.csv")

plot(prod~pib,data=df)

mod=lm(prod~pib,data=df)

im=summary(influence.measures(mod))
f=as.numeric(row.names(im))

df2=df[-f,]
plot(prod~pib,data=df2)

bigotesup=quantile(df$prod,0.75)+1.5*IQR(df$prod)
bigoteinf=quantile(df$prod,0.25)-1.5*IQR(df$prod)

df3=subset(df,df$prod<=bigotesup &df$prod>=bigoteinf)
plot(prod~pib,data=df3)
library(car)

mod3=lm(prod~pib,data=df3)
abline(mod3)
summary(powerTransform(mod3))



pib2=df3$pib^2


plot(df3$prod~pib2)

prod2=df3$prod**(1/4)
df3$prod2=prod2
plot(prod2~pib, data=df3)
mod_f=lm(prod2~pib, data=df3)
abline(mod_f)


df_nuevo=data.frame(pib=20)
v=predict(mod_f, newdata=df_nuevo, interval="prediction")

v[1]**4


shapiro.test(mod_f$res)
library(tseries)
library(lmtest)

bptest(mod_f)
bgtest(mod_f)


summary(mod_f)



####modelo 2



#############################
plot(prod~pib,data=df)
abline(mod)


shapiro.test(mod$res)
bptest(mod)
gqtest(mod)
bgtest(mod)


plot(mod$residuals~mod$fitted.values)
predict(mod,newdata=df_nuevo, interval='prediction')
summary(mod)

######

mod2=lm(prod~pib,data=df3)

plot(prod~pib,data=df3)
abline(mod2)

summary(mod2)

shapiro.test(mod2$res)
gqtest(mod2)
bptest(mod2)
bgtest(mod2)

plot(mod2$residuals~mod2$fitted.values)
