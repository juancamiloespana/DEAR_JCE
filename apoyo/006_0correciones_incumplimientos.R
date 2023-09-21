
library(car)
library(tseries)
library(nortest)
library(goftest)
library(lmtest)


#1. Datos inusuales #####

    ##1.1 basado en cada variable de manera independiente - Eliminar ####
        
        ####Crear data frame

        
      base_modelo=read.csv('https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/defectos.csv')
        
      ### Para simplificar nombre de variables
      
      y=base_modelo$defectos
      x1=base_modelo$empleados_calidad
      names(base_modelo)=c("y","x1")  
      
      ###Graficar para identificar datos inusuales

      
        
        plot(base_modelo$x1,base_modelo$y)
        boxplot(base_modelo$y)
        boxplot(base_modelo$x1)
        
        ####Ajustar modelo y verificar desempeño ###
        mod=lm(y~x1,data=base_modelo)
        plot(x1,y)
        abline(mod)
        summary(mod)
        mean(y)

        
  ##### verificar supuestos del modelo
        
        res=mod$residuals
        shapiro.test(res)
        jarque.bera.test(res)
        
        bptest(mod)
        dwtest(mod)
        bgtest(mod)
        

        
        ####Calcular límites para los atípicos
        
        lim_sup<- quantile(x1,0.75) +(IQR(x1)*1.5 ) 
        lim_inf<- quantile(x1,0.25) - (IQR(x1)*1.5 ) 
        
        ####Filtrar base para que se eliminen atípicos
        
        base_modelo=subset(base_modelo, x1<=lim_sup & x1>lim_inf)
        
        boxplot(base_modelo$x1)
        #### Verificar base despues de eliminar atípicos
        plot(base_modelo$x1, base_modelo$y)
        
        ####Ajustar modelo y verificar desempeño ###
        mod=lm(y~x1,data=base_modelo)
        summary(mod)
        
        plot(base_modelo$x1,base_modelo$y)
        abline(mod)
        boxplot(x1)
        
  ##### verificar supuestos del modelo 
        res=mod$residuals
        shapiro.test(res)
        jarque.bera.test(res)
        bptest(mod)
        dwtest(mod)
        bgtest(mod)
        
        
        
        
    ##1.2  Efectos sobre el modelo #####
        ####Crear variables ficticia 
          
        datos=read.csv('https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/plantas.csv')
        names(datos)=c("indice","y","x")
        
        
        plot(datos$x,datos$y) 
        mod=lm(y~x,data=datos)
        abline(mod,col="red")
        
        ###supuestos ###
        
        res=mod$residuals
        shapiro.test(res)
        jarque.bera.test(res)
        bptest(mod)
        dwtest(mod)
        bgtest(mod)
        
        ########## Eliminación puntos de influencia
        
        medidas=influence.measures(mod)
        solo_inf=summary(medidas)
        filas_filtrar=as.numeric(row.names(solo_inf))
        datos_f=datos[-filas_filtrar,]
        
        plot(datos_f$x,datos_f$y)
        
        mod2=lm(y~x,datos_f)
        summary(mod2)
        summary(mod)
        
        res2=mod2$residuals  
        shapiro.test(res2)
        jarque.bera.test(res2)
        bptest(mod2)
        dwtest(mod2)
        bgtest(mod2)

#2. Transformación de variable ####
        
  ## 2.1transformación explicativa####
  
    ###Crear base que incumple supuesto
    
        ###Crear base que incumple supuesto
        

        
        datos=read.csv('https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/flujo.csv')
        names(datos)=c('ind','y','x')
    
        
        datos$z=datos$x^2
        mod1=lm(y~x,data=datos)
        
        plot(datos$x,datos$y)
        abline(mod1,col="red")
        
        summary(mod1)
        res1=mod1$residuals
        shapiro.test(res1)
        bptest(mod1)
        dwtest(mod1)
        

        
        plot(datos$z,datos$y)
        
        mod2=lm(y~z,data=datos)
        abline(mod2,col="red")
        summary(mod2)
        
        res2=mod2$residuals
        shapiro.test(res2)
        bptest(mod2)
        dwtest(mod2)
        
        
        

 
 ## 2.2transformación respuesta####
    
    ###Crear base que incumple supuesto
    
    datos=read.csv('https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/energia.csv')
    
        
    names(datos)=c("i","y","x") 
    plot(datos$x,datos$y)
    mod=lm(y~x,data=datos)
    
    pt=powerTransform(mod)
    summary(pt)
    
    datos$w=datos$y^4
    
    plot(datos$x,datos$w)
    
    mod=lm(w~x,data=datos)
    abline(mod)
    
    res=mod$residuals
    shapiro.test(res)
    bptest(mod)
    dwtest(mod)
    
    
  

  
    
  