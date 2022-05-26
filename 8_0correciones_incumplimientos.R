
#1. Datos inusuales #####

    ##1.1 basado en cada variable de manera independiente - Eliminar ####
    
        ###### crear variables ficticias 
        
        x1=runif(n=1000,min=20,max=80)
        x1[901:1000]= runif(100,120,140)
        e=rnorm(1000,0,5)
        y= 30 + 0.5*x1 + e
        
        
        ####Agregar atípicos ficticios
        y[901:1000]= runif(100,150,200)
        
        ####Crear data frame
        base_modelo=data.frame(y,x1)
        
        ###Graficar para identificar datos inusuales
        
        plot(x1,y)

        boxplot(y)
        boxplot(x1)
        
        ####Ajustar modelo y verificar desempeño ###
        mod=lm(y~x1,data=base_modelo)
        plot(x1,y)
        abline(mod)
        summary(mod)

        
  ##### verificar supuestos del modelo (ejercicio)
        
        ####Calcular límites para los atípicos
        
        lim_sup<- quantile(y,0.75) +(IQR(y)*1.5 ) 
        lim_inf<- quantile(y,0.25) - (IQR(y)*1.5 ) 
        
        ####Filtrar base para que se eliminen atípicos
        
        base_modelo=subset(base_modelo, y<=lim_sup & y>lim_inf)
        
        
        #### Verificar base despues de eliminar atípicos
        plot(base_modelo$x1, base_modelo$y)
        
        ####Ajustar modelo y verificar desempeño ###
        mod=lm(y~x1,data=base_modelo)
        summary(mod)
        
        plot(base_modelo$x1,base_modelo$y)
        abline(mod)
        
  ##### verificar supuestos del modelo (ejercicio)
        
    ##1.2  Efectos sobre el modelo #####
        
        ####Crear variables ficticia 
        set.seed(200)
        x1=runif(n=1000,min=20,max=80)
        x1[901:1000]= runif(10,120,140)
        e=rnorm(1000,0,5)
        
        
        library(COUNT)
        
        data("azpro")
        
        
        ####Agregar atípicos ficticios
        y= 30 + 0.5*x1 + e
        y[901:1000]= runif(10,150,200)
        
        
        ####Crear data frame
        base_modelo=data.frame(y,x1)
        
        #####Ajustar un modelo de regresión ###
        plot(y,x1)
        mod<-lm(y~x1)
        
        #### Verificar desempeño con R2 y desviación del modelo
        
        summary(mod)
        
        
        
        ###Verificar graficamente datos inusuales
        plot(x1,y)
        abline(mod,col="red")
        
        #### Aplicar función para detectar datos influyentes 
        datos_influyentes<- influence.measures(mod)
        
    
        #####Extraer filas de los datos influyentes 
        
        influyentes<-summary(datos_influyentes)
        
        filas_filtrar<-as.numeric(row.names(influyentes))
        
        
        ####Base filtrada sin datos influyentes
        
        base_modelo2<-base_modelo[-filas_filtrar, ]
        
        
        ##### Gráfica sin influyentes.
        plot(base_modelo2$x1,base_modelo2$y)
        
        
        

#2. Transformación de variable ####
        
  ## 2.1transformación explicativa####
  
    ###Crear base que incumple supuesto
    
    x1=runif(1000,-80,80)
    e=rnorm(1000,10,500)
    
    y= 5+5*x1^2+ e
    
    
    #### ver graficamente incumplimiento
    plot(x1,y)
    abline(mod)
    mod1<-lm(y~x1)
    
    #### Verificar desempeño con R2 y desviación del modelo
    summary(mod1)
    
    ###Transformar variable para cumplimiento
  
    xmod=x1^2
    
    ### Ajustar modelo con variable transformada
    mod<-lm(y~xmod)
    
    #### Verificar desempeño con R2 y desviación del modelo
    summary(mod)

 
 ## 2.2transformación respuesta####
    
    ###Crear base que incumple supuesto
    
    set.seed(000)
    x1=runif(1000,0,560)
    e=rnorm(1000,1850,1850)
    y= ((200*x1+e)^(1/4))
    
    
    
    #### ver graficamente incumplimiento
  
   
    mod1<-lm(y~x1)

    plot(x1,y)
    abline(mod1)
    #### Verificar desempeño con R2 y desviación del modelo
    summary(mod1)
    
    ###Transformar variable para cumplimiento
    
    trans<- powerTransform(mod1)
    summary(trans)
    
    ymod<-y^4
    ### Ajustar modelo con variable transformada
    mod<-lm(ymod~x1)
    
    par(mfrow=c(1,2))
    
    plot(x1,ymod)
    abline(mod)

    #### Verificar desempeño con R2 y desviación del modelo
    summary(mod)
    
    


  
    
    
#3 Ejercicio ####
    
#generar el mejor modelo  posible (supuestos, R2, significancia)
# para la base precio_viviendas.csv, usando lo visto.