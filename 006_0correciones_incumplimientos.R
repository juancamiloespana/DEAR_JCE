
library(car) ###Power transform 
library(lmtest) #### para funciones bptest y bgtest



#1. Datos inusuales #####

    ##1.1 basado en cada variable de manera independiente - Eliminar ####
        
        ####Cargar data frame de github 'defectos.csv'
ruta_def='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/defectos.csv'
df_def=read.csv(ruta_def)
     
      ###Graficar para identificar datos inusuales
plot(df_def$defectos~df_def$empleados_calidad)


        ####Ajustar modelo y verificar desempeño ###
mod1=lm(defectos~empleados_calidad, data= df_def)
abline(mod1, col='red', lwd=2)

  ##### verificar supuestos del modelo

res=mod1$residuals
        
shapiro.test(res) ## prueba para normalidad
bptest(mod1) ## para homocedasticidad
bgtest(mod1) ### para independencia
        
          boxplot(df_def$empleados_calidad)
        
        ####Calcular límites para los atípicos
        q3=quantile(df_def$empleados_calidad, 0.75)
        riq=IQR(df_def$empleados_calidad)
        lim_sup=q3 + 1.5*riq
        
        q1=quantile(df_def$empleados_calidad, 0.25)
        lim_inf=q1-(1.5*riq)
       

        ####Filtrar base para que se eliminen atípicos
        df_deff=subset(df_def, df_def$empleados_calidad < lim_sup & df_def$empleados_calidad >lim_inf)
        

        #### Verificar base despues de eliminar atípicos
plot(df_deff$defectos~df_deff$empleados_calidad)
        
  
        ####Ajustar modelo y verificar desempeño ###
mod2=lm(defectos~empleados_calidad, data=df_deff)
        

      ##### verificar supuestos del modelo 
res2=mod2$residuals
shapiro.test(res2)
bptest(mod2)
bgtest(mod2)
        
        
        
        
    ##1.2  datos inusuales Efectos sobre el modelo #####
        
    ####analizar data frame 'plantas.csv'
          

        
        ###supuestos ###
        

        
        ########## Eliminación puntos de influencia
        
  
        
        #### graficar sin puntos de influencia

        
        
        ###ajustar modelo con datos influyentes eliminados
        

        
        #### volver a validar supuestos
        


#2. Transformación de variable ####
        

 ## 2.1transformación respuesta####
    
    ###cargar base energia de github
    
    #### graficar y austar modelo

    ###Analizar modelo y supuestos


    ### analizar posible transformacion de respuesta

    ### aplicar transformacion recomendada

    ### graficar con transformacion
    

    
    ### ajustar modelo con transfomracion y graficar


    ### analizar modelo y supuestos 

    
    
## 2.1transformación explicativa####
    
    ###Crear base que incumple supuesto
    
    ###cargar y Analizar base 'flujo.csv' 
    
    
    ##### ajustar modelo
    
    
    #### graficar modelo y variables explicativa y respuesta
    
    
    ##### analizar modelo y validar supuestos        
    
    
    ### transformar explicativa y graficar
    
    #### ajustar modelo transformado
    
    ### Analizar modelo y supuestos despues de transformacion
    
    
    
    

  
    
  