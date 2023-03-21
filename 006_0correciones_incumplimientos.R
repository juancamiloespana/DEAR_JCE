
library(car) ###Power transform 
library(tseries)
library(nortest)
library(goftest)
library(lmtest)


#1. Datos inusuales #####

    ##1.1 basado en cada variable de manera independiente - Eliminar ####
        
        ####Cargar data frame de github 'defectos.csv'
     
      ###Graficar para identificar datos inusuales

        ####Ajustar modelo y verificar desempeño ###

  ##### verificar supuestos del modelo
        

        
        ####Calcular límites para los atípicos
        

        ####Filtrar base para que se eliminen atípicos

        #### Verificar base despues de eliminar atípicos

        
        ####Ajustar modelo y verificar desempeño ###

        

      ##### verificar supuestos del modelo 

        
        
        
        
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
    
    
    
    

  
    
  