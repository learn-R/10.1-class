# 2. Librerías a utilizar

pacman::p_load(sjPlot, 
               tidyverse, 
               magrittr,
               car)



# 3. Importar datos

# Una vez cargado los paquetes a utilizar, debemos cargar los datos procesados.


load(url("https://github.com/learn-R/09-class/raw/main/output/data/datos_proc.RData"))


## Explorar datos

# Es relevante explorar los datos que utilizaremos, cómo están previamente procesados ¡no sabemos con que variables estamos trabajando!
  
names(datos_proc)

head(datos_proc)

sjPlot::view_df(datos_proc,
                encoding = "UTF-8")
### Recodificar

datos_proc$educacion <- as_factor(datos_proc$educacion)

# Para luego lo recodificaremos con la función `recode` del paquete `car`

datos_proc$educacion <- car::recode(datos_proc$educacion, recodes = c("'Nivel ignorado' = NA; 
                                                  c('Educación primaria (nivel 1)', 'Educación primaria (nivel 2)') = 'Educación primaria'"))

# Finalmente visualizamos los cambios de nuestra base procesada con `view_df`     

sjPlot::view_df(datos_proc,
                encoding = "UTF-8")


# 4. Modelos de regresión

## Regresión lineal simple

reg_1 <-lm((ingresos ~ edad), data = datos_proc)
reg_1

#pero el problema es que al observar el objeto creado, no es muy presentable para informes, por eso usaremos la función `tab_model` de `sjPlot`

sjPlot::tab_model(reg_1, show.ci=FALSE,  encoding = "UTF-8")


## Regresión múltiple

# Ahora queremos incorporar las demás variables al modelo, para lo haremos de la siguiente manera


reg_2 <-lm((ingresos ~ edad + sexo), data = datos_proc)

sjPlot::tab_model(reg_2, show.ci=FALSE,  encoding = "UTF-8")

#¡Pero espera! ¡`sexo` no es una variable continua!
  
## Predictores categoricos 
  
# Previo a esto hay que recordar que `sexo` no es un predictor continuo, y también debemos recordárselo a la base de datos (la variable `educación` tampoco lo es, pero ya la transformamos con `as_factor`)

datos_proc$sexo <- as_factor(datos_proc$sexo)

# Perfecto ahora si podemos añadir predictores categóricos a nuestra regresión múltiple

reg_2 <-lm((ingresos ~ edad + educacion), data = datos_proc)
reg_2
reg_3 <-lm((ingresos ~ edad + educacion + sexo), data = datos_proc)
reg_3

# Pero que pasa si queremos incluir todos los modelos creados en una sola tabla, para eso usaremos nuevamente la función `tab_model` de `sjPlot` 


sjPlot::tab_model(list(reg_1, reg_2, reg_3), # los modelos estimados
                  show.ci=FALSE, # no mostrar intervalo de confianza (por defecto lo hace)
                  p.style = "stars", # asteriscos de significación estadística
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"), # etiquetas de modelos o variables dep.
                  string.pred = "Predictores", string.est = "β", # nombre predictores y símbolo beta en tabla
                  encoding =  "UTF-8")

# 5. Visualización 

# Para visualizar o graficar los coeficientes de regresión para poder observar el impacto de cada variable en el modelo utilizaremos la función `plot_model` de `sjPlot`

sjPlot::plot_model(reg_3, ci.lvl = c(0.95), title = "Estimación de predictores", vline.color = "purple")

# Terminamos por este práctico ¡Pero aún falta la regresión logística!