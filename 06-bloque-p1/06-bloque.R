
# 2. Cargar librerías --------------------------------------------------------

pacman::p_load(sjPlot, 
               tidyverse, 
               sjmisc,
               dplyr,
               haven,
               magrittr,
               car)



# 3. Importar datos -------------------------------------------------------


# Una vez cargado los paquetes a utilizar, debemos cargar los datos procesados.


load(url("https://github.com/learn-R/09-class/raw/main/output/data/datos_proc.RData"))


# Explorar datos ----------------------------------------------------------

# Es relevante explorar los datos que utilizaremos, cómo están previamente procesados ¡no sabemos con que variables estamos trabajando!
  
names(datos_proc)

head(datos_proc)

sjPlot::view_df(datos_proc,
                encoding = "UTF-8")


# Recodificar -------------------------------------------------------------


# Primero, transformamos las variables sexo y educacion a tipo factor con la función
# `as_factor()` del paquete haven

datos_proc$educacion <- as_factor(datos_proc$educacion)
class(datos_proc$educacion)

# Recodificaremos con la función `recode` del paquete `car`

datos_proc$educacion <- car::recode(datos_proc$educacion, recodes = c("'Nivel ignorado' = NA; 
                                                  c('Educación primaria (nivel 1)', 'Educación primaria (nivel 2)') = 'Educación primaria'; 
                                                  'Educación técnica (Educación superior no universitaria)' = 'Educación técnica'")) 

# Y luego transformaremos en factor, asignando los niveles respectivos

datos_proc$educacion <- factor(datos_proc$educacion, levels = c('Nunca estudió', 
                                                      'Educación preescolar', 
                                                      'Educación primaria', 
                                                      'Educación secundaria', 
                                                      'Educación técnica', 
                                                      'Educación universitaria', 
                                                      'Postitulos y maestría', 
                                                      'Doctorado'))

# Finalmente visualizamos los cambios de nuestra base procesada con `view_df`     

sjPlot::view_df(datos_proc,
                encoding = "UTF-8")



# 4. Modelos de regresión -------------------------------------------------


# Regresión lineal simple -------------------------------------------------


reg_1 <-lm((ingresos ~ edad), data = datos_proc)
reg_1

#pero el problema es que al observar el objeto creado, no es muy presentable para informes, por eso usaremos la función `tab_model` de `sjPlot`

sjPlot::tab_model(reg_1, show.ci=FALSE,  encoding = "UTF-8")



# Regresión múltiple ------------------------------------------------------

# Ahora queremos incorporar las demás variables al modelo, para lo haremos de la siguiente manera


reg_2 <-lm((ingresos ~ edad + sexo), data = datos_proc)

sjPlot::tab_model(reg_2, show.ci=FALSE,  encoding = "UTF-8")

#¡Pero espera! ¡`sexo` no es una variable continua!
  

# Predictores categóricos -------------------------------------------------
  
# Previo a esto hay que recordar que `sexo` no es un predictor continuo, y también debemos recordárselo a la base de datos (la variable `educación` tampoco lo es, pero ya la transformamos con `as_factor`)

datos_proc$sexo <- as_factor(datos_proc$sexo)

# Perfecto, ahora si podemos añadir predictores categóricos a nuestra regresión múltiple

reg_2 <-lm((ingresos ~ edad + educacion), data = datos_proc)
reg_2
reg_3 <-lm((ingresos ~ edad + educacion + sexo), data = datos_proc)
reg_3

# Pero ¿qué pasa si queremos incluir todos los modelos creados en una sola tabla? para eso usaremos nuevamente la función `tab_model` de `sjPlot` 


sjPlot::tab_model(list(reg_1, reg_2, reg_3), # los modelos estimados
                  show.ci=FALSE, # no mostrar intervalo de confianza (por defecto lo hace)
                  p.style = "stars", # asteriscos de significación estadística
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"), # etiquetas de modelos o variables dep.
                  string.pred = "Predictores", string.est = "β", # nombre predictores y símbolo beta en tabla
                  encoding =  "UTF-8")


# 5. Visualización --------------------------------------------------------

# Para visualizar o graficar los coeficientes de regresión para poder observar el impacto de cada variable en el modelo utilizaremos la función `plot_model` de `sjPlot`

sjPlot::plot_model(reg_3, ci.lvl = c(0.95), title = "Estimación de predictores", vline.color = "purple")


# 6. Regresión logística --------------------------------------------------

# La regresión lineal es sumamente útil cuando estamos trabajando con variables continuas,
# como ingresos. Sin embargo, la mayor parte de las variables que nos interesa explicar
# en ciencias sociales tienden a ser categóricas, como la identidad de clase, grado de
# acuerdo con las medidas del gobierno, entre otras. La regresión logística es un modelo 
# que nos permitirá explicar o predecir variables de este tipo, en particular, variables
# dicotómicas (que presentan dos categorías de respuesta)

# Lo primero que debemos hacer es crear una variable dicotómica que deseemos explicar. En
# este caso, la variable refiere a si los sujetos se encuentran por sobre la media de 
# ingresos (1) o no (0)

datos_proc$ingresos_d <- ifelse(datos_proc$ingresos>mean(datos_proc$ingresos), 1, 0)

frq(datos_proc$ingresos_d)

# Una vez realizado lo anterior, es momento de generar nuestro modelo de regresión 
# logística. Emplearemos la función `glm()` del paquete base de `R`, especificando el 
# argumento `family` como `"binomial"`. Lo primero es especificar las variables con 
# las cuales construiremos el modelo: antes de la virgulilla (~) escribiremos nuestra 
# variable dependiente (en este caso, `ingresos_d`), y luego, las variables independientes 
# (aquí, `educacion`, `sexo` y `edad`), separadas con un signo más (+).

reg_log <- glm(ingresos_d~educacion+sexo+edad, data = datos_proc, family = "binomial")


# Visualización de regresión logística ------------------------------------

# La visualización de modelos de regresión logística es muy similar a la de modelos de 
# regresión lineal. Empleamos el comando `tab_model()` de la librería `sjPlot`
# Por defecto, para las medidas de ajuste, se presenta el R^2 de Tjur (Pseudo-R^2)

sjPlot::tab_model(reg_log, 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = "Modelo",
                  string.pred = "Predictores", 
                  string.est = "β")

# Para el caso de las variables categóricas (que han sido transformadas a tipo `factor`), 
# R toma como categoría de referencia al primer nivel (`level`) de la variable. En el caso 
# de `educacion`, por ejemplo, la categoría de referencia es Nunca estudió; y para 
# `sexo`, la categoría de referencia es Hombre. Sin embargo, podemos emplear la función 
# `relevel` para establecer una nueva categoría de referencia para una variable en 
# particular. Probemos estableciendo la categoría Mujer como la referencia para la 
# variable `sexo`:

datos_proc$sexo_r <- relevel(datos_proc$sexo, ref = 'Mujer') #Creamos nueva variable
                                                             # sexo_r (relevel)

#Creamos un nuevo modelo de reg. logística con sexo_r y comparamos con el modelo anterior

reg_log2 <- glm(ingresos_d~educacion+sexo_r+edad, data = datos_proc, family = "binomial")

sjPlot::tab_model(list(reg_log, reg_log2), 
                  show.ci=FALSE, 
                  p.style = "stars", 
                  dv.labels = c("Modelo", "Modelo (Relevel)"),
                                string.pred = "Predictores", 
                                string.est = "β")

# No obstante, por defecto el comando `tab_model()` arroja los coeficientes como 
# logaritmos de las chanches (Log-odds), lo cual dificulta la interpretación de los 
# coeficientes. Para que el output de la tabla se presente como Odds-ratio, debemos 
# especificar el argumento `transform = "exp"` 

sjPlot::tab_model(reg_log,
                  transform = "exp",
                  show.ci=FALSE,
                  p.style = "stars",
                  dv.labels = "Modelo (OR)",
                  string.pred = "Predictores",
                  string.est = "β")

# También podemos graficar la significancia estadística de nuestros coeficientes, con 
# `plot_model()` de `sjPlot`. Por defecto, esta función presenta los coeficientes como 
# Odds-ratio

plot_model(reg_log,
           ci.lvl = (0.95),
           title = "Estimación de predictores",
           vline.color = "purple",
           show.values = TRUE)+ 
  theme_sjplot()

 