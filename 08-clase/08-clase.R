# Sesion 8: Regresion Lineal Multiple -------------------------------------
## Codigo de analisis

# 1. Cargar paquetes ------------------------------------------------------
pacman::p_load(tidyverse, sjPlot, sjmisc, survey, srvyr)

# 2. Cargar datos ---------------------------------------------------------
datos <- readRDS(file = "output/data/datos_proc.rds")
# 3. Explorar datos -------------------------------------------------------
names(datos)

view_df(datos)

# Mas adelante, en este paso deben verificar que los factores y las dummy estén correctamente
## Solo por ahora veremos para la clase esa transformacion en paso 4

# 4. Modelo ------------------------------------------------------------------
# Modelo nulo sin pesos muestrales y con pesos muestrales -----------------
modelo0_sin <- lm(ing_t_t ~ 1,
              data = datos)
modelo0 <- lm(ing_t_t ~ 1,
              data = datos, weights = fact_cal_esi)
# Resumen del objeto
summary(modelo0);summary(modelo0_sin) ##Los dos al mismo tiempo aparecen en consola


# Modelos simples ---------------------------------------------------------

## Edad
modelo1 <- lm(ing_t_t ~ edad,
              data = datos, weights = fact_cal_esi)
summary(modelo1)

## Con sexo
modelo2_sin <- lm(ing_t_t ~ sexo,
              data = datos, weights = fact_cal_esi)

### Predictores categoricos
datos$sexo <-  forcats::as_factor(datos$sexo) #Ojo: esto deberia ir en codigo de preparacion
modelo2 <- lm(ing_t_t ~ sexo,
              data = datos, weights = fact_cal_esi)

### ¿Como se haría para modelo 3 y 4
datos$ciuo08 <-  forcats::as_factor(datos$ciuo08) #Ojo: esto deberia ir en codigo de preparacion
modelo3 <- lm(ing_t_t ~ ciuo08,
              data = datos, weights = fact_cal_esi)

summary(modelo3)

datos$est_conyugal <-  forcats::as_factor(datos$est_conyugal) #Ojo: esto deberia ir en codigo de preparacion

modelo4 <- lm(ing_t_t ~ est_conyugal,
              data = datos, weights = fact_cal_esi)

summary(modelo4)
## Modelo multiple (solo se suman predictores con un +)
modelo5 <- lm(ing_t_t ~ edad + sexo + ciuo08 + est_conyugal,
              data = datos, weights = fact_cal_esi)

summary(modelo5)

# Version con glm ------------------------------------------------------
modelo5_glm <- glm(ing_t_t ~ edad + sexo + ciuo08 + est_conyugal,
               family = gaussian(link = "identity"),
               data = datos, weights = fact_cal_esi)

summary(modelo5_glm)
# Version con survey ------------------------------------------------------
esi_design <- as_survey_design(datos, ids = 1, weights = fact_cal_esi)
modelo5_survey <- svyglm(ing_t_t ~ edad + sexo + ciuo08 + est_conyugal,
                         family = gaussian(link = "identity"),
                         design = esi_design)
summary(modelo5_survey)


# Tranformaciones funcionales ---------------------------------------------
# Modelo log-lineal
modelo5_log <- lm(log(ing_t_t) ~ edad + sexo + ciuo08 + est_conyugal,
              data = datos, weights = fact_cal_esi)
#Modelo log-cuadratico
modelo5_log_cuadratico <- lm(log(ing_t_t) ~ edad + (edad)^2 + sexo + ciuo08 + est_conyugal,
                  data = datos, weights = fact_cal_esi)

#Modelo log interaccion
modelo5_log_cuadratico <- lm(log(ing_t_t) ~ edad  + sexo + ciuo08 + est_conyugal +
                               sexo*est_conyugal,
                             data = datos, weights = fact_cal_esi)

# ¡Pueden hacer la transformacion funcional que estimen conveniente!

# Creación exploratoria ---------------------------------------------------
modelo_general <- lm(ing_t_t ~ .,
                     data = datos, weights = datos$fact_cal_esi,
                     na.action= na.omit)

step(modelo_general, keep = nobs)



# 2. Obtener informacion del modelo ---------------------------------------
str(modelo5) #estructura del objeto

# summary
summary(modelo1)
summary(modelo5)

# elementos en particular
modelo5$coefficients
modelo5$coefficients[2]
modelo5$coefficients["edad"]

#¿Que mas puedo extraer? str(modelo5) nos dira estructura
str(summary(modelo5))
summary(modelo5)$fstatistic
summary(modelo5)$r.squared

# Valores predichos
modelo5$fitted.values
get_model_data(modelo5, type = c("pred"))
get_model_data(modelo5, type = "pred", terms = "sexo")

# Adicional: Informacion como dataframe ----------------------------------------------
modelo_df <- broom::augment(modelo5) #puedo guardar como objeto

# 3. Representación gráfica -----------------------------------------------

# 3.1 Tablas --------------------------------------------------------------
tab_model(modelo0) # con uno

tab_model(list(modelo0, modelo1, modelo2))

tab_model(list(modelo0,modelo1,modelo2,modelo3,modelo4,modelo5),
          show.aic = T, # show.[inserte algun estadístico]
          show.reflvl = T,
          p.style = "stars")

# 3.2 Gráficos ------------------------------------------------------------

# Forest plot (default)
plot_model(modelo5)
plot_model(modelo5_log)

# Forest plot - arreglado
plot_model(modelo5_log,
           show.p = T,
           show.values =  T)

# Marginals effects
plot_model(modelo5_log,
           type = "pred")

plot_model(modelo5_log,
           type = "pred",
           terms = "sexo")


plot_model(modelo5_log,
           type = "pred",
           terms = c("sexo", "est_conyugal")) # Veran despues seleccion de algunos terminos

