# Sesion 8: Regresion Lineal Multiple -------------------------------------
## Codigo de Procesamiento

# 1. Cargar paquetes ------------------------------------------------------
pacman::p_load(tidyverse, sjPlot, sjmisc)

# 2. Cargar datos ---------------------------------------------------------
datos <- haven::read_dta("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/stata_esi/2020/esi-2020---personas.dta?sfvrsn=7a4b0e2b_4&amp;download=true")

# 3. Explorar datos -------------------------------------------------------
names(datos)

view_df(datos)

sjmisc::find_var(datos, "conglomerado")

# 4. Procesar variables ------------------------------------------------
datos_proc <- datos %>%
  select(ing_t_t, edad, sexo, est_conyugal, ciuo08 = b1, fact_cal_esi) %>% 
  mutate(est_conyugal = if_else(est_conyugal %in% c(1,2), "Con pareja",
                                if_else(est_conyugal %in% c(3,4,5,6),"Sin pareja", NA_character_))) %>% 
  filter(ing_t_t > 0)


# 5. Guardar --------------------------------------------------------------
saveRDS(datos_proc, file = "output/data/datos_proc.rds")


