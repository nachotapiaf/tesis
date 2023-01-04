
pacman::p_load(car,tidyverse,sjmisc,sjPlot,dplyr,haven,sjlabelled,forcats, texreg,srvyr,survey)

options(scipen=999) # evita notación científica

# 1. cargar datos desde el repositorio online dataverse

load(url("https://dataverse.harvard.edu/api/access/datafile/6160180")) # cargar desde repositorio de dataverse

# 2. seleccionar variables de interés -------------------------------------

datos_proc <- elsoc_2021 %>% 
  dplyr::select(idencuesta,
                confianza_gob = c05_01,
                confianza_pp = c05_02,
                confianza_congreso = c05_07,
                confianza_pdte = c05_08,
                opina_rrss = c08_04,
                interes_politica = c13,
                informa_politica = c14_02,
                habla_politica = c14_01,
                ident_ideologica = c15,
                edad = m0_edad,
                sexo = m0_sexo,
                nivel_educ = m01,
                fact_exp02,
                satis_dem = c01,
                c25)

# 3. Sumar NA antes de procesar -------------------------------------------

sum(is.na(datos_proc))

# 4. recodificación ----------------------------------------------------------

datos_proc <- datos_proc %>% # se seleccionan las variables poniendo cuidado en conservar el lugar de cada categoría de respuesta
  mutate_at(vars(sexo, satis_dem, c25, confianza_congreso, confianza_pdte,interes_politica,ident_ideologica, opina_rrss,informa_politica,
                 habla_politica,), ~(as.numeric(.))) %>% 
  mutate(satis_dem = car::recode(.$satis_dem, recodes = c("c(1,2)='Nada o poco satisfecho'; 3 ='Algo satisfecho'; c(4,5)='Bastante o muy satisfecho'; c(-999,-888,-777,-666)= NA"), as.factor = T, 
                                    levels = c('Nada o poco satisfecho', 'Algo satisfecho', 'Bastante o muy satisfecho')),
         sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T,  levels = c('Hombre', 'Mujer'), drop_na(apoyo_dem)),
         apoyo_dem = car::recode(.$c25, recodes = c("1 = 'La democracia es preferible a cualquier otra forma de gobierno'; 2 = 'En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico'; 3 = 'A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario'; c(-999,-888,-777,-666,4)= NA"), as.factor = T,
                                     levels = c('La democracia es preferible a cualquier otra forma de gobierno','En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico', 'A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario')),
         confianza_congreso = car::recode(.$confianza_congreso, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                        levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_pdte = car::recode(.$confianza_pdte, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), as.factor = T, 
                                      levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_gob = car::recode(.$confianza_gob, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                      levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_pp = car::recode(.$confianza_pp, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                     levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         interes_politica = car::recode(.$interes_politica, recodes = c("c(1,2)='Nada o poco interesado'; 3='Algo interesado'; c(4,5)='Bastante o muy interesado'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                        levels = c("Nada o poco interesado", "Algo interesado", "Bastante o muy interesado")),
         ident_ideologica = car::recode(.$ident_ideologica, recodes = c("c(0,1,2,3,4)='Izquierda'; 5='Centro'; c(6,7,8,9,10)='Derecha'; 11 = 'Independiente'; 12 = 'Ninguno'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                      levels = c("Izquierda", "Centro", "Derecha", "Independiente", "Ninguno")),
         opina_rrss = car::recode(.$opina_rrss, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                  levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         informa_politica = car::recode(.$informa_politica, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                        levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         habla_politica = car::recode(.$informa_politica, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                      levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         edad_tramo = car::recode(.$edad, recodes = c("18:29='Joven';30:65='Adulto'; 66:hi ='Adulto mayor'"),
                            as.factor = T, levels = c("Joven", "Adulto","Adulto mayor")),
         nivel_educ = car::recode(.$nivel_educ, recodes = c("c(1,2,3) = 'Primaria'; c(4,5) = 'Secundaria'; c(6,7) = 'Educación técnica'; c(8,9,10) = 'Universitaria o posgrado'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                      levels = c("Primaria", "Secundaria", "Educación técnica", "Universitaria o posgrado"))) %>%
  mutate_at(vars(edad_tramo, sexo, satis_dem, apoyo_dem, confianza_congreso, confianza_pdte, confianza_gob, confianza_pp,
                 interes_politica,ident_ideologica,opina_rrss,informa_politica,habla_politica,nivel_educ), ~(forcats::as_factor(.)))


str(datos_proc) # se verifican los cambios en los datos

## 4.1 transformar variable de apoyo a la democracia en dicotómica desde variable recodificada de apoyo_dem 
## esto crea una nueva variable

datos_proc <- datos_proc %>% # esto crea una variable dicotómica con dos niveles y deja la tercera categoría de respuesta como NA
  mutate(apoyo_dem_dic = ifelse(apoyo_dem == "La democracia es preferible a cualquier otra forma de gobierno",
                                        "Preferencia por democracia",
                                        ifelse(apoyo_dem == "En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico",
                                               "Preferencia por autoritarismo",
                                               NA))) %>%
  mutate(apoyo_dem_dic = factor(apoyo_dem_dic,
                                        levels = c("Preferencia por democracia", "Preferencia por autoritarismo")))


# 4.2 Se revisan los datos para ver cómo quedaron ---------------------------------------------------------------------

str(datos_proc)
frq(datos_proc$apoyo_dem_dic)

# 5. se revisan NA  -------------------------------------------------------
sum(is.na(datos_proc))


# 5.1 se eliminan NA y se comprueba ---------------------------------------

sum(is.na(datos_proc))
datos_proc <- na.omit(datos_proc) # se eliminan NA al no poder imputar datos y en orden a crear modelos de regresión logística
sum(is.na(datos_proc))

datos_proc %>% # se verifican NA por cada variable
summarize_all(funs(sum(is.na(.)))) 


# 6. asignamos etiquetas

view_df(datos_proc)
datos_proc$satis_dem = set_label(datos_proc$satis_dem, "¿Cuán satisfecho o insatisfecho está usted con el funcionamiento de la democracia en Chile?")
datos_proc$confianza_social = set_label(datos_proc$confianza_social, "Confianza Social Generalizada")
datos_proc$confianza_congreso = set_label(datos_proc$confianza_congreso, "Grado de confianza: El Congreso Nacional")
datos_proc$confianza_pdte = set_label(datos_proc$confianza_pdte, "Grado de confianza: El Presidente de la República")
datos_proc$interes_politica = set_label(datos_proc$interes_politica, "¿Qué tan interesado está usted en la política?")
datos_proc$ident_ideologica = set_label(datos_proc$ident_ideologica, "Autoubicacion escala izquierda-derecha")
datos_proc$apoyo_dem = set_label(datos_proc$apoyo_dem, "¿Con cuál de las siguientes frases está usted más de acuerdo?")
datos_proc$c25 = set_label(datos_proc$c25, "¿Con cuál de las siguientes frases está usted más de acuerdo?")
datos_proc$opina_rrss = set_label(datos_proc$opina_rrss, "Frecuencia: Usa redes sociales para opinar en temas publicos")
datos_proc$sexo = set_label(datos_proc$sexo, "Sexo del entrevistada/o")
datos_proc$edad_tramo = set_label(datos_proc$edad_tramo, "Edad por tramo")
datos_proc$nivel_educ = set_label(datos_proc$nivel_educ, "Nivel educacional")
datos_proc$habla_politica = set_label(datos_proc$habla_politica, "Frecuencia: Habla de politica con familiares o amigos")
datos_proc$informa_politica = set_label(datos_proc$informa_politica, "Frecuencia: Se informa sobre politica en medios de comunicacion")
datos_proc$confianza_gob = set_label(datos_proc$confianza_gob, "Grado de confianza: El Gobierno ")
datos_proc$confianza_pp = set_label(datos_proc$confianza_pp, "Grado de confianza: Los Partidos Politicos")
datos_proc$apoyo_dem_dic = set_label(datos_proc$apoyo_dem_dic, "Democracia o autoritarismo")

view_df(datos_proc)

# 7. Guardar y exportar los datos ----------------------------------------
  
saveRDS(datos_proc, file = "output/datos/datos_proc.rds")





