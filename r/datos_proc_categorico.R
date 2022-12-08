
pacman::p_load(car,tidyverse,sjmisc,sjPlot,dplyr,haven,sjlabelled,forcats, texreg,srvyr,survey, corrplot, ggcorrplot)

options(scipen=999) # evita notación científica

# 1. cargar datos desde el repositorio online dataverse

load(url("https://dataverse.harvard.edu/api/access/datafile/6160180")) # cargar desde repositorio de dataverse

# 2. seleccionar variables de interés -------------------------------------

datos_proc <- elsoc_2021 %>% 
  dplyr::select(satis_dem = c01,
                confianza_social = c02,
                c25,
                confianza_gob = c05_01,
                confianza_pp = c05_02,
                confianza_congreso = c05_07,
                confianza_pdte = c05_08,
                opina_rrss = c08_04,
                interes_politica = c13,
                informa_politica = c14_02,
                habla_politica = c14_01,
                ident_ideologica = c15,
                valor_movsoc = c20,
                edad = m0_edad,
                sexo = m0_sexo,
                nivel_educ = m01,
                fact_exp02,
                idencuesta)

# se filtran observaciones por edad <= 35 y se sobreescribe el objeto

datos_proc <- datos_proc %>% dplyr::filter(edad <= 30)
# el número de observaciones desciende a 364


# 3. recodificar variables
names(datos_proc)

datos_proc_f <- datos_proc %>% 
  mutate_at(vars(sexo, satis_dem, c25, confianza_congreso, confianza_pdte,interes_politica,ident_ideologica), funs(as.numeric(.))) %>% 
  mutate(satis_dem = car::recode(.$satis_dem, recodes = c("c(1,2)='Nada o poco satisfecho'; 3 ='Algo satisfecho'; c(4,5)='Bastante o muy satisfecho'; c(-999,-888,-777,-666)= NA"), as.factor = T, 
                                    levels = c('Nada o poco satisfecho', 'Algo satisfecho', 'Bastante o muy satisfecho')),
         sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T,  levels = c('Hombre', 'Mujer')),
         c25 = car::recode(.$c25, recodes = c("1 = 'La democracia es preferible a cualquier otra forma de gobierno'; 2 = 'En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico'; 3 = 'A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario'; c(-999,-888,-777,-666,4)= NA"), as.factor = T,
                           levels = c('La democracia es preferible a cualquier otra forma de gobierno', 'En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico', 'A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario')),
         confianza_congreso = car::recode(.$confianza_congreso, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), 
                                        levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_pdte = car::recode(.$confianza_pdte, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         interes_politica = car::recode(.$interes_politica, recodes = c("c(1,2)='Nada o poco interesado'; 3='Algo interesado'; c(4,5)='Bastante o muy interesado'; c(-999,-888,-777,-666)= NA"), 
                                        levels = c("Nada o poco interesado", "Algo interesado", "Bastante o muy interesado")),
         ident_ideologica = car::recode(.$ident_ideologica, recodes = c("c(0,1,2,3,4)='Izquierda'; 5='Centro'; c(6,7,8,9,10)='Derecha'; 11 = 'Independiente'; 12 = 'Ninguno'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Izquierda", "Centro", "Derecha", "Independiente", "Ninguno")))
  mutate_at(vars(sexo, satis_dem, c25, confianza_congreso, confianza_pdte,interes_politica,ident_ideologica), funs(forcats::as_factor(.)))#Transformo las variables en un factor para que mi modelo se estime de manera correcta,



# verificamos cambios -----------------------------------------------------

head(datos_proc_f)


# 7. Guardar y exportar los datos ----------------------------------------
  
saveRDS(datos_proc_f, file = "output/datos/datos_proc.rds")




