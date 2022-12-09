
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
                idencuesta,
                estrato)



# se filtran observaciones por edad <= 35 y se sobreescribe el objeto

datos_proc <- datos_proc %>% dplyr::filter(edad <= 30)
# el número de observaciones desciende a 364


# 3. recodificar variables
names(datos_proc)

datos_proc_test <- datos_proc %>% 
  mutate_at(vars(sexo, satis_dem, c25, confianza_congreso, confianza_pdte,interes_politica,ident_ideologica), funs(as.numeric(.))) %>% 
  mutate(satis_dem = car::recode(.$satis_dem, recodes = c("c(1,2)='Nada o poco satisfecho'; 3 ='Algo satisfecho'; c(4,5)='Bastante o muy satisfecho'; c(-999,-888,-777,-666)= NA"), as.factor = T, 
                                    levels = c('Nada o poco satisfecho', 'Algo satisfecho', 'Bastante o muy satisfecho')),
         sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T,  levels = c('Hombre', 'Mujer')),
         apoyo_dem = car::recode(.$c25, recodes = c("1 = 'La democracia es preferible a cualquier otra forma de gobierno'; 2 = 'En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico'; 3 = 'A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario'; c(-999,-888,-777,-666,4)= NA"), as.factor = T,
                           levels = c('La democracia es preferible a cualquier otra forma de gobierno', 'En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico', 'A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario')),
         confianza_congreso = car::recode(.$confianza_congreso, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), 
                                        levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_pdte = car::recode(.$confianza_pdte, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_gob = car::recode(.$confianza_gob, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_pp = car::recode(.$confianza_pp, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), 
                                     levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_social = car::recode(.$confianza_social, recodes = c("1 = 'Casi siempre se puede confiar en las personas'; 2 = 'Casi siempre hay que tener cuidado al tratar con las personas'; c(-999,-888,-777,-666,3)= NA"),
                                        as.factor = T,  levels = c('Casi siempre se puede confiar en las personas', 'Casi siempre hay que tener cuidado al tratar con las personas')),
         interes_politica = car::recode(.$interes_politica, recodes = c("c(1,2)='Nada o poco interesado'; 3='Algo interesado'; c(4,5)='Bastante o muy interesado'; c(-999,-888,-777,-666)= NA"), 
                                        levels = c("Nada o poco interesado", "Algo interesado", "Bastante o muy interesado")),
         ident_ideologica = car::recode(.$ident_ideologica, recodes = c("c(0,1,2,3,4)='Izquierda'; 5='Centro'; c(6,7,8,9,10)='Derecha'; 11 = 'Independiente'; 12 = 'Ninguno'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Izquierda", "Centro", "Derecha", "Independiente", "Ninguno")),
         opina_rrss = car::recode(.$opina_rrss, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), 
                                  levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         informa_politica = car::recode(.$informa_politica, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), 
                                        levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         habla_politica = car::recode(.$informa_politica, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         valor_movsoc = car::recode(.$valor_movsoc, recodes = c("1= 'Movimiento social de apoyo a la causa estudiantil';
                                                                2 = 'Movimiento social de apoyo a demandas laborales';
                                                                3 = 'Movimiento social de grupos ambientalistas';
                                                                4 = 'Movimiento social de apoyo a las demandas indigenas';
                                                                5 = 'Movimiento social de apoyo a la diversidad sexual';
                                                                6 = 'Movimiento social provida o antiaborto';
                                                                7 = 'Movimiento social antidelincuencia';
                                                                8 = 'Movimiento feminista o de apoyo a la igualdad de genero';
                                                                9 = 'Movimiento por el cambio al sistema de pensiones';
                                                                10 = 'Estallido social del 18 de Octubre de 2019';
                                                                11 = 'Otro';
                                                                12 = 'Ninguno';
                                                                c(-999,-888,-777,-666)= NA"), 
                                    levels = c("Movimiento social de apoyo a la causa estudiantil",
                                               "Movimiento social de apoyo a demandas laborales",
                                               "Movimiento social de grupos ambientalistas",
                                               "Movimiento social de apoyo a las demandas indigenas",
                                               "Movimiento social de apoyo a la diversidad sexual",
                                               "Movimiento social provida o antiaborto",
                                               "Movimiento social antidelincuencia",
                                               "Movimiento feminista o de apoyo a la igualdad de genero",
                                               "Movimiento por el cambio al sistema de pensiones",
                                               "Estallido social del 18 de Octubre de 2019",
                                               "Otro",
                                               "Ninguno")),
         edad_tramo = car::recode(.$edad, recodes = c("18:25='Joven';26:35='Adulto joven'; 36:60='Adulto'; 61:hi = 'Adulto mayor'"),
                            as.factor = T, levels = c("Joven", "Adulto joven", "Adulto", "Adulto mayor")),
         nivel_educ = car::recode(.$nivel_educ, recodes = c("c(1,2,3) ='Educación básica'; c(4,5) = 'Educación media'; c(6,7) = 'Educación técnica'; c(8,9) = 'Educación universitaria'; 10 = 'Estudios de posgrado'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Educación básica", "Educación media", "Educación ténica", "Educación universitaria", "Estudios de posgrado"))) %>%
  mutate_at(vars(edad_tramo, sexo, satis_dem, apoyo_dem, confianza_congreso, confianza_pdte, confianza_gob, confianza_pp, confianza_social,
                 interes_politica,ident_ideologica,opina_rrss,informa_politica,habla_politica,valor_movsoc,nivel_educ), funs(forcats::as_factor(.)))

names(datos_proc)
# verificamos cambios -----------------------------------------------------

head(datos_proc_test)


# 7. Guardar y exportar los datos ----------------------------------------
  
saveRDS(datos_proc_test, file = "output/datos/datos_proc_cat.rds")







## descriptivos borrador

# preferencia a la democracia según sexo
datos_proc %>% 
  group_by(sexo) %>%
  frq(c25)

















