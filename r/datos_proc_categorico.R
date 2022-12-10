
pacman::p_load(car,tidyverse,sjmisc,sjPlot,dplyr,haven,sjlabelled,forcats, texreg,srvyr,survey, corrplot, ggcorrplot)

options(scipen=999) # evita notación científica

# 1. cargar datos desde el repositorio online dataverse

load(url("https://dataverse.harvard.edu/api/access/datafile/6160180")) # cargar desde repositorio de dataverse

# 2. seleccionar variables de interés -------------------------------------

datos_proc <- elsoc_2021 %>% 
  dplyr::select(idencuesta,
                confianza_social = c02,
                confianza_gob = c05_01,
                confianza_pp = c05_02,
                confianza_congreso = c05_07,
                confianza_pdte = c05_08,
                opina_rrss = c08_04,
                interes_politica = c13,
                informa_politica = c14_02,
                habla_politica = c14_01,
                ident_ideologica = c15,
                asiste_marcha = c08_02,
                valor_movsoc = c20,
                edad = m0_edad,
                sexo = m0_sexo,
                nivel_educ = m01,
                fact_exp02,
                satis_dem = c01,
                estrato,
                c25)

# se filtran observaciones por edad <= 35 y se sobreescribe el objeto

datos_proc <- datos_proc %>% dplyr::filter(edad <= 40)
# el número de observaciones desciende a 882

sum(is.na(datos_proc)) # hay 394 NA
# debería sacar los NA antes de hacer modelos?

# 3. recodificar variables
names(datos_proc)

datos_proc <- datos_proc %>% # se seleccionan las variables poniendo cuidado en conservar el lugar de cada categoría de respuesta
  mutate_at(vars(sexo, satis_dem, c25, confianza_congreso, confianza_pdte,interes_politica,ident_ideologica), ~(as.numeric(.))) %>% 
  mutate(satis_dem = car::recode(.$satis_dem, recodes = c("c(1,2)='Nada o poco satisfecho'; 3 ='Algo satisfecho'; c(4,5)='Bastante o muy satisfecho'; c(-999,-888,-777,-666)= NA"), as.factor = T, 
                                    levels = c('Nada o poco satisfecho', 'Algo satisfecho', 'Bastante o muy satisfecho')),
         sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T,  levels = c('Hombre', 'Mujer')),
         apoyo_dem = car::recode(.$c25, recodes = c("1 = 'La democracia es preferible a cualquier otra forma de gobierno'; 2 = 'En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico'; 3 = 'A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario'; c(-999,-888,-777,-666,4)= NA"), as.factor = T,
                           levels = c('La democracia es preferible a cualquier otra forma de gobierno', 'En algunas circunstancias, un gobierno autoritario puede ser preferible a uno democratico', 'A la gente como uno, nos da lo mismo un regimen democratico que uno autoritario')),
         confianza_congreso = car::recode(.$confianza_congreso, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                        levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_pdte = car::recode(.$confianza_pdte, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), as.factor = T, 
                                      levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_gob = car::recode(.$confianza_gob, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_pp = car::recode(.$confianza_pp, recodes = c("c(1,2)='Nada o poca'; 3='Algo'; c(4,5)='Bastante o mucha'; c(-999,-888,-777,-666)= NA"), 
                                     levels = c("Nada o poca", "Algo", "Bastante o mucha")),
         confianza_social = car::recode(.$confianza_social, recodes = c("1 = 'Casi siempre se puede confiar en las personas'; 2 = 'Casi siempre hay que tener cuidado al tratar con las personas'; c(-999,-888,-777,-666,3)= NA"),
                                        as.factor = T,  levels = c('Casi siempre se puede confiar en las personas', 'Casi siempre hay que tener cuidado al tratar con las personas')),
         interes_politica = car::recode(.$interes_politica, recodes = c("c(1,2)='Nada o poco interesado'; 3='Algo interesado'; c(4,5)='Bastante o muy interesado'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                        levels = c("Nada o poco interesado", "Algo interesado", "Bastante o muy interesado")),
         ident_ideologica = car::recode(.$ident_ideologica, recodes = c("c(0,1,2,3,4)='Izquierda'; 5='Centro'; c(6,7,8,9,10)='Derecha'; 11 = 'Independiente'; 12 = 'Ninguno'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                      levels = c("Izquierda", "Centro", "Derecha", "Independiente", "Ninguno")),
         opina_rrss = car::recode(.$opina_rrss, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                  levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         informa_politica = car::recode(.$informa_politica, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), 
                                        levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         habla_politica = car::recode(.$informa_politica, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), 
                                      levels = c("Nunca o casi nunca", "A veces", "Frecuente o muy frecuentemente")),
         asiste_marcha = car::recode(.$asiste_marcha, recodes = c("c(1,2)='Nunca o casi nunca'; 3='A veces'; c(4,5)='Frecuente o muy frecuentemente'; c(-999,-888,-777,-666)= NA"), as.factor = T,
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
                                                                c(-999,-888,-777,-666)= NA"), as.factor = T, 
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
         edad_tramo = car::recode(.$edad, recodes = c("18:29='Joven';30:40='Adulto joven'"),
                            as.factor = T, levels = c("Joven", "Adulto joven")),
         nivel_educ = car::recode(.$nivel_educ, recodes = c("c(1,2,3) = 'Educación básica'; c(4,5) = 'Educación media'; c(6,7) = 'Educación técnica'; c(8,9) = 'Educación universitaria'; 10 = 'Estudios de posgrado'; c(-999,-888,-777,-666)= NA"), as.factor = T,
                                      levels = c("Educación básica", "Educación media", "Educación ténica", "Educación universitaria", "Estudios de posgrado"))) %>%
  mutate_at(vars(edad_tramo, sexo, satis_dem, apoyo_dem, confianza_congreso, confianza_pdte, confianza_gob, confianza_pp, confianza_social,
                 interes_politica,ident_ideologica,opina_rrss,informa_politica,habla_politica,valor_movsoc,nivel_educ,asiste_marcha), ~(forcats::as_factor(.)))

class(datos_proc$apoyo_dem)
view_df(datos_proc)
frq(datos_proc$edad_tramo)
sum(is.na(datos_proc))
# verificamos cambios -----------------------------------------------------

# asignamos etiquetas

view_df(datos_proc)
datos_proc$satis_dem = set_label(datos_proc$satis_dem, "¿Cuán satisfecho o insatisfecho está usted con el funcionamiento de la democracia en Chile?")
datos_proc$confianza_social = set_label(datos_proc$confianza_social, "Hablando en general, ¿diría usted que se puede confiar en la mayoría de las personas, o que hay que tener cuidado al tratar con ellas?")
datos_proc$confianza_congreso = set_label(datos_proc$confianza_congreso, "Grado de confianza: El Congreso Nacional")
datos_proc$confianza_pdte = set_label(datos_proc$confianza_pdte, "Grado de confianza: El Presidente de la República")
datos_proc$interes_politica = set_label(datos_proc$interes_politica, "¿Qué tan interesado está usted en la política?")
datos_proc$ident_ideologica = set_label(datos_proc$ident_ideologica, "Usando una escala de 0 a 10, donde 0 es ser de “izquierda”, 5 es ser de “centro” y 10 es ser de “derecha”, ¿Dónde se ubicaría usted en esta escala?.")
datos_proc$apoyo_dem = set_label(datos_proc$apoyo_dem, "¿Con cuál de las siguientes frases está usted más de acuerdo?")
datos_proc$c25 = set_label(datos_proc$c25, "¿Con cuál de las siguientes frases está usted más de acuerdo?")
datos_proc$asiste_marcha = set_label(datos_proc$asiste_marcha, "Durante los últimos 12 meses, con cuánta frecuencia usted ha : Asistido a una marcha o manifestación política")
datos_proc$opina_rrss = set_label(datos_proc$opina_rrss, "Durante los últimos 12 meses, con cuánta frecuencia usted ha : Usado las redes sociales para expresar su opinión en temas públicos")
datos_proc$valor_movsoc = set_label(datos_proc$valor_movsoc, "Pensando en la lista de movimientos sociales que a continuación le mostraré, por favor indique ¿cuál es el que usted más valora?")
datos_proc$sexo = set_label(datos_proc$sexo, "Sexo del entrevistada/o")
datos_proc$edad_tramo = set_label(datos_proc$sexo, "Edad por tramo")
datos_proc$nivel_educ = set_label(datos_proc$nivel_educ, "Nivel educacional")
datos_proc$habla_politica = set_label(datos_proc$habla_politica, "Frecuencia: Habla de politica con familiares o amigos")
datos_proc$informa_politica = set_label(datos_proc$informa_politica, "Frecuencia: Se informa sobre politica en medios de comunicacion")
datos_proc$confianza_gob = set_label(datos_proc$confianza_gob, "Grado de confianza: El Gobierno ")
datos_proc$confianza_pp = set_label(datos_proc$confianza_pp, "Grado de confianza: Los Partidos Politicos")



view_df(datos_proc)

# 7. Guardar y exportar los datos ----------------------------------------
  
saveRDS(datos_proc, file = "output/datos/datos_proc.rds")



















