
# 1. cargar librerías -------------------------------------------------

pacman::p_load(car,tidyverse,sjmisc,sjPlot,dplyr,haven,sjlabelled,forcats, texreg,srvyr,survey, corrplot, ggcorrplot)

options(scipen=999) # evita notación científica


# 2. cargar datos --------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/6160180")) # cargar desde repositorio de dataverse

# 3. exploración inicial -----------------------------------------------

frq(elsoc_2021$cuestion_completo)
frq(elsoc_2021$c15)
find_var(elsoc_2021, "identific")
class(datos_proc$c25)

# 4. seleccionar variables de interés -------------------------------------

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


# 5. procesar variables ------------------------------------------------------

# frecuencias para cada una (todas presentan valores perdidos (-999,-888,-777,-666))
# Recodificar estableciendo variables como numéricas, asignando casos perdidos y dejando sólo casos completos
# esto permitirá después construir el objeto encuesta para replicar el muestreo del estudio original
# estos 'datos_proc_regresión' harán alusión a los datos en formato numeric para todas sus variables, o sea, en variables continuas


datos_proc_regresion <- datos_proc %>%
  mutate_at(vars(1:15), ~as.numeric(.)) %>%
  mutate_at(vars(1:15), ~(car::recode(. ,"-999 = NA; -888 = NA; -777 = NA; -666 = NA"))) %>%
  drop_na()

# el número de observaciones vuelve a descender, ahora, a 167 debido a los casos no considerados como NA


summary(datos_proc_regresion) # añadir comentario del por qué llamo a esta tabla. Qué verifico con ella?


# 6. creación de objeto encuesta ------------------------------------------


objeto_elsoc <- datos_proc_regresion %>% #Creamos un nuevo objeto llamado movid_objeto con la información de datos_proc
  as_survey_design(ids = 1, #Aplicamos diseño muestral, especificando los ids a partir de 1 (entrevistado seleccionado),
                   weights = fact_exp02) #y el ponderador con factor de expansión 

# el 'objeto_elsoc' pasa ahora a ser el que contiene los datos de los 'datos_proc_regresion'.
# esto se debe a que con la creación del objeto encuesta, podemos aplicar el diseño muestral del estudio a
# través del uso de factores de expansión

# 7. correlaciones 

corrplot(cor(datos_proc_regresion), method = "shade", title = "Matriz de correlaciones",
         order = 'hclust', tl.col = "grey")

# 8. Incorporación parcial de variables

r1 <- lm(c25 ~ edad, 
         data = objeto_elsoc)

r2 <- lm(c25 ~ edad + satis_dem,
         data = objeto_elsoc)

r3 <- lm(c25 ~ edad + satis_dem + interes_politica,
         data = objeto_elsoc)

r4 <- lm(c25 ~ edad + satis_dem + interes_politica + nivel_educ,
         data = objeto_elsoc)
r5 <- lm(c25 ~ edad + satis_dem + interes_politica + informa_politica + confianza_congreso,
         data = objeto_elsoc)
r6 <- lm(c25 ~ edad + satis_dem + interes_politica + informa_politica + confianza_congreso + confianza_pp,
         data = objeto_elsoc)

# Resultados básicos con el paquete R base
summary(r4)


# 9. Combinamos modelos como una lista

modelos<-list(r1,r2,r3,r4,r5,r6)

# Podemos configurar los resultados para que comas sean decimales
options(OutDec= ",") 

# Función 'screenreg' permite "imprimir" una tabla de resultados más editada

screenreg(modelos,
          custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6"),  
          custom.coef.names = c("Constante", "edad", "satis_dem", "interes_politica","nivel_educ", "confianza_congreso", "confianza_pp", "sexo"))


# 10. descriptivos  -----------------------------------------------------------------------

# interés en la política por sexo

plot_grpfrq(datos_proc$sexo, datos_proc$interes_politica,
            type = "bar", title = "Gráfico de barras")
names(datos_proc_test)

# satisfacción con la democracia según sexo
plot_grpfrq(datos_proc$nivel_educ, datos_proc$satis_dem,
            type = "bar", title = "Gráfico de barras")

# preferencia de la democracia según sexo
plot_grpfrq(datos_proc$sexo, datos_proc$c25,
            type = "bar", title = "Gráfico de barras")




