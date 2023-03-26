# 8. Correlaciones
## 8.1 correlación para apoyo_dem_dic x sexo

datos_proc_corr_sexo <- datos_proc %>% select(apoyo_dem_dic, sexo)

## 8.2 crear tabla de contingencia para sexo (hay relación estadísticamente significativa)

ct <- datos_proc_corr_sexo %>% 
  table()

## 8.3 aplicar test chi2

chisq.test(ct)


## 8.4 correlación para apoyo_dem_dic x edad_tramo 

datos_proc_corr_edad_tramo <- datos_proc %>% select(apoyo_dem_dic, edad_tramo)

## 8.5 crear tabla de contingencia para edad_tramo (hay relación estadísticamente significativa)

ct_edad_tramo <- datos_proc_corr_edad_tramo %>% 
  table()

## 8.6 aplicar test chi2

chisq.test(ct_edad_tramo)

## 8.6 correlación para apoyo_dem_dic x nivel_educ (podría haber correlación)

datos_proc_corr_nivel_educ <- datos_proc %>% select(apoyo_dem_dic, nivel_educ)

## 8.7 crear tabla de contingencia 

ct_nivel_educ <- datos_proc_corr_nivel_educ %>% 
  table()

## 8.8 aplicar test chi2

chisq.test(ct_nivel_educ)

## 8.9 correlación para apoyo_dem_dic x interes_politica

datos_proc_corr_interes_politica <- datos_proc %>% select(apoyo_dem_dic, interes_politica)

## 8.10 crear tc

ct_interes_politica <- datos_proc_corr_interes_politica %>% 
  table()

## 8.11 aplicar test chi2

chisq.test(ct_interes_politica)


## 8.12 correlación para apoyo_dem_dic x satis_dem

datos_proc_corr_satis_dem <- datos_proc %>% select(apoyo_dem_dic, satis_dem)

## 8.13 crear tc

ct_satis_dem <- datos_proc_corr_satis_dem %>% 
  table()

## 8.14 aplicar test chi2

chisq.test(ct_satis_dem)



## conocer el número de observaciones por variable

sapply(datos_proc, function(x) sum(!is.na(x)))



## class dependent
class(datos_proc$apoyo_dem_dic)