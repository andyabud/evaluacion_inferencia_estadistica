
# Pregunta 1 - Cargar BBDD ------------------------------------------------

# Cargar librerías
library(tidyverse)
library(janitor)
library(samplingbook)
library(survey)
library(sampling)
library(SamplingUtil)

# Cargar bbdd lluvias.csv
data <- read.csv("data/lluvia.csv")

# Revisar variables y clase
glimpse(data)
# Hay un error con las Ñ de la palabra otoño

# Limpiar la base madre y sacarle las ñ
data <- data %>% 
  mutate(Estacion = str_replace_all(Estacion, "<f1>", "n"))
data <- data %>% 
  mutate(Estacion = str_replace_all(Estacion, "�", "n"))

# Extraer un muestreo aleatorio simple de n = 5000, establecer semilla 2022
set.seed(2022)
data_1 <- sample_n(data, size = 5000)

#Limpiar nombres con janitor y sacar las ñ con mutate
data_1 <- data_1 %>% 
  clean_names()
data_1 <- data_1 %>% 
  mutate(estacion = str_replace_all(estacion, "<f1>", "n"))
data_1 <- data_1 %>% 
  mutate(estacion = str_replace_all(estacion, "�", "n"))

# Deshabilitar anotaciones científicas
options(scipen = 999)

# Pregunta 2 --------------------------------------------------------------

## 2.a ---------------------------------------------------------------------
# Para ver la probabilidad hacer una tabla para ver la cantidad de días que sí
# llueve y luego un prop.table
table(data_1$lluvia_hoy) %>% 
  prop.table*100

## La probabilidad que llueva hoy es de 22,88%


## 2.b ---------------------------------------------------------------------
# Ver cuántos días con menos de 8 horas de sol hay en la tabla, para ello
# hace un nuevo dataframe en donde se cuente la cantidad de días que hay <8
# horas de sol, y los que no

data_1_b <- data_1 %>%
  count(sol <8)

# Hacer una tabla de proporciones con el dataframe recién creado
prop.table(data_1_b)*100

## Hay un 43,81% de probabilidad que un día tenga menos de 8 horas de sol


## 2.c ---------------------------------------------------------------------

# Hacer un segundo dataframe con filtro en invierno
data_1_c <- data_1 %>% 
  filter(estacion == "Invierno")%>% 
  count(temp3pm >20)

prop.table(data_1_c)*100

## La probabilidad que la temperatura sea <20º en invierno es de 24,78%


## 2.d ---------------------------------------------------------------------

data_1_d <- data_1 %>% 
  filter(koppen == "Subtropical")

data_1_d %>% 
  count(lluvia >= 5 & lluvia <= 10) %>%
  prop.table()*100

## La posibilidad que llueva entre 5 a 10mm en un clima Koppen Subtropical es de
# 5,37%


## 2.e ---------------------------------------------------------------------
### a - Los resultados varían casi en un 1%----
## Con la muestra aleatoria
table(data_1$lluvia_hoy) %>% 
  prop.table*100 #22.88%

## Con el dataset completo
table(data$LluviaHoy) %>% 
  prop.table()*100 #21.96%


### b - Los resultados varían casi en 0.5%--------------------------------------
## Con la muestra aleatoria
data_1_b <- data_1 %>%
  count(sol <8)

prop.table(data_1_b)*100 #43,81%

## Con el dataset completo
data %>%
  count(Sol <8) %>% 
  prop.table()*100 #44,21%

### c - Los resultados varían más de 1%----
## Con la muestra aleatoria
data_1_c <- data_1 %>% 
  filter(estacion == "Invierno")%>% 
  count(temp3pm >20)

prop.table(data_1_c)*100 # 24,78%

## Con el dataset completo
data %>% 
  filter(Estacion == "Invierno")%>% 
  count(Temp3pm >20) %>% 
  prop.table()*100 # 25,92%

### d - Los resultados varían aproximadamente 0,2%----
## Con la muestra aleatoria
data_1_d <- data_1 %>% 
  filter(koppen == "Subtropical")

data_1_d %>% 
  count(lluvia >= 5 & lluvia <= 10) %>%
  prop.table()*100 # 5,37%

## Con el dataset completo
data %>% 
  filter(Koppen == "Subtropical") %>% 
  count(Lluvia >= 5 & Lluvia <= 10) %>% 
  prop.table()*100 # 5,18%


# Pregunta 3 --------------------------------------------------------------

# Se establece la semilla
set.seed(2022)
# Hacer una tabla de proporciones de la variable Koppen
prop.table(table(data$Koppen))
nstrata(n = 4997, wh = c(0.02985, 0.08825, 0.35635, 0.385875, 0.13967)) 
# Se hizo con un tamaño de muestra menor ya que al ocupar n=5000 entregaba una
# muestra de 5003 sujetos

# Generar un nuevo dataframe estratificado con un N=5000
# Ocupar la lógica de filtrar, luego generar un sample_n
# Luego generar más filas de cada estrato con la función add_row
set.seed(2022)
estrat_koppen <- data %>% 
  filter(Koppen == "Desert") %>% 
  sample_n (150) %>% 
  add_row(data %>% 
            filter(Koppen == "Grassland") %>% 
            sample_n (441)) %>% 
  add_row(data %>% 
            filter(Koppen == "Subtropical") %>% 
            sample_n (1781)) %>% 
  add_row(data %>% 
            filter(Koppen == "Temperate") %>% 
            sample_n (1929)) %>% 
  add_row(data %>% 
            filter(Koppen == "Tropical") %>% 
            sample_n (699))

# Verificar si está correctamente estratificada
table(estrat_koppen$Koppen) %>% 
  prop.table()
table(data$Koppen) %>% 
  prop.table()
# Los valores son casi iguales pero tienen una leve variación


## 3.a ---------------------------------------------------------------------
### Con el dataset original----
table(data_1$lluvia_hoy) %>% 
  prop.table*100
# La probabilidad que llueva hoy es de 22,88%

### Con el dataset nuevo----
table(estrat_koppen$LluviaHoy) %>% 
  prop.table*100
# La probabilidad que llueva hoy es de 22,36%

## 3.b ---------------------------------------------------------------------
### Con el dataset original----
data_1_b <- data_1 %>%
  count(sol <8)

prop.table(data_1_b)*100
# Hay un 43,81% de probabilidad que un día tenga menos de 8 horas de sol

### Con el dataset nuevo----
estrat_sol <- estrat_koppen %>%
  count(Sol <8)

prop.table(estrat_sol)*100
# Hay un 44.09% de probabilidad que un día tenga menos de 8 horas de sol

## 3.c ---------------------------------------------------------------------
### Con el dataset original----
data_1_c <- data_1 %>% 
  filter(estacion == "Invierno")%>% 
  count(temp3pm >20)

prop.table(data_1_c)*100
# La probabilidad que la temperatura sea <20º en invierno es de 24,78%

### Con el dataset nuevo----
estrat_temp_invierno <- estrat_koppen %>% 
  filter(Estacion == "Invierno")%>% 
  count(Temp3pm >20)

prop.table(estrat_temp_invierno)*100
# La probabilidad que la temperatura sea <20º en invierno es de 26,48%

## 3.d ---------------------------------------------------------------------
### Con el dataset original----
data_1_d <- data_1 %>% 
  filter(koppen == "Subtropical")

data_1_d %>% 
  count(lluvia >= 5 & lluvia <= 10) %>%
  prop.table()*100

# La posibilidad que llueva entre 5 a 10mm en un clima Koppen Subtropical es de
# 5,37%

### Con el dataset nuevo----
estrat_subtropical <- estrat_koppen %>% 
  filter(Koppen == "Subtropical")

estrat_subtropical %>% 
  count(Lluvia >= 5 & Lluvia <= 10) %>%
  prop.table()*100

# La posibilidad que llueva entre 5 a 10mm en un clima Koppen Subtropical es de
# 5,10%

## Conclusión ----
# Los datos presentan una leve variación, es difícil decir si los datos han
# Mejorado o no ya que depende cuál sea el parámetro para definir si son
# Mejores o peores


# Pregunta 4 --------------------------------------------------------------
## 4.a----

t.test(estrat_koppen$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Con un 95% de confianza, la temperatura promedio de Australia a las 9:00 AM
# se encuentra en un rango entre 17,92 y 18,28

## 4.b----

# Verano -> 22.36924 22.96673
estrat_koppen_verano <- estrat_koppen %>% 
  filter(Estacion == "Verano")
t.test(estrat_koppen_verano$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Otoño -> 18.02107 18.64323
estrat_koppen_otono <- estrat_koppen %>% 
  filter(Estacion == "Otono")
t.test(estrat_koppen_otono$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Invierno -> 12.35051 12.93993
estrat_koppen_invierno <- estrat_koppen %>% 
  filter(Estacion == "Invierno")
t.test(estrat_koppen_invierno$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Primavera -> 18.52897 19.19293
estrat_koppen_primavera <- estrat_koppen %>% 
  filter(Estacion == "Primavera")
t.test(estrat_koppen_primavera$Temp9am, conf.level = 0.95)$conf.int[1:2]

## Conclusión_ La variación entre los rangos de cada estación ronda 0,6º C.
## La diferencia de temperatura promedio entre otoño y primavera es de app 0.5º C
## La diferencia de temperatura promedio entre verano e invierno es de app 10º C



## 4.c ---------------------------------------------------------------------
t.test(estrat_koppen$LluviaHoy, conf.level = 0.95)$conf.int[1:2]*100
# Con un 95% de confianza, la posibilidad de días con lluvias 
# oscila entre 21.20471% y 23.51529%


## 4.d ---------------------------------------------------------------------

### a. Promedio temperatura----

#### Con dataset original----

t.test(data$Temp9am, conf.level = 0.95)$conf.int[1:2]# -> 18,02 y 18,15

#### Con dataset estratificado----

t.test(estrat_koppen$Temp9am, conf.level = 0.95)$conf.int[1:2]# -> 17,92 y 18,28

## Los resultados se encuentran dentro del intervalo de confianza propuesto
#  por el dataset estratificado

### b. Promedio temperatura distintas estaciones----

# Como ya se hizo el ejercicio arriba, solamente se pegarán los valores
# del dataset estratificado para ahorrar líneas de código

# Verano -> 22.36924 22.96673 (estratificado) -> 22.73646 22.94448 (original)
data_verano <- data %>% 
  filter(Estacion == "Verano")
t.test(data_verano$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Otoño -> 18.02107 18.64323 (estratificado) -> 18.14532 18.36334 (original)
data_otoño <- data %>% 
  filter(Estacion == "Otono")
t.test(data_otoño$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Invierno -> 12.35051 12.93993 (estratificado) -> 12.65273 12.86300 (original)
data_invierno <- data %>% 
  filter(Estacion == "Invierno")
t.test(data_invierno$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Primavera -> 18.52897 19.19293 (estratificado) -> 18.64564 18.88164 (original)
data_primavera <- data %>% 
  filter(Estacion == "Primavera")
t.test(data_primavera$Temp9am, conf.level = 0.95)$conf.int[1:2]

## Conclusión: Todos los valores originales se encuentran dentro del rango
# propuesto por el intervalo estratificado

### c. Días con lluvias ----

# Dataset estratificado -> 21.20471% y 23.51529%
t.test(estrat_koppen$LluviaHoy, conf.level = 0.95)$conf.int[1:2]*100

# Dataset original -> 21.55678% 22.36822%
t.test(data$LluviaHoy, conf.level = 0.95)$conf.int[1:2]*100

## Conclusioón: Todos los valores originales se encuentran dentro del rango
# propuesto por el intervalo estratificado


# Pregunta 5  ---------------------------------------------------------------

## a. ----
# H_0: Temp9am > 18
# H_1: Temp9am < 18
t.test(estrat_koppen$Temp9am, mu=18, alternative = "g", conf.level = 0.9)
# Hay evidencia estadística suficiente como para acoger la hipótesis 0

## b. ----
# H_0: Horas de sol en el desierto = 9
# H_1: Horas de sol en el desierto ≠ 9
koppen_desierto <- estrat_koppen %>% 
  filter(Koppen == "Desert")

t.test(koppen_desierto$Sol, mu=9, alternative = "two.sided", conf.level = 0.9)
# Hay evidencia estadística suficiente como para rechazar la hipótesis 0

## c. ----
koppen_tropical_inv <- estrat_koppen %>% 
  filter(Koppen == "Tropical" & Estacion == "Invierno")

t.test(koppen_tropical_inv$Hum3pm, mu=45, alternative = "l", conf.level = 0.9)
# Hay evidencia estadística suficiente como para acoger la hipótesis 0