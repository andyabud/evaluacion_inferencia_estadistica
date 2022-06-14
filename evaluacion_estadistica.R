
# Pregunta 1 - Cargar y limpiar Dataset  ---------------------------------------

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

### a - Los resultados varían casi en un 1%-------------------------------------

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

### c - Los resultados varían más de 1%-----------------------------------------

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

### d - Los resultados varían aproximadamente 0,2%------------------------------

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


## Conclusión --------------------------------------------------------------

# La muestra obtenida sí es representativa ya que el % de variación es de máximo
# 1.4%, sin embargo podría ser más representativa aún, y para ello considero que
# debería hacer una muestra más grande para tener menor variación entre
# el dataset completo y las muestras obtenidas


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
# Los valores tienen una leve variación a partir de las milésimas


## 3.a ---------------------------------------------------------------------

### Con el dataset original

table(data_1$lluvia_hoy) %>% 
  prop.table*100

# La probabilidad que llueva hoy es de 22,88%

### Con el dataset nuevo

table(estrat_koppen$LluviaHoy) %>% 
  prop.table*100

# La probabilidad que llueva hoy es de 22,36%

## 3.b ---------------------------------------------------------------------

### Con el dataset original

data_1_b <- data_1 %>%
  count(sol <8)

prop.table(data_1_b)*100
# Hay un 43,81% de probabilidad que un día tenga menos de 8 horas de sol

### Con el dataset nuevo

estrat_sol <- estrat_koppen %>%
  count(Sol <8)

prop.table(estrat_sol)*100
# Hay un 44.09% de probabilidad que un día tenga menos de 8 horas de sol

## 3.c ---------------------------------------------------------------------

### Con el dataset original

data_1_c <- data_1 %>% 
  filter(estacion == "Invierno")%>% 
  count(temp3pm >20)

prop.table(data_1_c)*100
# La probabilidad que la temperatura sea <20º en invierno es de 24,78%

### Con el dataset nuevo

estrat_temp_invierno <- estrat_koppen %>% 
  filter(Estacion == "Invierno")%>% 
  count(Temp3pm >20)

prop.table(estrat_temp_invierno)*100
# La probabilidad que la temperatura sea <20º en invierno es de 26,48%

## 3.d ---------------------------------------------------------------------

### Con el dataset original

data_1_d <- data_1 %>% 
  filter(koppen == "Subtropical")

data_1_d %>% 
  count(lluvia >= 5 & lluvia <= 10) %>%
  prop.table()*100

# La posibilidad que llueva entre 5 a 10mm en un clima Koppen Subtropical es de
# 5,37%

### Con el dataset nuevo

estrat_subtropical <- estrat_koppen %>% 
  filter(Koppen == "Subtropical")

estrat_subtropical %>% 
  count(Lluvia >= 5 & Lluvia <= 10) %>%
  prop.table()*100

# La posibilidad que llueva entre 5 a 10mm en un clima Koppen Subtropical es de
# 5,10%

## Conclusión ----

# Los datos presentan una leve variación, es difícil decir si los datos han
# mejorado o no ya que depende cuál sea el parámetro para definir si son
# mejores o peores


# Pregunta 4 --------------------------------------------------------------

## 4.a----

t.test(estrat_koppen$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Con un 95% de confianza, la temperatura promedio de Australia a las 9:00 AM
# se encuentra en un rango entre 17,92 y 18,28

## 4.b----

estrat_koppen_verano <- estrat_koppen %>% 
  filter(Estacion == "Verano")
t.test(estrat_koppen_verano$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Con un 95% de confianza, la temperatura promedio en 
# Verano -> 22.36924 22.96673


estrat_koppen_otono <- estrat_koppen %>% 
  filter(Estacion == "Otono")
t.test(estrat_koppen_otono$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Con un 95% de confianza, la temperatura promedio en 
# Otoño -> 18.02107 18.64323


estrat_koppen_invierno <- estrat_koppen %>% 
  filter(Estacion == "Invierno")
t.test(estrat_koppen_invierno$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Con un 95% de confianza, la temperatura promedio en 
# Invierno -> 12.35051 12.93993


estrat_koppen_primavera <- estrat_koppen %>% 
  filter(Estacion == "Primavera")
t.test(estrat_koppen_primavera$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Con un 95% de confianza, la temperatura promedio en 
# Primavera -> 18.52897 19.19293


### Conclusión----
# La variación entre los rangos de cada estación ronda 0,6º C
# La diferencia de temperatura promedio entre otoño y primavera es de app 0.5º C
# La diferencia de temperatura promedio entre verano e invierno es de app 10º C


## 4.c ---------------------------------------------------------------------

t.test(estrat_koppen$LluviaHoy, conf.level = 0.95)$conf.int[1:2]*100 
  
# Con un 95% de confianza, la posibilidad de días con lluvias 
# oscila entre 21.20471% y 23.51529%


## 4.d ---------------------------------------------------------------------

### a. Promedio temperatura----

#### Con dataset original

t.test(data$Temp9am, conf.level = 0.95)$conf.int[1:2]# -> 18,02 y 18,15

#### Con dataset estratificado

t.test(estrat_koppen$Temp9am, conf.level = 0.95)$conf.int[1:2]# -> 17,92 y 18,28

## Los resultados se encuentran dentro del intervalo de confianza propuesto
#  por el dataset estratificado

### b. Promedio temperatura distintas estaciones----

# Como ya se hizo el ejercicio arriba, solamente se pegarán los valores
# del dataset estratificado para ahorrar líneas de código

data_verano <- data %>% 
  filter(Estacion == "Verano")
t.test(data_verano$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Verano -> 22.36924 22.96673 (estratificado) -> 22.73646 22.94448 (original)

data_otoño <- data %>% 
  filter(Estacion == "Otono")
t.test(data_otoño$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Otoño -> 18.02107 18.64323 (estratificado) -> 18.14532 18.36334 (original)


data_invierno <- data %>% 
  filter(Estacion == "Invierno")
t.test(data_invierno$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Invierno -> 12.35051 12.93993 (estratificado) -> 12.65273 12.86300 (original)

data_primavera <- data %>% 
  filter(Estacion == "Primavera")
t.test(data_primavera$Temp9am, conf.level = 0.95)$conf.int[1:2]
# Primavera -> 18.52897 19.19293 (estratificado) -> 18.64564 18.88164 (original)

#### Conclusión----
# Todos los valores originales se encuentran dentro del rango
# propuesto por el intervalo estratificado


### c. Días con lluvias ----

# Dataset estratificado -> 21.20471% y 23.51529%
t.test(estrat_koppen$LluviaHoy, conf.level = 0.95)$conf.int[1:2]*100

# Dataset original -> 21.55678% 22.36822%
t.test(data$LluviaHoy, conf.level = 0.95)$conf.int[1:2]*100

#### Conclusión----
# Todos los valores originales se encuentran dentro del rango
# propuesto por el intervalo estratificado


# Pregunta 5  ---------------------------------------------------------------


## a. ----

# H_0: Temp9am > 18
# H_1: Temp9am < 18

t.test(estrat_koppen$Temp9am, mu=18, alternative = "g", conf.level = 0.9)

# Hay evidencia estadística suficiente como para acoger la hipótesis nula
# dado que el p-value es 0.12 con una significancia del 10%


## b. ----

# H_0: Horas de sol en el desierto = 9
# H_1: Horas de sol en el desierto ≠ 9

koppen_desierto <- estrat_koppen %>% 
  filter(Koppen == "Desert")

t.test(koppen_desierto$Sol, mu=9, alternative = "two.sided", conf.level = 0.9)

# No hay evidencia estadística suficiente como para rechazar la hipótesis 0
# dado que el p-value es 0.15 con una significancia del 10%


## c. ----

# H_0: % humedad en una zona tropical < al 45%
# H_1: % humedad en una zona tropical > al 45%

koppen_tropical_inv <- estrat_koppen %>% 
  filter(Koppen == "Tropical" & Estacion == "Invierno")

t.test(koppen_tropical_inv$Hum3pm, mu=45, alternative = "l", conf.level = 0.9)

# Hay evidencia estadística suficiente como para acoger la hipótesis 0
# dado que el p-value es 0.557 con una significancia del 10%


## d. ----

# Generar base estratificada subtropical

estrat_tropical <- estrat_koppen %>% 
  filter(Koppen == "Tropical")

# Test
t.test(x = estrat_subtropical$Hum3pm, y = estrat_tropical$Hum3pm, 
       mu=0, alternative = "t", conf.level = 0.90)

# No hay evidencia estadística suficiente como para rechazar la hipótesis 0
# dado que el p-value es 0.00000000001873 con una significancia del 10%


## e. ----

### a. ----

## Dataset estratificado

t.test(estrat_koppen$Temp9am, mu=18, alternative = "g", conf.level = 0.9)
# Hay evidencia estadística suficiente como para acoger la hipótesis nula
# dado que el p-value es 0.12 con una significancia del 10%

## Dataset original

t.test(data$Temp9am, mu=18, alternative = "g", conf.level = 0.9)
# No hay evidencia estadística suficiente como para rechazar la hipótesis nula
# dado que el p-value es 0.004972 con una significancia del 10%

## Con el dataset estratificado sí existe evidencia para acoger la hipótesis nula
# Con el original, no hay evidencia para rechazarla


### b. ----

## Dataset estratificado

t.test(koppen_desierto$Sol, mu=9, alternative = "two.sided", conf.level = 0.9)
# No hay evidencia estadística suficiente como para rechazar la hipótesis 0
# dado que el p-value es 0.15 con una significancia del 10%

## Dataset original

# Crear sub-base desierto a partir de la original
data_desierto <- data %>% 
  filter(Koppen == "Desert")

t.test(data_desierto$Sol, mu=9, alternative = "two.sided", conf.level = 0.9)

# No hay evidencia estadística suficiente como para rechazar la hipótesis 0
# dado que el p-value es 0.55 con una significancia del 10%

## Ambos resultados indican que no hay evidencia suficiente para rechazar la
#  hipótesis 0
# Con los datos completos la media de x baja bastante

### c. ----

## Dataset estratificado

koppen_tropical_inv <- estrat_koppen %>% 
  filter(Koppen == "Tropical" & Estacion == "Invierno")

t.test(koppen_tropical_inv$Hum3pm, mu=45, alternative = "l", conf.level = 0.9)
# Hay evidencia estadística suficiente como para acoger la hipótesis 0
# dado que el p-value es 0.557 con una significancia del 10%

## Dataset original

# Crear sub base con filtros Tropical e Invierno
data_tropical_inv <- data %>% 
  filter(Koppen == "Tropical" & Estacion == "Invierno")

t.test(data_tropical_inv$Hum3pm, mu=45, alternative = "l", conf.level = 0.9)
# Hay evidencia estadística suficiente como para acoger la hipótesis 0
# dado que el p-value es 0.9936 con una significancia del 10%

# En ambos casos hay evidencia suficiente como para acoger la hipótesis 0
# Con los datos completos el p-value sube bastante y la media aumenta por casi
# 1º Celsius

### d. ----

## Dataset Original

# Crear sub base con el filtro subtropical
data_subtropical <- data %>% 
  filter(Koppen == "Subtropical")

t.test(x = data_subtropical$Hum3pm, y = estrat_tropical$Hum3pm, 
       mu=0, alternative = "t", conf.level = 0.90)

# No hay evidencia estadística suficiente como para rechazar la hipótesis 0
# dado que el p-value es 0.000000000009093 con una significancia del 10%

## Dataset estratificado

t.test(x = estrat_subtropical$Hum3pm, y = estrat_tropical$Hum3pm, 
       mu=0, alternative = "t", conf.level = 0.90)

# No hay evidencia estadística suficiente como para rechazar la hipótesis 0
# dado que el p-value es 0.00000000001873 con una significancia del 10%

## En ambos casos no hay suficiente evidencia estadística como para rechazar
#  la hipótesis 0


# Pregunta 6 --------------------------------------------------------------

## 6.a----

# Un primer approach que no se terminó ocupando
# ggplot(data.frame(estrat_koppen), aes(x = Temp9am))+
#  geom_histogram(aes(y=..density..), color = "gray", fill = "white")+
#  geom_density(fill = "black", alpha = 0.2)

table(estrat_koppen$Temp9am) %>%
  boxplot(notch = TRUE,
          xlab = "Estación: Verano",
          ylab = "Temperatura",
          col = "darkgreen")


## 6.b ---------------------------------------------------------------------

boxplot(estrat_koppen_verano$Temp9am, estrat_koppen_otono$Temp9am, 
        estrat_koppen_primavera$Temp9am, estrat_koppen_invierno$Temp9am, 
        notch = TRUE,
        xlab = "Estaciones",
        ylab = "Temperatura",
        names = c("Verano", "Otono", "Primavera", "Invierno"),
        col = "lightgreen")

## 6.c ---------------------------------------------------------------------

barplot(prop.table(table(estrat_koppen$LluviaHoy)) *100,
        main = "6.C: Posibilidad que llueva" ,
        xlab = "Ocurrencia", 
        ylab = "%", 
        names= c("No llueve: 22,36%", "Sí llueve, 77,64%"),
        col = "darkblue")


## 6.d ---------------------------------------------------------------------

### a----

table(data$Temp9am) %>%
  boxplot(notch = TRUE,
          main = "Temperatura Australia 9:00 - Data Original",
          xlab = "Todas las estaciones",
          ylab = "Temperatura",
          col = "lightblue")
# No pude detectar por qué el rango de temperatura está multiplicado por 10

### b----

boxplot(data_verano$Temp9am, data_otoño$Temp9am, 
        data_primavera$Temp9am, data_invierno$Temp9am, 
        notch = TRUE,
        main = "Promedio Temperatura 9AM en todas las estaciones - Data Original",
        xlab = "Estaciones",
        ylab = "Temperatura",
        names = c("Verano", "Otono", "Primavera", "Invierno"),
        col = "lightgreen")

### c----

barplot(prop.table(table(data$LluviaHoy)) *100,
        main = "Posibilidad que llueva - Data Original" ,
        xlab = "Ocurrencia", 
        ylab = "%", 
        names= c("No llueve: 78,0375%", "Sí llueve,21,9625%"),
        col = "darkblue")
# El eje y (rango porcentajes) debería llegar hasta 78% sin embargo 
# llega hasta 60, no pude detectar la razón de esto
