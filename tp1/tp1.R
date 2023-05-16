library(tidyverse)

set.seed(632)


# 1 -----------------------------------------------------------------------


#' Para la base de datos seleccionada genere una muestra aleatoria estratificada y
#' balanceada por “depósito” de tamaño n=2000 utilizando como semilla los últimos tres
#' dígitos del DNI/PASAPORTE. Guarde los datos en un archivo y realice todo el trabajo
#' práctico con la muestra generada.

# data <- read_csv("tp1/data.csv")
# 
# data_sample <- data %>% 
#   group_by(deposit) %>% 
#   slice_sample(n = 1000)
# 
# data_sample %>% 
#   write_csv('tp1/data_sample.csv')
# 
# rm(data)

data_sample <- read_csv('tp1/data_sample.csv')

# 2 -----------------------------------------------------------------------

#' Realice un análisis estadístico de cada una de las variables numéricas para cada valor de
#' depósito. Presente la información en forma tabular y conteniendo las siguientes
#' medidas descriptivas: +Cantidad de datos, +mínimo, +máximo, +media, +mediana, +moda,
#' varianza, desviación estándar, coeficiente de variación, cuartil 1, cuartil 3,
#'  rango intercuartílico, MAD, asimetría, curtosis.

data_sample <- data_sample %>% 
  mutate(pdays = na_if(pdays, 999))

resumen_a <- data_sample %>% 
  group_by(deposit) %>% 
  summarise(
    across(everything(),list(
    tipo = ~ class(.x), 
    N = ~ sum(!is.na(.x)),
    datos_faltantes = ~ sum(is.na(.x)),
    moda = ~ DescTools::Mode(.x, na.rm = T)
    ), .names = "{.col}-{.fn}")) %>%
  ungroup() %>%
      pivot_longer(cols = -c(deposit),
                   names_to = c("var", ".value"), names_sep = "-",
                   values_transform = as.character) 

resumen_b <- data_sample %>% 
  group_by(deposit) %>% 
  summarise(
    across(where(is.numeric),
           list(
      minimo =  ~ min(.x, na.rm = T),
      maximo = ~ max(.x, na.rm = T), 
      media = ~ mean(.x, na.rm = T),
      mediana = ~ median(.x, na.rm = T),
      varianza  = ~ var(.x, na.rm = T),
      desviacion_estandar = ~ sd(.x, na.rm = T),
      cv =  ~ sd(.x, na.rm = T)/mean(.x, na.rm = T),
      q1 = ~ quantile(.x, probs = .25, na.rm = T),
      q3 = ~ quantile(.x, probs = .75, na.rm = T),
      rango_iq = ~ IQR(.x, na.rm = T), 
      asimetria = ~ DescTools::Skew(.x, na.rm = T),
      curtosis = ~ DescTools::Kurt(.x, na.rm = T)
    ),
    .names = "{.col}-{.fn}"
    )
    ) %>% ungroup() %>%
  pivot_longer(cols = -c(deposit),
                       names_to = c("var", ".value"), names_sep = "-",
  values_transform = as.character) 

resumen <- left_join(resumen_a, resumen_b)

resumen 


# 3 -----------------------------------------------------------------------

#' Represente gráficamente cada variable numérica eligiendo el gráfico que
#' considere apropiado. Considere la posibilidad de generar rangos de datos
#' para su análisis y representación gráfica de las variables.
#' 

vars_numeric <- data_sample %>% select(where(is.numeric)) %>% colnames()

vars_numeric



data_sample %>% 
  ggplot() +
    geom_histogram(aes(age),
                   color = "black",
                   alpha = .6, position = "identity")

data_sample %>% 
  ggplot() +
  geom_density(aes(x = balance))

#' pdays: number of days that passed by after the client was
#' last contacted from a previous campaign (numeric; 999 means
#' client was not previously contacted)

data_sample %>% 
  ggplot() +
  geom_boxplot(aes(pdays))

#' previous: number of contacts performed before
#' this campaign and for this client (numeric)

data_sample %>% 
  ggplot() +
  geom_histogram(aes(previous),
                 color = "white")

data_sample %>% 
  ggplot() +
  geom_boxplot(aes(previous))

#' campaign: number of contacts performed during this campaign
#' and for this client (numeric, includes last contact)

data_sample %>% 
  ggplot() +
  geom_boxplot(aes(campaign))

data_sample %>% 
  ggplot() +
  geom_histogram(aes(day))

#'duration: last contact duration, in seconds (numeric). 

data_sample %>% 
  ggplot() +
  geom_boxplot(aes(duration))


# 4 -----------------------------------------------------------------------

#' Presente una tabla de frecuencias y porcentaje para la variable “marital”
#' (estado civil) según el nivel de la variable “deposit”.
#' 

data_sample %>% 
  count(deposit, marital) %>% class()

DescTools::PercTable(tab = table(data_sample$marital, 
                                 data_sample$deposit),
                     margins = c(1,2))


# 5 -----------------------------------------------------------------------


#' Realice un gráfico para representar la tabla construida en el punto 4.
#' 


data_sample %>% 
  ggplot() +
  geom_bar(aes(x = marital, fill = deposit), position = "fill", color = "white") +
  geom_label(aes(x = marital, y = after_stat(count),
                 label = after_stat(count), group = deposit),
            stat = "count", position = position_fill(vjust = .5)) +
  scale_y_continuous(labels = ~ scales::label_percent()(.x))
  

# 6 -----------------------------------------------------------------------

#' Elija dos variables continuas, establezca rangos que representen
#' distintos niveles de cada una y defina nuevas variables categóricas.
#' Aplique un test adecuado para entender si existe asociación entre ambas.
#'  Utilice un nivel de significación del 5%.

quantile(data_sample$age, probs = c(.25, .5,.75))
quantile(data_sample$balance, probs = c(.25, .5,.75))

test_data_balance_age <- data_sample %>% 
  mutate(age_bin = cut(age, breaks = c(19, 30, 60, max(age)), include.lowest = T),
         balance_bin = cut(balance, breaks = c(min(balance), quantile(balance,
                                                                     probs = c(.25, .75)),
                                                   max(balance)),
                           include.lowest = T)) %>% 
  select(age_bin, balance_bin)

table_balance_age <- table(test_data_balance_age$balance_bin, test_data_balance_age$age_bin)
table_balance_age
prop.table(table_balance_age, margin = c(2))

chi_balance_age <- chisq.test(table_balance_age)

any(chi_balance_age$expected == 0)
sum(chi_balance_age$expected < 5)/length(chi_balance_age$expected)

chi_balance_age$p.value

chi_balance_age$p.value < 0.05

#' Se rechaza la hipótesis nula:
#'  no hay evidencia estadística de que las variables sea independientes.



# 7 -----------------------------------------------------------------------


#' Seleccione la variable “education” y elija otra variable categórica.
#' Aplique un test adecuado para entender si existe asociación entre ambas.
#' Utilice un nivel de significación del 5%.


# tabla de contingencia
table_education <- table(data_sample$education,
      data_sample$default)

# Chi cuadrado
# Ho: las variables son independientes Ha: las variables no son independientes
chi_education_default <- chisq.test(table_education)
# warning de posible aproximacion incorrecta

chi_education_default 

chi_education_default$expected
# hay valores esperados menores a 5
# posible origen del warning

# Se decide evaluar con test exacto de Fisher con alfa = 0.05
# Ho: las variables son independientes Ha: las variables no son independientes
fisher.test(table_education)
# p valor no permite rechazar la hipostesis nula, se asume la independencia de las variables.


# 8 -----------------------------------------------------------------------

#' Seleccione otra variable continua y estime la diferencia de medias según
#'  el valor de la variable “deposit” con un nivel de confianza del 95%. 
#'  Interprete el resultado obtenido.
#'  


# test de normalidad con alfa = 0.05
# Ho = la variable sigue una distribución normal Ha = la variable no sigue una distribución normal
shapiro.test(data_sample$duration[data_sample$deposit == "no"])
shapiro.test(data_sample$duration[data_sample$deposit == "yes"])

# p-valor permite rechazar la hipotesis nula en ambos casos, las muestras no siguen una distribucion normal

# se testea si hay homocedasticidad 
# test de varianzas de Levene para muestras no normales
#  se prueba si las varianzas desconocidas son diferentes bajo un alfa de 0.05.
# H0: las varianzas son iguales Ha: las varianzas son diferentes
var_test <- leveneTest(duration ~ as_factor(deposit),
                       data=data_sample, alternative = "two.sided")    

var_test
# p valor permite rechazar la hipotesis nula. se asume varianzas diferentes



# se decide utilizar el estadístico Z basado en la varianza muestral 
# para muestras grandes con varianza poblacional desconocida.
# para la diferencia de medias para calcular el IC a 95%

BSDA::z.test(data_sample$duration[data_sample$deposit == "no"],
             data_sample$duration[data_sample$deposit == "yes"],
             alternative = "two.sided",
             conf.level = 0.05,
             sigma.x = sd(data_sample$duration[data_sample$deposit == "no"]),
             sigma.y = sd(data_sample$duration[data_sample$deposit == "yes"]))

# el intervalo de confianza es [ -339.1051; -337.2989]
# dado que que el intervalo de confianza obtenido no incluye el 0,
# se interpreta que las medias de ambas poblaciones (deposites yes y deposit no) son diferentes


# 9 -----------------------------------------------------------------------

#' Según el resultado obtenido en el punto 8.- realice un test de hipótesis
#' apropiado para determinar la diferencia de medias de la variable en estudio. 
#' Trabaje con una significación del 5%. Presente el planteo de hipótesis
#' adecuado, la resolución y la decisión a tomar.
#' 

#' Considerando los resultados del punto 8 para la variable 'balance':
#' 1. se asume que las muestras provienen de poblaciones con varianzas desconocidas diferentes
#' 2. se asume un distribución muestral no normal
#' 3. se trata una muestra grande

# Se utiliza un test de hipótesis basado en el estadístico Z
# para muestras grandes de poblaciones con varianza desconocida

# H0: la diferencia de medias es igual a cero, Ha: la diferencia de medias es distinta de cero
BSDA::z.test(data_sample$duration[data_sample$deposit == "no"],
             data_sample$duration[data_sample$deposit == "yes"],
             alternative = "two.sided",
             conf.level = 0.05,
             sigma.x = sd(data_sample$duration[data_sample$deposit == "no"]),
             sigma.y = sd(data_sample$duration[data_sample$deposit == "yes"]))

# el p-valor obtenido permite rechazar H0, se asume que la diferencia de medias es distinta de cero


# 10 ----------------------------------------------------------------------


#' 10. Seleccione una muestra de 30 elementos estratificada según la variable “deposit”.
#' ¿Se puede afirmar que hay diferencias significativas en el balance de los que realizaron
#' el depósito respecto a aquellos que no lo hicieron? Elija un test de hipótesis adecuado.
#' Trabaje con una significación del 5%. 

submuestra <- data_sample %>% 
  slice_sample(n = 15, by = deposit)

# testeo normalidad

# h0: la variable tiene distribucion normal ha: la variable no tiene distribucion normal
shapiro.test(submuestra$balance)

# p valor permite rechazar la hipotesis nula, se asume que la variable no tiene una distribucion normal
# dado: muestra pequeña, distribucion no normal y variable numerica
# se opta por un test no parametrico

#' H0: no hay diferencias de poblacion para balance entre deposit yes y deposit no
#' Ha: hay diferencias de poblacion para balance entre deposit yes y deposit no


wilcox.test(submuestra$balance[submuestra$deposit == 'no'],
            y = submuestra$balance[submuestra$deposit == 'yes'],
            exact = F, # aproximacion normal al p valor para muestras con empates
            digits.rank = 5,
            alternative = "two.sided")

# el p valor no permite rechazar la hipotesis nula: se asume que no hay diferencias significativas
# en el balance de los que realizaron el depósito respecto a aquellos que no lo hicieron
# se asume que ambas muestras de balance provienen de poblaciones identicas


# 11 ----------------------------------------------------------------------


#' 11. Decida si existen diferencias significativas en la duración respecto los niveles de
#' educación (“secondary”, “tertiary”, “primary”, “unknown”). Justifique. Utilice un test
#' adecuado. Realice las pruebas necesarias para comprobar los supuestos. Trabaje con
#' una significación del 5%. 

# Anova
#' - Las muestras deben ser aleatorias y las observaciones independientes +
#' - Las varianzas de las subpoblaciones deben ser iguales (homocedasticidad) 
#' - La distribución de cada subpoblación es normal

modelo <- aov(duration ~ factor(education),
            data=data_sample)

# chequeo homocedasticidad a nivel de sig 0.05

leveneTest(duration ~ as_factor(education), data = data_sample)

# el p valor indica que no hay evidencia para rechazar la hipotesis nula
# se asume que las varianzas son iguales

# chequeo normalidad por test de normalidad sobre los residuos a nivel de significacion 0.05
# h0: los residuos tienen distribucion normal ha: los residuos no tienen distribucion normal
shapiro.test(modelo$residuals)

# p valor indica que hay evidencia para rechazar la hipotesis nula de que los residuos se distribuyen normalmente

# se descarta el modelo de anova y pasamos a una prueba no parametrica  

# test de Kruskal-Wallis para k grupos con nivel de significancia 0.05
# Ho: todos los niveles educativos tienen una posicion central similar para la variable duration
# Ha al menos un nivel educativo tiene una posicion central diferente para  la variable duration

kruskal.test(duration ~ as_factor(education), data = data_sample)# Realiza el test de Kruskal Wallis
# p valor indica que no hay evidencia para rechazar hipotesis nula
# se asume una posicion central para la variable duracion igual entre los diferentes niveles educativos



# 12 ----------------------------------------------------------------------


#' 12. Elija dos variables cuantitativas, determine la variable explicativa y la variable explicada.
#' Encuentre la ecuación de la recta de regresión lineal que explique la relación entre las
#' variables elegidas. Escriba conclusiones acerca de la significatividad del modelo aplicado.
#' Puede acompañar el modelo de un gráfico adecuado.
#' 


lm_modelo <- lm(balance ~ age, data = data_sample, model = T)

summary(lm_modelo)

plot(lm_modelo)

