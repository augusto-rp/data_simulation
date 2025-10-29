library(tidyverse)
library(MASS)
library(dplyr)

library(usethis)
use_git()
use_github()


# --- Parameters ---
N_individuals <- 160
N_topics <- 8
N_fun_vars <- 16
topic_mean <- 3  # Center for the normally distributed topics (range 1-5)
topic_sd <- 1    # Standard deviation for the normally distributed topics
pol_mean <- 5.5  # Center for the 'pol' variable (range 1-10)
pol_sd <- 2.5    # Standard deviation for the 'pol' variable


set.seed(73)

#crear tabla con individuos y grupo
simulated_data <- tibble(
  id = 1:N_individuals,
  # Assign H/NH with equal numbers (160/2 = 80 per group)
  group = rep(c("H", "NH"), each = N_individuals / 2) %>% sample()
)

#agregar sexo
simulated_data <- simulated_data %>%
  mutate(
    sex = sample(1:2, N_individuals, replace = TRUE)
  )


#asignacion de ideologia poltiica siendo 1 extrema izquierda y 10 extrema derecha
simulated_data <- simulated_data %>%
  mutate(
    pol = round(rnorm(N_individuals, mean = pol_mean, sd = pol_sd)) %>%
      pmin(10) %>% # Clamp max to 10
      pmax(1)     # Clamp min to 1
  )

#Topicos distribuidos normalmente
topic_normal <- tibble(
  topic_1 = round(rnorm(N_individuals, mean = topic_mean, sd = topic_sd)) %>% pmin(5) %>% pmax(1),
  topic_2 = round(rnorm(N_individuals, mean = topic_mean, sd = topic_sd)) %>% pmin(5) %>% pmax(1),
  topic_3 = round(rnorm(N_individuals, mean = topic_mean, sd = topic_sd)) %>% pmin(5) %>% pmax(1),
  topic_4 = round(rnorm(N_individuals, mean = topic_mean, sd = topic_sd)) %>% pmin(5) %>% pmax(1)
)


#Topicos distribuidos sesgadamente

topic_random <- tibble(
  topic_5 = sample(1:5, N_individuals, replace = TRUE),
  topic_6 = sample(1:5, N_individuals, replace = TRUE),
  topic_7 = sample(1:5, N_individuals, replace = TRUE),
  topic_8 = sample(1:5, N_individuals, replace = TRUE)
)

#Unir esas distribuciones a data simulada
simulated_data <- bind_cols(simulated_data, topic_normal, topic_random)



#Para agregar valoraciones humoristicas correlacionas itrnausjeto

#Distribucion multivariada de valores que esten correlacionados entre si
fun_correlation_matrix <- matrix(0.35, nrow = N_fun_vars, ncol = N_fun_vars)
diag(fun_correlation_matrix) <- 1
fun_raw <- mvrnorm(n = N_individuals,
                   mu = rep(3, N_fun_vars), # Mean of 3
                   Sigma = fun_correlation_matrix)


#Transformar valores a discretos entre 1 y 5. Lo que ahce es que pasa valor a cuantiles y ahi les asigna valor
fun_simulated <- apply(fun_raw, 2, function(x) {
  cut(x,
      breaks = quantile(x, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),
      labels = 1:5,
      include.lowest = TRUE) %>% as.numeric()
}) %>% as_tibble()
names(fun_simulated) <- paste0("fun_", 1:N_fun_vars)
simulated_data <- bind_cols(simulated_data, fun_simulated)


#Creacion de vd
# 1. Define the 19 possible discrete values
ao_values <- seq(from = -2, to = 7, by = 0.5)
N_ao_values <- length(ao_values) # Should be 19

# 2. Matriz altamente correlacionada para las dos variables
ao_correlation_matrix <- matrix(c(1, 0.65, 0.65, 1), nrow = 2, ncol = 2)

#Distribucion normal de valores correlaacionados
ao_raw <- MASS::mvrnorm(n = N_individuals,
                        mu = rep(2.5, 2), # Center mean around the middle of the range
                        Sigma = ao_correlation_matrix)


ao_simulated <- apply(ao_raw, 2, function(x) {
  # Define probability breaks to create 19 bins (19 values + 1 break point)
  probs_breaks <- seq(0, 1, length.out = N_ao_values + 1)
  breaks <- quantile(x, probs = probs_breaks, names = FALSE)
  
  # Adjust outer breaks slightly to ensure min/max values are included
  breaks[1] <- min(x) - 1e-10
  breaks[length(breaks)] <- max(x) + 1e-10
  
  # Cut the continuous data into bins, labeling them with the discrete ao_values.
  # The output is a FACTOR.
  ao_factor <- cut(x,
                   breaks = breaks,
                   labels = ao_values,
                   include.lowest = TRUE,
                   right = TRUE)
  
  # CRITICAL FIX: Convert Factor -> Character -> Numeric to get the actual label value
  return(as.numeric(as.character(ao_factor)))
}) %>% as_tibble()

names(ao_simulated) <- c("ao_1", "ao_2")
simulated_data <- bind_cols(simulated_data, ao_simulated)

#cambiar nombre de objeto simulated_data a df
df <- simulated_data


#borrar todos los objetos excepto df
rm(list = setdiff(ls(), "df"))

###########HASTA ACA SERIA UNA TABLA DE VALORES TAL Y COMO LOS ENTREGA PARTICIPANTE
##########PERO TODAVIA HABRIA QUE INTRODUCIR EL FACTOR INTRA


#Agregar una columna que sea promedio columnas 13 a 20 y otra columan que sea promedio de 21 a 28

#seleccionar columnas de 13 a 20 y promediarlas
df$fun_p <- rowMeans(df[, 13:20], na.rm = TRUE)  
df$fun_np <- rowMeans(df[, 21:28], na.rm = TRUE)  


df$sex<-as.factor(df$sex) #H quedo como 1
df$group<-as.factor(df$group) #Hombre es 1
str(df)




#Topicos de interes son 1 (el politico) y 2 (el no politico)
#Eliminar columnas 7 a 12
df <- df|>
  select(-c(topic_5, topic_6, topic_7, topic_8, topic_3, topic_4))

data_long_revised <- df |>
  # 2a. Pivot the 'topic_1' and 'topic_2' columns.
  # names_to will capture the original column name ("topic_1" or "topic_2").
  # values_to will capture the numerical score (your new 'topic_value').
  pivot_longer(
    cols = c(topic_1, topic_2),
    names_to = "topic_type_temp",  # This temporary column distinguishes the two rows
    values_to = "topic_value"
  ) |>
  # 2b. Use the temporary column to create the two new columns: 'topic' (label) and 'ao_value'.
  mutate(
    # Create the 'topic' column (the label: "political" or "non")
    topic = case_when(
      topic_type_temp == "topic_1" ~ "political",
      topic_type_temp == "topic_2" ~ "non"
    ),
    # Create the 'ao_value' column (based on the original ao_1/ao_2)
    ao_value = case_when(
      topic_type_temp == "topic_1" ~ ao_1, # Topic 1 (political) gets ao_1
      topic_type_temp == "topic_2" ~ ao_2, # Topic 2 (non) gets ao_2
    )
  ) |>
  # 2c. Final cleanup: Select and reorder the desired columns, dropping temporary/old columns.
  select(
    id, group, pol,sex,
    topic, topic_value, # The two new, separate topic columns
    starts_with("fun_"),
    fun_p, fun_np,
    ao_value
  )

df_larga<-data_long_revised
rm(data_long_revised)


#Finalmente agregar una columna que sea una mezcla de codigo de group+topic
df_larga <- df_larga |>
  mutate(
    group_topic = case_when(
      group == "H" & topic == "political" ~ "H_political",
      group == "H" & topic == "non" ~ "H_non",
      group == "NH" & topic == "political" ~ "NH_political",
      group == "NH" & topic == "non" ~ "NH_non"
    )
  )

str(df_larga)
df_larga$topic<-as.factor(df_larga$topic)
df_larga$group_topic<-as.factor(df_larga$group_topic)

# ESTIMACION DE MODELO ----------------------------------------------------

#USANDO ezANOVA
library(ez)
library (psych)

#Exploracion de descriptivos
describe(df_larga)

#describir promedios de ao_value de acuerdo a niveles de group_topic

dif_promedios <- df_larga |>
  group_by(group_topic) |>
  summarise(
    mean_ao_value = mean(ao_value, na.rm = TRUE)
  )
print(dif_promedios )


#########Examinacion de criterios ANOVA
#Exploracion de mauchly sphericity test



###############ESTIMACION DE MODELO USANDO ezANOVA
#definir contrastes
c_humor<-c(1, -1) #contraste de humor vrs no humor
c_topico<-c(1, -1) #contraste de topico politico vrs no politico

contrasts(df_larga$group)<-cbind(c_humor)
contrasts(df_larga$topic)<-cbind(c_topico)

#hacer anova
modelo_anova <- ezANOVA(
  data = df_larga,
  dv = ao_value,
  wid = id,
  within = .(topic),
  between = .(group),
  type = 3,
  detailed = TRUE
)

modelo_anova
#como es de esperar nada es significativa because why would it be



##########  USANDO GLM
#recordar que parte repetida topic se especifica en parte aleatoria
library(lme4)

#creamos un modelo base usando solo efecto aleatorio

base <- lmer(ao_value ~ 1 + (1 |id),
                       data = df_larga,
                       REML = TRUE)

base
summary(base)

#ICC = var(intercepto sujeto)/(var(intercepto sujeto)+var(residual))
icc <- as.numeric(VarCorr(base)$id[1]) / (as.numeric(VarCorr(base)$id[1]) + attr(VarCorr(base), "sc")^2)
icc #0.649 es decir 64% varianza es explica por diferencias entre individuos, baselines muy disstintos individuales



#ahora agregar al modelo predictor fijo group
modelo1<-update(base, . ~ . + group)

#y ahora agrega a modelo1 predictor pol
modelo2<-update(modelo1, . ~ . + pol)

#y ahora comparar los modelos}

anova(base, modelo1, modelo2)
