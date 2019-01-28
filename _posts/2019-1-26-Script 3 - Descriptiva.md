---
layout: post
title: "Analítica con redes sociales - S3: Descriptiva"
---

**Objetivo**: En este script realizaremos la analítica descriptiva básica de las extracciones realizadas. Adicionalmente aplicaremos análisis de sentimiento a una base de tuits haciendo uso del paquete ```syuzhet```

## Descriptiva Básica de Time Lines

### Distribución temporal de los tweets

Como se hizo anteriormente, recordemos la distribución temporal de los tuits.

```{r}
library(lubridate)
#Crear histograma
ggplot(tmls, aes(x = as.Date(created_at), fill = screen_name)) +
  geom_histogram(position = "identity", bins = 40, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ screen_name, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
  
```
### Longitud media de los tweets por usuario

Podemos analizar la longitud media(en palabras, excluidas las *SW*.

```{r}
#Calculo de longitud media del tuit(palabras) y su desviación
DF_tidy %>% group_by(screen_name, status_id) %>% summarise(longitud = n()) %>% 
group_by(screen_name) %>% 
summarise(media_longitud = mean(longitud), sd_longitud = sd(longitud))

DF_tidy %>% group_by(screen_name, status_id) %>% summarise(longitud = n()) %>%
group_by(screen_name) %>% summarise(media_longitud = mean(longitud),
                          sd_longitud = sd(longitud)) %>%
                ggplot(aes(x = screen_name, y = media_longitud)) +
                geom_col() +
                geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                                  ymax = media_longitud + sd_longitud)) +
                coord_flip() + theme_bw()
```

### Comparación en el uso de palabras

Se analizan las palabras que se utilizan de forma más diferenciada por cada usuario. Se realiza mediante el *log of odds ratio* de las frecuencias.

```{r}
#################
#LOGS
#################
# Pivotaje y despivotaje
DF_spread <- DF_tidy %>% group_by(screen_name, token) %>% count(token) %>%
  spread(key = screen_name, value = n, fill = 0, drop = TRUE)

DF_unpivot <- DF_spread %>% gather(key = "screen_name", value = "n", -token)

# Se añade el total de palabras de cada autor
DF_unpivot <- DF_unpivot %>% left_join(DF_tidy %>%
                                                 group_by(screen_name) %>%
                                                 summarise(N = n()),
                                               by = "screen_name")
# Cálculo de odds y log of odds de cada palabra
logOdds <- DF_unpivot %>%  mutate(odds = (n + 1) / (N + 1))

logOdds <- logOdds %>% select(screen_name, token, odds) %>% 
  spread(key = screen_name, value = odds)

us1 = df_grouped$screen_name[1]
us2 = df_grouped$screen_name[2]
us3 = df_grouped$screen_name[3]

#Ojo, acá debemos cambiar los ratios
logOdds <- logOdds %>%  mutate(log_odds =log(IvanDuque/petrogustavo), abs_log_odds = abs(log_odds))
```
Si el logaritmo de *odds* es mayor que cero, significa que es una palabra tiene mayor probabilidad de ser de X.
Esto es así porque el ratio sea ha calculado como **X/Y**
Graficamos las 30 palabras más diferenciadas mediante log of odds ratio
```{r}
logOdds <- logOdds %>%  mutate(Categoria = if_else(log_odds > 0, us1,us2))
logOdds %>% arrange(desc(log_odds))

GG3 <- logOdds %>% group_by(Categoria) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = Categoria)) +
  geom_col() +
  labs(x = "palabra", y = paste("log odds ratio (", us1, "/", us2,")")) +
  coord_flip() + 
  theme_bw()
GG3
```

Este metodologia permite identificar palabras que se pueden asociar a la categoria y que serian útiles para clasificar las observaciones.

Ahora la repetiremos con los otros dos pares de opciones.


### METODO POR BIGRAMAS

El uso de palabras unitarias limita la capacidad del análisis, en realidad el lenguaje se crea por combinaciones no aleatorias de palabras, es decir, determinadas palabras tienden a utilizarse de forma conjunta. 
Para capturar esto en el análisis, aplicaremos el mismo enfoque pero observando por bigramas es decir pares de palabras.

```{r}
#########################################################
####### METODOS POR BIGRAMAS
#########################################################

##############################
####DEBEMOS OBTENER LOS BIGRAMAS
###############################
DF_bigram <- select(DF, c(status_id ,screen_name, text)) %>% unnest_tokens(input = text, output = "bigrama",
                token = "ngrams",n = 2, drop = FALSE)

# Contaje de ocurrencias de cada bigrama
DF_bigram  %>% count(bigrama, sort = TRUE)

# Separación de los bigramas 
DF_bigram_sep <- DF_bigram %>% separate(bigrama, c("palabra1", "palabra2"), sep = " ")

# Filtrado de los bigramas que contienen alguna stopword

DF_bigram_sep <- DF_bigram_sep  %>%
  filter(!palabra1 %in% sw) %>%
  filter(!palabra2 %in% sw)

# Unión de las palabras para formar de nuevo los bigramas
DF_bigram <- DF_bigram_sep %>%  unite(bigrama, palabra1, palabra2, sep = "_")

# Nuevo contaje para identificar los bigramas más frecuentes
DF_bigram  %>% count(bigrama, sort = TRUE) %>% print(n = 20)
remove(DF_bigram_sep)
```
Ya obtuvimos los bigramas sin stop words, a continuación realizaremos el mismo análisis gráfico.

```{r}
#Representación gráfica de las frecuencias
GG4<-DF_bigram %>% group_by(screen_name, bigrama) %>% count(bigrama) %>% group_by(screen_name) %>%
  top_n(10, n) %>% arrange(screen_name, desc(n)) %>%
  ggplot(aes(x = reorder(bigrama,n), y = n, fill = screen_name)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~screen_name,scales = "free", ncol = 1, drop = TRUE)
```
Ahora analizaremos los bigramas que se utilizan de forma más diferenciada

```{r}
# Pivotaje y despivotaje
DF_bigram_spread <- DF_bigram %>% group_by(screen_name, bigrama) %>% count(bigrama) %>%
  spread(key = screen_name, value = n, fill = 0, drop = TRUE)
DF_bigram_unpivot <- DF_bigram_spread %>% gather(key = "screen_name", value = "n", -bigrama)

# Se añade el total de palabras de cada categoria
DF_bigram_unpivot <- DF_bigram_unpivot %>% left_join(DF_bigram %>%
                                         group_by(screen_name) %>%
                                         summarise(N = n()),
                                       by = "screen_name")
                                       
# Cálculo de odds y log of odds de cada palabra
bilogOdds <- DF_bigram_unpivot %>%  mutate(odds = (n + 1) / (N + 1))
bilogOdds <- bilogOdds %>% select(screen_name, bigrama, odds) %>% 
  spread(key = screen_name, value = odds)

#Ojo de nuevo, acca se cambian los ratios
bilogOdds <- bilogOdds %>%  mutate(log_odds = log(IvanDuque/petrogustavo),
                                             abs_log_odds = abs(log_odds))
                                             
bilogOdds <- bilogOdds %>%
  mutate(Categoria = if_else(log_odds > 0,
                                   "IvanDuque",
                                   "petrogustavo"))
bilogOdds %>% arrange(desc(abs_log_odds)) 

GG5<- bilogOdds %>% group_by(Categoria) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(bigrama, log_odds), y = log_odds, fill = Categoria)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (IvanDuque/petrogustavo)") +
  coord_flip() + 
  theme_bw()

remove(DF_bigram_spread)
remove(DF_bigram_unpivot)
```

La siguiente gráfica presenta el *top 10* de bigramas más utilizados por cada categoria:

```{r}
GG4
```

se analizan los bigramas que se utilizan de forma más diferenciada por cada categoria. Se realiza mediante el *log of odds ratio* de las frecuencias.

```{r}
GG5
```

## Análisis de sentimiento

Se basa en relaciones estadísticas y de asociación, mas no en análisis lingüístico

El análisis de sentimiento está basado en lexicons, vocabulario (diccionario). 

Es un conjunto de palabras puntuadas positiva y negativamente

```{r}
#########################################################
####### IMPORTAR DATOS
#########################################################

SA<- read_twitter_csv("ELN.csv", unflatten = FALSE)

SA <- SA %>% select(status_id, created_at,
                      screen_name,
                      text,
                      source,
                      lang)

SA <- SA %>% filter(SA$lang == "es")

SA %>% group_by(SA$lang) %>% summarise(n = n())


#########################################################
####### LIMPIAR DATOS
#########################################################
# Retira caracteres extraños
SA$text <- gsub("[[:cntrl:]]", " ", SA$text)
# Retira URLs
SA$text <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", SA$text)
# Retira menciones
SA$text <- gsub("@\\w+", "", SA$text)
# Todo a minuscula
SA$text<- tolower(SA$text)
# Eliminación de signos de puntuación
SA$text <- removePunctuation(SA$text)
# Eliminación de números
SA$text <- removeNumbers(SA$text)
# Eliminación de espacios en blanco múltiples
SA$text <- stripWhitespace(SA$text)
```

Califica ocho "emociones": anger, anticipation, disgust, fear, joy, sadness, surprise, trust
También el puntaje positivo o negativo.

```{r}
#########################################################
####### ANALISIS DE SENTIMIENTO
#########################################################
library("syuzhet")

#Transformamos la base de textos importados en un vector para
#poder utilizar la función get_nrc_sentiment
palabra.df <- as.vector(SA$text)

#Aplicamos la función indicando el vector y el idioma y creamos
#un nuevo data frame llamado emocion.df
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")

#Unimos emocion.df con el vector tweets.df para ver como
#trabajó la función get_nrc_sentiment cada uno de los tweets
emocion.df2 <- cbind(SA, emocion.df)


#Creamos un data frame en el cual las filas serán las emociones
#y las columnas los puntajes totales

#Empezamos transponiendo emocion.df
emocion.df3 <- data.frame(t(emocion.df))

#Sumamos los puntajes de cada uno de los tweets para cada emocion
emocion.df3 <- data.frame(rowSums(emocion.df3))

#Nombramos la columna de puntajes como cuenta
names(emocion.df3)[1] <- "cuenta"

#Dado que las emociones son los nombres de las filas y no una variable
#transformamos el data frame para incluirlas dentro
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)

#Quitamos el nombre de las filas
rownames(emocion.df3) <- NULL

#Verificamos el data frame
print(emocion.df3)

```
Generamos algunos gráficos para observar la clasificación

```{r]
#Primer gráfico: se detallaran las 8 emociones con sus puntajes respectivos
sentimientos1 <- ggplot(emocion.df3[1:8,],
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  labs(title = "Análisis de sentimiento \n Ocho emociones",
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 5) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.position = "none")
print(sentimientos1)

#Segundo gráfico: se detallan los puntajes para las valoraciones
#positiva y negativa
sentimientos2 <- ggplot(emocion.df3[9:10,], 
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  labs(title = "Análisis de sentimiento \n Valoración positiva o negativa", 
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 5) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.position = "none")
print(sentimientos2)
```
Sentimiento  o   polaridad  o incluso el tipo de emoción no siempre resulta algo unívoco
