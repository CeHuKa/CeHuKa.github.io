---
layout: post
title: "Analítica con redes sociales - S1: Collecting Twitter Data"
---

**Objetivo**: En este script realizaremos la conexión a la API de *Twitter* con la libreria [RTweet](https://rtweet.info/reference/index.html) , a traves de nuestra [Twitter App](https://developer.twitter.com).

Probaremos las diferentes opciones de extraccion que tenemos con esta API:

* Extracción por palabra clave
* Extracción de TL por usuario

# Paso 0: Librerias y directorio de trabajo


Si te hace falta alguna librería puedes comprobar e instalarla asi:

```{r}

## install packages if not already
if (!requireNamespace("rtweet", quietly = TRUE)) 
  {
  install.packages("rtweet")
  }
  
```

Llamaremos las librerias que requerimos para esta actividad.

```{r}
##########################################################################################
####### LIBRERIAS
##########################################################################################
library(tm)                 ## Mineria de texto
library(ggplot2)            ## Gráficas
library(tidyverse)          ## Limpieza de datos
library(tidytext)           ## Limpieza de datos
library(dplyr)
library(RColorBrewer)       ## Paletas de colores
library(wordcloud)          ## WordCloud
library(gridExtra)
library(scales)
library(quanteda)
library(knitr)              ## Tablas bonitas

# load twitter library
library(rtweet)             ## R client Twitter’s REST and stream APIs
library(httpuv)             ## Conexión http

#Modelado
library(pROC)
library(randomForest)
library(mlbench)
library(caret)

```

Debes configurar tu directorio de trabajo, donde tienes los archivos descargados.
```{r}
#########################################################
####### DIRECTORIO DE TRABAJO
#########################################################

setwd("C:/MyRScripts/2019/WANRS_COL_AI/Data/")

```

# Paso 1: AUTENTICACION

Realizaremos la autenticación con las claves de  nuestra [Twitter App](https://developer.twitter.com)

```{r}
##########################################################################################
####### Collecting Twitter Data
##########################################################################################

### App

appname <- "InceptionCol"
##Consumer API keys

### API key
key <- "efw1ZYnxZRZ3Qt0kKMJyFvPEB"
### API secret key
secret <- "rhuOx6y9s8nN58wbKXFac685TDOTNIXmn32PKTIuEnykm5s0bd"

```

Existen dos formas de realizar la autenticación.

## Via web browser
```{r}
## autheticate via web browser

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)
  
```
Se debe abrir tu explorador y solicitará que inicies sesión en Twitter

## Via access token

Esta conexión se realiza directamente, para ello debes obtener el **access_token** y **access_secret**
```{r}
## autheticate via access token

token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = "943888858298638337-Du8GwFikyGmgnNMTZfWA2Eg2jeq59hP",
  access_secret = "hdgIkpOZvP3qAB4YWR8cWl1TrCXEfw8Lihag5JXKIgIlc")
  
```


# Paso 2:Publica un Tweet desde R

Vamos a probar tu conexión publicando un tuit desde la consola de R

```{r}
# Publicar un Tweet desde R
# Puedes cambiar/personalizar tu mensaje, conserva el Hastag(#) para un ejercicio posterior

post_tweet("Hola, Este es mi primer tuit desde el Workshop Analítica con redes sociales - Twitter #ColombiaAI")

```

# Paso 3:Explorar la REST API

### Busqueda por palabra clave

Buscaremos tuits por palabra clave o *hastag*, etc. Esta opción retorna máximo 9 días de información.

* Puedes elegir si quieres que tu consulta contenga *retweets* modificando el parámetro ``` include_rts = TRUE```
* Puedes elegir cuantos tuits quieres obtener. El limite máximo es de 18.000 cada 15min y . modificando el parámetro ``` n = 100```
* Algunas opciones de busqueda:
  * **"X"** : Buscará X
  * **"X Y"** = Buscará que este X y Y en cualquier orden en el tuit.
  * **"X O Y"** = Buscará que este X o Y en el tuit.
  * **"'X Y'"** = Buscará que en el tuit esta exactamente "X Y"

```{r}

# Buscar tweets por palabra clave, hastag, etc. Traeremos 500 sin incluir retweets

mipalabra = "Digita tu palabra o palabras aca"
tweets_KW <- search_tweets(mipalabra, n = 100, include_rts = FALSE)

view(tweets_KW)
columns(tweets_KW)

```
Puedes observar toda la información que se obtiene.

Ahora observa la serie de tiempo de tus resultados, el intervalo de tiempo es configurable. Consulta ```?ts_plot```

```{r}
## Serie de tiempo de tus resultados

ts_plot(tweets_KW, by = "hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = paste("Línea de tiempo de", mipalabra,"en Twitter",sep =" "),
    subtitle = "Conteo por intervalos de horas",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )
  
```

### Busqueda por Usuario: Time Line

Buscaremos y descargaremos los tuits hechos por un usuario. Esta opción retorna máximo 3200 tuits. También se puede configurar si quieres que tu consulta contenga *retweets* modificando el parámetro ``` include_rts = TRUE```

En este ejericio vamos a obtener el TL de 3 cuentas de nuestro interes.

```{r}
## Vamos a obtener el TL de 3 cuentas de nuestro interes.

Cuenta1 = "Avianca"
Cuenta2 = "LATAM_CO"
Cuenta3 = "VivaAirCol"

## Obtenemos el TL y guardamos en tmls 

tmls <- get_timelines(c(Cuenta1, Cuenta2, Cuenta3), n = 3200)

## podemos ver cuantos tuits recuperamos para cada usuario

tmls %>% group_by(screen_name) %>% summarise(numero_tweets = n()) 

```
Ahora comparemos la serie de tiempo de las 3 cuentas.

```{r}
library(lubridate)

ggplot(tmls, aes(x = as.Date(created_at), fill = screen_name)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ screen_name, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
  
```




