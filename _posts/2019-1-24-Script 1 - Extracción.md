---
layout: post
title: "Analítica con redes sociales - S1: Collecting Twitter Data"
---

**Objetivo**: En este script realizaremos la conexión a la API de *Twitter* con la libreria [RTweet](https://rtweet.info/reference/index.html) , a traves de nuestra [Twitter App](https://developer.twitter.com).

Probaremos las diferentes opciones de extraccion que tenemos con esta API:

* Extracción por palabra clave
* Extracción de TL por usuario
* Extracción de Amigos (Friends) por usuario
* Extracción de Seguidores (Followers) por usuario
* Extracción de Usuarios por palabra clave en Bio
* Extracción vía Stream API

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

# Paso 1: Autenticación

Realizaremos la autenticación con las claves de  nuestra [Twitter App](https://developer.twitter.com)

```{r}
##########################################################################################
####### Collecting Twitter Data
##########################################################################################

### App

appname <- "TUAPLICACION"
##Consumer API keys

### API key
key <- "TU API KEY"
### API secret key
secret <- "TU API SECRET KEY"

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
  access_token = "TU ACCESS TOKEN",
  access_secret = "TU ACCESS SECRET")
  
```


# Paso 2: Publica un Tweet desde R

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

View(tweets_KW)
colnames(tweets_KW)

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

### Busqueda por Usuario: Amigos (Friends)

Buscaremos y descargaremos las cuentas que sigue un usuario específico.

El limite es de 15 de estas solicitudes por 15 minutos, que máximo traen 5000 amigos.

Para seguir más de 5000 cuentas las cuentas deben cumplir ciertos requisitos. En consecuencia, la gran mayoría de los usuarios siguen menos de 5000 cuentas.


```{r}
## Obtener los IDs de usario que sigue la cuenta objetivo

CuentaObj_fds <- get_friends("ColombiaAI")

## Obtener la información de las cuentas
CuentaObj_fds_data <- lookup_users(ColombiaAI_fds$user_id)

```

### Busqueda por Usuario: Seguidores (Followers)

Buscaremos y descargaremos las cuentas que sigue un usuario específico.

5000, que es el número máximo de seguidores devueltos por una sola llamada. Permite hasta 15 de estas solicitudes cada 15 minutos, lpor tanto el número máximo de seguidores sin espera es de 75,000.

```{r}

## Obtener los IDs de usario que siguen a la cuenta objetivo
CuentaObj_flw <- get_followers("ColombiaAI", n = 5000)

## Obtener la información de las cuentas
CuentaObj_flw_data <- lookup_users(CuentaObj_flw$user_id)

```

### Busqueda por Usuario: Palabra clave en Bio

Buscaremos y descargaremos las cuentas que tienen alguna palabra clave en su biografía.

```{r}
## search for users with #rstats in their profiles
usrs <- search_users("Antropologo", n = 1000)
```
### Otras funciones:

* get_retweets
* get_retweeters
* get_trends
* get_mentions
* get_favorites


# Paso 4:Explorar la Stream API

La API de *Streaming* de Twitter permite el acceso casi en tiempo real a varios subconjuntos de datos de Twitter. 
Se pueden obtener de varias maneras: por ID de usuario, por palabra clave, por muestreo aleatorio, por ubicación geográfica, etc. 

### Busqueda aleatoria por tiempo

```{r}
## Busqueda por 30 segundos (default)
StrTw <- stream_tweets("")
```

### Busqueda aleatoria por palabra clave

```{r}
## stream tuits sobre una palabra clave por el tiempo definido
stream_tweets(
  "transmilenio",
  timeout = 30,
  file_name = "STRtweetsabout.json",
  parse = FALSE
)

## Leer los datos
STRtweets <- parse_stream("STRtweetsabout.json")
```

# Paso 5:Loop manejando el Rate limit

Los límites de la API de Twitter limitan el número de Tuits a 18,000 cada 15 minutos. Para obtener más, se establece ```retryonratelimit = TRUE``` y rtweet esperará a que se restablezca el límite de velocidad. 

Se debe tener en cuenta el tiempo que tardará la descarga al aplicar estos limites. Por ejmplo 2 millones de tuits tardarían mínimo 2 horas.

Así mismo seguirá aplicando el límite de 9 días como máximo.

```{r}
## retryonratelimit - Loop manejando el Rate limit

# Busqueda de 65,000 tweets por palabra clave
# TT recientes 20190125

TransMilenio <- search_tweets("TransMilenio", n = 65000, retryonratelimit = TRUE, include_rts = FALSE)
Copa <- search_tweets("Copa America", n = 65000, retryonratelimit = TRUE, include_rts = FALSE)
ELN <- search_tweets("ELN", n = 65000, retryonratelimit = TRUE, include_rts = FALSE)

```

# Exportar los datos

Hasta este punto hemos realizado varios ejemplos de extracción de datos haciendo uso del paquete rtweet.

Podemos almacenar cualquiera de las descargas que realizamos para esto utilizaremos la función ``` write_as_csv```

```{r}
# Tuits palabras clave
write_as_csv(tweets_KW, "tweets_KW.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
#timelines
write_as_csv(tmls, "tmls.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
# Amigos de cuenta objetivo
write_as_csv(CuentaObj_fds_data, "CuentaObj_fds_data.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
# Seguidores de cuenta objetivo
write_as_csv(CuentaObj_flw_data, "CuentaObj_flw_data.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
# Usuarios por palabra clave en bio
write_as_csv(usrs, "usrs.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
```


Podemos leer archivos *.csv* que hemos guardado con la función ``` read_twitter_csv```
```{r}
tmls<- read_twitter_csv("tmls.csv", unflatten = FALSE)
```



