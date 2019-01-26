---
layout: post
title: "Analítica con redes sociales - S2: Text Mining"
---

**Objetivo**: En este script realizaremos la minería de datos de la información que obtuvimos en el capítulo de extracción.

Con el set de datos seleccionado y estructurado, debemos realizar una limpieza dada la naturaleza de los datos, en **Twitter** los usuarios pueden escribir de la forma que quieran, lo que suele resultar en un uso elevado de abreviaturas, mala ortogafía y signos de puntuación.

También debemos estructurar la información, lo haremos identificando los *tokens* (unidades gramaticales más pequeñas), lo que implica representar el texto como una lista de palabras mediante una representación vectorial.

### Importar los datos
Podemos leer archivos *.csv* que hemos guardado con la función ``` read_twitter_csv```

Para este ejercicio trabajaremos con las TimeLines de tres usuarios.

```{r}
#Lineas de tiempo
tmls<- read_twitter_csv("tmls_3.csv", unflatten = FALSE)
```

### Selección de variables
Realizamos una selección de variables que utilizaremos para nuestro análisis.

```{r}
DF <- tmls %>% select(status_id,
                      created_at,
                      screen_name,
                      text,
                      source,
                      lang)
```

### Limpieza de texto

Eliminar todo aquello que no aporte información relevante para el análisis.
Para este ejercicio procederemos a eliminar:

+ Caracteres no informativos (@#$%&/ etc)
+ Signos de puntuación
+ Números
+ Espacios en blanco dobles
+ Caracteres sueltos

```{r }
#########################################################
####### LIMPIAR DATOS
#########################################################
# Retira caracteres extraños
DF$text <- gsub("[[:cntrl:]]", " ", DF$text)
# Retira URLs
DF$text <- gsub("\\<http\\S*\\>|[0-9]", " ", DF$text)
# Todo a minuscula
DF$text<- tolower(DF$text)
# Retira menciones
DF$text <- gsub("@\\w+", "", DF$text)
# Eliminación de signos de puntuación
DF$text <- removePunctuation(DF$text)
# Eliminación de números
DF$text <- removeNumbers(DF$text)
# Eliminación de espacios en blanco múltiples
DF$text <- stripWhitespace(DF$text)

```

### Tokenizar

*Tokenizar* un texto consiste en dividirlo en las unidades que lo conforman, entendiendo por unidad el elemento más sencillo con significado propio para el análisis en cuestión, en este caso, por palabras.

```{r }
#########################################################
####### METODOS POR PALABRAS
#########################################################

#######################
####### TOKENIZAR
#######################
tokenizar <- function(texto)
  {
  nuevo_texto <- str_split(texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
  }

# Se aplica la función de tokenización a cada observación
DF <- DF %>% mutate(tex_token = map(.x = DF$text,.f = tokenizar))

###################
####### "TIDYZAR""
##################

# "Tidyzar" los datos (Flatten/unflatten)
DF_tidy <- DF %>% select(-text) %>% unnest()
DF_tidy <- DF_tidy %>% rename(token = tex_token)
#Ejemplo salida
head(DF_tidy) 

```

Para cada observación se realizó la división por las palabras que la conforman, y con base en esto se realizó analitica descriptiva. 
Este es un ejemplo de *tokenización* presentando las primeras 15 palabras de una orden:

```{r}
DF_tidy[1:15,] 
```

La siguiente tabla presenta la cantidad de palabras encontradas en las cuentas analizadas y el total de palabras distintas.

```{r}
#Cantidad de ordenes por screen_name
DFO_G <- DF_tidy %>% select(screen_name, source) %>% distinct() %>%  group_by(screen_name) %>%  summarise(Fuentes = n()) 
#Cantidad palabras por screen_name
DFO_G1<- DF_tidy %>% group_by(screen_name) %>% summarise(Palabras = n()) 
#Cantidad palabras distintas por screen_name
DFO_G2 <-DF_tidy %>% select(screen_name, token) %>% distinct() %>%  group_by(screen_name) %>%
summarise(Distintas = n()) 
DFO_G <- inner_join(DFO_G,DFO_G1, by= "screen_name")
DFO_G <- inner_join(DFO_G,DFO_G2, by= "screen_name")
remove(DFO_G1, DFO_G2)

kable(DFO_G)
```

Observe y concluya acerca de la cantidad de palabras distintas (Vocabulario) por cada usuario, esta cantidad refleja la riqueza de lenguaje utilizado y/o puede representar el uso de muchas abreviaturas.

La siguiente gráfica presenta el *top 10* de palabras más utilizadas por cada usuario:
```{r }
#Representación gráfica de las frecuencias
GG1 <- DF_tidy %>% group_by(screen_name, token) %>% count(token) %>% group_by(screen_name) %>%
  top_n(10, n) %>% arrange(screen_name, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = screen_name)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~screen_name,scales = "free", ncol = 1, drop = TRUE)
GG1
```

Se puede observar que el top 10 de las palabras corresponde a lo que se conoce como *stopwords*, es decir artículos, preposiciones, pronombres, en general, palabras que no aportan información relevante. Sin embargo las stopwords dependen del contexto de análisis, por lo que se hace necesario siempre crear y/o adaptar un listado para su filtrado. para esto se adjunto un listado de palabras *stopwords* en español que se consideran útiles para este análisis.

```{r}

#Carga archivo de palabras vacías personalizada y lo convierte a ASCII
sw <- readLines("stopwordses.txt",encoding="UTF-8")
sw = iconv(sw, to="ASCII//TRANSLIT")

# SE FILTRAN STOPWORDS
DF_tidy <- DF_tidy %>% filter(!(token %in% sw))
```

Una vez hemos removido las *stopwords* definidas, podemos generar la siguiente gráfica que presenta el *top 10* de palabras más utilizadas, por cada usuario:
```{r}
#Representación gráfica de las frecuencias
GG2 <- DF_tidy %>% group_by(screen_name, token) %>% count(token) %>% group_by(screen_name) %>%
  top_n(10, n) %>% arrange(screen_name, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = screen_name)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~screen_name,scales = "free", ncol = 1, drop = TRUE)
GG2
```
Analiza estas palabras y determina si se requiere eliminar algunas palabras que sigas considerando sin valor.

```{r}
# Se crea una lista de las stopwords adicionales
lista_stopwords <- c('is', 'js', 'ay')

# Se filtran las stopwords
DF_tidy <- DF_tidy %>% filter(!(token %in% lista_stopwords))
```

Por último haremos uso de la herramienta visual de nubes de palabras o wordClouds, que nos permitira realizar otro análisis de posibles palabras a retirar.
Para ellos crearemos nuestras nubes de palabras por usuario

```{r}
#Funcion para nube de palabras
wordcloud_custom <- function(grupo, df){
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}
# Agrupar por usuario y lista de palabras
df_grouped <- DF_tidy %>% group_by(screen_name, token) %>% count(token) %>%
              group_by(screen_name) %>% mutate(frecuencia = n / n()) %>%
              arrange(screen_name, desc(frecuencia)) %>% nest() 
#Recorrer y generar los 3 WC
walk2(.x = df_grouped$screen_name, .y = df_grouped$data, .f = wordcloud_custom)
```

Analizando la nube de palabras, identifique otras palabras que requiera retirar y applique el filtro ya visto.

Así mismo una vez finalice la limpieza de su extracción de lineas de tiempo, realice ahora la limpieza de otra extracción realizada (por palabras, streaming, etc)

Tenga cuidado de no reemplazar los DataFrames ya creados. Debe copiar el codigo y modificar las salidas.
