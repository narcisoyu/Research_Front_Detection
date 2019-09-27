##################################################
## An??lisis bibliom??trico:                      ##
##    Carga datos brutos                        ##
##    y modificaciones generales datos          ##
##################################################

#Bibliotecas
#library(stringr)
#library(igraph)
#library(dplyr) # Para manipulaci??n tablas
#library(knitr) #opciones markdown, por ejemplo tablas
##Opcional, s??lo si se va a generar PDFs con markdwon
#library(extrafont) #Para cargar fuentes
library(bibliometrix)
library(readODS) # Para leer ODS "reemplazar" con la depuraci??n keywords
library(DataCombine) # Para hacer la depuraci??n
library(stringr) # Para hacer pasos previos a la depuraci??n

#carga datos texto plano (sin formato)
#largechar <- readLines("../twitter-wos-articulos.txt")
largechar <- readFiles("/Users/Jingyuan/Desktop/UAB\ DOCTORADO\ DOCUMENT/SCIENTOMETRICS/original\ plain\ text\ files/bibliometric.txt")
## Crea tabla wos a partir de los datos cargados
wos <- isi2df(largechar) #Convierte en Data con bibliometrix
#wos <- convert2df(largechar, dbsource = "isi", format = "plaintext")
wos_orig <- wos

## Elimina "largechar"
remove(largechar)

## Podemos cargar los datos generados hasta el momento sin necesidad de "carga datos texto plano"
## load('twitter-wos-articulos-01.RData')

###################################################
## Depurar palabras clave autores(DE) y kw+ (ID) ##
###################################################

## Primero copia los campos originales para conservar copia

wos$DE_orig <- wos$DE
wos$ID_orig <- wos$ID

# Sustituye espacions en blanco por guiones
wos$DE <- str_replace_all(wos$DE, ";;", ";")
wos$DE <- str_replace_all(wos$DE, ";\\s*", "#####")
wos$DE <- str_replace_all(wos$DE, "\\s+", " ")
wos$DE <- str_replace_all(wos$DE, " ", "-")
wos$DE <- str_replace_all(wos$DE, "#####", ";")

wos$ID <- str_replace_all(wos$ID, ";;", ";")
wos$ID <- str_replace_all(wos$ID, ";\\s*", "#####")
wos$ID <- str_replace_all(wos$ID, "\\s+", " ")
wos$ID <- str_replace_all(wos$ID, " ", "-")
wos$ID <- str_replace_all(wos$ID, "#####", ";")

# Carga el ods con las palabras a reemplazar
reemplazar <- as.data.frame(read_ods("../reemplazar.ods"))

# Para que el reemplazo funciones, hay que encontrar la forma de que la b??squeda s??lo de resultados con palabra completa
reemplazar$desde <- (paste(";", reemplazar$desde, ";", sep =""))
reemplazar$hacia <- (paste(";", reemplazar$hacia, ";", sep =""))

#tambi??n hay que a??adir el ";" al inicio y final de los kw
wos$DE <- (paste(";", wos$DE, ";", sep =""))
wos$ID <- (paste(";", wos$ID, ";", sep =""))

# Ejecuta la depuraci??n
wos$DE <- FindReplace(data= wos, Var= "DE", replaceData = reemplazar, from = "desde", to = "hacia", exact = FALSE, vector = TRUE)
wos$ID <- FindReplace(data= wos, Var= "ID", replaceData = reemplazar, from = "desde", to = "hacia", exact = FALSE, vector = TRUE)

# Quitar "twitter"
wos$DE <- str_replace_all(wos$DE, ";", "; ")
wos <- termExtraction(wos, Field = "DE", remove.terms = "TWITTER")
wos$DE <- wos$DE_TM

wos$ID <- str_replace_all(wos$ID, ";", "; ")
wos <- termExtraction(wos, Field = "ID", remove.terms = "TWITTER")
wos$ID <- wos$ID_TM

# Quitar el ";" inicial y final en DE e ID
#wos_2$DE <- str_sub(wos_2$DE, start=1, end=-1)
#wos_2$ID <- str_sub(wos_2$ID, start=1, end=-1)

# Sustituye "NA" por NA (missing) (comprobar n??mero columna)
#wos_2[,38 ][wos_2[,38]=="NA"] <- NA
#wos_2[,39 ][wos_2[,39]=="NA"] <- NA

save(list = ls(all = TRUE), file= "twitter-wos-articulos-001.RData")

