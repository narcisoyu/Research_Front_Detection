######################################
## An??lisis bibliom??trico twitter:  ##
##     Descriptivos referencias     ##
######################################

####Bibliotecas ####

library(bibliometrix)

## Cargar los datos generados hasta el momento ##
#################################################
load('twitter-wos-articulos-001.RData')

######################################
### BIBLIOMETRICS con Bibliometrix ###
######################################

## Crea elemento bibliometrics a partir de tabla wos
bibliometrics_wos <- biblioAnalysis(wos, sep = ";")

#Teoricamente se puede tener un summary directamente por: 
summary(bibliometrics_wos, k=20, pause=F, width=130) #y por este summary se puede elaborar la descripcion general.

## Crea datos adicionales

# Most cited first authors
citations_AU <- citations(wos, field = "author", sep = ";")

# Most cited references
citations_ART <-citations(wos, field = "article", sep = ";")

# Local citations
# How many times an author included in this collection have been cited by other authors also in the collection.
localCitations <- localCitations(wos, sep = ";")

###################################
## Modificaciones a la tabla wos ##
###################################

#First Author of each cited reference
#wos_new <- metaTagExtraction(wos, Field = "DE", sep = ";")

#Source of each cited reference
#wos <- metaTagExtraction(wos, Field = "CR_SO", sep = ";")

#No es necesario crear campos adicionales si se utilizan funciones de bibliometrix
# Por ejemplo: Pa??s del primer autor: wos$CO <- bibliometrics_wos$CO

#####################################
## Descriptivos (version anterior) ##
#####################################

# Utilizamos la funci??n *summary*, que nos mostrar?? la informaci??n principal de los datos bibliogr??ficos

# summary accepts two additional arguments: (1) k is a formatting value that indicates the number of rows of each table. (2) pause is a logical value (TRUE or FALSE) used to allow (or not) pause in screen scrolling.

#N??mero de elementos a listar en tablas (posterior a summary)
nelements <- 20

#Env??a resultados a archivo
sink(file="/twitter-wos-articulos-DescriptivosReferencias-01.txt")

cat("Informaci??n bibliogr??fica\n")
cat("Informaci??n descriptiva principal\n")
cat("* annual scientific production + Annual Percentage Grouwth Rate\n")
cat("* most productive authors\n")
cat("* most productive countries\n")
cat("* total citation per country\n")
cat("* most relevant sources (journals)\n")
cat("* most relevant keywords (DE & ID).\n")

## Muestra el sumario generado por biblioAnalysis (pause permite hacer pausa tras cada info de summary)

summary <- summary(bibliometrics_wos, k = nelements, pause = FALSE)

#Tipos de publicaciones
#head(sort(table(wos$DT), decreasing = TRUE), n=nelements)
cat("Tipos de publicaciones (DT)\n")
table(wos$DT)

#Tipo de publicaci??n (2)
#head(sort(table(wos$PT), decreasing = TRUE), n=nelements)
cat("\n\nTipos de publicaciones (PT)\n")
table(wos$PT)

#Idioma de las referencias
cat("\n\nIdioma de las referencias\n")
head(sort(table(wos$LA), decreasing = TRUE), n=nelements)

#Subject de la revista
cat("\n\nSubject (SC) de las revistas\n")
head(sort(table(wos$SC), decreasing = TRUE), n=nelements)

#Autores m??s citados
cat("\n\nAutores m??s citados\n")
head(citations_AU$Cited, n = nelements)

#Art??culos m??s citados
cat("\n\nArt??culos m??s citados\n")
#print(as.data.frame(Cited_ART$Cited[1:nelements]))
head(citations_ART$Cited, n = nelements)

# Local citations
cat("\n\nLocal citations")
cat("\nHow many times an author/paper included in this collection have been cited by other authors also in the collection.")

cat("\n\nAuthors\n")
head(localCitations$Authors, n = nelements)

cat("\n\nPapers\n")
head(localCitations$Papers, n = nelements )

#Subject de las revistas
cat("\n\nSubject (SC) de las revistas\n")
head(sort(table(wos$SC), decreasing = TRUE), n=nelements)

sink() #Detiene la salida a archivo

# Gr??ficos
plot(bibliometrics_wos, k=10, pause=FALSE)

save(list = ls(all = TRUE), file= "bibliometric-wos.RData")
