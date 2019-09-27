#############################################
## Pasar elementos de cluster a tabla wos
#############################################

library(bibliometrix)
library(igraph)
library(dplyr)
library(Matrix)

#Cargar los datos (RData)
load('twitter-wos-articulos-001.RData')

### biblio-coupling países (primer autor) ####
biblio_coupling <- biblioNetwork(wos, analysis = "coupling", network = "references", sep = ";")

# Normalización de la tabla de coocurrencias
biblio_coupling_norm <- normalizeSimilarity(biblio_coupling, type = "association")


# a partir de bibliocoupling (u otra matriz de coocurrencias)
# Definir el criterio de filtro para seleccionar menos datos
# Puede ser degree o suma de los valores de las filas
# si es suma de valores de filas, primero hay que quitar diagonal

diag(biblio_coupling_norm) <- NA

#grado <- degree(biblio_coupling@i)
grado_w <- rowSums(biblio_coupling_norm, na.rm = TRUE)

# un posible criterio es seleccionar valores superiores a la media, pero hay que probar
# diferentes opciones para que el filtro sea adecuado (tamaño resultante)
n <- mean(grado_w)

#y ahora aplicar el filtro subset?
bc <- biblio_coupling_norm[colSums(biblio_coupling_norm, na.rm = TRUE)>= n, colSums(biblio_coupling_norm, na.rm = TRUE)>= n]

graph = graph.adjacency(bc, mode = 'undirected', diag = FALSE, weighted = TRUE)

clp <- cluster_louvain(graph, weights = E(graph)$weight)

#A partir del análisis de cluster, crear data frame con nombres y pertenencia
xx <- data.frame(cbind(id = clp$names, grupo = clp$membership))

## Extraemos el identificador correcto a partir de los datos tipo "2 NA 2"
xx$id <- regmatches(xx$id, regexpr("[[:digit:]]+",xx$id))


## Añadir columna a la tabla original wos (la que se haya utilizado para el análisis)
## La columna se tiene que llamar "id"
wos$id <- rownames(wos)

## Nueva tabla wos seleccionando sólo las coincidencias de X1 (nombres) y añadiendo el cluster de pertenencia (X2)
wos_new <- dplyr::inner_join(wos, xx, by = "id")

#luego se puede crear "manualmente" una tabla para cada grupo
wos_c1 <- dplyr::filter(wos_new, grupo == 1)
wos_c2 <- dplyr::filter(wos_new, grupo == 2)
wos_c3 <- dplyr::filter(wos_new, grupo == 3)
wos_c4 <- dplyr::filter(wos_new, grupo == 4)
wos_c5 <- dplyr::filter(wos_new, grupo == 5)
wos_c6 <- dplyr::filter(wos_new, grupo == 6)
wos_c7 <- dplyr::filter(wos_new, grupo == 7)
wos_c8 <- dplyr::filter(wos_new, grupo == 8)


#o crear una "tabla múltiple"
#wos_new <- split(wos_new, wos_new$grupo)

# a la que se puede acceder con:
#wos_new$`1`
#wos_new$`2`

#y a cada uno de los valores...
#wos_new$`1`$AU

#o 
#wos_new[[3]]$AU

#Extraer los clusters
#cluster1 <- wos_new$`1`
#cluster2 <- wos_new$`2`
#cluster3 <- wos_new$`3`
#cluster4 <- wos_new$`4`
#cluster5 <- wos_new$`5`

#por lo tanto, se podría hacer (aunque los tarda más en hacerlo)
descriptivos_1 <- biblioAnalysis(wos_c1)
descriptivos_2 <- biblioAnalysis(wos_c2)
descriptivos_3 <- biblioAnalysis(wos_c3)
descriptivos_4 <- biblioAnalysis(wos_c4)
descriptivos_5 <- biblioAnalysis(wos_c5)
descriptivos_6 <- biblioAnalysis(wos_c6)
descriptivos_7 <- biblioAnalysis(wos_c7)
descriptivos_8 <- biblioAnalysis(wos_c8)

nelements = 20

summary_1 <- summary(descriptivos_1, k = nelements, pause = FALSE)
summary_2 <- summary(descriptivos_2, k = nelements, pause = FALSE)
summary_3 <- summary(descriptivos_3, k = nelements, pause = FALSE)
summary_4 <- summary(descriptivos_4, k = nelements, pause = FALSE)
summary_5 <- summary(descriptivos_5, k = nelements, pause = FALSE)
summary_6 <- summary(descriptivos_6, k = nelements, pause = FALSE)
summary_7 <- summary(descriptivos_7, k = nelements, pause = FALSE)
summary_8 <- summary(descriptivos_8, k = nelements, pause = FALSE)

#aunque se puede hacer con un for...

# descrip <- vector("list", length(wos_new))
# for (h in 1:length(wos_new))
#   {descrip[[h]] <- biblioAnalysis(wos_new[[h]])
#   }

#Estructura Conceptual
CS_c1 <- conceptualStructure(wos_c1, field = "DE", minDegree = 12, k.max = 12, labelsize = 10)
CS_c2 <- conceptualStructure(wos_c2, field = "ID", minDegree = 11, k.max = 10, labelsize = 10)
CS_c3 <- conceptualStructure(wos_c3, field = "ID", minDegree = 12, k.max = 12, labelsize = 10)
CS_c4 <- conceptualStructure(wos_c4, field = "ID", minDegree = 5, k.max = 5, labelsize = 10)
CS_c5 <- conceptualStructure(wos_c5, field = "ID", minDegree = 9, k.max = 9, labelsize = 10)
CS_c6 <- conceptualStructure(wos_c6, field = "ID", minDegree = 20, k.max = 20, labelsize = 10)
CS_c7 <- conceptualStructure(wos_c7, field = "ID", minDegree = 10, k.max = 10, labelsize = 10)
CS_c8 <- conceptualStructure(wos_c8, field = "ID", minDegree = 10, k.max = 10, labelsize = 10)

#Y para hacer el Thematic Map
#Thematic map cluster 1
NetMatrix_c1 <- biblioNetwork(wos_c1, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";")
S_c1 <- normalizeSimilarity(NetMatrix_c1, type = "association")
net_c1 <- networkPlot(S_c1, n = 100, Title = "co-occurrence network",type="fruchterman",
                      labelsize = 3, halo = FALSE, cluster = "infomap",remove.isolates=FALSE,
                      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res_c1 <- thematicMap(net_c1, NetMatrix_c1, S_c1)
plot(res_c1$map)

#Thematic map cluster 2
NetMatrix_c2 <- biblioNetwork(wos_c2, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";")
S_c2 <- normalizeSimilarity(NetMatrix_c2, type = "association")
net_c2 <- networkPlot(S_c2, n = 100, Title = "co-occurrence network",type="fruchterman",
                      labelsize = 3, halo = FALSE, cluster = "infomap",remove.isolates=FALSE,
                      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res_c2 <- thematicMap(net_c2, NetMatrix_c2, S_c2)
plot(res_c2$map)

#Thematic map cluster 3
NetMatrix_c3 <- biblioNetwork(wos_c3, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";")
S_c3 <- normalizeSimilarity(NetMatrix_c3, type = "association")
net_c3 <- networkPlot(S_c3, n = 100, Title = "co-occurrence network",type="fruchterman",
                      labelsize = 3, halo = FALSE, cluster = "infomap",remove.isolates=FALSE,
                      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res_c3 <- thematicMap(net_c3, NetMatrix_c3, S_c3)
plot(res_c3$map)

#Thematic map cluster 4
NetMatrix_c4 <- biblioNetwork(wos_c4, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";")
S_c4 <- normalizeSimilarity(NetMatrix_c4, type = "association")
net_c4 <- networkPlot(S_c4, n = 100, Title = "co-occurrence network",type="fruchterman",
                      labelsize = 3, halo = FALSE, cluster = "infomap",remove.isolates=FALSE,
                      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res_c4 <- thematicMap(net_c4, NetMatrix_c4, S_c4)
plot(res_c4$map)

#Thematic map cluster 5
NetMatrix_c5 <- biblioNetwork(wos_c5, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";")
S_c5 <- normalizeSimilarity(NetMatrix_c5, type = "association")
net_c5 <- networkPlot(S_c5, n = 100, Title = "co-occurrence network",type="fruchterman",
                      labelsize = 3, halo = FALSE, cluster = "infomap",remove.isolates=FALSE,
                      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res_c5 <- thematicMap(net_c5, NetMatrix_c5, S_c5)
plot(res_c5$map)

#Thematic map cluster 6
NetMatrix_c6 <- biblioNetwork(wos_c6, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";")
S_c6 <- normalizeSimilarity(NetMatrix_c6, type = "association")
net_c6 <- networkPlot(S_c6, n = 100, Title = "co-occurrence network",type="fruchterman",
                      labelsize = 3, halo = FALSE, cluster = "infomap",remove.isolates=FALSE,
                      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res_c6 <- thematicMap(net_c6, NetMatrix_c6, S_c6)
plot(res_c6$map)

#Thematic map cluster 7
NetMatrix_c7 <- biblioNetwork(wos_c7, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";")
S_c7 <- normalizeSimilarity(NetMatrix_c7, type = "association")
net_c7 <- networkPlot(S_c7, n = 100, Title = "co-occurrence network",type="fruchterman",
                      labelsize = 3, halo = FALSE, cluster = "infomap",remove.isolates=FALSE,
                      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res_c7 <- thematicMap(net_c7, NetMatrix_c7, S_c7)
plot(res_c7$map)

#Thematic map cluster 8
NetMatrix_c8 <- biblioNetwork(wos_c8, analysis = "co-occurrences",
                              network = "author_keywords", sep = ";")
S_c8 <- normalizeSimilarity(NetMatrix_c8, type = "association")
net_c8 <- networkPlot(S_c8, n = 100, Title = "co-occurrence network",type="fruchterman",
                      labelsize = 3, halo = FALSE, cluster = "infomap",remove.isolates=FALSE,
                      remove.multiple=FALSE, noloops=TRUE, weighted=TRUE)
res_c8 <- thematicMap(net_c8, NetMatrix_c8, S_c8)
plot(res_c8$map)


#Guardar los datos
#save(list = c("wos", "wos_2", "biblio_coupling"), file= "twitter-wos-articulos-association.RData")
save(list = ls(all = TRUE), file= "twitter-wos-articulos-association-cluster.RData")
