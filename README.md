# RSU.-R-Clusters
codigo en R para análisis de conglomerados
######################################################################
#            PRACTICA DE RESIDUOS SOLIDOS URBANOS INEGI
######################################################################
#Introduccion al trabajo:
#El objetivo del presente documento es presentar los elementos fundamentales y las características principales del fenómeno de los RSU, 
#así como la aplicación de las técnicas estadísticas de análisis de componentes principales y de análisis de conglomerados a esos datos 
#y una interpretación de los resultados derivados, mismos que pueden servir como guía en el proceso de toma de decisiones en políticas 
#públicas en la materia. En particular, se muestra que el fenómeno es básicamente bidimensional y que las entidades federativas se aglomeran, 
#según el fenómeno de los RSU, en estratos bien delimitados, presentándose así un desequilibrio geográfico y de infraestructura en el tema. 

#VAR1 = proporción de municipios y delegaciones con centros de acopio respecto al total de municipios en la entidad federativa.
#VAR2= proporción del total municipios y delegaciones con centros de acopio respecto al total nacional de centros de acopio.
#VAR3 = proporción de municipios y delegaciones con estaciones de transferencia, sólo almacenamiento temporal y traspaso respecto al total de municipios en la entidad federativa.
#VAR4 = proporción de municipios y delegaciones con estaciones de transferencia, compactación y/o selección de materiales respecto al total de municipios en la entidad federativa.
#VAR5 = proporción de municipios y delegaciones con estudios sobre la generación de residuos sólidos urbanos respecto al total de municipios en la entidad federativa.
#VAR6 = proporción de municipios y delegaciones con programas locales orientados a la gestión integral de los residuos sólidos urbanos respecto al total de municipios en la entidad federativa.
#VAR7 = proporción de municipios con estaciones de transferencia respecto al total de municipios de la entidad federativa.
#VAR8 = proporción del total de municipios con estaciones de transferencia respecto al total nacional.
#VAR9 = proporción de vehículos utilizados para la recolección de residuos sólidos urbanos con compactador respecto al total en la entidad federativa.
#VAR10 = proporción de vehículos utilizados para la recolección de residuos sólidos urbanos con caja abierta respecto al total en la entidad federativa.
#VAR11 = proporción de otros vehículos utilizados para la recolección de residuos sólidos urbanos y otro tipo de vehículos respecto al total en la entidad federativa.
#VAR12 = proporción del total de municipios y delegaciones con disponibilidad de servicios relacionados con los residuos sólidos urbanos respecto al total en la entidad federativa.
#VAR13 = proporción de municipios y delegaciones con disponibilidad de servicios de sólo recolección y disposición final de residuos sólidos urbanos respecto al total en la entidad federativa.
#VAR14 = proporción de municipios y delegaciones con disponibilidad de servicios de recolección, disposición final y tratamiento de residuos sólidos urbanos respecto al total en la entidad federativa.
#VAR15 = proporción del promedio diario de pesaje de residuos sólidos urbanos recolectados respecto al total en la entidad federativa.
#VAR16 = proporción del promedio diario por vehículo/capacidad/viajes de residuos sólidos urbanos recolectados respecto al total en la entidad federativa.
#VAR17 = proporción del promedio diario de otros métodos de obtención de residuos sólidos urbanos recolectados respecto al total en la entidad federativa.
#VAR18 = proporción de municipios y delegaciones con servicio de recolección y disposición final con reglamento relacionado con los residuos sólidos urbanos respecto al total en la entidad federativa.
#VAR19 = proporción de sitios de disposición final tipo relleno sanitario reportados como destino de los residuos sólidos urbanos respecto al total en la entidad federativa.
#VAR20 = proporción de sitios de disposición final tipo tiradero a cielo abierto reportados como destino de los residuos sólidos urbanos respecto al total en la entidad federativa.
#requiere(foreign)
a<-read.csv("//Users//zyanmctz//Desktop//Variables-de-proporcion1.csv") #Archivo que se va a importar.
a
a = na.omit(a)
x=a[-32,-1]
z<-scale(x[,1:9])
row.names(z)<-as.character(a[-32,1])
z
#Analisis con las Variables Estandarizadas
cj<-hclust(dist(z)^2,method = "average")
#Obtenemos la tabla de agrupamiento
cj$merge
cj$height  #valor de media proximidad a la que se van formando los cluster
print(cj)
#...........DENDOGRAMA..............
plot(cj,main="Dendograma",labels = row.names(z),hang = -1)
#Funciones cutree
#Puede usar el número de grupos a efectuar o bien un valor de la medida de distanciamiento (h) para agrupar.
g=cutree(cj,k=4)
cbind(z,g)
table(g)
#Kmeans indicando solo el numero de grupos
kmeans(z,4)
#Representación gráfica de las 2 primeras componentes de Z
#Usaremos ahora las dos primeras componentes principales de las variables del data frame Z para construir los 4 grupos mediante el método kmeans.
#Previo análisis de componentes principales. Resume las 9 variables en las 2 que capturan la máxima variabilidad del total.
acp=princomp(z)
#Tomamos las puntuaciones en las componentes primeras para cada Comunidad Autónoma
comp=predict(acp)[,1:2]
#Análisis cluster con las 2 componentes principales
km2=kmeans(comp,4)
#Representación gráfica de las Comunidades según las puntuaciones en las componentes y el grupo al que se han asignado. Representación de los centroides (medias de los grupos en las componentes)
plot(comp,col=km2$cluster) #cada cluster de un color
points(km2$centers, col = 1:4, pch = 8, cex=2) #medias de los cluster en las componentes 
text(comp[,1],comp[,2],labels=rownames(z),col=km2$cluster) #etiquetas de nombres Comunidades
#Kmean indicando las medias de los grupos
g=cutree(cj,k=4) #asignación de los elementos a cada grupo
g
inicial=tapply(z,list(rep(g,ncol(z)),col(z)),mean) #Calculo de medias por grupo
inicial
#Análisis cluster tomando como agrupamiento inicial el derivado previamente con hclust
km3=kmeans(z,inicial)
km3

