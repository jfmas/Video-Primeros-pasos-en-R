## Video Primeros pasos en R
## Operaciones básicas

print("Bienvenid@s al curso")
print("Bem vindes ao curso")


primervalor <- 5
segundovalor <- 4
suma <- primervalor + segundovalor
print(suma)
suma

suma <- sum(primervalor, segundovalor, primervalor)
print(suma)

# Datos de precipitación mensual en Ensenada, Baja California
Prec <- c(15, 40, 37, 37, 0, 0, 0, 0, 0, 7, 3, 77)
meses <- c("Enero", "Febrero", "Marzo", "Abril")
numeros <- 1:4

# Lista de los objetos
ls()

# Muestra el tipo de datos
class(Prec)
class(meses)
class(numeros)

print(prec) # prec no existe, es Prec 

# Pregunta es un vector?
is.vector(Prec)
is.vector(meses)
is.vector(numeros)

# Indica la longitud del vector
length(Prec)
length(meses)
length(numeros)

# Muestra el contenido del objeto en pantalla
print(Prec)
print(meses)
print(numeros)

# Calcula estadísticas básicas (Máxima, mínima, promedio y suma)
max(Prec)  # máximo
min(Prec)  # mínimo
mean(Prec) # promedio
sum(Prec)  # suma 

##  Importación de datos en R
# Determina la ruta del espacio de trabajo
setwd("/home/jf/pCloudDrive/libroRSIG2022")
# Muestra la ruta del espacio de trabajo
getwd()

# Carga la tabla "ensenada.csv" en el objeto tab
tab <- read.csv("recursos/ensenada.csv")

# Tipo del objeto tab
class(tab)

# Muestra la tabla completa
print(tab)

# Muestra las primeras filas de la tabla
head(tab)
head(tab, 3)  # Muestra las 3 primeras filas de la tabla
tail(tab, 2)  # Muestra las 2 últimas filas de la tabla

### Operaciones con tablas Dataframe
# Muestra nombres (encabezados) de las columnas
names(tab)

# Muestra una sola columna (P)
print(tab$P)

# Una columna de tabla es un vector
is.vector(tab$P)

# Calculemos el rango de temperatura (T max - T min)
# Nueva columna en la tabla: rango
tab$rango <- tab$Tmax - tab$Tmin

# Muestra las primeras filas de la tabla
head(tab)

# factor
tab$PCvid
class(tab$PCvid)
as.numeric(tab$PCvid)

# Guarda la tabla en un archivo de texto
write.table(tab, file="tabla.txt")

tab[1,2]
tab[,c(1,2,6)] # selecciona las filas enteras
tab[1:6, c(1:3,6)]
tab[-(10:12),-4]
tab[, c("mes","P","Tprom")] # equivalente a tab[,c(1,2,6)]

print(tab$Tmax > 25) # Resulta de la condición
tab[tab$Tmax > 25,] # Selección de filas
tab[tab$PCvid == "no",] # Con otra condición

# Selección de una columna
names(tab)
names(tab) == "Tmax"
tab[,names(tab) == "Tmax"]

# Selección de varias columnas
names(tab)
names(tab) %in% c("Tmin", "Tmax","Tprom")
tab[,names(tab) %in% c("Tmin", "Tmax","Tprom")]

# Selección de una celdas de la tabla
tab[tab$PCvid == "no",names(tab) == "Tmax"]

subset(tab,Tmax > 25) 
subset(tab,PCvid == "no")
subset(tab,PCvid == "no",select= c("mes","P","Tprom"))

## Paquete dplyr

library(dplyr)

dplyr::filter(tab,PCvid == "no")
dplyr::select(tab,c(mes, Tmin:Tprom))
dplyr::select(dplyr::filter(tab,PCvid == "no"),c(mes, Tmin:Tprom))

# Conversión entre tipos de objetos
# Conversión de data.frame a matriz
m <- as.matrix(tab[,1:7]) # Parte numérica de la tabla tab
class(m)
print(m)

# Indexación de matrices
# Indexación: 2a fila, 4a columna
print(tab[2,4])
print(m[2,4])
print(m[1:3,4])
print(m[,4])
print(m[1,])

# Unión de tablas
# Lee las dos tablas 
tab_cumul <- read.csv("recursos/Casos_cumul_BC.csv")
tab_pob <- read.csv("recursos/Pob_BCS.csv")
print(tab_pob)
print(tab_cumul)

# Inner Join
merge(tab_pob, tab_cumul)
merge(tab_pob, tab_cumul,by="CVEGEO")
# Full Join
merge(tab_pob, tab_cumul,by="CVEGEO",all=T)
## Left Join
merge(tab_pob, tab_cumul,by="CVEGEO",all.x=T)
## Right Join
merge(tab_pob, tab_cumul,by="CVEGEO",all.y=T)

library(dplyr)
left_join(tab_pob, tab_cumul,by="CVEGEO")

# Operaciones marginales: apply

# Crea una matriz de 3 x 3
# byrow=T: los números del vector entran por fila
m <- matrix(c(1,2,2,3,6,0,4,7,9),ncol=3,byrow=T)
print(m)

colSums(m)
rowSums(m)
colMeans(m)

apply(m,1,sum)
apply(m,2,sum)
apply(m,1,mean)
apply(m,1,max)
apply(m,2,sd)

# Operaciones por grupos
aggregate(P ~ PCvid, data = tab, FUN = "sum")
aggregate(x = tab$P, by = list(tab$PCvid), FUN = "sum")
aggregate(Tprom ~ PCvid, data = tab, FUN = "mean")

# con dplyr
por_estacion <- group_by(tab, PCvid)
summarise(por_estacion, Ptot = sum(P), Tprom_anual = mean(Tprom))

# Elaboración de gráficas
plot(tab$dias,tab$P)

plot(tab$dias,tab$P,  xlab="Número de días de lluvia", cex=0.8,
     ylab="Precipitación mensual (mm)", pch=22, col="darkblue", bg="blue",
     main="Relación días de lluvias / Precipitación", sub = "Ensenada, BC")

## Relación entre dos variables
# Correlación
cor(tab$dias,tab$P)

# Para cualquier duda, pedir ayuda!
help(cor)
?cor()
cor(tab$dias,tab$P, method = "pearson")
cor(tab$dias,tab$P, method = "spearman")

# Una regresión lineal entre la prec y el número de días de lluvia
reg <- lm(tab$P ~ tab$dias)

# Los resultados del ajuste lineal
summary(reg)
resumen <- summary(reg)

# Unas nuevas clases de objeto: lm (linear model) y summary.lm
class(reg)
class(resumen)

# summary.lm guarda la información en una matriz llamada coeffcients
resumen$coefficients
# Recuperando un elemento particular de la matriz (t value del intercept)
resumen$coefficients[1,3]

# Una lista (list) es una lista de objetos de diferentes tipos
lista <- list(Prec, reg, "lista rara")
lista

# Muestra el primer y el tercer elemento de la lista
lista[[1]]
lista[[3]]

# Creación de funciones

# Definición de una función para sumar un valor numérico
# con el doble de un segundo valor
Func <- function (a, b) {
  resultado <- a + 2 * b
  resultado
}

# Ejecución de la función
Func(3,7)  # 3 + 2 * 7 = 3 + 14 = 17

# Repeticiones

for (i in 1:6){
  print(i)
}

fac <- 1
for (i in 1:4){
  fac <- fac * i
}
print(fac)

vector <- c(0,0,0,0)
for (i in 1:4){
  vector[i] <- i*2
}
print(vector)

i <- 1
nombre <- paste("mapa",i,".txt",sep="")
print(nombre)

for (i in 1:4){
  nombre <- paste("mapa",i,".txt",sep="")
  print(nombre)
}

# Nombres bandas imagen Sentinel 2
for (i in c("B02","B03","B8A","TCI")){
  nombre <- paste("T23KNT_20170701T131241_",i,".TIF",sep="")
  print(nombre)
}

lista <- list(fac,i,vector,nombre,Func)
for (objeto in lista){
  print(class(objeto))
}

# Condiciones

x <- 1
print(x)
print(x == 1) # TRUE: Se cumple la condición
if (x == 1){sum <- x + 3} else {sum <- 6}
print(sum)

file.exists("recursos/ensenada.csv")
file.exists("recursos/fileausente.csv")

if(file.exists("recursos/ensenada.csv")){
  tab <- read.csv("recursos/ensenada.csv")} else {
    print("El archivo no se encuentra")}

Lista <- c("recursos/ensenada.csv", 
           "recursos/fileausente.csv", "Recursos/Ensenada.csv")
for (archivo in Lista){
  if (file.exists(archivo)){
    tab <- read.csv(archivo)
    print(head(tab))} else {
      print("El archivo no se encuentra")}
} # Cierra el loop

if (require(conflicted)){remove.packages("ABHgenotypeR")
  detach("package:ABHgenotypeR", unload=TRUE)}

print(require(dplyr))
print(require(ABHgenotypeR)) # Un paquete para visualizar genótipo
print(!require(ABHgenotypeR)) # El ! permite tener la respuesta opuesta

if (!require(ABHgenotypeR)){install.packages("ABHgenotypeR")} else {
  print("El paquete conflicted ya está instalado")
}

vector <- c(9,4,2,12,3,6)
suma <- 0
for (i in 1:length(vector)){
  if (vector[i] > 5) {suma <- suma + vector[i]} else {suma <- suma -1}
  print(suma)
}

# Operador pipe |>
resultado <- funcion3(funcion2(funcion1(entrada)))
resultado <- entrada |> funcion1() |> funcion2() |> funcion3()

# Dos funciones que se aplican de forma succesiva: 
# 2 líneas de código, un objeto intermediario (por_estacion)
por_estacion <- group_by(tab, PCvid)
summarise(por_estacion, Ptot = sum(P), Tprom_anual = mean(Tprom))

# Con pipe de la versión de base (para versión de R de 4.1 para adelante)
group_by(tab, PCvid) |> summarise(Ptot = sum(P), Tprom_anual = mean(Tprom))

# Con pipe de magrittr
group_by(tab, PCvid) %>% summarise(Ptot = sum(P), Tprom_anual = mean(Tprom))


# Manejo de los objetos
# Enlista los objetos y borra tab
ls()
rm(tab)

# Reciclando el objeto suma
print(suma)
suma <- suma + 1
suma <- sqrt(suma)
print(suma)
