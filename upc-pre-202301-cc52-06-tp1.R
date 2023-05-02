library(tidyverse)#la libreria que necesitamos para las graficas
library(scales)
#CARGAR DATOS#

#cargamos el data set del trabajo
parcial_data<-read.csv("D:/codigos de R studio/hotel_bookings.csv",header=TRUE,sep=",")
#observamos el data set completo en una tabla
View(parcial_data)
#observamos el nombre de las columnas del data set
names(parcial_data)
#observamos la estructura del conjunto de datos y los tipo de datos 
glimpse(parcial_data)

#LIMPIZA DE DATOS#
#observamos cuantas valores vacios estan en cada columna
en_blanco<-function(x){
  sum = 0
  for (i in 1:ncol(x)) {
    cat("En la columna",colnames(x[i]),"total de valores en blanco: ",colSums(x[i]==""),"\n")
  }
}
en_blanco(parcial_data)
#observamos que la columna children tiene valores vacios y observamos cuantas filas son
sum(is.na(parcial_data$children))
#como la cantidad de filas es muy poca definimos a las filas vacias como si no tuvieran hijos
#con el valor 0
parcial_data$children<-ifelse(is.na(parcial_data$children),0,parcial_data$children)
#observamos cuantos valores NULL tiene cada columna
en_NULL<-function(x){
  sum = 0
  for (i in 1:ncol(x)) {
    cat("En la columna",colnames(x[i]),"total de valores en blanco: ",colSums(x[i]=="NULL"),"\n")
  }
}
en_NULL(parcial_data)
#observamos que tanto en country, agent,company hay valores NULL
#como en country la cantidad es muy pequeña se reemplaza por el pais más
#comun o que mas se repite en la base de datos
#primero convertimos de factores a caracteres para mayor facilidad en el uso de los datos

parcial_data$country<-as.character(parcial_data$country)
#luego hallamos el pais que mas se repite en la columna
tabla_frecuencia <- table(parcial_data$country)
valor_mas_comun <- names(tabla_frecuencia)[which.max(tabla_frecuencia)]
#y lo reemplazamos en los valores con NULL
parcial_data$country = ifelse(parcial_data$country=="NULL", valor_mas_comun, parcial_data$country)

#en cuanto a las columnas agent y company al ser demasiados valores con NULL, lo reemplazaramos por 0 ya que los valores de la columna representa los IDs
#y no existe un ID 0, esto hace que no haya ni un problema al reemplazarlos
#primero transformaremos las columnas a caracteres para facilitar el uso de los datos
parcial_data$agent<-as.character(parcial_data$agent)
parcial_data$company<-as.character(parcial_data$company)
#reemplazamos los valores NULL con 0 en ambas columnas
parcial_data$agent = ifelse(parcial_data$agent=="NULL","0", parcial_data$agent)
parcial_data$company = ifelse(parcial_data$company=="NULL","0", parcial_data$company)

#GRAFICOS#

#grafico para poder ver cual es el hotel que recive más reservas
pie(table(parcial_data$hotel),main = "Tipo de Hotel",col=c("red","blue"),legend=c("Resort Hotel","City Hotel"))

#grafico que mide cual tipo de habitacion es la mas reservada en los dos tipos de hoteles
counts1=table(parcial_data$hotel,parcial_data$reserved_room_type)
barplot(counts1,col = c("red","blue"),legend=c("Resort Hotel","City Hotel"),main="Tipo de habitacion más reservada")

#grafico que mide que tipo de cliente cancela 
counts2=table(parcial_data$customer_type,parcial_data$is_canceled)
barplot(counts2,col = c("red","blue","green","yellow"),legend=c("Contact","Group","Transient","Trasient-Party"),names=c("Calcelado","No cancelado"),main="Tipo de cliente que cancela más o menos las reservas ")
