install.packages("dplyr")  
install.packages("readxl")
install.packages("openxlsx")

library(dplyr) 
library(openxlsx)
library(readxl)

df = read.csv("games.csv")

head(df)

#1 Crear vectores con los títulos de la dataset

nombres_columnas = c(names(df))
print(nombres_columnas)

#2 Crear vector numérico con el precio final de los videojuegos

vector_precio_final =  as.vector(unlist(df["price_final"]))
print(vector_precio_final)

#3 Crear una condición lógica de precios bajos (ejemplo precios menores a 10.99)

precios_menor = vector_precio_final[vector_precio_final < 10.99]
print(precios_menor)

#4 Sumar 5 al vector creado

vector_precio_final = vector_precio_final + 5
print(vector_precio_final)

#5 Dividir la puntuación entre 2

vector_precio_final = vector_precio_final / 2
print(vector_precio_final)

#6 Calcular la media, moda, max, min de los datos de tipo numérico (Verficar con
# la función Class)

moda = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

vector_precio_final2 = as.vector(unlist(df["price_final"]))

class(vector_precio_final2)

media = mean(vector_precio_final2)
print(media)

# Calculamos la moda
moda = moda(vector_precio_final2)
print(moda)

# Calculamos el máximo
maximo = max(vector_precio_final2)
print(maximo)

# Calculamos el mínimo
minimo = min(vector_precio_final2)
print(minimo)



#7 Crear un dataFrame de 13 col con la base de datos y guardar en una nueva
#variable
df2 = data.frame(df)
df2 = subset(df, select=c("app_id","title", "date_release", "win", "mac", "linux", "rating","positive_ratio","user_reviews","price_final","price_original","discount","steam_deck"))

#8 Agregar filas y columnas a la matriz (Sugerencia una columna de "1" y una fila
# de datos de un juego de tu preferencia)


df2m = as.matrix(df2)

fila_nueva_1 = c(9999, "Half-Life 3", "3000-11-08", TRUE, TRUE, TRUE, "Overwhelmingly Positive", 96, 68327, 9.99, 9.99, 0, TRUE)
df2m = rbind(df2m, fila_nueva_1)
print(df2m)

columna1_1 = rep(1, nrow(df2m))
df2m = cbind(df2m, columna1_1)
print(df2m)

#9 Agregar columna de "1"

columna1_2 = rep(1, nrow(df2m))
df2m = cbind(df2m, columna1_2)

print(df2m)

#10 Agregar fila con los datos de un video juego de tu preferencia

fila_nueva_2= c(9998, "Half-Life 4", "3000-11-08", TRUE, TRUE, TRUE, "Overwhelmingly Positive", 96, 68327, 9.99, 9.99, 0, TRUE, 1, 1)
df2m = rbind(df2m, fila_nueva_2)
print(df2m)

#11 Eliminar filas y columnas de la matriz

df3m = df2m[, -15]
df3m = df3m[-46070, ]

print(df3m)

#12 Seleccionar los elementos de la matriz

df3m[1:10, ]

#13 Convertir la matriz en data.frame y asignar nombres a las columnas

df3 = as.data.frame(df3m)
names(df3) = c("app_id","title", "date_release", "win", "mac", "linux", "rating","positive_ratio","user_reviews","price_final","price_original","discount","steam_deck","columna1")
print(df3)

#14 Acceder a los datos del dataframe

df3[2, ]

#15  Cambiar nombre de dataframe

names(df3) = "app_id"
print(df3)

#16 Seleccionar un elemento del dataframe

df3[2, 3]


#Parte 2

df = read.csv("games.csv")

write.xlsx(df, "games.xlsx", sheetName = "hoja1")
df = read.xlsx("games.xlsx", sheet = 1)

#1 Importar Datos desde Excel y Ordenar los datos con la función order(), de
#preferencia para la variable Price_final

df =  df[order(df$price_final), ]
print(df)

#2 Mostrar el dataframe ordenado de manera ascendente y descendente

df =  df[order(df$price_final, decreasing = TRUE), ]
print(df)
df =  df[order(df$price_final, decreasing = FALSE), ]
print(df)

#3 Calcular el resumen estadístico de los datos con la función que corresponde

summary(df)

#4 Realizar las graficas

hist(df$price_final)
plot(df$price_final, df$positive_ratio, xlab = "price_final", ylab = "positive_ratio")
barplot(df$price_final, names.arg = df$positive_ratio, xlab = "price_final", ylab = "positive_ratio")
boxplot(df$price_final, horizontal = TRUE, main = "price_final")

#Parte3

#1 Implementar una función para la multiplicación de dos vectores(xy) y probar
#con valores

multiplicar_vectores = function(x, y) {
  resultado = sum(x * y)
  return(resultado)
}

multiplicar_vectores(10,10)

#2 Implementar una función que muestre el resultado de la ecuación de Bhaskara y
# probar con valores.


bhaskara = function(a, b, c) {
  discriminante = b^2 - 4*a*c
  if (discriminante < 0) {
    print("La ecuación no tiene solución en los números reales.")
  } else {
    raiz_discriminante = sqrt(discriminante)
    x1 = (-b + raiz_discriminante) / (2*a)
    x2 = (-b - raiz_discriminante) / (2*a)
    print(paste0("Las raíces de la ecuación son x1 = ", x1, " y x2 = ", x2))
  }
}

bhaskara(1, 5, 6)

#3 Se quiere conocer la media muestral de n observaciones obtenidas 
# independientemente de una distribución normal con media = 0 y varianza =1.


set.seed(123) 
n  = 1000
muestras = rnorm(n, mean = 0, sd = 1)

print(muestras)
print(mean(muestras))

n_repeticiones = 5
n = 100

muestras <- replicate(n_repeticiones, sample(muestras, n, replace = FALSE))

print(muestras)

#3.1 Realizar una simulación, luego calcular las estadísticas descriptivas
#aplicando la función que corresponde y graficar

summary(muestras)

boxplot(muestras[,1], horizontal = TRUE, main = "V1")
boxplot(muestras[,2], horizontal = TRUE, main = "V2")
boxplot(muestras[,3], horizontal = TRUE, main = "V3")
boxplot(muestras[,4], horizontal = TRUE, main = "V4")


densidad <- density(muestras[,1])
plot(densidad, main = "Gráfico de Densidad", xlab = "Valor", ylab = "Densidad")
densidad <- density(muestras[,2])
plot(densidad, main = "Gráfico de Densidad", xlab = "Valor", ylab = "Densidad")
densidad <- density(muestras[,3])
plot(densidad, main = "Gráfico de Densidad", xlab = "Valor", ylab = "Densidad")
densidad <- density(muestras[,4])
plot(densidad, main = "Gráfico de Densidad", xlab = "Valor", ylab = "Densidad")

plot(muestras[,1],1:100)
plot(muestras[,2],1:100)
plot(muestras[,3],1:100)
plot(muestras[,4],1:100)
