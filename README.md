# Proyecto de Importación de Datos a MySQL usando R

Este proyecto tiene como objetivo automatizar el proceso de importación, limpieza y procesamiento de datos desde un archivo CSV a una base de datos MySQL utilizando R. El proyecto fue desarrollado como parte de la cátedra final del curso Programación en R, conducente al grado de Magister en Data Science.

## Funcionalidades

- Lectura de datos desde un archivo CSV.
- Normalización de nombres de columnas y valores.
- Manejo de valores faltantes (NA).
- Eliminación de filas duplicadas.
- Guardado de datos en una tabla MySQL.
- Generación de un resumen de estadísticas.
- Manejo de errores.
- Pruebas unitarias exhaustivas con librería TESTHAT.

Este es un video explicativo de mi proyecto:

[![Catedra Final Programacion R - UDLA](https://img.youtube.com/vi/hrbf_opnc1Q/maxresdefault.jpg)](https://www.youtube.com/watch?v=hrbf_opnc1Q)

En este video, cubro los siguientes puntos:
1. Introducción al proyecto.
2. Explicación del código.
3. Resultados y conclusiones.

## Uso

### Requisitos

- R
- Paquetes: `DBI`, `RMySQL`, `dplyr`, `broom`, `janitor`, `rio`, `stringi`, `testthat`

### Ejecución del Código

```r
# Funciones Jorge Melendez
source("fx_dataset_to_mysql.R")

# Librerías necesarias
library(DBI)       # Para conectar y ejecutar consultas en bases de datos
library(RMySQL)    # Para conectar y manejar bases de datos MySQL

# Conectar a la base de datos MySQL
con <- DBI::dbConnect(RMySQL::MySQL(),
                      dbname = "database name",
                      host = "localhost",
                      port = 3306,
                      user = "user",
                      password = "pass")

# Ejecutar la importación
result <- importar_a_mysql(file_path = "Most Streamed Spotify Songs 2024.csv",
                           table_name = "spotify",
                           db_con = con,
                           metodo_na = "media",
                           encoding = "UTF-8")

# Desconectar de MySQL
dbDisconnect(con)

# Imprimir el resumen de estadísticas
print(result$summary_stats)
