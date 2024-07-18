#====================================================================================================================================
# Autor:       Jorge Melendez Bastias.
# Motivo:      Para la cátedra final del curso Programación en R conducente a grado de Magíster en Data Science.
# Fecha:       10-07-2024
# Descripción: Este script se encarga de conectar a una base de datos MySQL, ejecutar la función
#              importar_a_mysql para importar y procesar un dataset, y finalmente desconectar de la base de datos.
#
#              Este código es libre para su uso por cualquier persona.
#====================================================================================================================================

# -------------------------------------------------------
# Importante habilitar en mysql:
#
# -- Habilitar local_infile globalmente
# SET GLOBAL local_infile = 1;
#
# -- Verificar que la configuración se ha aplicado
# SHOW VARIABLES LIKE 'local_infile';
# -------------------------------------------------------

setwd("C:/Users/jorge/OneDrive/MAGISTER/01/PROGRAMACION EN R/EVALUACIONES/02 - FUNCION_MEDIA")

# Funciones Jorge Melendez
source("fx_dataset_to_mysql.R")

# Librerías necesarias
library(DBI)       # Para conectar y ejecutar consultas en bases de datos
library(RMySQL)    # Para conectar y manejar bases de datos MySQL

# Conectar a MySQL
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "test_function",
                 host = "localhost",
                 port = 3306,
                 user = "usuario_R",
                 password = "ConectaR")

# Ejecutar la importación
# Parámetros:
# - file_path: Ruta al archivo de datos.
# - table_name: Nombre de la tabla en MySQL donde se guardará el dataframe.
# - db_con: Conexión a la base de datos MySQL.
# - metodo_na: Método para manejar los NA ("eliminar" o "media", por defecto "eliminar").
# - encoding: Codificación del archivo de datos (por defecto "UTF-8").
result <- importar_a_mysql(file_path = "Most Streamed Spotify Songs 2024.csv",
                           table_name = "spotify_5",
                           db_con = con,
                           metodo_na = "media",
                           encoding = "UTF-8")

# Desconectar de MySQL
dbDisconnect(con)

# Imprimir el resumen de estadísticas
# Muestra el resumen de estadísticas generado por la función importar_a_mysql
print(result$summary_stats)
