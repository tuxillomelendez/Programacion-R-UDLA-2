#====================================================================================================================================
# Autor:       Jorge Melendez Bastias.
# Motivo:      Para la cátedra final del curso Programación en R conducente a grado de Magíster en Data Science.
# Fecha:       10-07-2024
# Descripción: Este script contiene pruebas unitarias para la función `importar_a_mysql`.
#              Utiliza el paquete `testthat` para verificar que la función se comporta como se espera.
#              Las pruebas incluyen la validación de la estructura del objeto retornado y la correcta manipulación y almacenamiento
#              de datos en una base de datos MySQL.
# Librería:    testthat, que proporciona un marco para realizar pruebas unitarias y asegurar que las funciones se comporten
#              como se espera.
#
#              Este código es libre para su uso por cualquier persona.
#====================================================================================================================================

# test_importar_a_mysql.R
library(testthat)

# Cargar el archivo de funciones a probar
source("fx_dataset_to_mysql.R")

# Prueba para leer_data_set
test_that("Leer dataset funciona correctamente", {
  data <- leer_data_set("Most Streamed Spotify Songs 2024.csv", encoding = "UTF-8")
  expect_s3_class(data, "data.frame")
  expect_true(ncol(data) > 0)
  expect_true(nrow(data) > 0)
})

# Prueba para normalizar_nombres_columnas
test_that("Normalizar nombres de columnas funciona correctamente", {
  data <- data.frame("Columna 1" = 1:10, "Columna 2" = 11:20)
  data_norm <- normalizar_nombres_columnas(data)
  expect_true(all(colnames(data_norm) == c("columna_1", "columna_2")))
})

# Prueba para manejar_na
test_that("Manejar valores NA funciona correctamente", {
  data <- data.frame("Columna1" = c(1, NA, 3), "Columna2" = c(NA, 2, 3))
  data_na_elim <- manejar_na(data, metodo = "eliminar")
  expect_true(nrow(data_na_elim) == 1)
  data_na_media <- manejar_na(data, metodo = "media")
  expect_true(all(!is.na(data_na_media)))
})

# Prueba para eliminar_filas_duplicadas
test_that("Eliminar filas duplicadas funciona correctamente", {
  data <- data.frame("Columna1" = c(1, 1, 2), "Columna2" = c(3, 3, 4))
  data_sin_dups <- eliminar_filas_duplicadas(data)
  expect_true(nrow(data_sin_dups) == 2)
})

# Prueba para guardar_en_mysql
test_that("Guardar en MySQL funciona correctamente", {
  con <- dbConnect(RMySQL::MySQL(),
                   dbname = "test_function",
                   host = "localhost",
                   port = 3306,
                   user = "usuario_R",
                   password = "ConectaR")
  data <- data.frame("Columna1" = 1:10, "Columna2" = 11:20)
  expect_silent(guardar_en_mysql(data, "test_table", con))
  dbDisconnect(con)
})

# Prueba para importar_a_mysql
test_that("Importar a MySQL funciona correctamente", {
  con <- dbConnect(RMySQL::MySQL(),
                   dbname = "test_function",
                   host = "localhost",
                   port = 3306,
                   user = "usuario_R",
                   password = "ConectaR")
  result <- importar_a_mysql("Most Streamed Spotify Songs 2024.csv", "test_table", con, metodo_na = "media", encoding = "UTF-8")
  expect_s3_class(result, "DataResult")
  expect_true("data" %in% slotNames(result))
  expect_true("summary_stats" %in% slotNames(result))
  dbDisconnect(con)
})
