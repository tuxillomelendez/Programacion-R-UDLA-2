#====================================================================================================================================
# Autor:       Jorge Melendez Bastias.
# Motivo:      Para la cátedra final del curso Programación en R conducente a grado de Magíster en Data Science.
# Fecha:       10-07-2024
# Descripción: Este script contiene funciones para importar, limpiar, procesar y guardar datos en una base de datos MySQL.
#              Las funciones se encargan de leer un dataset, normalizar nombres de columnas, manejar valores NA, eliminar duplicados,
#              y guardar los datos en una tabla MySQL, además de generar un resumen de estadísticas.
#
#              Este código es libre para su uso por cualquier persona.
#====================================================================================================================================

# Librerías necesarias
library(DBI)       # Para conectar y ejecutar consultas en bases de datos
library(RMySQL)    # Para conectar y manejar bases de datos MySQL
library(dplyr)     # Para manipulación de datos
library(ggplot2)   # Para visualización de datos
library(broom)     # Para convertir modelos en dataframes ordenados
library(janitor)   # Para limpiar y normalizar nombres de columnas
library(rio)       # Para importar y exportar datos en diversos formatos
library(stringi)   # Para manipulación y limpieza de cadenas de texto

#' Escribir en el log de errores
#'
#' Registra un mensaje de error con un timestamp en el archivo 'error_log.txt'.
#'
#' @param mensaje Un string con el mensaje de error a registrar.
escribir_log <- function(mensaje) {
  log_file <- "error_log.txt"
  timestamp <- Sys.time()
  write(paste(timestamp, "-", mensaje), log_file, append = TRUE)
}

#' Leer dataset
#'
#' Importa datos desde un archivo especificado y maneja cualquier error que ocurra.
#'
#' @param file_path Ruta al archivo de datos.
#' @param encoding Codificación del archivo de datos (por defecto "UTF-8").
#' @return Un dataframe con los datos importados.
leer_data_set <- function(file_path,
                          encoding = "UTF-8") {
  tryCatch({
    data <- rio::import(file_path, encoding = encoding)
    return(data)
  }, error = function(e) {
    escribir_log(paste("Error en leer_data_set:", e$message))
    stop("Error en leer_data_set. Revisa el log para más detalles.")
  })
}

#' Normalizar nombres de columnas
#'
#' Limpia y normaliza los nombres de las columnas del dataframe.
#'
#' @param data El dataframe cuyas columnas se normalizarán.
#' @return El dataframe con los nombres de las columnas normalizados.
normalizar_nombres_columnas <- function(data) {
  tryCatch({
    data <- clean_names(data)
    return(data)
  }, error = function(e) {
    escribir_log(paste("Error en normalizar_nombres_columnas:", e$message))
    stop("Error en normalizar_nombres_columnas. Revisa el log para más detalles.")
  })
}

#' Normalizar columnas
#'
#' Normaliza las columnas numéricas del dataframe.
#'
#' @param data El dataframe cuyas columnas se normalizarán.
#' @return El dataframe con las columnas normalizadas.
normalizar_columnas <- function(data) {
  tryCatch({
    num_cols <- sapply(data, is.numeric)
    data[num_cols] <- scale(data[num_cols])
    return(data)
  }, error = function(e) {
    escribir_log(paste("Error en normalizar_columnas:", e$message))
    stop("Error en normalizar_columnas. Revisa el log para más detalles.")
  })
}

#' Manejar valores NA
#'
#' Maneja los valores NA en el dataframe, eliminándolos o reemplazándolos por la media de la columna.
#'
#' @param data El dataframe en el cual se manejarán los valores NA.
#' @param metodo Método para manejar los NA ("eliminar" o "media", por defecto "eliminar").
#' @return El dataframe con los valores NA manejados.
manejar_na <- function(data,
                       metodo = "eliminar") {
  tryCatch({
    if (metodo == "eliminar") {
      data <- na.omit(data)
    } else if (metodo == "media") {
      for (col in names(data)) {
        if (is.numeric(data[[col]])) {
          data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
        }
      }
    }
    return(data)
  }, error = function(e) {
    escribir_log(paste("Error en manejar_na:", e$message))
    stop("Error en manejar_na. Revisa el log para más detalles.")
  })
}

#' Eliminar filas duplicadas
#'
#' Elimina las filas duplicadas del dataframe.
#'
#' @param data El dataframe del cual se eliminarán las filas duplicadas.
#' @return El dataframe sin filas duplicadas.
eliminar_filas_duplicadas <- function(data) {
  tryCatch({
    data <- data[!duplicated(data), ]
    return(data)
  }, error = function(e) {
    escribir_log(paste("Error en eliminar_filas_duplicadas:", e$message))
    stop("Error en eliminar_filas_duplicadas. Revisa el log para más detalles.")
  })
}

#' Guardar dataset en MySQL
#'
#' Guarda el dataframe en una tabla de MySQL.
#'
#' @param data El dataframe a guardar.
#' @param table_name Nombre de la tabla en MySQL donde se guardará el dataframe.
#' @param db_con Conexión a la base de datos MySQL.
guardar_en_mysql <- function(data,
                             table_name,
                             db_con) {
  tryCatch({
    dbWriteTable(db_con, table_name, data, overwrite = TRUE, append = FALSE, row.names = FALSE)
  }, error = function(e) {
    escribir_log(paste("Error en guardar_en_mysql:", e$message))
    stop("Error en guardar_en_mysql. Revisa el log para más detalles.")
  })
}

#' Crear resumen de estadísticas
#'
#' Crea un resumen de estadísticas del dataframe.
#'
#' @param data El dataframe del cual se creará el resumen de estadísticas.
#' @return Una lista con el resumen de estadísticas del dataframe.
crear_resumen_estadisticas <- function(data) {
  resumen <- list(
    columnas = ncol(data),
    filas = nrow(data),
    resumen = summary(data)
  )
  return(resumen)
}

#' Importar datos a MySQL
#'
#' Realiza todas las etapas necesarias para importar un archivo a MySQL.
#'
#' @param file_path Ruta al archivo de datos.
#' @param table_name Nombre de la tabla en MySQL donde se guardará el dataframe.
#' @param db_con Conexión a la base de datos MySQL.
#' @param metodo_na Método para manejar los NA ("eliminar" o "media", por defecto "eliminar").
#' @param encoding Codificación del archivo de datos (por defecto "UTF-8").
#' @param limpiar_caracteres Booleano que indica si se deben limpiar caracteres especiales (por defecto TRUE).
#' @return Un objeto de clase DataResult con los resultados del análisis.
importar_a_mysql <- function(file_path,
                             table_name,
                             db_con,
                             metodo_na = "eliminar",
                             encoding = "UTF-8",
                             limpiar_caracteres = TRUE) {
  tryCatch({
    # Limpiar caracteres especiales si se especifica
    if (limpiar_caracteres) {
      file_path_limpio <- tempfile(fileext = ".csv")
      lines <- readLines(file_path, encoding = encoding)
      clean_lines <- stri_replace_all_regex(lines, "[^\x20-\x7E]", "")
      writeLines(clean_lines, file_path_limpio, useBytes = TRUE)
      file_path <- file_path_limpio
    }

    # Leer el dataset
    data <- leer_data_set(file_path, encoding = encoding)

    # Normalizar los nombres de las columnas
    data <- normalizar_nombres_columnas(data)

    # Normalizar las columnas
    data <- normalizar_columnas(data)

    # Manejar valores NA
    data <- manejar_na(data, metodo = metodo_na)

    # Eliminar filas duplicadas
    data <- eliminar_filas_duplicadas(data)

    # Guardar en MySQL
    guardar_en_mysql(data, table_name, db_con)

    # Crear resumen de estadísticas
    resumen_estadisticas <- crear_resumen_estadisticas(data)

    # Crear y retornar el objeto de clase S3
    result <- structure(list(data = data, summary_stats = resumen_estadisticas), class = "DataResult")
    return(result)
  }, error = function(e) {
    escribir_log(paste("Error en importar_a_mysql:", e$message))
    stop("Ocurrió un error durante la importación. Revisa el log para más detalles.")
  })
}

# Definir la clase DataResult
DataResult <- function(data, summary_stats) {
  structure(list(data = data, summary_stats = summary_stats), class = "DataResult")
}
