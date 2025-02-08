


library(tidyverse)
library(rvest)

team_code <- "https://baloncestoenvivo.feb.es/estadisticas/primerafeb/1/2024" %>%
  read_html() %>%
  html_elements("td.nombre.equipo a") %>%
  html_attr("href") %>%
  str_extract(.,"[0-9]+")

foto_df <- function(team_code){

url <- paste0("https://baloncestoenvivo.feb.es/equipo/",team_code) %>%
  read_html()

df <- tibble(
  foto =url%>%
  html_elements("a.table-data-foto img") %>%
  html_attr("src")) %>%
  mutate(idPlayer = str_extract(foto, "[0-9]+"))

return(df)
}


dfFotos <- map_df(team_code, foto_df)



descargar_fotos <- function(dfFotos, carpeta) {
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)  # Crea la carpeta si no existe

  walk2(dfFotos$foto, dfFotos$idPlayer, ~ download.file(.x, file.path(carpeta, paste0(.y, ".jpg")), mode = "wb"))

  message("Descarga completada en: ", carpeta)
}

# Ruta de la carpeta donde guardar las imágenes
carpeta_destino <- "C:/Users/iVo/Documents/R/R/RStudio/ACB/2025/primeraFEB/imagenes/imgJugadores"

# Ejecutar la función
descargar_fotos(dfFotos, carpeta_destino)


# database de jugadores ----------------------------------------------------------------------


jugadores_df <- function(team_code){

  url <- paste0("https://baloncestoenvivo.feb.es/equipo/",team_code) %>%
    read_html()

  df <- tibble(
    equipo = url%>%
      html_elements(".wrapper-text span.titulo") %>%
      html_text(),
    foto =url%>%
      html_elements("a.table-data-foto img") %>%
      html_attr("src"),
    nombe = url%>%
      html_elements(".nombre a") %>%
      html_text() %>%
      str_squish() %>%
      str_to_title(),
    dorsal =  url%>%
      html_elements("td.dorsal") %>%
      html_text()) %>%
    mutate(idPlayer = str_extract(foto, "[0-9]+"),
           team_code = team_code)



  return(df)
}


dfjugadores <- map_df(team_code, jugadores_df)




