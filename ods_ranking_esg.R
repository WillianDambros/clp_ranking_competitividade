# Downloading archive RANKING ESTADO

endereco <- 
  "https://www.clp.org.br/wp-content/uploads/2023/08/Estados-ESG-e-ODS_2023.xlsx"

arquivo_local <- paste0("C:/cpl/","estado_ranking_esg_ods", ".xlsx")

curl::curl_download(endereco, arquivo_local)

# Transforming Microdata

arquivo_folhas <- readxl::excel_sheets(arquivo_local)

arquivo_folhas <- arquivo_folhas[arquivo_folhas == "Ranking ESG"]

arquivo_vetor <- vector(mode = 'list', length = (length(arquivo_folhas)))

# getting variables names

arquivo_variaveis <- readxl::read_excel(arquivo_local, arquivo_folhas,
                                        col_names = F)

arquivo_variaveis <- arquivo_variaveis[1,]

# store properly the values

arquivo_variaveis_vetor <- vector(length = ncol(arquivo_variaveis))

for(i in seq_along(arquivo_variaveis)){
  arquivo_variaveis_vetor[i] <- as.character(arquivo_variaveis[[i]])
}

# renaming data

arquivo <- readxl::read_excel(arquivo_local, arquivo_folhas, col_names = F,
                              col_types = "text")

arquivo <- arquivo |> dplyr::rename_with(~arquivo_variaveis_vetor,
                                         .cols = 1:ncol(arquivo))

# removing some lines

arquivo <- arquivo |> 
  dplyr::filter(!stringr::str_detect(ESTADO,"ESTADO|Máximo|Mínimo"))


arquivo |> names()

# removing total columns

arquivo <- arquivo |> dplyr::select(!`Delta de posição ESG`) |> 
  dplyr::select(matches("ESTADO"), matches("Ambiental|Social|Governança|Delta"))


arquivo |> names()
arquivo_ambiental

#
arquivo_ambiental <- 
  arquivo |> dplyr::select("ESTADO", tidyselect::matches("Ambiental|Delta de posição E")) |>
  tidyr::pivot_longer(tidyselect::matches("Ambiental"),
                      names_to = "Ambiental_nomes",
                      values_to = "Ambiental_Valor")

arquivo_social <- 
  arquivo |> dplyr::select("ESTADO", tidyselect::matches("Social")) |>
  tidyr::pivot_longer(tidyselect::matches("Social|Delta"),
                      names_to = "Social_nomes",
                      values_to = "Social_Valor")


arquivo_social



arquivo_social <-
  arquivo |> dplyr::select("ESTADO", tidyselect::matches("Social")) |>
  tidyr:pivot_longer(tidyselect::matches("Social"))


arquivo_ambiental




?tidyr::pivot_longer
??macthes
tidyselect::ma
# Preparing to retrieve the ODS column

arquivo_ods <- arquivo |>
  dplyr::filter(!stringr::str_detect(
    `ESTADO`,
    "Ranking|ESTADO|Máximo|Mínimo")) |>
  dplyr::select(matches("ESTADO|Região"),
                dplyr::starts_with("ODS") & !matches("normalizado")) |>
  tidyr::pivot_longer(dplyr::starts_with("ODS") & !matches("normalizado"),
                      names_to = "ods", values_to = "ods_value") |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))
