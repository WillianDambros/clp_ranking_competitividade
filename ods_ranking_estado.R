# Downloading archive RANKING ESTADO

endereco <- 
  "https://www.clp.org.br/wp-content/uploads/2023/08/Estados-ESG-e-ODS_2023.xlsx"

arquivo_local <- paste0("C:/cpl/","estado-esg-ods_icms_cnae", ".xlsx")

curl::curl_download(endereco, arquivo_local)

# Transforming Microdata

arquivo_folhas <- readxl::excel_sheets(arquivo_local)

arquivo_folhas <- arquivo_folhas[arquivo_folhas == "Ranking ODS"]

arquivo_vetor <- vector(mode = 'list', length = (length(arquivo_folhas)))

   # getting variables names

arquivo_variaveis <- readxl::read_excel(arquivo_local, arquivo_folhas,
                                        col_names = F)

arquivo_variaveis <- arquivo_variaveis[2,]
    
   # store properly the values

arquivo_variaveis_vetor <- vector(length = ncol(arquivo_variaveis))

for(i in seq_along(arquivo_variaveis)){
  arquivo_variaveis_vetor[i] <- as.character(arquivo_variaveis[[i]])
}

   # reading data

arquivo <- readxl::read_excel(arquivo_local, arquivo_folhas, col_names = F,
                              col_types = "text")

arquivo <- arquivo |> dplyr::rename_with(~arquivo_variaveis_vetor,
                                         .cols = 1:ncol(arquivo))

#arquivo <- arquivo |>
#  dplyr::filter(!stringr::str_detect(
#    `ESTADO`,
#    "Ranking|ESTADO|Máximo|Mínimo")) |>
#  #dplyr::select(!`TOTAL ANO`) |>
#  tidyr::pivot_longer(dplyr::starts_with("ODS") & !matches("normalizado"),
#                      names_to = "ods", values_to = "ods_value") |>
#  tidyr::pivot_longer(matches("normalizado"), names_to = "normalizado_ods",
#                      values_to = "normalizado_ods_value") |>
#  tidyr::pivot_longer(matches("Ranking"), names_to = "ranking_ods",
#                      values_to = "ranking_ods_value") |>
#  tidyr::pivot_longer(matches("Delta"), names_to = "delta_ods",
#                      values_to = "delta_ods_value") |> 
#  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))
# Opção 2

arquivo_ods <- arquivo |>
  dplyr::filter(!stringr::str_detect(
    `ESTADO`,
    "Ranking|ESTADO|Máximo|Mínimo")) |>
  dplyr::select(matches("ESTADO|Região"),
                dplyr::starts_with("ODS") & !matches("normalizado")) |>
  tidyr::pivot_longer(dplyr::starts_with("ODS") & !matches("normalizado"),
                      names_to = "ods", values_to = "ods_value") |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))

arquivo_ods

arquivo_ods_normalizado <- arquivo |>
  dplyr::filter(!stringr::str_detect(
    `ESTADO`,
    "Ranking|ESTADO|Máximo|Mínimo")) |>
  dplyr::select(matches("ESTADO|Região"),matches("normalizado")) |>
  tidyr::pivot_longer(matches("normalizado"),
                      names_to = "ods_normalizado",
                      values_to = "ods_normalizado_value") |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))

arquivo_ods_normalizado

#arquivo_ranking <- arquivo |>
#  dplyr::filter(!stringr::str_detect(
#    `ESTADO`,
#    "Ranking|ESTADO|Máximo|Mínimo")) |>
#  dplyr::select(matches("ESTADO|Região"),
#                matches(paste0("Ranking ",
#                               lubridate::year(lubridate::today())))) |>
#  tidyr::pivot_longer(matches("Ranking"),
#                      names_to = "ranking",
#                      values_to = "ranking_value") |>
#  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))

arquivo_ranking

imediato_atual_condicao <- if(arquivo |> dplyr::select(
  tidyselect::ends_with(as.character(lubridate::year(lubridate::today())))) |>
  ncol() != 0){"ESTADO|Região"} else {"nao_ha_valor"}

imediato_atual <-
  arquivo |> dplyr::select(matches(imediato_atual_condicao),
                          tidyselect::ends_with(
                            as.character(lubridate::year(lubridate::today()))))

passado <-
  arquivo |> dplyr::select(matches("ESTADO|Região"),
                           tidyselect::ends_with(
                             as.character(
                               lubridate::year(lubridate::today())-1)))
retrasado <- 
  arquivo |> dplyr::select(matches("ESTADO|Região"),
                           tidyselect::ends_with(
                             as.character(
                               lubridate::year(lubridate::today())-2)))

arquivo_ranking_atual <-
  (if(nrow(imediato_atual) != 0) {passado} else {imediato_atual}) |>
  dplyr::filter(!stringr::str_detect(`ESTADO`,
                                     "Ranking|ESTADO|Máximo|Mínimo")) |>
  tidyr::pivot_longer(matches("Ranking"),
                      names_to = "ranking_ano_imediato",
                      values_to = "ranking_imediato_value") |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))

arquivo_ranking_atual

imediato_atual |> nrow()
arquivo_ranking_passado <-
  (if(nrow(imediato_atual) != 0) {retrasado} else {passado}) |>
  dplyr::filter(!stringr::str_detect(`ESTADO`,
                                     "Ranking|ESTADO|Máximo|Mínimo")) |>
  tidyr::pivot_longer(matches("Ranking"),
                      names_to = "ranking_ano_imediato",
                      values_to = "ranking_imediato_value") |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))

arquivo_ranking_atual
arquivo_ranking_passado


nrow(imediato_atual) != 0


arquivo_juntado <- arquivo_ods |>
  dplyr::bind_cols(dplyr::select(arquivo_ods_normalizado, matches("value"))) |>
  dplyr::bind_cols(dplyr::select(arquivo_ranking_passado, matches("value"))) |>
  dplyr::bind_cols(dplyr::select(arquivo_ranking_atual, matches("value")))

arquivo_juntado <- 
  dplyr::bind_cols(arquivo_ods,
                   dplyr::select(arquivo_ranking, matches("value")))

arquivo_juntado
arquivo_ods_normalizado |> dplyr::select(matches("value"))
arquivo  |> dplyr::select(matches("ESTADO|Região"), dplyr::starts_with("ODS") & !matches("normalizado")) |> dplyr::glimpse()
# Writing

nome_arquivo_csv <- "ranking_sustentabilidade_estados_ods_mediaponderada"

caminho_arquivo <- paste0(getwd(), "/", nome_arquivo_csv, ".txt")

readr::write_csv2(arquivo,
                 caminho_arquivo)
