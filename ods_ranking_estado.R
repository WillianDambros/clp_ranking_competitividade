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

       # Preparing to retrieve the normalizado column

arquivo_ods_normalizado <- arquivo |>
  dplyr::filter(!stringr::str_detect(
    `ESTADO`,
    "Ranking|ESTADO|Máximo|Mínimo")) |>
  dplyr::select(matches("ESTADO|Região"),matches("normalizado")) |>
  tidyr::pivot_longer(matches("normalizado"),
                      names_to = "ods_normalizado",
                      values_to = "ods_normalizado_value") |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))


             # Preparing to retrieve the ranking columns both years

                   # necessary conditions to extract the correct year update

           # the actual year require this condition to return or not any column
imediato_atual_condicao <- if(arquivo |> dplyr::select(
  tidyselect::ends_with(as.character(lubridate::year(lubridate::today())))) |>
  ncol() != 0){"ESTADO|Região"} else {"nao_ha_valor"}

imediato_atual <-
  arquivo |> dplyr::select(matches(imediato_atual_condicao),
                          tidyselect::ends_with(
                            as.character(
                              lubridate::year(lubridate::today())))) |>
  dplyr::select(
    !tidyselect::contains(paste0("Ranking ODS ",
                                 lubridate::year(lubridate::today()))))
  

passado <-
  arquivo |> dplyr::select(matches("ESTADO|Região"),
                           tidyselect::ends_with(
                             as.character(
                               lubridate::year(lubridate::today())-1))) |>
  dplyr::select(!contains(paste0("Ranking ODS ",
                                 lubridate::year(lubridate::today())-1)))

retrasado <- 
  arquivo |> dplyr::select(matches("ESTADO|Região"),
                           tidyselect::ends_with(
                             as.character(
                               lubridate::year(lubridate::today())-2))) |>
  dplyr::select(!contains(paste0("Ranking ODS ",
                                 lubridate::year(lubridate::today())-2)))

                   # current year necessary to rename the columns

arquivo_ranking_atual_ano <- if(ncol(imediato_atual)!=0){ 
  as.character(lubridate::year(lubridate::today()))} else {
    as.character(lubridate::year(lubridate::today())-1)}

                   # retrieving the current year ranking

arquivo_ranking_atual <-
  (if(ncol(imediato_atual) != 0) {imediato_atual} else {passado}) |>
  dplyr::filter(!stringr::str_detect(`ESTADO`,
                                     "Ranking|ESTADO|Máximo|Mínimo")) |>
  tidyr::pivot_longer(matches("Ranking"),
                      names_to = paste0("ranking_",arquivo_ranking_atual_ano),
                      values_to = 
                        paste0(
                          "ranking_", arquivo_ranking_atual_ano,"_value")) |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))


                     # past year necessary to rename the columns

arquivo_ranking_passado_ano <- if(ncol(imediato_atual)!=0){ 
  as.character(lubridate::year(lubridate::today())-1)} else {
    as.character(lubridate::year(lubridate::today())-2)}


                      # retrieving the last year ranking

arquivo_ranking_passado <-
  (if(ncol(imediato_atual) != 0) {passado} else {retrasado}) |>
  dplyr::filter(!stringr::str_detect(`ESTADO`,
                                     "Ranking|ESTADO|Máximo|Mínimo")) |>
  tidyr::pivot_longer(matches("Ranking"),
                      names_to = paste0("ranking_",
                                        arquivo_ranking_passado_ano),
                      values_to = paste0(
                        "ranking_",arquivo_ranking_passado_ano,"_value")) |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))


                      # Preparing to retrieve the delta column 

arquivo_ods_delta_posicao <- arquivo |>
  dplyr::filter(!stringr::str_detect(
    `ESTADO`,
    "Ranking|ESTADO|Máximo|Mínimo")) |>
  dplyr::select(matches("ESTADO|Região"),matches("Delta")) |>
  dplyr::select(!contains("Delta de posição ODS")) |>
  tidyr::pivot_longer(matches("Delta"),
                      names_to = "ods_delta_posicao",
                      values_to = "ods_delta_posicao_value") |>
  dplyr::mutate(across(dplyr::matches("Nota|value"), as.numeric))


# bind all the parts

arquivo_juntado <- arquivo_ods |>
  dplyr::bind_cols(dplyr::select(arquivo_ods_normalizado, matches("value"))) |>
  dplyr::bind_cols(dplyr::select(arquivo_ranking_passado, matches("value"))) |>
  dplyr::bind_cols(dplyr::select(arquivo_ranking_atual, matches("value"))) |>
  dplyr::bind_cols(dplyr::select(arquivo_ods_delta_posicao, matches("value")))

# adding order and descriptive columns

arquivo_juntado <- arquivo_juntado |>
  dplyr::mutate(ods_ordem =
                  dplyr::case_when(
                    ods == "ODS 1" ~ 1,
                    ods == "ODS 2" ~ 2,
                    ods == "ODS 3" ~ 3,
                    ods == "ODS 4" ~ 4,
                    ods == "ODS 5" ~ 5,
                    ods == "ODS 6" ~ 6,
                    ods == "ODS 7" ~ 7,
                    ods == "ODS 8" ~ 8,
                    ods == "ODS 9" ~ 9,
                    ods == "ODS 10" ~ 10,
                    ods == "ODS 11" ~ 11,
                    ods == "ODS 12" ~ 12,
                    ods == "ODS 13" ~ 13,
                    ods == "ODS 14" ~ 14,
                    ods == "ODS 15" ~ 15,
                    ods == "ODS 16" ~ 16,
                    ods == "ODS 17" ~ 17,
                  ), .keep = "all")


arquivo_juntado <- arquivo_juntado |>
  dplyr::mutate(ods_descricao =
                  dplyr::case_when(
                    ods == "ODS 1" ~ "Erradicação da pobreza",
                    ods == "ODS 2" ~ "Fome Zero e Agricultura Sustentável",
                    ods == "ODS 3" ~ "Saúde e Bem-Estar",
                    ods == "ODS 4" ~ "Educação de Qualidade",
                    ods == "ODS 5" ~ "Igualdade de Gênero",
                    ods == "ODS 6" ~ "Água Potável e Saneamento",
                    ods == "ODS 7" ~ "Energia Limpa e Acessível",
                    ods == "ODS 8" ~ "Trabalho Decente e Crescimento Econômico",
                    ods == "ODS 9" ~ "Indústria, Inovação e Infraestrutura",
                    ods == "ODS 10" ~ "Redução das Desigualdades",
                    ods == "ODS 11" ~ "Cidades e Comunidades Sustentáveis",
                    ods == "ODS 12" ~ "Consumo e Produção Responsáveis",
                    ods == "ODS 13" ~ "Ação Contra a Mudança Global do Clima",
                    ods == "ODS 14" ~ "Vida na Água",
                    ods == "ODS 15" ~ "Vida Terrestre",
                    ods == "ODS 16" ~ "Paz, Justiça e Instituições Eficazes",
                    ods == "ODS 17" ~ "Parcerias e Meios de Implementação",
                  ), .keep = "all")


arquivo_juntado
# Writing

nome_arquivo_csv <- "ranking_sustentabilidade_estados_ods_mediageralponderada"

caminho_arquivo <- paste0(getwd(), "/", nome_arquivo_csv, ".txt")

readr::write_csv2(arquivo_juntado,
                 caminho_arquivo)
