# Downloading archive RANKING ESTADO

endereco <- 
  "https://www.clp.org.br/wp-content/uploads/2023/08/Estados-ESG-e-ODS_2023.xlsx"

arquivo_local <- paste0("C:/cpl/","estados_ranking_esg_ods", ".xlsx")

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

# removing total columns
    
arquivo <- arquivo |> dplyr::select(!`Delta de posição ESG`) |>   
  dplyr::select(matches("ESTADO"), matches("Ambiental|Social|Governança|Delta"))

    
        # start pivot longer columns 

#

arquivo_esg <- arquivo |> dplyr::select("ESTADO", "Ambiental",
                                        "Social", "Governança") |>
  tidyr::pivot_longer(c("Ambiental", "Social", "Governança"),
                      names_to = "esg",
                      values_to = "esg_valor")

arquivo_normalizado <- arquivo |> dplyr::select("ESTADO",
                                                matches("normalizado")) |>
  tidyr::pivot_longer(matches("normalizado"),
                      names_to = "esg_normalizado",
                      values_to = "esg_normalizado_valor")

# necessary conditions to distinguished the ranking years columns

# the actual year require this condition to return or not any column
imediato_atual_condicao <- if(arquivo |> dplyr::select(
  tidyselect::ends_with(as.character(lubridate::year(lubridate::today())))) |>
  ncol() != 0){"ESTADO|Região"} else {"nao_ha_valor"}

# conditions that will go into formulas (multiple phases conditions)
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

# current year necessary to rename the columns (phase conditional)

arquivo_ranking_atual_ano <- if(ncol(imediato_atual)!=0){ 
  as.character(lubridate::year(lubridate::today()))} else {
    as.character(lubridate::year(lubridate::today())-1)}

# retrieving the current year ranking

arquivo_ranking_atual <-
  (if(ncol(imediato_atual) != 0) {imediato_atual} else {passado}) |>
  tidyr::pivot_longer(matches("Ranking"),
                      names_to = paste0("ranking_",arquivo_ranking_atual_ano),
                      values_to = 
                        paste0(
                          "ranking_", arquivo_ranking_atual_ano,"_valor"))

# past year necessary to rename the columns (phase conditional)

arquivo_ranking_passado_ano <- if(ncol(imediato_atual)!=0){ 
  as.character(lubridate::year(lubridate::today())-1)} else {
    as.character(lubridate::year(lubridate::today())-2)}


# retrieving the last year ranking

arquivo_ranking_passado <-
  (if(ncol(imediato_atual) != 0) {passado} else {retrasado}) |>
  tidyr::pivot_longer(matches("Ranking"),
                      names_to = paste0("ranking_",
                                        arquivo_ranking_passado_ano),
                      values_to = paste0(
                        "ranking_",arquivo_ranking_passado_ano,"_valor"))
# continuing to pivot_longer the rest of content

arquivo_delta <- arquivo |> dplyr::select("ESTADO",
                                                matches("Delta")) |>
  tidyr::pivot_longer(matches("Delta"),
                      names_to = "Delta",
                      values_to = "Delta_valor")

# bind all the parts

arquivo_juntado <- arquivo_esg |>
  dplyr::bind_cols(dplyr::select(arquivo_normalizado, matches("valor"))) |>
  dplyr::bind_cols(dplyr::select(arquivo_ranking_passado, matches("valor"))) |>
  dplyr::bind_cols(dplyr::select(arquivo_ranking_atual, matches("valor"))) |>
  dplyr::bind_cols(dplyr::select(arquivo_delta, matches("valor")))


arquivo_juntado |>  dplyr::mutate(across(dplyr::matches("valor"), as.numeric))

# Writing

nome_arquivo_csv <- "estados_ranking_esg"

caminho_arquivo <- paste0(getwd(), "/", nome_arquivo_csv, ".txt")

readr::write_csv2(arquivo_juntado,
                  caminho_arquivo)
