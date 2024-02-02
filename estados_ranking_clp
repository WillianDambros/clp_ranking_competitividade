# Downloading archive RANKING ESTADO

endereco <- 
  "https://www.clp.org.br/wp-content/uploads/2023/08/Estados-ESG-e-ODS_2023.xlsx"

arquivo_local <- paste0("C:/cpl/","estados_ranking_esg_ods", ".xlsx")

curl::curl_download(endereco, arquivo_local)

# Transforming Microdata

arquivo_folhas <- readxl::excel_sheets(arquivo_local)

arquivo_folhas <- arquivo_folhas[arquivo_folhas == "Base de dados normalizados "]

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


arquivo_variaveis_vetor
# renaming data

arquivo <- readxl::read_excel(arquivo_local, arquivo_folhas, col_names = F,
                              col_types = "text")

arquivo <- arquivo |> dplyr::rename_with(~arquivo_variaveis_vetor,
                                         .cols = 1:ncol(arquivo))

# removing some lines

arquivo <- arquivo |> 
  dplyr::filter(!stringr::str_detect(ESTADO,"ESTADO|Máximo|Mínimo"))

# removing total columns

arquivo <- arquivo |> tidyr::pivot_longer(cols = starts_with("Nota"),
                                          names_to = "nota_ano",
                                          values_to = "nota_valor")

arquivo |> dplyr::mutate(across(dplyr::matches("valor|Delta"), as.numeric))
  

# Writing

nome_arquivo_csv <- "estados_ranking_clp"

caminho_arquivo <- paste0(getwd(), "/", nome_arquivo_csv, ".txt")

readr::write_csv2(arquivo,
                  caminho_arquivo)
