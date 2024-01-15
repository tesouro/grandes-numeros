# API_Grandes_Numeros_STN.R

library(ckanr)
library(readxl)
library(utils)
library(tidyverse)
library(readr)
library(stringr)
#library(rtn)
library(gganimate)
library(RColorBrewer)
library(ggrepel)
library(future)
library(promises)
library(base64enc)

plan(multisession, gc = TRUE)

futures <- list()

my_futures <- list()

data_inicial_graficos <- "2010-01-01"


get_future_result <- function(id) {
  return(future::value(futures[[id]]))
}

encontrar_na_planilha <- function(arquivo, sheetNumber, columnNumber, texto) {
  # Ler a planilha
  planilha <- readxl::read_xlsx(arquivo, sheet = sheetNumber)

  # Pegar a coluna especificada
  coluna_especificada <- planilha[, columnNumber]

  print("coluna_especificada")
  print(coluna_especificada)

  # Iniciar um loop para percorrer todas as células na coluna
  for (i in 1:nrow(coluna_especificada)) {
    # Se for um texto, verificar se inicia com o texto
    if(!is.na(as.character(coluna_especificada[i, 1]))) {
      if (startsWith(as.character(coluna_especificada[i, 1]), texto)) {
        # Se iniciar, retornar o número da linha
        return(i)
      }
    }
    
  }

  # Se o loop terminar sem encontrar uma correspondência, retornar NULL
  return(NULL)
}

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Open Sans", colour = "grey20"),
      axis.text = element_text(family = "Open Sans Condensed Light", face = "plain", colour = "grey20", size = 14),
      title = element_text(family = "Open Sans Condensed", face = "bold", size = 16), # size para o Shiny
      plot.subtitle = element_text(family = "Open Sans Condensed Light", face = "plain", size = 20, color = "#1E4C7A"),
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 18),
      axis.ticks = element_line(size = 0.5),
      axis.ticks.length = unit(.25, "cm"),
      axis.title = element_text(size = 16),
      legend.position = 'bottom')
}


get_12_month_accumulated_account_data_by_month <- function(.data = NULL, month = NULL, match_required = TRUE){

  df_trabalho<- get_full_data()



  if (!is.null(.data) & match_required){

    contas<- .data

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(stringr::str_to_lower(Rubrica) %in% stringr::str_to_lower(contas))
  }

  if (!is.null(.data) & !match_required){

    contas<- stringr::str_trim(stringr::str_replace(.data,"[(](?<=[(]).*", ""))

    account_filter<- stringr::str_to_lower(str_c(contas,  collapse = "|"))

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(Rubrica), pattern = account_filter))
  }


  datas_rubricas<-
    df_trabalho %>%
    dplyr::filter(Data>="1997-12-01" ) %>%
    dplyr::distinct(Data, id, Rubrica) %>%
    dplyr::select(Data, id, Rubrica) %>%
    dplyr::arrange(id,Data)

  saldos_acumu<-
    df_trabalho %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      valor_historico= zoo::rollsum(valor_historico,12,align = "right"),
      valor_atualizado=zoo::rollsum(valor_atualizado,12,align = "right")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-id)


  df_trabalho <-
    datas_rubricas %>%
    dplyr::bind_cols(saldos_acumu) %>%
    dplyr::arrange(Data,id,Rubrica)

  if (!is.null(month)){

    df_trabalho<-
      df_trabalho %>%
      dplyr::filter(lubridate::month(Data) %in% month)
  }
  df_trabalho

}

get_full_data <- function(){


  tb_ckan<- ckanr::resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan")
  URL_add <- tb_ckan$url

  tmp <- tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp, extra = "-R", method = "libcurl")

  rtn_receita<- readxl::read_xlsx(tmp,sheet = 4,skip = 4,n_max = 57)
  rtn_despesa<- readxl::read_xlsx(tmp,sheet = 4,skip = 62,n_max = 92,col_names = FALSE )
  rtn_geral <- readxl::read_xlsx(tmp,sheet = 2,skip = 64,n_max = 8)

  names(rtn_receita)[1]<-"Rubrica"
  names(rtn_despesa)[1]<-"Rubrica"
  names(rtn_geral)[1]<-"Rubrica"

  #plano_contas<- tibble::tibble(Rubrica=c(rtn_receita$Rubrica, rtn_despesa$Rubrica, rtn_geral$Rubrica))


  # plano_contas<-
  #   plano_contas %>%
  #   dplyr::mutate(id=  dplyr::row_number()) %>%
  #   dplyr::select(Rubrica, id)

  linha_IPCA <- encontrar_na_planilha(tmp, 3, 1, "Deflator - IPCA")

  deflator_IPCA <- readxl::read_xlsx(tmp,sheet = 3,skip = linha_IPCA,n_max = 1, col_names = FALSE)
  names(deflator_IPCA)<-names(rtn_receita)
  names(rtn_despesa) <-names(rtn_receita)

  rtn_receita$id <- 1:NROW(rtn_receita)
  series_temporais_analise_rec<-tidyr::gather(rtn_receita,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise_rec$Data<-as.Date(as.numeric(series_temporais_analise_rec$Data), origin="1899-12-30")
  series_temporais_analise_rec$Valor <-as.numeric(series_temporais_analise_rec$Valor)
  series_temporais_analise_rec$Valor[is.na(series_temporais_analise_rec$Valor)]<-0
  series_temporais_analise_rec$tipo <- "R"

  proximo_id <- NROW(rtn_receita) +1
  ultimo_id <- proximo_id + NROW(rtn_despesa) -1


  rtn_despesa$id <- proximo_id:ultimo_id
  series_temporais_analise_desp<-tidyr::gather(rtn_despesa,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise_desp$Data<-as.Date(as.numeric(series_temporais_analise_desp$Data), origin="1899-12-30")
  series_temporais_analise_desp$Valor <-as.numeric(series_temporais_analise_desp$Valor)
  series_temporais_analise_desp$Valor[is.na(series_temporais_analise_desp$Valor)]<-0
  series_temporais_analise_desp$tipo <- "D"


  proximo_id <- ultimo_id +1
  ultimo_id <- proximo_id + NROW(rtn_geral) -1
  rtn_geral$id <- proximo_id:ultimo_id

  names(rtn_geral)<-names(rtn_receita)
  names(rtn_geral)[1]<-"Rubrica"


  series_temporais_analise<-tidyr::gather(rtn_geral,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise$Data<-as.Date(as.numeric(series_temporais_analise$Data), origin="1899-12-30")
  series_temporais_analise$Valor <-as.numeric(series_temporais_analise$Valor)
  series_temporais_analise$Valor[is.na(series_temporais_analise$Valor)]<-0

  names(deflator_IPCA)[1]<-"Rubrica"
  series_temporais_analise_IPCA<-tidyr::gather(deflator_IPCA,Data, Valor,c(-Rubrica))
  series_temporais_analise_IPCA$Data<-as.Date(as.numeric(series_temporais_analise_IPCA$Data), origin="1899-12-30")
  series_temporais_analise_IPCA$Valor <-as.numeric(series_temporais_analise_IPCA$Valor)

  serie_completa<-
    series_temporais_analise_rec %>%
    dplyr::bind_rows(series_temporais_analise_desp,
                     series_temporais_analise)

  serie_completa<-
    serie_completa %>%
    dplyr::mutate(valor_historico = Valor) %>%
    dplyr::inner_join(
      series_temporais_analise_IPCA %>%
        dplyr::mutate(deflator =  Valor) %>%
        dplyr::select(Data, deflator), by = "Data") %>%
    dplyr::mutate(valor_atualizado = deflator * valor_historico ) %>%
    dplyr::arrange(Data, id, Rubrica) %>%
    dplyr::mutate(Rubrica= stringr::str_trim(stringr::str_remove_all(Rubrica, pattern = "[0-9]+/")))%>%
    dplyr::select(Data,Rubrica, id, tipo,valor_historico, valor_atualizado )





  serie_completa
}


#* Retorna o estoque da dívida pública federal
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /estoque_dpf
function(formato = "string"){
  #tb_ckan<-resource_show(id="0402cb77-5e4c-4414-966f-0e87d802a29a",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="0402cb77-5e4c-4414-966f-0e87d802a29a",url="https://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 5, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,NCOL(df_dpf)])
  num <- as.numeric(as.numeric(df_dpf[5,NCOL(df_dpf)]))

  if (formato=="df"){
    data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  } else {
    unidade <-"bi"
    if (num>=1000) {
      unidade <- "tri"
      num<-num/1000
    }
    list("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade, " (",ult_mes,")"))
    #data.frame("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))
    #num<-paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade)
    #return (num)

  }



}


#* Retorna as emissões da DPF no ano
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /emissoes_dpf
function(formato="string"){
  #tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="https://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 5, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,(NCOL(df_dpf)-1)])
  num <- as.numeric(as.numeric(df_dpf[5,NCOL(df_dpf)]))

  if (formato=="df"){
    data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  } else {
    unidade <-"mi"
    if (num>=1000) {
      unidade <- "bi"
      num<-num/1000
    }
    #data.frame("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))
    list("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade, " (",ult_mes,")"))
  }
}


#* Retorna o resgate da dívida pública federal
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /resgate_dpf
function(formato = "string"){
  #tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="https://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 19, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,(NCOL(df_dpf)-1)])
  num <- as.numeric(as.numeric(df_dpf[19,NCOL(df_dpf)]))
  #list( num = num, unidade ="R$ Bi")
  if (formato=="df"){
    data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  } else {
    unidade <-"mi"
    if (num>=1000) {
      unidade <- "bi"
      num<-num/1000
    }
    #data.frame("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))
    list("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade, " (",ult_mes,")"))
  }
}


#* Retorna o resultado primário do governo central
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /resultado_primario
function(formato = "string"){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_rtn<-read_xlsx(tmp,sheet = 2,skip = 2,n_max = 78, col_names = FALSE)
  meses<-as.Date(as.numeric(df_rtn[3,c(2:NCOL(df_rtn))]), origin = "1899-12-30")
  ult_mes <- meses[NROW(meses)]
  pos_ano_corrente <- which(substr(meses,1,4)==substr(ult_mes,1,4))
  pos_ano_corrente <- pos_ano_corrente + 1

  unidade<- as.character(df_rtn[1,1])

  num <- as.numeric(sum(as.numeric(df_rtn[64,pos_ano_corrente])))
  if (formato=="df"){
    data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  } else {
    unidade <-"mi"
    if (abs(num)>=1000) {
      unidade <- "bi"
      num<-num/1000
    }
    #data.frame("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))
    list("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))
  }
}


#* Retorna a receita líquida primária do governo central
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /receita_primaria_liquida
function(formato = "string"){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_rtn<-read_xlsx(tmp,sheet = 2,skip = 2,n_max = 37, col_names = FALSE)
  meses<-as.Date(as.numeric(df_rtn[3,c(2:NCOL(df_rtn))]), origin = "1899-12-30")
  ult_mes <- meses[NROW(meses)]
  pos_ano_corrente <- which(substr(meses,1,4)==substr(ult_mes,1,4))
  pos_ano_corrente <- pos_ano_corrente + 1

  unidade<- as.character(df_rtn[1,1])

  num <- as.numeric(sum(as.numeric(df_rtn[36,pos_ano_corrente])))
  #list( num = num, unidade ="R$ Bi")
  if (formato=="df"){
    data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  } else {
    unidade <-"mi"
    if (num>=10^3 && num <10^6) {
      unidade <- "bi"
      num<-num/10^3
    } else if(num>10^6){
      unidade <- "tri"
      num<-num/10^6

    }
    #data.frame("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))
    list("num"=paste0("R$", format(round(num,2), digits=3, decimal.mark=",")," ", unidade))
  }
}


#* Retorna a despesa primária total do governo central
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /despesa_primaria_total
function(formato = "string"){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_rtn<-read_xlsx(tmp,sheet = 2,skip = 2,n_max = 38, col_names = FALSE)
  meses<-as.Date(as.numeric(df_rtn[3,c(2:NCOL(df_rtn))]), origin = "1899-12-30")
  ult_mes <- meses[NROW(meses)]
  pos_ano_corrente <- which(substr(meses,1,4)==substr(ult_mes,1,4))
  pos_ano_corrente <- pos_ano_corrente + 1

  unidade<- as.character(df_rtn[1,1])

  num <- as.numeric(sum(as.numeric(df_rtn[37,pos_ano_corrente])))
  if (formato=="df"){
    data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  } else {
    unidade <-"mi"
    if (num>=10^3 && num <10^6) {
      unidade <- "bi"
      num<-num/10^3
    } else if(num>10^6){
      unidade <- "tri"
      num<-num/10^6

    }
    #data.frame("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))
    list("num"=paste0("R$", format(round(num,2), digits=3, decimal.mark=",")," ", unidade))
  }
}


#* Retorna o teto do gasto atingido
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /teto_gasto_atingido
function(formato = "string"){

  limites<-readr::read_csv("https://raw.githubusercontent.com/tchiluanda/limites_teto/main/data/limites_teto_atualizados.csv")

  limites<-
  limites %>%
    mutate(ano = str_sub(referencia, str_length(referencia)-3,str_length(referencia)))





  package<- ckanr::package_show(id="8675a0a4-31c5-4593-a24d-fb8e17376eca",
                                url="https://www.tesourotransparente.gov.br/ckan")

  URL_add<- package[["resources"]][[2]][["url"]]
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)

  desp_sujeita_teto<- read_xlsx(tmp,sheet = 3,skip = 3, col_names = TRUE)[1,2]




  ano_corrente<-  readxl::excel_sheets(tmp)[2]

  teto<-
    (limites %>%
       filter(discriminacao=="VI. TOTAL",
              ano==ano_corrente))$valor




  num<- (desp_sujeita_teto[1,1]/(teto*10^6))*100

  names(num)<- "valor"

  num<- num$valor

  # load(file ="dt_ultima_atualizacao.RData")
  #
  #
  #
  #
  # #tb_ckan<-resource_show(id="a66311e0-fb60-4354-b6d4-5ed3dbe7b297",url="https://apickan.tesouro.gov.br/ckan")
  # #tb_ckan<-resource_show(id="a66311e0-fb60-4354-b6d4-5ed3dbe7b297",url="https://www.tesourotransparente.gov.br/ckan/")
  #
  #
  # if (package[["resources"]][[1]][["last_modified"]] == dt_ultima_atualizacao){
  #
  #   load(file ="df_teto.RData")
  #   if (formato == "string"){
  #     ls_num<-list("num"=paste0(format(round(df_teto$num,1), digits=2, decimal.mark=","), df_teto$unidade))
  #     return(ls_num)
  #   }
  #   else{
  #     load(file= "df_teto.RData")
  #     return(df_teto)
  #   }
  # }
  #
  # dt_ultima_atualizacao<- package[["resources"]][[1]][["last_modified"]]
  # save(file= "dt_ultima_atualizacao.RData", list = ("dt_ultima_atualizacao"))
  # #URL_add <- tb_ckan$url
  # #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  # URL_add<- package[["resources"]][[1]][["url"]]
  # tmp = paste(getwd(),"temp.xlsx")
  # tmp = tempfile(fileext = ".xlsx")
  # download.file(URL_add,mode = "wb", destfile = tmp)
  #
  #
  # desp_sujeita_teto<- read_xlsx(tmp,sheet = 3,skip = 3, col_names = TRUE)[1,2]
  #
  #
  #
  #
  #
  #
  #
  #
  # df_pag<-read_xlsx(tmp,sheet = 1, col_names = TRUE)
  # names(df_pag)[1] <- "mes"
  # names(df_pag)[22] <- "tratamento"
  # names(df_pag) [20] <- "pag_totais"
  #
  # ult_mes <- df_pag$mes[NROW(df_pag)]
  # ano <- substr(ult_mes,5,9)
  #
  # df_pag_total <- df_pag %>%
  #               filter(substr(mes,5,9) ==ano,
  #                      tolower(tratamento) == "despesa sujeita ao teto") %>%
  #               summarise(
  #                 total = sum(pag_totais)
  #               )
  # valor_pag <- as.numeric(df_pag_total$total)
  #
  # df_teto <- read_xlsx(tmp,sheet = 3,skip = 3, col_names = TRUE)
  #
  #
  # col<- paste0("Despesa Total - Limite ",ano)
  #
  # teto<-as.numeric(df_teto[NROW(df_teto), col])

  # num<- (valor_pag/(teto*10^6))*100


  unidade<- "%"
  #df_teto<-data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  #save(file ="df_teto.RData", list = "df_teto")


  if (formato=="df"){
    return(df_teto)

  } else {
    #data.frame("num"=paste0(format(round(num,1), digits=2, decimal.mark=","), unidade))
    ls_num<-list("num"=paste0(format(round(num,1), digits=2, decimal.mark=","), unidade))
    return(ls_num)

  }
}


#* Retorna a despesa de pessoal por RCL
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /despesa_pessoal_por_RCL
function(formato = "string"){
  #tb_ckan<-resource_show(id="92580d1e-0e30-45c9-9aa1-7a05586a30da",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="92580d1e-0e30-45c9-9aa1-7a05586a30da",url="https://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.csv")
  tmp = tempfile(fileext = ".csv")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_p_rcl<-read.csv2(tmp, sep = ";", fileEncoding="latin1")
  names(df_p_rcl)[1] <-"periodo"
  names(df_p_rcl)[5] <-"perc_desp_total_rcl"
  ult_mes <- df_p_rcl$periodo[NROW(df_p_rcl)]
  unidade<- "%"

  num <- (df_p_rcl$perc_desp_total_rcl[NROW(df_p_rcl)])*100
  if (formato=="df"){
    data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)

  } else {
    #data.frame("num"=paste0(format(round(num,1), digits=2, decimal.mark=","), unidade))
    list("num"=paste0(format(round(num,1), digits=2, decimal.mark=","), unidade))

  }
}

#* Retorna o limite de pagamento total do Poder Executivo Federal
#* @param formato O formato do retorno. Use df para dataframe. Default é string
#* @serializer unboxedJSON
#* @get /limite_pag_total_exec_federal
function(formato = "string"){
  #tb_ckan<-resource_show(id="51077823-a7a6-4df4-9b8f-ccfd44a8053b",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="51077823-a7a6-4df4-9b8f-ccfd44a8053b",url="https://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_pag<-read_xlsx(tmp,sheet = 6,skip = 8,n_max = 37, col_names = FALSE)
  unidade<- as.character(df_pag[1,NCOL(df_pag)])
  ult_mes<- NA
  num <- as.numeric(as.numeric(df_pag[37,NCOL(df_pag)]))
  if (formato=="df"){
    data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  } else {
    unidade <-"bi"
    num<- num/10^6
    if (num>=1000) {
      unidade <- "tri"
      num<-num/1000
    }
    list("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))
    #data.frame("num"=paste0("R$", format(round(num,1), digits=2, decimal.mark=",")," ", unidade))

  }
}



gera_graf_resultado_primario <- function(){
  recurso_TT <- resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",
                              url="http://www.tesourotransparente.gov.br/ckan")
  download.file(recurso_TT$url, destfile = "rtn.xlsx", mode = 'wb', quiet = TRUE )
  tabela <- read_excel("rtn.xlsx", sheet = "1.1-A", skip = 4)

  meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
  serie <- tabela %>%
    rename(rotulos = 1) %>%
    filter(str_detect(rotulos, "5. RESULTADO PRIMÁRIO") |
             str_detect(rotulos, "Deflator - IPCA")) %>%
    mutate(rotulos = c("Valor", "IPCA")) %>%
    gather(-1, key = "Periodo", value = "Valores") %>%
    spread(key = rotulos, value = Valores) %>%
    mutate(Valor = as.numeric(Valor),
           IPCA = as.numeric(IPCA),
           Periodo = as.Date(as.numeric(Periodo), origin = "1899-12-30"),
           Ano = lubridate::year(Periodo),
           Mes = lubridate::month(Periodo),
           Valor_12m = zoo::rollapply(Valor, width = 12, FUN = sum, fill = NA, align = 'right'),
           Resultado = ifelse(Valor_12m > 0, "Positivo", "Negativo"),
           Data = paste0(Ano, " - ", meses[Mes])) %>%
    filter(!is.na(Valor_12m)) %>%
    filter(Periodo >= data_inicial_graficos) # para ficar igual à série da dívida

  ultimo_mes<- serie$Periodo[NROW(serie)]



  file_periodo<- paste0("rtn",ultimo_mes,".gif")


  if (!file.exists(file_periodo)){
    palavra_chave <- "Grupo_"
    i <- 1
    ultimo_grupo <- paste0("Grupo_", i)
    grupo <- c(ultimo_grupo)
    vetor <- serie$Resultado
    for (j in 2:length(vetor)) {
      if (vetor[j] != vetor[j-1]) {
        i <- i+1
        ultimo_grupo <- paste0("Grupo_", i)
      }
      grupo <- c(grupo, ultimo_grupo)
    }
    serie$Grupos <- grupo

    vermelho <- brewer.pal(3, name = "Set1")[1]
    azul <- "#1f476a" # brewer.pal(3, name = "Set1")[2]
    verde <- brewer.pal(3, name = "Set1")[3]
    vermelho_claro <- "#ee7576"
    azul_claro     <- "#2c90bf" # "#87b1d4"
    grafico_linha <- ggplot(serie, aes(x = Periodo, y = Valor_12m, color = Resultado, fill = Resultado, group = 1)) +
      geom_area(aes(group = Grupos)) +
      geom_line(size = 1) +
      geom_point(size = 3, shape = 21, fill = "#f0f5f7") +
      geom_hline(yintercept = 0, color = '#f0f5f7', size = 1) +
      scale_color_manual(values = c("Negativo" = vermelho, "Positivo" = azul)) +
      scale_fill_manual(values = c("Negativo" = vermelho_claro, "Positivo" = azul_claro)) +
      scale_x_date(date_breaks = "1 years",
                   date_labels = "%Y",
                   limits = c(as.Date(data_inicial_graficos), NA), #"1997-12-01"
                   expand = expand_scale(mult = c(.04, .04))) +
      coord_cartesian(clip = 'off') +
      labs(x = NULL, y = NULL) +
      geom_text(aes(x = Periodo + 200,
                    label = format(round(Valor_12m/1000,0),
                                   big.mark = ".",
                                   decimal.mark = ",")),
                size = 7, fontface = 'plain', family = "Open Sans SemiBold") +
      tema() +
      theme(legend.position = 'none',
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle=90),
            plot.background = element_rect(color = "#f0f5f7", linetype = "solid", size = 2))

    #grafico_linha

    gif_linhas <- grafico_linha +
      transition_reveal(Periodo) +
      labs(
        #title = "Resultado do Tesouro Nacional (R$ bi)",
        subtitle = paste("Últimos 12 meses, atualizados pelo IPCA, até {lubridate::year(frame_along)}/{meses[lubridate::month(frame_along)]}"))



    anim_graf<-
      animate(gif_linhas, nframes = round(nrow(serie)/2), height = 488, width = 668,
              renderer = gifski_renderer(loop = FALSE)) #type = "cairo"


    anim_save(file_periodo, anim_graf)
  }


  con <- file(file_periodo, "rb")

  # Copiar o arquivo para o /home
  file.copy(file_periodo, "/home/graf_resultado_primario.gif")

  img <- readBin(con, "raw", file.info(file_periodo)$size)

  close(con)

  img


}

#* Gráfico do resultado primário do governo central
#* @serializer contentType list(type = "image/gif")
#* @get /graf_resultado_primario
function() {
  gera_graf_resultado_primario()
}

#* Gráfico do resultado primário do governo central
#* @get /graf_resultado_primario_async
function() {
  fut <- future_promise({gera_graf_resultado_primario()})

  id <- length(futures) + 1
  futures[[id]] <<- list(fvalue = fut, status = "Gerando", result = NULL)

  then(fut, onFulfilled = function(value) {
    futures[[id]]$status <<- "Sucesso"
    futures[[id]]$result <<- base64enc::base64encode(value)
  }, onRejected = function(reason) {
    futures[[id]]$status <<- "Erro"
    futures[[id]]$result <<- reason
  })

  list(id = id)
}


gera_graf_estoque_divida <- function(){



  recurso_dpf_TT <- resource_show(id="0402cb77-5e4c-4414-966f-0e87d802a29a",
                                  url="http://www.tesourotransparente.gov.br/ckan")

  download.file(recurso_dpf_TT$url, destfile = "./dpf.xlsx", mode = 'wb' )
  tabela_div <- read_excel("dpf.xlsx", skip = 4)

  meses_red <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  which(meses_red == "Fev")
  #         Mes_nome = factor(Mes, levels = meses_red)
  dpf <- tabela_div %>%
    rename(rotulos = 1) %>%
    filter(str_detect(rotulos, "DPF EM PODER")) %>%
    select(-1) %>%
    gather(key = Mes_Ano, value = Valor) %>%
    separate(Mes_Ano, into = c("Mes", "Ano"), sep = "/") %>%
    filter(!is.na(Ano)) %>% # (3)
    mutate(Ano = as.integer(paste0('20', Ano)), # (4)
           Mes_num = match(Mes, meses_red),
           Periodo = as.Date(paste0(Ano, "-",
                                    if_else(Mes_num < 10, "0", ""), Mes_num, "-",
                                    "01")))

  ultimo_mes<- dpf$Periodo[NROW(dpf)]

  file_periodo<- paste0("dpf",ultimo_mes,".gif")

  if (!file.exists(file_periodo)){

    recurso_TT_rtn <- resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",
                                url="http://www.tesourotransparente.gov.br/ckan")
    download.file(recurso_TT_rtn$url, destfile = "rtn.xlsx", mode = 'wb', quiet = TRUE )
    tabela <- read_excel("rtn.xlsx", sheet = "1.1-A", skip = 4)

    meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")


    serie <- tabela %>%
      rename(rotulos = 1) %>%
      filter(str_detect(rotulos, "5. RESULTADO PRIMÁRIO") |
               str_detect(rotulos, "Deflator - IPCA")) %>%
      mutate(rotulos = c("Valor", "IPCA")) %>%
      gather(-1, key = "Periodo", value = "Valores") %>%
      spread(key = rotulos, value = Valores) %>%
      mutate(Valor = as.numeric(Valor),
             IPCA = as.numeric(IPCA),
             Periodo = as.Date(as.numeric(Periodo), origin = "1899-12-30"),
             Ano = lubridate::year(Periodo),
             Mes = lubridate::month(Periodo),
             Valor_12m = zoo::rollapply(Valor, width = 12, FUN = sum, fill = NA, align = 'right'),
             Resultado = ifelse(Valor_12m > 0, "Positivo", "Negativo"),
             Data = paste0(Ano, " - ", meses[Mes])) %>%
      filter(!is.na(Valor_12m)) %>%
      filter(Periodo >= data_inicial_graficos)


    ipca <- serie %>% select(Periodo, IPCA)
    dpf <- dpf %>%
      left_join(ipca) %>%
      mutate(Valor_ipca = Valor * IPCA)

    grafico_linha_dpf <- ggplot(dpf, aes(x = Periodo, y = Valor_ipca, color = TRUE, group = 1)) +
      geom_area(fill = "#6cb2d2", color = NA) +
      geom_line(size = 1) +
      #geom_hline(yintercept = 0, color = '#f0f5f7', size = 1) +
      geom_point(size = 3, shape = 21, fill = "#f0f5f7") +
      scale_color_manual(values = c("TRUE" = "#1E4C7A")) +
      scale_x_date(date_breaks = "1 years",
                   date_labels = "%Y",
                   limits = c(as.Date(data_inicial_graficos), NA),
                   expand = expand_scale(mult = c(.05, .04))) +
      coord_cartesian(
        ylim = c(0,#min(dpf$Valor_ipca),
                 max(dpf$Valor_ipca)),
        clip = "on") + # ponto e valor iniciais fixos a seguit
      geom_point(size = 3, shape = 21, fill = "#f0f5f7",
                 x = dpf$Periodo[1], y = dpf$Valor_ipca[1]) +
      geom_text(x = dpf$Periodo[1] - 100, y = dpf$Valor_ipca[1] + 50,
                label = format(round(dpf$Valor_ipca[1]/1000, 2),
                               big.mark = ".",
                               decimal.mark = ","),
                size = 7, fontface = 'plain', family = "Open Sans SemiBold") + # fim vlrs iniciais
      labs(x = NULL, y = NULL) +
      geom_text(aes(x = Periodo + 200,
                    label = format(round(Valor_ipca/1000, 2),
                                   big.mark = ".",
                                   decimal.mark = ",")),
                size = 7, fontface = 'plain', family = "Open Sans SemiBold") +
      tema() +
      theme(legend.position = 'none',
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text( angle=90),
            plot.background = element_rect(color = "#f0f5f7", linetype = "solid", size = 2))
    # observações: se vc setar o ylim(), a área deixa de aparecer. para mantê-la, é preciso setar o ylim como parâmetro do coord_cartesian(). Da documentação (https://ggplot2.tidyverse.org/reference/lims.html):
    # "For changing x or y axis limits without dropping data observations, see coord_cartesian()."

    gif_linhas_dpf <- grafico_linha_dpf +
      transition_reveal(Periodo) +
      labs(
        #title = "Dívida Pública Federal (R$ tri)",
        subtitle = paste("Estoque atualizado pelo IPCA até {lubridate::year(frame_along)}/{meses[lubridate::month(frame_along)]}"))

    anima<- animate(gif_linhas_dpf, nframes = round(nrow(dpf)/2), height = 488, width = 668,
            renderer = gifski_renderer(loop = FALSE))
    anim_save(file_periodo, animation = anima)
  }

  con <- file(file_periodo, "rb")

  img <- readBin(con, "raw", file.info(file_periodo)$size)

  # Copiar o arquivo para o /home
  file.copy(file_periodo, "/home/graf_estoque_divida.gif")

  close(con)

  img

}

#* Gráfico do estoque da dívida
#* @serializer contentType list(type = "image/gif")
#* @get /graf_estoque_divida
function() {
  gera_graf_estoque_divida()
}

#* Gráfico do estoque da dívida
#* @get /graf_estoque_divida_async
function() {
  fut <- future_promise({gera_graf_estoque_divida()})

  id <- length(futures) + 1
  futures[[id]] <<- list(fvalue = fut, status = "Gerando", result = NULL)

  then(fut, onFulfilled = function(value) {
    futures[[id]]$status <<- "Sucesso"
    futures[[id]]$result <<- base64enc::base64encode(value)
  }, onRejected = function(reason) {
    futures[[id]]$status <<- "Erro"
    futures[[id]]$result <<- reason
  })

  list(id = id)
}



gera_graf_despesas_governo <- function(){


  series_temporais<-
  get_12_month_accumulated_account_data_by_month(.data=c("Bolsa família",
                                                              "4.2  Pessoal e Encargos Sociais",
                                                              "4.1  Benefícios Previdenciários",
                                                              "exceto PAC",
                                                              "4.4.2.1 Saúde",
                                                              "4.4.2.2 Educação"
                                                              ),
                                                      month = 1:12,
                                                      match_required = FALSE)

  series_temporais<-
  series_temporais %>%
    filter(lubridate::year(Data)>=2009)

  ultimo_mes<-as.character(series_temporais$Data[NROW(series_temporais)])

  file_periodo<- paste0("despesas",ultimo_mes,".gif")

  if (!file.exists(file_periodo)){

    paleta <- brewer.pal(6, "Set3")
    # ajeitar cores
    paleta_rgb <- col2rgb(paleta)
    # clareia <- function(pal, n, i, fator){
    #   componente <- round(pal[n,i] * fator, 0)
    #   if (componente > 255) return(255)
    #   else return(componente)
    # }
    fator <- 1.5
    paleta_darker <- NULL
    for (i in 1:dim(paleta_rgb)[2] ) {
      paleta_darker <- c(paleta_darker,
                         rgb(paleta_rgb[1,i] %/% fator,
                             paleta_rgb[2,i] %/% fator,
                             paleta_rgb[3,i] %/% fator, maxColorValue = 255))
    }
    paleta_original <- c("#832F11", "#CA4017", "#F8AC08", "#CEC806", "#96C11E", "#028063", "#149339", "#4E857E", "#08869B", "#1E466A", "#6E287C")


    janeiros <- unique(series_temporais$Data[str_detect(series_temporais$Data, "-01-01")])
    anos <- lubridate::year(janeiros)
    meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
    ultimo_periodo <- max(series_temporais$Data)

    graf_linhas_desp <-
      series_temporais %>%
      mutate(classificador= case_when(
        Rubrica == "4.1  Benefícios Previdenciários" ~ "Benefícios Previd.",
        Rubrica == "4.2  Pessoal e Encargos Sociais" ~ "Pessoal",
        Rubrica == "4.3.07  Créditos Extraordinários (exceto PAC)" ~ "Créd. extra",
        Rubrica == "4.4.1.2 Bolsa Família e Auxílio Brasil" ~ "Renda mínima",
        Rubrica == "4.4.2.1 Saúde" ~ "Saúde discric.",
        Rubrica == "4.4.2.2 Educação" ~ "Educação discric."
      )) %>%
      ggplot( aes(x = Data, y = valor_atualizado, group = classificador, color = str_wrap(classificador,10))) +
      geom_line(size = 1) +
      scale_x_date(breaks = janeiros,
                   labels = anos,
                   expand = expand_scale(mult = c(0, .50))) +
      scale_color_manual(values = paleta_darker) +
      coord_cartesian(clip = 'off') +
      labs(
        y = NULL,
        x = NULL
      ) +
      tema() +
      theme(legend.position = 'right') +
      theme(axis.text = element_text(size = 14, family = "Open Sans Condensed Light", angle=90), # era 10
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.text = element_text(size = 16, family = "Open Sans Condensed Light"),
            legend.title = element_text(size = 17, family = "Open Sans Condensed Light"),
            plot.background = element_rect(color = "#f0f5f7", linetype = "solid", size = 2)) +
      labs(color= "Classificador")

    # aqui tinha um problema. com o expand tão grande, a escala continuava até sei lá quanto (2025, acho). para resolver isso tentei usar o `limits`. Não resolveu. aí resolvi setar mais ou menos manualmente, bom breaks e labels no lugar de date_breaks e date_labels. Mas eu tinha deixado o limits, então o geom_text não estava sendo escrito, pq ficou fora do limite.
    graf_desp_para_gif <- graf_linhas_desp +
      geom_point(size = 3) +

      geom_text_repel(aes(label = paste(str_pad(round(valor_atualizado/10^3, 0), width = 3, pad = " "))), #,enc2native(classificador)
                      hjust = 1,
                      size = 7,
                      #nudge_y = c(0,0,30,-30,0,0),
                      direction = "both")



    gif_linhas_desp <- graf_desp_para_gif +
      transition_reveal(Data) +
      labs(subtitle = paste("Últimos 12 meses, atualizados pelo IPCA, até {lubridate::year(frame_along)}/{meses[lubridate::month(frame_along)]}"))

    anima<- animate(gif_linhas_desp, nframes = (round(nrow(series_temporais)/6)), height = 488, width = 668,
            renderer = gifski_renderer(loop = FALSE))


    anim_save(file_periodo, animation = anima)



  }


  con <- file(file_periodo, "rb")

  # Copiar o arquivo para o /home
  file.copy(file_periodo, "/home/graf_despesas_governo.gif")

  img <- readBin(con, "raw", file.info(file_periodo)$size)

  close(con)

  img

}

#* Gráfico de despesas de governo
#* @serializer contentType list(type = "image/gif")
#* @get /graf_despesas_governo
function() {
  gera_graf_despesas_governo()
}

#* Gráfico de despesas de governo
#* @get /graf_despesas_governo_async
function() {
  fut <- future_promise({gera_graf_despesas_governo()})

  id <- length(futures) + 1
  futures[[id]] <<- list(fvalue = fut, status = "Gerando", result = NULL)

  then(fut, onFulfilled = function(value) {
    futures[[id]]$status <<- "Sucesso"
    futures[[id]]$result <<- base64enc::base64encode(value)
  }, onRejected = function(reason) {
    futures[[id]]$status <<- "Erro"
    futures[[id]]$result <<- reason
  })

  list(id = id)
}

gera_todos_graf_async <- function(){
  graf_despesas_governo <- base64enc::base64encode(gera_graf_despesas_governo())
  graf_estoque_divida <- base64enc::base64encode(gera_graf_estoque_divida())
  graf_resultado_primario <- base64enc::base64encode(gera_graf_resultado_primario())

  list(graf_despesas_governo = graf_despesas_governo,
       graf_estoque_divida = graf_estoque_divida,
       graf_resultado_primario = graf_resultado_primario)
}

#* Gera todos gráficos async
#* @get /graf_todos_graf_async
function() {
  fut <- future_promise({gera_todos_graf_async()})

  id <- length(futures) + 1
  futures[[id]] <<- list(fvalue = fut, status = "Gerando", result = NULL, data = Sys.time())

  then(fut, onFulfilled = function(value) {
    futures[[id]]$status <<- "Sucesso"
    futures[[id]]$result <<- value
  }, onRejected = function(reason) {
    futures[[id]]$status <<- "Erro"
    futures[[id]]$result <<- reason
  })
  

  list(id = id)
}


#* @get /graf_despesas_governo.gif
function(res){
  res$setHeader("Content-Type", "image/gif")
  res$body <- readBin("/home/graf_despesas_governo.gif", "raw", file.info("/home/graf_despesas_governo.gif")$size)
  
  res
}

#* @get /graf_resultado_primario.gif
function(res){
  res$setHeader("Content-Type", "image/gif")
  res$body <- readBin("/home/graf_resultado_primario.gif", "raw", file.info("/home/graf_resultado_primario.gif")$size)
  
  res
}

#* @get /graf_estoque_divida.gif
function(res){
  res$setHeader("Content-Type", "image/gif")
  res$body <- readBin("/home/graf_estoque_divida.gif", "raw", file.info("/home/graf_estoque_divida.gif")$size)
  
  res
}

#* @get /result/<id>
function(id){
  # Recupera o future da lista global
  fut <- futures[[as.integer(id)]]

  # Percorre todos os resultados da lista global que forem mais antigos do que 1 dia
  # e apaga da lista o $result
  for (i in 1:length(futures)) {
    if (futures[[i]]$data < Sys.time() - 60*60*24) {
      futures[[i]]$result <<- NULL
    }
  }

  # Se o status do fut for "SUCESSO" ou "ERRO", retorna o resultado
  if (fut$status %in% c("Sucesso", "Erro")) {
    status <- fut$status
    result <- fut$result

    return(list(status = status, result = result))
  } else {
    return(list(status = "Gerando"))
  }

}

#* @get /result/<id>/limpar
function(id){
  # Recupera o future da lista global
  fut <- futures[[as.integer(id)]]

  # Apaga da lista o resultado
  futures[[as.integer(id)]]$result <<- NULL

  return(list(status = "Limpo"))
}

#* @get /ping
function() {
  "true"
}