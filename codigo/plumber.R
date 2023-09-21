
#* Plot a histogram
#* @png
#* @get /hist
function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Return csv from series_temporais_analise
#* @get /data.csv
function(res){
  load("../grafico_rtn/todas_series.Rdata")
  
  con <- textConnection("val","w")
  write.csv(series_temporais_analise,
            row.names=FALSE,
            fileEncoding = "UTF-8", con)
  close(con)
  
  res$body <- paste(val, collapse="\n")
  res
}


# API_Grandes_Numeros_STN.R

library(ckanr)
library(readxl)
library(utils)
library(tidyverse)


#* Retorna o estoque da dívida pública federal
#* @get /estoque_dpf
function(){
  #tb_ckan<-resource_show(id="0402cb77-5e4c-4414-966f-0e87d802a29a",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="0402cb77-5e4c-4414-966f-0e87d802a29a",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 5, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,NCOL(df_dpf)])
  valor <- as.numeric(as.numeric(df_dpf[5,NCOL(df_dpf)]))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna as emissões da DPF no ano
#* @get /emissoes_dpf
function(){
  #tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 5, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,(NCOL(df_dpf)-1)])
  valor <- as.numeric(as.numeric(df_dpf[5,NCOL(df_dpf)]))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna o resgate da dívida pública federal
#* @get /resgate_dpf
function(){
  #tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 19, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,(NCOL(df_dpf)-1)])
  valor <- as.numeric(as.numeric(df_dpf[19,NCOL(df_dpf)]))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna o resultado primário do governo central
#* @get /resultado_primario
function(){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan/")
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
  
  valor <- as.numeric(sum(as.numeric(df_rtn[74,pos_ano_corrente])))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna a receita líquida primária do governo central
#* @get /receita_primaria_liquida
function(){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan/")
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
  
  valor <- as.numeric(sum(as.numeric(df_rtn[37,pos_ano_corrente])))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna a despesa primária total do governo central
#* @get /despesa_primaria_total
function(){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan/")
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
  
  valor <- as.numeric(sum(as.numeric(df_rtn[38,pos_ano_corrente])))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}

#* Retorna o teto do gasto atingido
#* @get /teto_gasto_atingido
function(){
  #tb_ckan<-resource_show(id="a66311e0-fb60-4354-b6d4-5ed3dbe7b297",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="a66311e0-fb60-4354-b6d4-5ed3dbe7b297",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_pag<-read_xlsx(tmp,sheet = 1, col_names = TRUE)
  names(df_pag)[1] <- "mes"
  names(df_pag)[22] <- "tratamento"
  names(df_pag) [20] <- "pag_totais"
  
  ult_mes <- df_pag$mes[NROW(df_pag)]
  ano <- substr(ult_mes,5,9)
  
  df_pag_total <- df_pag %>%
    filter(substr(mes,5,9) ==ano,
           tolower(tratamento) == "despesa sujeita ao teto") %>%
    summarise(
      total = sum(pag_totais)
    )
  valor_pag <- as.numeric(df_pag_total$total)              
  
  df_teto <- read_xlsx(tmp,sheet = 3,skip = 3, col_names = TRUE)
  
  
  col<- paste0("Despesa Total - Limite ",ano)
  
  teto<-as.numeric(df_teto[NROW(df_teto), col])
  
  valor<- (valor_pag/(teto*10^6))*100
  
  
  unidade<- "%"
  
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna a despesa de pessoal por RCL
#* @get /despesa_pessoal_por_RCL
function(){
  #tb_ckan<-resource_show(id="92580d1e-0e30-45c9-9aa1-7a05586a30da",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="92580d1e-0e30-45c9-9aa1-7a05586a30da",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.csv")
  tmp = tempfile(fileext = ".csv")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_p_rcl<-read.csv2(tmp)
  names(df_p_rcl)[1] <-"periodo"
  names(df_p_rcl)[5] <-"perc_desp_total_rcl"
  ult_mes <- df_p_rcl$periodo[NROW(df_p_rcl)]
  unidade<- "%"
  
  valor <- (df_p_rcl$perc_desp_total_rcl[NROW(df_p_rcl)])*100
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}

#* Retorna o limite de pagamento total do Poder Executivo Federal
#* @get /limite_pag_total_exec_federal
function(){
  #tb_ckan<-resource_show(id="51077823-a7a6-4df4-9b8f-ccfd44a8053b",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="51077823-a7a6-4df4-9b8f-ccfd44a8053b",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_pag<-read_xlsx(tmp,sheet = 6,skip = 8,n_max = 37, col_names = FALSE)
  unidade<- as.character(df_pag[1,NCOL(df_pag)])
  ult_mes<- NA
  valor <- as.numeric(as.numeric(df_pag[37,NCOL(df_pag)]))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


