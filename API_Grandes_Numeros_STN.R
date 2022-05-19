# API_Grandes_Numeros_STN.R

library(ckanr)
library(readxl)
library(utils)
library(tidyverse)


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
  
  num <- as.numeric(sum(as.numeric(df_rtn[71,pos_ano_corrente])))
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
  
  num <- as.numeric(sum(as.numeric(df_rtn[37,pos_ano_corrente])))
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
  
  num <- as.numeric(sum(as.numeric(df_rtn[38,pos_ano_corrente])))
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
  
  load(file ="dt_ultima_atualizacao.RData")
  
  
  #tb_ckan<-resource_show(id="a66311e0-fb60-4354-b6d4-5ed3dbe7b297",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="a66311e0-fb60-4354-b6d4-5ed3dbe7b297",url="https://www.tesourotransparente.gov.br/ckan/")
  
  
  if (tb_ckan$last_modified == dt_ultima_atualizacao){
    
    load(file ="df_teto.RData")
    if (formato == "string"){
      ls_num<-list("num"=paste0(format(round(df_teto$num,1), digits=2, decimal.mark=","), df_teto$unidade))
      return(ls_num) 
    }
    else{
      load(file= "df_teto.RData")
      return(df_teto)
    }
  }
  
  dt_ultima_atualizacao<- tb_ckan$last_modified 
  save(file= "dt_ultima_atualizacao.RData", list = ("dt_ultima_atualizacao"))  
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
  
  num<- (valor_pag/(teto*10^6))*100


  unidade<- "%"
  df_teto<-data.frame("num" = num, "unidade" = unidade, "ult_mes" = ult_mes)
  save(file ="df_teto.RData", list = "df_teto")
  
  
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




