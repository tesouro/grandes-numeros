# Grandes Números (API em R)

A API grandes números é construída a partir da suíte de aplicativos RStudio, e tem como base a biblioteca `Plumber`. Os gráficos e cálculos| expostos pela API são adquiridos dos conjuntos de dados abertos CKAN do site do ```"Tesouro Transparente"```.

Desenvolvido pelo Grupo de Trabalho GT-CEAD do Tesouro Nacional, em 2021.

## URL de acesso principal

* https://grandesnumeros.tesouro.gov.br

_Observação: Esta URL quando acessada diretamente retorna um erro HTTP 404, pois é necessário acesso através de algum dos endpoints abaixo_

## Endpoints de acesso para consumo da API Grandes Números

`/estoque_dpf`
#### Retorna o estoque da dívida pública federal

`/emissoes_dpf`

#### Retorna as emissões da DPF no ano

`/resgate_dpf`

#### Retorna o resgate da dívida pública federal

`/resultado_primario`

#### Retorna o resultado primário do governo central

`/receita_primaria_liquida`

#### Retorna a receita líquida primária do governo central

`/despesa_primaria_total`

#### Retorna a despesa primária total do governo central

`/teto_gasto_atingido`

#### Retorna o teto do gasto atingido

`/despesa_pessoal_por_RCL`

#### Retorna a despesa de pessoal por RCL

`/limite_pag_total_exec_federal`

#### Retorna o limite de pagamento total do Poder Executivo Federal

`/graf_estoque_divida`

#### Retorna gráfico de estoque usado na capa do Tesouro Transparente (TT)

`/graf_gastos_governo`

#### Retorna gráfico de gastos usado na capa do TT

`/graf_resultado_primario`

#### Retorna gráfico de resultado primário usado na capa do TT

