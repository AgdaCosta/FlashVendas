#_______________________________________________________________________________________________________
#           Pacotes Rstudio
#-------------------------------------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(gsheet)
library(dm)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(ggplot2)
library(formattable)

#_______________________________________________________________________________________________________
#           Carregando dados
#-------------------------------------------------------------------------------------------------------

fato = gsheet2text("https://docs.google.com/spreadsheets/d/148h78xDiZBKNvJ9yvJXLXmONW-6hT21tFj9op7lmVBU/edit?usp=sharing")
vendedor = gsheet2text("https://docs.google.com/spreadsheets/d/1aqQlvOOq2Y957mm2IwujRyFr99ZBybT3Gc_9M1VwExc/edit?usp=sharing")
cliente = gsheet2text("https://docs.google.com/spreadsheets/d/1QPyHu1-SpUHFEgUN8_fmZ5Md4Uw_f9KZi-I8d4QGrB4/edit?usp=sharing")
produto = gsheet2text("https://docs.google.com/spreadsheets/d/13FTsICfGTlUCG1adCdD2bJUex63dYCPjq4IW_XXhGi4/edit?usp=sharing")
supervisor = gsheet2text("https://docs.google.com/spreadsheets/d/13si9hiw9b_N1na5Kg_x70dqwfWQS1offSiHyqC6HdTg/edit?usp=sharing")
loja = gsheet2text("https://docs.google.com/spreadsheets/d/1Mxq-o8LuhcHM-0ZjVKtvQu1rML81X3bjENoy4J5IudM/edit?usp=sharing")


fato = read.csv( text = fato, dec = ',', header = T, sep = "," )
vendedor = read.csv( text = vendedor, dec = ',', header = T, sep = "," )
cliente = read.csv( text = cliente, dec = ',', header = T, sep = "," )
produto = read.csv( text = produto, dec = ',', header = T, sep = "," )
supervisor = read.csv( text = supervisor, dec = ',', header = T, sep = "," )
loja = read.csv( text = loja, dec = ',', header = T, sep = "," )


#_______________________________________________________________________________________________________
#           Manipulando dados
#-------------------------------------------------------------------------------------------------------
vendedor = subset(vendedor,select = - c(LojaPK)) #Excluindo campos não utilizados
supervisor = subset(supervisor,select = - c(LojaPK))
fato = subset(fato , select = -c(Key))

fato$ValorTroca = fato$ValorTroca %>% accounting() #Tratamento dos campos de valor para formato 0.00
fato$ValorOMNI = fato$ValorOMNI  %>% accounting()
fato$ValorVenda = fato$ValorVenda %>% accounting()
fato$Meta = fato$Meta  %>% accounting()


fato$Data = dmy(fato$Data) #Troca de data de ano-mes-dia para dia-mes-ano

fato = fato %>% #Criação de novos campos
  dplyr::mutate( dia = day(Data),
                 mes = factor(month(Data),levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                        labels = c("Jan", "Fev", "Mar", "Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")) ,
                 ano = year(Data),
                 VendaLoja = ValorVenda - ValorTroca,
                 VendaTotal = ValorVenda - ValorTroca + ValorOMNI,
                 VendaTotalAcumulado = 0,
                 PMV = VendaLoja / length(unique(fato$TicketPK)),
                 diferença_meta = VendaLoja - Meta,
                 perc_Meta =  1-(VendaLoja/sum(Meta)),
                 TicketMedio = VendaLoja / (  (length(unique(fato$TicketPK))) -  (
                   length((unique(select(filter(fato, TipoCupom == 'Troca'), TicketPK)))$TicketPK)  )
                 )
               )  

acumulado = function(new, aux){ #Função para calcular acumulado
  i = 1
  soma = 0
  n = length(new)
  for(i in 1:n ){
    new[i] = aux[i] + soma
    soma = new[i]
  }  
  print(new)
} 

fato$perc_Meta = percent(fato$perc_Meta) #Formato %
fato$VendaTotalAcumulado = acumulado(new = fato$VendaTotalAcumulado, aux = fato$VendaLoja )

#_______________________________________________________________________________________________________
#         Associando tabelas
#-------------------------------------------------------------------------------------------------------
banco = dm(fato, cliente, loja, produto, supervisor, vendedor) #Selecionando as tabelas com relacionamento

bancoPK = banco %>% #Definindo as chaves Pk de cada tabela
  dm_add_pk(table = cliente, columns = ClientePK) %>%
  dm_add_pk(table = loja, columns = LojaPK) %>%
  dm_add_pk(table = produto, columns = ProdutoPK) %>%
  dm_add_pk(table = supervisor, columns = SupervisorPK) %>%
  dm_add_pk(table = vendedor, columns = VendedorPK) 

bancoFK = bancoPK %>% #Relacionando as chaves Pk de cada tabela com sua correspondente na tabela Fato
  dm_add_fk(table = fato, columns = ClientePK, ref_table = cliente) %>%
  dm_add_fk(table = fato, columns = LojaPK, ref_table = loja) %>%
  dm_add_fk(table = fato, columns = ProdutoPK, ref_table = produto) %>%
  dm_add_fk(table = fato, columns = SupervisorPK, ref_table = supervisor) %>%
  dm_add_fk(table = fato, columns = VendedorPK, ref_table = vendedor) 

banco = bancoFK

#_______________________________________________________________________________________________________
#         Visualização do Modelo de dados
#-------------------------------------------------------------------------------------------------------
banco %>%
       dm_set_colors( "#5986C4" = fato) %>%
                                        dm_draw()  #Imagem do modelo de dados


#_______________________________________________________________________________________________________
#         União dos dados 
#-------------------------------------------------------------------------------------------------------

dados = banco %>%
              dm_flatten_to_tbl(fato) 

rm(list=setdiff( ls(), "dados" ) ) #Remoção de todos os anteriores menos a tabela Dados 


#_______________________________________________________________________________________________________

#-------------------------------------------------------------------------------------------------------

 

  
  