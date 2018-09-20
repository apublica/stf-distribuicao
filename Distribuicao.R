#Análise da distribuição de processos do STF
#Por Juliana Marques

##### pacotes

library(tidyverse) 
library(janitor) 
library(formattable) 
library(haven)
library(stringi)
library(stringr)
library(cluster)
library(readxl)
library(lubridate)
library(fastCluster)
library(dendextend)
library(hybridHClust)
library(isopam)
library(pyclust)
library(sparcl)


#####

arquivo_complemento <- read.csv("processos.csv")
arquivo <- read.csv("lista-processos.csv")

##### arquivo_complemento
# arrumar codificação de caracteres

arquivo_complemento$classe_processo <- as.character(arquivo_complemento$classe_processo)
Encoding(arquivo_complemento$classe_processo) <- "UTF-8"

arquivo_complemento$relator <- as.character(arquivo_complemento$ministro)
Encoding(arquivo_complemento$relator) <- "UTF-8"

arquivo_complemento$procedencia_ata <- as.character(arquivo_complemento$procedencia_ata)
Encoding(arquivo_complemento$procedencia_ata) <- "UTF-8"

arquivo_complemento$origem_ata <- as.character(arquivo_complemento$origem_ata)
Encoding(arquivo_complemento$origem_ata) <- "UTF-8"

arquivo_complemento$origem_detalhe <- as.character(arquivo_complemento$origem_detalhe)
Encoding(arquivo_complemento$origem_detalhe) <- "UTF-8"

arquivo_complemento$partes <- as.character(arquivo_complemento$partes)
Encoding(arquivo_complemento$partes) <- "UTF-8"

# arrumar caixa alta
arquivo_complemento$relator <- stri_trans_totitle(arquivo_complemento$relator)
arquivo_complemento$classe_processo <- stri_trans_totitle(arquivo_complemento$classe_processo)
arquivo_complemento$procedencia_ata <- stri_trans_totitle(arquivo_complemento$procedencia_ata)
arquivo_complemento$origem_ata <- stri_trans_totitle(arquivo_complemento$origem_ata)
arquivo_complemento$origem_detalhe <- stri_trans_totitle(arquivo_complemento$origem_detalhe)


# 1 criar ID unica
arquivo_complemento$id_proc <- paste0(arquivo_complemento$codigo_classe_processo, arquivo_complemento$numero_processo) 

# ajustes outros
arquivo_complemento$data<-as.Date(arquivo_complemento$data_de_distribuicao,format = "%Y-%m-%d") 
arquivo_complemento$relator<-str_replace(arquivo_complemento$relator, "Min. ", "")
arquivo_complemento$ministro <- NULL
arquivo_complementodata_distribuicao <- NULL
arquivo_complemento$data_protocolo <- NULL

# retirar duplicacao

arquivo_complemento <- arquivo_complemento[!(duplicated(arquivo_complemento[c("id_proc")]) | duplicated(arquivo_complemento[c("id_proc")], fromLast =FALSE)),]

arquivo_complemento<-data.frame(arquivo_complemento$id_proc,arquivo_complemento$origem_detalhe,arquivo_complemento$origem_ata,arquivo_complemento$procedencia_ata)

names(arquivo_complemento)<-c("id_proc","origem_detalhe","origem_ata","procedencia_ata")

##### arquivo
# arrumar codificação de caracteres

arquivo$relator <- as.character(arquivo$relator)
Encoding(arquivo$relator) <- "UTF-8"

arquivo$classe_processo <- as.character(arquivo$classe_processo)
Encoding(arquivo$classe_processo) <- "UTF-8"

arquivo$procedencia <- as.character(arquivo$procedencia)
Encoding(arquivo$procedencia) <- "UTF-8"

arquivo$origem <- as.character(arquivo$origem)
Encoding(arquivo$origem) <- "UTF-8"

arquivo$partes <- as.character(arquivo$partes)
Encoding(arquivo$partes) <- "UTF-8"

# arrumar caixa alta
arquivo$classe_processo <- stri_trans_totitle(arquivo$classe_processo)
arquivo$origem <- stri_trans_totitle(arquivo$origem)
arquivo$procedencia <- stri_trans_totitle(arquivo$procedencia)
arquivo$relator <- stri_trans_totitle(arquivo$relator)

# 1 criar ID unica //640.261
arquivo$id_proc <- paste0(arquivo$codigo_classe_processo, arquivo$numero_processo)


# retirar duplicacao de mesmo dia //637.160

arquivo<- arquivo[!(duplicated(arquivo[c("id_proc","data")]) | duplicated(arquivo[c("id_proc","data")], fromLast =FALSE)),]


# ajustes outros
arquivo$numero_processo <- NULL
arquivo$recurso <- NULL
arquivo$numero_ata <- NULL
arquivo$eletronico <- NULL
arquivo$url <- NULL
arquivo <- subset(arquivo, arquivo$relator != "relator") # exlcuir erro head

#640.261

distribuicao <- merge(arquivo, arquivo_complemento, by ="id_proc",all.x=TRUE)

======



#####filtro data analise
distribuicao <- distribuicao %>% 
mutate(as.Date(data)) %>%
filter(`as.Date(data)` > "2009-12-31" & `as.Date(data)` <"2018-06-02")

min(as.Date(distribuicao[,"data"]))
max(as.Date(distribuicao[,"data"]))

#434.887

vagas<-data.frame(c("Cármen Lúcia","Celso De Mello","Marco Aurélio","Eros Grau","Luiz Fux","Gilmar Mendes","Ayres Britto","Roberto Barroso","Ricardo Lewandowski","Dias Toffoli","Menezes Direito","Sepúlveda Pertence","Edson Fachin","Joaquim Barbosa","Alexandre De Moraes","Cezar Peluso","Teori Zavascki","Ellen Gracie","Rosa Weber"),c("V2","V3","V4","V7","V7","V12","V13","V13","V16","V17","V17","V17","V18","V18","V19","V19","V19","V20","V20"))

names(vagas)<-c("relator","vaga")

distribuicao$ano<-year(distribuicao$data)

distribuicao$origemvsclasse<-paste0(distribuicao$origem_detalhe," | ",distribuicao$classe_processo)

distribuicao <- merge(distribuicao, vagas, by ="relator",all.x=TRUE)

distribuicao_total<-distribuicao

write.csv2(distribuicao_total,"C:/Users/Juliana/Desktop/Atual/STF/monocrática/distribuicao_total.csv")


#####elimina origem internacional e relatores nulos

distribuicao<- subset(distribuicao, !grepl("Arg ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Bol ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Cze ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Ecu ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Esp ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Eua ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Fin ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Fra ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Gbr ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Ger ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Lib ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Hun ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Par ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Per ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Pan ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Pol ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Por ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Srb ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Svk ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Sud ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Uru ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Uru ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Kor ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Ita ",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("",distribuicao$origem_detalhe)==FALSE)
distribuicao<- subset(distribuicao, !grepl("-",distribuicao$origem_detalhe)==FALSE)
distribuicao<- subset(distribuicao, !grepl("Helvética",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Não Informada",distribuicao$origem_detalhe)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Ministra Presidente",distribuicao$relator)==TRUE)
distribuicao<- subset(distribuicao, !grepl("Ministro Presidente",distribuicao$relator)==TRUE)
distribuicao<- subset(distribuicao, !grepl("",distribuicao$vaga)==FALSE)



#####teste por ano (0)

distribuicao_filtro <-subset(distribuicao,ano==2012)

min(as.Date(distribuicao_filtro[,"data"]))
max(as.Date(distribuicao_filtro[,"data"]))

qtd_min<-distribuicao_filtro %>% 
     group_by() %>% 
     summarise(Contagem = length(unique(vaga)))

View(qtd_min)

##### tempo STF (1)

tmp <- tapply(distribuicao_filtro$data, list(distribuicao_filtro$vaga), function(x) {length(unique(x))})
tmp <- data.frame(tmp)
tmp$relator <- rownames(tmp)
names(tmp) <- c("tempo_stf", "vaga")

#### visao relator (2)

relator <- data.frame(table(distribuicao_filtro$vaga))
names(relator) <- c("vaga", "total_processo")
relator <- merge(relator, tmp, by = "vaga")
relator$normalizado <- relator$total_processo/relator$tempo_stf

##### ajuste disposição (3)

proc_relator <- data.frame(table(distribuicao$vaga,distribuicao$origem_detalhe))
proc_relator<-subset(proc_relator,Freq>0)
names(proc_relator)<-c("vaga","origem_detalhe","QTD")
origem_detalhe<-reshape(proc_relator, v.names = "QTD", idvar = "vaga", timevar="origem_detalhe",direction="wide") 



##### origem_detalhe (Agrupamento das semelhanças/diferenças entre os objetos de um conjunto de dados) - K-Means (4)

analise1 <- merge(origem_detalhe,relator, by = "vaga")
relatores=analise1[,2:30]
rownames(relatores)=analise1[,1]



dados_padron=scale(relatores,center=TRUE,scale=TRUE)
View(dados_padron)

#para alterar qtd de clusters
saida_k_means=kmeans(dados_padron,4,nstart=11)



##### Visualizacao dos agrupamentos (5)

saida_k_means$cluster
relatores1 = which(saida_k_means$cluster==1)
relatores2 = which(saida_k_means$cluster==2)
relatores3 = which(saida_k_means$cluster==3)

#relatores_n1<-relatores[relatores1,]
#relatores_n2<-relatores[relatores2,]
#relatores_n3<-relatores[relatores3,]

View(relatores1)
View(relatores2)
View(relatores3)

##### parametros para analise (6)

saida_k_means$totss
saida_k_means$tot.withinss
saida_k_means$betweenss
saida_k_means$withinss



#####analisar para próximos 6 passos anterior com filtros de data_de_distribuicaos distintos
#distribuicao_filtro <-subset(distribuicao,ano==2010)
#distribuicao_filtro <-subset(distribuicao,ano==2011) 
#distribuicao_filtro <-subset(distribuicao,ano==2012) 
#distribuicao_filtro <-subset(distribuicao,ano==2013) 
#distribuicao_filtro <-subset(distribuicao,ano==2014) 
#distribuicao_filtro <-subset(distribuicao,ano==2015) 
#distribuicao_filtro <-subset(distribuicao,ano==2016) 
#distribuicao_filtro <-subset(distribuicao,ano==2017) 
#distribuicao_filtro <-subset(distribuicao,ano==2018) 


check<-data.frame(table(distribuicao$ano,distribuicao$vaga,distribuicao$origem_detalhe))

names(check)<-c("ano","vaga","estado","qtd_processo")

write.csv2(check,"resumo_estado_ano.csv")

==============================================


#####origem_ata_vs_origem_detalhe

#####teste por ano (0)

distribuicao$origem<-paste0(distribuicao$origem_detalhe," | ",distribuicao$origem_ata)

distribuicao_filtro <- distribuicao %>% 
mutate(as.Date(data)) %>%
filter(`as.Date(data)` > "2017-12-31")

min(as.Date(distribuicao_filtro[,"data"]))
max(as.Date(distribuicao_filtro[,"data"]))

qtd_min<-distribuicao_filtro %>% 
     group_by() %>% 
     summarise(Contagem = length(unique(vaga)))
View(qtd_min)

##### tempo STF (1)

tmp <- tapply(distribuicao_filtro$data, list(distribuicao_filtro$vaga), function(x) {length(unique(x))})
tmp <- data.frame(tmp)
tmp$relator <- rownames(tmp)
names(tmp) <- c("tempo_stf", "vaga")

#### visao relator (2)

relator <- data.frame(table(distribuicao_filtro$vaga))
names(relator) <- c("vaga", "total_processo")
relator <- merge(relator, tmp, by = "vaga")
relator$normalizado <- relator$total_processo/relator$tempo_stf

##### ajuste disposição (3)

proc_relator <- data.frame(table(distribuicao$vaga,distribuicao$origem))
names(proc_relator)<-c("vaga","origem","QTD")
origem<-reshape(proc_relator, v.names = "QTD", idvar = "vaga", timevar="origem",direction="wide") 



##### origem_detalhe (Agrupamento das semelhanças/diferenças entre os objetos de um conjunto de dados) - K-Means (4)

analise1 <- merge(origem,relator, by = "vaga")
relatores=analise1[,2:30]
rownames(relatores)=analise1[,1]



dados_padron=scale(relatores,center=TRUE,scale=TRUE)
View(dados_padron)

#para alterar qtd de clusters
saida_k_means=kmeans(dados_padron,2,nstart=10)



##### Visualizacao dos agrupamentos (5)

saida_k_means$cluster
relatores1 = which(saida_k_means$cluster==1)
relatores2 = which(saida_k_means$cluster==2)
relatores3 = which(saida_k_means$cluster==3)
relatores4 = which(saida_k_means$cluster==4)
relatores5 = which(saida_k_means$cluster==5)

#relatores_n1<-relatores[relatores1,]
#relatores_n2<-relatores[relatores2,]
#relatores_n3<-relatores[relatores3,]

View(relatores1)
View(relatores2)
View(relatores3)
View(relatores4)
View(relatores5)

##### parametros para analise (6)

saida_k_means$totss
saida_k_means$tot.withinss
saida_k_means$betweenss
saida_k_means$withinss


check<-data.frame(table(distribuicao$ano,distribuicao$vaga,distribuicao$origem))
names(check)<-c("ano","vaga","origem","qtd_processo")

write.csv2(check,"resumo_origemvsproc.csv")

#####analisar para próximos 6 passos anterior com filtros de data_de_distribuicaos distintos
#filter(`as.Date(data)` > "2016-12-31" & `as.Date(data)` <"2018-01-01")
#filter(`as.Date(data)` > "2017-12-31")



================================================

#####origem_detalhevsclasse

#####teste por ano (0)

distribuicao_filtro <-subset(distribuicao,ano==2016) 

min(as.Date(distribuicao_filtro[,"data"]))
max(as.Date(distribuicao_filtro[,"data"]))

qtd_min<-distribuicao_filtro %>% 
     group_by() %>% 
     summarise(Contagem = length(unique(vaga)))
View(qtd_min)

##### tempo STF (1)

tmp <- tapply(distribuicao_filtro$data, list(distribuicao_filtro$vaga), function(x) {length(unique(x))})
tmp <- data.frame(tmp)
tmp$relator <- rownames(tmp)
names(tmp) <- c("tempo_stf", "vaga")

#### visao relator (2)

relator <- data.frame(table(distribuicao_filtro$vaga))
names(relator) <- c("vaga", "total_processo")
relator <- merge(relator, tmp, by = "vaga")
relator$normalizado <- relator$total_processo/relator$tempo_stf

##### ajuste disposição (3)

proc_relator <- data.frame(table(distribuicao$vaga,distribuicao$origemvsclasse))
names(proc_relator)<-c("vaga","origemvsclasse","QTD")
origemvsclasse<-reshape(proc_relator, v.names = "QTD", idvar = "vaga", timevar="origemvsclasse",direction="wide") 



##### origem_detalhe (Agrupamento das semelhanças/diferenças entre os objetos de um conjunto de dados) - K-Means (4)

analise1 <- merge(origemvsclasse,relator, by = "vaga")
relatores=analise1[,2:30]
rownames(relatores)=analise1[,1]



dados_padron=scale(relatores,center=TRUE,scale=TRUE)
View(dados_padron)

#para alterar qtd de clusters
saida_k_means=kmeans(dados_padron,4,nstart=11)



##### Visualizacao dos agrupamentos (5)

saida_k_means$cluster
relatores1 = which(saida_k_means$cluster==1)
relatores2 = which(saida_k_means$cluster==2)
relatores3 = which(saida_k_means$cluster==3)
relatores4 = which(saida_k_means$cluster==4)


View(relatores1)
View(relatores2)
View(relatores3)
View(relatores4)


##### parametros para analise (6)

saida_k_means$totss
saida_k_means$tot.withinss
saida_k_means$betweenss
saida_k_means$withinss



#####analisar para próximos 6 passos anterior com filtros de anos distintos
#distribuicao_filtro <-subset(distribuicao,ano==2010)
#distribuicao_filtro <-subset(distribuicao,ano==2011) 
#distribuicao_filtro <-subset(distribuicao,ano==2012) 
#distribuicao_filtro <-subset(distribuicao,ano==2013) 
#distribuicao_filtro <-subset(distribuicao,ano==2014) 
#distribuicao_filtro <-subset(distribuicao,ano==2015) 
#distribuicao_filtro <-subset(distribuicao,ano==2016) 
#distribuicao_filtro <-subset(distribuicao,ano==2017) 
#distribuicao_filtro <-subset(distribuicao,ano==2018) 



check<-data.frame(table(distribuicao$ano,distribuicao$vaga,distribuicao$origemvsclasse))

names(check)<-c("ano","vaga","origemvsclasse","qtd_processo")

write.csv2(check,"resumo_estado_ano.csv")

