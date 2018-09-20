
library(ggplot2)  
library(tidyverse) 
library(janitor) 
library(formattable) 
library(haven)
library(stringi)
library(readxl)
library("dplyr")

#### Pacotes#
arquivo <- read.csv("processo-andamento-2017-12-04.csv")

### Arrumar codificação de caracteres
arquivo$relator <- as.character(arquivo$relator)
Encoding(arquivo$relator) <- "UTF-8"
arquivo$classe_processo <- as.character(arquivo$classe_processo)
Encoding(arquivo$classe_processo) <- "UTF-8"
arquivo$procedencia <- as.character(arquivo$procedencia)
Encoding(arquivo$procedencia) <- "UTF-8"
arquivo$origem <- as.character(arquivo$origem)
Encoding(arquivo$origem) <- "UTF-8"
arquivo$andamentos <- as.character(arquivo$andamentos)
Encoding(arquivo$andamentos) <- "UTF-8"

### limpeza de arquivo

# 1 criar ID unica

arquivo$id_proc <- paste0(arquivo$codigo_classe_processo, arquivo$numero_processo) 

arquivo$recurso <- NULL

arquivo <- subset(arquivo, arquivo$relator != "relator") # exlcuir erro head

arquivo <- arquivo %>% 
mutate(as.Date(data)) %>%
filter(`as.Date(data)` > "2009-12-31" & `as.Date(data)` <"2017-06-02")


#min(as.Date(arquivo[,"data"]))


#max(as.Date(arquivo[,"data"]))

prevento <- subset(arquivo, !grepl("preven",arquivo$andamentos)==FALSE) # somente prevento

arquivo <- subset(arquivo, !grepl("preven",arquivo$andamentos)==TRUE) # exlcuir prevento

prevento_unique <- prevento [!(duplicated(prevento[c("id_proc")]) | duplicated(prevento[c("id_proc")],fromLast = FALSE)), ]

prevento_unique<-data.frame(prevento_unique$id_proc,prevento_unique$relator)

names(prevento_unique)<-c("id_proc","relator")

arquivo_unique <- arquivo [!(duplicated(arquivo[c("id_proc")]) | duplicated(arquivo[c("id_proc")],fromLast = FALSE)), ]

arquivo_unique<-data.frame(arquivo_unique$id_proc,arquivo_unique$relator)

names(arquivo_unique)<-c("id_proc","relator")






=====================

###bases monocraticas
anos <- 2017:2018
url <- "http://www.stf.jus.br/arquivo/cms/publicacaoBOInternet/anexo/decisoes/monocraticas/"
lapply(paste0(url,"decisoes_monocraticas_lista_", anos,".xlsx"), function(x) download.file(x, basename(x)))
baixados <- list.files(pattern = '.xlsx') # cria lista dos arquivos baixados
mono <- do.call("rbind",lapply(baixados,FUN=function(files){read_excel(files,skip=5)})) #importa


mono$X__4 <- NULL
mono$X__7 <- NULL
mono <- subset(mono, mono$X__2 != "")

names(mono)<-c("Classe","Numero","Link","Data_Autuacao","Relator_Atual","Classificacao_STF","Tipo_Decisao","Orgao_Julgador","Data_Andamento","Andamento","Observacao_Andamento","Assuntos")

mono$Relator_Atual<-str_replace(mono$Relator_Atual, "MIN. ", "")

mono$Orgao_Julgador<-str_replace(mono$Orgao_Julgador, "MIN. ", "")

# 1 criar ID unica

mono$id_proc <- paste0(mono$Classe, mono$Numero)

# filtro data
mono <- mono %>% 
mutate(Data_Autuacao) %>%
filter(`Data_Autuacao` > "2009-12-31" & `Data_Autuacao` <"2017-06-01")

#####cruza arquivos


mono_prevento <- merge(mono, prevento_unique, by ="id_proc",all.x=TRUE)

monocratica_tratada <- merge(mono_prevento, arquivo_unique, by ="id_proc",all.x=TRUE)


monocratica_tratada$preven<- {ifelse(!grepl("",monocratica_tratada$relator.x)==FALSE,"sim","nao")}

monocratica_tratada$check<- {ifelse(!grepl("",monocratica_tratada$relator.y)==FALSE,"sim","nao")}

monocratica_tratada$check_dist<- {ifelse(monocratica_tratada$preven=="nao" & monocratica_tratada$check=="nao","nao","sim")}


monocratica_tratada$check<-NULL
monocratica_tratada$Observacao_Andamento<-NULL
monocratica_tratada$Assuntos<-NULL

names(monocratica_tratada)<-c("id_proc","Classe","Numero","Link","Data_Autuacao","Relator_Atual","Classificacao_STF","Tipo_Decisao","Orgao_Julgador","Data_Andamento","Andamento","primeiro_relator_preven","primeiro_relator_dist","id_preven","id_dist")

write.csv2(monocratica_tratada,"C:/Users/Juliana/Desktop/Atual/STF/monocrática/monocratica_anual.csv")


#####saidas2018

ano2018 <- read_excel("monocrática/anual/decisoes_monocraticas_lista_2018.xlsx",col_names = FALSE, col_types = c("text","numeric", "text", "numeric", "date","text", "numeric", "text", "text","text", "date", "text", "text", "text"))


ano2018$X__4 <- NULL
ano2018$X__7 <- NULL
ano2018 <- subset(ano2018, ano2018$X__2 != "")


names(ano2018)<-c("Classe","Numero","Link","Data_Autuacao","Relator_Atual","Classificacao_STF","Tipo_Decisao","Orgao_Julgador","Data_Andamento","Andamento","Observacao_Andamento","Assuntos")

ano2018$Relator_Atual<-str_replace(ano2018$Relator_Atual, "MIN. ", "")

ano2018$Orgao_Julgador<-str_replace(ano2018$Orgao_Julgador, "MIN. ", "")

# 1 criar ID unica

ano2018$id_proc <- paste0(ano2018$Classe, ano2018$Numero)

ano2018$Observacao_Andamento<-NULL
ano2018$Assuntos<-NULL

write.csv2(ano2018,"C:/Users/Juliana/Desktop/Atual/STF/monocrática/monocratica_anual_2018.csv")



#####distribuição vs monocratica

mono_filtro <- subset(monocratica_tratada, monocratica_tratada$Tipo_Decisao=="MONOCRÁTICA" & monocratica_tratada$id_preven=="nao" &  monocratica_tratada$id_dist=="sim")

mono_filtro$relator<-mono_filtro$Relator_Atual
mono_filtro$classe<-mono_filtro$Classe

tmp<- mono_filtro %>% 
  group_by(relator) %>% 
  summarise(qtd_dias_mono = length(unique(Data_Autuacao)))

tmp2 <- data.frame(table(mono_filtro$relator,mono_filtro$classe))
names(tmp2)<-c("relator","classe","qtd_mono")

mono_filtro2 <- merge(tmp2, tmp, by = "relator")

mono_filtro2$id<- paste0(mono_filtro2$relator, mono_filtro2$classe)


tmp<- arquivo %>% 
  group_by(relator) %>% 
  summarise(qtd_dias_dist = length(unique(data)))

names(tmp)<-c("relator","qtd_dias_dist")

tmp2 <- data.frame(table(arquivo$relator,arquivo$codigo_classe_processo))

names(tmp2)<-c("relator","classe","qtd_dist")


dist_filtro2 <- merge(tmp2, tmp, by = "relator")

dist_filtro2$id<- paste0(dist_filtro2$relator, dist_filtro2$classe)

dist_vs_mono <- merge(dist_filtro2, mono_filtro2, by ="id",all.x=TRUE)

dist_vs_mono<-data.frame(dist_vs_mono$relator.x,dist_vs_mono$classe.x,dist_vs_mono$qtd_dist,dist_vs_mono$qtd_dias_dist,dist_vs_mono$qtd_mono,dist_vs_mono$qtd_dias_mono)

names(dist_vs_mono)<-c("relator","classe","qtd_dist","qtd_dias_dist","qtd_mono","qtd_dias_mono")


write.csv(dist_vs_mono,"C:/Users/Juliana/Desktop/Atual/STF/monocrática/monocratica_comparacao2.csv")


#####correlacao entre dist e decisoes monocraticas

dist_end_2<- dist_filtro2 %>% 
  group_by(relator) %>% 
  summarise(qtd_dist = sum(qtd_dist),qtd_dias_dist=max(qtd_dias_dist))

dist_end_2$media_dist<-dist_end_2$qtd_dist/dist_end_2$qtd_dias_dist

mono_end_2<- mono_filtro2 %>% 
  group_by(relator) %>% 
  summarise(qtd_mono = sum(qtd_mono),qtd_dias_mono=max(qtd_dias_mono))


mono_end_2$media_mono<-mono_end_2$qtd_mono/mono_end_2$qtd_dias_mono

table2 <- merge(dist_end_2, mono_end_2, by ="relator",all.x=TRUE)

write.csv2(table2,"C:/Users/Juliana/Desktop/Atual/STF/monocrática/resumo_relator.csv")

cor(x=table2$media_dist, y=table2$media_mono) 

#baixa correlacao [1] 0.3696738

ggplot(table2, aes(x=media_dist, y=media_mono)) +  geom_point()+  geom_smooth(method=lm)


#####testes chi-quadrado

ggplot(mono_filtro , aes(relator)) + stat_count(width = 0.5) +  facet_wrap(~Classificacao_STF, nrow=14, ncol=2)

m<-table(mono_filtro$relator,mono_filtro$classe)
chi<-(m)
chi<-chisq.test(m)
chi

#n rejeita a hip de dist iguais























