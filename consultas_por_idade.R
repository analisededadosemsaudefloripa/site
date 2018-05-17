library(readxl)
library(tidyverse)
library(reshape2)
library(plotly)
library(plyr)
library(readr)

consultas <- read_csv("C:/Users/hp1806/Google Drive/RStudio/consulta_por_idade_aps/consultas_idade_unidade.csv")


pop_fpolis <- read_delim("C:/Users/hp1806/Google Drive/RStudio/consulta_por_idade_aps/pop_fpolis.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)

agregado <- consultas[, c(2,4:8)] 
agregado <- group_by(agregado, UNIDADE,IDADE) %>% 
    summarise_all(funs(sum))

idade <- agregado[,-1]
idade <- aggregate(idade[,-1], by = idade[,1], FUN = sum)

idade$FAIXA <- NA
idade$FAIXA[which(idade$IDADE <= 4)] <- "0 a 4 anos"
idade$FAIXA[which(idade$IDADE >= 5 & idade$IDADE <= 9)] <- "05 a 9 anos"
idade$FAIXA[which(idade$IDADE >= 10 & idade$IDADE <= 14)] <- "10 a 14 anos"
idade$FAIXA[which(idade$IDADE >= 15 & idade$IDADE <= 19)] <- "15 a 19 anos"
idade$FAIXA[which(idade$IDADE >= 20 & idade$IDADE <= 24)] <- "20 a 24 anos"
idade$FAIXA[which(idade$IDADE >= 25 & idade$IDADE <= 29)] <- "25 a 29 anos"
idade$FAIXA[which(idade$IDADE >= 30 & idade$IDADE <= 34)] <- "30 a 34 anos"
idade$FAIXA[which(idade$IDADE >= 35 & idade$IDADE <= 39)] <- "35 a 39 anos"
idade$FAIXA[which(idade$IDADE >= 40 & idade$IDADE <= 44)] <- "40 a 44 anos"
idade$FAIXA[which(idade$IDADE >= 45 & idade$IDADE <= 49)] <- "45 a 49 anos"
idade$FAIXA[which(idade$IDADE >= 50 & idade$IDADE <= 54)] <- "50 a 54 anos"
idade$FAIXA[which(idade$IDADE >= 55 & idade$IDADE <= 59)] <- "55 a 59 anos"
idade$FAIXA[which(idade$IDADE >= 60 & idade$IDADE <= 64)] <- "60 a 64 anos"
idade$FAIXA[which(idade$IDADE >= 65 & idade$IDADE <= 69)] <- "65 a 69 anos"
idade$FAIXA[which(idade$IDADE >= 70 & idade$IDADE <= 74)] <- "70 a 74 anos"
idade$FAIXA[which(idade$IDADE >= 75 & idade$IDADE <= 79)] <- "75 a 79 anos"
idade$FAIXA[which(idade$IDADE >= 80)] <- "80 anos ou mais"

idade_faixa <- idade[,-1]
idade_faixa$FAIXA <- as.factor(idade_faixa$FAIXA)
idade_faixa <- aggregate(idade_faixa[,-5], by = list(idade_faixa$FAIXA), FUN = sum)
names(idade_faixa)[1] <- "FAIXA"

#################################################################################
#Total de Usuários por Idade
#################################################################################
idade_usuario <- idade[,c(1,2,3)]
idade_usuario <- melt(idade_usuario, id = c("IDADE"))
names(idade_usuario)<- c("IDADE", "VARIAVEL", "USUARIOS")

ggplot(idade_usuario, aes(x=IDADE, y=USUARIOS, col = VARIAVEL))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,120,10))+
        scale_y_continuous(breaks = seq(0,17000,1000))+
        theme_bw()

idade_usuario1 <- idade[,c(1,2,3)]
idade_usuario1$USU_HOMEM <- -1*idade_usuario1$USU_HOMEM
idade_usuario1 <- melt(idade_usuario1, id = c("IDADE"))
names(idade_usuario1)<- c("IDADE", "VARIAVEL", "USUARIOS")


idade_usuario1$VARIAVEL <- as.character(idade_usuario$VARIAVEL)
ggplot(idade_usuario1, aes(x = IDADE, y = USUARIOS, fill = VARIAVEL)) + 
  geom_bar(subset = .(VARIAVEL == "USU_HOMEM"), stat = "identity") + 
  geom_bar(subset = .(VARIAVEL == "USU_MULHER"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-20000, 20000, 5000), 
                     labels = paste0(as.character(c(seq(20, 0, -5), seq(5, 20, 5))), "m")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()+
  ylab("Milhares de Usuários")+
  xlab("Idade")


#################################################################################
#Total de Usuários por Faixa Etária
#################################################################################
#idade_faixa$USU_HOMEM <- idade_faixa$USU_HOMEM/5 #fazendo a média, pois são dados de 5 anos (2013-17)
#idade_faixa$USU_MULHER <- idade_faixa$USU_MULHER/5 #fazendo a média, pois são dados de 5 anos (2013-17)
#idade_faixa$CONS_HOMEM <- idade_faixa$CONS_HOMEM/5 #fazendo a média, pois são dados de 5 anos (2013-17)
#idade_faixa$CONS_MULHER <- idade_faixa$CONS_MULHER/5 #fazendo a média, pois são dados de 5 anos (2013-17)

idade_faixa <- cbind(idade_faixa, pop_fpolis)
idade_faixa <- idade_faixa[,-c(4,5,6,9)]
idade_faixa$USU_HOMEM <- -1*idade_faixa$USU_HOMEM
idade_faixa$Masculino <- -1*idade_faixa$Masculino
idade_faixa$Masculino_DIF <- idade_faixa$Masculino - idade_faixa$USU_HOMEM
idade_faixa$Feminino_DIF <- idade_faixa$Feminino - idade_faixa$USU_MULHER
idade_faixa1 <- idade_faixa[,-c(4,5)]
idade_faixa1 <- melt(idade_faixa1, id = c("FAIXA"))
names(idade_faixa1)<- c("FAIXA", "VARIAVEL", "USUARIOS")
idade_faixa <- melt(idade_faixa, id = c("FAIXA"))
names(idade_faixa)<- c("FAIXA", "VARIAVEL", "USUARIOS")


idade_faixa1$VARIAVEL <- as.character(idade_faixa1$VARIAVEL)
ggplot(idade_faixa1, aes(x = FAIXA, y = USUARIOS, fill = VARIAVEL)) + 
  geom_bar(subset = .(VARIAVEL == "USU_HOMEM"), stat = "identity") +
  geom_bar(subset = .(VARIAVEL == "Masculino_DIFF"), stat = "identity") +
  geom_bar(subset = .(VARIAVEL == "USU_MULHER"), stat = "identity") +
  geom_bar(subset = .(VARIAVEL == "Feminino_DIFF"), stat = "identity") +
  scale_y_continuous(breaks = seq(-50000, 50000, 10000), 
                     labels = paste0(as.character(c(seq(50, 0, -10), seq(10, 50, 10))), "m")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Reds") + 
  theme_bw()+
  ggtitle("Piramide Etária - Florianópolis vs Usuários")+
  ylab("Milhares de pessoas")+
  xlab(" ")

idade_faixa_largo <- dcast(idade_faixa, FAIXA ~ VARIAVEL, value.var = "USUARIOS")
idade_faixa_largo$PERC_HOMEM <- idade_faixa_largo$USU_HOMEM/idade_faixa_largo$Masculino *100
idade_faixa_largo$PERC_MULHER <- idade_faixa_largo$USU_MULHER/idade_faixa_largo$Feminino *100
idade_faixa_largo <- idade_faixa_largo[,c(1,8,9)]
idade_faixa_largo$PERC_HOMEM <- -1*idade_faixa_largo$PERC_HOMEM
idade_faixa_largo <- melt(idade_faixa_largo, id = c("FAIXA"))
names(idade_faixa_largo)<- c("FAIXA", "VARIAVEL", "PERCENTUAL")

ggplot(idade_faixa_largo, aes(x = FAIXA, y = PERCENTUAL, fill = VARIAVEL)) + 
  geom_bar(subset = .(VARIAVEL == "PERC_HOMEM"), stat = "identity") +
  geom_bar(subset = .(VARIAVEL == "PERC_MULHER"), stat = "identity") +
  scale_y_continuous(breaks = seq(-100, 100, 10), 
                     labels = paste0(as.character(c(seq(100, 0, -10), seq(10, 100, 10))), "%")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Reds") + 
  theme_bw()+
  ggtitle("Percentual de utilização por faixa etária e sexo")+
  ylab(" ")+
  xlab(" ")


#################################################################################
#Total de Consultas por Idade
#################################################################################
idade_consulta <- idade[,c(1,4,5)]
idade_consulta <- melt(idade_consulta, id = c("IDADE"))
names(idade_consulta)<- c("IDADE", "VARIAVEL", "CONSULTAS")

ggplot(idade_consulta, aes(x=IDADE, y=CONSULTAS, col = VARIAVEL))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,120,10))+
        scale_y_continuous(breaks = seq(0,65000,5000))+
        theme_bw()


idade_consulta1 <- idade[,c(1,4,5)]
idade_consulta1$CONS_HOMEM <- -1*idade_consulta1$CONS_HOMEM
idade_consulta1 <- melt(idade_consulta1, id = c("IDADE"))
names(idade_consulta1)<- c("IDADE", "VARIAVEL", "CONSULTAS")

idade_consulta1$VARIAVEL <- as.character(idade_consulta$VARIAVEL)
ggplot(idade_consulta1, aes(x = IDADE, y = CONSULTAS, fill = VARIAVEL)) + 
  geom_bar(subset = .(VARIAVEL == "CONS_HOMEM"), stat = "identity") + 
  geom_bar(subset = .(VARIAVEL == "CONS_MULHER"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-60000, 60000, 5000), 
                     labels = paste0(as.character(c(seq(60, 0, -5), seq(5, 60, 5))), "m")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()



#################################################################################
#Taxa de Consulta por Idade
#################################################################################
idade$TAXA_HOMEM <- idade$CONS_HOMEM/idade$USU_HOMEM
idade$TAXA_MULHER <- idade$CONS_MULHER/idade$USU_MULHER 

idade_taxa <- idade[,c(1,6,7)]
idade_taxa <- melt(idade_taxa, id = c("IDADE"))
names(idade_taxa)<- c("IDADE", "VARIAVEL", "TAXA")

ggplot(idade_taxa, aes(x=IDADE, y=TAXA, col = VARIAVEL))+
        geom_line()+
        scale_x_continuous(breaks = seq(0,120,10))+
        scale_y_continuous(breaks = seq(0,10,1))+
        theme_bw()


#################################################################################
#Total de Usuários por Idade por unidade
#################################################################################

idade_usuario_cs <- agregado[,c(1,2,3,4)]
idade_usuario_cs <- melt(idade_usuario_cs, id = c("UNIDADE", "IDADE"))
names(idade_usuario_cs)<- c("UNIDADE", "IDADE", "VARIAVEL", "USUARIOS")
idade_usuario_cs <- na.omit(idade_usuario_cs)

a<-ggplot(idade_usuario_cs, aes(x = reorder(as.factor(UNIDADE), IDADE, FUN = median), y=IDADE, col = as.factor(UNIDADE)))+
        geom_boxplot(aes(fill = USUARIOS))+
        theme_bw()+
        coord_flip()
ggplotly(a) 

idade_usuario_cs$UNIDADE <- as.factor(idade_usuario_cs$UNIDADE)
idade_usuario_cs$IDADE <- as.factor(idade_usuario_cs$IDADE)

ggplot(idade_usuario_cs, aes(x = UNIDADE , y =IDADE ))+
        geom_dotplot(binaxis='y', stackdir='center', dotsize=0.001)+
        theme_bw()+
        coord_flip()



