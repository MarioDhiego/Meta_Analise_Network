
############## META-ANALISE NETWORK #########################################################################

####### Definicaode Meta-Analie ########################################################################################################################################################
# A metanalise e uma tecnica estatistica utilizada para combinar resultados provenientes de diferentes estudos. 
# Para isto, sao descritos as medidas de efeito utilizadas em metana-lise na area da sao de, bem como os modelos de efeitos fixo e de efeitos aleatorios. 
# O objetivo deste capitulo e apresentar de forma metodologica como realizar e interpretar uma metanalise nas pesquisas clinicas.  
##########################################################################################################################################################################


####### ETAPAS DA META-ANALISE ##########################################################################################################################################################
# 1)	Definir Claramente a Questao Problema (objetivo do Trabalho);
# 2)	Buscar Trabalhos confiaveis em diversas base de dados;
# 3)	Criar criterios de inclusao e de exclusao na base;
# 4)	Selecionar os Trabalhos;
# 5)	Avaliar a Heterogeneidade;
# 6)	Calcular os Resultados de cada estudo, Combinando-os;
# 7)	Avaliar o efeito da Variacao;
# 8)	Interpretar os Resultados; 
##############################################################################################################

############## INSTALACAO DE PACOTES ###########################################################################

install.packages(c("readxl", "meta", "netmeta", "readxlsb", "rgl", "dmetar"))
##############################################################################################################


############## CARREGAR PACOTES ####################################################################
library(meta)
library(netmeta)
library(readxl)
library(readxlsb)
library(rgl)
library(dmetar)  
######################################################################################################

############### DEFINIR DIRETORIO DE TRABALHO #########################################################
setwd("C:/Users/mario Dhiego/Documents/Meta_Network/Dados Meta_Tretament_time")
getwd()
######################################################################################################


############### LEITURA DA BASE DE DADOS #############################################################
Meta_NetWork_TreatmentTime_ajustado <- read_excel("Meta_NetWork_TreatmentTime_ajustado.xlsx")

######################################################################################################

############## VISUALIZAR A BASE DE DADOS COMPLETA ##################################################
view(Meta_NetWork_TreatmentTime_ajustado)
head(Meta_NetWork_TreatmentTime)
#####################################################################################################

##################### PARAMETROS DO MODELO ##################################################################
# 1)  n_i     : numero de pacientes do grupo experimental(tratamento)
# 2)  mean_i  : media do grupo experimental  
# 3)  sd_i    : desvio-padrao do grupo experimental
# 4)  n_c     : numero de pacientes do grupo controle
# 5)  mean_c  : media do grupo controle
# 6)  sd_c    : desvio-padrao do grupo controle
# 7)  TE      : diferença média entre os efeitos dos tratamentos
# 8)  seTE    : diferença média dos erros-padrão entre os tratamentos 
# 9)  studlab : autores
####################################################################################################################



######### META-ANALISE POR PARES/BRACOS #########################################################################
#Treatment time 
Meta_NetWork_TreatmentTime_ajustado <- pairwise(list(treat1,treat2),
                                                n=list(n_i,n_c),
                                                mean=list(media_i,media_c),
                                                sd=list(dp_i,dp_c),
                                                studlab= study, 
                                                data= Meta_NetWork_TreatmentTime_ajustado)
Meta_NetWork_TreatmentTime_ajustado
summary(Meta_NetWork_TreatmentTime_ajustado)
###########################################################################################################



############# Salvar os Resultados da Pairwise #############################################################
View(Meta_NetWork_TreatmentTime_ajustado)
write.csv(Meta_NetWork_TreatmentTime_ajustado, "Meta_NetWork_TreatmentTime_ajustado.csv", row.names = F)
###########################################################################################################


##########################################################################################################
# Modelo de Meta-Analise Geral
Meta_Geral <- metagen(TE,seTE, 
                      studlab, 
                      data= Meta_NetWork_TreatmentTime_ajustado, 
                      sm="MD")


# Resultados
Meta_Geral


# Grafico de Floresta Geral
forest.meta(Meta_Geral, comb.fixed = F)


# Grafico de Funil Geral
funnel.meta(Meta_Geral, comb.fixed = F)
##########################################################################################################


############ META-ANALISE P/ PARES ########################################################################################
##########################################################################################################
# Modelo de Meta-Analise - Minimal Presurgical

# Tratamento p/ Minimal Presurgical
Minimal_Presurgical <- subset(Meta_NetWork_TreatmentTime_ajustado, treat1 == "minimalpre")


# Modelo de Meta-Analise - Minimal Presurgical
Minimal_Presurgical <- metagen(TE, seTE, 
                               studlab, 
                               data = Minimal_Presurgical, 
                               sm = "MD")

# Resultados
Minimal_Presurgical

# Grafico de Floresta Minimal Presurgical
forest.meta(Minimal_Presurgical, comb.fixed = F)


# Grafico de Funil Minimal Presurgical
funnel.meta(Minimal_Presurgical, comb.fixed = F)


##########################################################################################################

##########################################################################################################
# Modelo de Meta-Analise - Surgery First

# Tratamento p/ Surgery First
Surgery_First <- subset(Meta_NetWork_TreatmentTime_ajustado, treat1 == "sfa")


# Modelo de Meta-Analise - Surgery First
Surgery_First <- metagen(TE, seTE, 
                         studlab, 
                         data = Surgery_First, 
                         sm = "MD")


# Resultados
Surgery_First

# Grafico de Floresta Surgery First
forest.meta(Surgery_First, comb.fixed = F)

# Grafico de Funil Surgery First
funnel.meta(Surgery_First, comb.fixed = F)

##########################################################################################################




##########################################################################################################
############# Meta-Analise em REDE ########################################################################

############# Modelo em Rede ############################################################################
meta_treatment_time <- netmeta(TE, seTE, 
                               treat1, treat2, 
                               studlab, 
                               data = Meta_NetWork_TreatmentTime_ajustado, 
                               sm="MD", comb.fixed = F,
                               reference.group = "convsurg") 

# Resultados da Rede
meta_treatment_time

# Grafico em Rede
netgraph.netmeta(meta_treatment_time,seq=c("sfa","minimalpre","convsurg"),
                 col = "blue")

# Grafico 3d
netgraph.netmeta(meta_treatment_time, dim = "3d")


# Graico de Calor Liquido
netheat(meta_treatment_time, nchar.trts = 4)
netheat(meta_treatment_time, 
        nchar.trts = 4,
        random = TRUE)

# Divisao Liquida
netsplit(meta_treatment_time)
forest(netsplit(meta_treatment_time))



#########################################################################################################



############# Mostrar os Resultados ######################################################################
meta_treatment_time
##########################################################################################################


############## Salvar os Resultados #######################################################################
sink("meta_treatment_time.txt") 
###########################################################################################################





netrank(meta_treatment_time, small.values = "good") # Ranking

sink(file = NULL) # Export results

############### Gerar Tabela de Resultados #############################################################
meta_treatment_time_league <- netleague(meta_treatment_time, comb.fixed = F, 
                                        digits = 2, bracket = "(", separator = " to",
                                        seq = netrank(meta_treatment_time)) 

# Resultados
meta_treatment_time_league
########################################################################################################

# Salvar
write.table(meta_treatment_time_league$random, file = "meta_treatment_time_league.csv",
            row.names = FALSE, col.names = FALSE,
            sep = ",") # Export league table
########################################################################################################

