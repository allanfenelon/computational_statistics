########### CARREGANDO PACOTES
if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)
if(!require(psych))
  install.packages("psych")
library(psych)
source('classicBoxPlot.R', encoding = 'UTF-8')
source('iqv_function.R', encoding = 'UTF-8')

########### CARREGANDO O BD
dados<-read.csv('dadoss.csv', sep = ",", dec = ".")

########### VISUALIZANDO BD
View(dados)
glimpse(dados)
######################## UNIVARIADO ############################################
########### FREQ VAR QUALITATIVA #########
########### FREQ ABSOLUTAS E RELATIVA
dfo=table(dados$Nível.de.escolaridade)  #Ordinal
dfo
prop.table(dfo)  #Ordinal

########### FREQ VAR QUANTITATIVA #########
########### FREQ ABSOLUTAS E RELATIVA
dfa=table(dados$Número.de.filhos) #Discreta
dfa
prop.table(dfa)

########### PLOTANDO OS GRÁFICOS ##########
plot(dados$Nível.de.escolaridade, ylab = "Número de pessoas", xlab= "Nível de escolaridade") #Ordinal
plot(dados$Número.de.filhos, ylab = "Número de filhos", xlab="Número de pessoas") #discreta
plot(dados$Sexo, ylab="Número de pessoas") #nominal
plot(dados$Altura, ylab="Altura", xlab="Número de pessoas") #contínua

################################################################################

########### FREQ VAR CONTÍNUA #########
###verificanco amplitude (var c
sort(dados$Altura)
analisando=summary(dados$Altura)
analisando
mini=min(analisando)
maximo=max(analisando)
h=(maximo-mini)/4 #max-min/sqrt(numero de obs)
table(cut(dados$Altura, seq(mini, maximo, h)))


######################### BIVARIADA ############################################
tb_esc_filhos=table(dados$Nível.de.escolaridade, dados$Número.de.filhos)#dist freq abso
tb_esc_filhos
prop.table(tb_esc_filhos)#dist freq relativa
plot(tb_esc_filhos, ylab="Numero de filhos") #plot grafico

######## MEDIDAS DE POSIÇÃO
tb_disp=c(dados$Nível.de.escolaridade)
sort(tb_disp)
mean(tb_disp) #Media
media=median(tb_disp) #Mediana
media
##### MEDIDAS DE DISPERSÃO
temp <- table(tb_disp) #Gera uma tabela de freq pra obter a moda
temp
names(temp)[temp == max(temp)] #MODA
diff(range(tb_disp)) #AMPLITUDE

q = quantile(x = dados$Número.de.filhos, probs = c(.25, .75), na.rm = TRUE)
AIQ = as.numeric(q[2] - q[1])
print(AIQ)
variancia = var(dados$Número.de.filhos, na.rm=TRUE)
dp = sd(dados$Número.de.filhos, na.rm=TRUE)#sqrt(variancia)
dp
cv = dp/media
cv
iqv(freq = tb_disp, varName = "Idade")

