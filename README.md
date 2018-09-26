# Aula4-Econometria-Avancada
Raiz Unitária



#Aula 4 - Raizes UnitÃ¡rias

#obs. data na forma de ano-mÃªs-dia  aaaa-mm-dd

install.packages("urca")                                                        #Instala pacote "Urca"
library("urca")                                                                 #Carrega Pacote 
library(readxl)
interdaay <- read_excel("C:/Econometria/interdaay.xls",
                        col_types = c("date", "numeric", "numeric", "numeric")) #Le o arquivo xls "Interday"
colnames(interdaay)[3] <- "variacao"                                            #Altera o nome da terceira coluna para variacao
interdaay <- interdaay[,-1]                                                     #Deleta a primeira coluna
dados_diarios <- ts(interdaay, start = 2017-01-10, frequency = 365)             #Cria a Serie Temporal com as trÃªs variaveis
plot(dados_diarios, col= "blue", main="Dados do Indice Bovespa", xlab="Dias")   #Plota os trÃªs grÃ¡ficos em azul e com as legendas
variacao <- ts(interdaay$variacao, start = 2017-01-10, frequency = 365)               #Cria a serie temporal "variacao" somente da variavel "variacao"
Ibovespa <- ts(interdaay$Ibovespa, start = 2017-01-10, frequency = 365)               #Cria a serie temporal "Ibovespa" somente da variavel "Ibovespa"
Quantidade <- ts(na.omit(interdaay$Quantidade, start = 2017-01-10, frequency = 365))   #Cria a serie temporal "Quantidade" somente da variavel "Quantidade" removendo os valores NAÂ´s
plot(variacao, main="Percentual de VariaÃ§Ã£o")                             #Grafico da variacao, com legendas especificadas
plot(Ibovespa, main="Indice do Dia",col="red")                            #Grafico da Ibovespa, com cores e legendas especificadas
plot(Quantidade, main="Indice do Dia", xlab="Dias", col="blue")           #Grafico da Quantidade, com cores, legendas e eixo "x" especificados

                              #Realizando teste DF-Dick-Fuller para Raiz UnitÃ¡ria

#Para Dados da Variacao do Ibovespa

TesteDF_Variacao_none <- ur.df(variacao, "none",lags = 0)            #Teste DF-DickFuller sem drift e sem tendencia
summary(TesteDF_Variacao_none)                                       #Resumo EstatÃ­stico do Teste
TesteDF_Variacao_drift <- ur.df(variacao, "drift", lags=0)           #Teste DF-DickFuller com drift e sem tendencia
summary(TesteDF_Variacao_drift)                                      #Resumo EstatÃ­stico do Teste
TesteDF_Variacao_trend <- ur.df(variacao, "trend", lags = 0)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Variacao_trend)                                      #Resumo EstatÃ­stico do Teste

                   #Construindo Tabela com Resultados Obtidos

Col1_Variacao <- c(" ",-2.58,"" ,-3.43, -3.43," ", -3.96, -3.96,-3.96)             #Vetor da primeira Coluna
Col2_Variacao <- c(" ", -41.58,"" ,-41.572,0.133," ", -41.578, 0.805,-0.853)       #Vetor da segunda Coluna
Col3_Variacao <- c(" ", "0.000"," ","0.000",0.894," ","0.000",0.421,0.394)         #Vetor da terceira Coluna
Col4_Resultado <- c("", "EstacionÃ¡ria","","EstacionÃ¡ria", "Sem Drift","", 
                    "EstacionÃ¡ria", "Sem Drift", "Sem TendÃªncia" )                 #Vetor da quarta Coluna
Resultado_Variacao <- cbind(Col1_Variacao,Col2_Variacao,Col3_Variacao,Col4_Resultado)    #Transformando os vetores em colunas
colnames(Resultado_Variacao) <- c("T CrÃ­tico 1%", "Estatistica T","P-Value", "Resultado")   #Nomeando as colunas
rownames(Resultado_Variacao) <- c("SEM CONSTANTE E SEM TENDÃNCIA", 
                                  "Yt-1",
                                  "COM CONSTANTE",
                                  "Yt-1", "Drift",
                                  "COM CONSTANTE E COM TENDÃNCIA",
                                  "Yt-1", "Drift","Trend")                               #Nomeando as linhas
View(Resultado_Variacao)
