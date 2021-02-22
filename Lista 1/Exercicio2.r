#Exec 3
library(readxl)
dados_domiciliosCEA15P02 <- read_excel("C:/Users/djham/Google Drive/Universidade/USP/MAE0399/Lista/dados_domiciliosCEA15P02.xlsx")
cea <- dados_domiciliosCEA15P02
summary(cea$consumo_per_capita)

#A)
cea$TIPODEDOMICILIO[cea$TIPODEDOMICILIO==1] <- "Casa"
cea$TIPODEDOMICILIO[cea$TIPODEDOMICILIO==2] <- "Apartamento"
cea$TIPODEDOMICILIO[cea$TIPODEDOMICILIO==3] <- "Comodo"
boxplot(cea$consumo_per_capita~cea$TIPODEDOMICILIO, main="Consumo de Gas Anual (KG) por Tipo de Domicilio", xlab="Tipo de Domicílio", ylab="Consumo de Gás Anual KG")



#B)
casa  <- cea$consumo_per_capita[cea$TIPODEDOMICILIO=="Casa"]
apartamento <- cea$consumo_per_capita[cea$TIPODEDOMICILIO=="Apartamento"]
comodo <- cea$consumo_per_capita[cea$TIPODEDOMICILIO=="Comodo"]
summary(casa)
summary(apartamento)
summary(comodo)
//Não sei como montar uma tabela com tudo

#C) Os 2 grupos parecem bem similares tirando os dados discrepantes

#D)
cea$REDEGERALDEENERGIAELETRICA[cea$REDEGERALDEENERGIAELETRICA==1] <- "Sim"
cea$REDEGERALDEENERGIAELETRICA[cea$REDEGERALDEENERGIAELETRICA==2] <- "Nao"

boxplot(cea$consumo_per_capita~cea$REDEGERALDEENERGIAELETRICA, main="Consumo de Gas Anual (KG) por Conexao com a Rede Eletrica", xlab="Conectado a Rede Eletrica", ylab="Consumo de Gás Anual KG")

#E)
hist(cea$consumo_per_capita)
#Não parece uma normal, pq ele não é simétrico

#F)
sm <- 937.0
cea$RENDATOTALMENSALDODOMICILIOAJUST[cea$RENDATOTALMENSALDODOMICILIO<=sm] <- "Até 1 s.m."
cea$RENDATOTALMENSALDODOMICILIOAJUST[cea$RENDATOTALMENSALDODOMICILIO >sm & cea$RENDATOTALMENSALDODOMICILIO<=sm*2] <- "1 a 2 s.m."
cea$RENDATOTALMENSALDODOMICILIOAJUST[cea$RENDATOTALMENSALDODOMICILIO >sm*2 & cea$RENDATOTALMENSALDODOMICILIO<=sm*4] <- "2 a 4 s.m."
cea$RENDATOTALMENSALDODOMICILIOAJUST[cea$RENDATOTALMENSALDODOMICILIO>(sm*4)] <- "mais 4 s.m."

#G)
boxplot(cea$consumo_per_capita~cea$RENDATOTALMENSALDODOMICILIOAJUST, main="Consumo de Gas Anual (KG) por Renda", xlab="Renda Mensal", ylab="Consumo de Gás Anual KG")
summary(cea$RENDATOTALMENSALDODOMICILIO)
