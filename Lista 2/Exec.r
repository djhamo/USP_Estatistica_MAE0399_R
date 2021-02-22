x <- list(turma="A", notas=c(5,7,3,5,4,6), turma="B", notas=c(2,5,4,3,6), turma="C", notas=c(2,2,2,7,8,3,4))
notas <- c(c(5,7,3,5,4,6), c(2,5,4,3,6), c(2,2,2,7,8,3,4))
turmas <- c("A", "B", "C")
info <- data.frame(turmas, notas)
info2 <- data.frame( notas, turmas)
table(turmas, notas)
x <- list(turma=c("A", "B", "C"), notas=c(c(5,7,3,5,4,6), c(2,5,4,3,6), c(2,2,2,7,8,3,4)))

A <- c(5,7,3,5,4,6)
B <- c(2,5,4,3,6)
C <- c(2,2,2,7,8,3,4)
var_media <- (6*1.67+5*2+7*5.23)/(6+5+7)
mean(A, B,C)
var_media
S <- c(A,B,C)
var_S <- mean((S-mean(S))^2)
Rquadrado <- 1 - var_media/var_S
Rquadrado
[1] 0.0866129



trimestre <- rep(1:8)
y <- c(96, 92, 99, 104, 117, 106,112, 105)
x <- c(20, 20, 25, 25, 30, 30, 35, 35)
z <- c(90, 70, 90, 70, 80, 80, 100, 90)
remedios <- data.frame(Trimestre=trimestre, Vendas=y, Temp_Media=x, Despesas_Prop=z)
plot(remedios$Vendas~remedios$Despesas_Prop, main="Vendas vs Despesas com Propaganda no Semestre", xlab="Despesas com Propaganda", ylab="Vendas")
model1 <- lm(remedios$Vendas~remedios$Despesas_Prop)
abline(model1)
plot(remedios$Vendas~remedios$Temp_Media, main="Vendas vs Temperatura Média no Semestre", xlab="Temperatura Média", ylab="Vendas")
model2 <- lm(remedios$Vendas~remedios$Temp_Media)
abline(model2)

cor(remedios$Vendas, remedios$Despesas_Prop)
cor.test(remedios$Vendas, remedios$Despesas_Prop)

cor(remedios$Vendas, remedios$Temp_Media)
cor.test(remedios$Vendas, remedios$Temp_Media)

library(readxl)
DadosDomi <- read_excel("C:/Users/djham/Google Drive/Universidade/USP/MAE0399/Lista/dados_domiciliosCEA15P02.xlsx")
summary(DadosDomi)


cor(DadosDomi$consumo_per_capita, DadosDomi$REDEGERALDEENERGIAELETRICA)

sm <- 10.0
t = c(0,0)
t[1] = sum((DadosDomi$consumo_per_capita<=sm & DadosDomi$REDEGERALDEENERGIAELETRICA==1) * DadosDomi$consumo_per_capita)
t[2] = sum((DadosDomi$consumo_per_capita<=sm & DadosDomi$REDEGERALDEENERGIAELETRICA==2) * DadosDomi$consumo_per_capita)
t[3] = sum((DadosDomi$consumo_per_capita >sm & DadosDomi$consumo_per_capita<=sm*2  & DadosDomi$REDEGERALDEENERGIAELETRICA==1) * DadosDomi$consumo_per_capita)
t[4] = sum((DadosDomi$consumo_per_capita >sm & DadosDomi$consumo_per_capita<=sm*2 & DadosDomi$REDEGERALDEENERGIAELETRICA==2) * DadosDomi$consumo_per_capita)
t[5] = sum((DadosDomi$consumo_per_capita >sm*2 & DadosDomi$consumo_per_capita<=sm*3 & DadosDomi$REDEGERALDEENERGIAELETRICA==1) * DadosDomi$consumo_per_capita)
t[6] = sum((DadosDomi$consumo_per_capita >sm*2 & DadosDomi$consumo_per_capita<=sm*3 & DadosDomi$REDEGERALDEENERGIAELETRICA==2) * DadosDomi$consumo_per_capita)
t[7] = sum((DadosDomi$consumo_per_capita >sm*3 & DadosDomi$consumo_per_capita<=sm*4 & DadosDomi$REDEGERALDEENERGIAELETRICA==1) * DadosDomi$consumo_per_capita)
t[8] = sum((DadosDomi$consumo_per_capita >sm*3 & DadosDomi$consumo_per_capita<=sm*4 & DadosDomi$REDEGERALDEENERGIAELETRICA==2) * DadosDomi$consumo_per_capita)
t[9] = sum((DadosDomi$consumo_per_capita>(sm*4) & DadosDomi$REDEGERALDEENERGIAELETRICA==1) * DadosDomi$consumo_per_capita)
t[10] = sum((DadosDomi$consumo_per_capita>(sm*4) & DadosDomi$REDEGERALDEENERGIAELETRICA==2) * DadosDomi$consumo_per_capita)

mat <- matrix(t, ncol=2)
colnames(mat) = c('sim', 'não')
rownames(mat) = c('<10', '10 e 20', '20 e 30', '30 e 40', '>50')
chisq.test (mat)


t[1] = sum(()*DadosDomi$consumo_per_capita)
t[2] = sum((DadosDomi$REDEGERALDEENERGIAELETRICA==2)*DadosDomi$consumo_per_capita)
h = data.frame(t)
row.names(h) = c('sim', 'não')
column.names(h) = c('<10', '10 e 20', '20 e 30', '30 e 40', '>50')
names(h) = c('consumo_per_capita')
chisq.test (h)