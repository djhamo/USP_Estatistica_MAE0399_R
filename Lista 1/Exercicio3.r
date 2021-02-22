#Exercício 3
#A)
A <- c(5,7,3,5,4,6)
plot(ecdf(A), main="Turma A: frequência acumulada das notas", xlab="Notas", ylab="Frequência acumulada")
plot(sort(unique(A)),(cumsum(table(A))-0.5)/length(table(A)), main="Turma A: função acumulada empírica", xlab="Notas", ylab=expression('F'[E](x[i])), type="o")
#B)
B <- c(2,5,4,3,6)
qqplot(A, B, main="Q-Q Plot: Turma A vs Turma B", xlab="Notas Turma A", ylab="Notas Turma B")⁠⁠⁠⁠