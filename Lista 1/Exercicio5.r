
notas_alunos <- c(7.3, 5.4, 6.8, 9.5, 5.7, 8.5, 3.5, 2.3, 6.75, 8.7, 9.8, 4.3, 2.5, 7, 6.4, 4.7, 8.8, 9.7, 5.3, 7.7, 1.5, 9.7, 5.8, 6.8)
tam <- length(notas_alunos)
media = sum(notas_alunos) / tam
media
variancia = sum((notas_alunos - media)^2)/ tam
variancia

variancia2 = sum((notas_alunos - media)^2)/ (tam -1)
variancia2
mean(notas_alunos)
var(notas_alunos)
aprovados <- sum(1*(notas_alunos >= 5))
print(aprovados)
reprovados <- sum(1*(notas_alunos < 5))
print(reprovados)
print(tam - aprovados)

#Os Valores da Média e Da variância não são os mesmos pq: São medidas diferentes, a média mostra o valor médio da amostra e a Var mostra a dispersão dos dados em relação a média, inclusive não estão nem na mesma medida.