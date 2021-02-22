Exercicio 4

n <- 10

a <- vector()
for(i in 1:n){
	x = runif(1, 0, 1)
	if (x <= 0.15) {
		a[i] = 2
	} else if (x <= 0.30) {
		a[i] = 4
	} else if (x <= 0.55) {
		a[i] = 1
	} else if (x <= 1) {
		a[i] = 3
	}
}
a

freq <- vector();
for(i in 1:4){
	freq[i] <- sum(1*(a == i))/n
}
freq

n <- 100
CalcSimX <- function(n) {
a <- vector()
for(i in 1:n){
	x = runif(1, 0, 1)
	if (x <= 0.15) {
		a[i] = 2
	} else if (x <= 0.30) {
		a[i] = 4
	} else if (x <= 0.55) {
		a[i] = 1
	} else if (x <= 1) {
		a[i] = 3
	}
}
return (a)
}

CalcFreqRelativa <- function(a, n) {
freq <- vector();
for(i in 1:4){
	freq[i] <- sum(1*(a == i))/n
}
return(freq)
}

Exercicio 5

coincidencia <- function(baralho){
	y <- 0
	for(i in 1:length(baralho)){
		if(baralho[i] == i){
			y[i] <- 1
		}
		else{
			y[i] <- 0
		}
	}
	return(sum(y))
}
n <- 100

simulacao <- function(baralho){
	cartas <- 1:100
	a <- vector()
	for(i in 1:n){
		baralho <- sample(cartas)
		a[i] = coincidencia(baralho)	
	}
	return(a)
}

n <- 100000
a <- simulacao(n)
mean(a)
var(a)


coincidencia <- function(baralho){
	y <- 0
	for(i in 1:length(baralho)){
		if(baralho[i] == i){
			y[i] <- 1
		}
		else{
			y[i] <- 0
		}
	}
	return(sum(y))
}

simulacao <- function(baralho){
	cartas <- 1:100
	a <- vector()
	for(i in 1:n){
		baralho <- sample(cartas)
		a[i] = coincidencia(baralho)
	}
	return(a)
}

Exercicio 6

SimDado <- function() {
	x = runif(1, 0, 1)
	if (x <= 1/6) {
		a[i] = 1
	} else if (x <= 2/6) {
		a[i] = 2
	} else if (x <= 3/6) {
		a[i] = 3
	} else if (x <= 4/6) {
		a[i] = 4
	} else if (x <= 5/6) {
		a[i] = 5
	} else if (x <= 1) {
		a[i] = 6
	}
}

SimTodosNumero <- function(){
	a <- rep(0, 11)
	x <- 0 
	while(sum(a) < 11){
		dado1 <- SimDado()
		dado2 <- SimDado()
		a[(dado1 + dado2) -1] = 1
		x <- x +1
	}
	return(x)
}

simulacao <- function(n){
	a <- vector()
	for(i in 1:n){
		a[i] = SimTodosNumero()
	}
	return(a)
}
