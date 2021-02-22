Exec 1

pphSimulation <- function(t, L){
   S = 0
   I = 0
   Y = c()
   while(1){
     u = runif(1, 0, 1)
     x = (-1/L)*(log(u))
     S = S + x
     if(S > t){
       return(I)
     }
     I = I+1
     Y[I] = S
   }
}

exerc1_lista4_itemc <- function(n, t, L){
   M <- vector()
   for(i in 1:n){
     h <- pphSimulation(t, L)
     M <- c(M, runif(h, 20, 40))
   }
   return(M)
}
 
x <- exerc1_lista4_itemc(500,1,5)
var(x)

torcedores <- runif(500, 20, 40)
var(torcedores)

Exec 4

n <- 80
s <- rnorm(n, 0, 1)
sn <- var(s)
za <- 0.05
e0 <- 0.01
x <- za*(sqrt(sn)/sqrt(n))
x
s2 <- runif(n, 0, 1)
sn2 <- var(s2)
x2 <- za*(sqrt(sn2)/sqrt(n))
x2
(x <= e0)
(x2 <= e0)

n <- 80
e0 <- 0.1
Lista4_ex4 <- function(n, e0){
	s2 <- rnorm(n, 0, 1)
	e <- sd(s2)/sqrt(n)
	amfinal <- s2
	while (e > e0) {
		n <- n+1
		s2 <- c(s2, rnorm(1, 0, 1))
		e <- sd(s2)/sqrt(n)
		amfinal <- s2
	}
	return(amfinal)
}
x <- Lista4_ex4(n, e0)
length(x)
mean(x)
var(x)
hist(x)
hist(x,prob=T)
curve(dnorm( x),add=T)

Exec 5
	while(sd(v) >= 0.05 && length(v) <= n) {
	while((des >= 0.05) & (length(v) <= n)) {
	while(sd(v) > 0.05) {
		v <- c(v, CalcIntegral_ex2_b(10))
	}
	while(sd(v) > 0.05) {
		v <- c(v, CalcIntegral_ex2_b(10))
	}
Para a integral, h(x) = e^(x^2), com o integrando de 0 até 1. 
 
CalcIntegral_ex2_b <- function(n){
	vetor = runif(n, 0, 1)
	vetor = exp(vetor^2)
	final = sum(vetor)/n
	return(final)
}

Lista4_ex5 <- function(n){
	v <- vector()
	v <- CalcIntegral_ex2_b(100);
	while(length(v) <= n) {
		v <- c(v, CalcIntegral_ex2_b(10))
	}
	while(sd(v) > 0.05) {
		v <- c(v, CalcIntegral_ex2_b(10))
	}
	return(v)	
}
x <- Lista4_ex5(100)
x

Exec 6
int_conf <- function(x, conf = 0.95){
	n <- length(x)
	media <- mean(x)
	variancia <- var(x)
	quantis <- qt(c((1-conf)/2, 1 - (1-conf)/2), df = n-1)
	ic <- media + quantis * sqrt(variancia/n)
	return(ic)
}
am_ini <- c(102, 112, 131, 107, 114, 95, 133, 145, 139, 117, 93, 111, 124, 122, 136, 141, 119, 122, 151, 143)
var(am_ini)
sd(am_ini)
int_conf(am_ini, 0.99)