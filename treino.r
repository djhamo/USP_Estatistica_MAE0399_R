20174391816060

norm_sim <- function(n) {
	x <- vector()
	for(i in 1:n) {
		u <- runif(1, 0, 1)
		m <- runif(1, 0, 1)
		if (m < 0.5) 
			h <- (1/sqrt(2*pi))*exp(-u^2 /2)
		else
			h <- (1/sqrt(2*pi))*exp(-u^2 /2)*-1
			
		x[i] <- h
	}
	result <- sum(x)/n
	return(result)
}

norm_sim3 <- function(n) {
	x <- vector()
	for(i in 1:(n/2)) {
		u1 <- runif(1, 0, 1)
		u2 <- runif(1, 0, 1)
		h = sqrt(−2 * log(u1)) * sin(2 * pi * u2)
		z = sqrt(−2 * log(u1)) * cos(2 * pi * u2)
		x[i] <- h
		x[i+1] <- z
	}
	result <- sum(x)/n
	return(result)
}


simulator <- function(n) {
	x <- vector()
	for(i in 1:n) {
		x[i] <- norm_sim3(1000)
	}
	return(x)
}

x <- simulator(1000)
hist(x)

y <- norm_sim2(1000, 0.12)
y