# 2.B

get_prob_of_extinction = function(lambda) {
  if (lambda <= 1){
    return (1)
  }
  n <- 10
  G <- Vectorize(function(s) {sum(s^(0:n)*dpois(0:n, lambda))})
  f <- function(s) (G(s)-s)^2
  return (optimize(f, lower=0, upper=0.999)[[1]])
  
}

# 2.C 

f1 <- function(lambda) {dgamma(lambda, shape = 15, rate = 9)*get_prob_of_extinction(lambda)}

result <- integrate(Vectorize(f1), 0, 10)$value

print(' ---- 2C ---- ')
print('Probability of extinction using numerical integration:')
print(result)

# 2.D

sim_extinction <- function() {
  lambda <- rgamma(1, 15, 9)
    
  a <- dpois(0:10,lambda)
  maxchildren <- length(a)-1
  n <- 100
  
  population <- 1
  for (i in 2:n) {
    children <- sample(0:maxchildren, population, prob=a, TRUE)
    population <- sum(children)
    if (population == 0) {
      return(0)
    }
    if (population > 100) {
      return(1)
    }
    
  }
  return(1)
}

sim_result <- replicate(1000, sim_extinction())

prob_of_extinction <- (1-mean(sim_result))

print(' ---- 2D ---- ')
print('Simulated probability of extinction, 100 generations, simulated 1000 times:')
print(prob_of_extinction)

# 2.E

f <- function(lambda) -dgamma(lambda, shape = 15, rate = 9)
lambda <- optimize(f, lower = 0, upper = 10)[[1]]

print(' ---- 2E ---- ')
print('Maximum likelihood estimate for lambda:')
print(lambda)

ext_prob <- get_prob_of_extinction(lambda)

print('Probability of extinction using maximum likelihood estimate for lambda:')
print(ext_prob)

