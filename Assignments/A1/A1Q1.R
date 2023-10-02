lambda_max = 8
# 1.A
x <- seq(0, lambda_max, by = 0.01)
plot(x, xlab="lambda", ylab="probability", dgamma(x, shape = 21, rate = 6), main = "Posterior distr. for lambda given prior proportional to 1/lambda")

# 1.B
x <- seq(0, 16, by = 1)
plot(x, xlab="number of requests", ylab="probability", dnbinom(x, size = 21, prob = 6 / 7), main = "Predictive distr. for requests in 7th hour")

# 1.C

k <- 800
lambdas <- seq(0.001, lambda_max, length.out = k)
prior <- 1 / lambdas
placeholder_list <- seq(1, 1, length.out = k)
for (d in c(2, 6, 3, 4, 3, 3)) {
    likelihood_i <- dpois(d, lambdas)
    placeholder_list <- placeholder_list * likelihood_i
}
likelihood <- placeholder_list
posterior_distr <- prior * likelihood / sum(prior * likelihood) * k / lambda_max # No need to divide a by its sum before this step
plot(lambdas, posterior_distr, xlab="lambda", ylab="probability", main = "Posterior distr. for lambda using discretization")

# 1.D

k <- 800
lambdas <- seq(0.001, lambda_max, length.out = k)
prior <- dnorm(lambdas, mean = 2.3, sd = 1.7)
placeholder_list <- seq(1, 1, length.out = k)
for (d in c(2, 6, 3, 4, 3, 3)) {
    likelihood_i <- dpois(d, lambdas)
    placeholder_list <- placeholder_list * likelihood_i
}
posterior_distr <- prior * placeholder_list / sum(prior * placeholder_list) * k / lambda_max # No need to divide a by its sum before this step
plot(lambdas, posterior_distr, xlab="lambda", ylab="probability", main = "Posterior distr. for lambda using normally distr. prior")

# 1.E


f1 <- function(lambda) {
    dpois(3, lambda) * dpois(2, lambda) * dpois(6, lambda) * dpois(3, lambda) * dpois(4, lambda) * dpois(3, lambda) * dpois(3, lambda) * dnorm(lambda, mean = 2.3, sd = 1.7)
}
f2 <- function(lambda) {
    dpois(2, lambda) * dpois(6, lambda) * dpois(3, lambda) * dpois(4, lambda) * dpois(3, lambda) * dpois(3, lambda) * dnorm(lambda, mean = 2.3, sd = 1.7)
}

numerator <- integrate(Vectorize(f1), 0, lambda_max)$value
denominator <- integrate(Vectorize(f2), 0, lambda_max)$value
result <- numerator / denominator
print("Predictive probability for receiving exactly 3 reqesust the 7th hour:")
print(result)

# 1.F


sampled_lambdas <- sample(lambdas, size=10000, replace = TRUE, prob=posterior_distr)
integer_sequence <- seq(0, 20)

requests = list()
for (lambda in sampled_lambdas){
    sample_prob <- dpois(integer_sequence, lambda)
    request_sample = sample(integer_sequence, size=1, replace = TRUE, prob=sample_prob)
    requests <- append(requests, request_sample)
}

hist(unlist(requests), xlab="Number of requests", ylab="Frequency", main="Predictive distr. for number of requests in 7th hour")
