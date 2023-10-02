data = read.table("dataAssignment3.txt", header=TRUE)

prob <- function(x, y, theta) {
  
  t1 = theta[1]
  t2 = theta[2]
  t3 = theta[3]
  
  t1_term = exp(t1) * x
  t2_t3_term = exp(t2) * (y - t3)^2
  
  numerator = exp(t1_term + t2_t3_term) - 1
  denominator = exp(t1_term + t2_t3_term) + 1
  
  return(numerator/denominator)
  
}

# ------------------ 1B ------------------ 

calculate_log_sum <- function(theta) {
  sum = 0
  
  for (i in 1:nrow(data)){
    x = data[i,1]
    y = data[i,2]
    z = data[i,3]
    
    if(z == 0) 
      p = 1 - prob(x,y,theta)
    else
      p = prob(x,y,theta)
    
    sum = sum + log(p)
  }
  
  return(sum)
}

create_list <- function(theta, N) {
  theta_list = list()
  
  for (i in 1:N){
    len <- length(theta_list)
    theta_list[[len+1]] <- theta
  }
  
  return(theta_list)
  
}

# ------------------ 1C ------------------ 


# Creates a scatter plot for the data
color = ifelse(data$z == 0, "blue", "red")
plot(data$x, data$y, col = color, bg = color, pch = 21, ylab='Temperature', xlab='Pollutant Concentration', main='Scatter plot of data')


legend(x = "bottomright", legend=c("Healthy", "Sick"),
       fill = c("blue", "red"))


mcmc <- function(theta_list, N) {
  count = 0
  
  likelihood = numeric(N-1)
  a_list = numeric(N-1)
  
  for(i in 2:N) {
    proposal = c(0, 0, 0)
    
    for(j in 1:3) {
      norm_factor = rnorm(1, 0, 0.4)  
      proposal[j] = theta_list[[i-1]][j] + norm_factor
    }
    
    likelihood_num = calculate_log_sum(proposal)
    likelihood_den = calculate_log_sum(theta_list[[i-1]])
    a = exp(likelihood_num)/exp(likelihood_den)
    
    if (a > 1)
      a_list[i-1] = 1
    else
      a_list[i-1] = a
    
    if(runif(1) < a) {
      theta_list[[i]] = proposal
      likelihood[i-1] = likelihood_num
    }
    else {
      theta_list[[i]] = theta_list[[i-1]]
      likelihood[i-1] = likelihood_den
    }
    
  }
  plot(likelihood, main="log_likelihood", type='l')
  hist(a_list, breaks=seq(0,max(a_list),l=20))
  return(theta_list)
  
}

create_histograms <- function(result, N) {
  
  t1 = numeric(N)
  t2 = numeric(N)
  t3 = numeric(N)
  
  for (i in 1:N) {
    t1[i] = result[[i]][1]
    t2[i] = result[[i]][2]
    t3[i] = result[[i]][3]
  }
  
  hist(t1)
  hist(t2)
  hist(t3)
  
}

create_plots <- function(result, N) {
  
  t1 = numeric(N)
  t2 = numeric(N)
  t3 = numeric(N)
  
  for (i in 1:N) {
    t1[i] = result[[i]][1]
    t2[i] = result[[i]][2]
    t3[i] = result[[i]][3]
  }
  
  plot(t1, main="theta1", type='l')
  plot(t2, main="theta2", type='l')
  plot(t3, main="theta3", type='l')
  
}


theta = c(-2, -3, 18)

N = 10000

theta_list = create_list(theta, N)

results = mcmc(theta_list, N)

create_histograms(results, N)

create_plots(results, N)


# ------------------ 1D ------------------ 

sick_prob_list = numeric(length(results))
sick_binom = numeric(length(results))

for(i in 1:length(results)) {
  
  theta = results[[i]]
  sick_prob = prob(3, 13, theta)
  
  sick_prob_list[i] = sick_prob
  sick_binom[i] = dbinom(9, 10, sick_prob)
  
}

print('Probability of being sick given x = 3, y = 13 and mean theta:')
print(mean(sick_prob_list))

print('Probability that 9 of 10 animals exposed to this temp and pollutant conc. get sick:')
print(mean(sick_binom))

