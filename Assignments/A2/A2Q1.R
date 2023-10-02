library(LearnBayes)
data = c(1, 2, 3, 2, 3, 1, 2, 1, 3, 2)
chain_length = 100 - length(data) + 1
simulate1 = function(should_plot) {
    
    count_matrix = matrix(c(0, 2, 1, 
                             1, 0, 2,
                             1, 2, 0), 3, 3, byrow=T)

    prior_param = matrix(1, 3, 3)
    new_chain = rep(data[length(data)], chain_length)


    for (step in 2:chain_length) {
        posterior_param = prior_param + count_matrix
        posterior = matrix(0, 3, 3)
        for (i in 1:3) {
            param = posterior_param[i,]
            posterior[i,] = param / sum(param)
        }

        state = new_chain[step-1]
        current_prob = posterior[state,]
        next_state = sample(c(1, 2, 3), 1, prob=current_prob)
        new_chain[step] = next_state
        count_matrix[state, next_state] = count_matrix[state, next_state] + 1
    }
    if (should_plot) {
        print(new_chain)
        print(count_matrix)
        plot(c(data, new_chain[-1]), type="l")
    }
    return (posterior)
}

simulate2 = function() {
    posterior = simulate1(FALSE)
    return (posterior[1, 2])
}

results2 = replicate(1000, simulate2())
hist(results2, main="simulate2 histogram", xlab="P_12", breaks=seq(0,1,l=20))

simulate3 = function() {
    count_matrix = matrix(c(0, 2, 1, 
                             1, 0, 2,
                             1, 2, 0), 3, 3, byrow=T)

    prior_param = matrix(1, 3, 3)
    new_chain = rep(data[length(data)], chain_length)

    posterior_param = prior_param + count_matrix
    posterior = matrix(0, 3, 3)
    for (i in 1:3) {
        param = posterior_param[i,]
        posterior[i,] = param / sum(param)
    }

    for (step in 2:chain_length) {
        state = new_chain[step-1]
        current_prob = posterior[state,]
        next_state = sample(c(1, 2, 3), 1, prob=current_prob)
        new_chain[step] = next_state
        count_matrix[state, next_state] = count_matrix[state, next_state] + 1
    }

    posterior_param = prior_param + count_matrix
    posterior = matrix(0, 3, 3)
    for (i in 1:3) {
        param = posterior_param[i,]
        posterior[i,] = param / sum(param)
    }
    return (posterior[1, 2])
}

results3 = replicate(1000, simulate3())
hist(results3, main='simulate3 histogram', xlab="P_12", breaks=seq(0,1,l=20))

simulate4 = function() {
  count_matrix = matrix(c(0, 2, 1, 
                          1, 0, 2,
                          1, 2, 0), 3, 3, byrow=T)
  
  prior_param = matrix(1, 3, 3)
  new_chain = rep(data[length(data)], chain_length)
  
  posterior_param = prior_param + count_matrix
  posterior = matrix(0, 3, 3)
  for (i in 1:3) {
    posterior[i,] <- rdirichlet(1, posterior_param[i,])

  }
  
  for (step in 2:chain_length) {
    state = new_chain[step-1]
    current_prob = posterior[state,]
    next_state = sample(c(1, 2, 3), 1, prob=current_prob)
    new_chain[step] = next_state
    count_matrix[state, next_state] = count_matrix[state, next_state] + 1
  }
  
  posterior_param = prior_param + count_matrix
  posterior = matrix(0, 3, 3)
  for (i in 1:3) {
    param = posterior_param[i,]
    posterior[i,] = param / sum(param)
  }
  return (posterior[1, 2])
}

results4 = replicate(1000, simulate4())
hist(results4, main='simulate4 histogram', xlab="P_12", breaks=seq(0,1,l=20))
