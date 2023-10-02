# --------- 2 A --------- 

lambda = 36
A = (0.6-0.2)^2

res = 1-ppois(5, lambda*A)


# --------- 2 B --------- 

area_AB = 0.2*0.2
area_A_minus_AB = 0.4*0.4 - area_AB
sum = 0
for (k in 0:4){
  sum = sum + dpois(k, area_AB*lambda)*dpois(4-k, area_A_minus_AB*lambda)^2
}
print(sum)


# --------- 2 C --------- 

lambda <- 36
squarearea <- 1

N <- rpois(1, lambda*squarearea)
xpoints <- runif(N,0,1)
ypoints <- runif(N,0,1)

plot(xpoints, ypoints, pch=2)


# --------- 2 D --------- 

lambda <- rgamma(1, 36, 1)
squarearea <- 1

N <- rpois(1, lambda*squarearea)
xpoints <- runif(N,0,1)
ypoints <- runif(N,0,1)

plot(xpoints, ypoints, pch=2)


# --------- 2 E --------- 

lambda <- 36
squarearea <- 1
trials <- 100
simlist <- numeric(trials)
for (nTrials in 1:trials) {
  N <- rpois(1, lambda*squarearea)
  xpoints <- runif(N,0,1)
  ypoints <- runif(N,0,1)
  
  total = 0
  
  for(i in 1:N) {
    distances <- numeric(N)
    distances[i] = 2*squarearea^2
    for(j in 1:N) {
     if(i != j) {
       distances[j] = sqrt((xpoints[i]-xpoints[j])^2 + (ypoints[i]-ypoints[j])^2)
     } 
    }
    total = total + min(distances)
  }
  
  Z = total / N
  simlist[nTrials] = Z
}

var(simlist)

hist(simlist)



