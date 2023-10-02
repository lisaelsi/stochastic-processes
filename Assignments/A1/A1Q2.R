library(expm)

# 2A
tMatrix <- c(0.25, 0.25,  0.25, 0.25, 0,
             0.25, 0,     0.25, 0.25, 0.25,
             0.5,  0,     0,    0.25, 0.25, 
             0.5,  0.25,  0,    0,    0.25, 
             0,    0,     0,    0,    1)

tMatrix <- matrix(tMatrix, nrow=5, ncol=5, byrow = TRUE) 

Q <- tMatrix[1:4, 1:4]
I <- diag(4)

# Fundamental matrix 
F <- solve(I-Q)

ones <- c(1,1,1,1)

E = F%*%ones

avgSteps = sum(E)/4+1


# 2B
tMatrix <-  c(0.25, 0.25,  0.25,  0.25, 0,
              0.25, 0,     0.25,  0.25, 0.25,
              0.5,  0.25,  0,     0,    0.25, 
              0,    0,     0,     1,    0,
              0,    0,     0,     0,    1)

tMatrix <- matrix(tMatrix, nrow=5, ncol=5, byrow = TRUE) 

Q <- tMatrix[1:3, 1:3]
I <- diag(3)
R <- tMatrix[1:3, 4:5]

# Fundamental matrix 
F <- solve(I-Q)

FR <- F%*%R

quarters <- c(1/4, 1/4, 1/4)

# probability of visiting state 3 before the end of the game
prob <- FR[1:3, 1]%*%quarters + 0.25
prob

#2C
tMatrix <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
             1, 0, 1, 1, 1, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 2, 0, 0, 1, 1,
             2, 1, 0, 0, 1, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 4, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 1, 1, 1, 1, 0,
             0, 0, 0, 0, 0, 1, 0, 1, 1, 1,
             0, 0, 0, 0, 0, 0, 0, 4, 0, 0,
             0, 0, 0, 0, 0, 2, 1, 0, 0, 1,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 4)

tMatrix <- matrix(tMatrix, nrow=10, ncol=10, byrow = TRUE) 

tMatrix <- tMatrix*0.25

# Reorder matrix to form fundamental matrix 

oldOrder <- c(1:10)
newOrder <- c(1:4, 6, 7, 9, 5, 8, 10)

tMatrix[oldOrder, ] <- tMatrix[newOrder, ]
tMatrix[, oldOrder] <- tMatrix[, newOrder]
tMatrix

Q <- tMatrix[1:7, 1:7]
I = diag(7)

R <- tMatrix[1:7, 8:10]

# Fundamental matrix 
F <- solve(I-Q)

FR <- F%*%R
FR

quarters <- c(1/4, 1/4, 1/4, 1/4, 0, 0, 0)

prob <- FR[1:7, 2]%*%quarters 

# Probability of visiting state 3 two or more times
prob