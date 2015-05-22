library(UsingR)
library(dplyr)

# What is the average height of the sons (don't round off)?
mean(father.son$fheight)

# What is the mean of the son heights for fathers that have a height of 71 inches? 
mean(filter(father.son, round(fheight) == 71)$sheight)

X = matrix(1:1000,100,10)

# What is the entry in row 25, column 3 ?
X[25,3]

# Using the function cbind, create a 10 x 5 matrix with first column

x=1:10

# Then columns 2*x, 3*x, 4*x and 5*x in columns 2 through 5.

# What is the sum of the elements of the 7th row?

X = cbind(x, 2*x, 3*x, 4*x, 5*x)
rowSums(X)[7]

# Which of the following creates a matrix with multiples of 3 in the third column?
matrix(1:60,20,3,byrow=TRUE)

# Suppose X is a matrix in R. Which of the following is not equivalent to X?
X %*% matrix(1,ncol(X) ) 

# Solve the following system of equations using R:
X = matrix(c(3, 4, -5, 1,
             2, 2, 2, -1,
             1, -1, 5, -5,
             5, 0, 0, 1), 4, 4, byrow = TRUE)
X
solve(X) %*% c(10, 5, 7, 4)

a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)

ab <- a %*% b

# What is the value in the 3rd row and the 2nd column of the matrix product of 'a' and 'b'
ab[3,2]

# Multiply the 3rd row of 'a' with the 2nd column of 'b', using the element-wise vector
# multiplication with *. What is the sum of the elements in the resulting vector?

sum(a[3,] * b[,2])
