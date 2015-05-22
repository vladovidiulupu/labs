X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")

beta <- c(5, 2)
X

X %*% beta

# What is the fitted value for the A samples? (The fitted Y values.)
# 5

# What is the fitted value for the B samples? (The fitted Y values.)
# 7

X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")

beta <- c(10,3,-3)

X %*% beta

# What is the fitted value for the B samples?
# 13

# What is the fitted value for the C samples?
# 7