# Functional Programming in R
# Author: M. H. Ferdosi
###############################################################################
# rm(list=ls())
# Pure function
average <- function(x)
{
  mean(x)
}
average(1:3)
average(1:3)

# Impure function
A <- 3
Add <- function(x)
{
  A <<- A + x
  return(A)
}
Add(2)
Add(2)

# Recursion
Fibonacci <- function(x)
{
  if (x == 0) 
    return(0)
  if (x == 1) 
    return(1)
  return(Fibonacci(x - 1) + Fibonacci(x - 2))
}
Fibonacci(3)
Fibonacci(5)

# First class and higher order functions
sapply(1:10, sin)

# Regular Function
SampleFun <- function(x)
{
  stopifnot(!is.character(x))
  return(x^2)
}
SampleFun(5)

# Functionals
My10Number <- function(fun) fun(1:10)
class(My10Number)
typeof(My10Number)
My10Number(mean)
My10Number(sd)
My10Number(log)

# Map function
(A <- Map(log, 1:10))
class(A)
(B <- mapply(log, 1:10))
class(B)

# Reduce function
Reduce(f = "+", x = 1:10, accumulate = TRUE)
Reduce(f = "+", x = 1:10, accumulate = FALSE)
Reduce(f = "-", x = 1:10, accumulate = TRUE)
Reduce(f = paste, x = 1:10, accumulate = TRUE)

set.seed(2)
L <- mapply(sample, rep(list(1:10), 10), 20, replace = TRUE, SIMPLIFY = FALSE)
Reduce(f = intersect, L)

# Filter function
Data <- data.frame(A = 1:3, B = letters[1:3], C = runif(3))
lapply(Data, class)
apply(Data, 2, class)


Filter(function(x) !is.numeric(x), Data)
Filter(function(x) is.numeric(x), Data)
Filter(function(x) is.character(x), Data)
Filter(function(x) is.factor(x), Data)
Filter(function(x) sum(as.numeric(x)) > 10, Data)
Filter(function(x) sum(as.numeric(x)) > 10, 1:20)
(Ind <- Position(function(x) sum(as.numeric(x)) > 10, 2:20))
Find(function(x) sum(as.numeric(x)) > 10, 2:20)
T <- 2:20
T[Ind]

# Apply function
A <- matrix(1:10, nrow = 5)
apply(A, 1, sum)
apply(A, 2, sum)

# sapply, lapply and vapply function
sapply(1:10, log)
class(sapply(1:10, log))
lapply(1:10, log)
class(lapply(1:10, log))
vapply(1:10, log, FUN.VALUE = double(1))
vapply(1:10, log, FUN.VALUE = numeric(1))
vapply(1:10, log, FUN.VALUE = integer(1))
vapply(1:10, log, FUN.VALUE = character(1))

FunA <- function()
{
  matrix(sample(1:10, 10), nrow = 5)
}
set.seed(1)
B <- list(FunA(), FunA(), FunA())
Res_1 <- lapply(B, rowSums)
class(Res_1)
Res_2 <- sapply(B, rowSums)
class(Res_2)
(Res_3 <- do.call(rbind, Res_1))
(Res_4 <- do.call(cbind, Res_1))


# mapply function
mapply(sum, Res_1)
lapply(Res_1, sum)

# tapply function
set.seed(1)
Data <- data.frame(A = 1:10, B = sample(1:3, 10, replace = TRUE), C = runif(10))
tapply(Data$A, Data$B, sum)

# Function factories
sumsum <- function(x)
{
  newFun <- function(y)
  {
    x + y
  }
  return(newFun)
}
sumsum(5)(3)
Afun <- sumsum(2)

