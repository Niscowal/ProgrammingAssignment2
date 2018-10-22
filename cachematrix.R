# Put comments here that give an overall description of what your
# functions do

# Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
# 
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- solve
    getInverse <- function() inv
    list(
      set = set, 
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created with the above function 
## This is only done if the inverse has already been calculated (and the matrix has not changed), 
## Otherwise, it calculates the inverse via the solve function


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv) 
    inv
}

#Test cases
# > a <- makeCacheMatrix(matrix(1:4,2,2))
# > a$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > b <- makeCacheMatrix(matrix(10:15,2,2))
# > b$get()
# [,1] [,2]
# [1,]   10   12
# [2,]   11   13
# > cacheSolve(b)
# [,1] [,2]
# [1,] -6.5    6
# [2,]  5.5   -5

