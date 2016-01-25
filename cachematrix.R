# Example:Caching the Mean of a Vector

# <<- operator: can be used to assign a value to an object in an environment that 
# is different from the current environment

#makeVector creates a special "vector", which is a list containing a function to:
#1. Set the value of the vector
#2. Get the value of the vector
#3. Set the value of the mean
#4. Get the value of the mean

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

#The following function calculates the mean of the special "vector" created with the 
# above function.
# First checks to see if the mean has already been calculated, if so it gets the 
# mean from the cache and skips the computation. Otherwise, it calculates the mean
# of the data and sets the value of the mean in the cache via setmean function.

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

#Assignment 2 R Programming: Caching the Inverse of a Matrix
#Matrix inversion is most likely a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly.

# The following 2 functions will cache the inverse of a matrix.

#makeCacheMatrix function creates a special "matrix" object  that can cache its inverse.
#1.Set the value of the matrix
#2.Get the value of the matrix
#3.Set the inverse of the matrix
#4.Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv
        matrix(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#The following function cacheSolve calculates the inverse of the special "matrix" returned
# by the makeCacheMatrix function above.
# It first checks if the inverse has already been calculated. If so, it gets the inverse
# and skips the computation. Otherwise, it calculates the inverse of the matrix 
# and sets the value of the inverse in the cache via setinverse function.
# Assuming that the matrix supplied is always invertible.

cacheSolve <- funtion(x, ...) {
        inv <- get$inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
