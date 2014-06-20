# MATRIX INVERSION AND CACHE FUNCTIONS
# The following two functions are used to compute the inverse of a matrix and shows the benefit
# of caching values rather than computing it repeatedly.

# makeCacheMatrix 
# S1. set the value of the matrix
# S2. get the value of the matrix
# S3. set the value of inverse of the matrix
# S4. get the value of inverse of the matrix
makeCacheMatrix <- function(mtx = matrix()) {
    inv <- NULL
    set <- function(mty) {
        mtx <<- mty
        inv <<- NULL
    }
    get <- function() mtx
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The cacheSolve function returns the inverse of the matrix. Check if inverse computation is done. 
# If Yes, it gets the result and skips the computation. 
# If No, it computes the inverse, sets the value in the cache via setinverse function.
#  Assumption : The provided matrix is always invertible.

cacheSolve <- function(mtx, ...) {
    inv <- mtx$getinverse()
    if(!is.null(inv)) {
        message("Retrieving cached matrix data...")
        return(inv)
    }
    data <- mtx$get()
    inv <- solve(data)
    mtx$setinverse(inv)
    inv
}


## TEST RUN SAMPLE FROM RSTUDIO
##> x = rbind(c(1, -3/4), c(-3/4, 1))
##> m = makeCacheMatrix(x)
## m$get()
##[,1]  [,2]
##[1,]  1.00 -0.75
##[2,] -0.75  1.00

##> cacheSolve(m)
##[,1]     [,2]
##[1,] 2.285714 1.714286
##[2,] 1.714286 2.285714

##> cacheSolve(m)
##Retrieving cached matrix data...
##[,1]     [,2]
##[1,] 2.285714 1.714286
##[2,] 1.714286 2.285714
##> 

