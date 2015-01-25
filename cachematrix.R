## Functions to support caching of the inverse of a matrix
## usage:
## > x <- makeCacheMatrix(matrix(rnorm(16), 4, 4))
## > cacheSolve(x)
##             [,1]        [,2]       [,3]        [,4]
## [1,] -0.5217698  0.59491642 -0.3728793 -0.09381126
## [2,]  1.5125709 -0.58157656  0.4206847 -0.32677740
## [3,]  0.1860804 -0.08813034 -0.5291406  0.38721203
## [4,] -0.4011924  0.36633059 -0.0295378  0.46514693
##
## (call cacheSolve successively returns inverse from cache)
## > cacheSolve(x)
## getting matrix from cache
##             [,1]        [,2]       [,3]        [,4]
## [1,] -0.5217698  0.59491642 -0.3728793 -0.09381126
## [2,]  1.5125709 -0.58157656  0.4206847 -0.32677740
## [3,]  0.1860804 -0.08813034 -0.5291406  0.38721203
## [4,] -0.4011924  0.36633059 -0.0295378  0.46514693

## Function to store a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function to calculate the inverse of a matrix.
## Uses a cache to store and retrieve previous calculations.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting matrix from cache")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m    
}
