## The functions below demonstrates caching time consuming computations in R
## `makeCacheMatrix` is a factory function that returns an matrix object capable
## of caching its own inverse. `cacheSolve` is a wrapper for solve for CacheMatrix
## objects. It will look for cached inverse. If not computed yet, 
# the function will compute the inverse and store the result on the CacheMatrix object.


## `makeCacheMatrix` Create a matrix object that may cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) inv <<- inverse
    get.inverse <- function() inv
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## `cacheSolve` returns inverse of CacheMatrix using cache if available, otherwise
## will solve and cache result

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$get.inverse()
    if (is.null(inv)) {
        message('Computing inverse...')
        mat <- x$get()
        inv <- solve(mat, ...)
        x$set.inverse(inv)
    } else {
        message('Retriving from cache...')
    }
    return(inv)
}

# Example

X <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
cachingX <- makeCacheMatrix(X)
cacheSolve(cachingX)  # Compute
cacheSolve(cachingX)  # Use cached value
