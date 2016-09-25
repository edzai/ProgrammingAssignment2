## Caching the Matrix Inverse:
## The functions below create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(z = matrix()) {
        inv <- NULL
        set <- function(y) {
                z <<- y
                inv <<- NULL
        }
        get <- function() z
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'z'
        inv <- z$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- z$get()
        inv <- solve(mat, ...)
        z$setInverse(inv)
        inv
}