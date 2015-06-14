## cachematrix.R
##
## These methods make it possible to cache the inverse of a square matrix.

## `makeCacheMatrix(squareMatrix = matrix())`
##
## This method creates a cacheable Matrix.
## It is implemented as a list of functions:
##   - `get` Returns the original square matrix
##   - `set` Sets a new square matrix to be inversed (and also resets the cache 
##           to NULL)
##   - `setInverse` Sets the calculated inverse to the cache
##   - `getInverse` Gets the cached inverse (or NULL if not yet set)
##
makeCacheMatrix <- function(squareMatrix = matrix()) {
    ## The cache, initialized to `NULL`
    cache <- NULL

    ## Only there to make the object reusable, so you can set new data.
    ## The cache will be reset to `NULL`
    set <- function(newSquareMatrix) {
        squareMatrix <<- newSquareMatrix
        cache <<- NULL
    }

    ## Get the original square matrix
    get <- function() squareMatrix

    ## Sets the calculated inverse to the cache
    setInverse <- function(inverse) cache <<- inverse

    ## Gets the cache
    getInverse <- function() cache

    ## List of functions (methods) of this R `object`
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## `cacheSolve(cacheableSquareMatrix, ...)`
##
## This function solves for the inverse of a cacheable matrix by first checking
## the cache. If that is NULL, then it actually does the costly inverse 
## calculation using `solve`.
cacheSolve <- function(cacheableSquareMatrix, ...) {
    ## Get the cache from the cacheable matrix
    cachedInverse <- cacheableSquareMatrix$getInverse()

    ## Check if the cache is valid (= not NULL). If so, return it.
    if(!is.null(cachedInverse)) {
        message("getting cached data")

        return(cachedInverse)
    }

    ## Cache is invalid, so get the original square matrix from the cacheable 
    ## matrix.
    originalSquareMatrix <- cacheableSquareMatrix$get()

    ## Solve the original square matrix
    cachedInverse <- solve(originalSquareMatrix, ...)
    
    ## Set the calculated inverse to the cache of the cacheable matrix
    cacheableSquareMatrix$setInverse(cachedInverse)
    
    ## Return the result
    cachedInverse
}
