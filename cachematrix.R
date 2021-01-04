## these functions together cache a given matrix and store its inverse to avoid
## repeated computations which can be time consuming

## this function caches a matrix and gives it 4 functions which are:
    ## get the matrix
    ## set the matrix
    ## get the inverse
    ## set the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getInv <- function() inv
    setInv <- function(inverse) inv <<- inverse
    list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## this function accepts a cached matrix from the above function
## if the inverse has already been calculated, it gets the cached value,
## otherwise it calculates the value and sets it in the cache via the setInv() 
## function before returning it

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
