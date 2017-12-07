## Put comments here that give an overall description of what your
## functions do

## Stores the matrix and its inverse, it also creates the functions
## to set and get those two variables

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invmatrix) inv <<- invmatrix
    getinv <- function() inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Checks if an inverse is already cached, if not, then it calculates it
## and caches it for later use

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        ## Return cached inverse
        print("cached")
        return(inv)
    }

    tempmatrix <- x$get()
    tempsolved <- solve(tempmatrix)
    tempinverse <- tempsolved %*% tempmatrix
    x$setinv(tempinverse)

    ## Return a matrix that is the inverse of 'x' after calculating it
    tempinverse
}
