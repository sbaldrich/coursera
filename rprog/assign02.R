## The following functions allow to compute and cache the inverse of a matrix
## to avoid unnecesary recomputations.

## Create a "cache matrix". These matrices allow to store the result of computing
## their inverse thus avoiding its recomputation on future invocations of the cacheSolve
## function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Verify whether the inverse of the x "cache matrix" has been already computed and
## cached and if so, return it. Otherwise compute it, cache it and return it.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
       if(!is.null(inv)){
         message("Getting cached inverse matrix")
         return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...)
       x$setinverse(inv)
       inv
}
