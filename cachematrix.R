## The following pair of functions provides a mechanism to cache the inverse of
## a matrix rather than compute it repeatedly assuming the matrix has not changed.
## Please note that all matrices passed to these functions are assumed to be
## invertible.

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## The return object is actually a list containing a function to do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse matrix
## 4. get the value of its inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()

        # obtaining cached inverse if possible
        if(!is.null(inv)) {
                message("getting cached data")
                
                # returning cached inverse
                return(inv)
        }
        
        data <- x$get()
        
        # computing inverse
        inv <- solve(data, ...)
        
        # setting inverse in cache for later use
        x$setinv(inv)
        
        # returning computed inverse
        inv
}
