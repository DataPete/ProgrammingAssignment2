## Function makeCacheMatrix creates an inverse matrix and saves it to the cache.
## This stores the previous value in the matrix to be returned whenever a
## calculation for a matrix inverse is done.  This saves computing time instead
## of repeating the entire calculation

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calculates the inverse of the special "matrix" created from
## the function makeCacheMatrix.  It checks to see if the inverse has already
## ben calculated.  If it has, it gets the inverse from the cache and skips
## the compuation.  Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setinverse function.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}