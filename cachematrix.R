## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL                                      ## inverse vector
         set <- function(y){                              ## function to set the value
             x <<- y
             inv <- NULL
         }
         get <- function() x                              ## function to get the value
         setsolve <- function(cacheSolve) inv <<- solve   ## function to set the inverse values
         getsolve <- function() inv                       ## function to get the inverse values
         list (set = set, get = get, setsolve = setsolve, getsolve = getsolve) ## this is required for $ operator
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)){                                ## get cached matrix
           message("getting cached data")
           return(inv)
        }   
        data <- x$get()                                   ## first time around
        inv <- solve(data)
        x$setsolve(inv)
        inv
}
