## The makeCacheMatrix function will take a vector of type matrix as an input and can cache its inverse.

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

## The cacheSolve function will compute the inverse of the matrix by the above function. If the inverse has already been computed, 
## then it retrieves it from the cache. Creating an inverse of a matrix is expensive to compute, so by saving the inverse in cache,
## and retrieving it when required, is an efficient way of managing resources.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)){                                ## get cached matrix
           message("getting cached data")
           return(inv)
        }   
        data <- x$get()                                   ## first time around
        inv <- solve(data)                                ## create the inverse matrix
        x$setsolve(inv)
        inv                                               
}
