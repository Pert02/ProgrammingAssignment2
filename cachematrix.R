## This is the assigment for week 3
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix  


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                
                x <<- y
                
                inv <<- NULL
                
        }
        
        get <- function() x
        
        setinv <- function(invers) inv <<- invers
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}

## If the inverse has already been calculated 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                
                message("getting cached data")
                
                return(inv)
                
        }
        
        data <- x$get()
        
        inv <- solve(data)
        
        x$setinv(inv)
        
        inv
        
}
