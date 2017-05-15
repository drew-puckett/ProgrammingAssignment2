## These two functions work together to cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize object i to eventually contain matrix inverse
    i <- NULL
    
    ## assign matrix in y to matrix x from parent environment.  
    ## Value of i from parent environment needs to be reset to null since x has been changed
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## return the matrix stored in x
    get <- function() x
    
    ## assign the the matrix newinverse to object i from parent environment
    setinverse <- function(newinverse) i <<- newinverse
    
    ## return the matrix stored in i
    getinverse <- function() i
    
    ## return a list (special "matrix" object) with column names
    ## matching the 4 previously defined functions
    ## so the functions can be easily referenced by cacheSolve using the $ operator
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    ## call the getinverse function in special "matrix" object x
    i <- x$getinverse()
    
    ## if i is not null a message indicates that the value of i 
    ## had been cached before the value of i (inverse) is returned.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if i is null then the values of matrix x are assigned to an object called data.
    ## Data is then passed to the built-in solve function
    ## which returns the inverse of matrix x assuming it is
    ## invertible.
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    ## call setinverse to cache the new calculated inverse of x
    x$setinverse(i)
    
    ## return i
    i  
}
