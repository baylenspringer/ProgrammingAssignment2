## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    
    
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## check to see if the matrix inverse already exists
## if yes, return the inverse of the matrix
## otherwise set the value of the new matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    
}
