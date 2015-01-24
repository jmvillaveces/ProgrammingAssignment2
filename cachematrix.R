## functions to test caching in R

## Special matrix that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #return current matrix
        get <- function() x
        #set inverse
        setinverse <- function(inverse) m <<- inverse
        #returns inverse matrix
        getinverse <- function() m
        
        #returns list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
# if the inverse is chached it is returned else it will be calculated
# a reference will be saved and retuned
cacheSolve <- function(x, ...) {
        
        m <- x$getinverse() #get cached inverse
        if(!is.null(m)) { # return cached inverse it if exists
                message("getting cached data")
                return(m)
        }
        
        message("calculating inverse")
        
        data <- x$get() #get original matrix
        m <- solve(data, ...) # find inverse
        x$setinverse(m) # save a reference to inverse matrix
        m  #return m
}
