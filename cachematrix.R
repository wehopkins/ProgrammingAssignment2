## Here are a pair of functions to support creating a matrix that can cache (save a copy)
## of the calculated inverse. It will calculate the inverse once, then return that cached
## answer when the inverse is requested again.


# Construct and return the matrix object; can provide an initial value when constructed
makeCacheMatrix <- function(x = matrix()) {
    # this is null until the inverse is first requested, then it contains the
    # inverse of x
    x.inverse <- NULL
    
    ## Public interface to matrix object
    
    # set the matrix value
    set <- function (y) {
        # save the new matrix in the object bound when this function was defined
        x <<- y
        
        # reset cached value back to NULL to indicate it no longer
        # contains the inverse of the matrix this object now represents
        x.inverse <<- NULL
    }
    
    # get the matrix value
    get <- function() x
    
    
    ## Private interface to the matrix object
    
    # save the value of the inverse - called by cacheSolve
    setinverse <- function(matrix.inverse) x.inverse <<- matrix.inverse
    
    # return the value of the inverse - called by cacheSolve
    getinverse <- function() x.inverse
    
    list (set=set,
          get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # attempt to retrieve a cached copy of the inverse
    x.inv <- x$getinverse()
    
    # if there was a cached copy of the inverse
    if (!is.null(x.inv)) {
        # give the user a message and return the inverse
        message("getting cached data")
        return(x.inv)
    }
    
    # get a copy of the matrix
    x.val <- x$get()
    
    # compute the inverse
    x.inv <- solve(x.val)
    
    # save the value of the inverse in the matrix object
    x$setinverse(x.inv)
    
    # return the value of the inveser
    x.inv
}

