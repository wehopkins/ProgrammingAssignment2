## Here are a pair of functions to support creating a matrix object that can cache (save a copy)
## the calculated inverse. It will calculate the inverse once, then return that cached
## answer when the inverse is requested again.
## This style of saving a calculated result to avoid future recalculation is known
## as memoization.


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
    
    # return the matrix object, consisting of the set of operators and implied copies
    # of the matrix and cache object
    list (set=set,
          get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}


## Take a matrix object constructed using makeCacheMatrix and return the
## inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # retrieve cached value
    x.inv <- x$getinverse()
    
    # if it was a cached copy of the inverse
    if (!is.null(x.inv)) {
        # give the user a message and return the inverse
        message("getting cached data")
        return(x.inv)
    }
    
    # get a copy of the matrix
    x.val <- x$get()
    
    # compute the inverse
    x.inv <- solve(x.val)
    
    # save the value of the inverse in the matrix object cache
    x$setinverse(x.inv)
    
    # return the value of the inverse
    x.inv
}

