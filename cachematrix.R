
## This function creates a special matrix object that can cache 
## its inverse. It contains the following functions:
## set()
## get()
## setInverse()
## getInverse()
## In the end, the function returns a list contains the four functions above.

makeCacheMatrix <- function(x = matrix()) {
    aInv <- NULL # setting aInv for a future value
    
    set <- function (y) {
        x <<- y
        m <<- NULL # reset the inverse value (aInv)
    }
    
    get <- function () x # return x
    
    setInverse <- function (z) aInv <<- z
    
    getInverse <- function () aInv # returns the inverse matrix (aInv)
    
    # return a list contains all functions defined above
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by 'makeCacheMatrix'. If the inverse has
## already been calculated (and the matrix has not changed), then
## 'cacheSolve' should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    aInv <- x$getInverse()
    
    if (!is.null(aInv)) { # Test whether or not the inverse matrix already exists
        message("Getting cached inversed matrix")
        return (aInv)
    }
    
    data <- x$get() # get the original matrix and assign it to data variable
    aInv <- solve(data) # call function solve() to calculate the inverse matrix and assign it to aInv
    x$setInverse(aInv) # Set aInv
    
    aInv # retrun aInv
}

## Run example
## Step 1. creat a matrix and assign it to a
## > a <- rbind(c(4, 3), c(1, 1))
## Step 2. call makeCacheMatrix and assign it to b
## > b <- makeCacheMatrix(a)
## Step 3. First time to call cacheSolve(b). You would see 
## the inverse matrix is returned, but not from cache.
## cacheSolve(b)
## Step 4. Second time to call cacheSlve(). You would see the same inverse matrix
## is returned and this time, it is from the cache.

