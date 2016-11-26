###################################################################
## pair of functions can be used to return the inverse of a matrix 
## matrix is recomputed only when it is changed
## otherwise a cached inverse is returned
###################################################################


## makeCacheMatrix function creates a list of fucntions that set local
## and global values of variables - to save cached value and to 
## determine whether a cached inverse value is stored

makeCacheMatrix <- function(mtx = matrix()) {
    cacheInv <- NULL   #initialize cache
    
    set <- function(y) {
        mtx <<- y
        cacheInv <<- NULL
    }
    get <- function() {
        mtx
    }
    setInv <- function(inv) {
        cacheInv <<- inv
    }
    getInv <- function() {
        cacheInv
    }
    
    # output - define list of functions
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv
    )

}


## cacheSolve function returns the inverse of the matrix.
## if the invert is cached exists --> returns value
## else --> inverse is calculated, cached and returned

# input into the cacheSolve should be the list from makeCacheMatrix
cacheSolve <- function(x, ...) {
    
    cachedInverse <- mtx$getInv()
    ## if cached Inverse is not null --> return the cached value
    if(!is.null(cachedInverse)) {
        message("getting cached data")
        return(cachedInverse)
    }
    
    # if cached value does not exist
    # get matrix (which was stored in the "get" element of the makeCacheMatrix) 
    # --> calculate inverse --> update cache
    mtxData <- x$get()
    newInv <- solve(mtxData, ...)
    mtx$setInv(newInv)
    
    #retun result
    newInv
}

#########################################
## TEST:

## cacheSolve(z<-makeCacheMatrix(x<-matrix(c(2,2,8,2,9,1,0,4,1),3,3)))

## The function returns:            
##            [,1]        [,2]       [,3]
## [1,]  0.07142857 -0.02857143  0.1142857
## [2,]  0.42857143  0.02857143 -0.1142857
## [3,] -1.00000000  0.20000000  0.2000000

## Part 2: --> try cache --> cacheSolve(z)

## The function returns: 
## getting cached data
##             [,1]        [,2]       [,3]
## [1,]  0.07142857 -0.02857143  0.1142857
## [2,]  0.42857143  0.02857143 -0.1142857
## [3,] -1.00000000  0.20000000  0.2000000
