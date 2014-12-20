## cachematrix.R - handle matrces with chaching their inversions.
## Functions provided:
## makeCacheMatrix - returns list of functions representing a "matrix"
## cacheSolve - calculates and caches inverse of a "matrix"
##
## Example:
##
## A <- makeCacheMatrix(matrix(1:4,2,2))
## B <- cacheSolve(A)
## C <- cacheSolve(A) # getting cached data
## D <- cacheSolve(A) # getting cached data
## A$set(matrix(c(1,2,4,8,0,0,1,2,0),3,3))
## B <- cacheSolve(A)
## C <- cacheSolve(A) # getting cached data



makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse
    ## Arguemt 'x': a matrix (must me invertable), default: empty matrix
    ## Returns: list of functions
    
    inv <- NULL
    set <- function(y) {
        #change matrix data
        x <<- y
        #invalideate cacje
        inv <<- NULL
    }
    get <- function() {
        #get the matrix data
        x
    }
    setinv <- function(i) {
        #save to cache
        inv <<- i
    }
    getinv <- function() {
        #return chached value (NULL if not cached)
        inv
    }
    #return list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



cacheSolve <- function(x, ...) {
    ## This function computes the inverse of the special "matrix" 
    ## returned by makeCacheMatrix. If the inverse has already been
    ## calculated (and the matrix has not changed), then the inverse 
    ## is retrieved from the cache. We assume that the matrix supplied
    ## is always invertible
    ## Agrument 'x': a list of functions as returned by makeCacheMatrix
    ## Additional arguments will be passed to 'solve' function
    ## Returns: a matrix that is the inverse of 'x'
    
    ## chceck if we already have cached inverse
    inv <- x$getinv()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    ## read the data of matrix
    data <- x$get()
    ## do the calculation (pass additional parameters)
    inv <- solve(data, ...)
    ## cache result
    x$setinv(inv)
    inv
}

