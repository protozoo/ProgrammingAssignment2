## These functions create a cacheable object (a matrix)
## so that when values are requested it will first check
## if it's already computed and return that "cached" values if that's the case

## Creates an object to store a cacheable inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolved <- function(solved) s <<- solved
    getsolved <- function() s
    list( set=set, get=get, setsolved=setsolved, getsolved=getsolved )
}


## Computes an inverted matrix if it has not been computed yet
## Returns the computed inverted matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolved()
    if( !is.null(s) ){
        message("getting cache data")
        return( s )
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolved(s)
    s
}

## to test it out....
## mx <- matrix( c( 1, 2, 3, 4  ), nrow=2, ncol=2, byrow=T  )
## c <- makeCacheMatrix( mx )
## sol1 <- cacheSolve(c)
## sol2 <- cacheSolve(c)