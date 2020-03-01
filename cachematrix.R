## makeCacheMatrix() and cacheSolve() are a pair of functions that work 
##      together to create an object that stores a matrix and caches
##      its inverse

## makeCacheMatrix() 1. set the value of the matrix
##                   2. get the value of the matrix
##                   3. set the inverse of the matrix using solve()
##                   4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m=NULL
        set<- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m<<- solve
        getsolve <- function() m
        list(set=set, get=get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve() returns the inverse of the matrix created by makeCacheMatrix
##              Before calculating the inverse the function looks to see if 
##              the inverse is already cached. If there is a cached inverse
##              the message "getting cache data" is printed in the console and
##              the cached inverse is printed. If there is no cached inverse
##              the inverse of the matrix is calculated and printed in the 
##              console

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return (m)
        }
        data<- x$get()
        m<- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}



