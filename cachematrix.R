## makeCacheMatrix inputs a square matrix and creates a vector,
## which is really a list containing functions to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m<<-solve
    getinv <- function() m
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates the inverse of the matrix created with
## the makeCacheMatrix function.  It checks first to see if the inverse has
## already been calculated.  If so, it gets the inverse from the 
## cache and skips the computation.  Otherwise it calculates the inverse
## of the matrix and sets the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
