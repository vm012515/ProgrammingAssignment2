## The makeCacheMatrix function is a frame to create a matrix which allows 
##to save the inverted values of the metrix in cache 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setsolve <- function (solve) i<<-solve
        getsolve<-function () i
        list (set=set, get=get,
              setsolve=setsolve,
              getsolve=getsolve)
}


## The cacheSolve function takes the results of the makeCacheMatrix
##and returns the inversed matrix. If the inversed matrix is saved in the cache, 
##it returns it from the cashe. If it is not saved there, then it calculate the inverse matrix.

cacheSolve <- function(x, ...) {
        i <- x$getsolve ()
        if(!is.null(i)) {
                message ("getting cashed data")
                return (i)
        }
        data <- x$get ()
        i <- solve(data, ...)
        x$setsolve (i)
        i
        ## Return a matrix that is the inverse of 'x'
}
