## This function creates a list with a function that
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse of a matrix
## 4. get the value of the inverse of a matrix
## the matrix is assumed to be squared and invertible
## this function does not check the previous conditions


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function cacheSolve checks whether or not
## the inverse of the given matrix has been computed
## If the inverse has been computed previously
## the function print the message "getting cached data"
## and provides the inverse.
## If the matrix has changed or its inverse has not been calculate
## the function computes the inverse

## Note: every time you invoke the function makeCacheMatrix
## a new list will be created. However, if you assign a name to that list
## (for example, c<-makeCacheMatrix(a), where a is a matrix)
## and invoke twice the function cacheSolve, then
## 1. the first time the "inverse" of c (i.e. the inverse of a) will be computed
## 2. the second time the "inverse" of c (i.e. the inverse of a) will
## be retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
