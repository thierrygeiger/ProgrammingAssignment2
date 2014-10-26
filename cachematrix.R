## R programming - Programmng assignment 2
## Practicing lexical scoping and superassignment operator
## Thierry Geiger - 25 Oct 2014

## The first function, makeVector creates 
## a list containing four functions to respectively 
## 1) set the value of the matrix
## 2) get its value
## 3) set the value of the inverse of the matrix
## 4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# This function returns the inverse of the matrix provided as argument if 
# it exists in cache (that is, if it's already been calculated)
# if it doesn't exist than it calculates it using 'solve' 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Calls to both functions for testing purposes
# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# cacheSolve(amatrix) 



