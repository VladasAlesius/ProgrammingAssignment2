## The purpose of these functions is to calculate the inverse of a matrix
## To avoid unnecessary calculations, inverse matrix value is stored 
## so that function cacheSolve could retrieve it in case matrix does not change

## Vladas Alesius, R programming course, Week 3 assignment, 2017-07-20

## Function makeCacheMatrix creates a list of functions that
## set and get values of both input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse){
                m <<- inverse
        }
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)   
}


## cacheSolve retrieves an inverse matrix in case 
## the inverse has already been calculated and the input matrix has not changed
## or it calculates an inverse matrix if the new input matrix is provided
## solve function is used for calculations

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}



