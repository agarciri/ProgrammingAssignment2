###  Caching the Inverse of a Matrix
###------------------------------------
###------------------------------------


# Matrix inversion is usually a costly computation and there may be #some benefit to caching the inverse of a matrix rather than compute #it repeatedly. These pair of functions. The following functions, #makeCacheMatrix and cacheSolve, can be use to calculate and cache the #inverse of a matrix.


# makeCacheMatrix
#-----------------

# In this function, x is an input square invertible matrix

# This function creates and returns a list which is the input to #containing a function to cacheSolve(). This list containing a #function to set the value of the matrix, get the value of the matrix, #set the value of inverse of the matrix and get the value of inverse #of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(z) {
        x <<- z
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve 
#------------

#In this function, the input is the output of makeCacheMatrix() 
# This function returns the inverse of the original matrix. It checks if the inverse has been calculated. If it has computed, it has not calculated again and if it not, it will be computed the inverse of x,and sets the value with setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}