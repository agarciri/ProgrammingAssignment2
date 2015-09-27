###  Caching the Inverse of a Matrix
###------------------------------------
###------------------------------------


# Matrix inversion is usually a costly computation and there may be #some benefit to caching the
#inverse of a matrix rather than compute #it repeatedly. These pair of functions. The following 
#functions, makeCacheMatrix and cacheSolve, can be use to calculate and cache the inverse of a matrix.


# makeCacheMatrix
#-----------------

# In this function, x is an input square invertible matrix

# This function creates and returns a list which is the input to 
#containing a function to cacheSolve(). This list containing a #function to set the value of the matrix, 
#get the value of the matrix, #set the value of inverse of the matrix and get the value of inverse #of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(z) {
        x <<- z
        inv <<- NULL
    }
    get <- function() x
    setinverse_mtx <- function(inverse) inv <<- inverse
    getinverse_mtx <- function() inv
    list(set=set, get=get, setinverse_mtx=setinverse_mtx, getinverse_mtx=getinverse_mtx)
}

# cacheSolve 
#------------

#In this function, the input is the output of makeCacheMatrix() 
# This function returns the inverse of the original matrix. It checks if the inverse has been calculated. 
#If it has computed, it has not calculated again and if it not, it will be computed the inverse of x,and 
#sets the value with setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse_mtx()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse_mtx(inv)
    inv
}

## Example run:

## > x = rbind(c(1, 3), c(0,-2))
## > mt = makeCacheMatrix(x)
## > mt$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    0   -2
## > cacheSolve(mt)
## [,1] [,2]
## [1,]    1  1.5
## [2,]    0 -0.5
## > cacheSolve(mt)
## getting cached data.
## [,1] [,2]
## [1,]    1  1.5
## [2,]    0 -0.5
