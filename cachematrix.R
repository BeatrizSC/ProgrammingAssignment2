## These functions cache the value of a invertible matrix's inverse.  

## This function generates a special matrix formed through a list containing a  
## function that can cache its inverse: set and get the value of the matrix, set  
## and get the value of the matrix's inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
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

## This function computes the inverse of the special matrix produced in the previous 
## function. Firstly, it checks if the inverse has already been calculated. If it has 
## been calculated, it gets the inverse from the cache -it does not calculate it again- 
## and skips calculation. If not, it calculates the inverse and sets its value in the 
## cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
