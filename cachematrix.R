## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a "matrix" object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse){
                i <<- inverse
        }
        getInverse <- function(){
                i
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve function computes the inverse of the special "matrix" x if it is not calculated before 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        matrixObject <- x$get()
        i <- solve(matrixObject, ...)
        x$setInverse(i)
        i
}
