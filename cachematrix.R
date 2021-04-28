## These function are caching the inverse of a matrix 

## This function creates a special matrix 

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x <<- y
                inverse<-NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        inverse<-x$getinverse()
        if(!is.null(inverse)){
                message("getting cached matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <-  solve(data,...)
        x$setinverse(inverse)
        inverse
}
