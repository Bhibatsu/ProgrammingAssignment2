## The function purpose is for matrix inversion, which is done by
## catching the inverse of a matrix.

## The function creates a special matrix object that can cache its inverse.
## 1.- set the value of the matrix
## 2.- get the value of the matrix
## 3.- set the value of the mean
## 4.- get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  
            inv <- NULL
            set <- function(y) {
              x <<- y
              inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set = set, get = get, setinverse = setinverse,
                 getinverse = getinverse)

}


## The function computes the inverse of the special matrix returnes 
## by the above function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
              inv <- x$getinverse()
              if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
              }
              data <- x$get()
              inv <- solve(data, ...)
              x$setinverse(inv)
              return(inv)
}
