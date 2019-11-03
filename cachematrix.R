# makeCacheMatrix creates a list containing a function which sets and gets the value of a matrix.
# It also sets and gets the value of the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
    inverseval <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverseval <<- inverse
    getinverse <- function() inverseval
    list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
  
}


## This function returns the inverse of the matrix. 
## If the inverse has already been calculated, the function is able to retrieve the inverse from caching.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseval <- x$getinverse()
    if(!is.null(inverseval)) {
      message("getting cached data")
      return(inverseval)
    }
    data <- x$get()
    inverseval <- solve(data)
    x$setinverse(inverseval)
    inverseval
}
