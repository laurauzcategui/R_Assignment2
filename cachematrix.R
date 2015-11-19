# @author @laura_uzcategui
# makeCacheMatrix is taking the matrix as argument
# the it will build the set and gets functions
# notice the use of <<- as to be search in parent environment

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# this function will try to get the inverse matrix
# which is stored in the environment but if not will try to
# use solve function and set the values w/ defined functions
# at makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)){
        message("getting inverse matrix data")
        return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      i
}
