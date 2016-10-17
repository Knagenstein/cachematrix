#First attempt to Assignment 2 Coursera
#makeCacheMatrix: special matrix object thatcan cache its inverse
# Part 1 of Assignment 2

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {  #changes vector stored in main function
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x  #returns vector x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Part 2 of Assignment 2 - cache solve 
#computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("Getting the cached inverse matrix data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}