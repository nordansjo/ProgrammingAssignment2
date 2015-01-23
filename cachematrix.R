##These two functions can be used together to calculate the invert of a matrix. If the calculation have been made
## earlier it uses that value instead of running the calculation again and therefore could saves resources.




## makeCacheMatrix creates a function that that makes a matrix-ish with 4 functions included. 
## Set that sets the value of the matrix and removes the old free variable. get that grab the value of the matrix.
## setinv takes a value from cacheSolves and sets the inverted matrix that have been calculated with the solve function. getinv that grabs the inverted matrix.
 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  get     <- function() x
  setinv  <- function(solve) inv <<- solve
  getinv  <- function() inv
    
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}
  



## cacheSolves uses the matrix function created in makeChaceMatrix and applies the solve function to the value. If the invert already 
## have been calculated it doesn´t make the calculation again, instead it uses the value from the free variabel inv and print a message that it uses a cached value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
    if(!is.null(inv)) {
      message("Getting Value From Cache")
     return (inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}