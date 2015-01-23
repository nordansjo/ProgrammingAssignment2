##Theese two functions creates can be used together to to calculate the invert of a matrix. if the calculation have been made
## earlier it uses that value and because of that dosn´t have to do the calculation again and therefore saves time.




## MakeCacheMatrix creates a function that that makes a kind of matrix with 4 functions included. 
## Set that sets the value of the matrix and removes the old free variable. get that grab the value of the matrix.
## setinv that sets the inverted matrix with the solve function. getinv that grabs the inverted matrix.
 

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
  



## cache solves uses the kind of matrix created in makeChaceMatrix and applies the solve function. if the invert already 
## have been calculated it uses the value from the free variabel inv and print a message that it uses a cached value.

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