## The first function creates a Matrix that can cache it's inverse
## The second function solves the inverse of the matrix Or if the matrix hasn't changed
## Return the cache

## This function creates a special "matrix" Object that can cache it's inverse
## 

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
  }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list( set  = set, get  = get, 
          set_inverse  = set_inverse,
          get_inverse = get_inverse)
    
  
}


## This will calculate the inverse of the matrix or return the cache 
## If the matrix has not changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$set_inverse(inv)
  inv
}
#A test to make sure the functions work.Renamed fun to make it easier to test
fun <- makeCacheMatrix(matrix( 1:4, 2))
fun$get()
fun$get_inverse()
cacheSolve(fun)
cacheSolve(fun)
