## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a list that contains a matrix as well as fuctions to
##    get/set the matrix: get() and set() and functions to
##    get/set the inverse of the matrix: getinverse() and setinverse(inverse)
##
## cacheSolve returns the inverse of a given matrix 'x'. It does so by either:
##    (a) returning the cached inverse or by
##    (b) calculating the inverse, storing it in the cache, and returning it
##
## based on the ' cache vector' example of the assignment
## Note: All code assumes that given matrices are invertible
## The code makes use of the '<<-' operator which is described at
## http://cran.r-project.org/doc/manuals/r-release/R-intro.html#Assignment-within-functions

## If you dare you can test the functionality as follows:
## 1. Create a 'makeCacheMatrix' matrix: test = makeCacheMatrix(matrix(1:4, nrow=2))
## 2. Invert the matrix using: testinverted = cacheSolve(test)
## 3. Invert the matrix again using: testinverted = cacheSolve(test)
## The second invocation of cacheSolve will produce the string "getting cached data"
## before returning the result.
## To really see the effect you could try the same but use
## test = makeCacheMatrix(matrix(rexp(1000000), 1000)) in step 1. above
## (rexp generates a random exponential distribution, see also: ?rexp)


## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really a list containing 
## a function to:
##    (1) set the value of the matrix: set(matrix)
##    (2) get the value of the matrix: get()
##    (3) set the value of the inverse matrix: setInverse(inverse)
##    (4) get the value of the inverse matrix: getInverse()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the special "matrix" created with the 
## 'makeCacheMatrix' function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the 'setinverse(inverse)' function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv  
}
