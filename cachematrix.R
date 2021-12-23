## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL         ## initialize object inver as NULL; holds value of matrix inverse
  set <- function(y){   ## define function "set"
    x <<- y             ## to assign new matrix y to be value of x (in parent environment)
    inver <<- NULL      ## if there is a new matrix, then reset the inver to NULL
  }
  get <- function() {x} ## define function "get" to return value of matrix argument
  setInverse <- function(inverse) {inver <<- inverse} ## define function "setInverse" to assign inverse value in inver object in the parent environment
  getInverse <- function() {inver} ## define function "getInverse" to return value of inver object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## creating list to allow calling functions with the $ operator
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inver <- x$getInverse() ## assigning inverse value in x (if present) into inver object
  if(!is.null(inver)) { ## if inver object is not null i.e. has a value
    message("retrieving cached data") 
    return(inver) ## retrieve the cached inverse value
  }
  matr <- x$get() 
  inver <- solve(matr, ...) ## calculating the inverse of the matrix
  x$setInverse(inver) 
  inver 
}
