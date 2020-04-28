##These two functions calculate inverses of matrices. The first function
##is a cache function: it stores a value of the inverse of a matrix. The second
##function finds the inverse of the matrix that first function gives. If the 
##input and output of first function remain same, the second function keep the 
##matrix unchanged.

## This is the first function.It calculates inverse of matrices.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL()
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



##This is the second function. It takes the output of the first function as its
##input.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
