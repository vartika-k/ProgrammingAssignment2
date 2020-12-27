##Two functions that cache the inverse of a matrix


## makeCacheMatrix function creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) inv <<- inver
  getinverse <- function() inv
  list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special matrix 
## OR
## If inverse is calculated previously, it should retrieve it from cache 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

