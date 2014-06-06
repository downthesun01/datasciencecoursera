##The functions will create a matrix, and then find its inverse.
##If the inverse for that matrix has already been found,
##the inverse matrix will be returned from cache, instead of being
##recalculated.

##The first function creates a list that contains a function
##that will se the values of the matrix, get those values, set the values
##of the inverse matrix, and set thos values


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}
}


## The second function finds the inverse of the "matrix" created by
##makeMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setMatrix(m)
  m
}
