
## This function creates an enhanced matrix (holder) that has following functions:
## get/set the value of the matrix and get/set the value of the inverse matrix
## Courtesy (a.k.a copy-pasta of the assignment example)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # getter and setter for the matrix value
  get <- function() x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # getter and setter for the inverse matrix value  
  getInverseMatrix <- function() m
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  # returing list of the previous functions
  list(get = get, set = set, getInverseMatrix = getInverseMatrix, setInverseMatrix = setInverseMatrix)
}


## This function computes the inverse matrix, pushes it to the cache and returns the result
## or it just returns the previously cached result
## Courtesy (a.k.a copy-pasta of the assignment example)
cacheSolve <- function(x, ...) {        
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
