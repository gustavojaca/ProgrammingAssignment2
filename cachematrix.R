## This pair of functions operates return the inverse of a given matrix
## using the cache memory to avoid re-calculations


## This function returns a "list" of functions which operates the matrix and
## it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  list(
    get = function() {x},
    set = function(value) {
      x <<- value
      inverse <<- NULL
    },
    get_inverse = function() {inverse},
    set_inverse = function(value) {inverse <<- value}
  )
}

## This function uses the list of functions returned from 'makeCacheMatrix()'
## to check if the inverse matrix was previouly calculated or not and return
## the inverse of a matrix

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("Data Stored in Cache")
  }
  else {
    data <- x$get()
    inverse <- solve(data)
  }
  x$set_inverse(inverse)
  inverse
}
