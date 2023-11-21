# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x) {
  # Initialize a variable to store the matrix
  matrix <- NULL
  
  # Function to set the matrix
  setMatrix <- function(newValue) {
    matrix <<- newValue
    # Clear the cache when the matrix is updated
    cache <<- NULL
  }
  
  # Function to get the matrix
  getMatrix <- function() {
    matrix
  }
  
  # Function to set the inverse of the matrix to the cache
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Function to get the inverse from the cache
  getInverse <- function() {
    cache
  }
  
  # Return a list of functions
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inverse <- x$getInverse()
  
  # If not cached, compute the inverse and store it in the cache
  if (is.null(inverse)) {
    matrix <- x$getMatrix()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
  }
  
  # Return the inverse
  inverse
}
