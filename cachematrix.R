# =======================
# These functions will create a special matrix, in wich its inverse will be 
# cached on calculation, as to avoid computing time if the inverse is 
# already calculated.
# =======================

# -----------------------
# This function creates the special matrix, wich is a list containing functions
# to set/get the matrix, and set/get its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m_inverse <<- inverse
  getInverse <- function() m_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# -----------------------
# This function calculates the inverse of a matrix. If it was already 
# calculated, it will return the cached value.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m_inverse <- x$getInverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data, ...)
  x$setInverse(m_inverse)
  m_inverse
}
