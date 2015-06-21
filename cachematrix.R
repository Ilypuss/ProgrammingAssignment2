# The two functions defined here deal with the invertion of a matrix.
# They assume the matrix is square and is mathematically able to be inverted.
# They attempt to use a cached copy of a previous inversion if possible to save time
# They also demonstrate use of <<- assignment, opening up the scope of the variables
# stored such that they are available between two functions.
#
# makeCacheMatrix returns a list of functions to deal with the administration
# of the matrix being inverted and the result of any inversion.
# cacheSolve returns the inversion of a matrix with help from the functions
# created by the other function.


# Sumary of makeCacheMatrix: 
# Takes as an argument a matrix or creates a null one
# Returns the list of functions described below to administrate that 
# matrix and any previously calculated inversions of it.
#
# setMatrix              sets the value of a matrix and flushes the cache of its inverse
# getMatrix              retrieves the value of the current matrix
# cacheInversionResult   stores a cached copy of an inversion
# getInverse             get the cached value or null if not calculated
#                        before or if the matrix has changed

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialise the store of the matrix to to NULL
  matrixInversionCache <- NULL
  
  # Store the matrix with the one passed and as we are changing
  # the matrix, any previously cached inversion is rendered irrelevent 
  # so we wipe that cache.
  # This step means that testing for a null cache will test both that:
  # 1) the inverse hasn't already been calculated
  # 2) and it determines whether the matrix has changed and therefore
  #    the cached inversion is moot.
  setMatrix <- function(newValue) {
    x <<- newValue                    # Store matrix
    matrixInversionCache <<- NULL     # Flush inversion result cache
  }
  
  # Gets (returns) the current matrix
  getMatrix <- function() {
    x
  }
  
  # Cache the result of the inversion for later retrieval
  cacheInversionResult <- function(inversionResult) {
    matrixInversionCache <<- inversionResult
  }
  
  # Get the cached value by return.
  getInverse <- function() {
    matrixInversionCache
  }
  
  # The final line of this initialisation function returns the four functions
  # Each of them identified and hence callable by name.
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInversionResult = cacheInversionResult, getInverse = getInverse)
}


# cacheSolve: Takes an argument the list of functions created with function above.
# Returns an inversion by calculating it using 'Solve()' or more quickly by using a  
# previously calculated cached copy of the inverted form.
# It also lets the user know if it has saved time by using that cache.

cacheSolve <- function(x, ...) {
  
  # Try to retrieve a cached value.
  inverted <- x$getInverse()
  
  # Test to see whether or not it is null, either a result of the matrix having just
  # been setup a first time, or if the matrix has changed and hence had the cache flushed.
  # If not null then let the user know and progress to the end to return the cache.
  if(!is.null(inverted)) {
    message("Using cached data")
  }
  
  # else, i.e. if it is null, then {
  #   retrieve the matrix using the appropriate function from the list
  #   calcualate it's inverse
  #   use the caching function from the list to store the result for later use.
  #   }
  else {
  matrixToBeInverted <- x$getMatrix()
  inverted <- solve(matrixToBeInverted)
  x$cacheInversionResult(inverted)
  }
  # Finally return the inverted matrix
  inverted
}