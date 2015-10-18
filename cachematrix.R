## Programming Assignment 2 - R Programming
## Data Science Specialization Track

## This file contains all that is necessary in order to complete the
## programming assignment #2 for the `R-Programming` course at Coursera.

## It is used in order to make a matrix and the give it hte ability to
## make an inverse and cache the results (since the creation of a matrix's
## inverse can be an expensive process).

## Sample Overall Usage:

## # Build a matrix
## m <- matrix(1:4, nrow=2, ncol=2)

## # Make the `cached` matrix
## cached <- makeCacheMatrix(m)

## # Calculate the inverse of the matrix
## # **Please Note**:
## # This time around, it will not hit the cached version of the matrix
## inv <- cacheSolve(cached)

## # Now, it is time to try to calculate the inverse again
## inv2 <- cacheSolve(cached)
## # Note: This time you will see a message "getting cached data"

# This function is used in order to make a matrix that has the
# cache-ability functionality.  It will take in any matrix and
# prepare an object that will allow us to access both the value,
# and the cached matrix.
makeCacheMatrix <- function(matrix = matrix()) {
  
  # Clear out our local inverted matrix
  inverted <- NULL
  
  # Define the "setter" functionality
  set <- function(data) {
    # Set the matrix with the new data
    matrix <<- data
    
    # Clear out the inverted matrix because the cache
    # is no longer valid
    inverted <<- NULL
  }
  
  # The "getter" for the current matrix
  get <- function() {
    matrix
  }
  
  # Set the calculated inverse with whatever is provided
  setinverse <- function(inv) {
    inverted <<- inv
  }
  
  # Retrieves the inverse of a matrix
  getinverse <- function() {
    inverted
  }
  
  # Return the available public methods
  list (
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse
  )
}


# This function is used in order to either retrieve either a 
# cached version of the inverted matrix, or calculate it and then
# automatically cache it for future use.

# Assumptions:
#  i) `matrix` parameter provided is an object created by the `makeCacheMatrix` function
#  ii) The matrix which has been provided is invertible
cacheSolve <- function(matrix, ...) {
  
  # Check to see if the inverse of the matrix has already been set
  inverse <- matrix$getinverse()
  
  # Did it have data?
  if(!is.null(inverse)) {
    # It did have data, so return a message and we are done
    message("getting cached data")
    
    # Return the inverse
    return(inverse)
  }
  
  # The data wasn't cached, so we need to calculate the inverse
  
  # Grab the matrix data
  data <- matrix$get()
  
  # Solve the inverse
  inverse <- solve(data, ...)
  
  # Cache the data
  matrix$setinverse(inverse)
  
  # Return the inverse
  inverse
}
