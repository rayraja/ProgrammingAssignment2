
##################################        Example       #######################################################################

# > m <- makeCacheMatrix(matrix(c(1, 3, 2, 4), c(2, 2)))


# first run  - no cache present - computation takes place
# > cacheSolve(m)
#       [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5


# second run  - cache present - data retrieved from cache
# > cacheSolve(m)
# getting cached data
#       [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5


##################################            Assumptions     #################################################################

# For this assignment, we assume that the matrix supplied is always invertible.

# For this assignment, we assume that the matrix supplied is square, allowing the use of the solve function in R

##################################            Notes            ################################################################


# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing functions to

# (1) Set the value of the matrix (setMatrixValue)
# (2) Get the value of the matrix (getMatrixValue)
# (3) Set the value of the inverse (setInverseValue)
# (4) Get the value of the inverse (getInverseValue)

# The second function, cacheSolve calculates the mean of the special "matrix" created with the first function. However, it first 
# checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.


##################################        makeCacheMatrix       ################################################################


# Declare function "makeCacheMatrix" and assign the argument as type matrix equal to variable "x"
makeCacheMatrix <- function(x = matrix()) {
  
  # define a variable for the cache and it to NULL
  cacheValue <- NULL
  
  # declare function "setMatrixValue" to store a matrix value 
  setMatrixValue <- function(matrixValue) {
    x <<- matrixValue
    # flush the cache
    cacheValue <<- NULL
  }
  
  # declare function "getMatrixValue" to return the stored matrix value (x)
  getMatrixValue <- function() {
    x
  }
  
  # declare function "setInverse" to calculate the inverse of the matrix value then cache value using the <<- operator
  setInverseValue <- function(solve) {
    cacheValue <<- solve
  } 
  
  # get the cached value
  getInverseValue <- function() {
    cacheValue
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrixValue = setMatrixValue, 
       getMatrixValue = getMatrixValue, 
       setInverseValue = setInverseValue, 
       getInverseValue = getInverseValue)
}

##################################        End Of Function     ##################################################################


# The second function, cacheSolve calculates the mean of the special "matrix" created with the first function. However, it first 
# checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.


##################################        cacheSolve           #################################################################

cacheSolve <- function(y, ...) {
  
  # retrive the the cached value from getInverseValue
  # Assign y$getInverseValue() to variable inverseMatrix
  inverseMatrix <- y$getInverseValue()
  
  # if a cached value exists return it
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # if no cache value exists, caclulate the inverse and store it in the cache
  # Assign y$getMatrixValue() to variable newCacheData
  cacheValue <- y$getMatrixValue()
  
  # calculate inverse using solve function
  inverseMatrix <- solve(cacheValue)
  y$setInverseValue(inverseMatrix)
  
  # return the inverse matrix value
  inverseMatrix
}

##################################        End Of Function     ##################################################################