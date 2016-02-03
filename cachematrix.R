# makeCacheMatrix takes an N x K matrix as input and returns a list containing 4 elements:
# 1. A function (set) which sets the value of the input matrix
# 2. A function (get) which gets the value of the input matrix
# 3. A function (setinverse) which sets the value of the inverse of the matrix
# 4. A function (getinverse) which gets the value of the inverse of the matrix
#
# cacheSolve takes in the list created by the makeCacheMatrix function and checks whether the
# inverse of the matrix contained in that list has already been calculated and cached. If it has,
# the cached value is returned along with a message stating that the value is retrieved from
# cached data. Otherwise, the function calculates the inverse of the matrix and returns the
# inverse of the input matrix originally entered into the function makeCacheData
#
# NOTE: The makeCacheMatrix and cacheSolve functions below make use of the "ginv()" function which
# calculates the inverse of a matrix of any dimensions (N x K), not just N x N.
# To use the "ginv()" function requires the MASS package so the first step is to include
# this package prior to setting the functions. This is done by typing in 'library(MASS)' prior
# to defining the functions

library(MASS)

######## makeCacheMatrix ###########
# Input for this function is a matrix e.g.
# f <- makeCacheMatrix(matrix(1:6, 2, 3))
makeCacheMatrix <- function(x = matrix()) 
{
  # Set the matrix
  elc <- NULL
  
  set <- function(y) 
  {
    x <<- y
    elc <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the function which calculates the inverse of the matrix
  setinverse<- function(ginv) elc <<- ginv
  
  # Get the function which calculates the inverse of the matrix
  getinverse <- function() elc
  
  # Return the list with the 4 components as described above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



######## cacheSolve ###########
# Input for this function is the list created by the makeCacheMatrix - the object 'f'
# in the example given in the comments above the definition of the makeCacheMatrix function
# Output:
# The matrix in 'f' is a 2 (rows) by 3 (columns) as follows:
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
#
#The output will give the inverse as follows:
#            [,1]       [,2]
# [1,] -1.3333333  1.0833333
# [2,] -0.3333333  0.3333333
# [3,]  0.6666667 -0.4166667
#
# If the inverse has already been calculated and cached, then the message
# "Get cache data." will be returned right before the inverse matrix is returned
cacheSolve <- function(mat, ...) 
{
  # Check whether the inverse of the matrix has already been calculated
  # If it has then print a message saying that it will be extracted from
  # the cache, return the cached value and exit the function
  inv <- mat$getinverse()  
  if(!is.null(inv)) 
  {
    message("Get cache data.")
    return(inv)
  }
  
  # If the inverse has not yet been calculated, compute the inverse by invoking
  # the relevant components of the makeCacheMatrix function to perform the
  # necessary computation and return the result
  data <- mat$get()
  
  inv <- ginv(data)
  
  mat$setinverse(inv)
  
  inv
}
