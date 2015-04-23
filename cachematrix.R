## The two functions below create a cached version of the "solve" function
## as it is used to invert a square matrix. The first function creates an
## object to hold both the original matrix as well as any previously 
## computed (cached) copies of the inverse of that matrix. The second
## function checks for previously cached copies of the inverse before
## calling the standard "solve" function.

## "makeMatrix" takes a square matrix as argument. It will return a list 
## that includes the functions to access and change both the original 
## matrix and its inverse once it is calculated.

makeMatrix <- function(x = matrix()) {
  xInverse <- NULL 

  # "set" and "get" are accessors to the matrix "x":
  set <- function(y) {
    # In order to affect variables at the parent level, 
    # the "superassignment" operator (<<-) must be used for "set"
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  
  # "setInverse" and "getInverse" access the matrix "xInverse":
  # Again, "superassignment" operator is necessary to affect parent scope
  setInverse <- function(matrixInverse) xInverse <<- matrixInverse
  getInverse <- function() xInverse

  # A list of the four accessor functions is returned
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## "cacheSolve" takes the result of "makeMatrix", a list of functions, 
## and returns the inverse of the matrix to which those functions have
## access. If the matrix inverse has been computed already, it will use
## that value. Otherwise, it will compute it using "solve", and use the
## "setInverse" accessor function to cache the result.

cacheSolve <- function(x, ...) {
  ## First, check to see if the inverse is cached already
  xInverse <- x$getInverse()
  if(!is.null(xInverse)) {
    message("getting cached data")
    return(xInverse)
  }
  
  ## If the inverse was not yet computed, access the original matrix:
  data <- x$get()

  ## compute its inverse:
  xInverse <- solve(data, ...)

  ## cache the inverse:
  x$setInverse(xInverse)
  
  ## and last, return the inverse:
  xInverse
}

## code can be tested by sourcing and comparing to "solve":
##
## ## sourcing may vary based on architecture:
## source('~/R/ProgrammingAssignment2/cachematrix.R') 
## randomMatrix <- matrix(rnorm(1000000),nrow=1000,ncol=1000)
## inverseRandomMatrix <- solve(randomMatrix)
## cachedRandomMatrix <- makeMatrix(randomMatrix)
## identical(inverseRandomMatrix,cacheSolve(cachedRandomMatrix))
## identical(inverseRandomMatrix,cacheSolve(cachedRandomMatrix))
## ## Both "identical" commands should return TRUE, 
## ## second run should return much faster 
## ## (and with printed message, "getting cached data")