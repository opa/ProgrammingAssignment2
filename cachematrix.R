## Bruce Pezzlo 23 November 2014
## Coursera MOOC John Hopkins' R Programming by Roger Peng

## Assignment 2 - practice Lexical Scoping

# "this Programming Assignment will take advantage of the scoping rules of the R language and
# how they can be manipulated to preserve state inside of an R object."

# 2 functions: {makeCacheMatrix ; cacheSolve} used to cache potentially time-consuming calculations
# Maintain a Cache of the inverse of a matrix 
# When requesting the inverse, before calculating, see if its already cached
  #if chached, 
     # return cached 
  #else 
     # calculate inverse using r's solve(x) method, 
     # cache inverse and 
     # return inverse of matrix


## TEST USE CASES:
###  Uncomment below lines and run to test all functional use cases of the assignment
###########################################
# # create 3 test matrixes to test function - two valid and one invalid
# m1 <- matrix(1:4,nrow=2,ncol=2)  # create first test matrix 
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# m2 <- hilbert(8)  #create second test matrix
# m3 <- matrix(1:6,nrow=3,ncol=2) # THIS IS AN UNSOLVABLE MATRIX SHOULD ERROR
# 
# # instantiate makeCacheMatrix and test its functions
# cm <- (makeCacheMatrix(m1)) 
# cacheSolve(cm)
# cacheSolve(cm)  # expecting cached message
# cm$get()
# cm$getInverse()  # expecting cached version
# cm$set(m2)
# cm$getInverse()  # expecting NULL
# cacheSolve(cm)
# cacheSolve(cm)  # expecting cached message
### cm$set(m3) # this is an Unsolvable non-square matrix should error  (uncomment to test)
### cacheSolve(cm) # this SHOULD error - it is an unsolvable request  (uncomment to test)
############################################


makeCacheMatrix <- function(x = matrix()) {
  
  # stores matrix x locally when the function is first called
  # stores inverse matrix im locally when it is first requested
  
#  this function creates and returns a special "vector", which is really a list containing four functions to:
#  set the value of a matrix
#  get the value of a matrix
#  set the value of the inverse of matrix
#  get the value of the inverse of matrix

     im <- NULL  # initialize private stored inverseMatrix
     set <- function(y) {
       x <<- y  # store a matrix 
       im <<- NULL  # reset the privately stored inverseMatrix (no longer valid, its a new matrix)
     }
     get <- function() x  # retrieve the matrix (x is defined when function created and can be redefined with set function)
     setInverse <- function(inversematrix) im <<- inversematrix # cache the inverse matrix
     getInverse <- function() im # retrieve private stored inverseMatrix 
     # return a list of the four functions 
     list( set= set, 
           get = get,
           setInverse = setInverse, 
           getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## This takes in a vector of type makeCacheMatrix (stored as x) and returns an inversematrix from the vectors'
  ## matrix' cached value, calculating its inverse only when its not already in cache

  ## Returns a matrix that is the inverse of 'x'
 
      inversematrix <- x$getInverse()  #retrieve cached IM from the vector x
      if(!is.null(inversematrix)) {  #return the cache
        message("getting cached data")
        return(inversematrix)
      }
      # no cache found, calculate it now
      data <- x$get() # use the matrix from the passed in vector
      inversematrix <- solve(data, ...) #solve for the inverse
               # let default solve(x) function errors bubble up - for example if matrix is not numeric or if it is not a square matrix allow errors to surface
              # assignment allows to assume all passed in matrix will be solveable
      x$setInverse(inversematrix)  #cache the matrix
      inversematrix  #return the inversematrix that was just calculated
}



