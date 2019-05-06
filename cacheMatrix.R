## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix receives a matrix as an input, sets the value of the matrix,
# gets the value of the matrix, sets the inverse Matrix and get it. 
# The matrix object can cache its own object. 

# The '<<-' operator is used to assign a value to an object in an environment that is different 
# from the current environment 

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  #set the value of the Matrix

  setMatrix <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  
  getmatrix <- function() x                               #get the value of the Matrix               
  setinverse <- function(inverse) invmatrix <<- inverse   #set the value of the invertible matrix
  getinverse <- function() invmatrix                      #get the value of the invertible matrix
  list(setmatrix = setmatrix, getmatrix = getmatrix,
  setnverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# Return the value of the invertible matrix from the makeCacheMatrix function

cacheSolve <- function(x, ...) {

#get the value of the invertible matrix from the makeCacheMatrix function

  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {            #if inverse matrix is not NULL         
    return(invMatrix)                            
  }
  else {   #if value of the invertible matrix is NULL then  
    MatrixData <- x$getMatrix()                    #get the original Matrix Data 
    invMatrix <- solve(MatrixData, ...)            #use solve function to inverse the matrix 
    x$setInverse(invMatrix)                        #set the invertible matrix
    return(invMatrix)                              #return the invertible matrix
  }
  

}
