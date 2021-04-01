#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  #mc=matrix cache
  #set mc initial value to NULL
  mc <- NULL
  # function to set value of the matrix
  set <- function(y){
   # double arrow operator can allow one to modify variables in parent levels 
    #(closer works from parent function).
     x <<- y
     # will modify mc later
    mc <<- NULL
  }
  #next get the value of the matrix
  get <- function(){x}
  #make function to set the value of the set inverse
  setInverse <- function(inverse) {mc <<- inverse}
  #make function to get the value of the set inverse
  getInverse <- function() {mc}
  # create a list 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x' then assigns it to the object mc
  mc <- x$getInverse()
  # check to see if inverse is calculated with if statement
  if(!is.null(mc)){
    #display message and return the inverse of the matrix
    message("grabbing cached data")
    return(mc)
  }
  mat <- x$get()
  # use the solve function to get inverse (assume matrix is symmetrical)
  mc <- solve(mat,...)
  #set value of the inverse matrix to the cache
  x$setInverse(mc)
  mc
}
# attempted to use a symetrical matrix got an error. 
mat1<-makeCacheMatrix(matrix(1:20,nrow=10, ncol=10))
mat1$get()
mat1$getInverse()                      
cacheSolve(mat1)
