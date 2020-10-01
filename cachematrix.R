## Assuming that the matrix passed as a parameter is reversible, the following functions allow, respectively, 
## to create and cache the matrices (makeCacheMatrix), 
## then calculate the corresponding inverted matrix (cacheSolve). 
## If the inverted matrix has already been calculated (caching) and the matrix has not changed, the inversion is returned instead of recalculating it.


## This function allows you to cache a matrix and the corresponding inverted matrix.
## It takes as parameter a reversible matrix.
## 'x': a reversible matrix,
## 'matrixToInverse': recovers and stores the matrix passed as parameter,
## 'matrixAlreadyInverted': stores the previously processed matrix in order to compare it during the next processing,
## 'inversedMatrix': stores the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  #Cached inversed matrix
  matrixToInverse <- NULL
  inversedMatrix <- NULL
  matrixAlreadyInverted <- NULL
  
  
  get <- function() matrixToInverse <- x
  
  setMatrixAlreadyInverted <- function(givenMatrix){
    matrixAlreadyInverted <<- givenMatrix
  }
  getMatrixAlreadyInverted <- function() matrixAlreadyInverted
  
  setinversedMatrix <- function(inverse) inversedMatrix <<- inverse
  getinversedMatrix <- function() inversedMatrix
  
  list(get = get, setinversedMatrix = setinversedMatrix, getinversedMatrix = getinversedMatrix, setMatrixAlreadyInverted = setMatrixAlreadyInverted, getMatrixAlreadyInverted = getMatrixAlreadyInverted)

}


## This function checks if the cache is available and if it is the same matrix.
## If YES, the cached matrix is returned,
## if NO, the inverted matrix is calculated and cached.
## It takes as parameter the list of objects returned by the first function.

cacheSolve <- function(storedList, ...) {

  ## Return a matrix that is the inverse of 'x'
  
  #get matrix
  inversedMatrix <- storedList$getinversedMatrix()
  
  #Test if 'matrixInverse' is not null and has not changed
  if (!is.null(inversedMatrix) & identical(storedList$get(), storedList$getMatrixAlreadyInverted())) {
    message("getting cached data")
    return(inversedMatrix)
  }
  
  ##Inversed matrix has not been already calculated
  #Store the matrix
  storedList$setMatrixAlreadyInverted(storedList$get())
  
  #Compute the Inverse matrix
  data <- storedList$get()
  inversedMatrix <- solve(data,...)
  storedList$setinversedMatrix(inversedMatrix)
  
  #Print inversed matrix
  inversedMatrix
}
