## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makCacheMatrix():
## This function accepts a matrix parameter as input
## It creates functions to:
##    (a) set the matrix
##    (b) return the matrix
##    (c) set the inverse of the matrix
##    (d) return inverse of the matrix
## It sets the matrix ONLY IF the parameter 'x' is null or if the current matrix
## is different from the earlier matrix
## Otherwise, it helps cacheSolve() use the available matrix and inverse
## So, this helps calculate the inverse even if setMat() and cacheSolve() are directly called
## because x$setMat() ensures that the matrix is initialized and invMat is made Null only if
## it is a different matrix from the previous one
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  x <- NULL
  setMat <- function(matrx)
  {
    if(is.null(x) || !identical(x, matrx))   ## is 'x' null or is it not same as prev matrix?
    {
      print("----------- NEW matrix BEING INITIALIZED -----------")
      x <<- matrx                            ## Only then, init 'x' and make
      invMat <<- NULL;                       ## invMat as NULL; only now, cacheSolve() should
    }                                        ## calculate a new inverse
  } 
  getMat <- function() x
  setInv <- function(invMatrx) invMat <<- invMatrx
  getInv <- function() invMat
  list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve():
## This function takes the list of matrix functions returned by makeCacheMarix(), as input
## It then checks if the current inverse is there; only if there is no inverse (null),
## it calculates inverse; otherwise, it just gets the available inverse and returns it
## The job of ensuring that this inverse belongs to a new matrix (if it was null) is done
## by makeCacheMatrix()
## So, even if the caller just sets a new matrix using x$setMat() and then calls cacheSolve()
## it works fine

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInv()
  if(!is.null(invMatrix))
  {
    print("In cacheSolve(): Reading Cached Inverse")
    return(invMatrix)
  }
  matx <- x$getMat()
  invMatrix <- solve(matx, ...)
  x$setInv(invMatrix)
  invMatrix
}