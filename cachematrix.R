## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(M=matrix()) {
  #Initialize inverse Matrix of M: TempInvOfM
  TempInvOfM <- NULL
  set <- function(Y) {
    M <<- Y
    TempInvOfM <<- NULL
  }
  get <- function() M
  setinv <- function(M_inv) TempInvOfM <<- M_inv
  getinv <- function() TempInvOfM
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(M, ...) {
  TempInvOfM <- M$getinv()
  if(!is.null(TempInvOfM)) {
    message("getting cached data")
    return(TempInvOfM)
  }
  data <- M$get()
  TempInvOfM <- solve(data, ...)
  M$setinv(TempInvOfM)
  TempInvOfM
}