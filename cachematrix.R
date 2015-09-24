## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(M=matrix()) {
  #Input: Matrix M
  #Output: Cached Matrix 
  #Function: Generates a cache Matrix of the Matrix M
  
  #Initialize inverse Matrix of M: TempInvOfM
  TempInvOfM <- NULL
  #Implementation of the Setter function
  set <- function(Y) {
    M <<- Y #M takes value of Y <<- Operator used to acces environment
    TempInvOfM <<- NULL #Null Pointer
  }
  get <- function() M #get Value for M
  setinv <- function(M_inv) TempInvOfM <<- M_inv #set the inverse of M
  getinv <- function() TempInvOfM
  list(set = set, get = get, #Save all functions in a list
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(M, ...) {
  #Input: Matrix M (e.g. Output of makeCacheMatrix)
  #Output: Inverse Matrix of M
  #Function: Generates a Matrix inverse of M
  
  TempInvOfM <- M$getinv() #access list 
  if(!is.null(TempInvOfM)) { #if TempInvOfM points something
    message("getting cached data") 
    return(TempInvOfM)
  }
  data <- M$get() #Get the get()-function of the list
  TempInvOfM <- solve(data, ...)
  M$setinv(TempInvOfM) #setting data
  TempInvOfM
}