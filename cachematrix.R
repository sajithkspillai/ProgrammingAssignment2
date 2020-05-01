## The first function makeCacheMatrix take matrix as input and output a special matrix object,list of functions 
## with matrix value.The second function takes the special matrix object as input and share the cached 
## inversion matrix of the matrix held by the object. if the cached matrix is missing then it is recalculated.

## makeCacheMatrix function has functions set to clear cache,get to get matrix value, 
## getinv to give inverse matrix and setinv to generate inverse matrix
## All functions shared as a  list. Additional check placed to confirm input is a Matrix and square.

makeCacheMatrix <- function(x = matrix()) {
  if(class(x)!="matrix"){stop("not a matrix")}        #check if matrix
  if(nrow(x)!=ncol(x)){stop("not square matrix")}     #check if square
  
  y<-NULL                                             #inverse is set to null
  
  
  #variable$setMatrix(matrix) eg: l$set()x will clear the earlier value of matrix and assign new value.
  ##Note--**if matrix() is empty NA will be generated as output for cachesolve(variable);cacheSolve(l)
  setMatrix <-function(m=matrix()){
    x<<-m                                              #input matrix value assigned to storage variable
    y<<-NULL                                           #inverse value is cleared
  }
  
  getMatrix <- function() x                        #function give the earlier input in next function
  
  setInv <- function(x) y<<-solve(x)              #input is passed to solve and output inversion stored in y
  getInv <- function() y                          #function give the earlier inversion output
  
  list(set=setMatrix,get=getMatrix,setCache=setInv,getCache=getInv) #list of functions
}


## cacheSolve function take Matrix object as input and generate the inverted matrix

cacheSolve <- function(x, ...) {
  y<-x$getCache()                                     #Inverted matrix is otained using matrix object(list)
  if(!is.null(y)) {                                   # true when setclear=set has been used.
    message("getting the cached data..")
    return(y)
  }
  
  m<-x$get()                                          #The original matrix is received.
  y<-x$setCache(m)                                    #solve(matrix)    for inversion
  x$getCache()                                         #store Inversion matrix in cache
  
  #Returns the inverse of the matrix 'x' when set is used.
  y
}
