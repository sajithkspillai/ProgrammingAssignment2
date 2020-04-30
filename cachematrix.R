## The first function makeCacheMatrix take matrix as input and output a special matrix object,list of functions 
## with matrix value.The second function takes the special matrix object as input and share the cached 
## inversion matrix of the matrix held by the object. if the cached matrix is missing then it is recalculated. 
## 

## makeCacheMatrix function has two functions setCache and getCache which set inverse and get the original Matrix
## input used in the function. Additional check placed to confirm input is a Matrix and square.

makeCacheMatrix <- function(x = matrix()) {
  m<-x
  if(class(x)!="matrix"){stop("not a matrix")}
  if(nrow(x)!=ncol(x)){stop("not square matrix")}
  
  setCache <-function(x= m){
    y<<-solve(x)
  }
  getCache<-function() x
  list(set=setCache,get=getCache)
}


## cacheSolve function take Matrix object as input with inversed matrix as input and provide the original matrix

cacheSolve <- function(x,...) {
  m<-x$get()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
}