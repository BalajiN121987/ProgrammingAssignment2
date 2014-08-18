## Put comments here that give an overall description of what your
## functions do

## Create a function that can cache the inverse of a matrix object

makeCacheMatrix <- function(x = matrix()) {
  
##Initialize the inverse  
  m<- NULL
  
##A method to set the matrix  
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  

##A method to get the matrix
  get<-function() 
  {  
    x
  }
  
##A method to set the inverse of a matrix
  setCache<-function(solve) 
  {  
    m<<-solve
  }
  
##A method to get the inverse of a matrix
  getCache<-function()
  {
    m
  }
##Return list of all the methods defined
  list(set=set,get=get,setCache=setCache,getCache=getCache)
}


## Compute the inverse of a matrix defined by makeCacheMatrix
## If the inverse of the matrix has been calculated, this function will
## retrieve the inverse of a matrix from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getCache()

  ## Check if the inverse is already available in cache
  if(!is.null(m)){
    print("Getting cached data")
    return(m)
  }
  
  ## Get the matrix
  data<-x$get()
  
  ##Calculate the matrix inverse
  m<-solve(data)
  
  ##Set the inverse of the matrix
  x$setCache(m)
  
  ##Return the matrix object
  m
}
