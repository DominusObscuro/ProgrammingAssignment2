## Put comments here that give an overall description of what your
## functions do
#it sets a function, gets a function, sets the inverse and gets the inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<NULL
  
  #set function of matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  #get function of matrix
  get<-function()x
  
  #set inverse
  setinverse<-function(inverse) inv<<-inverse
  
  #get inverse
  getinverse<-function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function
#it decides if it's inversed, if yes it retrieves the information, if not it calculates the new information

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #is inverse computed?
  inv<-x$getinverse()
  
  #if yes
  if(!is.null(inv)) {
    #retrieve computed inverse
    message("getting cached data")
    return(inv)
  }
  
  #if not get matrix
  data<-x$get()
  
  #find inverse
  inv<-solve(data, ...)
  
  #cache result
  x$setinverse(inv)
  
  (return)inv
}
