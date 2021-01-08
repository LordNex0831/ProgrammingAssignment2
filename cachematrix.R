## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(m=matrix()){
  i<-NULL
  
  ## Setting the matrix
  set<-function(matrix){
    m<<-matrix
    i<<-NULL
  }
  
  ##Getting the matrix
  get<-function(){
    m
  }
  
  ##Setting the inverse of matrix
  setInverse<-function(inverse){
    i<<-inverse
  }
  
  ##Getting the inverse of matrix
  getInverse<-function(){
    i
  }
  
  ## Returning lisst of methods
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cahceSolve<-function(x,...){
  m<-x$getInverse()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)%*%data
  x$setInverse(m)
  m
}
