makeCacheMatrix <- function(p=matrix()) {
  inv<- NULL
  set<-function(y) {
    p<<-y
    inv<<-NULL
  }
  get<-function()p
  setinv <- function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set= set, get= get, setinv = setinv, getinv=getinv)
  
}

CacheSolve <- function(p,...){
  
  inv<-p$getinv()
  if(!is.null(inv)){
    message("getting cached result")
    return(inv)
  }
  data<-p$get()
  inv<-solve(data,...)
  p$setinv(inv)
  inv
}