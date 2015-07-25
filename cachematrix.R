## makeCacheMtrix is a function that will store inverse of matrix x.  
## If a matrix has been cached, cacheSolve(x) will return that store inverse.

## makeCacheMatrix creates an object m to which the inverse of the matrix is written to.  

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
x1<-CacheMatrix(x)

## cacheSolve checks if there is a stored value m, if so it returns that object.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  print(m)
}
##Running cacheSolve(x1) will return the cached x object.
