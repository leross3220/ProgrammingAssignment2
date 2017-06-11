## Two functions that work together to cache the inverse 
## of a matrix 

## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
    
  }
  get<-function()x
  setinverse <-function(solve) m <<- solve
  getinverse <-function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates or retrieves the inverse matrix from the object created
## in makeCacheMatrix by using the functions defined in the earlier matrix. 

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m
}
