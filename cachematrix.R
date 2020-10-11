## Here we construct two functions MakeCacheMatrix which stores the matrix and caches its inverse
## and CacheSolve whch returns inverse of the cache matrix

## This function creates a special object saves the matrix of the matrix and then caches(collects) its inverse

makeCacheMatrix <- function(x = matrix()) {
   i<- NULL
   set<- function(y){
     x<<-y
     i<<-NULL
   }
   get<-function() x
   setinverse<-function(inverse) i<<-inverse
   getinverse<- function() i
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## this function will calculate the inverse of the matrix returned by makeCacheMatrix if the inverse is already calculated from the previous function retrieve it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i<-x$getinverse()
      if(!is.null(i)){
        message("retrieving the inverse")
        return(i)
      }
      data<-x$get()
      i=solve(data, ...)
      x$setinverse(i)
      i
}
