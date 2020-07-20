## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function creates a matrix that returns list of functions:-
## 1.set function to set matrix
## 2.get function to get matrix
## 3.setinverse function to set inverse of matrix to i.
## 4.getinverse function to get inverse of matix.
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inv) i<<-inv
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve function solves the x invertible square matrix to inverse(i).
## It checks whether inverse of x is calculated or not,if not,then it compute the inverse.
## It calculates the inverse by solve() function.
cacheSolve <- function(x, ...) {
      i<-x$getinverse()
      if(!is.null(i)){
        message("getting catched data")
        return(i)
      }
      data<-x$get()
      i<-solve(a=data,...)
      x$setinverse(i)
      i
}

