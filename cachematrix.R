

## 2 fns makeCacheMatrix, makeCacheMatrix
## makeCacheMatrix consists of set,get,setinv, getinv
##library(MASS) used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL            #initializing inv as NULL
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                   }
  get<-function()x             #fn to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){ 
                    inver<-ginv(x)
                    inver%*%x           #fn to obtain invof the matrix
                    }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this fn
## get the cache data

cacheSolve <- function(x, ...) ##gets cache data      
  {
  inv<-x$getinv()                  
  if(!is.null(inv)){                 #checking whether inv is NUll 
                     message("getting cached data!")
                     return(inv)                       #ret inv value
  }
  data<-x$get()
  inv<-solve(data,...)              #cal inv value
  x$setinv(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}
