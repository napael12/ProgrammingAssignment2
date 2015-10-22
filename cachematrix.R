##  makeCacheMatrix function 'wraps' matrix and defines methods to 
## 1) set/get value of matrix
## 2) get value of matrix inverse
## 
## both methods provide access to variables from another context -- this way 
## makeCacheMatrix can be used to retreive values from another function defining it
##
## run 'testMyWork() function to test
##
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

##
## cacheSolve takes makeCacheMatrix as parameter and: 
##  1) solves the inverse
##  2) stores inverse matrix in 'cache'
##  if cached value exists, it returns. if cached value does not exist
##  calculate inverse of matrix and cache its value
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## get matrix data
  data <- x$get()
  
  ## solve inverse
  m <- solve(data, ...)
  
  ## stoer inverse data
  x$setinverse(m)
  m
}

##
## function to test cacheSolve 
##
testMyWork<-function() {
  
  m<-matrix(diag(c(2,1)), nrow =2, ncol=2)
  
  ##create test matrix
  print("Created Matrix: ")
  print(m)
  
  inv<-solve(m)
  print("Inverse of Matrix: ")
  print(inv)
  
  ##make cache matrix
  mc<-makeCacheMatrix(m)
  
  ##cache solve
  test1<-cacheSolve(mc)
  print("Use cacheSolve to solve inverse of matrix")
  print(test1)
  
  test2<-cacheSolve(mc)
  print("Use cacheSolve to display inverse of matrix")
  print(test2)
  
  ##test inverse vs. 'cacheSolved' matrix
  if (inv[2,2] == test1[2,2] && inv[2,2] == test2[2,2]) {
    print("It wokrs")
  }
  else {
    print("Does not work")
  }
}
  