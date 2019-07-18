#-------------------------------------------------------------------------------------------------------------------------------#
#                                                                                                                               #
#                                                Coursera - R Programming                                                       #
#                                             Programming Assignment - Week 3                                                   #
#                                                      18/Jul/2019                                                              #
#                                             Ana Sapata (ana_sapata@sapo.pt)                                                   #
#                                                                                                                               #
#-------------------------------------------------------------------------------------------------------------------------------#


# The file is composed by two functions. The first one is compose by other 4 fucntions that set/get the matrix and the respetive inverse,
# the other one receive the result of the first and calculate the inverse matrix, if necessary


# This functions is compose by another 4 functions that
# 1)set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This fucntion use a special "vector" cretaed with the above function. If this vector don't contained the inverse matrix, 
# then the inverse is calculate and returned, otherwise just return it withouth make any calculations
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
