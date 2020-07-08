## makeCacheMatrix stores a matrix, can change its values, stores the inverse
# and can return the value of the inverse
## cacheSolve returns the value of the inverse if its cached otherwise it
# proceeds to calculate it


##Both functions where tested with the examples provided in the 
## discussion forum of week3 on coursera

#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#myMatrix_object <- makeCacheMatrix(m1)
#cacheSolve(myMatrix_object)

#n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
#myMatrix_object$set(n2)
#cacheSolve(myMatrix_object)


# makeCacheMatrix creates an especial matrix that can specify the 
# values of a matrix (Set section of code), can obtain what are the values
# in the matrix (get section of code), can store the value of the inversion
# (setinv section of code) and can retrieve the value of the inversion 
# (getinv section of code)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversion) inv <<- inversion
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# CacheSolve returns the inverse of a matrix, first it looks the value of the
#inverse in the object m (that is an object of type makeCacheMatrix ), 
#if its not null (the inverse is already calculated) 
# then it prints the value, otherwise it proceeds to get the values of the 
# matrix, calculates the inverse with the 'solve' function 
# and stores it so is cached next time the function is called


cacheSolve <- function(m, ...) {
  inv <- m$getinv()
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}
