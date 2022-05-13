
# Creating the makeCacheMatrix function -----------------------------------------------------------

# Creating the 4 functions to make inverse
makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  #Defining Variables
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  
  get <- function() x
  # setting inverse
  SetInverse <- function(solve) Inverse <<- solve
  
  GetInverse <- function() Inverse
  
  list(set = set, 
       get = get,
       SetInverse = SetInverse,
       GetInverse = GetInverse)
}

# Creating cacheSolve function --------------------------------------------

#checking if inverse is calculated 
cacheSolve <- function(x, ...) {
  Inverse <- x$GetInverse()
  if(!is.null(Inverse)) {
    message("Getting cached inverse data")
    return(Inverse)
  }
  #Inverse not calculated therefore calculating it
  data <- x$get()
  Inverse <- solve(data, ...)
  x$SetInverse(Inverse)
  Inverse
}


# Testing -----------------------------------------------------------------


MyMatrix <-matrix(1:4,2,2)

Test_1<- makeCacheMatrix(MyMatrix)

cacheSolve(Test_1)
