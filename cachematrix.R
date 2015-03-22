## Creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  #store the cached inverse matrix
  inv <- NULL 
  
  # Setter for the matrix
  set <- function(y) 
  {  
    x <<- y
    inv <<- NULL
  }
  
  # Getter for the matrix
  get <- function() x  
  
  # Setter for the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Getter for the inverse
  getinverse <- function() inv
  
  # Return
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  # If the inverse has already been calculated, retrieve it
  if(!is.null(inv)) 
  {
    message("getting cached data...")
    return(inv)
  }
  
  # If the inverse has yet to be calculated, calculate it
  data <- x$get()
  inv <- solve(data,...)
  
  # Cache
  x$setinverse(inv)
  
  #Return
  inv
}

#example

# x <- matrix(c(1,4,4,1),nrow=2)
# cmx$get()
#     [,1] [,2]
#[1,]    1    4
#[2,]    4    1

#cacheSolve(cmx)
#            [,1]        [,2]
#[1,] -0.06666667  0.26666667
#[2,]  0.26666667 -0.06666667

#cacheSolve(cmx)
#getting cached data...
#            [,1]        [,2]
#[1,] -0.06666667  0.26666667
#[2,]  0.26666667 -0.06666667