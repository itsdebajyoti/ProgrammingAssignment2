## ## These two function together does a mtrix inversion of a square invertible matrix
## and cache the inverted matrix which helpes not to recompute again the matrix inversion.
#makeCacheMatric takes a input square invertible matrix
#creates a list of 3 functions which are ready 
#to manipulate with the input matrix upon calling
makeCacheMatrix <- function(x = matrix()) {
  matinverse <- NULL
  # not needed
  #set <- function(y) {
  #  x <<- y
  #  matinvesrse <<- NULL
  #}
  #ends not needed
  
  get <- function() x
  setinverse <-function(inverse) matinverse <<- inverse
  getinverse <-function() matinverse
  
  
  #list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  #we can access the list of functional computation 
  #by $get, $setinverse, $getinverse
  list(get = get, setinverse = setinverse,getinverse = getinverse)
}
#returns a output list y 


## cacheSolve returns a matrix that is the inverse of 'x' by using the output of the
## makeCacheMatrix say y as its input.
cacheSolve <- function(y, ...) {  # y = output of makeCacheMatrix(x)}

  # check for cached inverse matrix exist or not.
  finalmatinverse <- y$getinverse()
  if(!is.null(finalmatinverse)) { #if yes retunrs cached matrix
    message("getting cached data")
    return(finalmatinverse)
  }
  else{                          
  matrixdata <- y$get() 
  finalmatinverse <- solve(matrixdata, ...) # else computes the inversion
  y$setinverse(finalmatinverse) # stores the inverted matrix in cache
  return(finalmatinverse) # returns the matrix inverse
  }
}
