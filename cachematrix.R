# This script allow R to calcul and save in memory the inverse of a matrix to make easier its 
#future recall

#Function makeCacheMatrix is made to support cachesolve funtion, feeding parent's environment
# via pointers.


makeCacheMatrix <- function(x=matrix()){
   m <- NULL  #initialy m is assign to NULL (child level)
  
  set <- function(y) { 
    # this part of function will be used only when call x$set to modify matrix directly
    x <<- y
    m <<- NULL 
    
  }

  get <- function() x 
  
  setsolve <- function(solve) m <<- solve 
  getsolve <- function() m
  
  # list below is created to make easier and more readable the use of those functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


# The goal of this function is function is to get the inverse of a matrix. 

# The first step and most critical, will be to establish m value. For that, it call x$getsolve 
# which return m (m value is looked first inside the current function environment then,
# where that function was defined (child level of makeCacheMatrix environment)

# Two case are possible:
  ## 1. this matrix have not been proceed before (case of m is NULL), 
  ## so it will calcul the inverse of the matrix and return its value to the parent environment 
  ## via the object m
  ## 2. the inverse of this matrix have been find before, so it will call the cache inverse matrix

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...) 
  x$setsolve(m) 
  m 
}