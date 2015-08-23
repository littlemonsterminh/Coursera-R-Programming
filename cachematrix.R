## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL
    	set <- function(y) 
	{
      x <<- y
      inv <<- NULL
    	}
	# return the input matrix
	get <- function() x 
	
	# set the inversed matrix
      setInv <- function(inv) xinv <<- inv 

	# return the inversed matrix
      getInv <- function() xinv 

      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	
	# get the inversed matrix from object x
	m <- x$getInv()
 
      # CASE 1: NULL it will be null if uncalculated, because the first line "xinv <- NULL" in the previous function
      if(!is.null(m)) 
	{ 
		# if the inversion result is there
	  	message("getting cached data")
	  	return(m) # return the calculated inversion
      }
	
	# CASE 2: we do x$get to get the matrix object
      data <- x$get()  
	#solve is a built-in method to calculate inverse of the matrix: http://www.statmethods.net/advstats/matrix.html
      m <- solve(data)  
      # we then set it to the object
	x$setInv(m) 
      m # return the solved result
}

## TESTl
## Creating the metrix: 
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## Getting the inverse: 
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)

##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
