## The first function is used to build the object of cache, and matrix, inversed 
##   matrix will be written in the cache. The second function is used to give 
##   out the inversed matrix. If the function found that the inversed matrix 
##   has not been stored in cache, then this function will compute the inversed 
##   matrix, and write it in cache. If the function found the inversed matrix 
##   in cache, then result will be cited from cache, directly.

## This function is used to build the object of cache, and matrix, inversed 
##   matrix will be written in the cache.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
		}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function is used to give out the inversed matrix. If the function found 
##    that the inversed matrix has not been stored in cache, then this function 
##    will compute the inversed matrix, and write it in cache. If the function
##    found the inversed matrix in cache, then result will be cited from
##    cache, directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if (!is.null(m)){
		message("getting cached data")
		return(m)
		}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
        m
}
