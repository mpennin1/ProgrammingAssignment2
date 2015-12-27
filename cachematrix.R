## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
        inv = NULL
        set = function(x) {
      	mat <<- x
                inv <<- NULL
        }
        get = function() mat
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(data, ...) {
        inv = data$getinv()
        if (!is.null(inv)) {
	        return(inv)
	}
        mat.data = data$get()
        inv = solve(mat.data, ...)
        data$setinv(inv)  
        return(inv)
}
