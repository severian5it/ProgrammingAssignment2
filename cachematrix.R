## two function to avoid heavy computation in the case of a matrix inversion
## the function makeCacheMatrix is caching the matrix and its inverse
## the function cachesolve try to access the cached valued of the inverse:
## if it exist return a message and access the cached inverse
## if it it doesn't call the solve function but cache the inverse for future access

## the function below creates list containing a function to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## get the value of the matrix
        get <- function() x
		## set the value of the inverse
        setsolve <- function(solve) m <<- solve
		## get the value of the invers
        getsolve <- function() m
		## special matrix definition
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function below calculates the inverse of a matrix . first thing it checks whether exist the cache created with the above function. 
## if it exists it access it returning a getting data message, skipping the computation
## if it doesn't exist it compute it and cache the computation for future access.


cachesolve <- function(x, ...) {
		## first access to the cache
        m <- x$getsolve()
        if(!is.null(m)) {
				## inverse is cached
                message("getting cached data")
                return(m)
        }
		## inverse is not cached
        data <- x$get()
        m <- solve(data, ...)
		## caching for future access
        x$setsolve(m)
		## Return a matrix that is the inverse of 'x'
        m
}
