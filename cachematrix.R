## This function gets an invertible square matrix. 
## Here we assume are matrices in the input have the inverse. 

## We check to see if the inverse is known and if not its inverse will be 
##computed. 

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                set.Invers <- function(inverse) m <<- inverse
                get.Invers <- function() m
                list(set = set, get = get,
                     set.Invers = set.Invers,
                     get.Invers = get.Invers)
     
}


## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get.Invers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set.Invers(m)
        m
        }
