## Set the input x as a matrix and then set solve value "z" as a NULL

## Changed every reference to "mean" to "Inverse"

makeCacheMatrix <- function(x = matrix()) {
        z = NULL
        set <- function(y){
                x <<- y
                z <<- NULL
  }
        get  <- function()x
        setInverse <- function(inverse) z <<-inverse
        getInverse <- function() z
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)    
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        if(is.null(z)){
        message("getting cached data")
        return(z)
        }
        mat <- x$get()
        z <- solve(mat,...)
        x$setInverse(z)
        z
}
