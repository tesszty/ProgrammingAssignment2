makeCacheMatrix<-function(x=matrix()) {   #change to matrix
    inv <- NULL #change mean to inv
    set <- function(y) { # set the value
        x <<- y
        inv <<- NULL
    }
    get <- function() x # get the value
    
    setinverse <- function(inverse) inv <<- inverse # set the inv
    getinverse <- function() inv # get the inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve<- function(x,...){
    inv<-x$getinverse()
    
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) #compute inverse with the suggested function solve
        x$setinverse(inv)
        inv
}
