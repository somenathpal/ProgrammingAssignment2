## 
##There are two functions. The second function (cacheSolve) primarily 
##calculates the inverse of given matrix. 
#But before calculating the inverse the cacheSolve function checks if 
#the inverse is already being calculated. 
#If it is already being calculated then it does not calculate the inverse 
#again instead it just returns the previously calculated value for the cache.
#The first function makeCacheMatrix is a helper function for the second function. 
#It provides the required functionality to store the result of the inverse of the matrix.
#It also provides the getter/setter method for the stored result. 


## makeCacheMatrix create the special matrix and cache's its 
#inverse
#It also provides the following functions to
#1) initialize the matrix and it's inverse to NULL
#2) get the matrix
#3) set the inverse of the matrix
#4) get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(input_matrix) {
        x <<- input_matrix
        inverse <<- NULL
    }
   getMatrix <- function() {
       x
   }
   setInverse <- function(inv) {
       
       inverse <<- inv
   }
   getInverse <- function() {
       inverse
   }
   list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)

}


##short comment describing this function
#calulate the inverse of matrix
#Before calculating check if the inverse is already calcualted
#if it is in cache the return it from cache 
#eles calculate the inverse using the solve function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<- x$getInverse()
    if ( !is.null(inv)) {
        message("Getting cached data..")
        return(inv)
    }
    
    input_matrix <- x$getMatrix()
    inv <- solve(input_matrix,...)
    x$setInverse(inv)
    inv
}
