##  A pair of functions that cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) ##define the arguement with model matrix
{
        inverse <- NULL #value of inverse
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x  #gets the matrix x
	setinverse <- function(i) inverse <- i #sets the inverse
	getinverse <- function() inverse        #gets the inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inverse <- x$getinverse()  #gets the inverse and saves it in the variable
        if(!is.null(inverse)) { #checking for cached data
        	message("getting cached inverse")
        	return(inverse)
        }
        data <- x$get()  #get the values and store in data
        inverse <- solve(data, ...) #computes inverse and stores
        x$setinverse(inverse) 
        inverse    #returns the inverse
        
}
