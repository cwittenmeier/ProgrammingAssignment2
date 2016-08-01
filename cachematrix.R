
## This function represents an Object that can store a matrix and its inverse.
## The retrunend Object is a list.
## This list contains a "get"-function, that returns the matrix itself.
## It also returns a "set"- function, that can be used to set the matrix.
## When it is used, the inverse of the matric is set to 'NULL'.
## The inverse has also a "setter" and a "getter"-function.

makeCacheMatrix <- function(x = matrix()) {
        inverse <-NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <-function() x
        setinverse <- function(matinverse) inverse <<- matinverse
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function expects an list that is returned by the makeCacheMatrix-
## function as an argument. It first checks, if the passed Object does already 
## contain the inverse. If yes it just returns it. Otherwise it calculates the 
## inverse and set it in the passed object and returns the calculted inverse.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached inverse")
                return(inverse)
        }
        mat<-x$get()
        matinverse<-solve(mat,...)
        x$setinverse(matinverse)
        matinverse
}