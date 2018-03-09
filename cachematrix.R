##A program to find the inverse of a matrix and uses a cache
##makeCacheMatrix creates a matrix object to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    #Set vector and get vector
    m <- NULL
    set <- function(y) {
        x <<-y 
        m <<-NULL
    }
    
    get <- function() x
    
    #set the inverse matrix and get the inverse matrix
    setinv <-function(solve) m<<-solve
    
    #get the matrix
    getinv <-function() m
    
    #return a list with the 4 member functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
    
}



#Compute the inverse of the matrix by makeCacheMatrix
#If matrix is already computed, retrieve the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv
    
    #If data is already in cache return it
    if(!is.null(m)){
        message("getting cached data")
        return(m)    
    }
    
    
    #If data isn't there solve the matrix
    
    data <-x$get()
    m<-solve(data, ...)
    x$setinv(m)
    m #return result
    
}