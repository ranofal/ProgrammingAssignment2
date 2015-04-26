
##This function create a speical "vector" which is really a list containing a function to
##set the value of the matrix m
##get the value of the matrix m
##set the value of the inverse matrix im
##get the value of the inverse matrix im

makeCacheMatrix <- function(m = matrix()) {
    im <- NULL  #first inverse matrix is Null, 
    set <- function(y) { #set function saves the matrix m and the inverse matrix to Null 
        m <<- y 
        im <<- NULL
    }
    get <- function() m  #get function simply returns matrix m
    setinv <- function(m) im <<- solve(m) #function calculate the inverse matrix matrix 
    getinv <- function() im  #function returns inverse matrix
    list(set = set , get = get, setinv = setinv, getinv = getinv)
}


## This function finds the inverse of the special "matrix" created with the above function
## However, if first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinv function 

cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of matrix m
    im <- m$getinv() #first check if the inverse matrix is cached
    if (!is.null(im)) { #if yes 
        message("getting cached data") #simply type a message
        return(im) #then simply returns the cached value the inverse matrix
    }
    data <- m$get() #get the matrix 
    im <- solve(data) #calculate the inverse matrix 
    m$setinv(im) #cache the inverse matrix 
    im #retun the inverse matrix
}

##This function calc the time when calling inverse function 
##The point is to check if the cached inverse matrix are retrieved faster
calcTime <- function(m) {
    st <- Sys.time()
    im <- cacheSolve(makeCacheMatrix(m))
    #print(im)
    et <- Sys.time() 
    tt <- et - st 
    message("time taken ",tt)
    im
}
