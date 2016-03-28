## Put comments here that give an overall description of what your
## functions do

#The first function creates a matrix, then creates the inverse of the matrix.
#The second function checks to see if the inverse has been created. If it has,
# the cached data is displayed. If not, an inverse of the matrix is created and displayed.

## Write a short comment describing this function
#The function sets then gets the value of the matrix.
# The function then sets the inverse of the matrix and caches it.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                    #creates value m
    set <- function(y) {                         # sets the value of the matrix
      x <<- y                                    #caches matrix input so cacheSolve can see if it has changed
      m <<- NULL
    }
    get <- function() x                          #assigns "value of matrix" to get
    setinverse <- function(solve) m <<- solve    #sets inverse value of matrix 
    getinverse <- function() m                   #assigns "inverse value of matrix" to "getinverse"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
# This function checks to see if inverse matrix is created and if it has changed.
# If the inverse matrix is created and unchanged, it is retreived from the cache and displayed.
# If there is no inverse matrix, it is created and displayed.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()             #gets the inverse if it has been calculated
    if( (!is.null(m)) && (identical(m,x$getinverse() ) ) )   {    #checks to see if inverse is present (in m) and if value of matrix has changed
      message("getting cached data")
      return(m)                      #cached data retrieved
    }
    print("no cached data")
    data <- x$get()                 #if no inverse data, it is created
    m <- solve(data, ...)           #creates inverse data
    x$setinverse(m)                 #sets the value of the inverse, caches inverse
    m                               #matrix m displayed
}
## Return a matrix that is the inverse of 'x'

