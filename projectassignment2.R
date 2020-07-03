# The first function, `makeCacheMatrix` creates a list containing a function to
# 
# 1.  set the value of the matrix
@@ -12,16 +13,16 @@
  makeCacheMatrix <- function(x = matrix()) {
    
    mat_inv <- NULL
    set <- function(y) {
      set <- function(y) { #Set the value of the matrix
        x <<- y
        mat_inv <<- NULL
      }
      get <- function() x
      setmat <- function(solve) mat_inv <<- solve
      getmat <- function() mat_inv
      get <- function() x #Get the value of the matrix
      setmat <- function(temp) mat_inv <<- temp # sets the value of the inverse of the matrix
      getmat <- function() mat_inv #gets the value of the inverse of the matrix
      list(set = set, get = get,
           setmat = setmat,
           getmat = getmat)
      getmat = getmat) #Prepares the list to pass to the cacheSolve()


    }
    @@ -36,17 +37,20 @@ makeCacheMatrix <- function(x = matrix()) {
      
      cacheSolve <- function(x, ...) {
        
        mat_inv <- x$getmat()
        mat_inv <- x$getmat() # gets the value that was calculated
        if(!is.null(mat_inv)) {
          message("getting cached matrix")
          return(mat_inv)
          return(mat_inv) #If there is a value stored it prints it
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$setmat(mat_inv)
        mat_inv
        mat_inv #If there is no value stored then it calculates the inverse of the matrix and prints it
        
      }