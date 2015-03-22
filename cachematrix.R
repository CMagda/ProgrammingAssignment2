##This function makes inverse of specified matrix

## The first function sets null value to matrix argument and
## declares the function where the value of the function will be catched


makeCacheMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y) {
    
    x<<-y
    i<<-NULL}

  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)}

##Second function compute the inverse for cases 
##that the result is not null and retrieve the inverse matrix.

CacheMatrix <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)}

  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
    
  }

