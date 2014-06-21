## makeCacheMatrix & cacheSolve
#  Two functions used in tandem to implement matrix
#  inversion, and returning of a cached result
#  when the inversion has already been computed.
#  The functions assume the matrix is invertible.
#
#  makeCacheMatrix returns a list containing the
#  the following 4 functions:
#   set: initializes a CacheMatrix
#   get: returns the matrix in CacheMatrix
#   setInverse: sets the matrix inverse
#   getInverse: gets the matrix inverse
#
#  cacheSolve returns the cached inverse if already
#  computed, otherwise the inverse is computed for
#  the first time.
#
#  Usage:
#   set.seed(1)
#   matrix_x<-matrix(rnorm(4*4,mean=0,sd=1),4,4) # generate a 4 x 4 matrix
#   CacheMatrix_x<-makeCacheMatrix(matrix_x)     # make a CacheMatrix
#   matrix_xinv<-cacheSolve(CacheMatrix_x)       # get the inverse


###############################################################################
## makeCacheMatrix
#
# Arguments:
#  x   a matrix
#
#  Returns a list containing 4 functions
#  (set, get, setInverse, getInverse)
#  that are explained in the overall description above
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(        set = set
               ,       get = get
               ,setInverse = setInverse
               ,getInverse = getInverse)
}


###############################################################################
## cacheSolve
# Arguments:
#  x   a list created with makeCacheMatrix
#
#  Returns the inverse of the matrix cached in x.
#  If a cached inverse is already stored,
#  then the cached result is returned.
#  Otherwise, the solve function is used to 
#  calculate the inverse.
#  Usage is demonstrated in the overall description above
#
#  Note:
#  If x is not in the makeCacheMatrix structure,
#  then cacheSolve will give an error.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}