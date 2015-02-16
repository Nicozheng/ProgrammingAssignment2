## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ## create an empty varible to store the matrix
      m <- NULL
      ## set cache environment
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      ## load matrix
      setMatrix <- function(matrix) m <<- matrix
      getMatrix <- function() m
      ## return matrix
      list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## load matrix into the new function 
      m <- x$getMatrix()
      ## check if m is null
      if(!is.null(m)){
            message("getting cached matrix")
            return(m)
      }
      data <- x$get()
      ## caculate inverse matrix
      m <- solve(data, ...)
      x$setMatrix(m)
      ## return inverse matrix
      m
}


## test the function 
## make random matrix for test
randomMatrix <- matrix(rnorm(100), 10, 10)      
##inverse the matrix though usual way
inverseMatrix <- solve(randomMatrix)      
##use function to inverse
newInverseMatrix <- cacheSolve(makeCacheMatrix(randomMatrix))     
tureOrFalse <- as.list(inverseMatrix == newInverseMatrix)
## print results
for (i in 1:100){
      if (tureOrFalse[i] == FALSE){
            cat("the function has bugs!")
      }
}

