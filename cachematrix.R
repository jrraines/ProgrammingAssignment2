## R programming Assignment 2:
##
## create a new matrix type that caches the result of the "solve" operation for square matrices
##   
##USAGE:
## > c=rbind(c(1, -1/4), c(-1/4, 1))  ## create a matrix
##> m<-makeCacheMatrix(c)             ##pass it to the new matrix type
##> cacheSolve(m)                     ##solve for the inverse
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> cacheSolve(m)                    ##request the inverse again, retreive the stored result from the prior call
##getting cached inverse matrix :
##      [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##
##This function creates the new matrix type--basically setters, getters
makeCacheMatrix<-function(x = matrix()){
	  ## x is used to store the unmodified matrix
      m<-NULL           ##stored inverse
      set<-function(y){
            x<<-y        ##store y matrix
            m<<-NULL
      }
      get <-function() { x }  ##retrieve stored y
      setInverse <-function(inverse) { m<<-inverse }  ##store inverse matrix
      getInverse <-function() { m }                   ##retrieve stored matrix
      
      ##the return value is a list of the functions for the new type (think methods)
      return(list (set=set, get=get, setInverse=setInverse, getInverse=getInverse) )
}
##
##function to solve for the inverse of the matrix:
cacheSolv <- function(x,...) {
      m<-x$getInverse()
      if ( !is.null(m) ) {        ## has the matrix already been stored?
            message ("getting cached inverse matrix :")
            return (m)
      }
      mat <- x$get()             ##matrix has not been solved before, retrieve it
      inv <- solve(mat, ...)     ##solve it
      x$setInverse(inv)          ##store it so we don't have to solve next time
      return(inv)
}
