##Programming assignment 2 for Coursera R Programming with Peng
##Barry Weinberg
##7-24-15

## These functions create matrix objects that can cache its inverse and
##compute the inverse of matrices, checking first if it has already been calculated and cahced


## The function takes the original matrix as its argument and returns a list of functions that
# can set or retrieve the original matrix, stores the inverse of the matrix, and retrieves the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ##sets inverted matrix variable to null
  inversedM<- NULL
  
  ##allows assignment of values to matrix without inversion
  set<- function(y){
    x<<- y
    inversedM<<- NULL
  }
  ##allows retrieval of matrix
  get<- function()x
  ##
  setInversedM<-function(inverse) inversedM<<-inverse
  ##
  getInversedM<-function() inversedM
  ##Use below two lines for testing to confirm functionality
  ##check<- x %*% inversedM
  ##print(check)
  list(set = set, get = get, setInversedM = setInversedM, getInversedM = getInversedM)
  
  ##matrix()
}


## This function first checks to see if the inverse is already stored in the cache
## by checking x$getInversedM for inversedM. If it is not null, it returns the stored matrix
## If the cache is null, it computes the inverse, stores it in the cache, and returns it

cacheSolve <- function(x, ...) {
    inversedM<-x$getInversedM()    
  if(!is.null(inversedM)){
    ##Unspecified in assignment instructions, but optional line below to indicate if pulling from cache
    ##message("Displaying cached inverse")
    return(inversedM)
  }
    ##pulls original matrix
    parentM<- x$get()
    ##solves for inverse of original
    inversedM<-solve(parentM)
    ##stores inverse in cache
    x$setInversedM(inversedM)
    ##returns inverse
    inversedM
  
}
