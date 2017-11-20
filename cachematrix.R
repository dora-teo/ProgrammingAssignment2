## Put comments here that give an overall description of what your
## functions do
#### Write a short comment describing this function 
# "makeCacheMatrix" function creates an object that can cache its inverse. 

# This is desirable because obtaining the inverse of a function is computationally costly. 
# Such a function, in combination with the "cacheSolve" function would allow us to cache the inverse of the matrix 
# in memory instead of repeatedly calculating it. As long as the input matrix does not change, 
# the inverse of the matrix can be retrieved from cache and reused
# in subsequent calculations. 

# These two functions take advantage of the lexical scoping, where the value of an object defined by "<-" is 
# only available within the environment where it was defined (for example in the body of a function). 
# Alternatively, an object defined by "<<-" will be available in the parent environment, as illustrated below.

# The first step is initialization of two objects: x (initialized as an emplty matrix in the function argument) 
# and Inv which is set to NULL 
# 2nd step: makeCacheMatrix defines the set() function  that takes an argument y (assumed to be a matrix). 
# Within the set() function, we use the "<<-" asignment operator which will make it available in the parent 
# environment which in this case is the environment of the makeCacheMatrix function. 
# With this method, we assign the input argument to the x obkect in the parent envirobment and also asign a NULL
# value to the inverse (Inv) object in the parent enviroment. This code clears the cache when a new matrix is defined.
# 3rd step: a setter for the inverse Inv is defined using the setinverse(), and the "<<-" operator that makes 
# Inv available in the parent environment.  
# 4th step: a getter for the inverse Inv. 
# 5th step: in this step, each of the functions are assigned to a list. In this way, when the function ends, it 
# will return a fully formed object. 

rm(list=ls())
makeCacheMatrix <- function(x = matrix()) { 
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) Inv <<- solve
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#### Write a short comment describing this function ####

# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated in a previous step (and the matrix has not changed), 
# then `cacheSolve` should retrieve the inverse from the cache.
# 1st step: the function is defined with two arguments: x and an ellipses (...) 
# that allows passing other arguments to the function
# 2nd step: the function attempts to retrieve the existing inverse by callint the getinverse() function 
# 3rd step: using an if statement, the function check to see if the result is NULL (which would be the case when
# either the inverse has not been calculated yet, or when a new input matrix has been defined, which would 
# automatically set the Inv to NULL)
# If the result of the if statement is TRUE (meaning that the Inverse has been calculated before and 
# the Inv is not NULL), then cacheSolve gets the vector from the input object, calculates the 
# If the result of the if Statement is FALSE, the cacheSolve calculates the inverse using solve() function, 
# uses setinv() function to set the inverse in the input object, and then returns the value of the inverse to
# the parent environment by printing the inverse function. 


cacheSolve <-  function(x, ...) {
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv
}

#### Test  using a simple test matrice ####  

# define matrix 
m1 <- matrix(c(2, -1.4, -1.1, 2.5), nrow = 2, ncol = 2)

# using the solve() function calculate the inverse so we can compare with the results of our function
solve(m1)

# call the makeCacheMatrix on the matrix and assign it to myMatrix_object 
myMatrix_object <- makeCacheMatrix(m1)

# call the cacheSolve on myMatrix_object
cacheSolve(myMatrix_object)

#  call the cacheSolve on myMatrix_object one more time to test whether the second time it will retrieve the
# inverse from the cached data
cacheSolve(myMatrix_object)
