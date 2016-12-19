## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#the makeCacheMatrix function starts by defining the x argument as a matrix
makeCacheMatrix <- function( x = matrix()){
        # i is the variable that will store the previously calculated matrix inversion after cacheSolve is ran at least once
        i <- NULL
        #set_matrix is a function that is not used while running makeCacheMatrix() or cachesolve(). 
        #set_matrix is used to have an easy way to change the matrix to be inverted without having to create a new MakeCacheMatrix() object
        #set_matrix will be used by calling the function inside the MakeCacheMatrix object with a new matrix  (as argument "y") to be used.
        
        set_matrix <- function(y){
                # <<- will assign the value of the "new" matrix to x in the parent environment to be used by the get_matrix() function
                x <<- y
                #i <<-NULL will "erase" the cached data, since a new matrix was set to be used. 
                i <<- NULL
        }
        #get_matrix will be used to retrieve the "data" (matrix) to be inverted.
        get_matrix <- function() x
        #set_inverse_matrix  is a function that will use the argument "inverse"(assigned when set_inverse_matrix is called from cachesolve())
        set_inverse_matrix <- function(inverse) i <<- inverse
        #get_inverse_matrix simply works as an "entrance" to let cachesolve() access the current value of the inverted matrix "i"
        get_inverse_matrix <- function() i
        #makecachematrix returns a list of the functions previouslt created. They are named to be easily called.
        list(set_matrix = set_matrix, get_matrix = get_matrix, 
             set_inverse_matrix = set_inverse_matrix, 
             get_inverse_matrix =get_inverse_matrix)
}





## Write a short comment describing this function

#Cachesolve will first check if there is an inverted matrix to be returned
cacheSolve <- function(x, ...) {
        #retrieve the cached value
        i <- x$get_inverse_matrix()
        #check if value is not NULL. Returnes cached matrix if true, skips to new calculation if value is NULL
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        #Aquisition of new matrix and calculation of the inverted matrix
        data <- x$get_matrix()
        i <- solve(data, ...)
        #Stores the new value in the cached data
        x$set_inverse_matrix(i)
        i
}

