###########################################################################################################
# Name: Pradeep Sathyamurthy
# Date of submission: 09 - Oct - 2016
# Problem statement:
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we 
# will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.
# Write the following functions:
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.
############################################################################################################

############################################################################################################
# makeCacheMatrix - Function where we get and set our matrix data and its inverse
# This will be an object with list of 4 elements and two attributes of class matrix
# This object will be used to cache the value which will be used by other function using lexical scoping
############################################################################################################

makeCacheMatrix <- function(m=matrix()){ # Defining the function with one argument of class matrix
    
    # initializing an object which can used later in this function, scope of this object is restricted within the enviromnent of makeCacheMatrix
    inverse_matrix <- NULL 
    
    # Trying to set the value of matrix obtained in to a object called 'm', this is a setter method for data/matrix being obtained
    # since we need a valid square matrix to be set in a object this set function will carry one argument
    # Since this is a new matrix for which we are finding inverse, we are refreshing the old cached inverse value is any and thus making variable that carries inverse matrix as NULL
    # We will be using <<- assignment operator which will provide a scope of accesing these varible outside the makeCacheMatrix function
    set <- function(matrix){ # 
        m <<- matrix
        inverse_matrix <<- NULL
    }
    
    # Getter method for data, this is used to fetch the data from the makeCacheMatrix object
    get <- function() m
    
    # Setter method for inverse matrix that we have found for the give matrix
    # We will be using <<- assignment operator which will provide a scope of accesing this varible outside the makeCacheMatrix function
    setmatrix <- function(matrix){
        
        inverse_matrix <<- matrix
    }
    
    # Getter method for fetching the inverse of given matrix from the makeCacheMatrix object
    getmatrix <- function() inverse_matrix
    
    # we return all these setter and getter method of data and inverse matrix as a list to the makeCacheMatrix function
    # Thus list will carry 4 function get, set, getmatrix and setmatrix
    list(get=get,set=set,getmatrix=getmatrix,setmatrix=setmatrix)
}



############################################################################################################
# cachesolve - Function which is used to calculate the matirx inverse
# This object will refer the objects of makeCacheMatrix with the help of lexical scoping funtionality
# If we pass the same matrix, inverse matrix of it which is cached will be passed by makeCacheMatrix object
# If for new value, then it will pass NULL and its respective inverse value is calculated in this cachesolve function
############################################################################################################

# creating a function cachesolve which will carry an argument in it
cachesolve <- function(m){
    
    # we are trying to get the value of matrix inverse for the matrix passed if stored in cache (i.e in the makeCacheMatrix environment) through lexical scoping
    # we try to access the getter method of inverse matrix, getmatrix() in makeCacheMatrix environment
    # Through lexical scoping it would get the value of inverse of a same matrix passed whose value if stored in cache
    inverse_matrix <- m$getmatrix()
    
    # We see if the value is already cached in makeCacheMatrix environment
    # If valid value is stored we are passing that value with no further manipulation
    # If it is null we will skip without returning any values and try to manupulate inverse of matrix outsie the if loop
    if(!is.null(inverse_matrix)){
        print("fetching value from cache")
        return(inverse_matrix)
    }
    
    # If in case the inverse matrix value is not available in cache, then we need to compute it
    # Thus, inorder to compute inverse of a matrix we need a valid square matrix
    # So, we are trying to fetch the data of matrix passed by the user from makeCacheMatrix object
    matrix_data <- m$get()
    
    # Once the matrix data is obtained, we use solve() function to find the inverse matrix of it
    inverse_matrix <- solve(matrix_data)
    
    # Once the inverse of a matrix is computed, we set its value in makeCacheMatrix object using setmatrix function
    m$setmatrix(inverse_matrix)
    
    # Now that the value is cached, we are returning the value if inverse matrix found.
    return(inverse_matrix)
}


############################################################################################################
# Creating a square matrix and Executing the above function to check
# creating a 4x4 square matrix using rbind function
############################################################################################################

x1=c(4,2,6,6)
x2=c(5,-5,6,0)
x3=c(-5,-3,-4,4)
x4=c(5,1,6,-5)
m=rbind(x1,x2,x3,x4)

############################################################################################################
# I have switched on the debuger to validate the navigation of the function and how the lexical scoping takes place
# Kindly u can enable it if interested by removing # from below two lines
############################################################################################################

# debug(cachesolve)
# debug(makeCacheMatrix)

############################################################################################################
# Passing a Matrix to find a inverse of it using lexical scoping
############################################################################################################

cachesolve(makeCacheMatrix(m))

