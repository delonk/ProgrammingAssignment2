##  The makeCacheMatrix function creates a special "matrix" object that can 
##  cache its inverse
##  It accepts a matrix as its input argument. 
##  useage: rpog03 <- makeCacheMatrix(my_matrix) where "my_matrix" must exist and 
##  be 'class(my_matrix) = "matrix"'
##
##  solve.it is a function which solves "my_matrix and returns inverse of the 
##  matrix passed as arugment "x". solve.it uses the 'solve' built-in function 
##  and caches the result. 
##  The solve.it() function is used as follows:
##  useage: g <- rprog03$solve.it() 
##
##  solve.it is a function which returns the inverse of the matrix passed 
##  as arugment "x". solve.it uses the solve built-in function and caches
##  the result. 
##  The solve.it() function is used as follows:
##  useage: g <- rprog03$solve.it() 
##  
##  get.initial is a function which returns the original matrix which was passed as 
##  an arugment to the MakeCacheMatrix function. It accecpts no arguments and 
##  can be used as follows:
##  useage: h <- rprog$get.it()
##  
##  The list in the main function is a list of all functions which can be 
##  called by the main function.
##

makeCacheMatrix <- function(x = matrix()) 
{
       cached.matrix <- NULL   
       
       solve.it <- function()
       {
              cached.matrix <<- solve(x)
       }
  
       get.initial <- function() x
     
       get.solved <- function() cached.matrix
    
       list(get.initial = get.initial, solve.it = solve.it, get.solved = get.solved)

}


##
##  cacheSolve is a function which produces the inverse of a matrix object, if it 
##  has not previously been solved. It uses the "get.solved" method to check if the 
##  matrix has previously been solved and thus, cached. 
##  If get.solved returns NULL, it solves the matrix using the solve.it method 
##  and caches the result.
##  If the matrix has been previously solved, it returns the value already cached.
##  Usage:
##  m <- cachesolve(rprog03)
##  if the data is being read from cache it prints "Data is coming from the cache"
##  if it has to copmute the inverse, it prints "Solving..."


cacheSolve <- function(x, ...) 
{
 
       solved <- x$get.solved()
       
       if(!is.null(solved))
       {   print("Data is coming from the cache")
              return(cached.matrix)
       }
  
       else
       {
              print("solving...")
              cached.matrix <- x$solve.it()
    
       }
  
} 



