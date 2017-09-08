#' Create Neighborhood Matrix from Adjacency List 
#'
#' Create Neighborhood Matrix from Adjacency List
#'
#' @param adj adjacency list, as created by poly2nb fron spdep package
#' @param num the number of adjacent neighbors for each list item above
#' @param n total number of list items
#'
#' @return a matrix, with 1's indicating neighbors, and 0 otherwise, in the same order as adj.
#'
#' @author Jay Ver Hoef
#' @rdname Neighmat
#' @export Neighmat 

Neighmat <- function(adj, num, n)
{
	N.mat <- matrix( 0, nrow = n, ncol = n )
	k <- 1
	for (i in 1:n){
		if(num[i] > 0) {
				N.mat[i,adj[[i]]] <- 1
			}
		}
	N.mat
}


