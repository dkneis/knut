#' Simplify a consumer-resource matrix
#'
#' Computes the effect of forced extinctions on a consumer-resource matrix.
#'
#' @param m Square matrix filled with 0 and 1. Column names represent consumers,
#'   row names represent ressources. Row and column names must be identical.
#'   A value of 1 indicates that the consumer in the respective column feeds on
#'   the resource in the respective row.
#' @param drop Vector of character strings defining the rows and columns to be
#'   removed from the food web (forced extinctions).
#' @param keep Vector of character strings defining the rows and columns to be
#'   always kept in the system. Typically, these represent primary producers.
#'   The elements in \code{keep} should not be present in \code{drop} as well.
#' 
#' @return A square matrix with less rows and columns than the input matrix
#'   \code{m}. The result matrix reflects both the forced extinctions (specified
#'   in \code{drop}) and the secondary extinctions being triggered by the forced
#'   ones.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' # A simple aquatic food consisting of algae, zooplankton
#' # planktivorous fish and a top predator.
#' 
#' m <- rbind(
#'   algae =  c(0,1,1,0,0,0),
#'   zoopl1 = c(0,0,0,1,1,0),
#'   zoopl2 = c(0,0,0,0,1,0),
#'   fish1 =  c(0,0,0,0,0,1),
#'   fish2 =  c(0,0,0,0,0,1),
#'   top =    c(0,0,0,0,0,0)
#' )
#' colnames(m) <- rownames(m)
#' print("Original food web:")
#' print(m)
#' 
#' m2 <- extinctSimplify(m,
#'   drop=c("zoopl1","fish2"), keep=c("algae"))
#' print("Resulting food web:")
#' print(m2)

extinctSimplify <- function(m, drop, keep) {
  # Check inputs
  if (!is.matrix(m) || !is.numeric(m))
    stop("'m'must be a numeric matrix")
  if (!identical(rownames(m), colnames(m)))
    stop("row names and column names of 'm' must be identical")
  if (!all(drop %in% colnames(m)))
    stop("all elements of 'drop' must be present in 'm'")
  if (!all(keep %in% colnames(m)))
    stop("all elements of 'keep' must be present in 'm'")
  if (any(keep %in% drop))
    stop("elements of 'keep' must not be present in 'drop'")
  # Primary deletion of species requested by user
  i <- match(drop, colnames(m))
  x <- m[-i, -i, drop=FALSE]
  # Remove species that go extinct in response to the primary deletions
  while (TRUE) {
    # Find secondary producers without resource
    i <- which((colSums(x) == 0) & (!colnames(x) %in% keep))
    if (length(i) > 0) {
      x <- x[-i, -i, drop=FALSE]
    } else {
      break  # no more deletions; result matrix has reached minimum size
    }
  }
  x
}
