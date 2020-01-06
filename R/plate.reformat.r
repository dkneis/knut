#' Prepares plate reader data for further analysis
#'
#' Plate reader data are reformatted to facilitate further analysis.
#' Specifically, the function merges info from two separate files and also
#' subtracts the blank value.
#'
#' @param layout Data frame with info on plate layout. Must have
#'   columns 'id_well', 'id_sample', 'id_replicate'. It is necessary that, for
#'   one or more records, the value in column 'id_sample' is set to 'blank'.
#' @param dynamics Data frame with observed data. Must have column
#'   'time'. All additional columns must be named after wells as specified in
#'   the field 'id_well' of the other input file.
#'   ".
#'   
#' @return A data frame.
#' 
#' @note The subtracted blank value is the mean of the blank wells for the
#'   respective time step. Possible negative results are set to zero.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example to be prepared
#' }

plate.reformat <- function(layout, dynamics) {

  req <- c("id_well","id_sample","id_replicate")
  if (!all(req %in% names(layout)))
    stop(paste0("'layout' must have columns: '",
      paste(req, collapse="', '"),"'"))
  if (!"blank" %in% layout[,"id_sample"])
    stop("no record(s) with sample ID 'blank' found in 'layout'")
  i <- which(duplicated(layout[,"id_well"]))
  if (length(i) > 0)
    stop(paste0("found duplicates well ID(s) in 'layout'",
      "; first duplicate occurred at line no. ",i[1]+1))
  i <- which(duplicated(layout[layout$id_sample != "blank",c("id_sample", "id_replicate")]))
  if (length(i) > 0)
    stop(paste0("found duplicates in file 'layout'",
      " based on the combined primary key columns 'id_sample' and 'id_replicate'",
      "; first duplicate occurred at line no. ",i[1]+1))
  
  req <- c("time")
  if (!all(req %in% names(dynamics)))
    stop(paste0("'dynamics' must have columns: '",
      paste(req, collapse="', '"),"'"))
  if (length(unique(names(dynamics))) != length(names(dynamics)))
    stop("column names in 'dynamics' not unique")
  
  as.hours <- function(x) {
    x <- unlist(strsplit(x, split=":", fixed=TRUE))
    if (length(x) != 3)
      stop("bad time string")
    round(sum(as.numeric(x) * c(1, 1/60, 1/3600)), 3)
  }
  dynamics$time <- sapply(dynamics$time, as.hours)
  if (is.unsorted(dynamics$time, strictly=TRUE))
    stop("times in 'dynamics' not strictly increasing'")

  blankWells <- layout$id_well[layout$id_sample == "blank"]
  blank <- apply(dynamics[,(names(dynamics) %in% blankWells),drop=FALSE], 1, mean)
  for (i in which(!names(dynamics) %in% c("time", blankWells)))
    dynamics[,i] <- pmax(0, dynamics[,i] - blank)

  dynamics <- reshape2::melt(data=dynamics, id.vars="time", variable.name="id_well")
  bad <- unique(dynamics$id_well[!dynamics$id_well %in% layout$id_well])
  if (length(bad) > 0)
    stop(paste0("well ID from 'dynamics' not listed in 'layout'",
      " ; Details: '", paste(bad, collapse="', '"),"'"))
  bad <- layout$id_well[!layout$id_well %in% dynamics$id_well]
  if (length(bad) > 0)
    stop(paste0("well ID from 'layout' not referenced in 'dynamics'",
      "' ; Details: '", paste(bad, collapse="', '"),"'"))
  dynamics <- merge(x=layout, y=dynamics, by="id_well")
  dynamics <- dynamics[dynamics$id_sample != "blank",]
  dynamics$id_well <- NULL

  return(dynamics)
  
}
