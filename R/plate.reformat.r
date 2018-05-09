#' Prepares plate reader data for further analysis
#'
#' Plate reader data are reformatted to facilitate further analysis.
#' Specifically, the function merges info from two separate files and also
#' subtracts the blank value.
#'
#' @param file_plate Delimited text file with info on plate layout. Must have
#'   columns 'id_well', 'id_sample', 'id_replicate'. It is necessary that, for
#'   one or more records, the value in column 'id_sample' is set to 'blank'.
#' @param file_growth Delimited text file with observed data. Must have column
#'   'time'. All additional columns must be named after wells as specified in
#'   the field 'id_well' of the other input file.
#'   ".
#' @param file_out Name for the output file.
#' @param sep_in The field delimiter for input files. Default is TAB and the
#'   latter is always used for the output.
#' @param dec_in The decimal character for input files. Default is period and
#'   the latter is always used for the output. 
#' @param overwrite Logical. Should existing output files be overwritten?
#'   
#' @return \code{NULL}. All results are written to the specified output file.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   plate.reformat(file_plate="in/strains.txt", file_growth="in/growth.txt",
#'     file_out="out.txt")
#' }

plate.reformat <- function(file_plate, file_growth, file_out,
  sep_in="\t", dec_in=".", overwrite=FALSE) {

  if (!file.exists(file_plate))
    stop(paste0("input file '",file_plate,"' not found"))
  plate <- utils::read.table(file=file_plate, header=TRUE, sep=sep_in,
    stringsAsFactors=FALSE)
  req <- c("id_well","id_sample","id_replicate")
  if (!all(req %in% names(plate)))
    stop(paste0("input file '",file_plate,"' must have columns: '",
      paste(req, collapse="', '"),"'"))
  if (!"blank" %in% plate[,"id_sample"])
    stop("no record(s) with sample ID 'blank' in file '",file_plate,"'")
  i <- which(duplicated(plate[,"id_well"]))
  if (length(i) > 0)
    stop("found duplicates well ID(s) in file '",file_plate,
      "'; first duplicate occurred at line no. ",i[1]+1)
  i <- which(duplicated(plate[plate$id_sample != "blank",c("id_sample", "id_replicate")]))
  if (length(i) > 0)
    stop("found duplicates in file '",file_plate,
      "' based on the combined primary key columns 'id_sample' and 'id_replicate'",
      "; first duplicate occurred at line no. ",i[1]+1)
  
  if (!file.exists(file_growth))
    stop(paste0("input file '",file_growth,"' not found"))
  growth <- utils::read.table(file=file_growth, header=TRUE, sep=sep_in, dec=dec_in,
    check.names=FALSE, stringsAsFactors=FALSE)
  req <- c("time")
  if (!all(req %in% names(growth)))
    stop(paste0("input file '",file_growth,"' must have columns: '",
      paste(req, collapse="', '"),"'"))
  if (length(unique(names(growth))) != length(names(growth)))
    stop(paste0("column names in input file '",file_growth,"' not unique"))
  
  as.hours <- function(x) {
    x <- unlist(strsplit(x, split=":", fixed=TRUE))
    if (length(x) != 3)
      stop("bad time string")
    round(sum(as.numeric(x) * c(1, 1/60, 1/3600)), 3)
  }
  growth$time <- sapply(growth$time, as.hours)
  if (is.unsorted(growth$time, strictly=TRUE))
    stop("times not strictly increasing in file '",file_growth,"'")
  
  growth <- reshape2::melt(data=growth, id.vars="time", variable.name="id_well")
  bad <- unique(growth$id_well[!growth$id_well %in% plate$id_well])
  if (length(bad) > 0)
    stop(paste0("well ID in growth data file '",file_growth,
      "' not listed in plate layout file '",
      file_plate,"'; Details: '", paste(bad, collapse="', '"),"'"))
  bad <- plate$id_well[!plate$id_well %in% growth$id_well]
  if (length(bad) > 0)
    stop(paste0("well ID in plate layout file '",file_plate,
      "' not referenced in growth data file '",
      file_growth,"'; Details: '", paste(bad, collapse="', '"),"'"))
  growth <- merge(x=plate, y=growth, by="id_well")
  growth$id_well <- NULL
  rm(plate)
  
  blank <- mean(growth$value[growth$id_sample == "blank"])
  growth$value <- growth$value - blank
  growth <- growth[growth$id_sample != "blank",]
  
  if (file.exists(file_out) && (!overwrite))
    stop(paste0("unwilling to overwrite existing output file '",file_out,"'"))
  utils::write.table(growth, file=file_out, sep="\t",
    col.names=TRUE, row.names=FALSE, quote=FALSE)

  invisible(NULL)
}
