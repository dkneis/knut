#' Prepares UHEL growth rate data for further analysis
#'
#' Prepares growth rate data produced by Stefanie at UHEL for further analysis.
#' The function merges info from two separate files and also subtracts the
#' blank value.
#'
#' @param file_plate Delimited text file with info on plate layout. Must have
#'   columns 'Well_ID', 'Strain', 'Replicate'.
#' @param file_growth Delimited text file with observed data. Must have column
#'   'Time'. All additional columns must be named after wells as specified in
#'   the field 'Well_ID' of the other input file or the colum must carry the
#'   name 'blank'.
#'   ".
#' @param file_out Name for the output file.
#' @param sep_in The field delimiter for input files. Default is TAB and the
#'   later is always used for the output.
#' @param dec_in The decimal character for input files. Default is period and
#'   the latter is always used for the output. 
#' @param overwrite Logical. Should existing output files be overwritten?
#'   
#' @return Nothing. All results are writte to the specified output file.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   growthDB(file_plate="in/strains.txt", file_growth="in/growth.txt",
#'     file_out="out.txt")
#' }

prepGrowth <- function(file_plate, file_growth, file_out,
  sep_in="\t", dec_in=".", overwrite=FALSE) {

  if (!file.exists(file_plate))
    stop(paste0("input file '",file_plate,"' not found"))
  plate <- utils::read.table(file=file_plate, header=TRUE, sep=sep_in,
    stringsAsFactors=FALSE)
  req <- c("Well_ID","Strain","Replicate")
  if (!all(req %in% names(plate)))
    stop(paste0("input file '",file_plate,"' must have columns: '",
      paste(req, collapse="', '"),"'"))
  names(plate)[names(plate) == "Well_ID"] <- "id_well"
  names(plate)[names(plate) == "Strain"] <- "id_strain"
  names(plate)[names(plate) == "Replicate"] <- "id_replicate"
  
  if (!file.exists(file_growth))
    stop(paste0("input file '",file_growth,"' not found"))
  growth <- utils::read.table(file=file_growth, header=TRUE, sep=sep_in, dec=dec_in,
    check.names=FALSE, stringsAsFactors=FALSE)
  req <- c("Time")
  if (!all(req %in% names(growth)))
    stop(paste0("input file '",file_growth,"' must have columns: '",
      paste(req, collapse="', '"),"'"))
  names(growth)[names(growth) == "Time"] <- "time"
  if (length(unique(names(growth))) != length(names(growth)))
    stop(paste0("column names in input file '",file_growth,"' not unique"))
  
  as.hours <- function(x) {
    x <- unlist(strsplit(x, split=":", fixed=TRUE))
    if (length(x) != 3)
      stop("bad time string")
    round(sum(as.numeric(x) * c(1, 1/60, 1/3600)), 3)
  }
  growth$time <- sapply(growth$time, as.hours)
  
  growth <- reshape2::melt(data=growth, id.vars="time", variable.name="id_well")
  growth$id_well <- gsub(x=growth$id_well, pattern="Well ", replacement="", fixed=TRUE)
  bad <- unique(growth$id_well[!growth$id_well %in% plate$id_well])
  if (length(bad) > 0)
    stop(paste0("well_id in growth data file '",file_growth,
      "' not listed in plate layout file '",
      file_plate,"'; Details: '", paste(bad, collapse="', '"),"'"))
  bad <- plate$id_well[!plate$id_well %in% growth$id_well]
  if (length(bad) > 0)
    stop(paste0("well_id in plate layout file '",file_plate,
      "' not referenced in growth data file '",
      file_growth,"'; Details: '", paste(bad, collapse="', '"),"'"))
  growth <- merge(x=plate, y=growth, by="id_well")
  growth$id_well <- NULL
  rm(plate)
  
  blank <- min(growth$value[growth$id_strain == "blank"])
  growth$value <- growth$value - blank
  growth <- growth[growth$id_strain != "blank",]
  
  if (file.exists(file_out) && (!overwrite))
    stop(paste0("unwilling to overwrite existing output file '",file_out,"'"))
  utils::write.table(growth, file=file_out, sep="\t",
    col.names=TRUE, row.names=FALSE, quote=FALSE)

  invisible(NULL)
}
