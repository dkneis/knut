#' Set-up a compiled rodeo based model from text files
#'
#' Creates a rodeo object and compiles Fortran source code
#'
#' @param dir Directory holding text files with fixed names "vars.txt",
#'   "pars,txt", "funs.txt", "pros.txt", "stoi.txt" as well as a
#'    Fortran source file "functions.f95".
#'
#' @return A rodeo object ready for compiled-code simulation.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

rodeoInitialize <- function(dir) {
  rd <- function(f, ...) {
    read.table(paste0(dir,"/",f), header=TRUE, sep="\t",
      stringsAsFactors=FALSE, ...)
  }
  model <- rodeo$new(vars=rd("vars.txt"), pars=rd("pars.txt"),
    funs=rd("funs.txt"), pros=rd("pros.txt"),
    stoi=as.matrix(rd("stoi.txt", row.names=1)), asMatrix=TRUE, dim=c(1))
  model$compile(sources=paste0(dir,"/functions.f95"))
  return(model)
}
