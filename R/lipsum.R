#' Insert filler text in document
#'
#' @param n Number of paragraphs
#'
#' @return
#' @export
#'
#' @examples
#' lipsum(3)
#' lipsum(10)
lipsum <- function(n=1){
  if(n>15) stop("Só tenho 15 parágrafos. Você precisa de tanto texto assim..?")
  cat(lipsum_data[1:n], sep = "\n\n" )
}
