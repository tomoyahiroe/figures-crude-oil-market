#### make_matrix関数の定義 ####
make_matrix <- function(source_file) {
  data <- read.table(source_file)
  return(as.matrix(data))
}