snake_case <- function (x) {
  s <- gsub("\\.", "_", x) # Replace dots with underscores
  s <- gsub("(.)([A-Z][a-z]+)", "\\1_\\2", s) # Separate with underscores on capitalization
  s <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", s)) #lowercase
  s <- gsub("__", "_", s) #double to single underscore
  s <- gsub("^[_, .]", "", s) # del first char underscore "_" or period "."
  s <- gsub(' ', '', s) # remove spaces
}

fix_column_names <- function(x) {
  names(x) <- snake_case(names(x))
  return(x)
}