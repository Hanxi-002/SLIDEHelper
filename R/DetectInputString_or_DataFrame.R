
# check whether we are passed a
DetectInputString_or_DataFrame = function(input, type = "matrix") {

  type = stringr::str_to_lower(type)

  if (type == "matrix") {
    data_load_function = function(x) { return(as.matrix(read.csv(x, row.names = 1, check.names = F))) }
  } else if (type == "yaml") {
    data_load_function = function(x) { return(yaml::yaml.load_file(x)) }
  } else {
    cat("\n Type error in loading string/dataframe. Valid options are 'matrix' or 'yaml' ")
  }

  # check whether we have a string that points to a valid file
  if (is.character(input) & file.exists(input)) {
    return(data_load_function(input))
  } else if (is.list(input) | is.data.frame(input) | is.matrix(input)) {
    return(input)
  }
  return(NULL)
}
