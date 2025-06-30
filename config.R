

root_dir <- getwd()  # file of Github Project

#Creation of input case
input_dir <- file.path(root_dir, "input")
if (!dir.exists(input_dir)) {
  dir.create(input_dir, recursive = TRUE)
}


output_dir <- file.path(root_dir, "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}