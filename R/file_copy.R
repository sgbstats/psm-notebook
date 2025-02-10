copy_files <- function(source_dir, dest_dir) {
  # Ensure both source and destination directories exist
  if (!dir.exists(source_dir)) {
    stop("Source directory does not exist: ", source_dir)
  }
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Get a list of all files in the source directory
  files <- list.files(source_dir, full.names = TRUE)
  
  # Copy each file to the destination directory
  file.copy(files, dest_dir, overwrite = TRUE)
  
  message("Files copied successfully from ", source_dir, " to ", dest_dir)
}

# Example usage:
copy_files("C:\\Users\\mbbx4sb5\\Dropbox (The University of Manchester)\\00_ICS_RECODE_Shared\\R/prediction/",
           "R/prediction/")
