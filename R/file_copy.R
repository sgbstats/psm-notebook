#!/usr/bin/env Rscript
#' Script to find all files that are sourced in .qmd files in the main directory.
#' Searches for various source patterns commonly used in Quarto documents.

library(stringr)

#' Find all files that are sourced in .qmd files in the specified directory.
#' 
#' @param directory Directory to search for .qmd files (default: current directory)
#' @return List with sourced files by qmd file and all unique sourced files
find_sourced_files <- function(directory = ".") {
  
  # Patterns to match different ways files can be sourced in .qmd files
  patterns <- c(
    # R code chunks with source()
    'source\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    
    # Python exec() or execfile()
    'exec\\s*\\(\\s*open\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    'execfile\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    
    # Include statements
    '\\{\\{< include ([^>]+) >\\}\\}',
    
    # File reading functions
    'read\\.csv\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    'read\\.table\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    'read_csv\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    'readr::read_csv\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    'read\\.xlsx\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    'readxl::read_excel\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    'load\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    'readRDS\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    
    # Image/figure references
    '!\\[[^\\]]*\\]\\(([^)]+)\\)',
    
    # knitr::include_graphics
    'include_graphics\\s*\\(\\s*["\']([^"\']+)["\']\\s*\\)',
    
    # General file references in quotes (more permissive)
    '["\']([^"\']*\\.[a-zA-Z0-9]+)["\']'
  )
  
  # Find all .qmd files in the directory
  qmd_files <- list.files(directory, pattern = "\\.qmd$", full.names = TRUE)
  
  if (length(qmd_files) == 0) {
    cat("No .qmd files found in directory:", directory, "\n")
    return(list(by_file = list(), all_files = character(0)))
  }
  
  sourced_files <- list()
  all_sourced_files <- character(0)
  
  for (qmd_file in qmd_files) {
    sourced_files[[qmd_file]] <- character(0)
    
    # Read file content
    tryCatch({
      content <- readLines(qmd_file, warn = FALSE)
      content <- paste(content, collapse = "\n")
      
      # Search for each pattern
      for (pattern in patterns) {
        matches <- str_extract_all(content, pattern, simplify = FALSE)[[1]]
        
        if (length(matches) > 0) {
          # Extract the captured groups (file paths)
          file_matches <- str_match_all(content, pattern)[[1]]
          
          if (ncol(file_matches) > 1) {
            for (i in 2:ncol(file_matches)) {
              valid_matches <- file_matches[, i]
              valid_matches <- valid_matches[!is.na(valid_matches)]
              
              for (match in valid_matches) {
                # Clean up the match
                clean_match <- str_trim(match)
                
                # Skip if it's clearly not a file (URLs, etc.)
                if (str_detect(clean_match, "^(https?://|ftp://|mailto:)")) {
                  next
                }
                
                # Skip common non-file patterns
                if (clean_match %in% c("TRUE", "FALSE", "NULL", "NA", "")) {
                  next
                }
                
                # Skip if it doesn't look like a file path
                if (!str_detect(clean_match, "\\.|/|\\\\")) {
                  next
                }
                
                # Add to results if not already present
                if (!(clean_match %in% sourced_files[[qmd_file]])) {
                  sourced_files[[qmd_file]] <- c(sourced_files[[qmd_file]], clean_match)
                  all_sourced_files <- c(all_sourced_files, clean_match)
                }
              }
            }
          }
        }
      }
    }, error = function(e) {
      cat("Error reading", qmd_file, ":", e$message, "\n")
    })
  }
  
  # Remove duplicates from all_sourced_files
  all_sourced_files <- unique(all_sourced_files)
  
  return(list(by_file = sourced_files, all_files = all_sourced_files))
}

#' Check which of the sourced files actually exist on the filesystem.
#' 
#' @param files Vector of file paths to check
#' @param base_directory Base directory to resolve relative paths
#' @return List with existing and missing file vectors
check_file_existence <- function(files, base_directory = ".") {
  existing <- character(0)
  missing <- character(0)
  
  for (file_path in files) {
    # Try both absolute and relative to base directory
    full_path <- file.path(base_directory, file_path)
    
    if (file.exists(file_path) || file.exists(full_path)) {
      existing <- c(existing, file_path)
    } else {
      missing <- c(missing, file_path)
    }
  }
  
  return(list(existing = existing, missing = missing))
}

#' Main function to run the analysis
main <- function() {
  # Get command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  directory <- if (length(args) > 0) args[1] else "."
  
  cat("Searching for sourced files in .qmd files in directory:", 
      normalizePath(directory), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Find sourced files
  results <- find_sourced_files(directory)
  sourced_by_file <- results$by_file
  all_sourced <- results$all_files
  
  if (length(sourced_by_file) == 0) {
    return()
  }
  
  # Display results by .qmd file
  cat("\nFiles sourced by each .qmd file:\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  for (qmd_file in names(sourced_by_file)) {
    cat("\n", basename(qmd_file), ":\n", sep = "")
    files <- sourced_by_file[[qmd_file]]
    
    if (length(files) > 0) {
      unique_files <- unique(files)
      for (file in sort(unique_files)) {
        cat("  -", file, "\n")
      }
    } else {
      cat("  (no .R files found)\n")
    }
  }
  
  # Display unique list of all sourced files
  cat("\n\nAll unique sourced files (", length(all_sourced), " total):\n", sep = "")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  if (length(all_sourced) > 0) {
    for (file in sort(all_sourced)) {
      cat("  -", file, "\n")
    }
  } else {
    cat("  (no files found)\n")
  }
  
  # Check file existence
  if (length(all_sourced) > 0) {
    cat("\n\nFile existence check:\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    
    existence_check <- check_file_existence(all_sourced, directory)
    
    cat("\nExisting files (", length(existence_check$existing), "):\n", sep = "")
    if (length(existence_check$existing) > 0) {
      for (file in sort(existence_check$existing)) {
        cat("  ✓", file, "\n")
      }
    }
    
    if (length(existence_check$missing) > 0) {
      cat("\nMissing files (", length(existence_check$missing), "):\n", sep = "")
      for (file in sort(existence_check$missing)) {
        cat("  ✗", file, "\n")
      }
    }
  }
  
  # Save distinct .R files to CSV
  if (length(all_sourced) > 0) {
    cat("\nSaving distinct .R files to CSV...\n")
    
    # Create a data frame with the file information
    existence_check <- check_file_existence(all_sourced, directory)
    
    # Create a comprehensive data frame
    df_files <- data.frame(
      file_path = sort(all_sourced),
      exists = sort(all_sourced) %in% existence_check$existing,
      stringsAsFactors = FALSE
    )
    
    # Add count of how many .qmd files reference each .R file
    df_files$referenced_by_count <- sapply(df_files$file_path, function(file) {
      sum(sapply(sourced_by_file, function(files) file %in% files))
    })
    
    # Add which .qmd files reference each .R file
    df_files$referenced_by <- sapply(df_files$file_path, function(file) {
      qmd_names <- names(sourced_by_file)[sapply(sourced_by_file, function(files) file %in% files)]
      paste(basename(qmd_names), collapse = "; ")
    })
    
    # Generate filename with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    csv_filename <- paste0("sourced_R_files", ".csv")
    
    # Write to CSV
    write.csv(df_files, csv_filename, row.names = FALSE)
    
    cat("✓ Saved", nrow(df_files), "distinct .R files to:", csv_filename, "\n")
    cat("  Columns: file_path, exists, referenced_by_count, referenced_by\n")
  } else {
    cat("\nNo .R files found to save.\n")
  }
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}

path_prefix <- "C:\\\\Users\\\\mbbx4sb5\\\\Dropbox (The University of Manchester)\\\\00_ICS_RECODE_Shared\\\\"
x=read.csv("sourced_R_files.csv")%>% filter(grepl("\\.R$", file_path, ignore.case = TRUE)) %>% 
  mutate(clean=gsub(path_prefix, "", file_path, fixed = TRUE))

for(i in x$clean){
  file.copy(paste0(path_prefix, i), i)
}


