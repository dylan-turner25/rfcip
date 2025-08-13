#!/usr/bin/env Rscript

# Precompile vignettes for rfcip package
# This script processes .Rmd.orig files to create precompiled vignettes
# Run this script manually before package releases

library(knitr)
library(rmarkdown)

# Set up knitr options for vignette processing
opts_knit$set(root.dir = getwd())

# Define vignette processing function
precompile_vignette <- function(orig_file, output_file = NULL) {
  if (is.null(output_file)) {
    # For precompiled vignettes, we want the final .Rmd to contain processed markdown
    rmd_file <- gsub("\\.orig$", "", orig_file)
  }
  
  message("Precompiling: ", basename(orig_file))
  message("Output: ", basename(rmd_file))
  
  # Set figure path for this vignette (relative to vignettes directory)
  fig_path <- file.path("figures", gsub("\\.Rmd.*$", "", basename(orig_file)))
  
  # Configure knitr options
  opts_chunk$set(
    echo = TRUE,
    fig.path = paste0(fig_path, "/"),
    fig.width = 7,
    fig.height = 5,
    comment = "#>",
    collapse = TRUE
  )
  
  # Create intermediate .md file for inspection
  md_file <- gsub("\\.Rmd\\.orig$", ".md", orig_file)
  
  # First, knit to markdown
  knit(input = orig_file, output = md_file)
  
  # Read the files
  orig_lines <- readLines(orig_file)
  md_lines <- readLines(md_file)
  
  # Find where YAML ends in original file
  yaml_end <- which(orig_lines == "---")[2]
  yaml_header <- orig_lines[1:yaml_end]
  
  # Find where content starts in md file (skip any YAML if present)
  if (md_lines[1] == "---") {
    content_start <- which(md_lines == "---")[2] + 1
  } else {
    content_start <- 1
  }
  md_content <- md_lines[content_start:length(md_lines)]
  
  # Combine YAML header with processed markdown content
  final_content <- c(yaml_header, "", md_content)
  
  # Write the final .Rmd file
  writeLines(final_content, rmd_file)
  
  message("Kept intermediate markdown file: ", basename(md_file))
  
  message("Successfully precompiled: ", basename(rmd_file))
}

# Create figures directory if it doesn't exist
if (!dir.exists("vignettes/figures")) {
  dir.create("vignettes/figures", recursive = TRUE)
}

# Process all .Rmd.orig files in vignettes directory
orig_files <- list.files("vignettes", pattern = "\\.Rmd\\.orig$", full.names = TRUE)

if (length(orig_files) == 0) {
  message("No .Rmd.orig files found in vignettes directory")
} else {
  message("Found ", length(orig_files), " vignette(s) to precompile:")
  for (orig_file in orig_files) {
    message("- ", basename(orig_file))
  }
  message("")
  
  # Process each vignette
  for (orig_file in orig_files) {
    tryCatch({
      precompile_vignette(orig_file)
    }, error = function(e) {
      message("Error processing ", basename(orig_file), ": ", e$message)
    })
  }
}

