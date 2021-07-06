#' Password Protect an R Markdown File
#'
#' @description `protect_rmd` provides lightweight password protection for your
#'   HTML format R Markdown reports. Simply point `protect_rmd` to the location
#'   of your file and provide a password.
#'
#' @param input Path to the input R Markdown file to be rendered.
#' @param output Path to where the output files should be saved.
#' @param pw Password.
#' @param ... Additional arguments passed to `rmarkdown::render()`.
#'
#' @return `input`, invisibly.
#'
#' @export
protect_rmd <- function(
  input,
  output = NULL,
  pw,
  ...
){
  input <- fs::path_real(input)

  if (is.null(output)) {
    output <- fs::path_dir(input)
  } else {
    output <- fs::path_real(output)
  }

  hash <- openssl::sha1(pw)

  hash_dir <- paste0(output, "/", hash)

  if (!fs::dir_exists(hash_dir)) fs::dir_create(hash_dir)

  rmarkdown::render(
    input = input,
    output_file = paste0(hash_dir, "/index.html"),
    ...
  )

  write_index_html(output)

  invisible(input)
}

write_index_html <- function(path) {
  index_html <- system.file("index.html", package = "rmdprotectr")
  writeLines(
    text = readLines(index_html),
    con = paste0(path, "/index.html"),
    sep = "\n"
  )
}
