#' Password Protect an R Markdown File
#'
#' @description `protect_rmd` provides lightweight password protection for your
#'   HTML format R Markdown reports. Simply point `protect_rmd` to the location
#'   of your file and provide a password.
#'
#' @param path Path to a R Markdown file.
#' @param pw Password.
#' @param ... Additional arguments passed to `rmarkdown::render`.
#'
#' @return `path`, invisibly.
#'
#' @export
protect_rmd <- function(path, pw, ...){
  rmd_dir <- fs::path_dir(fs::path_real(path))

  hash <- openssl::sha1(pw)

  hash_dir <- paste0(rmd_dir, "/", hash)

  if(!fs::dir_exists(hash_dir)) fs::dir_create(hash_dir)

  rmarkdown::render(
    path,
    output_file = paste0(hash_dir, "/index.html"),
    ...
  )

  write_index_html(rmd_dir)

  invisible(path)
}

write_index_html <- function(path) {
  index_html <- system.file("index.html", package = "rmdprotectr")
  writeLines(
    text = readLines(index_html),
    con = paste0(path, "/index.html"),
    sep = "\n"
  )
}
