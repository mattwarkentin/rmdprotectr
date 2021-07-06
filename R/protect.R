#' Password Protect an R Markdown File
#'
#' @description `protect_rmd` provides lightweight password protection for your
#'   HTML format R Markdown reports. Simply point `protect_rmd` to the location
#'   of your file and provide a password.
#'
#' @param input Path to the input R Markdown file to be rendered.
#' @param output Path to where the output files should be saved.
#' @param pw Password.
#' @param style Object returned from `stylize()`.
#' @param ... Additional arguments passed to `rmarkdown::render()`.
#'
#' @return `input`, invisibly.
#'
#' @export
protect_rmd <- function(
  input,
  output = NULL,
  pw,
  style = stylize(),
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

  write_index_html(output, style)

  invisible(input)
}

write_index_html <- function(path, style) {
  index_html <- system.file("index.html", package = "rmdprotectr")

  template <- readLines(index_html)

  filled <- whisker::whisker.render(
    template = template,
    data = style
  )

  writeLines(
    text = filled,
    con = paste0(path, "/index.html"),
    sep = "\n"
  )
}

#' Stylize Landing Page
#'
#' @description Use this to style the appearance of the landing page
#'   for the protected Rmd file.
#'
#' @param header_text Header text.
#' @param incorrect_text Incorrect password text.
#' @param placeholder Password input placeholder.
#' @param button_text Button text.
#' @param font_family Font family.
#' @param background_color Background color.
#' @param font_color Font color.
#' @param box_color Box color.
#' @param btn_font_color Button font color.
#' @param btn_bg_color Button background color.
#' @param btn_hover_color Button hover color.
#' @param alert_font_color Alert font color.
#' @param alert_bg_color Alert background color.
#' @param alert_border_color Alert border color.
#'
#' @return A list.
#'
#' @examples
#'
#' stylize(font_family = "Times")
#'
#' @export

stylize <- function(
  header_text = "Please enter the password",
  incorrect_text = "Incorrect password",
  placeholder = "password",
  button_text = "Continue",
  font_family = "-apple-system",
  background_color = "#f2f2f2",
  font_color = "#2d3737",
  box_color = "#ffffff",
  btn_font_color = "#ffffff",
  btn_bg_color = "#228843",
  btn_hover_color = "#1c6d36",
  alert_font_color = "#a94442",
  alert_bg_color = "#f2dede",
  alert_border_color = "#ebccd1"
) {
  structure(
    list(
      header_text = header_text,
      incorrect_text = incorrect_text,
      placeholder = placeholder,
      button_text = button_text,
      font_family = font_family,
      background_color = background_color,
      font_color = font_color,
      box_color = box_color,
      btn_font_color = btn_font_color,
      btn_bg_color = btn_bg_color,
      btn_hover_color = btn_hover_color,
      alert_font_color = alert_font_color,
      alert_bg_color = alert_bg_color,
      alert_border_color = alert_border_color
    ),
    class = "rmdprotectr_styling"
  )
}
