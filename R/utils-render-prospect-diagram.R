
#' Render an RMarkdown Document and Write Contents as PNG
#'
#' @param rmd_doc File path to input RMarkdown Document
#' @param image_output_path Where should the png file be written?
#'
#' @return The path of the generated png (invisibly)
#' @export
#'
#' @examples
render_prospect_figure <- function(
  rmd_doc,
  image_output_path
  ) {

  tmp <- tempfile(fileext = ".pdf")
  on.exit(
    unlink(tmp),
    add = TRUE
  )

  rmarkdown::render(
    input = rmd_doc,
    output_format = "pdf_document",
    output_file = tmp
  )

  magick::image_read_pdf(
    path = tmp
  ) %>%
    magick::image_trim() %>%
    magick::image_write(
      path = image_output_path
    )


  invisible(image_output_path)

}
