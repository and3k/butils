#' Automatically create, update, and validate package files
#'
#' @param quiet If `TRUE`, all output is suppressed.
#' @inheritParams rcmdcheck::rcmdcheck
#' @examples
#' \dontrun{
#' package_magic()
#' }
#' @export
package_magic <- function(quiet = TRUE, error_on = "note") {
  create_package_files(quiet = quiet)
  update_package_files(quiet = quiet)
  validate_package_files(quiet = quiet, error_on = error_on)
}

#' Automatically create package files
#'
#' @inheritParams package_magic
create_package_files <- function(quiet) {
  # GitHub Action for R CMD check:
  # delete file, because changed files can’t be quietly overwritten by use_github_action()
  github_action_check_path <- usethis::proj_path(".github", "workflows", "R-CMD-check.yaml")
  if(file.exists(github_action_check_path)) {
    file.remove(github_action_check_path)
  }
  usethis::use_github_action("check-standard.yaml", save_as = "R-CMD-check.yaml")
  github_action_check_path %>%
    readLines() %>%
    stringr::str_replace("^(name:).+", "\\1 R CMD check") %>%
    cat(file = github_action_check_path, sep = "\n")
}

#' Automatically update package files
#'
#' @inheritParams package_magic
update_package_files <- function(quiet) {
  devtools::document(quiet = quiet)
}

#' Automatically update package files
#'
#' This function is read-only.
#'
#' @inheritParams package_magic
validate_package_files <- function(quiet, error_on) {
  lintr::lint_package()
  rcmdcheck::rcmdcheck(quiet = quiet, error_on = error_on)
}
