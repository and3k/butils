#' Automatically create, update, and validate package files
#'
#' @param quiet Should all output be suppressed?
#' @param github_actions Should GitHub Action files be written?
#' @inheritParams rcmdcheck::rcmdcheck
#' @examples
#' \dontrun{
#' package_magic()
#' }
#' @export
package_magic <- function(quiet = TRUE, github_actions = TRUE, error_on = "note") {
  create_package_files(quiet = quiet, github_actions = github_actions)
  update_package_files(quiet = quiet)
  validate_package_files(quiet = quiet, error_on = error_on)
}

#' Automatically create package files
#'
#' @inheritParams package_magic
create_package_files <- function(quiet = TRUE, github_actions = TRUE) {
  if(rlang::is_true(github_actions)) {
    create_package_files_for_github_actions(quiet = quiet)
  }
}

#' Automatically create package files for GitHub Actions
#'
#' @inheritParams package_magic
create_package_files_for_github_actions <- function(quiet = TRUE) {
  # GitHub Action for R CMD check:

  github_action_check_path <-
    usethis::proj_path(".github", "workflows", "R-CMD-check.yaml")
  if(file.exists(github_action_check_path)) {
    # delete first because modified files can not be quietly overwritten
    # by use_github_action()
    file.remove(github_action_check_path)
  }
  usethis::use_github_action("check-standard.yaml", save_as = "R-CMD-check.yaml")
  github_action_check_path %>%
    readLines() %>%
    stringr::str_replace("^(name:).+", "\\1 R CMD check") %>%
    stringr::str_subset("r: 'devel'", negate = TRUE) %>%
    cat(file = github_action_check_path, sep = "\n")

  # GitHub Action for lintr:
  usethis::use_github_action("lint.yaml")
}

#' Automatically update package files
#'
#' @inheritParams package_magic
update_package_files <- function(quiet = TRUE) {
  devtools::document(quiet = quiet)
}

#' Automatically update package files
#'
#' This function is read-only.
#'
#' @inheritParams package_magic
validate_package_files <- function(quiet = TRUE, error_on = "note") {
  lintr::lint_package()
  rcmdcheck::rcmdcheck(quiet = quiet, error_on = error_on)
}
