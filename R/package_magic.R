#' Automatically create, update, and validate package files
#'
#' @param quiet Should all output be suppressed?
#' @param github_actions Should GitHub Action files be written?
#' @param rcmdcheck_args Passed on to `args` argument in [rcmdcheck::rcmdcheck()].
#' @inheritParams rcmdcheck::rcmdcheck
#' @examples
#' \dontrun{
#' package_magic()
#' }
#' @export
package_magic <- function(quiet = TRUE, github_actions = TRUE, rcmdcheck_args = "--no-manual", error_on = "note") {
  create_package_files(quiet = quiet, github_actions = github_actions)
  update_package_files(quiet = quiet)
  validate_package_files(quiet = quiet, rcmdcheck_args = rcmdcheck_args, error_on = error_on)
}

#' Automatically create package files
#'
#' @inheritParams package_magic
create_package_files <- function(quiet = TRUE, github_actions = TRUE) {
  # TODO: quiet is not working because use_testthat() has no quiet argument
  # NOTE: this will cause an R CMD check if there are no tests
  #  i.e., a test needs to added too usethis::use_test('fun')
  usethis::use_testthat()

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
  # TODO: quiet is not working because use_github_action() has no quiet arg
  usethis::use_github_action("check-standard.yaml", save_as = "R-CMD-check.yaml")
  github_action_check_path %>%
    readLines() %>%
    stringr::str_replace("^(name:).+", "\\1 R CMD check") %>%
    stringr::str_subset("r: 'devel'", negate = TRUE) %>%
    stringr::str_replace("error_on = \"warning\"", "error_on = \"note\"") %>%
    stringr::str_remove(", \"--as-cran\"") %>%
    cat(file = github_action_check_path, sep = "\n")

  # GitHub Action for lintr:
  usethis::use_github_action("lint.yaml")
}

#' Automatically update package files
#'
#' @inheritParams package_magic
update_package_files <- function(quiet = TRUE) {
  usethis::use_tidy_description()

  devtools::document(quiet = quiet)
}

#' Automatically update package files
#'
#' This function is read-only.
#'
#' @inheritParams package_magic
validate_package_files <- function(quiet = TRUE, rcmdcheck_args = "--no-manual", error_on = "note") {
  # TODO: this isn’t acutally working
  # TODO: quiet is not working: there are dots printed
  lintr::lint_package()

  # TODO: quiet is not working: the final results are still printed
  rcmdcheck::rcmdcheck(quiet = quiet, args = rcmdcheck_args, error_on = error_on)
}
