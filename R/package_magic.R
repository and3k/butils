#' Automatically create, update, and validate package files
#'
#' @param quiet Should all output be suppressed?
#' @param github_actions Should GitHub Action files be written?
#' @param rcmdcheck_args
#'   Passed on to `args` argument in [rcmdcheck::rcmdcheck()].
#' @inheritParams rcmdcheck::rcmdcheck
#' @inheritParams usethis::use_spell_check
#' @examples
#' \dontrun{
#' package_magic()
#' }
#' @export
package_magic <- function(quiet = TRUE, lang = "en-GB",
                          github_actions = TRUE,
                          rcmdcheck_args = "--no-manual",
                          error_on = "note") {
  # TODO: quiet is not working mostly, take a capture the output approach!
  #   see devtools::document

  create_package_files(
    quiet = quiet,
    lang = lang,
    github_actions = github_actions
  )

  update_package_files(
    quiet = quiet
  )

  validate_package_files(
    quiet = quiet,
    rcmdcheck_args = rcmdcheck_args,
    error_on = error_on
  )

  return(invisible())
}

#' Automatically create package files
#'
#' @inheritParams package_magic
create_package_files <- function(quiet = TRUE, lang = "en-GB",
                                 github_actions = TRUE) {
  if (!file.exists(usethis::proj_path("DESCRIPTION"))) {
    usethis::use_description()
  }
  if (!file.exists(usethis::proj_path("NAMESPACE"))) {
    usethis::use_namespace()
  }

  usethis::use_rstudio()

  gitignore_from <-
    "https://raw.githubusercontent.com/github/gitignore/master/R.gitignore"
  gitignore_contents <- readLines(gitignore_from)
  gitignore_to <- usethis::proj_path(".gitignore")
  writeLines(gitignore_contents, gitignore_to)

  # TODO: quiet is not working because use_testthat() has no quiet argument
  # NOTE: this will cause an R CMD check if there are no tests
  #  i.e., a test needs to added too usethis::use_test('fun')
  usethis::use_testthat()

  # BUG: rcmdcheck::rcmdcheck doesn’t seem to trigger the spell check?
  # TODO: quiet is not working because use_spell_check() has no quiet argument
  usethis::use_spell_check(lang = lang, error = TRUE)

  if (rlang::is_true(github_actions)) {
    create_github_action_files(quiet = quiet)
  }
}

#' Automatically create package files for GitHub Actions
#'
#' @inheritParams package_magic
create_github_action_files <- function(quiet = TRUE) {
  # GitHub Action for R CMD check:

  github_action_check_path <-
    usethis::proj_path(".github", "workflows", "R-CMD-check.yaml")
  if (file.exists(github_action_check_path)) {
    # delete first because modified files can not be quietly overwritten
    # by use_github_action()
    file.remove(github_action_check_path)
  }
  # TODO: quiet is not working because use_github_action() has no quiet arg
  usethis::use_github_action(
    "check-standard.yaml",
    save_as = "R-CMD-check.yaml"
  )
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

  # TODO: quiet is not working because style_pkg() has no quiet argument
  styler::style_pkg(usethis::proj_get())

  devtools::document(quiet = quiet)
}

#' Automatically update package files
#'
#' This function is read-only.
#'
#' @inheritParams package_magic
validate_package_files <- function(quiet = TRUE,
                                   rcmdcheck_args = "--no-manual",
                                   error_on = "note") {
  # TODO: these commands can be run in parallel

  # TODO: quiet is not working: there are dots printed
  lint_result <- lintr::lint_package()
  print(lint_result)

  if (rstudioapi::isAvailable()) {
    rstudioapi::executeCommand("checkPackage")
  } else {
    # BUG: rcmdcheck::rcmdcheck doesn’t seem to trigger the spell check?
    # TODO: quiet is not working: the final results are still printed
    rcmdcheck::rcmdcheck(
      quiet = quiet,
      args = rcmdcheck_args,
      error_on = error_on
    )
  }
}
