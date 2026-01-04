#' Retrieve a specific candidate by their FEC ID
#'
#' @param candidate_id A character string of the candidate's FEC ID.
#' @param cycle A numeric value of the two-year election cycle.
#'
#' @returns a `tibble` of a single candidate
#' @export
#'
#' @examplesIf has_api_key()
#' get_candidate(candidate_id = "H6TX18232")
#'
#' If you're unfamiliar with candidate IDs, using
#' @list_candidates() will help you locate candidates based on other attributes.

# using Chris Menefee as a test_id H6TX18232


#' @endpoint /v1/candidate/{candidate_id}/
#' detailed information about a particular candidate
get_candidate <- function(candidate_id, clean = TRUE) {

  req <- httr2::request(api_url()) |>
    httr2::req_url_path_append('candidate', candidate_id) |>
    httr2::req_url_query(api_key = api_key)

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (isTRUE(clean)) {
    out <- resp |>
      list_to_row() |>
      add_singletons(resp)

    out[[3]][[1]]
  } else {
    resp
  }
}

#' @endpoint /v1/candidate/{candidate_id}/history/
#' candidate's characteristics over time. This is particularly useful if
#' the candidate runs for the same office in different districts or
#' you want to know more about a candidate's previous races.
#'
#' This information is organized by candidate_id, so it won't help you find a candidate
#' who ran for different offices over time; candidates get a new ID for each office.
get_candidate_history <- function(candidate_id, clean = TRUE) {
  req <- httr2::request(api_url()) |>
    httr2::req_url_path_append('candidate', candidate_id, 'history') |>
    httr2::req_url_query(api_key = api_key)

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (isTRUE(clean)) {
    out <- resp |>
      list_to_row() |>
      add_singletons(resp)

    out[[3]][[1]]
  } else {
    resp
  }
}

#' not sure how this is different than @get_candidate_history()
get_candidate_cycle <- function(candidate_id, cycle, clean = TRUE) {
  req <- httr2::request(api_url()) |>
    httr2::req_url_path_append('candidate', candidate_id, 'history', cycle) |>
    httr2::req_url_query(api_key = api_key)

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (isTRUE(clean)) {
    out <- resp |>
      list_to_row() |>
      add_singletons(resp)

    out[[3]][[1]]
  } else {
    resp
  }
}

#' @endpoint /v1/candidate/{candidate_id}/totals/
#' information about a committee's Form 3, Form 3X, or Form 3P financial reports,
#' which are aggregated by two-year period, @param cycle
#'
#' The cycle is named after the even-numbered year and includes the year before it.
#' To obtain totals from 2013 and 2014, you would use 2014.
#' In odd-numbered years, the current cycle is the next year
#' (i.e. in 2015, the current cycle is 2016).
#'
#' For presidential and Senate candidates, multiple two-year cycles exist between elections.
get_candidate_totals <- function(candidate_id, clean = TRUE) {
  req <- httr2::request(api_url()) |>
    httr2::req_url_path_append('candidate', candidate_id, 'totals') |>
    httr2::req_url_query(
      api_key = api_key
    )

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  dplyr::bind_rows(resp$results)
}




