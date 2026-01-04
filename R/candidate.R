#' Retrieve a specific candidate by their FEC ID
#'
#' @param candidate_id
#'
#' @returns a `tibble` of a single candidate
#' @export
#'
#' @examplesIf has_api_key()
#' get_candidate(candidate_id = "H6TX18232")


#' If you're unfamiliar with candidate IDs, using
#' @list_candidates() will help you locate candidates based on other attributes.

#' using Chris Menefee as a test_id H6TX18232

get_candidate <- function (candidate_id, clean = TRUE) {

  req <- httr2::request(api_url()) |>
    httr2::req_url_path_append('candidate', candidate_id) |>
    httr2::req_url_query(api_key = api_key)

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (isTRUE(clean)) {
    resp |>
      list_to_row() |>
      add_singletons(resp)
  } else {
    resp
  }
}


