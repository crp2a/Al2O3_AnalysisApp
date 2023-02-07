# DATASET

#' Verification Hash
#'
#' Codes are used to avoid that users upload weird sequences never approved and
#' then complain.
#' @note
#' To create a new valid hash code:
#' ```
#' temp <- Luminescence::read_XSYG2R(...)
#' digest::digest(names(temp))
#' ```
#' If `temp` is list, then type `temp[[1]]`.
#' @format A [`character`] vector.
#' @family datasets
#' @keywords datasets internal
"verification_hash"
