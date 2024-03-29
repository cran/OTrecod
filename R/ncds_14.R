#' National Child Development Study: a sample of the first four waves of data collection
#'
#' This database is a sample of the first four waves of data collection of the National Child Development Study (NCDS)
#' started in 1958 (\url{https://cls.ucl.ac.uk/cls-studies/1958-national-child-development-study/}).
#' The NCDS project is a continuing survey which follows the lives of over 17,000 people born in England,
#' Scotland and Wales in a same week of the year 1958.
#'
#' The ncds identifier have been voluntarily anonymized to allow their availability for the package.
#'
#' This sample has 5,476 participants included in the study between the first and fourth wave of data collection.
#'
#'
#' @format A data.frame with 5,476 participants (rows) and 6 variables
#' \describe{
#'   \item{ncdsid}{the anonymised ncds identifier}
#'   \item{GO90}{the Goldthorp social class 90 scale coded as a 12-levels factor: higher-grade professionals \code{10},
#'   lower-grade professionals \code{20}, routine non-manual employees with higher grade (administration, commerce) \code{31},
#'   routine non-manual employees with lower grade (sales and services) \code{32}, small proprietors with employees \code{41},
#'   small proprietors without employees \code{42}, farmers, small holders and workers in primary production \code{43},
#'   lower-grade technicians \code{50}, skilled manual workers \code{60}, semi-skilled and unskilled manual workers \code{71},
#'   other workers in primary production \code{72}, and \code{0} when the scale was not applicable to the participant. This variable has 806 NAs.}
#'   \item{health}{the health status of the participant stored in a 4 ordered levels factor: \code{1} for excellent,
#'   \code{2} for good, \code{3} for fair, \code{4} for poor. This variable has 2 NAs.}
#'   \item{employ}{the employment status at inclusion stored in a 7-levels factor: \code{1} for unemployed status, \code{2} for govt sheme,
#'   \code{3} for full-time education, \code{4} for housework or childcare, \code{5} for sick or handicapped, \code{6} for other, \code{7}
#'   if employed between 16 and 33. This variable has 58 NAs.}
#'   \item{gender}{the gender of the participant stored in a 2-levels factor: \code{1} for male, \code{2} for female}
#'   \item{study}{a 2-level factor equals to \code{1} for participant with completed graduate studies or \code{2} otherwise}
#' }
#' @source INSERM - This database is a sample of the National Child Development Study
"ncds_14"
