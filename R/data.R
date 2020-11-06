#' Integrated Database (IDB)
#'
#' A dataset containing Football Players data of two (2) main sources:
#' Football Manager
#' Footy Stats
#' 
#' The dataset includes a PCA tranformation containing 80% of Explained Variance
#' 
#'
#' @format A data frame with 14103 rows and 585 variables:
#' \describe{
#'   \item{season}{football season}
#'   \item{player_id}{player's unique id per season}
#'   \item{unique_id}{player's unique id}
#'   \item{player_name_fm}{player's Name in FM database}
#'   \item{club}{club who has the player's rights}
#'   \item{club_id}{player's club unique id per season}
#'   \item{club_current}{club in which the player currently plays. Migth be on loan}
#'   ...
#' }
"idb"