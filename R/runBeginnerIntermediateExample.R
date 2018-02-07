#' Run Beginner-Intermediate Example Shiny App
#'
#' This function runs the Beginner-Intermediate example app.
#' @export
#' @examples
#' runBeginnerIntermediateExample()

runBeginnerIntermediateExample <- function(){

  shiny::runApp(file.path(find.package("Jan2018ShinyMeetupCRUG"),"Apps", "Beginner-Intermediate"))


}


