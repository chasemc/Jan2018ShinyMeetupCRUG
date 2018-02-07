#' Run Beginner Example Shiny App
#'
#' This function runs the beginner example app.
#' @export
#' @examples
#' runBeginnerExample()

runBeginnerExample <- function(){

  shiny::runApp(file.path(find.package("Jan2018ShinyMeetupCRUG"),"Apps", "BeginnerExample"),display.mode = "showcase")

}
