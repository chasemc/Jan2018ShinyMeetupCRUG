#' Open Beginner-Intermediate App
#'
#' This function opens the Beginner-Intermediate example app for editing.
#' @export
#' @examples
#' openBeginnerIntermediateExampleApp()

openBeginnerIntermediateExampleApp <- function(){

  file.edit(file.path(find.package("Jan2018ShinyMeetupCRUG"),"Apps", "Beginner-Intermediate","app.R"))

}



