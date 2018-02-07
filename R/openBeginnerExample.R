#' Open Beginner App
#'
#' This function opens the beginner example app for editing.
#' @export
#' @examples
#' openBeginnerExampleApp()

#openBeginnerExampleApp <- function()file.edit("Apps/BeginnerExample/app.R")
openBeginnerExampleApp <- function(){

  file.edit(file.path(find.package("Jan2018ShinyMeetupCRUG"),"Apps", "BeginnerExample","app.R"))



}
