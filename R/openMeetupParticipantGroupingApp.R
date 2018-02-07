#' Open MeetupParticipantGrouping App
#'
#' This function opens the MeetupParticipantGrouping example app for editing.
#' @export
#' @examples
#' openMeetupParticipantGroupingExampleApp()

openMeetupParticipantGroupingApp <- function(){

  file.edit(file.path(find.package("Jan2018ShinyMeetupCRUG"),"Apps", "MeetupParticipantGrouping","app.R"))

}


