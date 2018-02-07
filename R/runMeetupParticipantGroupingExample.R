#' Run MeetupParticipantGrouping Example Shiny App
#'
#' This function runs the MeetupParticipantGrouping example app.
#' @export
#' @examples
#' runMeetupParticipantGroupingExample()

runMeetupParticipantGroupingExample <- function(){

  shiny::runApp(file.path(find.package("Jan2018ShinyMeetupCRUG"),"Apps", "MeetupParticipantGrouping"))


}
