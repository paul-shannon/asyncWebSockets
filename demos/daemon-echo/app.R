library(httpuv)
#--------------------------------------------------------------------------------
setup <- function(wsCon)
{
   wsCon$open <- FALSE
   wsCon$wsID <- NULL
   wsCon$ws <- NULL
   wsCon$result <- NULL

   wsCon$call = function(req) { # "call" processes http requests
     wsUrl = paste(sep='',
                   '"',
                  "ws://",
                  ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                  '"')
    list(
      status = 200L,
      headers = list('Content-Type' = 'text/html'),
      body = c(file="index.html"))
     }
   wsCon$onWSOpen = function(ws) {
      wsCon$ws <- ws
      ws$onMessage(function(binary, rawMessage) {
         print(rawMessage)
         }) # onMessage
       wsCon$open <- TRUE
       } # onWSOpen

   wsCon

} # setup
#--------------------------------------------------------------------------------
send <- function(wsCon, msg)
{
  wsCon$ws$send(msg)
  
} # send
#--------------------------------------------------------------------------------
wsCon <- new.env(parent=emptyenv())
#--------------------------------------------------------------------------------
demo <- function()
{
   wsCon <- setup(wsCon)
   port <- 8765
   browseURL(sprintf("http://localhost:%d", port))
   wsCon$id <- startDaemonizedServer("0.0.0.0", port, wsCon)

   Sys.sleep(2)
   send(wsCon, "test1")
   Sys.sleep(2)

   send(wsCon, "test2")
   Sys.sleep(2)

} # demo
#--------------------------------------------------------------------------------


