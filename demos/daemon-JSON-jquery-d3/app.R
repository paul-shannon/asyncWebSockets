library(httpuv)
#library(RJSONIO)
library(jsonlite)

myws <- NULL

.lastMessage <- NULL
#--------------------------------------------------------------------------------
app <- list(
  call = function(req) {
    wsUrl = paste(sep='',
                  '"',
                  "ws://",
                  ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                  '"')
    list(
      status = 200L,
      headers = list('Content-Type' = 'text/html'),
      body = c(file="index.html"))
    },
  onWSOpen = function(ws) {
    myws <<- ws
    ws$onMessage(function(binary, rawMessage) {
      printf("=== new message received: ")
      #browser();
      print(rawMessage)
      message <- fromJSON(rawMessage);
      .lastMessage <<- message
      if(!is(message, "list")){
          printf("new websocket message is not a list");
          return;
         }
      if (! "cmd" %in% names(message)){
         printf("new websocket messages has no 'cmd' field");
         return;
         }
      cmd <- message$cmd
      if(cmd == "handleSelection"){
         printf("--- calling handleSelection");
         #browser();
         printf(" selected points: %s", paste(message$payload, collapse=","));
         handleSelection(message)
         }
      else if(cmd == "ping"){
         printf("--- received ping");
         }
      else{
         printf("unrecognized incoming command: %s",  cmd);
        }
     })
  }
)
#--------------------------------------------------------------------------------
go <- function(){
  port <- 9454
  browseURL(sprintf("http://localhost:%d", port))
  server <- startDaemonizedServer("0.0.0.0", port, app)
  }
#--------------------------------------------------------------------------------
stop <- function(server){
  stopDaemonizedServer(server)
  }
#--------------------------------------------------------------------------------
send <- function(cmd)
{
    myws$send(toJSON(cmd, auto_unbox=TRUE))

} # sendTest
#--------------------------------------------------------------------------------
selectByName <- function(names=c( "Merc 280", "Merc 280C", "Ferrari Dino"))
{
   msg <- list(cmd="selectByName", callback="", payload=names)
   myws$send(toJSON(msg, auto_unbox=TRUE))


} # selectByName
#--------------------------------------------------------------------------------
clearSelection <- function()
{
   msg <- list(cmd="clearSelection", callback="", payload="");
   myws$send(toJSON(msg, auto_unbox=TRUE))

} # clearSelection
#--------------------------------------------------------------------------------
testPlot <- function()
{
   mpg <- mtcars$mpg
   disp <- mtcars$disp
   names <- rownames(mtcars)
   msg <- list(cmd="plotxy", payload=list(x=mpg,  xMin=min(mpg),  xMax=max(mpg),
                                          y=disp, yMin=min(disp), yMax=max(disp),
                                          names=names))
   myws$send(toJSON(msg, auto_unbox=TRUE))

} # testPlot
#--------------------------------------------------------------------------------
getSelection <- function()
{
   .lastMessage <<- NULL
   myws$send(toJSON(list(cmd="getSelection", callback="handleSelection",
                         payload=""), auto_unbox=TRUE));
   while(is.null(.lastMessage)) {
       printf("sleeping in getSelection, waiting on .lastMessage");
       print(.lastMessage)
       Sys.sleep(1)
       }
   .lastMessage

} # getSelection
#--------------------------------------------------------------------------------
# fails to return without a carriage return in the console
# when this function DOES return, no values are returned.
# i suspect something in the httpuv code.
handleSelection <- function(message)
{
   printf("--- handleSelection");
   .lastMessage <<- message$payload
   #browser();
   #x <- 99
   #print(1)
   #result <- message$payload
   #print(2)
   #print(result)
   #print(3)
   #invisible(result);
   NULL

} # handleSelection
#--------------------------------------------------------------------------------

