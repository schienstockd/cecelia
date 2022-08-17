## Add JS for hotkeys
addJSForClickAndX <- function(key, id){
  HTML(sprintf(
  'window.addEventListener("click", function(e) {
        if (e.%s) {
          Shiny.onInputChange("%s", "1", {priority: "event"});
        }
      }, false);'
  , key, id))
}