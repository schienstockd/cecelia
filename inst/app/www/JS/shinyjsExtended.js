// make boxes collapsable via shinyjs
// https://stackoverflow.com/a/32110791/13766165
shinyjs.collapseBox = function(boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
