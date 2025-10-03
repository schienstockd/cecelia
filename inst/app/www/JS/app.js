// unbind a datatable. Needs to be done before a table is redrawn.
// https://stackoverflow.com/questions/60098909/text-input-in-dtdatatable-unbinds-and-i-cant-rebind-it
Shiny.addCustomMessageHandler('unbind-DT', function(id) {
  Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
});

// restore bookmark
Shiny.addCustomMessageHandler('restoreBookmark', function(url) {
  window.location.replace(url);
});

// expand shinydashboard menu items
// https://stackoverflow.com/questions/32465177/how-to-manually-expand-a-submenu-in-a-shiny-dashboard-side-bar
Shiny.addCustomMessageHandler('selectMenuItem', function(i) {
  setTimeout(function(){
    $('.treeview > a').eq(i).click();
  }, 200);
});

Shiny.addCustomMessageHandler('selectMenuSubItem', function(i) {
    setTimeout(function(){
        $('.treeview-menu > li > a').eq(i).click();
    }, 800);
});

/*
// push data back to shiny
Shiny.addCustomMessageHandler('pushDataBackToShiny', function(id) {
  // get back to Shiny
  Shiny.setInputValue('JSretGetInputClass', idClass, {priority: "event"});
});
*/