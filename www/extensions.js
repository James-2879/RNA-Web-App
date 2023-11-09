// UI functions.

// Prevent access to specified tabs.
shinyjs.disableTab = function (name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function (e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

// Enable access to specified tabs.
shinyjs.enableTab = function (name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}

// Collapse specified boxes.
shinyjs.collapse = function (boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}

// Switch to a 'collapsed' sidebar.
shinyjs.sidebarState = function () {
  var el2 = document.querySelector(".skin-black");
  el2.className = "skin-black sidebar-mini";
  var clicker = document.querySelector(".sidebar-toggle");
  clicker.id = "switchState";
}

// Hide the title (for use when collapsing sidebar).
shinyjs.changeTitleVisibility = function () {
  var title = document.querySelector(".logo")
  if (title.style.visibility == "hidden") {
    title.style.visibility = "visible";
  } else {
    title.style.visibility = "hidden";
  }
}