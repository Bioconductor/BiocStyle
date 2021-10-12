$(document).ready(function ()  {
  
  // Map "enter" keypress to the same action as a cursor click
  function navigateLink(e) {
    if (e.key === "Enter") {
      $(this).trigger("click");
    }
  }

  var toc_items = document.querySelectorAll(".tocify-item");
  for (var i = 0; i < toc_items.length; i++) {
    // The link role tells screen readers this is for navigation
    toc_items.item(i).setAttribute("role", "link");
    // tabindex = 0 allows selection via keyboard tab presses
    toc_items.item(i).setAttribute("tabindex", "0");
    // Listen for "Enter" keypress when item is selected
    toc_items.item(i).addEventListener("keydown", navigateLink);
  }
});
