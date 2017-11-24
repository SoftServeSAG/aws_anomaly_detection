window.onload = function() {
  height = Number(window.getComputedStyle(
    document.getElementsByTagName("nav")[0])["height"].slice(0,-2))
  margin = Number(window.getComputedStyle(
    document.getElementsByTagName("nav")[0])["margin-bottom"].slice(0,-2))
  panelHeight = window.innerHeight - (height + margin + 20)

  // skim though style sheets, find ui.css and set appropriate property
  for(var i = 0; i < document.styleSheets.length; i++) {
    if(document.styleSheets[i].href.includes("ui.css")) {
      for(var j = 0; j < document.styleSheets[i].cssRules.length; j++) {
        if(document.styleSheets[i].cssRules[j].selectorText == ".sad-app-container-sizeable") {
          document.styleSheets[i].cssRules[j].style.setProperty("height", panelHeight + "px")
          return
        }
      }
    }
  }
}
