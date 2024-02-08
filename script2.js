$(document).ready(function() {
  const elements = document.querySelectorAll('.BP_nlink')
  
  elements.forEach(element => {
    element.addEventListener('click', () => Shiny.onInputChange("BPn", element.id))
  })
  
})