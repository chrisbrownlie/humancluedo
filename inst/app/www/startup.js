$(document).on('shiny:connected', function(event) {
  // Get iframe source url
  src = parent.document.getElementsByTagName('iframe')[0].getAttribute('src')

  // Create link for setting favicon in the browser
  var link = parent.document.createElement('link');
  link.rel = 'shortcut icon'
  link.href = src + 'favicon.svg'

  // Append link to the head of the document containing the iframe
  parent.document.getElementsByTagName('head')[0].appendChild(link);
}());
