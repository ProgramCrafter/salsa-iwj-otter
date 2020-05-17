//

// xxx deployment note: need a whole bunch of domains for SSE conn limit

status_node = document.getElementById('spong');
status_node.innerHTML = 'js-done'

es = new EventSource("updates");
es.onmessage = function(event) {
  status_node.innerHTML = event.data;
}
