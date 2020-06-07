//

// xxx deployment note: need a whole bunch of domains for SSE conn limit

status_node = document.getElementById('spong');
status_node.innerHTML = 'js-done'

var our_dnd_type = "text/puvnex-game-server-dummy";

  console.log('foo1');

function drag_mousedown(e) {
  console.log('mousedown', e);
  dcx = e.clientX;
  dcy = e.clientY;
  window.addEventListener('mousemove', drag_mousemove, true);
  window.addEventListener('mouseup',   drag_mouseup,   true);
}

function drag_mousemove(e) {
  console.log('mousemove',
	      e.clientX - dcx,
	      e.clientY - dcy);
}

function drag_mouseup(e) {
  console.log('mouseup',
	      e.clientX - dcx,
	      e.clientY - dcy);
  window.removeEventListener('mousemove', drag_mousemove, true);
  window.removeEventListener('mouseup',   drag_mouseup,   true);
}

es = new EventSource("updates");
es.onmessage = function(event) {
  status_node.innerHTML = event.data;
}
