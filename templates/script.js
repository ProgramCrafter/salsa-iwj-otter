//

// xxx deployment note: need a whole bunch of domains for SSE conn limit

status_node = document.getElementById('spong');
status_node.innerHTML = 'js-done'

var our_dnd_type = "text/puvnex-game-server-dummy";

dragthresh = 5;

  console.log('foo1');

function drag_mousedown(e) {
  console.log('mousedown', e);
  dcx = e.clientX;
  dcy = e.clientY;
  dragging = false;
  window.addEventListener('mousemove', drag_mousemove, true);
  window.addEventListener('mouseup',   drag_mouseup,   true);
}

function drag_mousemove(e) {
  ddx = e.clientX - dcx;
  ddy = e.clientY - dcy;
  if (!dragging) {
    ddr2 = ddx*ddx + ddy*ddy;
    if (ddr2 > dragthresh) {
      dragging = true;
    }
  }
  console.log('mousemove',
	      ddx, ddy, dragging);
}

function drag_mouseup(e) {
  console.log('mouseup');
  drag_mousemove(e);
  window.removeEventListener('mousemove', drag_mousemove, true);
  window.removeEventListener('mouseup',   drag_mouseup,   true);
  if (dragging) {
    console.log('dragged', ddx, ddy);
  } else {
    console.log('clicked');
  }
}

es = new EventSource("updates");
es.onmessage = function(event) {
  status_node.innerHTML = event.data;
}
