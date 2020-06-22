//

// xxx deployment note: need a whole bunch of domains for SSE conn limit

general_timeout = 10000;
messages = Object();
var our_dnd_type = "text/puvnex-game-server-dummy";

function xhr_post_then(url,data,good) {
  var xhr = new XMLHttpRequest();
  xhr.onreadystatechange = function(){
    if (xhr.readyState != XMLHttpRequest.DONE) { return; }
    if (xhr.status != 200) { xhr_report_error(xhr); }
    else { good(xhr); }
  };
  xhr.timeout = general_timeout;
  xhr.open('POST',url);
  xhr.setRequestHeader('Content-Type','application/json');
  xhr.send(data);
}

function xhr_report_error(xhr) {
  let error_message = JSON.stringify({
    statusText : xhr.statusText,
    responseText : xhr.responseText,
  });
  let errornode = document.getElementById('error');
  errornode.textContent = 'Error (reloading may help?):' + error_message;
}

function drag_mousedown(e) {
  drag_cancel();
  console.log('mousedown', e);
  delt = e.target;
  if (!delt.dataset.p) { return; }
  if (delt.dataset.g) { return; }
  dcx = e.clientX;
  dcy = e.clientY;
  dox = parseFloat(delt.getAttributeNS(null,"x"));
  doy = parseFloat(delt.getAttributeNS(null,"y"));
  dragging = false;
  window.addEventListener('mousemove', drag_mousemove, true);
  window.addEventListener('mouseup',   drag_mouseup,   true);
  
}

function drag_mousemove(e) {
  ctm = space.getScreenCTM();
  ddx = (e.clientX - dcx)/ctm.a;
  ddy = (e.clientY - dcy)/ctm.d;
  if (!dragging) {
    ddr2 = ddx*ddx + ddy*ddy;
    if (ddr2 > dragthresh) {
      dragging = true;
    }
  }
  console.log('mousemove',
	      ddx, ddy, dragging);
  if (dragging) {
    var x = dox + ddx;
    var y = doy + ddy;
    delt.setAttributeNS(null, "x", x);
    delt.setAttributeNS(null, "y", y);
    console.log(delt);
  }
}

function drag_mouseup(e) {
  console.log('mouseup');
  drag_mousemove(e);
  drag_cancel(e);
  if (dragging) {
    console.log('dragged', ddx, ddy);
  } else {
    console.log('clicked');
  }
}

function drag_cancel() {
  window.removeEventListener('mousemove', drag_mousemove, true);
  window.removeEventListener('mouseup',   drag_mouseup,   true);
}

messages.TestCounter = function(data) {
  status_node.innerHTML = data.value;
}

function startup() {
  status_node = document.getElementById('status');
  status_node.innerHTML = 'js-done'
  dragthresh = 5;
  space = document.getElementById('space');

  es = new EventSource("/_/updates");
  es.onmessage = function(event) {
    var j = JSON.parse(event.data);
    var k = Object.keys(j)[0];
    messages[k](j[k]);
  }
}

function doload(){
  console.log('DOLOAD');
  var elem = document.getElementById('loading_token');
  token = elem.dataset.token;
  xhr_post_then('/_/session', 
		JSON.stringify({ token : token }),
		loaded);
}

function loaded(xhr){
  console.log('LOADED');
  var body = document.getElementById('loading_body');
  body.outerHTML = xhr.response;
}

doload();
