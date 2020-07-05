//

// xxx deployment note: need a whole bunch of domains for SSE conn limit

general_timeout = 10000;
messages = Object();
var our_dnd_type = "text/puvnex-game-server-dummy";
api_queue = [];
api_posting = false;
var us;
var gen = 0;
var cseq = 0;

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
  json_report_error({
    statusText : xhr.statusText,
    responseText : xhr.responseText,
  });
}

function json_report_error(error_json) {
  let error_message = JSON.stringify(error_json);
  let errornode = document.getElementById('error');
  errornode.textContent = 'Error (reloading may help?):' + error_message;
}

function api(meth, data) {
  api_queue.push([meth, data]);
  api_check();
}
function api_delay(meth, data) {
  api_queue.push([meth, data]);
  window.setTimeout(api_check, 10);
}
function api_check() {
  if (api_posting) { return; }
  if (!api_queue.length) { return; }
  do {
    var [meth, data] = api_queue.shift();
  } while (meth == 'm' &&
	   api_queue.length &&
	   api_queue[0][0] == meth);
  api_posting = true;
  xhr_post_then('/_/api/'+meth, JSON.stringify(data), api_posted);
}
function api_posted() {
  api_posting = false;
  api_check();
}

// ----- clicking/dragging pieces -----

// dataset
//   delt.pice     piece id (static)
//   delt.gplayer grabbed user (player id string, or "")
//   delt.cseq     client sequence (see PROTOCOL.md)

const DRAGGING = { // bitmask
  NO           : 0,
  MAYBE_GRAB   : 1,
  MAYBE_UNGRAB : 2,
  YES          : 4,
};

var delt;
var dragging = DRAGGING.NO;

function drag_mousedown(e) {
  console.log('mousedown', e);
  var piece = e.target.dataset.piece;
  if (!piece) { return; }
  drag_cancel();
  delt = e.target;
  var gplayer = delt.dataset.gplayer;
  if (gplayer != "" && gplayer != us) { return; }
  dcx = e.clientX;
  dcy = e.clientY;
  dox = parseFloat(delt.getAttributeNS(null,"x"));
  doy = parseFloat(delt.getAttributeNS(null,"y"));

  if (gplayer == us) {
    dragging = DRAGGING.MAYBE_UNGRAB;
  } else {
    cseq += 1;
    delt.dataset.cseq = cseq;

    dragging = DRAGGING.MAYBE_GRAB;
    set_grab(delt, piece, us);
    api('grab', {
      ctoken : ctoken,
      piece : piece,
      gen : gen,
      cseq : cseq,
    })
  }

  window.addEventListener('mousemove', drag_mousemove, true);
  window.addEventListener('mouseup',   drag_mouseup,   true);
}

function set_grab(elem, piece, owner) {
  elem.dataset.gplayer = owner;
//  var [p, piece] = 
  piece_cleanup_grab(elem);
  var nelem = document.createElementNS(svg_ns,'use');
  nelem.setAttributeNS(null,'href','#select'+piece);
  elem.appendChild(nelem);
}
function set_ungrab(elem) {
  elem.dataset.gplayer = "";
  piece_cleanup_grab(elem);
}
function piece_cleanup_grab(elem) {
//  var piece = elem.dataset.piece;
//  var elem = document.getElementById('piece'+piece);
  while (elem.children.length > 1) {
    elem.lastElementChild.remove();
  }
//  return [p, elem];
}

function drag_mousemove(e) {
  ctm = space.getScreenCTM();
  ddx = (e.clientX - dcx)/ctm.a;
  ddy = (e.clientY - dcy)/ctm.d;
  if (!(dragging & DRAGGING.YES)) {
    ddr2 = ddx*ddx + ddy*ddy;
    if (ddr2 > dragthresh) {
      dragging |= DRAGGING.YES;
    }
  }
  //console.log('mousemove', ddx, ddy, dragging);
  if (dragging & DRAGGING.YES) {
    var x = Math.round(dox + ddx);
    var y = Math.round(doy + ddy);
    delt.setAttributeNS(null, "x", x);
    delt.setAttributeNS(null, "y", y);
    //console.log(delt);
    api_delay('m',{
      t : token,
      p : delt.dataset.piece,
      l : [x, y],
    });
  }
}

function drag_mouseup(e) {
  console.log('mouseup', dragging);
  drag_mousemove(e);
  //console.log('mouseup ...', dragging);
  if (dragging == DRAGGING.MAYBE_UNGRAB ||
      dragging == (DRAGGING.MAYBE_GRAB | DRAGGING.YES)) {
    set_ungrab(delt);
    api('ungrab', {
      ctoken : token,
      piece : delt.dataset.piece,
    });
  }
  drag_cancel(e);
}

function drag_cancel() {
  window.removeEventListener('mousemove', drag_mousemove, true);
  window.removeEventListener('mouseup',   drag_mouseup,   true);
  dragging = DRAGGING.NO;
  delt = null;
}

// ----- test counter, startup -----

messages.PieceUpdate = function(data) {
  console.log('PIECE UPDATE ',data)
}

function startup() {
  var body = document.getElementById("main-body");
  ctoken = body.dataset.ctoken;
  us = body.dataset.us;
  gen = parseInt(body.dataset.gen);
  status_node = document.getElementById('status');
  status_node.innerHTML = 'js-done'
  dragthresh = 5;
  space = document.getElementById('space');
  svg_ns = space.getAttribute('xmlns');

  es = new EventSource("/_/updates/"+ctoken+'/'+gen);
  es.onmessage = function(event) {
    console.log('GOTEVE', event)
    var j = JSON.parse(event.data);
    var k = Object.keys(j)[0];
    messages[k](j[k]);
  }
  es.addEventListener('commsworking', function(event) {
    console.log('GOTDATA', event);
    status_node.innerHTML = event.data;
  });
  es.addEventListener('recorded', function(event) {
    var j = JSON.parse(event.data);
    var elem = document.getElementById('piece'+j.piece);
    if (j.cseq >= elem.dataset.cseq) { elem.dataset.cseq = 0 }
    gen = j.gen;
  });
  es.onerror = function(e) {
    console.log('FOO',e,es);
    json_report_error({
      updates_error : e,
      updates_event_source : es,
      updates_event_source_ready : es.readyState,
      update_oe : e.className,
    })
  }
}

function doload(){
  console.log('DOLOAD');
  var elem = document.getElementById('loading_token');
  ptoken = elem.dataset.ptoken;
  xhr_post_then('/_/session', 
		JSON.stringify({ ptoken : ptoken }),
		loaded);
}

function loaded(xhr){
  console.log('LOADED');
  var body = document.getElementById('loading_body');
  body.outerHTML = xhr.response;
  startup();
}

doload();
