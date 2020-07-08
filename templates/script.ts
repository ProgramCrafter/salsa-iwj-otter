// -*- JavaScript -*-

// xxx deployment note: need a whole bunch of domains for SSE conn limit


// elemnts for a piece
//
// In svg toplevel
//
//   uelem
//      #use{}
//      <use id="use{}", href="#piece{}" x= y= >
//         .piece   piece id (static)
//         .gplayer  grabbed user (player id string, or "")
//         .cseq     client sequence (see PROTOCOL.md)
//      container to allow quick movement and hang stuff off
//
//   delem
//      #defs{}
//      <def id="defs{}">
//
// And in each delem
//
//   pelem
//   #piece{}
//         .dragraise   dragged more than this ?  raise to top!
//      <g id="piece{}" >
//      currently-displayed version of the piece
//      to allow addition/removal of selected indication
//      contains 1 or 2 subelements:
//      first is straight from server and not modified
//      second is possible <use href="#select{}" >
//
//   #select{}
//      generated by server, referenced by JS in pelem for selection
//
//   #def.{}.stuff
//      generated by server

type PieceId = string;
type PlayerId = string;
type Pos = [number, number];
type ClientSeq = number;
type Generation = number;

type MessageHandler = (op: Object) => void;
type PieceHandler = (piece: PieceId, info: Object) => void;
interface DispatchTable<H> { [key: string]: H };

var general_timeout : number = 10000;
var messages : DispatchTable<MessageHandler> = Object();
var pieceops : DispatchTable<PieceHandler> = Object();
var our_dnd_type = "text/puvnex-game-server-dummy";
var api_queue : [string, Object][] = [];
var api_posting = false;
var us : string;
var gen = 0;
var cseq : ClientSeq = 0;
var ctoken : string;

var svg_ns : string;
var space : SVGGraphicsElement;
var logdiv : HTMLElement;
var status_node : HTMLElement;


function xhr_post_then(url : string, data: string,
		       good : (xhr: XMLHttpRequest) => void) {
  var xhr : XMLHttpRequest = new XMLHttpRequest();
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

function xhr_report_error(xhr: XMLHttpRequest) {
  json_report_error({
    statusText : xhr.statusText,
    responseText : xhr.responseText,
  });
}

function json_report_error(error_for_json: Object) {
  let error_message = JSON.stringify(error_for_json);
  let errornode = document.getElementById('error')!;
  errornode.textContent = 'Error (reloading may help?):' + error_message;
}

function api(meth: string, data: Object) {
  api_queue.push([meth, data]);
  api_check();
}
function api_delay(meth: string, data: Object) {
  api_queue.push([meth, data]);
  window.setTimeout(api_check, 10);
}
function api_check() {
  if (api_posting) { return; }
  if (!api_queue.length) { return; }
  do {
    var [meth, data] = api_queue.shift()!;
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

function api_piece(f: (meth: string, payload: Object) => void,
		   meth: string, piece: string,
		   uelem: SVGGraphicsElement, op: Object) {
  cseq += 1;
  uelem.dataset.cseq! = cseq+"";
  f(meth, {
    ctoken : ctoken,
    piece : piece,
    gen : gen,
    cseq : cseq,
    op : op,
  })
}

function piece_element(base: string, piece: PieceId): SVGGraphicsElement | null
{
  let elem = document.getElementById(base+piece);
  return elem as unknown as (SVGGraphicsElement | null);
}

// ----- clicking/dragging pieces -----

enum DRAGGING { // bitmask
  NO           = 0,
  MAYBE_GRAB   = 1,
  MAYBE_UNGRAB = 2,
  YES          = 4,
};

var drag_uelem : SVGGraphicsElement | null;
var dragging = DRAGGING.NO;

var dox : number | null;
var doy : number | null;
var dcx : number | null;
var dcy : number | null;

const DRAGTHRESH = 5;

function drag_mousedown(e : MouseEvent) {
  console.log('mousedown', e);
  var target = e.target as SVGGraphicsElement; // we check this just now!
  var piece = target.dataset.piece;
  if (!piece) { return; }
  drag_cancel();
  drag_uelem = target;
  var gplayer = drag_uelem.dataset.gplayer;
  if (gplayer != "" && gplayer != us) { return; }
  var pelem = piece_element('piece',piece)!;

  dcx = e.clientX;
  dcy = e.clientY;
  dox = parseFloat(drag_uelem.getAttributeNS(null,"x")!);
  doy = parseFloat(drag_uelem.getAttributeNS(null,"y")!);

  if (gplayer == us) {
    dragging = DRAGGING.MAYBE_UNGRAB;
  } else {
    dragging = DRAGGING.MAYBE_GRAB;
    set_grab(drag_uelem, pelem, piece, us);
    api_piece(api, 'grab', piece, drag_uelem, { });
  }

  window.addEventListener('mousemove', drag_mousemove, true);
  window.addEventListener('mouseup',   drag_mouseup,   true);
}

function set_grab(uelem: SVGGraphicsElement, pelem: SVGGraphicsElement,
		  piece: PieceId, owner: PlayerId) {
  var nelem = document.createElementNS(svg_ns,'use');
  uelem.dataset.gplayer = owner;
  piece_cleanup_grab(pelem);
  nelem.setAttributeNS(null,'href','#select'+piece);
  nelem.setAttributeNS(null,'stroke-dasharray',"3 1  1 1  1 1");
  pelem.appendChild(nelem);
}
function set_ungrab(uelem: SVGGraphicsElement, pelem: SVGGraphicsElement) {
  uelem.dataset.gplayer = "";
  piece_cleanup_grab(pelem);
}
function piece_cleanup_grab(pelem: SVGGraphicsElement) {
  while (pelem.children.length > 1) {
    pelem.lastElementChild!.remove();
  }
}

function drag_mousemove(e: MouseEvent) {
  var ctm = space.getScreenCTM()!;
  var ddx = (e.clientX - dcx!)/ctm.a;
  var ddy = (e.clientY - dcy!)/ctm.d;
  var ddr2 = ddx*ddx + ddy*ddy;
  if (!(dragging & DRAGGING.YES)) {
    if (ddr2 > DRAGTHRESH) {
      dragging |= DRAGGING.YES;
    }
  }
  //console.log('mousemove', ddx, ddy, dragging);
  if (dragging & DRAGGING.YES) {
    var x = Math.round(dox! + ddx);
    var y = Math.round(doy! + ddy);
    drag_uelem!.setAttributeNS(null, "x", x+"");
    drag_uelem!.setAttributeNS(null, "y", y+"");
    var piece = drag_uelem!.dataset.piece!;
    var pelem = document.getElementById('piece'+piece);
    api_piece(api_delay, 'm', piece, drag_uelem!, [x, y] );
  }
  return ddr2;
}

function drag_mouseup(e: MouseEvent) {
  console.log('mouseup', dragging);
  let ddr2 : number = drag_mousemove(e);
  let piece = drag_uelem!.dataset.piece!;
  let pelem = piece_element('piece',piece)!;
  let dragraise = +pelem.dataset.dragraise!;
  console.log('CHECK RAISE ', dragraise, dragraise*dragraise, ddr2);
  if (dragraise > 0 && ddr2 >= dragraise*dragraise) {
    api_piece(api, "raise", piece, drag_uelem!, { });
  }
  if (dragging == DRAGGING.MAYBE_UNGRAB ||
      dragging == (DRAGGING.MAYBE_GRAB | DRAGGING.YES)) {
    set_ungrab(drag_uelem!, pelem);
    api_piece(api, 'ungrab', piece, drag_uelem!, { });
  }
  drag_cancel();
}

function drag_cancel() {
  window.removeEventListener('mousemove', drag_mousemove, true);
  window.removeEventListener('mouseup',   drag_mouseup,   true);
  dragging = DRAGGING.NO;
  drag_uelem = null;
}

// ----- logs -----

messages.Log = <MessageHandler>function
(j: { html: string }) {
  add_log_message(j.html);
}

function add_log_message(msg_html: string) {
  var lastent = logdiv.lastElementChild;
  var in_scrollback =
    lastent == null ||
    // inspired by
    //   https://stackoverflow.com/questions/487073/how-to-check-if-element-is-visible-after-scrolling/21627295#21627295
    // rejected
    //   https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API
    lastent.getBoundingClientRect()!.bottom >
    logdiv.getBoundingClientRect()!.bottom;

  console.log('ADD LOG MESSAGE ',in_scrollback, msg_html);
  var nelem = document.createElement('div');
  nelem.innerHTML = msg_html;
  logdiv.appendChild(nelem);

  if (!in_scrollback) {
    lastent = logdiv.lastElementChild!;
    lastent.scrollIntoView();
  }
}

// ----- test counter, startup -----

messages.Piece = <MessageHandler>function
(j: { piece: PieceId, op: Object }) {
  console.log('PIECE UPDATE ',j)
  var piece = j.piece;
  var m = j.op as { [k: string]: Object };
  var k = Object.keys(m)[0];
  pieceops[k](piece, m[k]);
};

pieceops.Modify = <PieceHandler>function
(piece: PieceId, info: { svg: string, held: PlayerId, pos: Pos }) {
  console.log('PIECE UPDATE MODIFY ',piece,info)
  var uelem = piece_element('use',piece)!;
  var delem = piece_element('defs',piece)!;
  var pelem = piece_element('piece',piece)!;
  delem.innerHTML = info.svg;
  uelem.setAttributeNS(null, "x", info.pos[0]+"");
  uelem.setAttributeNS(null, "y", info.pos[1]+"");
  uelem_checkconflict(piece, uelem);
  if (info.held == null) {
    set_ungrab(uelem, pelem);
  } else {
    set_grab(uelem, pelem, piece, info.held);
  }
  console.log('MODIFY DONE');
}

pieceops.Move = <PieceHandler>function
(piece, info: Pos ) {
  var uelem = piece_element('use',piece)!;
  uelem_checkconflict(piece, uelem);
  uelem.setAttributeNS(null, "x", info[0]+"");
  uelem.setAttributeNS(null, "y", info[1]+"");
}

messages.Recorded = <MessageHandler>function
(j: { piece: PieceId, cseq: ClientSeq, gen: Generation } ) {
  var uelem = document.getElementById('use'+j.piece)!;
  if (uelem.dataset.cseq != null && j.cseq >= +uelem.dataset.cseq) {
    delete uelem.dataset.cseq;
  }
  gen = j.gen;
}

function uelem_checkconflict(piece: PieceId, uelem: SVGGraphicsElement) {
  if (uelem.dataset.cseq == null) { return; }
  delete uelem.dataset.cseq;
  add_log_message('Conflict! - simultaneous update');
}

function startup() {
  var body = document.getElementById("main-body")!;
  ctoken = body.dataset.ctoken!;
  us = body.dataset.us!;
  gen = +body.dataset.gen!;
  status_node = document.getElementById('status')!;
  status_node.innerHTML = 'js-done';
  space = document.getElementById('space') as unknown as SVGGraphicsElement;
  logdiv = document.getElementById("log")!;
  svg_ns = space.getAttribute('xmlns')!;

  var es = new EventSource("/_/updates/"+ctoken+'/'+gen);
  es.onmessage = function(event) {
    console.log('GOTEVE', event)
    var j = JSON.parse(event.data);
    var tgen = j.pop();
    for (var m of j) {
      var k = Object.keys(m)[0];
      messages[k](m[k]);
    }
    gen = tgen;
  }
  es.addEventListener('commsworking', function(event) {
    console.log('GOTDATA', event);
    status_node.innerHTML = (event as any).data;
  });
  es.onerror = function(e) {
    console.log('FOO',e,es);
    json_report_error({
      updates_error : e,
      updates_event_source : es,
      updates_event_source_ready : es.readyState,
      update_oe : (e as any).className,
    })
  }
}

function doload(){
  console.log('DOLOAD');
  var elem = document.getElementById('loading_token')!;
  var ptoken = elem.dataset.ptoken;
  xhr_post_then('/_/session', 
		JSON.stringify({ ptoken : ptoken }),
		loaded);
}

function loaded(xhr: XMLHttpRequest){
  console.log('LOADED');
  var body = document.getElementById('loading_body')!;
  body.outerHTML = xhr.response;
  startup();
}

doload();
