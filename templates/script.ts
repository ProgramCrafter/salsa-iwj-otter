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
//      contains 1 or 3 subelements:
//      one is straight from server and not modified
//      one is possible <use href="#select{}" >
//      one is possible <use href="#halo{}" >
//
//   #select{}
//      generated by server, referenced by JS in pelem for selection
//
//   #def.{}.stuff
//      generated by server, reserved for Piece trait impl

type PieceId = string;
type PlayerId = string;
type Pos = [number, number];
type ClientSeq = number;
type Generation = number;

type PieceInfo = {
  held : PlayerId | null,
  cseq : number | null,
  z : number,
  zg : Generation,
  uelem : SVGGraphicsElement,
  delem : SVGGraphicsElement,
  pelem : SVGGraphicsElement,
}

/*
interface PieceMap {
  [piece: string]: PieceInfo;
}
var pieces : PieceMap = new Map();
*/

let pieces : { [typeid: string]: PieceInfo } = Object();
    //Object.create(null);

type MessageHandler = (op: Object) => void;
type PieceHandler = (piece: PieceId, p: PieceInfo, info: Object) => void;
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
var pieces_marker : SVGGraphicsElement;
var defs_marker : SVGGraphicsElement;
var logdiv : HTMLElement;
var status_node : HTMLElement;
var halo : PieceId | null;

type PlayerInfo = {
  dasharray : string,
}
var players : { [player: string]: PlayerInfo };

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
		   meth: string,
		   piece: PieceId, p: PieceInfo,
		   op: Object) {
  cseq += 1;
  p.cseq = cseq;
  if (halo == piece) {
    piece_undisplay_ancillary(p, "#halo"+halo);
    halo = null;
  }
  f(meth, {
    ctoken : ctoken,
    piece : piece,
    p : p,
    gen : gen,
    cseq : cseq,
    op : op,
  })
}

function svg_element(id: string): SVGGraphicsElement | null {
  let elem = document.getElementById(id);
  return elem as unknown as (SVGGraphicsElement | null);
}
function piece_element(base: string, piece: PieceId): SVGGraphicsElement | null
{
  return svg_element(base+piece);
}


// ----- clicking/dragging pieces -----

enum DRAGGING { // bitmask
  NO           = 0,
  MAYBE_GRAB   = 1,
  MAYBE_UNGRAB = 2,
  YES          = 4,
  RAISED       = 8,
};

var drag_piece : PieceId | null;
var dragging = DRAGGING.NO;

var dox : number | null;
var doy : number | null;
var dcx : number | null;
var dcy : number | null;

const DRAGTHRESH = 5;

function drag_mousedown(e : MouseEvent) {
  console.log('mousedown', e);
  var target = e.target as SVGGraphicsElement; // we check this just now!
  var piece = target.dataset.piece!;
  if (!piece) { return; }
  drag_cancel();
  let p = pieces[piece]!;
  drag_piece = piece;
  var held = p.held;
  if (held != null && held != us) {
    add_log_message('That piece is held by another player.');
    return;
  }

  dcx = e.clientX;
  dcy = e.clientY;
  dox = parseFloat(p.uelem.getAttributeNS(null,"x")!);
  doy = parseFloat(p.uelem.getAttributeNS(null,"y")!);

  if (held == us) {
    dragging = DRAGGING.MAYBE_UNGRAB;
  } else {
    dragging = DRAGGING.MAYBE_GRAB;
    set_grab(piece,p, us);
    api_piece(api, 'grab', piece,p, { });
  }

  window.addEventListener('mousemove', drag_mousemove, true);
  window.addEventListener('mouseup',   drag_mouseup,   true);
}

function set_grab(piece: PieceId, p: PieceInfo, owner: PlayerId) {
  p.held = owner;
  display_grab(piece,p);
}
function display_grab(piece: PieceId, p: PieceInfo) {
  piece_undisplay_grab(piece, p);
  var nelem = document.createElementNS(svg_ns,'use');
  nelem.setAttributeNS(null,'href','#select'+piece);
  let da = players[p.held!]!.dasharray;
  nelem.setAttributeNS(null,'stroke-dasharray',da);
  p.pelem.appendChild(nelem);
}
function set_ungrab(piece: PieceId, p: PieceInfo) {
  p.held = null;
  piece_undisplay_grab(piece,p);
}
function piece_undisplay_grab(piece: PieceId, p: PieceInfo) {
  piece_undisplay_ancillary(p, "#select"+piece);
}
function piece_undisplay_ancillary(p: PieceInfo, href: string) {
  for (let celem = p.pelem.firstElementChild;
       celem != null;
       celem = celem.nextElementSibling) {
    let thref = celem.getAttributeNS(null,"href");
    console.log('UNDISPLAY ANCILLARY',href,thref);
    if (thref == href) {
      celem.remove();
      return;
    }
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
    let piece = drag_piece!;
    let p = pieces[piece]!;
    var x = Math.round(dox! + ddx);
    var y = Math.round(doy! + ddy);
    p.uelem.setAttributeNS(null, "x", x+"");
    p.uelem.setAttributeNS(null, "y", y+"");
    api_piece(api_delay, 'm', piece,p, [x, y] );

    if (!(dragging & DRAGGING.RAISED)) {
      let dragraise = +p.pelem.dataset.dragraise!;
      if (dragraise > 0 && ddr2 >= dragraise*dragraise) {
	dragging |= DRAGGING.RAISED;
	console.log('CHECK RAISE ', dragraise, dragraise*dragraise, ddr2);
	piece_set_zlevel(piece,p, (oldtop_piece) => {
	  let oldtop_p = pieces[oldtop_piece]!;
	  let z = oldtop_p.z + 1;
	  p.z = z;
	  api_piece(api, "setz", piece,p, { z: z });
	});
      }
    }
  }
  return ddr2;
}

function drag_mouseup(e: MouseEvent) {
  console.log('mouseup', dragging);
  let ddr2 : number = drag_mousemove(e);
  let piece = drag_piece!;
  let p = pieces[piece]!;
  if (dragging == DRAGGING.MAYBE_UNGRAB ||
      (dragging & ~DRAGGING.RAISED) == (DRAGGING.MAYBE_GRAB | DRAGGING.YES)) {
    set_ungrab(piece,p);
    api_piece(api, 'ungrab', piece,p, { });
  }
  drag_cancel();
}

function drag_cancel() {
  window.removeEventListener('mousemove', drag_mousemove, true);
  window.removeEventListener('mouseup',   drag_mouseup,   true);
  dragging = DRAGGING.NO;
  drag_piece = null;
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
  let p = pieces[piece]!;
  pieceops[k](piece,p, m[k]);
};

pieceops.Modify = <PieceHandler>function
(piece: PieceId, p: PieceInfo,
 info: { svg: string, held: PlayerId, pos: Pos, z: number, zg: Generation}) {
  console.log('PIECE UPDATE MODIFY ',piece,info)
  p.delem.innerHTML = info.svg;
  p.pelem= piece_element('piece',piece)!;
  p.uelem.setAttributeNS(null, "x", info.pos[0]+"");
  p.uelem.setAttributeNS(null, "y", info.pos[1]+"");
  if (halo == piece) { halo = null }
  piece_checkconflict(piece,p);
  if (info.held == null) {
    set_ungrab(piece,p);
  } else {
    set_grab(piece,p, info.held);
  }
  // xxx handle z
  console.log('MODIFY DONE');
}

function piece_set_zlevel(piece: PieceId, p: PieceInfo,
			  modify : (oldtop_piece: PieceId) => void) {
  // Calls modify, which should set .z and/or .gz, and/or
  // make any necessary API call.
  //
  // Then moves uelem to the right place in the DOM.  This is done
  // by assuming that uelem ought to go at the end, so this is
  // O(new depth), which is right (since the UI for inserting
  // an object is itself O(new depth) UI operations to prepare.

  let oldtop_elem = (defs_marker.previousElementSibling! as
		     unknown as SVGGraphicsElement);
  let oldtop_piece = oldtop_elem.dataset.piece!;
  modify(oldtop_piece);

  let ins_before = defs_marker
  let earlier_elem;
  for (; ; ins_before = earlier_elem) {
    earlier_elem = (ins_before.previousElementSibling! as
		   unknown as SVGGraphicsElement);
    if (earlier_elem == pieces_marker) break;
    if (earlier_elem == p.uelem) continue;
    let earlier_p = pieces[earlier_elem.dataset.piece!]!;
    if (!piece_z_before(p, earlier_p)) break;
  }
  if (ins_before != p.uelem)
    space.insertBefore(p.uelem, ins_before);
}

function piece_z_before(a: PieceInfo, b: PieceInfo) {
  if (a.z  < b.z ) return true;
  if (a.z  > b.z ) return false;
  if (a.zg < b.zg) return true;
  if (a.zg > b.zg) return false;
  return false;
}

pieceops.Move = <PieceHandler>function
(piece,p, info: Pos ) {
  piece_checkconflict(piece,p);
  p.uelem.setAttributeNS(null, "x", info[0]+"");
  p.uelem.setAttributeNS(null, "y", info[1]+"");
}

pieceops.SetZLevel = <PieceHandler>function
(piece,p, info: { z: number, zg: Generation }) {
  let uelem = piece_element('use',piece)!;
  piece_set_zlevel(piece,p, (oldtop_piece)=>{
    let oldtop_p = pieces[oldtop_piece]!;
    p.z  = info.z;
    p.zg = info.zg;
  });
}

messages.Recorded = <MessageHandler>function
(j: { piece: PieceId, cseq: ClientSeq, gen: Generation,
      zg: Generation|null } ) {
  let piece = j.piece;
  let p = pieces[piece]!;
  if (p.cseq != null && j.cseq >= p.cseq) {
    p.cseq == null;
  }
  if (j.zg != null) {
    var zg_new = j.zg; // type narrowing doesn't propagate :-/
    piece_set_zlevel(piece,p, (oldtop_piece: PieceId)=>{
      p.zg = zg_new;
    });
  }
  gen = j.gen;
}

function piece_checkconflict(piece: PieceId, p: PieceInfo) {
  if (halo != piece) {
    if (halo != null) {
      console.log('UNHALOING',halo);
      piece_undisplay_ancillary(pieces[halo]!, "#halo"+halo);
    }
    console.log('HALOING',piece);
    halo = piece;
    var nelem = document.createElementNS(svg_ns,'use');
    nelem.setAttributeNS(null,'href','#halo'+piece);
    p.pelem.prepend(nelem);
  }

  if (p.cseq == null) { return; }
  p.cseq = null;
  add_log_message('Conflict! - simultaneous update');
}

function test_swap_stack() {
  let old_bot = pieces_marker.nextElementSibling!;
  let container = old_bot.parentElement!;
  container.insertBefore(old_bot, defs_marker);
  window.setTimeout(test_swap_stack, 1000);
}

function startup() {
  var body = document.getElementById("main-body")!;
  ctoken = body.dataset.ctoken!;
  us = body.dataset.us!;
  gen = +body.dataset.gen!;
  status_node = document.getElementById('status')!;
  status_node.innerHTML = 'js-done';
  logdiv = document.getElementById("log")!;
  let dataload = JSON.parse(body.dataset.load!);
  players = dataload.players!;
  delete body.dataset.load;

  space = svg_element('space')!;
  pieces_marker = svg_element("pieces_marker")!;
  defs_marker = svg_element("defs_marker")!;
  svg_ns = space.getAttribute('xmlns')!;

  for (let uelem = pieces_marker.nextElementSibling! as SVGGraphicsElement;
       uelem != defs_marker;
       uelem = uelem.nextElementSibling! as SVGGraphicsElement) {
    let piece = uelem.dataset.piece!;
    let p = JSON.parse(uelem.dataset.info!);
    p.uelem = uelem;
    p.delem = piece_element('defs',piece);
    p.pelem = piece_element('piece',piece);
    delete uelem.dataset.info;
    pieces[piece] = p;
    if (p.held != null) display_grab(piece,p);
  }

  var es = new EventSource("/_/updates/"+ctoken+'/'+gen);
  es.onmessage = function(event) {
    console.log('GOTEVE', event)
    var [tgen, ms] = JSON.parse(event.data);
    for (var m of ms) {
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

//  test_swap_stack();
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
