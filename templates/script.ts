// -*- JavaScript -*-

// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY. -->

// xxx deployment note: need a whole bunch of domains for SSE conn limit

// xxx some way to choose faces / rotate

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
type UoKind = "Global"| "Piece" | "ClientExtra" | "GlobalExtra";

type UoDescription = {
  kind: UoKind;
  def_key: string,
  opname: string,
  desc: string,
}

type UoRecord = UoDescription & {
  targets: PieceId[] | null,
}

type PieceInfo = {
  held : PlayerId | null,
  cseq : number | null,
  z : number,
  zg : Generation,
  uos : UoDescription[],
  uelem : SVGGraphicsElement,
  delem : SVGGraphicsElement,
  pelem : SVGGraphicsElement,
  queued_moves : number,
  last_seen_moved : DOMHighResTimeStamp | null, // non-0 means halo'd
}

let pieces : { [piece: string]: PieceInfo } = Object.create(null);

type MessageHandler = (op: Object) => void;
type PieceHandler = (piece: PieceId, p: PieceInfo, info: Object) => void;
type PieceErrorHandler = (piece: PieceId, p: PieceInfo, m: PieceOpError)
  => boolean;
interface DispatchTable<H> { [key: string]: H };

var general_timeout : number = 10000;
var messages : DispatchTable<MessageHandler> = Object();
var pieceops : DispatchTable<PieceHandler> = Object();
var update_error_handlers : DispatchTable<MessageHandler> = Object();
var piece_error_handlers : DispatchTable<PieceErrorHandler> = Object();
var our_dnd_type = "text/puvnex-game-server-dummy";
var api_queue : [string, Object][] = [];
var api_posting = false;
var us : string;
var gen = 0;
var cseq : ClientSeq = 0;
var ctoken : string;
var uo_map : { [k: string]: UoRecord | null } = Object.create(null);

var svg_ns : string;
var space : SVGGraphicsElement;
var pieces_marker : SVGGraphicsElement;
var defs_marker : SVGGraphicsElement;
var logdiv : HTMLElement;
var status_node : HTMLElement;
var uos_node : HTMLElement;

const uo_kind_prec : { [kind: string]: number } = {
  'GlobalExtra' :  50,
  'Global'      : 100,
  'Piece'       : 200,
  'ClientExtra' : 500,
}

type PlayerInfo = {
  dasharray : string,
}
var players : { [player: string]: PlayerInfo };

type MovementRecord = {
  piece: PieceId,
  p: PieceInfo,
  this_motion: DOMHighResTimeStamp,
}
var movements : MovementRecord[] = [];

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
  string_report_error(error_message);
}

function string_report_error(error_message: String) {
  let errornode = document.getElementById('error')!;
  errornode.textContent += '\nError (reloading may help?):' + error_message;
}

function api(meth: string, data: Object) {
  api_queue.push([meth, data]);
  api_check();
}
function api_delay(meth: string, data: Object) {
  if (api_queue.length==0) window.setTimeout(api_check, 10);
  api_queue.push([meth, data]);
}
function api_check() {
  if (api_posting) { return; }
  if (!api_queue.length) { return; }
  do {
    var [meth, data] = api_queue.shift()!;
    if (meth != 'm') break;
    let piece = (data as any).piece;
    let p = pieces[piece];
    if (p == null) break;
    p.queued_moves--;
    if (p.queued_moves == 0) break;
  } while(1);
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
  clear_halo(piece,p);
  cseq += 1;
  p.cseq = cseq;
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

type DragInfo = {
  piece : PieceId,
  dox : number,
  doy : number,
}

enum DRAGGING { // bitmask
  NO           = 0,
  MAYBE_GRAB   = 1,
  MAYBE_UNGRAB = 2,
  YES          = 4,
  RAISED       = 8,
};

var drag_pieces : DragInfo[] = [];
var dragging = DRAGGING.NO;
var dcx : number | null;
var dcy : number | null;

const DRAGTHRESH = 5;

function drag_add_piece(piece: PieceId, p: PieceInfo) {
  drag_pieces.push({
    piece: piece,
    dox: parseFloat(p.uelem.getAttributeNS(null,"x")!),
    doy: parseFloat(p.uelem.getAttributeNS(null,"y")!),
  });
}

function some_mousedown(e : MouseEvent) {
  console.log('mousedown', e);

  if (e.button != 0) { return }
  if (e.altKey) { return }
  if (e.metaKey) { return }
  if (e.shiftKey) {
    if (e.ctrlKey) {
      return;
    } else {
      // group select
    }
  } else {
    if (e.ctrlKey) {
      // region indication
    } else {
      drag_mousedown(e);
    }
  }
}

function drag_mousedown(e : MouseEvent) {
  var target = e.target as SVGGraphicsElement; // we check this just now!
  var piece = target.dataset.piece!;
  if (!piece) { return; }
  drag_cancel();

  let p = pieces[piece]!;
  let held = p.held;

  drag_pieces = [];
  if (held == null) {
    dragging = DRAGGING.MAYBE_GRAB;
    drag_add_piece(piece,p);
    set_grab(piece,p, us);
    api_piece(api, 'grab', piece,p, { });
  } else if (held == us) {
    dragging = DRAGGING.MAYBE_UNGRAB;
    drag_add_piece(piece,p); // contrive to have this one first
    for (let tpiece of Object.keys(pieces)) {
      if (tpiece == piece) continue;
      let tp = pieces[tpiece]!;
      if (tp.held != us) continue;
      drag_add_piece(tpiece,tp);
    }
  } else {
    add_log_message('That piece is held by another player.');
    return;
  }
  dcx = e.clientX;
  dcy = e.clientY;

  window.addEventListener('mousemove', drag_mousemove, true);
  window.addEventListener('mouseup',   drag_mouseup,   true);
}

function set_grab(piece: PieceId, p: PieceInfo, owner: PlayerId) {
  p.held = owner;
  redisplay_ancillaries(piece,p);
  recompute_keybindings();
}
function set_ungrab(piece: PieceId, p: PieceInfo) {
  p.held = null;
  redisplay_ancillaries(piece,p);
  recompute_keybindings();
}

function clear_halo(piece: PieceId, p: PieceInfo) {
  p.last_seen_moved = null;
  redisplay_ancillaries(piece,p);
}

function ancillary_node(piece: PieceId, stroke: string): SVGGraphicsElement {
  var nelem = document.createElementNS(svg_ns,'use');
  nelem.setAttributeNS(null,'href','#surround'+piece);
  nelem.setAttributeNS(null,'stroke',stroke);
  nelem.setAttributeNS(null,'fill','none');
  return nelem as any;
}

function redisplay_ancillaries(piece: PieceId, p: PieceInfo) {
  let href = '#surround'+piece;
  console.log('REDISPLAY ANCILLARIES',href);

  for (let celem = p.pelem.firstElementChild;
       celem != null;
       celem = nextelem) {
    var nextelem = celem.nextElementSibling
    let thref = celem.getAttributeNS(null,"href");
    if (thref == href) {
      celem.remove();
    }
  }

  if (p.last_seen_moved != null) {
    let nelem = ancillary_node(piece, 'yellow');
    if (p.held != null) {
      nelem.setAttributeNS(null,'stroke-width','2px');
    }
    p.pelem.prepend(nelem);
  }
  if (p.held != null) {
    let da = players[p.held!]!.dasharray;
    let nelem = ancillary_node(piece, 'black');
    nelem.setAttributeNS(null,'stroke-dasharray',da);
    p.pelem.appendChild(nelem);
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
    console.log('DRAG PIECES',drag_pieces);
    for (let dp of drag_pieces) {
      console.log('DRAG PIECES PIECE',dp);
      let tpiece = dp.piece;
      let tp = pieces[tpiece]!;
      var x = Math.round(dp.dox + ddx);
      var y = Math.round(dp.doy + ddy);
      tp.uelem.setAttributeNS(null, "x", x+"");
      tp.uelem.setAttributeNS(null, "y", y+"");
      tp.queued_moves++;
      api_piece(api_delay, 'm', tpiece,tp, [x, y] );
    }
    if (!(dragging & DRAGGING.RAISED) && drag_pieces.length==1) {
      let dp = drag_pieces[0];
      let piece = dp.piece;
      let p = pieces[piece]!;
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
  drag_end();
}

function drag_end() {
  if (dragging == DRAGGING.MAYBE_UNGRAB ||
      (dragging & ~DRAGGING.RAISED) == (DRAGGING.MAYBE_GRAB | DRAGGING.YES)) {
    let dp = drag_pieces[0]!;
    let piece = dp.piece;
    let p = pieces[piece]!;
    set_ungrab(piece,p);
    api_piece(api, 'ungrab', piece,p, { });
  }
  drag_cancel();
}

function drag_cancel() {
  window.removeEventListener('mousemove', drag_mousemove, true);
  window.removeEventListener('mouseup',   drag_mouseup,   true);
  dragging = DRAGGING.NO;
  drag_pieces = [];
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

type PieceStateMessage = {
  svg: string,
  held: PlayerId,
  pos: Pos,
  z: number,
  zg: Generation,
  uos: UoDescription[],
}

pieceops.Modify = <PieceHandler>function
(piece: PieceId, p: PieceInfo, info: PieceStateMessage) {
  console.log('PIECE UPDATE MODIFY ',piece,info)
  piece_modify(piece, p, info, false);
}

piece_error_handlers.PosOffTable = <PieceErrorHandler>function()
{ return true ; }
piece_error_handlers.Conflict = <PieceErrorHandler>function()
{ return true ; }

function piece_modify(piece: PieceId, p: PieceInfo, info: PieceStateMessage,
		      conflict_expected: boolean) {
  p.delem.innerHTML = info.svg;
  p.pelem= piece_element('piece',piece)!;
  p.uelem.setAttributeNS(null, "x", info.pos[0]+"");
  p.uelem.setAttributeNS(null, "y", info.pos[1]+"");
  p.held = info.held;
  piece_set_zlevel(piece,p, (oldtop_piece)=>{
    p.z  = info.z;
    p.zg = info.zg;
    p.uos = info.uos;
  });
  piece_checkconflict_nrda(piece,p,conflict_expected);
  redisplay_ancillaries(piece,p);
  recompute_keybindings();
  console.log('MODIFY DONE');
}

/*
pieceops.Insert = <PieceHandler>function
(piece: PieceId, p: null,
 info: { svg: string, held: PlayerId, pos: Pos, z: number, zg: Generation}) {
  console.log('PIECE UPDATE INSERT ',piece,info)
  delem = document.createElementNS(svg_ns,'defs');
  delem.setAttributeNS(null,'id','defs'+piece);
  delem.innerHTML = info.svg;
  space.appendChild(delem);
  pelem = 

  nelem.setAttributeNS(null,'stroke',stroke);
  nelem.setAttributeNS(null,'fill','none');
*/

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
  piece_checkconflict_nrda(piece,p,false);
  let now = performance.now();

  let need_redisplay = p.last_seen_moved == null;
  p.last_seen_moved = now;
  if (need_redisplay) redisplay_ancillaries(piece,p);

  let cutoff = now-1000.;
  while (movements.length > 0 && movements[0].this_motion < cutoff) {
    let mr = movements.shift()!;
    if (mr.p.last_seen_moved != null &&
	mr.p.last_seen_moved < cutoff) {
      mr.p.last_seen_moved = null;
      redisplay_ancillaries(mr.piece,mr.p);
    }
  }
  movements.push({ piece: piece, p: p, this_motion: now });

  p.uelem.setAttributeNS(null, "x", info[0]+"");
  p.uelem.setAttributeNS(null, "y", info[1]+"");
}

pieceops.SetZLevel = <PieceHandler>function
(piece,p, info: { z: number, zg: Generation }) {
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
    p.cseq = null;
  }
  if (j.zg != null) {
    var zg_new = j.zg; // type narrowing doesn't propagate :-/
    piece_set_zlevel(piece,p, (oldtop_piece: PieceId)=>{
      p.zg = zg_new;
    });
  }
  gen = j.gen;
}

messages.Error = <MessageHandler>function
(m: any) {
  console.log('ERROR UPDATE ', m);
  var k = Object.keys(m)[0];
  update_error_handlers[k](m[k]);
}

type PieceOpError = {
  piece: PieceId,
  error: string,
  state: PieceStateMessage,
};

update_error_handlers.PieceOpError = <MessageHandler>function
(m: PieceOpError) {
  let p = pieces[m.piece];
  if (p == null) return;
  let conflict_expected = piece_error_handlers[m.error](m.piece, p, m);
  piece_modify(m.piece, p, m.state, conflict_expected);
}

function piece_checkconflict_nrda(piece: PieceId, p: PieceInfo,
				  conflict_expected: boolean): boolean {
  if (p.cseq != null) {
    p.cseq = null;
    if (drag_pieces.some(function(dp) { return dp.piece == piece; })) {
      console.log('drag end due to conflict');
      drag_end();
    }
    if (!conflict_expected) {
      add_log_message('Conflict! - simultaneous update');
    }
  }
  return false;
}

function test_swap_stack() {
  let old_bot = pieces_marker.nextElementSibling!;
  let container = old_bot.parentElement!;
  container.insertBefore(old_bot, defs_marker);
  window.setTimeout(test_swap_stack, 1000);
}

function recompute_keybindings() {
  uo_map = Object.create(null);
  for (let piece of Object.keys(pieces)) {
    let p = pieces[piece];
    if (p.held != us) continue;
    for (var uo of p.uos) {
      let currently = uo_map[uo.def_key];
      if (currently === null) continue;
      if (currently !== undefined) {
	if (currently.opname != uo.opname) {
	  uo_map[uo.def_key] = null;
	  continue;
	}
      } else {
	currently = {
	  targets: [],
	  ...uo
	};
	uo_map[uo.def_key] = currently;
      }
      currently.desc = currently.desc < uo.desc ? currently.desc : uo.desc;
      currently.targets!.push(piece);
    }
  }
  uo_map['W'] = {
    kind: 'ClientExtra',
    def_key: 'W',
    opname: 'wrest',
    desc: 'Enter wresting mode',
    targets: null,
  }
  var uo_keys = Object.keys(uo_map);
  uo_keys.sort(function (ak,bk) {
    let a = uo_map[ak]!;
    let b = uo_map[bk]!;
    return uo_kind_prec[a.kind] - uo_kind_prec[b.kind]
      || ak.localeCompare(bk);
  });
  let out = document.createElement('div');
  out.setAttribute('class','uokeys');
  for (var kk of uo_keys) {
    let uo = uo_map[kk]!;
    let ent = document.createElement('div');
    ent.setAttribute('class','uokey-'+uo.kind);
    ent.innerHTML = '<b>' + kk + '</b> ' + uo.desc;
    out.appendChild(ent);
  }
  uos_node.firstChild!.replaceWith(out);
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
  uos_node = document.getElementById("uos")!;

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
    p.queued_moves = 0;
    delete uelem.dataset.info;
    pieces[piece] = p;
    redisplay_ancillaries(piece,p);
  }

  var es = new EventSource("/_/updates/"+ctoken+'/'+gen);
  es.onmessage = function(event) {
    console.log('GOTEVE', event);
    var k;
    var m;
    try {
      var [tgen, ms] = JSON.parse(event.data);
      for (m of ms) {
	k = Object.keys(m)[0];
	messages[k](m[k]);
      }
      gen = tgen;
    } catch (exc) {
      var s = exc.toString();
      string_report_error('exception handling update '
			  + k + ': ' + JSON.stringify(m) + ': ' + s);
    }
  }
  es.addEventListener('commsworking', function(event) {
    console.log('GOTDATA', event);
    status_node.innerHTML = (event as any).data;
  });
  es.addEventListener('updates_expired', function(event) {
    console.log('UPDATES-EXPIRED', event);
    string_report_error('connection to server interrupted too long');
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
  recompute_keybindings();

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

// xxx scroll of log messages to bottom does not always work somehow

doload();
