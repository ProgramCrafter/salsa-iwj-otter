// -*- JavaScript -*-

// Copyright 2020 Ian Jackson
// SPDX-License-Identifier: AGPL-3.0-or-later
// There is NO WARRANTY. -->

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

// xxx Error (reloading may help?):exception handling update Piece: {"Piece":{"piece":"10.1","op":{"Modify":{"pos":[151,40],"svg":"<g id=\"piece10.1\" data-dragraise=\"4\"><g transform=\"scale(0.2 0.2) translate(-22.5 -22.5)\"><!-- otter.git#3413043bf64abcedc4332b192dd207882a0f3abf+ library/wikimedia/chess-b-R.usvg CC-BY-SA-4.0, see library/wikimedia/LICENCE -->\n<svg width=\"45\" height=\"45\" viewBox=\"0 0 45 45\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:usvg=\"https://github.com/RazrFalcon/resvg\" usvg:version=\"0.11.0\">\n    <defs/>\n    <path fill=\"#000000\" fill-rule=\"evenodd\" stroke=\"#000000\" stroke-width=\"1.5\" stroke-linejoin=\"round\" d=\"M 9 39 L 36 39 L 36 36 L 9 36 L 9 39 Z\"/>\n    <path fill=\"#000000\" fill-rule=\"evenodd\" stroke=\"#000000\" stroke-width=\"1.5\" stroke-linejoin=\"round\" d=\"M 12.5 32 L 14 29.5 L 31 29.5 L 32.5 32 L 12.5 32 Z\"/>\n    <path fill=\"#000000\" fill-rule=\"evenodd\" stroke=\"#000000\" stroke-width=\"1.5\" stroke-linejoin=\"round\" d=\"M 12 36 L 12 32 L 33 32 L 33 36 L 12 36 Z\"/>\n    <path fill=\"#000000\" fill-rule=\"evenodd\" stroke=\"#000000\" stroke-width=\"1.5\" d=\"M 14 29.5 L 14 16.5 L 31 16.5 L 31 29.5 L 14 29.5 Z\"/>\n    <path fill=\"#000000\" fill-rule=\"evenodd\" stroke=\"#000000\" stroke-width=\"1.5\" stroke-linejoin=\"round\" d=\"M 14 16.5 L 11 14 L 34 14 L 31 16.5 L 14 16.5 Z\"/>\n    <path fill=\"#000000\" fill-rule=\"evenodd\" stroke=\"#000000\" stroke-width=\"1.5\" stroke-linejoin=\"round\" d=\"M 11 14 L 11 9 L 15 9 L 15 11 L 20 11 L 20 9 L 25 9 L 25 11 L 30 11 L 30 9 L 34 9 L 34 14 L 11 14 Z\"/>\n    <path fill=\"none\" stroke=\"#ffffff\" stroke-linecap=\"round\" d=\"M 12 35.5 L 33 35.5 L 33 35.5\"/>\n    <path fill=\"none\" stroke=\"#ffffff\" stroke-linecap=\"round\" d=\"M 13 31.5 L 32 31.5\"/>\n    <path fill=\"none\" stroke=\"#ffffff\" stroke-linecap=\"round\" d=\"M 14 29.5 L 31 29.5\"/>\n    <path fill=\"none\" stroke=\"#ffffff\" stroke-linecap=\"round\" d=\"M 14 16.5 L 31 16.5\"/>\n    <path fill=\"none\" stroke=\"#ffffff\" stroke-linecap=\"round\" d=\"M 11 14 L 34 14\"/>\n</svg>\n</g></g><path id=\"surround10.1\" d=\"M 0 4.95 a 4.95 4.95 0 1 0 0 -9.9 a 4.95 4.95 0 1 0 0 9.9 z\"/>","held":"2#1","z":"g04r000000","zg":60,"pinned":false,"uos":[]}}}}: TypeError: players[p.held] is undefined

type PieceId = string;
type PlayerId = string;
type Pos = [number, number];
type ClientSeq = number;
type Generation = number;
type UoKind = 'Client' | "Global"| "Piece" | "ClientExtra" | "GlobalExtra";
type WhatResponseToClientOp = "Predictable" | "Unpredictable" | "UpdateSvg";
type Timestamp = number; // unix time_t, will break in 285My
type Layout = 'Portrait' | 'Landscape';

type UoDescription = {
  kind: UoKind;
  wrc: WhatResponseToClientOp,
  def_key: string,
  opname: string,
  desc: string,
}

type UoRecord = UoDescription & {
  targets: PieceId[] | null,
}

type ZCoord = string;

type PieceInfo = {
  held : PlayerId | null,
  cseq : number | null,
  cseq_updatesvg : number | null,
  z : ZCoord,
  zg : Generation,
  pinned: boolean,
  uos : UoDescription[],
  uelem : SVGGraphicsElement,
  delem : SVGGraphicsElement,
  pelem : SVGGraphicsElement,
  queued_moves : number,
  last_seen_moved : DOMHighResTimeStamp | null, // non-0 means halo'd
}

let wasm : wasm_bindgen.InitOutput;

let pieces : { [piece: string]: PieceInfo } = Object.create(null);

type MessageHandler = (op: Object) => void;
type PieceHandler = (piece: PieceId, p: PieceInfo, info: Object) => void;
type PieceErrorHandler = (piece: PieceId, p: PieceInfo, m: PieceOpError)
  => boolean;
interface DispatchTable<H> { [key: string]: H };

var globalinfo_elem : HTMLElement;
var layout: Layout;
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
var keyops_local : { [opname: string]: (uo: UoRecord) => void } = Object();

var svg_ns : string;
var space : SVGGraphicsElement;
var pieces_marker : SVGGraphicsElement;
var defs_marker : SVGGraphicsElement;
var log_elem : HTMLElement;
var logscroll_elem : HTMLElement;
var status_node : HTMLElement;
var uos_node : HTMLElement;
var wresting: boolean;

const uo_kind_prec : { [kind: string]: number } = {
  'GlobalExtra' :  50,
  'Client'      :  70,
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
  // todo want to fix this for at least basic game reconfigs, auto-reload?
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

// ----- key handling -----

function recompute_keybindings() {
  uo_map = Object.create(null);
  let all_targets = [];
  for (let piece of Object.keys(pieces)) {
    let p = pieces[piece];
    if (p.held != us) continue;
    all_targets.push(piece);
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
  let add_uo = function(targets: PieceId[] | null, uo: UoDescription) {
    uo_map[uo.def_key] = {
      targets: targets,
      ...uo
    };
  };
  if (all_targets.length) {
    add_uo(all_targets, {
      def_key: 'l',
      kind: 'Client',
      wrc: 'Predictable',
      opname: "lower",
      desc: "lower (send to bottom)",
    });
  }
  if (all_targets.length) {
    let got = 0;
    for (let t of all_targets) {
      got |= 1 << Number(pieces[t]!.pinned);
    }
    if (got == 1) {
      add_uo(all_targets, {
	def_key: 'P',
	kind: 'ClientExtra',
	opname: 'pin',
	desc: 'Pin to table',
	wrc: 'Predictable',
      });
    } else if (got == 2) {
      add_uo(all_targets, {
	def_key: 'P',
	kind: 'ClientExtra',
	opname: 'unpin',
	desc: 'Unpin from table',
	wrc: 'Predictable',
      });
    }
  }
  add_uo(null, {
    def_key: 'W',
    kind: 'ClientExtra',
    opname: 'wrest',
    desc: wresting ? 'Exit wresting mode' : 'Enter wresting mode',
    wrc: 'Predictable',
  });
  var uo_keys = Object.keys(uo_map);
  uo_keys.sort(function (ak,bk) {
    let a = uo_map[ak]!;
    let b = uo_map[bk]!;
    return uo_kind_prec[a.kind] - uo_kind_prec[b.kind]
      || ak.localeCompare(bk);
  });
  let mid_elem = null;
  for (let celem = uos_node.firstElementChild;
       celem != null;
       celem = nextelem) {
    var nextelem = celem.nextElementSibling;
    let cid = celem.getAttribute("id");
    if (cid == "uos-mid") mid_elem = celem;
    else if (celem.getAttribute("class") == 'uos-mid') { }
    else celem.remove();
  }
  for (var kk of uo_keys) {
    let uo = uo_map[kk]!;
    let prec = uo_kind_prec[uo.kind];
    let ent = document.createElement('div');
    ent.innerHTML = '<b>' + kk + '</b> ' + uo.desc;
    if (prec < 400) {
      ent.setAttribute('class','uokey-l');
      uos_node.insertBefore(ent, mid_elem);
    } else {
      ent.setAttribute('class','uokey-r');
      uos_node.appendChild(ent);
    }
  }
}

function some_keydown(e: KeyboardEvent) {
  // https://developer.mozilla.org/en-US/docs/Web/API/Document/keydown_event
  // says to do this, something to do with CJK composition.
  // https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
  // says that keyCode is deprecated
  // my tsc says this isComposing thing doesn't exist.  wat.
  if ((e as any).isComposing /* || e.keyCode === 229 */) return;

  let uo = uo_map[e.key];
  if (uo === undefined || uo === null) return;

  console.log('KEY UO', e, uo);
  if (uo.kind == 'Client' || uo.kind == 'ClientExtra') {
    let f = keyops_local[uo.opname];
    f(uo);
    return;
  }
  if (!(uo.kind == 'Global' || uo.kind == 'GlobalExtra'))
    throw 'bad kind '+uo.kind;

  if (uo.wrc == 'UpdateSvg' || uo.wrc == 'Predictable') {
    for (var piece of uo.targets!) {
      let p = pieces[piece]!;
      api_piece(api, 'k', piece, p, { opname: uo.opname, wrc: uo.wrc });
      if (uo.wrc == 'UpdateSvg') {
	p.cseq_updatesvg = p.cseq;
	redisplay_ancillaries(piece,p);
      }
    }
  }
}

type LowerTodoItem = {
  piece: PieceId,
  p: PieceInfo,
  pinned: boolean,
};

type LowerTodoList = { [piece: string]: LowerTodoItem };

keyops_local['lower'] = function (uo: UoRecord) { lower_targets(uo); }

function lower_targets(uo: UoRecord): boolean {
   function target_treat_pinned(p: PieceInfo): boolean {
    return wresting || p.pinned;;
  }

  let targets_todo : LowerTodoList = Object.create(null);

  for (let piece of uo.targets!) {
    let p = pieces[piece]!;
    let pinned = target_treat_pinned(p);
    targets_todo[piece] = { p, piece, pinned, };
  }
  let problem = lower_pieces(targets_todo);
  if (problem !== null) {
    add_log_message('Cannot lower: ' + problem);
    return false;
  }
  return true;
}

function lower_pieces(targets_todo: LowerTodoList):
 string | null
{
  // This is a bit subtle.  We don't want to lower below pinned pieces
  // (unless we are pinned too, or the user is wresting).  But maybe
  // the pinned pieces aren't already at the bottom.  For now we will
  // declare that all pinned pieces "should" be below all non-pinned
  // ones.  Not as an invariant, but as a thing we will do here to try
  // to make a sensible result.  We implement this as follows: if we
  // find pinned pieces above non-pinned pieces, we move those pinned
  // pieces to the bottom too, just below us, preserving their
  // relative order.
  //
  // Disregarding pinned targets:
  //
  // Z     <some stuff not including any unpinned targets>
  // Z
  //       topmost unpinned target         *
  // B (
  // B     unpinned non-target
  // B |   unpinned target                 *
  // B |   pinned non-target, mis-stacked  *
  // B )*
  // B
  //       bottommost unpinned non-target
  //        if that is below topmost unpinned target
  //            <- tomove_unpinned: insert targets from * here        Q ->
  //            <- tomove_misstacked: insert non-targets from * here  Q->
  // A
  // A     pinned things (nomove_pinned)
  //            <- tomove_pinned: insert all pinned targets here      P ->
  //
  // When wresting, treat all targets as pinned.

  type Entry = {
    piece: PieceId,
    p: PieceInfo,
  };
  // bottom of the stack order first
  let tomove_unpinned     : Entry[] = [];
  let tomove_misstacked   : Entry[] = [];
  let nomove_pinned       : Entry[] = [];
  let tomove_pinned       : Entry[] = [];
  let bottommost_unpinned : Entry | null = null;

  let n_targets_todo_unpinned = 0;
  for (const piece of Object.keys(targets_todo)) {
    let p = targets_todo[piece];
    if (!p.pinned) n_targets_todo_unpinned++;
  }

  let walk = pieces_marker;
  for (;;) { // starting at the bottom of the stack order
    if (n_targets_todo_unpinned == 0
	&& bottommost_unpinned !== null) {
      // no unpinned targets left, state Z, we can stop now
      console.log('LOWER STATE Z FINISHED');
      break;
    }
    if (Object.keys(targets_todo).length == 0 &&
       bottommost_unpinned !== null) {
      console.log('LOWER NO TARGETS BUT UNPINNED!', n_targets_todo_unpinned);
      break;
    }

    let new_walk = walk.nextElementSibling;
    if (new_walk == null) {
      console.log('LOWER WALK NO SIBLING!');
      break;
    }
    walk = new_walk as SVGGraphicsElement;
    let piece = walk.dataset.piece;
    if (piece == null) {
      console.log('LOWER WALK REACHED TOP');
      break;
    }

    let todo = targets_todo[piece];
    if (todo) {
      console.log('LOWER WALK', piece, 'TODO', todo.pinned);
      delete targets_todo[piece];
      if (!todo.pinned) n_targets_todo_unpinned--;
      (todo.pinned ? tomove_pinned : tomove_unpinned).push(todo);
      continue;
    }

    let p = pieces[piece]!;
    if (bottommost_unpinned === null) { // state A
      if (!p.pinned) {
	console.log('LOWER WALK', piece, 'STATE A -> Z');
	bottommost_unpinned = { p, piece };
      } else {
	console.log('LOWER WALK', piece, 'STATE A');
	nomove_pinned.push({ p, piece });
      }
      continue;
    }

    // state B
    if (p.pinned) {
      console.log('LOWER WALK', piece, 'STATE B MIS-STACKED');
      tomove_misstacked.push({ p, piece });
    } else {
      console.log('LOWER WALK', piece, 'STATE B');
    }
  }

  let z_top =
      bottommost_unpinned ? bottommost_unpinned.p.z :
      walk.dataset.piece != null ? pieces[walk.dataset.piece!].z :
      // rather a lack of things we are not adjusting!
      wasm_bindgen.def_zcoord();

  type PlanEntry = {
    content: Entry[], // bottom to top
    z_top: ZCoord | null,
    z_bot: ZCoord | null,
  };

  let plan : PlanEntry[] = [];

  let partQ = tomove_unpinned.concat(tomove_misstacked);
  let partP = tomove_pinned;

  if (nomove_pinned.length == 0) {
    plan.push({
      content: partQ.concat(partP),
      z_top,
      z_bot : null,
    });
  } else {
    plan.push({
      content: partQ,
      z_top,
      z_bot: nomove_pinned[nomove_pinned.length-1].p.z,
    }, {
      content: partP,
      z_top: nomove_pinned[0].p.z,
      z_bot: null,
    });
  }

  console.log('LOWER PLAN', plan);

  for (const pe of plan) {
    for (const e of pe.content) {
      if (e.p.held != null && e.p.held != us) {
	return "lowering would disturb a piece held by another player";
      }
    }
  }

  z_top = null;
  for (const pe of plan) {
    if (pe.z_top != null) z_top = pe.z_top;
    let z_bot = pe.z_bot;
    let zrange = wasm_bindgen.range(z_bot, z_top, pe.content.length);
    console.log('LOQER PLAN PE',
		pe, z_bot, z_top, pe.content.length, zrange.debug());
    for (const e of pe.content) {
      let p = e.p;
      piece_set_zlevel(e.piece, p, (oldtop_piece) => {
	let z = zrange.next();
	p.z = z;
	api_piece(api, "setz", e.piece, e.p, { z });
      });
    }
  }
  return null;
}

keyops_local['wrest'] = function (uo: UoRecord) {
  wresting = !wresting;
  document.getElementById('wresting-warning')!.innerHTML = !wresting ? "" :
    " <strong>(wresting mode!)</strong>";
  ungrab_all();
  recompute_keybindings();
}

keyops_local['pin'  ] = function (uo) {
  if (!lower_targets(uo)) return;
  pin_unpin(uo, true);
}
keyops_local['unpin'] = function (uo) {
  pin_unpin(uo, false);
}

function pin_unpin(uo: UoRecord, newpin: boolean) {
  for (let piece of uo.targets!) {
    let p = pieces[piece]!;
    p.pinned = newpin;
    api_piece(api, 'pin', piece,p, newpin);
    redisplay_ancillaries(piece,p);
  }
  recompute_keybindings();
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
  if (e.ctrlKey) {
    return;
  } else {
    drag_mousedown(e, e.shiftKey);
  }
}

function drag_mousedown(e : MouseEvent, shifted: boolean) {
  var target = e.target as SVGGraphicsElement; // we check this just now!
  var piece = target.dataset.piece!;
  if (!piece) { return; }
  let p = pieces[piece]!;
  let held = p.held;

  drag_cancel();

  drag_pieces = [];
  if (held == us) {
    dragging = DRAGGING.MAYBE_UNGRAB;
    drag_add_piece(piece,p); // contrive to have this one first
    for (let tpiece of Object.keys(pieces)) {
      if (tpiece == piece) continue;
      let tp = pieces[tpiece]!;
      if (tp.held != us) continue;
      drag_add_piece(tpiece,tp);
    }
  } else if (held == null || wresting) {
    if (p.pinned && !wresting) {
      add_log_message('That piece is pinned to the table.');
      return;
    }
    if (!shifted) {
      ungrab_all();
    }
    dragging = DRAGGING.MAYBE_GRAB;
    drag_add_piece(piece,p);
    set_grab(piece,p, us);
    api_piece(api, wresting ? 'wrest' : 'grab', piece,p, { });
  } else {
    add_log_message('That piece is held by another player.');
    return;
  }
  dcx = e.clientX;
  dcy = e.clientY;

  window.addEventListener('mousemove', drag_mousemove, true);
  window.addEventListener('mouseup',   drag_mouseup,   true);
}

function ungrab_all() {
  for (let tpiece of Object.keys(pieces)) {
    let tp = pieces[tpiece]!;
    if (tp.held == us) {
	  set_ungrab(tpiece,tp);
      api_piece(api, 'ungrab', tpiece,tp, { });
    }
  }
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

  let halo_colour = null;
  if (p.cseq_updatesvg != null) {
    halo_colour = 'purple';
  } else if (p.last_seen_moved != null) {
    halo_colour = 'yellow';
  } else if (p.held != null && p.pinned) {
    halo_colour = '#8cf';
  }
  if (halo_colour != null) {
    let nelem = ancillary_node(piece, halo_colour);
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
	  let z = wasm_bindgen.increment(oldtop_p.z);
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
(j: { when: string, logent: { html: string } }) {
  add_timestamped_log_message(j.when, j.logent.html);
}

function add_log_message(msg_html: string) {
  add_timestamped_log_message('', msg_html);
}

function add_timestamped_log_message(ts_html: string, msg_html: string) {
  var lastent = log_elem.lastElementChild;
  var in_scrollback =
    lastent == null ||
    // inspired by
    //   https://stackoverflow.com/questions/487073/how-to-check-if-element-is-visible-after-scrolling/21627295#21627295
    // rejected
      //   https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API
      (() => {
	let le_top = lastent.getBoundingClientRect()!.top;
	let le_bot = lastent.getBoundingClientRect()!.bottom;
	let ld_bot = logscroll_elem.getBoundingClientRect()!.bottom;
	console.log("ADD_LOG_MESSAGE bboxes: le t b, bb",
		    le_top, le_bot, ld_bot);
	return 0.5 * (le_bot + le_top) > ld_bot;
      })();

  console.log('ADD LOG MESSAGE ',in_scrollback, layout, msg_html);

  var ne : HTMLElement;

  function add_thing(elemname: string, cl: string, html: string) {
    var ie = document.createElement(elemname);
    ie.innerHTML = html;
    ie.setAttribute("class", cl);
    ne.appendChild(ie);
  }

  if (layout == 'Portrait') {
    ne = document.createElement('tr');
    add_thing('td', 'logmsg', msg_html);
    add_thing('td', 'logts',  ts_html);
  } else if (layout == 'Landscape') {
    ne = document.createElement('div');
    add_thing('span', 'logts',  ts_html);
    ne.appendChild(document.createElement('br'));
    add_thing('span', 'logmsg', msg_html);
    ne.appendChild(document.createElement('br'));
  } else {
    throw 'bad layout ' + layout;
  }
  log_elem.appendChild(ne);

  if (!in_scrollback) {
    logscroll_elem.scrollTop = logscroll_elem.scrollHeight;
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
  z: ZCoord,
  zg: Generation,
  pinned: boolean,
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
  p.pinned = info.pinned;
  p.uos = info.uos;
  piece_set_zlevel(piece,p, (oldtop_piece)=>{
    p.z  = info.z;
    p.zg = info.zg;
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
(piece,p, info: { z: ZCoord, zg: Generation }) {
  piece_set_zlevel(piece,p, (oldtop_piece)=>{
    let oldtop_p = pieces[oldtop_piece]!;
    p.z  = info.z;
    p.zg = info.zg;
  });
}

messages.Recorded = <MessageHandler>function
(j: { piece: PieceId, cseq: ClientSeq, gen: Generation
      zg: Generation|null, svg: string | null } ) {
  let piece = j.piece;
  let p = pieces[piece]!;
  if (p.cseq != null && j.cseq >= p.cseq) {
    p.cseq = null;
  }
  if (p.cseq_updatesvg != null && j.cseq >= p.cseq_updatesvg) {
    p.cseq_updatesvg = null;
    redisplay_ancillaries(piece,p);
  }
  if (j.svg != null) {
    p.delem.innerHTML = j.svg;
    p.pelem= piece_element('piece',piece)!;
    redisplay_ancillaries(piece,p);
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

function startup() {
  console.log('STARTUP');
  console.log(wasm_bindgen.setup("OK"));

  var body = document.getElementById("main-body")!;
  ctoken = body.dataset.ctoken!;
  us = body.dataset.us!;
  gen = +body.dataset.gen!;
  let sse_url_prefix = body.dataset.sseUrlPrefix!;
  status_node = document.getElementById('status')!;
  status_node.innerHTML = 'js-done';
  log_elem = document.getElementById("log")!;
  logscroll_elem = document.getElementById("logscroll") || log_elem;
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

  var es = new EventSource(
    sse_url_prefix + "/_/updates?ctoken="+ctoken+'&gen='+gen
  );
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
  es.addEventListener('player-gone', function(event) {
    console.log('PLAYER-GONE', event);
    status_node.innerHTML = (event as any).data;
    es.close();
  });
  es.addEventListener('updates-expired', function(event) {
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
  space.addEventListener('mousedown', some_mousedown);
  document.addEventListener('keydown',   some_keydown);
}

declare var wasm_input : any;
var wasm_promise : Promise<any>;;

function doload(){
  console.log('DOLOAD');
  globalinfo_elem = document.getElementById('global-info')!;
  layout = globalinfo_elem!.dataset!.layout! as any;
  var elem = document.getElementById('loading_token')!;
  var ptoken = elem.dataset.ptoken;
  xhr_post_then('/_/session/' + layout, 
		JSON.stringify({ ptoken : ptoken }),
		loaded);

  wasm_promise = wasm_input
    .then(wasm_bindgen);
}

function loaded(xhr: XMLHttpRequest){
  console.log('LOADED');
  var body = document.getElementById('loading_body')!;
  wasm_promise.then((got_wasm) => {
    wasm = got_wasm;
    body.outerHTML = xhr.response;
    startup();
  });
}

// todo scroll of log messages to bottom did not always work somehow
//    think I have fixed this with approximation

doload();
