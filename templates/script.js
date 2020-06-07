//

// xxx deployment note: need a whole bunch of domains for SSE conn limit

status_node = document.getElementById('spong');
status_node.innerHTML = 'js-done'

var our_dnd_type = "text/puvnex-game-server-dummy";

  console.log('foo1');
function test_dragstart(event) {
  console.log('foo2', event);
  if (event.target instanceof HTMLLIElement) {
    event.dataTransfer.setData(our_dnd_type,
			       event.target.dataset.objid);
    event.dataTransfer.effectAllowed = "move";
  } else {
    event.preventDefault();
  }
}

es = new EventSource("updates");
es.onmessage = function(event) {
  status_node.innerHTML = event.data;
}
