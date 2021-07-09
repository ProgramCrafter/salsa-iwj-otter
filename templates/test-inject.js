<script>
    orig_console = window.console;
    window.console = (function(){
      var new_console = { saved: [] };
      for (k of ['log','error','warn','info']) {
        (function(k){
          var orig = orig_console[k];
          new_console[k] = function() {
            let args = [].slice.call(arguments);
            new_console.saved.push([k, [args.map(s => (
	      s === undefined ? "<undefined>" :
		s === null ? "<null>" :
		s.toString()
	    ))]]);
            orig.apply(orig_console, arguments);
          }
        })(k);
      }
      return new_console;
    })();
    console.log('wdriver.rs console log starts');
</script>
