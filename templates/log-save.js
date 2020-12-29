<script>
    orig_console = window.console;
    window.console = (function(){
        var saved = [ ];
        var new_console = { saved: saved};
        for (k of ['log','error','warn','info']) {
            (function(k){
                var orig = orig_console[k];
                new_console[k] = function() {
                    saved.push([k, arguments]);
                    orig.apply(orig_console, arguments);
                }
            })(k);
        }
        return new_console;
    })();

    console.log('wdriver.rs console log starts');
</script>
