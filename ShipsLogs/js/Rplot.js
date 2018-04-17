(function($) {
    $(document).ready(function() {
	
	$('#Rplot').scianimator({
	    'images': ['images/Rplot1.png'],
	    'width': 480,
	    'delay': 10,
	    'loopMode': 'loop',
 'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed']
	});
	$('#Rplot').scianimator('play');
    });
})(jQuery);
