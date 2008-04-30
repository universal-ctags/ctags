// I've attached the simple test case as bugDemo.js. Trigger the bug by
// uncommenting line 8. Attached are tags files with line 21 commented/not
// commented. This is the current tip, svn r663.
//
// When the container.dirtyTab line is uncommented you see these methods:
//     TabChrome.createTabTile
//     TabChrome.init
//
// When the container.dirtyTab line is commented you see these methods:
//     TabChrome.createTabTile
//     TabChrome.destroyTabTile
//     TabChrome.init
//
TabChrome.prototype = {
	init: function() 
	{
		this.browserMap = new Object();
	},
	createTabTile: function(browser) 
	{
		//container.dirtyTab = {'url': false, 'title':false, 'snapshot':false, '*': false}		
		return container;
	},
	destroyTabTile: function(tile)
	{
	}
}
Different.prototype = {
	init: function() 
	{
		this.browserMap = new Object();
	},
	createTabTile: function(browser) 
	{
		container.dirtyTab = {'url': false, 'title':false, 'snapshot':false, '*': false}		
		return container;
	},
	destroyTabTile: function(tile)
	{
	}
}
