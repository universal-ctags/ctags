// Bugs item #681824, was opened at 2003-02-06 18:14
// You can respond by visiting: 
// https://sourceforge.net/tracker/?func=detail&atid=106556&aid=681824&group_id=6556
// 
// Category: None
// Group: None
// Status: Open
// Resolution: None
// Priority: 5
// Submitted By: Thomi Dammann (thomi35)
// Assigned to: Nobody/Anonymous (nobody)
// Summary: Comments disturb ctags (v. 5.4)  scaning a PHP-file?
// 
// Initial Comment:
// If I have (correct) statements like
// 
 function db_query
    ($pDB,				// connection handle
     $pQuery)		  // query string
  {
...
}
// 
// it seems that ctags used with vim doesn't find the
// function names anymore. I think that either the
// comments or the distribution on more than one line
// disturb the parser.
// 
// Regards
// 
// Thomi
