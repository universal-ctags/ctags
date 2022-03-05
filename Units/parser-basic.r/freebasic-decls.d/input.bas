' Taken from https://termbin.com/x9wo submitted by @Glog78
namespace curses 'public use this
	#include "curses_priv.bi"
	#inclib "curses"

	extern "c"

		type win as curses.curses_priv._win_st

		declare	function	_Init	alias "initscr" () as win ptr
		declare	function	GetMaxX alias	"getmaxx" (byval as win ptr) as integer
		declare	function	GetMaxY alias "getmaxy"	(byval as win ptr) as integer
		declare	function	CreateWindow alias "newwin" (byval as integer, byval as integer, byval as integer, byval as integer) as win
		declare	function	CreateSubWindow alias "subwin" (byval as win ptr, byval as integer, byval as integer, byval as integer, byval as integer) as win ptr
		declare function	GetChar alias "getch" () as UByte
		declare function	CBreak alias "cbreak" () as integer
		declare function	EreaseTerm alias "erase" () as integer
		declare function	ClearTerm alias "clear" () as integer
		declare function EreaseWindow alias "werase" (byval as win ptr) as integer
		declare function ClearWindow alias "wclear" (byval as win ptr) as integer
		declare function DeleteWindow alias "delwin" (byval as win ptr) as integer
		declare function _Exit alias "endwin" () as integer
		declare function RefresTerm alias "refresh" () as integer
		declare function RefreshWindow alias "wrefresh" (byval as win ptr) as integer
		declare function PrintTerm  alias "printw" (byval as zstring ptr, ... ) as integer
		declare function PrintWindow alias "wprintw" (byval as win ptr, byval as zstring ptr, ... ) as integer
		declare function SetCursorPositionTerm alias "move" (byval as integer,byval as integer) as integer
		declare function SetCursorPositionWindow alias "wmove" (byval as win ptr, byval as integer, byval as integer) as integer

	end extern

end namespace 'curses

' vim: bs=2 sw=2 ss=2 ts=2 nu noet ft=basic
