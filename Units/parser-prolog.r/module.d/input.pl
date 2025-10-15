% Taken from swipl-9.2.9

/*

    Part of CLP(Q) (Constraint Logic Programming over Rationals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(clpq,
	  [ {}/1,
	    maximize/1,
	    minimize/1,
	    inf/2, inf/4, sup/2, sup/4,
	    bb_inf/3,
	    bb_inf/4,
	    ordering/1,
	    entailed/1,
	    clp_type/2,
	    dump/3%, projecting_assert/1
	  ]).
:- license(gpl_swipl, 'CLP(Q)').
:- use_module(library(dialect)).
:- expects_dialect(swi).

%
% Don't report export of private predicates from clpq
%
:- multifile
	user:portray_message/2.

:- dynamic
	user:portray_message/2.
%
user:portray_message(warning,import(_,_,clpq,private)).

:- use_module([ clpq/bb_q,
		clpq/bv_q,
		clpq/fourmotz_q,
		clpq/ineq_q,
		clpq/itf_q,
		clpq/nf_q,
		clpq/store_q,
		clpqr/class,
		clpqr/dump,
		clpqr/geler,
		clpqr/itf,
		clpqr/ordering,
		clpqr/project,
		clpqr/redund,
		library(ugraphs)
	      ]).

		 /*******************************
		 *	 TOPLEVEL PRINTING	*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(query(YesNo,Bindings)) --> !,
	{dump_toplevel_bindings(Bindings,Constraints)},
	dump_format(Constraints),
        '$messages':prolog_message(query(YesNo,Bindings)).

dump_toplevel_bindings(Bindings,Constraints) :-
	dump_vars_names(Bindings,[],Vars,Names),
	dump(Vars,Names,Constraints).

dump_vars_names([],_,[],[]).
dump_vars_names([Name=Term|Rest],Seen,Vars,Names) :-
	(   var(Term),
	    (   get_attr(Term,clpqr_itf,_)
	    ;   get_attr(Term,clpqr_geler,_)
	    ),
	    \+ memberchk_eq(Term,Seen)
	->  Vars = [Term|RVars],
	    Names = [Name|RNames],
	    NSeen = [Term|Seen]
	;   Vars = RVars,
	    Names = RNames,
	    Seen = NSeen
	),
	dump_vars_names(Rest,NSeen,RVars,RNames).

dump_format([]) --> [].
dump_format([X|Xs]) -->
	['{~w}'-[X], nl],
	dump_format(Xs).

memberchk_eq(X,[Y|Ys]) :-
	(   X == Y
	->  true
	;   memberchk_eq(X,Ys)
	).
