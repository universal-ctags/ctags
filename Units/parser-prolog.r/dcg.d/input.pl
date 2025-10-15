% Taken from swipl-9.2.9
/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): Public domain
*/

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_session')).
:- use_module(library('http/http_error')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This demo shows session state  management   in  a very simple calculator
package. It also demonstrates the use of  the html_write library. To use
it, start Prolog, load this file and run

        ?- server.

Now direct your browser to http://localhost:3000/
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

server :-
    server(3000, []).

server(Port, Options) :-
    http_server(reply,
                [ port(Port),
                  timeout(20)
                | Options
                ]).

reply(Request) :-
    memberchk(path(Path), Request),
    reply(Path, Request).

reply(/, _Request) :-
    http_session_retractall(formula(_)),
    Formula = 0,
    http_session_assert(formula(Formula)),
    page(Formula).

reply('/calc', Request) :-
    memberchk(search(Search), Request),
    memberchk(operation=Op, Search),
    memberchk(value=AtomVal, Search),
    atom_number(AtomVal, Val),
    http_session_retract(formula(Formula0)),
    debug(calc, 'Formula0 = ~w', [Formula0]),
    Formula =.. [Op, Formula0, Val],
    http_session_assert(formula(Formula)),
    page(Formula).


page(Formula) :-
    reply_page('HTTP Session DEMO',
               [ h2('Simple session demo'),
                 form([ action('/calc'),
                        method('GET')
                      ],
                      table([align(center), border(1)],
                            [ tr(td(\formula(Formula))),
                              tr(td([ \ops,
                                      input([ name(value) ]),
                                      input([ type(submit),
                                              value('Calc!')
                                            ])
                                    ]))
                            ]))
               ]).

formula(Formula) -->
    { sformat(S, '~w', [Formula]),
      Value is Formula
    },
    html([ S, ' = ', Value ]).

ops -->
    html(select(name(operation),
                [ option([selected], +),
                  option([], -),
                  option([], /),
                  option([], *)
                ])).

reply_page(Title, Content) :-
    phrase(page(title(Title), Content), HTML),
    format('Content-type: text/html~n~n'),
    print_html(HTML).
