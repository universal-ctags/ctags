-module(maze).
-vsn('2002.0317').
-author('cpressey@catseye.mb.ca').
-copyright('Copyright (c)2002 Cat`s Eye Technologies. All rights reserved.').

%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%   Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%%   Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in
%%%   the documentation and/or other materials provided with the
%%%   distribution.
%%%
%%%   Neither the name of Cat's Eye Technologies nor the names of its
%%%   contributors may be used to endorse or promote products derived
%%%   from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
%%% OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
%%% OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%%% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE. 

-include("maze.hrl").

-export([build/0, generate/1]).


%%% BEGIN maze.erl %%%

%%% A simple maze-drawing program.

%% Driver function -----------------------------------------------------

build() ->
  Tot = generate(#maze{}),
  tot_print(Tot).

%% Maze generation function --------------------------------------------

generate(#maze{}=M) ->
  seed(),
  {X, Y} = {random:uniform(M#maze.width div 2) * 2, random:uniform(M#maze.height div 2) * 2},
  
  R2 = tot_put(X, Y, tot_new(M#maze.width, M#maze.height, M#maze.wall), M#maze.space),
  generate(M, R2, X, Y).
  
generate(#maze{}=M, R, X, Y) ->
  lists:foldl(fun({DX, DY}, A) ->
                NX = X + DX * 2, NY = Y + DY * 2,
		W = M#maze.wall,
                case catch tot_get(NX, NY, A) of
		   W ->
                    M1 = tot_put(X + DX, Y + DY, A, M#maze.space),
                    M2 = tot_put(NX, NY, M1, M#maze.space),
                    generate(M, M2, NX, NY);
	          _ -> A
		end 
	      end, R, scramble([{-1,0}, {1,0}, {0,-1}, {0,1}])).

%%% ToT (Tuple-of-Tuples) Utilities ------------------------------------

tot_new(W, H, Cell) ->
  erlang:make_tuple(H, erlang:make_tuple(W, Cell)).
  
tot_get(X, Y, Tot) ->
  element(X, element(Y, Tot)).
  
tot_put(X, Y, Tot, V) ->
  setelement(Y, Tot, setelement(X, element(Y, Tot), V)).

tot_print(ToT) ->
  tot_print(1, ToT).
tot_print(Y, ToT) when Y =< size(ToT) ->
  tot_print_tuple(element(Y, ToT)),
  io:fwrite("~n"),
  tot_print(Y+1, ToT);
tot_print(Y, ToT) -> ok.
tot_print_tuple(T) ->
  tot_print_tuple(1, T).
tot_print_tuple(X, T) when X =< size(T) ->
  io:fwrite("~s", [[element(X, T)]]),
  tot_print_tuple(X+1, T);
tot_print_tuple(X, T) -> ok.
  
%%% Randomness Functions -----------------------------------------------

%% Seed the random number generator so that it will produce unpredictable
%% values.  Should be called once at startup, before using random numbers.

seed() ->
  {H,M,S} = time(),
  random:seed(S,M,H),
  random:uniform(23).  % prime the pump - first number can be iffy

%% Pick a random element from a tuple or a list (equal chance for every
%% element.)

pick(Tuple) when tuple(Tuple) ->
  pick(tuple_to_list(Tuple));
pick(List) ->
  lists:nth(random:uniform(length(List)), List).

%% Mix up the order (shuffle or scramble) a tuple or list.

scramble(Tuple) when tuple(Tuple) ->
  list_to_tuple(scramble(tuple_to_list(Tuple)));
scramble(List) ->
  scramble(List, []).
scramble([], Acc) -> Acc;
scramble(List, Acc) ->
  S = pick(List),
  scramble(List -- [S], Acc ++ [S]).

%%% END of maze.erl %%%   
