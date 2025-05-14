% Taken from pl-9.2.9-build/swipl-9.2.9/src/test.pl
% 0'. can be a trouble.

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1996-2023, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/
                 /*******************************
                 *      UNICODE FILENAMES       *
                 *******************************/

unicode_file_name(Name) :-
    current_prolog_flag(pid, Pid),
    atom_codes(Name0, [1074, 1086, 1079, 1076, 1091, 1093, 1072]),
    atomic_list_concat([Name0, -, Pid], Name).

unicode_file(mkdir-1) :-                        % create Cyrillic directory
    unicode_file_name(Dir),
    catch(delete_directory(Dir), _, true),
    make_directory(Dir),
    exists_directory(Dir),
    working_directory(Old, Dir),
    working_directory(O2, '..'),
    same_file(Old, '.'),
    same_file(O2, Dir),
    delete_directory(Dir).
unicode_file(file-1) :-                         % create Cyrillic file
    unicode_file_name(File),
    Term = hello(world),
    catch(delete_file(File), _, true),
    open(File, write, Out),
    format(Out, '~q.~n', [Term]),
    close(Out),
    exists_file(File),
    open(File, read, In),
    read(In, Read),
    close(In),
    Read =@= Term,
    delete_file(File).
unicode_file(absfile-1) :-
    unicode_file_name(File),
    absolute_file_name(File, Path),
    file_directory_name(Path, Dir),
    same_file(Dir, '.'),
    file_base_name(Path, Base),
    Base == File.
unicode_file(ext-1) :-
    atom_codes(File, [1074, 1086, 1079, 1076, 0'., 1091, 1093, 1072]),
    file_name_extension(Base, Ext, File),
    atom_codes(Base, [1074, 1086, 1079, 1076]),
    atom_codes(Ext, [1091, 1093, 1072]).


                 /*******************************
                 *              SEEK            *
                 *******************************/

seek(write-1) :-
    tmp_file(seek, File),
    open(File, write, S, [type(binary)]),
    Max = 999,
    forall(between(0, Max, _),
           format(S, '1234567890~n', [])),
    forall(between(0, Max, N),
           (   Pos is N * 11 + 6,
               seek(S, Pos, bof, _),
               format(S, 'x', [])
           )),
    close(S),

    open(File, read, In, [type(binary)]),
    atom_codes('123456x890\n', Bytes),
    forall(between(0, Max, N),
           must_read(Bytes, In)),
    close(In),
    delete_file(File).
