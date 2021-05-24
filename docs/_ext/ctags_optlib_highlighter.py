from pygments.lexer import RegexLexer, bygroups
from pygments.token import *

#
# The choice of license is done for merging this to the upstream project
# in the future.
#

# Copyright (c) 2021 by Masatake YAMATO.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#
# References:
# - https://pygments.org/docs/lexerdevelopment/
# - https://pygments.org/docs/tokens/#module-pygments.token
#
# For testing:
#   python -m pygments -x -l ctags_highlighter.py:CtagsOptlibLexer foo.ctags  | less -r
#

class CtagsOptlibLexer(RegexLexer):
    name = 'ctags'
    aliases = ['ctags']
    filenames = ['*.ctags']

    tokens = {
        'root': [
            (r'#.*\n', Comment.Single),
            (r'^[ \t]+', Whitespace),
            (r'(--langdef)(=)([a-zA-Z0-9#+]+)(.*\n)',
             bygroups(Keyword.Namespace, Punctuation,
                      Name.Namespace, Text
                      )),
            (r'(--map-)([a-zA-Z0-9#+]+)(=\+?)(.*\n)',
             bygroups(Keyword.Declaration, Name.Namespace, Punctuation,
                      Text
                      )),
            (r'(--langmap)(=\+?)([a-zA-Z0-9#+]+)(:)(.*\n)',
             bygroups(Keyword.Declaration, Punctuation, Name.Namespace, Punctuation,
                      Text
                      )),
            # kinds
            (r'(--kinddef-)([a-zA-Z0-9#+]+)(=)(.)(,)([a-zA-Z0-9]+)(,)(.*\n)',
             bygroups(Keyword.Declaration, Name.Namespace, Punctuation,
                      Name.Entity, Punctuation, Name.Entity, Punctuation, String.Doc)),
            (r'(--kinds-)([a-zA-Z0-9#+]+)(=\+?)(.*\n)',
             bygroups(Keyword, Punctuation,
                      Text
                      )),
            # fields
            (r'(--_fielddef-)([a-zA-Z0-9#+]+)(=)([a-zA-Z0-9]+)(,)(.*)(\n)',
             bygroups(Keyword.Declaration, Name.Namespace, Punctuation,
                      Name.Type, Punctuation, String.Doc,
                      Text
                      )),
            (r'(--fields)(=\+?)(.*\n)',
             bygroups(Keyword, Punctuation,
                      Text
                      )),
            (r'(--fields-)([a-zA-Z0-9#+]+)(=\+?)(.*\n)',
             bygroups(Keyword, Name.Namespace, Punctuation,
                      Text)),
            # extras
            (r'(--_extradef-)([a-zA-Z0-9#+]+)(=)([a-zA-Z0-9]+)(,)(.*)(\n)',
             bygroups(Keyword.Declaration, Name.Namespace, Punctuation,
                      Name.Variable, Punctuation, String.Doc,
                      Text
                      )),
            (r'(--extras?)(=\+?)(.*\n)',
             bygroups(Keyword, Punctuation,
                      Text)),
            (r'(--extras?-)([a-zA-Z0-9#+]+)(=\+?)(.*\n)',
             bygroups(Keyword, Name.Namespace, Punctuation,
                      Text
                      )),
            # roles
            (r'(--_roledef-)([a-zA-Z0-9#+]+)(\.)([a-zA-Z])(=)([a-zA-Z0-9]+)(,)(.*)(\n)',
             bygroups(Keyword.Declaration, Name.Namespace, Punctuation, Name.Entity, Punctuation,
                      Name.Decorator, Punctuation, String.Doc,
                      Text
                      )),
            (r'(--roles-)([a-zA-Z0-9#+]+)(\.)([a-zA-Z])(=\+?)(.*\n)',
             bygroups(Keyword, Name.Namespace, Punctuation, Name.Entity, Punctuation,
                      Text
                      )),
            # regex  and tables
            (r'(--regex-|--mline-regex-)([a-zA-Z0-9#+]+)(=)(.*\n)',
             bygroups(Keyword.Declaration, Name.Namespace, Punctuation,
                      # TODO
                      Text
                      )),
            (r'(--_tabledef-)([a-zA-Z0-9#+]+)(=)([a-zA-Z0-9]*)(\n)',
             bygroups(Keyword.Declaration, Name.Namespace, Punctuation,
                      Name.Function,
                      Text
                      )),
            (r'(--_mtable-regex-)([a-zA-Z0-9#+]+)(=)([a-zA-Z0-9]*)(.*\n)',
             bygroups(Keyword.Declaration, Name.Namespace, Punctuation,
                      Name.Function,
                      Text
                      )),
            # TODO
            (r'.*\n', Text)
        ]
    }
