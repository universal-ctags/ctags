..
.. you can use rst2pdf or something rst2* to generate a sophisticated
.. file for reading and printing from rst files.
..
.. TODO: Makefile targets for converting are needed.
..

========================================================================
fishman-ctags hacking guide
========================================================================



:Version: Draft
:Editor: Masatake YAMATO <yamato@redhat.com>
:Web Page: https://github.com/fishman/ctags

Introduction
======================================================================
fishman-ctags is a project/git repository/command forked from
exuberant-ctags. The project is hosted at github.

The goal of the project is preparing and maintaining common/unified
working space where people interested in making ctags better can work
together.

fishman-ctags repository is started by Reza Jelveh
<reza.jelveh@gmail.com>.  I, the author of this document, is a
co-maintainer of the project.

This guide is for developers. ctags.1 man page is for users.
Though ctags.1 is not updated yet because fishman-ctags is still in
development.

This document is far from perfect. I am still
researching the original code derived from exuberant-ctags.

Proofreading and pull-requests are welcome!

Contents
======================================================================

.. contents::

.. section-numbering::

.. raw:: pdf

   PageBreak oneColumn

.. include:: f-tracking.rst
.. raw:: pdf

   PageBreak oneColumn

.. include:: f-news.rst
.. raw:: pdf

   PageBreak oneColumn

.. include:: f-building.rst
.. raw:: pdf

   PageBreak oneColumn

.. include:: f-units.rst
.. raw:: pdf

   PageBreak oneColumn

.. include:: f-optlib.rst
.. raw:: pdf

   PageBreak oneColumn

.. include:: f-xcmd.rst
.. raw:: pdf

   PageBreak oneColumn

.. include:: f-internal.rst
.. raw:: pdf

   PageBreak oneColumn
