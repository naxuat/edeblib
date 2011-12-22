==============================================
edeblib -- a library to work with Debian files
==============================================

`travis-ci status`_: |ci-status|

.. |ci-status| image:: https://secure.travis-ci.org/sa2ajj/edeblib.png?branch=master

.. _travis-ci status: http://travis-ci.org/#!/sa2ajj/edeblib

.. contents::

Introduction
============

This library was written as a part of an internal project and made open source
so other people can benefit (we also hope for contributions, but this is
another story).

There are three modules of interest for users of the library:

``edeblib_parser.erl``
    Parse various Debian formatted files (control, .dsc, .changes, Sources, Packages)

``edeblib_load.erl``
    Load files (.changes, .dsc, .deb)

``edeblib_version.erl``
    Parse, format and compare Debian versions according to `Debian Policy`_.

.. note::

    Parsing (as well as loading) works on non-signed files only.

.. _Debian Policy: http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version

``edeblib_parser``
==================

``edeblib_parser`` provides access to the parsing functionality of the library.
There are three ways to parse data:

* parse a given file (``.changes``, ``.dsc``, ``control``)
* parse a given blob (the blob must be one or more stanzas)
* create an instance of a parser and feed data to it

Parse a File
------------

.. function:: edeblib_parser:parse_file(FileName)

    Parse the given file.

    :param FileName: name of the file to parse
    :type FileName: string() or binary()
    :rtype: [proplist()]

Parse a BLOB
------------

.. function:: edeblib_parser:parse_blob(Blob)

    Parse the given blob.

    :param Blob: a blob to parse
    :type Blob: binary()
    :rtype: [proplist()]

Use a Parser
------------

.. function:: edeblib_parser:new()

    Create an instance of the parser.

    :rtype: edeblib_parser:parser()

.. function:: edeblib_parser:feed(Data, Parser)

    Feed data to the parser.

    :param Data: data to process
    :type Data: binary() or ``eof``
    :param edeblib_parser:parser() Parser: parser to feed
    :rtype: edeblib_parser:parser() or [proplist()]

``edeblib_load``
================

``edeblib_load`` module allows to:

* load data from the given file (in other words, a bit more sophisticated
  interface to ``edeblib_parser``)
* check the input files
* determine the type of input file

.. function:: edeblib_load:load_changes(FileName)

    Load ``.changes`` file.

    :param FileName: file to load
    :type FileName: binary() or string()
    :rtype: {``ok``, proplist()} or {``error``, Error}

.. function:: edeblib_load:load_source(FileName)

    Load ``.dsc`` file.

    :param FileName: file to load
    :type FileName: binary() or string()
    :rtype: {``ok``, proplist()} or {``error``, Error}

.. function:: edeblib_load:load_binary(FileName)

    Load ``.deb`` file.

    :param FileName: file to load
    :type FileName: binary() or string()
    :rtype: {``ok``, proplist()} or {``error``, Error}

.. function:: edeblib_load:load(FileName)

    Load file based on the extension.

    :param FileName: file to load
    :type FileName: binary() or string()
    :rtype: {``ok``, proplist()} or {``error``, Error}

.. function:: edeblib_load:check(FileName, FileEntry)

    Check if the given file fulfills given properties.

    :param FileName:
    :type FileName: binary() or string()
    :rtype: ``ok`` or {``error``, Error}

.. function:: edeblib_load:type(FileName)

    Determine type of the file.

    :param FileName:
    :type FileName: binary() or string()
    :rtype: ``changes`` | ``source`` | ``binary`` | ``unknown``

``edeblib_version``
===================

``edeblib_version`` allows to create, parse and compare Debian versions.

.. function:: edeblib_version:new(Epoch, Upstream, Revision)

    Produce an instance of edeblib_version:version() according to the given
    parameters.

    :param integer() Epoch: Debian version epoch
    :param binary() Upstream: Debian version upstream
    :param binary() Revision: Debian version revision
    :rtype: edeblib_version:version()

.. function:: edeblib_version:parse(Version)

    Parse the given version.

    :param binary() Version: version to parse
    :rtype: edeblib_version:version()

.. function:: edeblib_version:format(Version)

    Format the given version.

    :param edeblib_version:version() Version: version to format
    :rtype: binary()

.. function:: edeblib_version:compare(VersionA, VersionB)

    Compare given versions.

    :param VersionA: first version to compare
    :type VersionA: edeblib_version:version() or binary()
    :param VersionB: second version to compare
    :type VersionB: edeblib_version:version() or binary()
    :rtype: -1 or 0 or 1

Licence
=======

::

    Copyright (C) 2011 by the edeblib contributors.  Please see file AUTHORS for a complete list.

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.

..
    vim:tw=80
