=========
parthenon
=========

.. image:: https://github.com/AntoineGagne/parthenon/actions/workflows/erlang.yml/badge.svg
    :target: https://github.com/AntoineGagne/parthenon/actions

:Author: `Antoine Gagné <gagnantoine@gmail.com>`_

.. contents::
    :backlinks: none

.. sectnum::

A library that parses AWS Athena structures into Erlang terms.

Usage
=====

.. code-block:: erlang

    {ok, Binary} = file:read_file("/path/to/athena-structure-schema").
    parthenon:add_schema(athena_structure, Binary).

Development
===========

Running all the tests and linters
---------------------------------

You can run all the tests and linters with the ``rebar3`` alias:

.. code-block:: sh

    rebar3 check
