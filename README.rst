=========
parthenon
=========

.. image:: https://github.com/AntoineGagne/parthenon/actions/workflows/erlang.yml/badge.svg
    :target: https://github.com/AntoineGagne/parthenon/actions
.. image:: https://coveralls.io/repos/AntoineGagne/parthenon/badge.png?branch=master
    :target: https://coveralls.io/r/AntoineGagne/parthenon?branch=master
.. image:: http://img.shields.io/hexpm/v/parthenon.svg?style=flat
    :target: https://hex.pm/packages/parthenon

:Author: `Antoine Gagné <gagnantoine@gmail.com>`_

.. contents::
    :backlinks: none

.. sectnum::

An OTP application that parses AWS Athena structures into Erlang terms.

Usage
=====

Inside ``rebar.config``:

.. code-block:: erlang
    
    %% ...
    {deps, [
        %% ...
        parthenon
    ]}. 

Inside ``.app.src``:

.. code-block:: erlang
    
    %% ...
    {applications, [
        %% ...
        parthenon
    ]},

Inside the code:

.. code-block:: erlang

    parthenon:add_schema(athena_structure, <<"struct<a: int, b: int>">>).
    parthenon:decode(athena_structure, <<"{a=123, b=456}">>).

    {ok, Binary} = file:read_file("/path/to/athena-structure-schema").
    parthenon:add_schema(athena_structure, Binary).
    {ok, RawStructure} = file:read_file("/path/to/raw-athena-structure").
    parthenon:decode(athena_structure, RawStructure).

Development
===========

Running all the tests and linters
---------------------------------

You can run all the tests and linters with the ``rebar3`` alias:

.. code-block:: sh

    rebar3 check
