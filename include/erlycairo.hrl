%% @author Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%% @copyright Roberto Saccon 2007
%%
%% @doc ErlyCairo: Erlang bindings for the Cairo 2D graphics library.
%%
%% This module contains a subset of the enumerations from the 
%% cairo header files (as Erlang  constants)

%% For license information see LICENSE.txt

%% cairo_font_slant_t
-define(CAIRO_FONT_WEIGHT_NORMAL,     0).
-define(CAIRO_FONT_WEIGHT_BOLD,       1).

%% cairo_font_weight_t
-define(CAIRO_FONT_SLANT_NORMAL,      0).
-define(CAIRO_FONT_SLANT_ITALIC,      1).
-define(CAIRO_FONT_SLANT_OBLIQUE,     2).

%% cairo_operator_t
-define(CAIRO_OPERATOR_CLEAR,         0).
-define(CAIRO_OPERATOR_SOURCE,        1).
-define(CAIRO_OPERATOR_OVER,          2).
-define(CAIRO_OPERATOR_IN,            3).
-define(CAIRO_OPERATOR_OUT,           4).
-define(CAIRO_OPERATOR_ATOP,          5).
-define(CAIRO_OPERATOR_DEST,          6).
-define(CAIRO_OPERATOR_DEST_OVER,     7).
-define(CAIRO_OPERATOR_DEST_IN,       8).
-define(CAIRO_OPERATOR_DEST_OUT,      9).
-define(CAIRO_OPERATOR_DEST_ATOP,    10).
-define(CAIRO_OPERATOR_XOR,          11).
-define(CAIRO_OPERATOR_ADD,          12).
-define(CAIRO_OPERATOR_SATURATE,     13).
 
