%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright  2007 Roberto Saccon
%%% @doc
%%%This module contains functions for starting a c-node which binds  a subset
%%% of the cairo 2D graphics functions
%%% @reference  See <a href="http://erlycairo.googlecode.com" target="_top">http://erlycairo.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(erlycairo_demo).
-author('rsaccon@gmail.com').

-export([create_images/0, scale/3]).
 

%%=====================================================================
%%  API Functions
%%=====================================================================

    
create_images()->
    rect("images/rect.png", 100, 100, {1.0, 0.2, 0.7, 1.0}).
     
scale(File, X, Y) ->
	{ok, S} = erlycairo_server:surface_create_from_png(File),
	{ok, Width} = erlycairo_server:surface_get_width(S),
	{ok, Height} = erlycairo_server:surface_get_height(S),
	erlycairo_server:new_image_blank(Width * X, Height * Y),
	erlycairo_server:scale(X, Y),
	erlycairo_server:set_source_surface(S, 0, 0),
	erlycairo_server:paint(),
	{ok, Stream} = erlycairo_server:write_to_png_stream(),
	erlycairo_server:surface_destroy(S),
	erlycairo_server:close_image(),
	Stream.

%% ============================================
%% Internal functions
%% ============================================

rect(File, Width, Height, {Red, Green, Blue, Alpha}) ->
    case erlycairo_server:new_image_blank(Width, Height) of
        ok ->
            erlycairo_server:set_source_rgba(Red, Green, Blue, Alpha),
            erlycairo_server:rectangle(0, 0, Width, Height), 
            erlycairo_server:fill(),
            erlycairo_server:write_to_png(File),
            erlycairo_server:close_image(),
            ok;
        {error, Reason} ->
            exit(Reason)
    end.

