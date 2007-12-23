%%%-------------------------------------------------------------------
%%% File:      erlycairo_server.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright 2007 Roberto Saccon
%%% @doc  
%%% This module contains functions for starting a c-node which binds a subset
%%% of the cairo 2D graphics functions
%%% @end
%%% @reference  See <a href="http://erlycairo.googlecode.com" target="_top">http://erlycairo.googlecode.com</a> for more information  
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
%%% @since 2007-11-29 by Roberto Saccon
%%%-------------------------------------------------------------------
-module(erlycairo_server).
-author('rsaccon@gmail.com').

-behaviour(gen_server).

-define(DEFAULT_CNODE_NUMBER, 1).
-define(TIMEOUT, 1000).

%% API
-export([start_link/0, 
    start_link/1,
    stop/0,
    new_image_blank/2,
    write_to_png/1,
    close_image/0,
    save/0,
    restore/0,
    set_line_width/1,
    set_source_rgba/4,
    set_operator/1,
    move_to/2,
    line_to/2,
    curve_to/6,
    rel_move_to/2,
    rel_line_to/2,
    rel_curve_to/6,
    rectangle/4,
    arc/5,
    arc_negative/5,
    close_path/0,
    paint/0,
    fill/0,
    fill_preserve/0,
    stroke/0,
    stroke_preserve/0,
    translate/2,
    scale/2,
    rotate/1,
    select_font_face/3,
    set_font_size/1,
    show_text/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {cnode, cnode_number, port}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ?DEFAULT_CNODE_NUMBER, []).
    
    
%%--------------------------------------------------------------------
%% @spec (CNodeNumber::integer()) -> {ok,Pid} | ignore | {error,Error}
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link(CNodeNumber) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, CNodeNumber, []).
  
  
%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc 
%% Stops the server
%% @end 
%%--------------------------------------------------------------------  
stop() ->
    gen_server:call(?MODULE, stop).
     
            
%%--------------------------------------------------------------------
%% @spec (Width::integer(), Height::integer()) ->
%%     (ok | {error, Reason})
%% @doc
%% Creates a blank image.
%% @end 
%%--------------------------------------------------------------------
new_image_blank(Width, Height) ->
    gen_server:call(?MODULE, {new_image_blank, {Width, Height}}).


%%--------------------------------------------------------------------
%% @spec (Filename::string()) -> (ok | {error, Reason})
%% @doc
%% Saves image as PNG file.
%% @end 
%%--------------------------------------------------------------------
write_to_png(Filename) ->
    gen_server:call(?MODULE, {write_to_png, {list_to_atom(Filename)}}).


%%--------------------------------------------------------------------
%% @spec () -> (ok | {error, Reason})
%% @doc
%% Closes image.
%% @end 
%%--------------------------------------------------------------------
close_image() ->
    gen_server:call(?MODULE, {close_image, {}}).


%%--------------------------------------------------------------------
%% @spec () -> (ok | {error, Reason})  
%% @doc 
%% Saves current drawing context to stack.
%% @end 
%%--------------------------------------------------------------------
save() ->
    gen_server:call(?MODULE, {save, {}}).
   
%%--------------------------------------------------------------------
%% @spec () -> (ok | {error, Reason})
%% @doc
%% Resores last drawing context from stack.
%% @end 
%%--------------------------------------------------------------------
restore() ->
    gen_server:call(?MODULE, {restore, {}}).


%%--------------------------------------------------------------------
%% @spec (Width::integer() | Width::float()) -> (ok | {error, Reason})
%% @doc
%% Sets the current line width within the cairo context.
%% @end 
%%--------------------------------------------------------------------
set_line_width(Width) ->
    gen_server:call(?MODULE, {set_line_width, {Width}}).


%%--------------------------------------------------------------------
%% @spec ((Red::integer() | Red::float()),
%%        (Green::integer() | Green::float()),
%%        (Blue::integer() | Blue::float()),
%%        (Alpha::integer() | Alpha::float())) -> (ok | {error, Reason})
%% @doc
%% Sets the current line width within the cairo context.
%% @end 
%%--------------------------------------------------------------------
set_source_rgba(Red, Green, Blue, Alpha) ->
    gen_server:call(?MODULE, {set_source_rgba, {Red, Green, Blue, Alpha}}).


%%--------------------------------------------------------------------
%% @spec (Opertor::integer()) -> (ok | {error, Reason})
%% @doc
%% Sets the current operator within the cairo context.
%% @end 
%%--------------------------------------------------------------------
set_operator(Operator) ->
    gen_server:call(?MODULE, {set_operator, {Operator}}).


%%--------------------------------------------------------------------
%% @spec ((X::integer() | X::float()), (Y::integer() | Y::float()))) -> 
%%     (ok | {error, Reason})
%% @doc
%% Starts a new subpath, current point will be (X, Y).
%% @end 
%%--------------------------------------------------------------------
move_to(X, Y) ->
    gen_server:call(?MODULE, {move_to, {X, Y}}).


%%--------------------------------------------------------------------
%% @spec ((X:integer() | X::float()), (Y::integer() | Y::float()))) ->
%%     (ok | {error, Reason})
%% @doc
%% Adds a line to the path from the current point to position (X, Y)
%% @end 
%%--------------------------------------------------------------------
line_to(X, Y) ->
    gen_server:call(?MODULE, {line_to, {X, Y}}).


%%--------------------------------------------------------------------
%% @spec ((X1::integer() | X1::float()),
%%        (Y1::integer() | Y1::float())
%         (X2::integer() | X2::float()),
%%        (Y2::integer() | Y2::float())
%%        (X3::integer() | X3::float()),
%%        (Y3::integer() | Y3::float()))) -> (ok | {error, Reason}) 
%% @doc
%% Adds a cubic Bezier spline to the path from the current point to position (X3, Y3)
%% @end 
%%--------------------------------------------------------------------
curve_to(X1, Y1, X2, Y2, X3, Y3) ->
    gen_server:call(?MODULE, {curve_to, {X1, Y1, X2, Y2, X3, Y3}}).


%%--------------------------------------------------------------------
%% @spec ((X::integer() | X::float()), (Y::integer() | Y::float()))) ->
%%     (ok | {error, Reason})
%% @doc
%% Relative-coordinate version of move_to().
%% @end 
%%--------------------------------------------------------------------
rel_move_to(X, Y) ->
    gen_server:call(?MODULE, {rel_move_to, {X, Y}}).


%%--------------------------------------------------------------------
%% @spec ((X::integer() | X::float()), (Y::integer() | Y::float()))) ->
%%     (ok | {error, Reason})
%% @doc
%% Relative-coordinate version of line_to().
%% @end 
%%--------------------------------------------------------------------
rel_line_to(X, Y) ->
    gen_server:call(?MODULE, {rel_line_to, {X, Y}}).


%%--------------------------------------------------------------------
%% @spec ((X1::integer() | X1::float()),
%%        (Y1::integer() | Y1::float())
%%        (X2::integer() | X2::float()),
%%        (Y2::integer() | Y2::float())
%%        (X3::integer() | X3::float()),
%%        (Y3::integer() | Y3::float()))) -> (ok | {error, Reason})
%% @doc
%% Relative-coordinate version of curve_to() to position (X3, Y3)
%% @end 
%%--------------------------------------------------------------------
rel_curve_to(X1, Y1, X2, Y2, X3, Y3) ->
    gen_server:call(?MODULE, {rel_curve_to, {X1, Y1, X2, Y2, X3, Y3}}).


%%--------------------------------------------------------------------
%% @spec ((X::integer() | X::float()),
%%        (Y::integer() | Y::float())
%%        (Width::integer() | Width::float()),
%%        (Height::integer() | Height::float())) -> (ok | {error, Reason})
%% @doc
%% Adds a closed sub-path rectangle of the given size to the current path at position (X, Y)
%% @end 
%%--------------------------------------------------------------------
rectangle(X, Y, Width, Height) ->
    gen_server:call(?MODULE, {rectangle, {X, Y, Width, Height}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% Adds an arc of the given radius and angles to the 
%% current path at position (X, Y)
%% @end 
%%--------------------------------------------------------------------
arc(X, Y, Radius, Ang1e1, Angle2) ->
     gen_server:call(?MODULE, {arc, {X, Y, Radius, Ang1e1, Angle2}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% Adds an arc of the given radius and angles to the 
%% current path at position (X, Y)
%% @end 
%%--------------------------------------------------------------------
arc_negative(X, Y, Radius, Ang1e1, Angle2) ->
     gen_server:call(?MODULE, {arc_negative, {X, Y, Radius, Ang1e1, Angle2}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% Closes the Path
%% @end 
%%--------------------------------------------------------------------
close_path() ->
    gen_server:call(?MODULE, {close_path, {}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
paint() ->
    gen_server:call(?MODULE, {paint, {}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
fill() ->
    gen_server:call(?MODULE, {fill, {}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
fill_preserve() ->
    gen_server:call(?MODULE, {fill_preserve, {}}).
    
    
%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
stroke() ->
    gen_server:call(?MODULE, {stroke, {}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
stroke_preserve() ->
    gen_server:call(?MODULE, {stroke_preserve, {}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
translate(TX, TY) ->
    gen_server:call(?MODULE, {translate, {TX, TY}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
scale(SX, SY) ->
    gen_server:call(?MODULE, {scale, {SX, SY}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
rotate(Angle) ->
    gen_server:call(?MODULE, {rotate, {Angle}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
select_font_face(Family, Slant, Weight) ->
    gen_server:call(?MODULE, {select_font_face, {list_to_atom(Family), Slant, Weight}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
set_font_size(Size) ->
    gen_server:call(?MODULE, {set_font_size, {Size}}).


%%--------------------------------------------------------------------
%% @spec  
%% @doc
%% @end 
%%--------------------------------------------------------------------
show_text(Text) ->
    gen_server:call(?MODULE, {show_text, {list_to_atom(Text)}}).   
   
        
        
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------  
init(CNodeNumber) ->  
    CNodeBinPath = filename:join([filename:dirname(code:which(?MODULE)),"..", "priv", "bin", "erlycairo"]),
    Cookie = atom_to_list(erlang:get_cookie()),
    Node = atom_to_list(node()),
    Cmd = lists:concat([CNodeBinPath, " ", CNodeNumber, " ", Cookie, " ", Node]),
    Port = open_port({spawn, Cmd}, []),   
    HostName = string:strip(os:cmd("hostname -s"), right, $\n),
    CNodeName = lists:concat(["c", CNodeNumber, "@", HostName]),
    {ok, #state{cnode_number = CNodeNumber, cnode = list_to_atom(CNodeName), port=Port}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    call_cnode(State#state.cnode, State#state.cnode_number, {stop, {}}),
    {stop, normal, State};
    
handle_call(Msg, _From, State) ->
    Reply = call_cnode(State#state.cnode, State#state.cnode_number, Msg),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("TRACE ~p:~p ~p~n",[?MODULE, ?LINE, "terminate"]),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

call_cnode(CNode, CNodeNumber, Msg) ->    
    {any, CNode} ! {call, self(), Msg},
    receive      
        {cnode, CNodeNumber, Result} ->
            Result
    after 
        ?TIMEOUT ->
            %% TODO: proper errorlogging
            {error, timeout}
    end.