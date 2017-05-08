-module(erc).
%-compile(export_all).
-export([connect/2,history/1, start/0,chat/2,filter/3,plunk/2,censor/2,
client_chat/3, client_plunk/3, client_censor/3,
client_filter_compose/3,client_filter_replace/3,get_history_len/3]).
-import(srv, [handle_connect/2,startServ/0,len/1]).

request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.
%%% Test API

% For testing chat
client_chat(C, S, Msg) ->
  S ! {C, chat, Msg}.

% For testing filter compose
client_filter_compose(C, S, P) ->
  S ! {C, filter, {compose, P}}.

% For testing filter replace
client_filter_replace(C, S, P) ->
  S ! {C, filter, {replace,P}}.

% For testing plunk
client_plunk(C,S,Nick) ->
  S ! {C,filter, {compose, fun({User,_}) ->
    case User of
      Nick -> false;
      _-> true end end}}.

% For testing censor
client_censor(C,S,Words) ->
  S ! {C, filter, {compose, fun({_,Msg}) ->
    free_of_word(Msg,Words)
    end}}.

populate_history(C,S,1) -> client_chat(C,S,"message");
populate_history(C,S,Number) ->
  client_chat(C,S,"message" ++ integer_to_list(Number)),
  populate_history(C,S,Number-1).

get_history_len(C,S,Number) ->
  populate_history(C,S,Number),
  A = history(S),
  len(A).
%%% Client API
% Start the server
start() -> startServ().

% Connect to a server with a username, this call creates a client
% server, and asks the server to connect through startcli
connect(Server, Username) ->
  % Do it here
  spawn(fun() -> startcli(Username, Server) end).

% Ask the client to connect, with the username Nick, start client
% server by looping.
startcli(Nick, Pid) ->
  handle_connect(Pid, {self(),connect,Nick}),
  loop(Pid,Nick).

% Send a message to the other clients on the server.
chat(Server,Cont) ->
  Server ! {self(),chat, Cont}.

% Retrieve the message history from the server.
history(Server) ->
  request_reply(Server, get_history).

% Add a filter to a client of a given method (compose or replace)
% which filters messages sent to the client
filter(Server, Method, P) ->
  Server ! {self(), filter, {Method,P}}.

% Mute another user Nick, by adding a filter which discards
% all messages sent from Nick
plunk(Server,Nick) ->
  Server ! {self(),filter, {compose, fun({User,_}) ->
    case User of
      Nick -> false;
      _-> true end end}}.

% discard message to the client if the message contains any of
% the blacklisted words
censor(Server, Words) ->
  Server ! {self(), filter, {compose, fun({_,Msg}) ->
    free_of_word(Msg,Words)
    end}}.

% returns true if Substring is not a substring of String
% returns false otherwise
is_not_substring(String, Substring)->
  string:str(String, Substring) =:= 0.

% Returns true if the given word is not a substring of any
% of the censored words
free_of_word(_,[]) -> true;
free_of_word(String,[Word|Wordlist]) ->
  is_not_substring(String, Word) andalso free_of_word(String,Wordlist).

%% Client Server:
loop(Pid,Nick) ->
  receive
    % Connection was succesfull
    {ok,_} ->
      io:format("~s has connected ~n",[Nick]),
      loop(Pid,Nick);
    % Connection was unsuccesfull, break loop
    {error,Username, is_taken} ->
      io:format("Error: ~s is taken",[Username]),
      loop(Pid, Nick);
    % Received message, which is then printed
    {new_msg, From, Msg} ->
      io:format("[~s's client] - ~s: ~s~n", [Nick, From, Msg]),
      loop(Pid, Nick);
    {From, Other} ->
      From ! {self(), {error,unknow_request, Other}},
      loop(Pid,Nick)
  end.