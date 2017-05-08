-module(srv).
%-compile(export_all).
-export([handle_connect/2,startServ/0,len/1]).

% Send message to server, and wait for reply
request_reply(Pid, {From, Request,Nick}) ->
  Pid ! {self(), {Request,Nick}},
  receive
    {Pid, Response} -> From ! Response
  end.

% Process client connection request
handle_connect(Server,Request) ->
  request_reply(Server,Request).

% start the server with empty list for State, History, Filter
startServ() -> spawn(fun () -> loop([],[],[]) end).

%% Server:
loop(State,History,Filter) ->
  % Create unique reference for newly connected user
  Ref = make_ref(),
  receive
    % Handle connection request by adding client to list of clients
    % if the user name isn't already used
    {From,{connect, Nick}} ->
      case (not_exists(State,Nick))  of
        true ->
          From ! {self(), {ok,Ref}},
          loop([{Nick, From} | State],History,Filter);
        false ->
          From ! {self(), {error,Nick, is_taken}},
          loop(State, History,Filter)
      end;
    % Handle chat request
    {From, chat, Msg} ->
      % get user name of connected client
      User = find(From,State),
      % add message to history
      Newhistory = add_msg(History,{User,Msg}),
      % get recipients who can receive message based on their predicates
      Recipients = get_eligible_recepients({User,Msg},State,Filter),
      % broadcast only to eligible recipients
      broadcast(new_msg, Recipients, {User, Msg}),
      loop(State,Newhistory,Filter);
    {From, get_history} ->
      % Send back message history
      From ! {self(), History},
      loop(State,History,Filter);
    {From, filter,{replace, P}} ->
      % Delete previous entries for user and add new filter to filterlist
      Newfilter = [{From, P}] ++ delete_filter_entries(From, Filter),
      loop(State,History,Newfilter);
    {From, filter,{compose, P}} ->
      % add filter to filterlist
      Newfilter = [{From, P}] ++ Filter,
      loop(State, History,Newfilter);
    {From, Other} ->
      From ! {self(), {error,unknow_request, Other}},
      loop(State,History,Filter)
  end.

% Return list of recipients who can receive message, based
% on their predicates.
get_eligible_recepients(_,[],_) -> [];
get_eligible_recepients(Msg, [{User,Pid}|Clients],Filterlist) ->
  case should_send_predicate(Pid,Msg,Filterlist) of
    true  -> [{User,Pid}] ++ get_eligible_recepients(Msg, Clients,Filterlist);
    false -> get_eligible_recepients(Msg, Clients,Filterlist)
  end.

% given a list of predicates and a user,
% determine if a message should be send
should_send_predicate(From,Msg,Filterlist) ->
  A = get_predicate_list(From, Filterlist),
  should_send(Msg, A).

% Given a message and a list of predicates
% returns true if the message should be send
should_send(_, []) -> true;
should_send(Msg,[Pred|PredList]) ->
  Pred(Msg) andalso should_send(Msg,PredList).

% Returns a list of predicates of the given user
get_predicate_list(_, []) -> [];
get_predicate_list(From, [{From, Pred}|T]) ->
  [Pred] ++ get_predicate_list(From, T);
get_predicate_list(From, [_|T]) ->
  get_predicate_list(From, T).

% Delete filter entries corresponding to a given User
delete_filter_entries(_,[]) -> [];
delete_filter_entries(Pid,[{Pid,_}|T]) -> delete_filter_entries(Pid,T);
delete_filter_entries(From, [H|T]) ->
  [H] ++ delete_filter_entries(From,T).

not_exists([],_) -> true;
not_exists([{Elem,_}|_],Elem) -> false;
not_exists([{_,_} | T], Elem) -> not_exists(T, Elem).

% Get length
len([])    -> 0;
len([_|T]) -> 1 + len(T).

% Reverse
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

% Get tail of list
tail([_|T]) -> T.

% Drops the last element in the list by reversing the list
drop_last(List) -> reverse(tail(reverse(List))).

%% Keep track of message history size
add_msg(History,Msg) ->
  case len(History) of
    N when N < 42 -> [Msg | History];
    _ -> [Msg | drop_last(History)]
  end.

% Find user name, given process id.
find(_, []) -> "Notfounduser";
find(From, [{Username, Pid} | _]) when From == Pid ->
  Username;
find(From, [_ | T]) ->
  find(From, T).

% broadcast message to all users in State
broadcast(new_msg,State,{Nick,Msg})->
  sendtoall({new_msg,Nick,Msg},State).

sendtoall(Msg,State) ->
  lists:foreach(fun({_, Pid}) -> Pid ! Msg end, State).