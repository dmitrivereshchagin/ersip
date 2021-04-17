%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Session-Expires and Min-SE header fields (RFC 4028)
%%%

-module(ersip_hdr_session_expires).

-export([interval/1,
         refresher/1,
         set_refresher/2,
         param/2,
         set_param/3,
         make/1,
         parse/1,
         assemble/1,
         assemble_bin/1
        ]).

-export_type([session_expires/0,
              interval/0,
              refresher/0,
              raw/0
             ]).

%%====================================================================
%% Types
%%====================================================================

-record(session_expires, {interval :: interval(),
                          hparams  :: ersip_hparams:hparams()}).

-opaque session_expires() :: #session_expires{}.

-type interval()  :: pos_integer().  % MUST NOT be less than 90 seconds
-type refresher() :: uac | uas.

-type raw() :: #{interval  := interval(),
                 refresher => refresher(),
                 params    => ersip_hparams:raw()
                }.

%%====================================================================
%% API
%%====================================================================

-spec interval(session_expires()) -> interval().
interval(#session_expires{interval = Interval}) ->
    Interval.

-spec refresher(session_expires()) -> refresher() | undefined.
refresher(#session_expires{hparams = HParams}) ->
    case ersip_hparams:find(refresher, HParams) of
        {ok, Refresher} ->
            Refresher;
        not_found ->
            undefined
    end.

-spec set_refresher(refresher(), session_expires()) -> session_expires().
set_refresher(uac, #session_expires{} = SE) ->
    set_param(<<"refresher">>, <<"uac">>, SE);
set_refresher(uas, #session_expires{} = SE) ->
    set_param(<<"refresher">>, <<"uas">>, SE).

-spec param(binary(), session_expires()) -> {ok, binary()} | not_found.
param(Name, #session_expires{hparams = HParams}) ->
    ersip_hparams:find_raw(Name, HParams).

-spec set_param(binary(), binary(), session_expires()) -> session_expires().
set_param(Name, Value, #session_expires{hparams = HParams0} = SE) ->
    case ersip_hparams:set(Name, Value, fun parse_known/2, HParams0) of
        {ok, HParams} ->
            SE#session_expires{hparams = HParams};
        {error, Reason} ->
            error(Reason)
    end.

-spec make(binary()) -> session_expires().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, SE} ->
            SE;
        {error, Reason} ->
            error(Reason)
    end;
make(#{interval := Interval} = Raw) when is_integer(Interval), Interval >= 90 ->
    HParams = ersip_hparams:make(maps:get(params, Raw, #{})),
    SE0 = #session_expires{interval = Interval, hparams = HParams},
    case maps:find(refresher, Raw) of
        {ok, Refresher} ->
            set_refresher(Refresher, SE0);
        error ->
            SE0
    end.

-spec parse(binary()) -> {ok, session_expires()} | {error, term()}.
parse(Bin) ->
    Parsers = [fun parse_interval/1, fun parse_params/1],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [Interval, HParams], <<>>} ->
            SE = #session_expires{interval = Interval, hparams = HParams},
            {ok, SE};
        {ok, _Int, Rest} ->
            {error, {invalid_session_expires, {garbage_at_the_end, Rest}}};
        {error, Reason} ->
            {error, {invalid_session_expires, Reason}}
    end.

-spec assemble(session_expires()) -> iolist().
assemble(#session_expires{interval = Interval, hparams = HParams}) ->
    Params =
        case ersip_hparams:is_empty(HParams) of
            false ->
                [$; | ersip_hparams:assemble(HParams)];
            true ->
                []
        end,
    [integer_to_binary(Interval), Params].

-spec assemble_bin(session_expires()) -> binary().
assemble_bin(#session_expires{} = SE) ->
    iolist_to_binary(assemble(SE)).

-spec parse_interval(binary()) -> ersip_parser_aux:parse_result(interval()).
parse_interval(Bin) ->
    case ersip_parser_aux:parse_non_neg_int(Bin) of
        {ok, Int, Rest} when Int >= 90 ->
            {ok, Int, Rest};
        {ok, Int, _Rest} ->
            {error, {invalid_interval, Int}};
        {error, Reason} ->
            {error, {invalid_interval, Reason}}
    end.

-spec parse_params(binary()) -> ersip_parser_aux:parse_result(ersip_hparams:hparams()).
parse_params(<<$;, Bin/binary>>) ->
    ersip_hparams:parse(fun parse_known/2, Bin);
parse_params(Bin) ->
    {ok, ersip_hparams:new(), Bin}.

-spec parse_known(binary(), binary()) -> ersip_hparams:parse_known_fun_result().
parse_known(<<"refresher">>, <<"uac">>) ->
    {ok, {refresher, uac}};
parse_known(<<"refresher">>, <<"uas">>) ->
    {ok, {refresher, uas}};
parse_known(<<"refresher">>, Value) ->
    {error, {invalid_refresher, Value}};
parse_known(_Name, _Value) ->
    {ok, unknown}.
