%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Min-SE Header Field tests
%%%

-module(ersip_hdr_session_expires_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Cases
%%====================================================================

parse_interval_test() ->
    SE = ersip_hdr_session_expires:make(<<"1800">>),
    ?assertEqual(1800, ersip_hdr_session_expires:interval(SE)).

parse_refresher_test_() ->
    [fun() ->
             SE = ersip_hdr_session_expires:make(FieldValue),
             ?assertEqual(MaybeRefresher, ersip_hdr_session_expires:refresher(SE))
     end
     || {MaybeRefresher, FieldValue} <-
            [{undefined, <<"1800">>},
             {uac,       <<"1800;refresher=uac">>},
             {uas,       <<"1800;refresher=uas">>}
            ]].

set_refresher_test() ->
    {with, ersip_hdr_session_expires:make(<<"1800">>),
     [fun(SE0) ->
              SE1 = ersip_hdr_session_expires:set_refresher(Refresher, SE0),
              ?assertEqual(Refresher, ersip_hdr_session_expires:refresher(SE1))
      end
      || Refresher <- [uac, uas]]}.

parse_param_test() ->
    SE = ersip_hdr_session_expires:make(<<"1800;p=v">>),
    ?assertEqual({ok, <<"v">>}, ersip_hdr_session_expires:param(<<"p">>, SE)).

set_param_test() ->
    SE0 = ersip_hdr_session_expires:make(<<"1800">>),
    SE1 = ersip_hdr_session_expires:set_param(<<"p">>, <<"v">>, SE0),
    ?assertEqual({ok, <<"v">>}, ersip_hdr_session_expires:param(<<"p">>, SE1)).

set_param_failed_test() ->
    SE0 = ersip_hdr_session_expires:make(<<"1800">>),
    ?assertError({invalid_refresher, _},
                 ersip_hdr_session_expires:set_param(<<"refresher">>, <<"x">>, SE0)).

make_failed_test() ->
    ?assertError({invalid_session_expires, _},
                 ersip_hdr_session_expires:make(<<"x">>)).

parse_failed_test_() ->
    [{Comment,
      ?_assertMatch({error, {invalid_session_expires, _}},
                    ersip_hdr_session_expires:parse(FieldValue))}
     || {Comment, FieldValue} <-
            [{"invalid number",     <<"x">>},
             {"garbage at the end", <<"1800x">>},
             {"too small interval", <<"64">>},
             {"invalid params",     <<"1800;">>},
             {"invalid refresher",  <<"1800;refresher=x">>}
            ]].

assemble_test() ->
    SE = ersip_hdr_session_expires:make(<<"1800">>),
    ?assertEqual(<<"1800">>, ersip_hdr_session_expires:assemble_bin(SE)).

assemble_with_params_test() ->
    SE = ersip_hdr_session_expires:make(<<"1800;refresher=uac">>),
    ?assertEqual(<<"1800;refresher=uac">>, ersip_hdr_session_expires:assemble_bin(SE)).

make_from_raw_test() ->
    SE = ersip_hdr_session_expires:make(#{interval => 1800, refresher => uac, params => #{<<"p">> => <<"v">>}}),
    ?assertEqual(1800, ersip_hdr_session_expires:interval(SE)),
    ?assertEqual(uac, ersip_hdr_session_expires:refresher(SE)),
    ?assertEqual({ok, <<"v">>}, ersip_hdr_session_expires:param(<<"p">>, SE)).
