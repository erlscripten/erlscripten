-module(binaries).
-erlscripten_output(autogenerated_transpilation_tests).
-compile({parse_transform, erlps_parse_transform}).
-compile(nowarn).
-compile(export_all).

test_build_empty() ->
    <<>>.
test_build_single() ->
    <<1>>.
test_build_multi() ->
    <<1,2,3>>.
test_build_string() ->
    <<"XD">>.
test_build_bin() ->
    <<<<1,2,3>>/binary>>.
test_build_float() ->
    <<1.0/float>>.
test_build_mixed() ->
    <<"XD",1,2,<<3,4>>/binary,5/float>>.

test_build_int_size() ->
    <<1:32>>.
test_build_int_size_unit() ->
    <<1:4/unit:8>>.
test_build_int_endian_little() ->
    <<1:32/little>>.
test_build_int_underflow() ->
    <<-1>>.
test_build_int_overflow() ->
    <<256>>.

test_build_string_size_unit() ->
    <<"XD":2/unit:8>>.

test_build_bin_size() ->
    <<<<1,2,3>>:2/binary>>.
test_build_bin_unit() ->
    <<<<1,2,3>>:16/binary-unit:1>>.

test_build_float_32() ->
    <<1:32/float>>.
test_build_float_32_unit() ->
    <<1:4/float-unit:8>>.
test_build_float_little() ->
    <<1/float-little>>.

test_build_comprehension() ->
    << <<X, Y:16>> || X <- [1,2,3], Y <- [4] >>.

add_two_numbers(A, B) ->
    A + B.

test_match_empty() ->
    <<>> = <<>>,
    ok.

test_match_single() ->
    <<1>> = <<1>>,
    ok.

test_match_single_var() ->
    <<X>> = <<1>>,
    X = 1,
    ok.

test_match_many() ->
    <<X,2,Y,4>> = <<1,2,3,4>>,
    X = 1,
    Y = 3,
    ok.

test_match_int_size() ->
    S = add_two_numbers(8, 16),
    <<X:S>> = <<1,2,3>>,
    X = 66051,
    ok.

test_match_int_size_unit() ->
    S = add_two_numbers(1, 2),
    <<X:S/unit:8>> = <<1,2,3>>,
    X = 66051,
    ok.

test_match_int_little() ->
    <<X:16/little>> = <<1:16>>,
    X = 256,
    ok.

test_match_int_signed() ->
    <<X/signed>> = <<128>>,
    X = -128,
    ok.

test_match_int_mix() ->
    <<X:3/unit:8-signed-little, Y, 9775:16/big, Z/unsigned>> =
        <<735859:4/unit:8, 2502467:3/unit:8>>,
    X = 3803904,
    Y = 115,
    Z = 67,
    ok.

test_match_string() ->
    <<X, "XD", Y>> = <<1, 88, 68, 2>>,
    X = 1,
    Y = 2,
    ok.

test_match_bin() ->
    <<B1:3/binary, X, B2:4/binary>> =
        <<28666836:4/unit:8, 356327667:4/unit:8>>,
    B1 = <<1,181,107>>,
    X = 212,
    B2 = <<21,61,32,243>>,
    ok.

test_match_bin_end() ->
    <<1,2,X, B/binary>> = <<1,2,3,4,5,6,7,8>>,
    X = 3,
    B = <<4,5,6,7,8>>,
    ok.

test_match_float_64() ->
    <<F/float>> = <<123/float>>,
    F = 123.0,
    ok.

test_match_float_32() ->
    <<F:32/float>> = <<123:32/float>>,
    F = 123.0,
    ok.

test_match_wildcard_binary() ->
    T = [<<>>, <<1>>, <<1,2>>, <<1,2,3>>, <<1,2,3,4>>, <<1,2,3,4,5>>],
    [<<_/binary>> = X || X <- T],
    [<<_/bitstring>> = X || X <- T],
    ok.

test_match_padded_bitstring() ->
    <<C:8/bitstring>> = <<1>>,
    C = <<1>>,

    <<D:32/bitstring, R/binary>> = <<1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2>>,
    D = <<1,2,3,4>>,
    R = <<5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2>>,
    ok.

check(F, What, Expected) ->
    try
        Expected = F()
    catch
        _:X ->
            io:format(user, "Failed: ~p ~p", [What, X]),
            io:format("\n"),
            put(failed, true)
    end.

test_cases_from_erl_eval() ->
    put(failed, false),
    io:format(user, "Running tests from erl_eval"),
    check(fun() -> <<"abc">> = <<"abc">> end,
	  "<<\"abc\">> = <<\"abc\">>. ",
	  <<"abc">>),
    check(fun() ->
		  <<Size,B:Size/binary,Rest/binary>> = <<2,"AB","CD">>,
		  {Size,B,Rest}
	  end,
	  "begin <<Size,B:Size/binary,Rest/binary>> = <<2,\"AB\",\"CD\">>, "
	  "{Size,B,Rest} end. ",
	  {2,<<"AB">>,<<"CD">>}),

    check(fun() -> if <<"hej">> == <<"hopp">> -> true;
		      true -> false end end,
	  "begin if <<\"hej\">> == <<\"hopp\">> -> true;
                          true -> false end end.",
                false),
    check(fun() -> if <<"hej">> == <<"hej">> -> true;
		      true -> false end end,
	  "begin if <<\"hej\">> == <<\"hej\">> -> true;
                          true -> false end end.",
                true),

    %% FIXME: FAILS
    check(fun() -> X = 32, [X || X <- [1,2,3]] end,
	  "begin X = 32, [X || X <- [1,2,3]] end.",
	  [1,2,3]),

    %% FIXME: FAILS
    check(fun() -> X = 32,
		   [X || <<X:X>> <- [<<1:32>>,<<2:32>>,<<3:8>>]] end,
	  % "binsize variable"          ^
	  "begin X = 32,
                 [X || <<X:X>> <- [<<1:32>>,<<2:32>>,<<3:8>>]] end.",
                [1,2]),

    %% FIXME: FAILS
    check(fun() -> Y = 13,[X || {X,Y} <- [{1,2}]] end,
	  "begin Y = 13,[X || {X,Y} <- [{1,2}]] end.",
	  [1]),

    check(fun() -> [X || X <- [1,2,3,4], not (X < 2)] end,
	  "begin [X || X <- [1,2,3,4], not (X < 2)] end.",
	  [2,3,4]),
    check(fun() -> [X || X <- [true,false], X] end,
	  "[X || X <- [true,false], X].", [true]),

    check(fun() -> <<(3.0+2.0):32/float>> = <<5.0:32/float>> end,
	  "<<(3.0+2.0):32/float>> = <<5.0:32/float>>.",
	  <<5.0:32/float>>),

    check(fun() -> if is_binary(<<>>) -> true; true -> false end end,
	  "if is_binary(<<>>) -> true; true -> false end.", true),
    check(fun() -> if binary(<<>>) -> true; true -> false end end,
	  "if binary(<<>>) -> true; true -> false end.", true),

    check(fun() -> L = 8,
                     F = fun(<<A:L,B:A>>) -> B end,
                     F(<<16:8, 7:16>>)
            end,
            "begin
               L = 8, F = fun(<<A:L,B:A>>) -> B end, F(<<16:8, 7:16>>)
             end.",
            7),

    check(fun() -> L = 8,
                     F = fun(<<L:L,B:L>>) -> B end,
                     F(<<16:8, 7:16>>)
            end,
            "begin
               L = 8, F = fun(<<L:L,B:L>>) -> B end, F(<<16:8, 7:16>>)
             end.",
            7),

    check(fun() -> L = 8, <<A:L,B:A>> = <<16:8, 7:16>>, B end,
            "begin L = 8, <<A:L,B:A>> = <<16:8, 7:16>>, B end.",
            7),

    check(fun() -> U = 8, (fun(<<U:U>>) -> U end)(<<32:8>>) end,
                "begin U = 8, (fun(<<U:U>>) -> U end)(<<32:8>>) end.",
                32),
    % FIXME: FAILS
    check(fun() -> U = 8, [U || <<U:U>> <- [<<32:8>>]] end,
                "begin U = 8, [U || <<U:U>> <- [<<32:8>>]] end.",
                [32]),

    check(fun() -> [X || <<A:8,
                             B:A>> <- [<<16:8,19:16>>],
                           <<X:8>> <- [<<B:8>>]] end,
            "[X || <<A:8,
                             B:A>> <- [<<16:8,19:16>>],
                           <<X:8>> <- [<<B:8>>]].",
            [19]),

    check(fun() ->
                    << <<X>> || <<X>> <- [1,2,3] >>
            end,
            "<< <<X>> || <<X>> <- [1,2,3] >>.",
            <<>>),
    check(fun() ->
                        << <<X>> || X <- [1,2,3] >>
                end,
                "<< <<X>> || X <- [1,2,3] >>.",
                <<1,2,3>>),
    check(fun() -> <<3.14:32/float-big>> end,
                "<<3.14:32/float-big>>.",
                <<64,72,245,195>>),
    check(fun() -> <<3.14:32/float-little>> end,
                "<<3.14:32/float-little>>.",
                <<195,245,72,64>>),
    check(fun() -> <<X:32/float-big>> = <<2.0:32/float-big>>, X end,
                "begin <<X:32/float-big>> = <<2.0:32/float-big>>,
                        X end.",
                2.0),
    check(fun() -> <<X:32/float-little>> = <<2.0:32/float-little>>,
                         X end,
                "begin <<X:32/float-little>> = <<2.0:32/float-little>>,
                        X end.",
                2.0),
    check(fun() -> <<X:32/float-native>> = <<2.0:32/float-native>>,
                         X end,
                "begin <<X:32/float-native>> = <<2.0:32/float-native>>,
                        X end.",
                2.0),

   %%-----------------------------------------------------------
    % <FAILS_TO_COMPILE>
    %check(
    %        fun() ->
    %                [X || <<"hej",X:8>> <= <<"hej",8,"san",9,"hej",17,"hej">>]
    %        end,
    %        "[X || <<\"hej\",X:8>> <=
    %                    <<\"hej\",8,\"san\",9,\"hej\",17,\"hej\">>].",
    %        [8,17]),

    %check(
    %      fun() ->
    %              L = 8, << <<B:32>> || <<L:L,B:L>> <= <<16:8, 7:16>> >>
    %      end,
    %      "begin L = 8, << <<B:32>> || <<L:L,B:L>> <= <<16:8, 7:16>> >>
    %       end.",
    %      <<0,0,0,7>>),

    %check(fun() -> [ 3 || <<17/float>> <= <<17.0/float>>] end,
    %            "[ 3 || <<17/float>> <= <<17.0/float>>].",
    %            [3]),
    %check(fun() -> [ 3 || <<17/float>> <- [<<17.0/float>>]] end,
    %            "[ 3 || <<17/float>> <- [<<17.0/float>>]].",
    %            [3]),
    %check(fun() -> [ X || <<17/float,X:3>> <= <<17.0/float,2:3>>] end,
    %            "[ X || <<17/float,X:3>> <= <<17.0/float,2:3>>].",
    %            [2]),
    %check(fun() ->
    %             [ foo || <<(1 bsl 1023)/float>> <= <<(1 bsl 1023)/float>>]
    %            end,
    %            "[ foo || <<(1 bsl 1023)/float>> <= <<(1 bsl 1023)/float>>].",
    %            [foo]),
    %check(fun() ->
    %             [ foo || <<(1 bsl 1023)/float>> <- [<<(1 bsl 1023)/float>>]]
    %            end,
    %           "[ foo || <<(1 bsl 1023)/float>> <- [<<(1 bsl 1023)/float>>]].",
    %            [foo]),

    %check(fun() ->
    %             [ foo || <<(1 bsl 1024)/float>> <- [<<(1 bsl 1023)/float>>]]
    %            end,
    %            "[ foo || <<(1 bsl 1024)/float>> <-
    %                        [<<(1 bsl 1023)/float>>]].",
    %            []),
    %check(fun() ->
    %             [ foo || <<(1 bsl 1024)/float>> <= <<(1 bsl 1023)/float>>]
    %            end,
    %            "[ foo || <<(1 bsl 1024)/float>> <=
    %                        <<(1 bsl 1023)/float>>].",
    %            []),
    %check(fun() ->
    %                    L = 8,
    %                    [{L,B} || <<L:L,B:L/float>> <= <<32:8,7:32/float>>]
    %            end,
    %            "begin L = 8,
    %                   [{L,B} || <<L:L,B:L/float>> <= <<32:8,7:32/float>>]
    %             end.",
    %            [{32,7.0}]),

    %check(fun() ->
    %                  [foo || <<"s">> <= <<"st">>]
    %          end,
    %          "[foo || <<\"s\">> <= <<\"st\">>].",
    %          [foo]),

    %check(fun() -> [foo || <<_:32>> <= <<17:32,20:32>>] end,
    %            "[foo || <<_:32>> <= <<17:32,20:32>>].",
    %            [foo,foo]),
    % </FAILS_TO_COMPILE>
   %%-----------------------------------------------------------

    check(fun() -> <<_:32>> = <<17:32>> end,
              "<<_:32>> = <<17:32>>.",
              <<17:32>>),

    check(fun() -> << <<X:32>> || X <- [1,2,3], X > 1 >> end,
                "<< <<X:32>> || X <- [1,2,3], X > 1 >>.",
                <<0,0,0,2,0,0,0,3>>),

    check(
            fun() -> <<16:(1024*1024)>> = <<16:(1024*1024)>> end,
            "<<16:(1024*1024)>> = <<16:(1024*1024)>>.",
            <<16:1048576>>),

  %% UTF-8.
    %check(
	  %  fun() -> <<65>> = <<65/utf8>> end,
	  %  "<<65>> = <<65/utf8>>.",
	  %  <<65>>),
%% FIXME: FAILS
    %check(
	  %  fun() -> <<350/utf8>> = <<197,158>> end,
	  %  "<<350/utf8>> = <<197,158>>.",
	  %  <<197,158>>),
%% FIXME: FAILS
    %check(
	  %  fun() -> <<$b,$j,$\303,$\266,$r,$n>> = <<"bj\366rn"/utf8>> end,
	  %  "<<$b,$j,$\303,$\266,$r,$n>> = <<\"bj\366rn\"/utf8>>.",
	  %  <<$b,$j,$\303,$\266,$r,$n>>),

    %% UTF-16.
    %check(
	  %  fun() -> <<0,65>> = <<65/utf16>> end,
	  %  "<<0,65>> = <<65/utf16>>.",
	  %  <<0,65>>),
    %check(
	  %  fun() -> <<16#D8,16#08,16#DF,16#45>> = <<16#12345/utf16>> end,
	  %  "<<16#D8,16#08,16#DF,16#45>> = <<16#12345/utf16>>.",
	  %  <<16#D8,16#08,16#DF,16#45>>),
    %check(
	  %  fun() -> <<16#08,16#D8,16#45,16#DF>> = <<16#12345/little-utf16>> end,
	  %  "<<16#08,16#D8,16#45,16#DF>> = <<16#12345/little-utf16>>.",
	  %  <<16#08,16#D8,16#45,16#DF>>),

    %check(
	  %  fun() -> <<350/utf16>> = <<1,94>> end,
	  %  "<<350/utf16>> = <<1,94>>.",
	  %  <<1,94>>),
    %check(
	  %  fun() -> <<350/little-utf16>> = <<94,1>> end,
	  %  "<<350/little-utf16>> = <<94,1>>.",
	  %  <<94,1>>),
    %check(
	  %  fun() -> <<16#12345/utf16>> = <<16#D8,16#08,16#DF,16#45>> end,
	  %  "<<16#12345/utf16>> = <<16#D8,16#08,16#DF,16#45>>.",
	  %  <<16#D8,16#08,16#DF,16#45>>),
    %check(
	  %  fun() -> <<16#12345/little-utf16>> = <<16#08,16#D8,16#45,16#DF>> end,
	  %  "<<16#12345/little-utf16>> = <<16#08,16#D8,16#45,16#DF>>.",
	  %  <<16#08,16#D8,16#45,16#DF>>),

    %% UTF-32.
    %check(
	  %  fun() -> <<16#12345/utf32>> = <<16#0,16#01,16#23,16#45>> end,
	  %  "<<16#12345/utf32>> = <<16#0,16#01,16#23,16#45>>.",
	  %  <<16#0,16#01,16#23,16#45>>),
    %check(
	  %  fun() -> <<16#0,16#01,16#23,16#45>> = <<16#12345/utf32>> end,
	  %  "<<16#0,16#01,16#23,16#45>> = <<16#12345/utf32>>.",
	  %  <<16#0,16#01,16#23,16#45>>),
    %check(
	  %  fun() -> <<16#12345/little-utf32>> = <<16#45,16#23,16#01,16#00>> end,
	  %  "<<16#12345/little-utf32>> = <<16#45,16#23,16#01,16#00>>.",
	  %  <<16#45,16#23,16#01,16#00>>),
    %check(
	  %  fun() -> <<16#12345/little-utf32>> end,
	  %  "<<16#12345/little-utf32>>.",
	  %  <<16#45,16#23,16#01,16#00>>),

    %% Mixed.
    %% FIXME: FAILS
    %check(
	  %  fun() -> <<16#41,16#12345/utf32,16#0391:16,16#2E:8>> end,
	  %  "<<16#41,16#12345/utf32,16#0391:16,16#2E:8>>.",
	  %  <<16#41,16#00,16#01,16#23,16#45,16#03,16#91,16#2E>>),

    check(
            fun() ->
                  E = fun(N) ->
                              if
                                  is_integer(N) -> <<N/integer>>;
                                  true -> throw(foo)
                              end
                      end,
                  try << << (E(V))/binary >> || V <- [1,2,3,a] >>
                  catch foo -> ok
                  end
            end,
            "begin
                 E = fun(N) ->
                            if is_integer(N) -> <<N/integer>>;
                               true -> throw(foo)
                            end
                     end,
                 try << << (E(V))/binary >> || V <- [1,2,3,a] >>
                 catch foo -> ok
                 end
             end.",
            ok),
    check(
            fun() ->
                  E = fun(N) ->
                              if
                                  is_integer(N) -> <<N/integer>>;

                                  true -> erlang:error(foo)
                              end
                      end,
                  try << << (E(V))/binary >> || V <- [1,2,3,a] >>
                  catch error:foo -> ok
                  end
            end,
            "begin
                 E = fun(N) ->
                            if is_integer(N) -> <<N/integer>>;
                               true -> erlang:error(foo)
                            end
                     end,
                 try << << (E(V))/binary >> || V <- [1,2,3,a] >>
                 catch error:foo -> ok
                 end
             end.",
            ok),

    check(fun() ->
			{'EXIT',{badarg,_}} = (catch <<not_a_number:0>>),
			ok
		end, "begin {'EXIT',{badarg,_}} = (catch <<not_a_number:0>>), "
		"ok end.", ok),

    check(fun() -> if erlang:'+'(3,a) -> true ; true -> false end end,
	  "if erlang:'+'(3,a) -> true ; true -> false end.",
	  false),
    check(fun() -> if erlang:is_integer(3) -> true ; true -> false end
	  end,
	  "if erlang:is_integer(3) -> true ; true -> false end.",
	  true),
    check(fun() -> [X || X <- [1,2,3], erlang:is_integer(X)] end,
	  "[X || X <- [1,2,3], erlang:is_integer(X)].",
	  [1,2,3]),
    check(fun() -> if is_atom(is_integer(a)) -> true ; true -> false end
	  end,
	  "if is_atom(is_integer(a)) -> true ; true -> false end.",
	  true),
    check(fun() -> if erlang:is_atom(erlang:is_integer(a)) -> true;
		      true -> false end end,
	  "if erlang:is_atom(erlang:is_integer(a)) -> true; "
	  "true -> false end.",
	  true),
    check(fun() -> if is_atom(3+a) -> true ; true -> false end end,
	  "if is_atom(3+a) -> true ; true -> false end.",
	  false),
    check(fun() -> if erlang:is_atom(3+a) -> true ; true -> false end
	  end,
	  "if erlang:is_atom(3+a) -> true ; true -> false end.",
	  false),

    check(fun() -> case 4 of 2+2 -> ok end end,
	  "case 4 of 2+2 -> ok end. ",
	  ok),
    check(fun() -> case 2 of +2 -> ok end end,
	  "case 2 of +2 -> ok end. ",
	  ok),

    check(fun() -> case {a, b} of {a, _X}=Y -> {x,Y} end end,
	  "case {a, b} of {a, X}=Y -> {x,Y} end. ",
	  {x, {a, b}}),
    check(fun() -> case {a, b} of Y={a, _X} -> {x,Y} end end,
	  "case {a, b} of Y={a, X} -> {x,Y} end. ",
	  {x, {a, b}}),
    check(fun() -> case {a, b} of Y={a, _X}=Z -> {Z,Y} end end,
	  "case {a, b} of Y={a, X}=Z -> {Z,Y} end. ",
	  {{a, b}, {a, b}}),
    check(fun() -> A = 4, B = 28, <<13:(A+(X=B))>>, X end,
	  "begin A = 4, B = 28, <<13:(A+(X=B))>>, X end.",
	  28),

    check(fun() -> case "abc" of "ab" ++ L -> L end end,
	  "case \"abc\" of \"ab\" ++ L -> L end. ",
	  "c"),
    check(fun() -> case "abcde" of "ab" ++ "cd" ++ L -> L end end,
	  "case \"abcde\" of \"ab\" ++ \"cd\" ++ L -> L end. ",
	  "e"),
    check(fun() -> case "abc" of [97, 98] ++ L -> L end end,
	  "case \"abc\" of [97, 98] ++ L -> L end. ",
	  "c"),


    check(fun() -> false andalso kludd end, "false andalso kludd.",
	  false),
    check(fun() -> true andalso true end, "true andalso true.",
	  true),
    check(fun() -> true andalso false end, "true andalso false.",
	  false),
    check(fun() -> true andalso kludd end, "true andalso kludd.",
	  kludd),

    check(fun() -> if false andalso kludd -> a; true -> b end end,
	  "if false andalso kludd -> a; true -> b end.",
	  b),
    check(fun() -> if true andalso true -> a; true -> b end end,
	  "if true andalso true -> a; true -> b end.",
	  a),
    check(fun() -> if true andalso false -> a; true -> b end end,
	  "if true andalso false -> a; true -> b end.",
	  b),

    check(fun() -> true orelse kludd end,
	  "true orelse kludd.", true),
    check(fun() -> false orelse false end,
	  "false orelse false.", false),
    check(fun() -> false orelse true end,
	  "false orelse true.", true),
    check(fun() -> false orelse kludd end,
	  "false orelse kludd.", kludd),

    check(fun() -> if true orelse kludd -> a; true -> b end end,
	  "if true orelse kludd -> a; true -> b end.", a),
    check(fun() -> if false orelse false -> a; true -> b end end,
	  "if false orelse false -> a; true -> b end.", b),
    check(fun() -> if false orelse true -> a; true -> b end end,
	  "if false orelse true -> a; true -> b end.", a),

    check(fun() -> [X || X <- [1,2,3], X+2] end,
	  "[X || X <- [1,2,3], X+2].", []),

    check(fun() -> [X || X <- [1,2,3], [X] == [X || X <- [2]]] end,
	  "[X || X <- [1,2,3], [X] == [X || X <- [2]]].",
	  [2]),
    check(fun() -> F = fun(1) -> ett; (2) -> zwei end,
		   ett = F(1), zwei = F(2) end,
	  "begin F = fun(1) -> ett; (2) -> zwei end,
                         ett = F(1), zwei = F(2) end.",
                zwei),
    check(fun() -> F = fun(X) when X == 1 -> ett;
			  (X) when X == 2 -> zwei end,
		   ett = F(1), zwei = F(2) end,
	  "begin F = fun(X) when X == 1 -> ett;
                              (X) when X == 2 -> zwei end,
	  ett = F(1), zwei = F(2) end.",
                zwei),
    check(fun() -> if length([1]) == 1 -> yes;
		      true -> no end end,
	  "if length([1]) == 1 -> yes;
                            true -> no end.",
                yes),
    check(fun() -> if is_integer(3) -> true; true -> false end end,
	  "if is_integer(3) -> true; true -> false end.", true),
    check(fun() -> if integer(3) -> true; true -> false end end,
	  "if integer(3) -> true; true -> false end.", true),
    check(fun() -> if is_float(3) -> true; true -> false end end,
	  "if is_float(3) -> true; true -> false end.", false),
    check(fun() -> if float(3) -> true; true -> false end end,
	  "if float(3) -> true; true -> false end.", false),
    check(fun() -> if is_number(3) -> true; true -> false end end,
	  "if is_number(3) -> true; true -> false end.", true),
    check(fun() -> if number(3) -> true; true -> false end end,
	  "if number(3) -> true; true -> false end.", true),
    check(fun() -> if is_atom(a) -> true; true -> false end end,
	  "if is_atom(a) -> true; true -> false end.", true),
    check(fun() -> if atom(a) -> true; true -> false end end,
	  "if atom(a) -> true; true -> false end.", true),
    check(fun() -> if is_list([]) -> true; true -> false end end,
	  "if is_list([]) -> true; true -> false end.", true),
    check(fun() -> if list([]) -> true; true -> false end end,
	  "if list([]) -> true; true -> false end.", true),
    check(fun() -> if is_tuple({}) -> true; true -> false end end,
	  "if is_tuple({}) -> true; true -> false end.", true),
    check(fun() -> if tuple({}) -> true; true -> false end end,
	  "if tuple({}) -> true; true -> false end.", true),
    check(fun() -> if is_pid(self()) -> true; true -> false end end,
	  "if is_pid(self()) -> true; true -> false end.", true),
    check(fun() -> if pid(self()) -> true; true -> false end end,
	  "if pid(self()) -> true; true -> false end.", true),
    check(fun() -> R = make_ref(), if is_reference(R) -> true;
				      true -> false end end,
	  "begin R = make_ref(), if is_reference(R) -> true;"
	  "true -> false end end.", true),
    check(fun() -> R = make_ref(), if reference(R) -> true;
				      true -> false end end,
	  "begin R = make_ref(), if reference(R) -> true;"
	  "true -> false end end.", true),
    check(fun() -> if is_port(a) -> true; true -> false end end,
	  "if is_port(a) -> true; true -> false end.", false),
    check(fun() -> if port(a) -> true; true -> false end end,
	  "if port(a) -> true; true -> false end.", false),
    check(fun() -> if is_function(a) -> true; true -> false end end,
	  "if is_function(a) -> true; true -> false end.", false),
    check(fun() -> if function(a) -> true; true -> false end end,
	  "if function(a) -> true; true -> false end.", false),
    check(fun() -> if is_binary(<<>>) -> true; true -> false end end,
	  "if is_binary(<<>>) -> true; true -> false end.", true),
    check(fun() -> if binary(<<>>) -> true; true -> false end end,
	  "if binary(<<>>) -> true; true -> false end.", true),
    check(fun() -> if is_integer(a) == true -> yes;
		      true -> no end end,
	  "if is_integer(a) == true -> yes;
                            true -> no end.",
                no),
    check(fun() -> if [] -> true; true -> false end end,
	  "if [] -> true; true -> false end.", false),
    check(fun() -> if a+b -> true; true -> false end end,
	  "if a + b -> true; true -> false end.", false),
    check(fun() -> if + b -> true; true -> false end end,
	  "if + b -> true; true -> false end.", false),
    check(fun() -> case a of
		       X when X == b -> one;
		       X when X == a -> two
		   end end,
	  "begin case a of
                             X when X == b -> one;
	      X when X == a -> two
	 end end.", two),

	  check(fun() -> $c = $c end, "$c = $c.", $c),
	  check(fun() -> _ = bar end, "_ = bar.", bar),
	  check(fun() -> A = 14, A = 14 end,
                "begin A = 14, A = 14 end.", 14),
	  check(fun() -> "hej" = "hej" end,
                "\"hej\" = \"hej\".", "hej"),
	  check(fun() -> [] = [] end, "[] = [].", []),
	  check(fun() -> trunc((1 * 3 div 3 + 4 - 3) / 1) rem 2 end,
                "begin trunc((1 * 3 div 3 + 4 - 3) / 1) rem 2 end.", 0),
	  check(fun() -> (2#101 band 2#10101) bor (2#110 bxor 2#010) end,
                "(2#101 band 2#10101) bor (2#110 bxor 2#010).", 5),
	  check(fun() -> (2#1 bsl 4) + (2#10000 bsr 3) end,
                "(2#1 bsl 4) + (2#10000 bsr 3).", 18),
	  %% TODO: Fixme
    %%check(fun() -> ((1<3) and ((1 =:= 2) or (1 =/= 2))) xor (1=<2) end,
    %%            "((1<3) and ((1 =:= 2) or (1 =/= 2))) xor (1=<2).", false),
	  check(fun() -> (a /= b) or (2 > 4) or (3 >= 3) end,
                "(a /= b) or (2 > 4) or (3 >= 3).", true),
	  check(fun() -> "hej" ++ "san" =/= "hejsan" -- "san" end,
                "\"hej\" ++ \"san\" =/= \"hejsan\" -- \"san\".", true),
	  check(fun() -> (bnot 1) < -0 end, "(bnot (+1)) < -0.", true),

    case get(failed) of
      true -> exit(failed);
      _ -> ok
    end.
