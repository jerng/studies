% note how some arguments require [ char ] 
%   whereas others accept either [ char ] or << bin >>

-module(mainb).
-export([loop/0]).

loop() -> 
inets:start(),

RuntimeIdCandidate = os:getenv("AWS_LAMBDA_RUNTIME_API"),
io:format(<<"\n-\n  RuntimeId : '">>),
if 
not RuntimeIdCandidate -> 
    io:format(standard_error,<<"~s">>,
        [<<"not found by os:getenv(\"AWS_LAMBDA_RUNTIME_API\")\n">>]);
true ->
    io:format(<<"~s~s">>,[RuntimeIdCandidate,<<"'\n">>]),
    { GetReturned, GetData } = httpc:request(get, 
            { [<<"http://">>, RuntimeIdCandidate, <<"/2018-06-01/runtime/invocation/next">>],
            %{<<"http://localhost:8080">>,
                []},
            [],[]),

    case GetReturned of 
    error -> 
        io:format(standard_error,<<"~s">>,[<<"GET failed\n">>]);
    ok -> 
        io:format(<<"GET succeeded\n  InvocationId : ">>),
        % io:format("~p",[GetData]),
        
        InvocationIdCandidate = lists:search(
            fun ({X,_}) -> X=="lambda-runtime-aws-request-id" end,
            element(2,GetData)),
        
        case InvocationIdCandidate of
        false ->
            io:format(standard_error,<<"~s">>,[<<"NO HEADER\n">>]);
        {_,{_,InvocationId}} -> 
            io:format(<<"~s~s~s">>, [ <<"'">>, InvocationId, <<"'\n">> ]),
        
            EventData = element(3,GetData),
            { PostReturned, _ } = httpc:request(post,
                    { [<<"http://">>, RuntimeIdCandidate,<<"/2018-06-01/runtime/invocation/">>, InvocationId,<<"/response">>],
                    %{<<"http://localhost:8080">>,
                        [],
                        %<<"Content-Type: text/plain">>,
                        [<<"text/plain">>],
                        EventData},
                    [],[]
            ),
            case PostReturned of 
            ok -> 
            %    io:format(<<"POST succeeded\n  EVENT_DATA : ">>),
            %    io:format(<<"~p">>,[EventData]); 
                silence;
            error ->
                io:format(standard_error,<<"~s">>,["POST failed\n"]) 
            end
        end
    end
end,
loop()
.
