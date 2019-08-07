%%%-------------------------------------------------------------------
%% @doc Client module for grpc service Ateles.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-08-07T10:09:14+00:00 and should not be modified manually

-module(ateles_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'Ateles').
-define(PROTO_MODULE, 'ateles_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec create_context(ateles_pb:create_context_request()) ->
    {ok, ateles_pb:create_context_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
create_context(Input) ->
    create_context(ctx:new(), Input, #{}).

-spec create_context(ctx:t() | ateles_pb:create_context_request(), ateles_pb:create_context_request() | grpcbox_client:options()) ->
    {ok, ateles_pb:create_context_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
create_context(Ctx, Input) when ?is_ctx(Ctx) ->
    create_context(Ctx, Input, #{});
create_context(Input, Options) ->
    create_context(ctx:new(), Input, Options).

-spec create_context(ctx:t(), ateles_pb:create_context_request(), grpcbox_client:options()) ->
    {ok, ateles_pb:create_context_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
create_context(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/Ateles/CreateContext">>, Input, ?DEF(create_context_request, create_context_response, <<"CreateContextRequest">>), Options).

%% @doc Unary RPC
-spec add_map_funs(ateles_pb:add_map_funs_request()) ->
    {ok, ateles_pb:add_map_funs_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
add_map_funs(Input) ->
    add_map_funs(ctx:new(), Input, #{}).

-spec add_map_funs(ctx:t() | ateles_pb:add_map_funs_request(), ateles_pb:add_map_funs_request() | grpcbox_client:options()) ->
    {ok, ateles_pb:add_map_funs_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
add_map_funs(Ctx, Input) when ?is_ctx(Ctx) ->
    add_map_funs(Ctx, Input, #{});
add_map_funs(Input, Options) ->
    add_map_funs(ctx:new(), Input, Options).

-spec add_map_funs(ctx:t(), ateles_pb:add_map_funs_request(), grpcbox_client:options()) ->
    {ok, ateles_pb:add_map_funs_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
add_map_funs(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/Ateles/AddMapFuns">>, Input, ?DEF(add_map_funs_request, add_map_funs_response, <<"AddMapFunsRequest">>), Options).

%% @doc 
-spec map_docs() ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
map_docs() ->
    map_docs(ctx:new(), #{}).

-spec map_docs(ctx:t() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
map_docs(Ctx) when ?is_ctx(Ctx) ->
    map_docs(Ctx, #{});
map_docs(Options) ->
    map_docs(ctx:new(), Options).

-spec map_docs(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
map_docs(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/Ateles/MapDocs">>, ?DEF(map_docs_request, map_docs_response, <<"MapDocsRequest">>), Options).

%% @doc Unary RPC
-spec reset(ateles_pb:reset_request()) ->
    {ok, ateles_pb:reset_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
reset(Input) ->
    reset(ctx:new(), Input, #{}).

-spec reset(ctx:t() | ateles_pb:reset_request(), ateles_pb:reset_request() | grpcbox_client:options()) ->
    {ok, ateles_pb:reset_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
reset(Ctx, Input) when ?is_ctx(Ctx) ->
    reset(Ctx, Input, #{});
reset(Input, Options) ->
    reset(ctx:new(), Input, Options).

-spec reset(ctx:t(), ateles_pb:reset_request(), grpcbox_client:options()) ->
    {ok, ateles_pb:reset_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
reset(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/Ateles/Reset">>, Input, ?DEF(reset_request, reset_response, <<"ResetRequest">>), Options).

