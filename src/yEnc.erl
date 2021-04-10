-module(yEnc).
-author('dev@sntran.com').

%% API
-export([
  encode/1,
  decode/1,
  post/2
]).

-export_type([

]).

-include("yEnc.hrl").
-include("yEnc_internal.hrl").

%% ==================================================================
%% API
%% ==================================================================

%%-------------------------------------------------------------------
%% @doc Performs raw yEnc encoding on data returning the result.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(binary()) -> binary().
encode(<<>> = Data) -> Data.

%%-------------------------------------------------------------------
%% @doc Performs raw yEnc decoding on data returning the result.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(binary()) -> binary().
decode(<<>> = Data) -> Data.

%%-------------------------------------------------------------------
%% @doc Returns a single yEnc encoded post, suitable for posting.
%%
%% @end
%%-------------------------------------------------------------------
-spec post(file:name(), binary()) -> binary().
post(Filename, Data) ->
  Size = integer_to_binary(byte_size(Data)),
  Header = <<"=ybegin line=128 size=", Size/binary, " name=", Filename/binary, "\r\n">>,
  Encoded = encode(Data),
  CRC32 = crc32(Data),
  Footer = <<"\r\n=yend size=", Size/binary, " crc32=", CRC32/binary>>,
  <<Header/binary, Encoded/binary, Footer/binary>>.

%%-------------------------------------------------------------------
%% @doc Calculates hexadecimal CRC32 checksum of data.
%%
%% @end
%%-------------------------------------------------------------------
crc32(<<>>) -> <<"00000000">>;
crc32(Data) ->
  integer_to_binary(erlang:crc32(Data), 16).
