-module(yEnc).
-author('dev@sntran.com').

%% API
-export([
  encode/1,
  decode/1,
  post/2,
  crc32/1
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
%% Encodes each bytes of data to another byte in the extended ASCII
%% using the formula:
%%
%% O = (I+42) % 256
%%
%% Under special circumstances, a single escape character (ASCII 3Dh,
%% "=") is used to indicate that the following output character is
%% "critical", and requires special handling.
%%
%% Critical characters include the following:
%%
%% ASCII 00h (NULL)
%% ASCII 0Ah (LF)
%% ASCII 0Dh (CR)
%% ASCII 3Dh (=)
%%
%% > ASCII 09h (TAB)  -- removed in version (1.2)
%%
%% A typical encoding process might look something like this:
%%
%%  1. Fetch a character from the input stream.
%%  2. Increment the character's ASCII value by 42, modulo 256
%%  3. If the result is a critical character (as defined in the previous
%%    section), write the escape character to the output stream and increment
%%    character's ASCII value by 64, modulo 256.
%%  4. Output the character to the output stream.
%%  5. Repeat from start.
%%
%% @end
%%-------------------------------------------------------------------
-spec encode(binary()) -> binary().
% Leading TAB (9) or SPACE (32).
% (223 + 42) % 256 = 9.
% (246 + 42) % 256 = 32.
encode(<<Byte, Rest/binary>>) when ?ENCODE(Byte) =:= $\t; ?ENCODE(Byte) =:= $\s ->
  Encoded = ?ESCAPE(?ENCODE(Byte)),
  encode(Rest, <<$=, Encoded>>);
encode(Data) -> encode(Data, <<>>).

% @TODO: use compile time function clauses instead of calculation.
encode(<<>>, Acc) -> Acc;
% When a character, when encoded, becomes critical character, we adds
% 64 to it, modulo 256, and prefix it with the escape character.
encode(<<Byte/integer, Rest/binary>>, Acc) when ?critical(Byte) ->
  Encoded = ?ESCAPE(?ENCODE(Byte)),
  encode(Rest, <<Acc/binary, $=, Encoded>>);
% For other characters, we adds 42, modulo 256.
encode(<<Byte/integer, Rest/binary>>, Acc) ->
  Encoded = ?ENCODE(Byte),
  encode(Rest, <<Acc/binary, Encoded>>).

%%-------------------------------------------------------------------
%% @doc Performs raw yEnc decoding on data returning the result.
%%
%% Decodes each bytes of data to another byte in the extended ASCII
%% using the following process:
%%
%% 1. Fetch a character from the input stream.
%% 2. If the charater is the escape character, skip it, then substract
%%    the next character from 64.
%% 3. If the result is greater than 41, substract 42 from it, else,
%%    add 214 to it.
%% 4. Output the character to the output stream.
%% 5. Repeat from start.
%%
%% @end
%%-------------------------------------------------------------------
-spec decode(binary()) -> binary().
decode(Data) -> decode(Data, <<>>).

decode(<<>>, Acc) -> Acc;
% Special case when the escaped character is the escape character.
% 125 - 64 = 61 = $=.
decode(<<$=, 125, Rest/binary>>, Acc) ->
  Decoded = ?DECODE(125 - 64),
  decode(Rest, <<Acc/binary, Decoded>>);
% Other escaped characters.
decode(<<$=, Byte, Rest/binary>>, Acc) ->
  decode(<<(Byte - 64), Rest/binary>>, Acc);
% Regular character.
decode(<<Byte/integer, Rest/binary>>, Acc) ->
  Decoded = ?DECODE(Byte),
  decode(Rest, <<Acc/binary, Decoded>>).

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
-spec crc32(binary()) -> binary().
crc32(<<>>) -> <<"00000000">>;
crc32(Data) ->
  string:lowercase(
    integer_to_binary(
      erlang:crc32(Data),
      16
    )
  ).
