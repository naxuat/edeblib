-module(edeblib_sums).

-export([
    new/0,
    new/1,
    update/2,
    final/2,
    final/1
]).

-record(edeblib_sums, {
    size :: integer(),
    md5 :: term(),
    sha1 :: term(),
    sha256 :: term()
}).

new() ->
    #edeblib_sums{size=0, md5=crypto:md5_init(), sha1=crypto:sha_init(), sha256=erlsha2:sha256_init()}.

new(Data) when is_binary(Data) ->
    update(new(), Data).

update(#edeblib_sums{size=Size, md5=MD5, sha1=SHA1, sha256=SHA256}, Data) when is_binary(Data) ->
    #edeblib_sums{size=Size+byte_size(Data),
                  md5=crypto:md5_update(MD5, Data),
                  sha1=crypto:sha_update(SHA1, Data),
                  sha256=erlsha2:sha256_update(SHA256, Data)}.

final(#edeblib_sums{}=Context, Data) ->
    final(update(Context, Data)).

final(#edeblib_sums{size=Size, md5=MD5, sha1=SHA1, sha256=SHA256}) -> [
    {md5, crypto:md5_final(MD5)},
    {sha1, crypto:sha_final(SHA1)},
    {sha256, erlsha2:sha256_final(SHA256)},
    {size, Size}
].
