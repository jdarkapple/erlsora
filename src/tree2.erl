-module(tree2).

-export( [ new/0,
           make_empty/1,
           is_empty/1,
           insert/3,
           delete/2,
           lookup/2,
           map/2 ] ).

-export( [ size/1 ] ).
-export( [ keys/1 ] ).
-export( [ values/1 ] ).
-export( [ to_list/1 ] ).
-export( [ min/1 ] ).
-export( [ max/1 ] ).

-export([write_tree/1]).

-export( [ test_insert/0 ] ).

new() ->
    nil.

is_empty( nil ) ->
    true;
is_empty( { _K, _V , L, R } ) when is_tuple( L ) andalso is_tuple( R )->
    false;
is_empty( _ ) ->
    badarg.

insert( K, V, nil ) ->
    { K, V, nil, nil };
insert( K, V, { K, _ , L, R } ) ->
    { K, V, L, R };
insert( K, V, { K1, V1, Left, Right }) when K < K1 ->
    { K1, V1, insert( K, V, Left ), Right };
insert( K, V, { K1, V1, Left, Right }) when K > K1 ->
    { K1, V1, Left, insert( K, V, Right ) }.
    
delete( Key, { Key1, Value1, Smaller, Bigger } ) when Key < Key1 ->
    { Key1, Value1, delete( Key, Smaller ), Bigger };
delete( Key, { Key1, Value1, Smaller, Bigger } ) when Key > Key1 ->
    { Key1, Value1, Smaller, delete( Key, Bigger ) };
delete( Key, { Key, _, Smaller, Bigger }) ->
    merge( Smaller, Bigger ).

merge( Smaller, nil ) ->
    Smaller;
merge( nil, Bigger ) ->
    Bigger;
merge( Smaller, Bigger ) ->
    %% 从右子树中取出一个最小值作为一个根节点来连接左子树和去除最小值后右子树
    { Key, Value, Bigger1 } = take_smallest( Bigger ),
    { Key, Value, Smaller, Bigger1 }.

take_smallest( { Key, Value, nil, Bigger } ) ->
    { Key, Value, Bigger };
take_smallest( { Key, Value, Smaller, Bigger } ) ->
    { Key1, Value1, Smaller1 } = take_smallest( Smaller ),
    { Key1, Value1, { Key, Value, Smaller1, Bigger } }.

lookup( _K, nil ) ->
    not_found;
lookup( K, { K, V, _, _ } ) ->
    V;
lookup( K, { K1, _, L, _ } ) when K < K1 ->
    lookup( K, L );
lookup( K, { K1, _, _, R } ) when K > K1 ->
    lookup( K, R ).

make_empty( _T ) ->
    nil.

map( _, nil ) ->
    nil;
map( F, { K, V, Smaller, Bigger } ) ->
    { K, F( V ), map( F, Smaller ), map( F, Bigger ) }.

size( Tree ) ->
    size1( Tree, 0 ).

size1( { _, _, Smaller, Bigger }, Size ) ->
    size1( Smaller, size1( Bigger, Size + 1) );
size1( nil, Size ) ->
    Size.

keys( Tree ) ->
    keys1( Tree, [] ).

keys1( {K, _, Smaller, Bigger }, L ) ->
    %% 中序遍历
    keys1( Smaller, [ K | keys1( Bigger, L ) ] );
    %% 后序遍历
    %%keys1( Bigger, [ K | keys1( Smaller, L ) ] );
    %% 先序遍历
    %%[ K | keys1( Smaller, keys1( Bigger, L ) ) ];
keys1( nil, L ) ->
    L.

values( Tree ) ->
    values1( Tree, [] ).

values1( { _, V, Smaller, Bigger }, L ) ->
    values1( Smaller, [ V | values1( Bigger, L ) ] );
values1( nil, L ) ->
    L.

to_list( Tree ) ->
    to_list1( Tree, [] ).

to_list1( { K, V, Smaller, Bigger }, L ) ->
    to_list1( Smaller, [ { K, V } | to_list1( Bigger, L ) ] );
to_list1( nil, L ) ->
    L.

min( { Key, Value, nil, _ } ) ->
    { Key, Value };
min( { _, _, Smaller, _ } ) ->
    min( Smaller ).

max( { Key, Value, _, nil } ) ->
    { Key, Value };
max( { _, _, _, Bigger } ) ->
    max( Bigger ).

write_tree( Tree ) ->
    write_tree( 0, Tree ).

write_tree( D, nil ) ->
    tab( D ),
    io:format( 'nil', []);
write_tree( D, { Key, Value, Smaller, Larger } ) ->
    D1 = D + 6,
    write_tree( D1, Larger ),
    io:format("~n", []),
    tab( D ),
    io:format("~s ==> ~ts~n", [ Key, unicode:characters_to_list( Value ) ] ),
    write_tree( D1, Smaller ).

tab( 0 ) ->
    ok;
tab( N ) ->
    io:format( "-", [] ),
    tab( N - 1 ).
%% ------------
%% Test
%% ------------
test_insert() ->
    %%T0 = new(),
    %%T1 = insert( orange, 23, T0 ),
    %%T2 = insert( banana, 3, T1),
    %%T3 = insert( apple, 12, T2 ),
    %%T4 = insert( lemon, 500, T3 ),
    %%insert( zz, 1, T4 ).
    T0 = new(),
    { ok, Bin } = file:read_file( "./priv/fruit.data" ),
    Pairs = binary:split( Bin, <<"\n">>, [ global, trim ] ),
    KVs = [ begin
          [ En, Cn | _ ] = binary:split( P, <<" ">>, [ global, trim ] ),
          { En, Cn }
      end ||
    P <- Pairs ],
    do_insert( KVs, T0).

do_insert( [], Tree ) ->
    Tree;
do_insert( [ { K, V } | Rest ], Tree ) ->
   do_insert( Rest, insert( K, V, Tree ) ).
