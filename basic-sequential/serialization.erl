-module(serialization).
-exports([tree_to_list/1, list_to_tree/1]).

tree_to_list({leaf, Val}) ->
    [2, Val];
tree_to_list({node, T1, T2}) ->
    TL1 = tree_to_list(T1),
    TL2 = tree_to_list(T2),
    [Size1, _]     = TL1,
    [Size2, List2] = TL2,
    [Size1 + Size2 | TL1 ++ List2].

list_to_tree([2, N]) ->
    {leaf, N};
list_to_tree([N]) ->
    {leaf, N};
list_to_tree([Size | Items] = _Node) ->
    {LeftNode, RightNode} = lists:split(Size - 1, Items),
    {node, list_to_tree(LeftNode), list_to_tree(RightNode)}.