{module, test_define}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{test,0}]}.

{attributes, []}.

{labels, 7}.


{function, test, 0, 2}.
  {label,1}.
    {line,[{location,"test_define.erl",10}]}.
    {func_info,{atom,test_define},{atom,test},0}.
  {label,2}.
    {move,{atom,no_heheaa},{x,0}}.
    return.


{function, module_info, 0, 4}.
  {label,3}.
    {line,[]}.
    {func_info,{atom,test_define},{atom,module_info},0}.
  {label,4}.
    {move,{atom,test_define},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,test_define},{atom,module_info},1}.
  {label,6}.
    {move,{x,0},{x,1}}.
    {move,{atom,test_define},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
