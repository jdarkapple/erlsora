-module(ranch_protocol).

%% 为给定的socket开启新的链接进程
-callback start_link(
			Ref::ranch:ref(),
			Socket::any(),
			Transport::module(),
			ProtocolOptions::any())
       -> {ok, ConnectionPid::pid()}.
