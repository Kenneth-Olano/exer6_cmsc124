-module(olano_senires).
-compile(export_all).

receiveMessage(ReceiverName) ->
    receive
        bye -> %when bye message is received
            io:format("~nYour partner disconnected~n"),
            halt(); %exit the erlang shell of receiving node, reference: erlang.org/docs/26/man/erlang
        {SenderName, String} -> %receives sendername and string message
            io:format("~s: ~s~n", [SenderName, String]), %print received message with name of sender
            receiveMessage(ReceiverName)  %recursively wait for other messages
    end.


sendMessage(SenderName, ReceiverName) -> 
    String = string:trim(io:get_line("You: ")),  %get message input
    case String of
        "bye" -> 
            global:send(ReceiverName, bye),  %sends bye to PID(Receivername) globally reference: erlang.org/doc/apps/kernel/global.html
            ok,
            halt();  %exit erlang shell of sending node
        _ -> 
            global:send(ReceiverName, {SenderName, String}),  %send name of sender and string message to receiver globally
            sendMessage(SenderName, ReceiverName)  %loop back to send more messages
    end.

%Initialize node 1
init_chat() -> 
    _Name = string:trim(io:get_line("Name: ")),  %get name
    Self = spawn(olano_senires, receiveMessage, [_Name]),  %spawn the receiving process
    global:register_name(receiveMessage_local, Self),  %register the receiving spawned process globally
    io:format("Successfully connected. You can now send and receive messages.~n"),
    sendMessage(_Name, receiveMessage_remote).  %Call sendMessage with global name

%Initialize node 2 using node 1
init_chat2(ReceiverNode) -> 
    _Name = string:trim(io:get_line("Name: ")),  
    case net_adm:ping(ReceiverNode) of %try connecting node 2 to node 1
        pong ->  %if connection is successful
            Self = spawn(olano_senires, receiveMessage, [_Name]), %spawn receive message
            global:register_name(receiveMessage_remote, Self),  %register the receiving spawned process globally where receiveMessage_remote is associated with the pid of the spawned process
                                                                %reference: erlang.org/doc/apps/kernel/global.html
            io:format("Successfully connected. You can now send and receive messages.~n"),
            sendMessage(_Name, receiveMessage_local);  %call sendMessage with global name
        pang ->  %if connection not successful
            io:format("Unable to make a connection."),
            ok
    end.