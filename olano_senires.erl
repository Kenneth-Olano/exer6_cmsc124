-module(olano_senires).
-compile(export_all).

% Function to receive messages
receiveMessage(ReceiverName) -> 
    % io:format("~s: ", [ReceiverName]),
    receive
        bye -> 
            io:format("~nYour partner disconnected~n"),
            global:send(self(), bye),
            ok;  % Exit the function
        {SenderName, String} -> 
            io:format("~s: ~s~n", [SenderName, String]),
           
            receiveMessage(ReceiverName)  % Recursively wait for more messages
    end.

% Function to send messages (directly called in the main process)
sendMessage(SenderName, ReceiverName) -> 
    % io:format("~s: ", [SenderName]),  % Explicit prompt for message
    String = string:trim(io:get_line("You:")),  % Capture the input after the prompt
    case String of
        "bye" -> 
            global:send(ReceiverName, bye),  % Send the 'bye' message globally
            ok;  % Terminates the chat
        _ -> 
            global:send(ReceiverName, {SenderName, String}),  % Send the message globally
            sendMessage(SenderName, ReceiverName)  % Continue sending messages
    end.

% Initialize chat (local node)
init_chat() -> 
    _Name = string:trim(io:get_line("Name: ")),  % Request user name but ignore it for now
    Self = spawn(olano_senires, receiveMessage, [_Name]),  % Spawn the receiving process
    global:register_name(receiveMessage_local, Self),  % Register the receiving process globally
    io:format("Chat initialized locally. You can now send and receive messages.~n"),
    sendMessage(_Name, receiveMessage_remote).  % Call sendMessage with global name

% Initialize chat with another node (distributed)
init_chat2(ReceiverNode) -> 
    _Name = string:trim(io:get_line("Name: ")),  % Request user name but ignore it
    case net_adm:ping(ReceiverNode) of
        pong ->  % If ping is successful, proceed
            Self = spawn(olano_senires, receiveMessage, [_Name]),
            global:register_name(receiveMessage_remote, Self),  % Register the receiving process globally
            io:format("Connected to receiver node: ~p. Chat initialized.~n", [ReceiverNode]),
            sendMessage(_Name, receiveMessage_local);  % Call sendMessage with global name
        pang ->  % Handle the case where the receiver node is not reachable
            io:format("Receiver node ~p is unreachable.~n", [ReceiverNode]),
            init_chat2(ReceiverNode)  % Optionally, retry the connection
    end.