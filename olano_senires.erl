-module(olano_senires).
-compile(export_all).

% Function to receive messages
receiveMessage() -> 
    receive
        bye -> 
            io:format("A person left~n"),
            ok;  % Exit the function
        {SenderNode, String} -> 
            io:format("Person: ~s~n", [String]),
            receiveMessage()  % Recursively wait for more messages
    end.

% Function to send messages (directly called in the main process)
sendMessage(ReceiverName) -> 
    io:format("Message: "),  % Explicit prompt for message
    String = io:get_line(""),  % Capture the input after the prompt
    case String of
        "bye\n" -> 
            global:send(ReceiverName, bye);  % Send the 'bye' message globally
        _ -> 
            global:send(ReceiverName, {self(), String}),  % Send the message globally
            sendMessage(ReceiverName)  % Continue sending messages
    end.

% Initialize chat (local node)
init_chat() -> 
    _Name = string:trim(io:get_line("Name: ")),  % Request user name but ignore it for now
    Self = spawn(olano_senires, receiveMessage, []),  % Spawn the receiving process
    global:register_name(receiveMessage, Self),  % Register the receiving process globally
    io:format("Chat initialized locally. You can now send and receive messages.~n"),
    sendMessage(receiveMessage).  % Call sendMessage with global name

% Initialize chat with another node (distributed)
init_chat2(ReceiverNode) -> 
    _Name = string:trim(io:get_line("Name: ")),  % Request user name but ignore it
    case net_adm:ping(ReceiverNode) of
        pong ->  % If ping is successful, proceed
            Self = spawn(olano_senires, receiveMessage, []),
            global:register_name(receiveMessage, Self),  % Register the receiving process globally
            io:format("Connected to receiver node: ~p. Chat initialized.~n", [ReceiverNode]),
            sendMessage(receiveMessage);  % Call sendMessage with global name
        pang ->  % Handle the case where the receiver node is not reachable
            io:format("Receiver node ~p is unreachable.~n", [ReceiverNode]),
            init_chat2(ReceiverNode)  % Optionally, retry the connection
    end.