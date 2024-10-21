-module(olano_senires).
-compile(export_all).


receiveMessage(SenderName, ReceiverNode) -> 
    receive
        bye ->
            io:format("~s left ~n", ReceiverNode);
        String ->
            receiveMessage(SenderName, ReceiverNode)
    end.


sendMessage(SenderName, ReceiverNode) -> 
    String = io:get_line("~s: ", SenderName),
    io:format("~s: ~s ~n", SenderName, String),
    ReceiverNode ! String,
    sendMessage(SenderName, ReceiverNode).


init_chat() -> 
    Name = io:get_line("Name: "),
    register(receiveMessage, spawn(olano_senires,receiveMessage,[Name, self()])),
    register(sendMessage, spawn(olano_senires,sendMessage,[Name, self()])).
    

init_chat2(Receiver) -> 
    Name = io:get_line("Name: "),
    net_adm:ping(Receiver),
    register(sendMessage, spawn(olano_senires,sendMessage,[Name, self()])),
    register(receiveMessage, spawn(olano_senires,receiveMessage,[Name, self()])).
    