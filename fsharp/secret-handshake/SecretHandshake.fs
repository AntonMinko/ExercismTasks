module SecretHandshake

let commands number =
    let addBit flag phrase handshake =
        if number &&& flag = flag then 
            phrase::handshake 
        else 
            handshake
    
    let handshake =
        [] 
        |> addBit 0b0001 "wink"
        |> addBit 0b0010 "double blink"
        |> addBit 0b0100 "close your eyes"
        |> addBit 0b1000 "jump"

    if number &&& 0b10000 = 0b10000 then 
        handshake
    else
        handshake |> List.rev
