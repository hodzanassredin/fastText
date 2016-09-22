namespace FastText
module Utils = 
    let spaceCode = 0x20uy

    let SIGMOID_TABLE_SIZE = 512
    let SIGMOID_TABLE_SIZEf = float(SIGMOID_TABLE_SIZE)
    let MAX_SIGMOID = 8
    let MAX_SIGMOIDf = float(MAX_SIGMOID)
    let LOG_TABLE_SIZE = 512
    let LOG_TABLE_SIZEf = float(LOG_TABLE_SIZE)

    let t_sigmoid = Array.zeroCreate (SIGMOID_TABLE_SIZE + 1)
    
    for i in 0..SIGMOID_TABLE_SIZE do
        let x = float(i * 2 * MAX_SIGMOID) / SIGMOID_TABLE_SIZEf - MAX_SIGMOIDf
        t_sigmoid.[i] <- 1.0 / (1.0 + exp(-x))

    let t_log = Array.zeroCreate (LOG_TABLE_SIZE + 1)
    for i in 0..LOG_TABLE_SIZE do
        let x = (float(i) + 1e-5) / LOG_TABLE_SIZEf
        t_log.[i] <- log(x)

    let log(x : float) =
        if x > 1.0 then 0.0
        else let i = int(x * LOG_TABLE_SIZEf)
             t_log.[i]

    let sigmoid(x : float) =
        if x < float(-MAX_SIGMOID) then 0.0
        else if x > float(MAX_SIGMOID) then 1.0
        else let i = int((x + float(MAX_SIGMOID)) * SIGMOID_TABLE_SIZEf / MAX_SIGMOIDf / 2.)
             t_sigmoid.[i] //todo

