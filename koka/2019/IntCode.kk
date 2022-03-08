import std/os/env

effect vm
    fun op-code(): int
    fun arg(idx: int): int
    fun read(addr: int): int
    fun write(addr: int, value: int): ()
    fun advance(): ()

fun step()
    match op-code()
        1 -> arg(2).write(arg(0).read + arg(1).read)
        2 -> arg(2).write(arg(0).read * arg(1).read)
        99 -> return ()
        x -> throw("Unexpected op-code " ++ x.show)
    advance()
    step()

fun run(memory: vector<int>)
    var mem := memory
    var instruction-idx := 0
    handle step
        fun op-code() mem[instruction-idx]
        fun arg(idx) mem[instruction-idx + idx + 1]
        fun read(addr) mem[addr]
        fun write(addr, value) mem[addr] := value
        fun advance() instruction-idx := instruction-idx + 4
    mem[0]

fun solve-for(result: int, memory: vector<int>)
    for-while(0, 99) fn(noun)
        for-while(0, 99) fn(verb)
            var mem := memory
            mem[1] := noun
            mem[2] := verb
            if (try-default(False) { run(mem) == result }) then
                Just(100 * noun + verb)
            else
                Nothing

fun main()
    try {
        match get-args()
            [arg] ->
                var memory := arg.split(",").map(fn(x) -> parse-int-default(x, 0)).vector
                // part 1
                memory[1] := 12
                memory[2] := 2
                println("Day 2.1 result: " ++ memory.run.show)
                // part 2
                match solve-for(19690720, memory)
                    Just(res) -> println("Day 2.2 result: " ++ res.show)
                    Nothing -> println("Unable to find solution")
            _ -> println("Bad arguments")
    } fn(ex) -> println(ex.show)

