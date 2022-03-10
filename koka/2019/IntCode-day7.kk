import std/os/readline
import std/os/env

effect vm
    fun read-next(): int
    fun read(addr: int): int
    fun write(value: int, addr: int): ()
    fun jump(addr: int): ()
    fun input(): int
    fun output(x: int): ()

type param
    Position(addr: int)
    Immediate(x: int)

type instruction
    Add(x: param, y: param, addr: int)
    Mul(x: param, y: param, addr: int)
    Input(addr: int)
    Output(x: param)
    JumpTrue(x: param, dest: param)
    JumpFalse(x: param, dest: param)
    LessThan(x: param, y: param, addr: int)
    Equals(x: param, y: param, addr: int)
    Halt

fun decode-param(mode: int) match mode
    0 -> Position(read-next())
    1 -> Immediate(read-next())
    x -> throw("Unexpected param mode: " ++ x.show)

fun show(param: param): string match param
    Immediate(x) -> "Immediate(" ++ x.show ++ ")"
    Position(addr) -> "Position(" ++ addr.show ++ ")"

fun show(ins: instruction): string match ins
    Add(x, y, addr) -> "Add(" ++ x.show ++ ", " ++ y.show ++ ", " ++ addr.show ++ ")"
    Mul(x, y, addr) -> "Mul(" ++ x.show ++ ", " ++ y.show ++ ", " ++ addr.show ++ ")"
    Input(addr) -> "Input(" ++ addr.show ++ ")"
    Output(addr) -> "Output(" ++ addr.show ++ ")"
    JumpTrue(x, dest) -> "JumpTrue(" ++ x.show ++ ", " ++ dest.show ++ ")"
    JumpFalse(x, dest) -> "JumpFalse(" ++ x.show ++ ", " ++ dest.show ++ ")"
    LessThan(x, y, addr) -> "LessThan(" ++ x.show ++ ", " ++ y.show ++ ", " ++ addr.show ++ ")"
    Equals(x, y, addr) -> "Equals(" ++ x.show ++ ", " ++ y.show ++ ", " ++ addr.show ++ ")"
    Halt -> "Halt"

fun decode(): <exn,vm> instruction
    val instruction = read-next()
    val op-code = instruction % 100
    val (m1, m2) = (instruction / 100 % 10, instruction / 1000 % 10)
    match op-code
        1 -> Add(m1.decode-param, m2.decode-param, read-next())
        2 -> Mul(m1.decode-param, m2.decode-param, read-next())
        3 -> Input(read-next())
        4 -> Output(m1.decode-param)
        5 -> JumpTrue(m1.decode-param, m2.decode-param)
        6 -> JumpFalse(m1.decode-param, m2.decode-param)
        7 -> LessThan(m1.decode-param, m2.decode-param, read-next())
        8 -> Equals(m1.decode-param, m2.decode-param, read-next())
        99 -> Halt
        x -> throw("Unexpected op-code: " ++ x.show)

fun param-value(p: param) match p
    Immediate(x) -> x
    Position(addr) -> read(addr)

fun step()
    match decode()
        Add(x, y, addr) -> write(x.param-value + y.param-value, addr)
        Mul(x, y, addr) -> write(x.param-value * y.param-value, addr)
        Input(addr) -> input().write(addr)
        Output(x) -> output(x.param-value)
        JumpTrue(x, dest) -> if x.param-value != 0 then dest.param-value.jump
        JumpFalse(x, dest) -> if x.param-value == 0 then dest.param-value.jump
        LessThan(x, y, addr) -> write(if x.param-value < y.param-value then 1 else 0, addr)
        Equals(x, y, addr) -> write(if x.param-value == y.param-value then 1 else 0, addr)
        Halt -> return ()
    step()

fun run-instance(memory: vector<int>, phase: int, signal: int)
    var mem := memory
    var instruction-ptr := 0
    var input-call-count := 0
    var result := Nothing
    with ctl throw-exn(ex)
        throw-exn(Exception(
            "in memory " ++ mem.list.show ++ " at address " ++ instruction-ptr.show ++ ":\n  " ++ ex.message,
            ex.info
        ))
    handle step
        fun read-next()
            instruction-ptr := instruction-ptr + 1
            mem[instruction-ptr - 1]
        fun read(addr) mem[addr]
        fun write(value, addr) mem[addr] := value
        fun jump(addr) instruction-ptr := addr
        fun input()
            input-call-count := input-call-count + 1
            match input-call-count
                1 -> phase
                2 -> signal
                _ -> throw("unexpected additional input request")
        fun output(x) result := Just(x)
    match result
        Just(x) -> x
        Nothing -> throw("no output received")

fun run(memory: vector<int>)
    list(0, 4).permutations.foldl(-10000) fn(thrust, phases)
        val x = phases.foldl(0) fn(signal, phase) run-instance(memory, phase, signal)
        max(thrust, x)

fun main()
    try {
        match get-args()
            [arg] ->
                var memory := arg.split(",").map(fn(x) -> parse-int-default(x, 0)).vector
                val thrust = run(memory)
                println("max thrust: " ++ thrust.show)
            _ -> println("Bad arguments")
    } fn(ex) -> println(ex.show)


fun permutations(head: list<a>, tail: list<a> = []): <div> list<list<a>>
    match head
        Nil -> [tail]
        _ ->
            fun iterate(head', tail') match head'
                Cons(x, xs) -> permutations(xs ++ tail', Cons(x, tail)) ++ iterate(xs, Cons(x, tail'))
                Nil -> []
            iterate(head, [])
