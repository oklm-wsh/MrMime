include (module type of Parser)
 with type 'a Input.t = 'a RingBuffer.Committed.t
  and type err = Parser.err
  and type s   = Parser.s
  and type ('a, 'input) k = 'input Parser.Input.t -> Parser.s -> 'a
  and type ('a, 'input) state = ('a, 'input) Parser.state
  and type ('r, 'input) fail = ('r, 'input) Parser.fail
  and type ('a, 'r, 'input) success = ('a, 'r, 'input) Parser.success
  and type 'a t = 'a Parser.t
