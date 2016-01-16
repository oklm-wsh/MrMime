module Position =
struct
  type t =
    {
      filename : string option;
      lnum     : int;
      cnum     : int;
      seek     : int;
    }

  let gt a b =
    match a.lnum > b.lnum,
          a.lnum = b.lnum,
          a.cnum > b.lnum with
    | true,    _,    _ -> true
    |    _, true, true -> true
    | _                -> false

  let lt a b =
    match a.lnum < b.lnum,
          a.lnum = b.lnum,
          a.cnum < b.cnum with
    | true,    _,    _ -> true
    |    _, true, true -> true
    | _                -> false

  let lt_eq a b =
    match a.lnum < b.lnum,
          a.lnum = b.lnum,
          a.cnum < b.cnum,
          a.cnum = b.cnum with
    | true,    _,    _,    _ -> true
    |    _, true, true,    _ -> true
    |    _, true,    _, true -> true
    | _                      -> false
end

module Area =
struct
  type t =
    {
      filename   : string option;
      start_line : int;
      start_col  : int;
      end_line   : int;
      end_col    : int;
    }


  let join (p1 : Position.t) (p2 : Position.t) =
    assert (p1.Position.filename = p2.Position.filename);
    assert (p1.Position.lnum <= p2.Position.lnum);

    { filename   = p1.Position.filename;
      start_line = p1.Position.lnum;
      start_col  = p1.Position.cnum;
      end_line   = p2.Position.lnum;
      end_col    = p2.Position.cnum; }
end

type t =
  | Unknown
  | Known of (Position.t * Position.t * Area.t)

let make a b =
  let dismantle pos =
    { Position.filename =
        if String.length pos.Lexing.pos_fname = 0
           || pos.Lexing.pos_fname = "<dummy>"
        then None
        else Some pos.Lexing.pos_fname;
      lnum = pos.Lexing.pos_lnum;
      cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1;
      seek = pos.Lexing.pos_cnum; }
    in
  let a = dismantle a in
  let b = dismantle b in
  assert (a.Position.filename = b.Position.filename);
  if Position.lt_eq a b
  then Known (a, b, Area.join a b)
  else Known (b, a, Area.join b a)

let unknown = Unknown

let join a b = match a, b with
  | Known (a, _, z1), Known (_, b, z2) ->
    if Position.lt_eq a b
    then Known (a, b, { z1 with Area.end_line = z2.Area.end_line;
                                end_col = z2.Area.end_col; })
    else Known (b, a, { z2 with Area.end_line = z1.Area.end_line;
                                end_col = z1.Area.end_col; })
  | _ -> Unknown

let of_lexbuf lex =
  make
    (Lexing.lexeme_start_p lex)
    (Lexing.lexeme_end_p lex)

let pp fmt = function
  | Unknown ->
    Format.fprintf fmt "unknown location"
  | Known (a, b, ({ Area.filename = Some filename; _ } as area)) ->
    Format.fprintf fmt "file %S, l.%d c.%d"
      filename area.Area.start_line area.Area.start_col
  | Known (a, b, ({ Area.filename = None; _ } as area)) ->
    Format.fprintf fmt "l.%d c.%d"
      (area.Area.start_line - 1) area.Area.start_col

let pp_of_file ?(hidden=['\n'; '\t'; '\r']) fmt = function
  | Known (a, b, { Area.filename = Some filename; _ }) ->
    let in_ch = open_in filename in
    let buffer = Buffer.create 16 in

    seek_in in_ch a.Position.seek;

    let rec collect = function
      | 0, _ -> Buffer.contents buffer
      | n, c when List.exists ((=) c) hidden = false ->
        Buffer.add_char buffer c;
        collect (n - 1, input_char in_ch)
      | n, _ -> collect (n - 1, input_char in_ch)
    in

    Format.fprintf fmt "%S" (collect ((b.Position.seek - a.Position.seek), input_char in_ch));

    close_in in_ch
  | _ -> raise (Invalid_argument "Loc.pp_of_file")
