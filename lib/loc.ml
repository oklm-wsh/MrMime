type position =
  {
    filename : string option;
    lnum     : int;
    cnum     : int;
    seek     : int;
  }

type area =
  {
    filename   : string option;
    start_line : int;
    start_col  : int;
    end_line   : int;
    end_col    : int;
  }

type t =
  | Unknown
  | Known of (position * position * area)

let gt_position a b =
  match a.lnum > b.lnum,
        a.lnum = b.lnum,
        a.cnum > b.lnum with
  | true,    _,    _ -> true
  |    _, true, true -> true
  | _                -> false

let lt_position a b =
  match a.lnum < b.lnum,
        a.lnum = b.lnum,
        a.cnum < b.cnum with
  | true,    _,    _ -> true
  |    _, true, true -> true
  | _                -> false

let lt_eq_position a b =
  match a.lnum < b.lnum,
        a.lnum = b.lnum,
        a.cnum < b.cnum,
        a.cnum = b.cnum with
  | true,    _,    _,    _ -> true
  |    _, true, true,    _ -> true
  |    _, true,    _, true -> true
  | _                      -> false


let join_position (p1 : position) (p2 : position) =
  assert (p1.filename = p2.filename);
  assert (p1.lnum <= p2.lnum);

  { filename   = p1.filename;
    start_line = p1.lnum;
    start_col  = p1.cnum;
    end_line   = p2.lnum;
    end_col    = p2.cnum; }

let make a b =
  let dismantle pos =
    { filename =
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
  assert (a.filename = b.filename);
  if lt_eq_position a b
  then Known (a, b, join_position a b)
  else Known (b, a, join_position b a)

let unknown = Unknown

let join a b = match a, b with
  | Known (a, _, z1), Known (_, b, z2) ->
    if lt_eq_position a b
    then Known (a, b, { z1 with end_line = z2.end_line;
                                end_col = z2.end_col; })
    else Known (b, a, { z2 with end_line = z1.end_line;
                                end_col = z1.end_col; })
  | _ -> Unknown

let of_lexbuf lex =
  make
    (Lexing.lexeme_start_p lex)
    (Lexing.lexeme_end_p lex)

let pp fmt = function
  | Unknown ->
    Format.fprintf fmt "unknown location"
  | Known (a, b, ({ filename = Some filename; _ } as area)) ->
    Format.fprintf fmt "file %S, l.%d c.%d"
      filename area.start_line area.start_col
  | Known (a, b, ({ filename = None; _ } as area)) ->
    Format.fprintf fmt "l.%d c.%d"
      (area.start_line - 1) area.start_col

let pp_of_file ?(hidden=['\n'; '\t'; '\r']) fmt = function
  | Known (a, b, { filename = Some filename; _ }) ->
    let in_ch = open_in filename in
    let buffer = Buffer.create 16 in

    seek_in in_ch a.seek;

    let rec collect = function
      | 0, _ -> Buffer.contents buffer
      | n, c when List.exists ((=) c) hidden = false ->
        Buffer.add_char buffer c;
        collect (n - 1, input_char in_ch)
      | n, _ -> collect (n - 1, input_char in_ch)
    in

    Format.fprintf fmt "%S" (collect ((b.seek - a.seek), input_char in_ch));

    close_in in_ch
  | _ -> raise (Invalid_argument "Loc.pp_of_file")
