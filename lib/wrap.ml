open Encoder

module Queue =
struct
  type 'a cell =
    | Nil
    | Cons of 'a cell'
  and 'a cell' = { mutable head : 'a; mutable tail : 'a cell; }

  type 'a t =
    { mutable insert : 'a cell
    ; mutable body   : 'a cell }

  let make () =
    { insert = Nil
    ; body   = Nil }

  let clear queue =
    queue.insert <- Nil;
    queue.body <- Nil

  let add x queue =
    let c = Cons { head = x; tail = Nil; } in
    match queue with
    | { insert = Cons cell; _ } ->
      queue.insert <- c;
      cell.tail <- c
    | { insert = Nil; _ } ->
      queue.insert <- c;
      queue.body <- c

  exception Empty

  let peek = function
    | { body = Cons { head = x; _ }; _ } -> x
    | { body = Nil; _ } -> raise Empty

  let take = function
    | { body = Cons { head = x; tail = tl; }; _ } as queue ->
      queue.body <- tl;
      if tl = Nil then queue.insert <- Nil;
      x
    | { body = Nil; _ } -> raise Empty
end

type t =
  { state                  : Encoder.t
  ; mutable scan_stack     : scan_element list
    (* the pretty-printer formatting stack.
     * each element is (left_total, element) where left_total is the value of
     * left_total when the element has bee enqueued. *)
  ; mutable format_stack   : format_element list
    (* the pretty-printer formatting stack.
     * each stack element describes a pretty-printing box. *)
  ; mutable margin         : int  (* value of right margin. *)
  ; mutable max_indent     : int  (* maximum value of indentation: no box can be opened further. *)
  ; mutable space_left     : int  (* space remaining on the current line *)
  ; mutable min_space_left : int
  ; mutable current_indent : int  (* current value of indentation. *)
  ; mutable left_total     : int  (* total width of tokens already printed. *)
  ; mutable right_total    : int  (* total width of tokens ever put in queue. *)
  ; mutable is_new_line    : bool (* true when the line has been broken by the pretty-print. *)
  ; mutable curr_depth     : int  (* current number of opened boxes. *)
  ; queue                  : element Queue.t }
and element =
  { mutable size   : int
  ; token          : token
  ; length         : int }
and token =
  | Text of string
  | Break of int * int
  | Begin of int * box
  | End
  | Newline
  | IfNewline
and box =
  | HBox
  | VBox
  | HVBox
  | HOVBox
  | Box
  | Fits
and format_element = Format of box * int
and scan_element = Scan of int * element

type 'r               k0 = (t -> 'r) -> t -> 'r
type ('a, 'r)         k1 = 'a -> (t -> 'r) -> t -> 'r
type ('a, 'b, 'r)     k2 = 'a -> 'b -> (t -> 'r) -> t -> 'r

let box_to_string = function
  | HBox -> "hbox"
  | VBox -> "vbox"
  | HVBox -> "hvbox"
  | HOVBox -> "hovbox"
  | Box -> "box"
  | Fits -> "fits"

let sp = Format.sprintf

let _token_to_string = function
  | Text s -> sp "Text %S" s
  | Break (n, off) -> sp "Break (%d, %d)" n off
  | Begin (n, box) -> sp "Begin (%d, %s)" n (box_to_string box)
  | End -> sp "End"
  | Newline -> sp "Newline"
  | IfNewline -> sp "IfNewline"

let enqueue state ({ length; _ } as token) =
  state.right_total <- state.right_total + length;
  Queue.add token state.queue

let clear_queue state =
  state.left_total <- 1;
  state.right_total <- 1;
  Queue.clear state.queue

let output_string s k ({ state; _ } as fmt) =
  string s (fun state -> k { fmt with state = state }) state

let output_newline k ({ state; _ } as fmt) =
  string "\r\n" (fun state -> k { fmt with state = state }) state

let output_spaces n k ({ state; _ } as fmt) =
  string (String.make n ' ') (fun state -> k { fmt with state = state }) state

let ( $ ) x y k e = x (y k) e

let break_new_line offset width =
  output_newline
  $ fun k fmt ->
    fmt.is_new_line <- true;
    let indent = fmt.margin - width + offset in
    let indent = min fmt.max_indent indent in
    fmt.current_indent <- indent;
    fmt.space_left <- fmt.margin - fmt.current_indent;
    output_spaces fmt.current_indent k fmt

let break_line width fmt = break_new_line 0 width fmt

let break_same_line width k fmt =
  fmt.space_left <- fmt.space_left - width;
  output_spaces width k fmt

let force_break_line k fmt =
  match fmt.format_stack with
  | [] -> output_newline k fmt
  | Format (box, width) :: _ ->
    if width > fmt.space_left
    then match box with
         | Fits | HBox -> k fmt
         | VBox | HVBox | HOVBox | Box ->
           break_line width k fmt
    else k fmt

let skip_token k fmt =
  match Queue.take fmt.queue with
  | { size; length; _ } ->
    fmt.left_total <- fmt.left_total - length;
    fmt.space_left <- fmt.space_left - size;
    k fmt

let compute_token size token k fmt =
  match token with
  | Text str ->
    fmt.space_left <- fmt.space_left - size;
    output_string str
      (fun fmt ->
       fmt.is_new_line <- false;
       k fmt)
      fmt
  | Begin (off, box) ->
    let insertion_point = fmt.margin - fmt.space_left in
    (if insertion_point > fmt.max_indent
     then force_break_line
     else noop)
    (fun fmt ->
     let offset = fmt.space_left - off in
     let box = match box with
       | VBox -> VBox
       | HBox | HVBox | HOVBox | Box | Fits ->
         if size > fmt.space_left then box else Fits in
     fmt.format_stack <- Format (box, offset) :: fmt.format_stack;
     k fmt)
    fmt
  | End ->
    (match fmt.format_stack with
     | _ :: r -> fmt.format_stack <- r; k fmt
     | _ -> k fmt)
  | Newline ->
    (match fmt.format_stack with
     | Format (_, width) :: _ -> break_line width k fmt
     | [] -> output_newline k fmt)
  | IfNewline ->
    if fmt.current_indent <> (fmt.margin - fmt.space_left)
    then skip_token k fmt
    else k fmt
  | Break (n, off) ->
    match fmt.format_stack with
    | [] -> k fmt
    | Format (box, width) :: _r ->
      match box with
      | HOVBox ->
        if size > fmt.space_left
        then break_new_line off width k fmt
        else break_same_line n k fmt
      | Box ->
        (* Have the line just been broken here? *)
        if fmt.is_new_line
        then break_same_line n k fmt
        else if size > fmt.space_left
        then break_new_line off width k fmt
        (* Break the ine here leads to new indentation ? *)
        else if fmt.current_indent > fmt.margin - width + off
        then break_new_line off width k fmt
        else break_same_line n k fmt
      | HVBox -> break_new_line off width k fmt
      | Fits -> break_same_line n k fmt
      | VBox -> break_new_line off width k fmt
      | HBox -> break_same_line n k fmt

let infinity = 1000000010

let rec advance k fmt =
  match Queue.peek fmt.queue with
  | { size; token; length; } ->
    if not (size < 0 && (fmt.right_total - fmt.left_total < fmt.space_left))
    then begin
      ignore (Queue.take fmt.queue);
      compute_token
        (if size < 0 then infinity else size) token
        (fun fmt ->
         fmt.left_total <- length + fmt.left_total;
         advance k fmt)
        fmt
    end else k fmt
  | exception Queue.Empty -> k fmt
    (* XXX: [advance_left] catch this exception but, I don't know why,
     * [Queue.Empty] escapes [advance_left] and fail the encoder. So, I catch
     * this exception inside [advance] function and compute [k fmt] (like
     * [advance_left] will do if he catches the exception). *)

let advance_left k fmt =
  try advance k fmt
  with Queue.Empty -> k fmt

let enqueue_advance tok k fmt =
  enqueue fmt tok;
  advance_left k fmt

let make_element size token length =
  { size; token; length; }

let enqueue_string_as size str k fmt =
  enqueue_advance (make_element size (Text str) size) k fmt

let _enqueue_string str k fmt =
  let size = String.length str in
  enqueue_string_as size str k fmt

let bottom =
  let first = make_element (-1) (Text "") 0 in
  [ Scan (-1, first) ]

let clear_scan k fmt =
  fmt.scan_stack <- bottom;
  k fmt

let set_size break_or_box k fmt =
  match fmt.scan_stack with
  | [] -> k fmt
  | Scan (left_total, ({ size; token; _ } as elem)) :: t ->
    if left_total < fmt.left_total
    then clear_scan k fmt
    else match token with
    | Break _ ->
      if break_or_box
      then begin
        elem.size <- fmt.right_total + size;
        fmt.scan_stack <- t;
        k fmt
      end else k fmt
    | Begin _ ->
      if not break_or_box
      then begin
        elem.size <- fmt.right_total + size;
        fmt.scan_stack <- t;
        k fmt
      end else k fmt
    | Text _ | End | Newline | IfNewline -> k fmt

let push scan token k fmt =
  enqueue fmt token;
  let e fmt =
    fmt.scan_stack <- Scan (fmt.right_total, token) :: fmt.scan_stack;
    k fmt in
  if scan then set_size true e fmt else e fmt

let open_box_gen indent box k fmt =
  fmt.curr_depth <- fmt.curr_depth + 1;
  let e = make_element (- fmt.right_total) (Begin (indent, box)) 0 in
  push false e k fmt

let open_sys_box k fmt = open_box_gen 0 HOVBox k fmt

let close_box k fmt =
  if fmt.curr_depth > 1
  then begin
    enqueue fmt ({ size = 0; token = End; length = 0 });
    set_size true (set_size false (fun fmt -> fmt.curr_depth <- fmt.curr_depth - 1; k fmt)) fmt
  end else k fmt

let rinit k fmt =
  clear_queue fmt;
  clear_scan
    (fun fmt -> fmt.format_stack <- [];
      fmt.current_indent <- 0;
      fmt.space_left <- fmt.margin;
      open_sys_box k fmt)
    fmt

let flush newline k fmt =
  let rec close_all_box k fmt =
    if fmt.curr_depth > 1
    then close_box (close_all_box k) fmt
    else k fmt
  in

  close_all_box
    (fun fmt ->
       fmt.right_total <- infinity;
       advance_left (if newline then output_newline (rinit k) else rinit k) fmt)
    fmt

let min_space n k fmt =
  if n >= 1
  then let n = if n < infinity then n else pred infinity in
       fmt.min_space_left <- n;
       fmt.max_indent <- fmt.margin - fmt.min_space_left;
       rinit k fmt
  else noop k fmt

let max_indent n k fmt =
  min_space (fmt.margin - n) k fmt

let limit n k fmt =
  if n >= 1
  then let limit = if n < infinity then n else pred infinity in
       begin
         fmt.margin <- limit;
         let new_max_indent =
           if fmt.max_indent <= fmt.margin
           then fmt.max_indent
           else max (max (fmt.margin - fmt.min_space_left) (fmt.margin / 2)) 1
         in

         max_indent new_max_indent k fmt
       end
  else noop k fmt

let as_size size str k fmt =
  enqueue_string_as size str k fmt

let string str k fmt =
  as_size (String.length str) str k fmt

let close_box k fmt =
  close_box k fmt

let hbox k fmt =
  open_box_gen 0 HBox k fmt

let vbox indent k fmt =
  open_box_gen indent VBox k fmt

let hvbox indent k fmt =
  open_box_gen indent HVBox k fmt

let hovbox indent k fmt =
  open_box_gen indent HOVBox k fmt

let box indent k fmt =
  open_box_gen indent Box k fmt

let newline k fmt =
  flush true k fmt

let flush k fmt =
  flush false k fmt

let force_newline k fmt =
  enqueue_advance (make_element 0 Newline 0) k fmt

let if_newline k fmt =
  enqueue_advance (make_element 0 IfNewline 0) k fmt

let break width offset k fmt =
  let e = make_element (- fmt.right_total) (Break (width, offset)) width in
  push true e k fmt

let space k fmt = break 1 0 k fmt
let cut k fmt = break 0 0 k fmt
let char chr k fmt = string (String.make 1 chr) k fmt

let lift ?(margin = 998) k state =
  let queue = Queue.make () in
  let sys_token = make_element (-1) (Begin (0, HOVBox)) 0 in
  Queue.add sys_token queue;
  let sys_scan_stack = Scan (1, sys_token) :: bottom in
  let min_space_left = 10 in
  k { state
    ; scan_stack = sys_scan_stack
    ; format_stack = []
    ; margin
    ; max_indent = margin - min_space_left
    ; space_left = margin
    ; min_space_left
    ; current_indent = 0
    ; left_total = 1
    ; right_total = 1
    ; curr_depth = 1
    ; is_new_line = true
    ; queue }

let unlift k fmt =
  flush (fun { state; _ } -> k state) fmt
