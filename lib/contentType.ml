type t =
  {
    ty         : string;
    subty      : string;
    parameters : (string * string) list;
  }

let make ?(parameters = []) ty subty =
  { ty; subty; parameters; }

let pp fmt { ty; subty; parameters; } =
  let rec aux fmt = function
    | [] -> ()
    | (attribute, value) :: r ->
      Format.fprintf fmt "; %s=%s" attribute value;
      aux fmt r
  in
  Format.fprintf fmt "%s/%s%a"
    ty subty aux parameters

let equal = (=)
