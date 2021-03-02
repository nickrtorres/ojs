open Types

type attribute = ReadOnly | DontEnum | DontDelete | Internal

type jsobject = {
  properties : (string, js_type) Hashtbl.t;
  prototype : jsobject option;
}

let new_object () = { properties = Hashtbl.create 100; prototype = None }

let new_object_with_prototype prototype =
  { properties = Hashtbl.create 100; prototype = Some prototype }

(* 8.6.2.1 *)
let rec get name obj =
  match Hashtbl.find_opt obj.properties name with
  | Some value -> value
  | None -> (
      match obj.prototype with
      | Some proto -> get name proto
      | None -> TyUndefined)

(* FIXME 8.6.2.3 *)
let can_put _name _obj = true

let put name value obj =
  if can_put name obj then Hashtbl.replace obj.properties name value else ()

let has_property name obj =
  let rec lookup = function
    | None -> false
    | Some proto -> Hashtbl.mem proto.properties name || lookup proto.prototype
  in
  Hashtbl.mem obj.properties name || lookup obj.prototype

(* Tests *)
let%test "add properties" =
  let obj = new_object () in
  let () = put "foo" (TyNumber 42.) obj in
  get "foo" obj = TyNumber 42.

let%test "update properties" =
  let obj = new_object () in
  let () = put "foo" (TyNumber 100.) obj in
  let () = put "foo" (TyNumber 42.) obj in
  get "foo" obj = TyNumber 42.

let%test "has property" =
  let obj = new_object () in
  let () = put "foo" (TyNumber 42.) obj in
  has_property "foo" obj

let%test "prototype properties" =
  let a = new_object () in
  let b = new_object_with_prototype a in
  let c = new_object_with_prototype b in
  let () = put "foo" (TyNumber 100.) a in
  has_property "foo" c
