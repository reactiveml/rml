(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Pierre Chambart
 * Laboratoire PPS - CNRS Universit√© Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js
open Dom

class type blob = object
  method size : int readonly_prop
  method _type : js_string t readonly_prop
  method slice : int -> int -> blob meth
  method slice_withContentType : int -> int -> js_string t -> blob meth
end

class type file = object
  inherit blob
  method name : js_string t readonly_prop
  method lastModifiedDate : js_string t readonly_prop
end

type file_any

module CoerceTo : sig
  val string : file_any -> js_string t Opt.t
  val arrayBuffer : file_any -> Typed_array.arrayBuffer t Opt.t
end

class type fileList = object
  inherit [file] Dom.nodeList
end

class type fileError = object
  method code : int readonly_prop
end

(* {2 Events} *)
class type ['a] progressEvent = object
  inherit ['a] event
  method lengthComputable : bool t readonly_prop
  method loaded : int readonly_prop
  method total : int readonly_prop
end

class type progressEventTarget = object ('self)
  method onloadstart : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onprogress : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onload : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onabort : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onerror : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onloadend : ('self t, 'self progressEvent t) event_listener writeonly_prop
end

type readyState = EMPTY | LOADING | DONE

class type fileReader = object ('self)

  method readAsArrayBuffer : #blob t -> unit meth
  method readAsBinaryString : #blob t -> unit meth
  method readAsText : #blob t -> unit meth
  method readAsText_withEncoding : #blob t -> js_string t -> unit meth
  method readAsDataURL : #blob t -> unit meth

  method abort : unit meth

  method readyState : readyState readonly_prop

  method result : file_any readonly_prop
  method error : fileError t readonly_prop

  method onloadstart : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onprogress : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onload : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onabort : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onerror : ('self t, 'self progressEvent t) event_listener writeonly_prop
  method onloadend : ('self t, 'self progressEvent t) event_listener writeonly_prop

  inherit progressEventTarget
end

module ReaderEvent : sig
  type typ = fileReader progressEvent t Dom.Event.typ
  val loadstart : typ
  val progress : typ
  val abort : typ
  val error : typ
  val load : typ
  val loadend : typ
end

val filename : file t -> js_string t
(** [filename] handles old firefox without name property *)

val fileReader : fileReader t constr
(* be carefull, this might not be implemented in all browser.
   To check availability, use [Js.Optdef.to_option (Js.def fileReader)] *)

val readAsBinaryString : #blob t -> js_string t Lwt.t
val readAsText : #blob t -> js_string t Lwt.t
val readAsText_withEncoding  : #blob t -> js_string t -> js_string t Lwt.t
val readAsDataURL : #blob t -> js_string t Lwt.t

val addEventListener :
  (#progressEventTarget t as 'a) -> 'b Event.typ ->
  ('a, 'b) event_listener -> bool t -> event_listener_id
  (** Add an event listener.  This function matches the
      [addEventListener] DOM method, except that it returns
      an id for removing the listener. *)
