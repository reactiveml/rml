(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2012 Jacques-Pascal Deplaix
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

type readyState =
  | CONNECTING
  | OPEN
  | CLOSING
  | CLOSED

class type ['a] closeEvent = object
  inherit ['a] Dom.event

  method code : int Js.readonly_prop
  method reason : Js.js_string Js.t Js.readonly_prop
  method wasClean : bool Js.t Js.readonly_prop
end

class type ['a] messageEvent = object
  inherit ['a] Dom.event

  method data : Js.js_string Js.t Js.readonly_prop
end

class type webSocket = object ('self)
  inherit Dom_html.eventTarget

  method url : Js.js_string Js.t Js.readonly_prop

  method readyState : readyState Js.readonly_prop
  method bufferedAmount : int Js.readonly_prop

  method onopen :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop
  method onclose :
    ('self Js.t, 'self closeEvent Js.t) Dom.event_listener Js.writeonly_prop
  method onerror :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method extensions : Js.js_string Js.t Js.readonly_prop
  method protocol : Js.js_string Js.t Js.readonly_prop
  method close : unit Js.meth
  method close_withCode : int -> unit Js.meth
  method close_withCodeAndReason :
    int -> Js.js_string Js.t -> unit Js.meth

  method onmessage :
    ('self Js.t, 'self messageEvent Js.t) Dom.event_listener Js.writeonly_prop
  method binaryType : Js.js_string Js.t Js.prop
  method send : Js.js_string Js.t -> unit Js.meth
end

val webSocket :
  (Js.js_string Js.t -> webSocket Js.t) Js.constr
val webSocket_withProtocol :
  (Js.js_string Js.t -> Js.js_string Js.t -> webSocket Js.t) Js.constr

val is_supported : unit -> bool
