let bubble_nat =
  #| fn nat -> type {
  #|     (t : type) -> (s: (n: t) -> t) -> (z: t) -> t
  #| }

let bubble_add =
  #| fn add(a: nat) (b: nat) -> nat {
  #|     |t| { |s| { |z| { ((a t) s) (((b t) s) z) } } }
  #| }

let bubble_mul =
  #| fn mul(a: nat) (b: nat) -> nat {
  #|     |t| { |s| { |z| { ((a t) ((b t) s)) z } } }
  #| }
