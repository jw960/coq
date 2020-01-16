Require Import Reals.
Require Import ZArith.
From Flocq Require Import Raux Defs.

Notation radix2 := Zaux.radix2.
Record float2 : Set := Float2 { Fnum : Z ; Fexp : Z }.
Coercion float2R (x : float2) := F2R (Float radix2 (Fnum x) (Fexp x)).
Definition radix10 := Build_radix 10 (refl_equal true).
Record float10 : Set := Float10 { Fnum10 : Z ; Fexp10 : Z }.
Coercion float10R (x : float10) := F2R (Float radix10 (Fnum10 x) (Fexp10 x)).

Record FF: Set := makepairF { lower : float2 ; upper : float2 }.

Definition BND (x : R) (xi : FF) :=
 (lower xi <= x <= upper xi)%R.
Definition ABS (x : R) (xi : FF) :=
 (0 <= lower xi)%R /\ (lower xi <= Rabs x <= upper xi)%R.
Definition REL (x1 x2 : R) (xi : FF) :=
 exists x : R, (lower xi <= x <= upper xi)%R /\ (x1 = x2 * (1 + x))%R.
Definition FIX (x : R) (n : Z) :=
 exists f : float2, float2R f = x /\ (n <= Fexp f)%Z.
Definition FLT (x : R) (n : positive) :=
 exists f : float2, float2R f = x /\ (Z.abs (Fnum f) < Zpower_pos 2 n)%Z.
Definition NZR (x : R) := (x <> 0)%R.
