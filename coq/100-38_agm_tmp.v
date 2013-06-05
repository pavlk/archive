Require Import Rbase.
Require Import Rpow_def.
Require Import Rprod.
Require Import Rfunctions.
Require Import RIneq.
Require Import Coq.fourier.Fourier.
Require Export Coq.micromega.Psatz.
Require Import Classical_Prop.

AddPath "~/src/cpdt/src" as. 
Require Import CpdtTactics.

Open Scope R_scope. 
Infix "^" := pow : R_scope. 

Lemma agm_lemma2b : forall (n:nat) (x:nat -> R),
  (forall i:nat, (i <= n)%nat -> x i > 0 /\ x i = x O)
   -> prod_f_R0 x n = (x O)^(n+1).
Proof.
intros. induction n as [|ni]. info auto with *. simpl. rewrite Rmult_comm.
replace (x (S ni)) with (x O). f_equal. auto.  symmetry. apply H. trivial.
Qed.

Lemma agm_lemma2c : forall (n:nat) (x:nat -> R),
  (forall i:nat, (i <= n)%nat -> x i > 0 /\ x i = x O)
   -> sum_f_R0 x n / INR (n + 1) = (x O).
Proof.
intros. induction n as [|ni]. simpl. unfold Rdiv. rewrite Rinv_1. auto with *.
replace (S ni + 1)%nat with (ni + 2)%nat. simpl.
replace (sum_f_R0 x ni) with (INR (ni + 1) * x 0%nat).
replace (x (S ni)) with (x O). symmetry.
replace (INR (ni + 1) * x 0%nat + x 0%nat) with (INR (S ni) * x 0%nat + 1 * x 0%nat).
rewrite S_INR. rewrite <- Rmult_plus_distr_r.
replace (INR ni + 1+1) with (INR (ni + 2)).
rewrite <- Rmult_comm. unfold Rdiv. rewrite -> Rmult_assoc. rewrite Rinv_r.
rewrite Rmult_1_r. auto. assert (2 > 0). auto with *. apply Rgt_not_eq. auto with *.
rewrite plus_INR. simpl. rewrite -> Rplus_assoc. reflexivity.
rewrite Rmult_1_l. replace (ni + 1)%nat with (S ni). reflexivity. info omega.
symmetry. apply H. trivial. rewrite <- Rmult_1_r with (r:=sum_f_R0 x ni).
elim Rinv_r with (r:=INR (ni + 1)). symmetry. rewrite <- Rmult_comm.
replace (INR (ni + 1) * / INR (ni + 1) * sum_f_R0 x ni) with (INR (ni + 1) * sum_f_R0 x ni * / INR (ni + 1)).
rewrite Rmult_assoc. auto with *. rewrite -> Rmult_comm.  unfold Rdiv.
rewrite <- (Rmult_comm (/ INR (ni + 1))). auto with *. auto with *. omega.
Qed.

Lemma agm_lemma2 : forall (n:nat) (x:nat -> R),
  (forall i:nat, (i <= n)%nat -> x i > 0 /\ x i = x O)
    -> prod_f_R0 x n = (sum_f_R0 x n/(INR (n+1)))^(n+1).
Proof. intros. rewrite agm_lemma2b.  rewrite agm_lemma2c; trivial. trivial. Qed.

Lemma agm_lema1R : forall a b x: R, 0 < a <= b /\ x > 0 -> (a <= x <= b <-> a * b + x * x <= x * b + x * a).
Proof. intros. intuition. Admitted. (* psatz R 2. Qed. *)

Lemma agm_lema1 : forall a b x: R,
0 < a <= b /\ x > 0 -> (a <= x <= b <-> a*b/x + x <= a+b).
Proof. Admitted.

Lemma agm_lema11 : forall a b x: R,
0 < a <= b /\ x > 0 -> (a <= x <= b <-> a*b/x <= a+b).
Proof. Admitted.


Lemma agm_lemma2x : forall (n:nat) (x:nat -> R),
  ~(forall i:nat, (i <= n)%nat -> x i > 0 /\ x i = x O)
   -> prod_f_R0 x n <= (sum_f_R0 x n/(INR (n+1)))^(n+1).
Proof. Admitted.


Lemma agm_help : forall (n:nat) (x:nat -> R),
              (n < 2)%nat  -> (forall i : nat, (i <= n)%nat) -> 
                    (~(forall i:nat, (i <= n)%nat -> x i = x O)
                        -> (x 0%nat < x 1%nat) \/ (x 1%nat < x 0%nat)).
Proof.
intuition. Admitted.

Lemma agm_lemma2y : forall (n:nat) (x:nat -> R),
  (forall i:nat, (i <= n)%nat -> x i > 0 /\ x i = x O) ->
     (~(forall i:nat, (i <= n)%nat -> x i = x O)
        -> prod_f_R0 x n <= (sum_f_R0 x n/(INR (n+1)))^(n+1)).
Proof.
intros.  case (le_lt_dec 2 n). 
Focus 2. intuition. intros. 
replace (prod_f_R0 x n <= (sum_f_R0 x n/(INR (n+1)))^(n+1)) with ((x 0%nat * x 1%nat) / ((x 0%nat + x 1%nat) / (4) ) <= (x 0%nat + x 1%nat) ).
apply agm_lema11 with (a:= x 0%nat) (b:= x (S 0)%nat).
intuition.  apply H.
omega. cut (x 0%nat < x 1%nat).
auto with *. Admitted. (* TODO: apply  agm_help.
Focus 2. 

apply H.
 intros.
 apply Rge_refl. apply H. omega.*)


Lemma agm_help : ~(forall i : nat, (n < 2)%nat, (i <= n)%nat -> x i = x 0%nat) -> (x 0%nat < x 1%nat) \/ (x 1%nat < x 0%nat).

(* AGM inequality theorem. *)
Theorem agm : forall (n:nat) (x: nat -> R), 
 (forall i:nat, (i <= n)%nat -> x i > 0) -> prod_f_R0 x n <= ( sum_f_R0 x n / (INR (n+1)) )^(n+1).
Proof.
intuition. cut ((forall i:nat, (i <= n)%nat -> x i > 0 /\ x i = x O) \/ ~(forall i:nat, (i <= n)%nat -> x i > 0 /\ x i = x O)).
intuition. rewrite agm_lemma2. apply Rge_refl.
trivial.  apply agm_lemma2y. trivial. auto. tauto.
Qed.

(* -------------------------------------- *)

Open Scope Z_scope.
Lemma ex1 : forall x, (x-2)*(x-1) <= 0 -> x <= 3.
Admitted. (* pintros; psatz Z. Qed. *)

Lemma ex2 : forall x, 0 < (3 * x - 4) * (3 * x - 5).
intros x.
case (Z_le_dec 2 x). intros x2.
apply Zmult_lt_0_compat; omega.
replace ((3*x-4)*(3*x-5)) with ((4-3*x)*(5-3*x)) by ring.
intros x2.
apply Zmult_lt_0_compat; omega.
Qed.

Lemma agm_lema1bZ: forall a b x: Z,
(0 < a <= b /\ x > 0) -> (a * b + x * x <= x * b + x * a) -> (a <= x <= b).
Proof. Admitted. (* intros. psatz Z 2. Qed. *)

Lemma agm_lema1Z : forall a b x: Z, 0 < a <= b /\ x > 0 -> (a <= x <= b <-> a * b + x * x <= x * b + x * a).
Proof. Admitted. (* intros. psatz Z 2. Qed. *)