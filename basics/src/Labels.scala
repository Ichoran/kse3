// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023-24 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.labels


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.compiletime.{codeOf, summonInline, summonFrom}
import scala.annotation.targetName

import kse.basics.{LabelVal, \ => \^, \< => \<^, \> => \>^ }


object LabelConflicts {
  inline def head2[L <: LabelVal, L1 <: LabelVal](inline msg: String): Unit = summonFrom:
    case _: (L =:= L1) =>
      compiletime.error("Label " + msg + " is not unique")
    case _ => ()

  inline def head3[L <: LabelVal, L1 <: LabelVal, L2 <: LabelVal](inline msg: String): Unit = summonFrom:
    case _: ((L | L1 | L2) =:= (L1 | L2)) =>
      compiletime.error("Label " + msg + " is not unique")
    case _ => ()

  inline def head4[L <: LabelVal, L1 <: LabelVal, L2 <: LabelVal, L3 <: LabelVal](inline msg: String): Unit = summonFrom:
    case _: ((L | L1 | L2 | L3) =:= (L1 | L2 | L3)) =>
      compiletime.error("Label " + msg + " is not unique")
    case _ => ()

  inline def head5[L <: LabelVal, L1 <: LabelVal, L2 <: LabelVal, L3 <: LabelVal, L4 <: LabelVal](inline msg: String): Unit = summonFrom:
    case _: ((L | L1 | L2 | L3 | L4) =:= (L1 | L2 | L3 | L4)) =>
      compiletime.error("Label " + msg + " is not unique")
    case _ => ()

  inline def head6[L <: LabelVal, L1 <: LabelVal, L2 <: LabelVal, L3 <: LabelVal, L4 <: LabelVal, L5 <: LabelVal](inline msg: String): Unit = summonFrom:
    case _: ((L | L1 | L2 | L3 | L4 | L5) =:= (L1 | L2 | L3 | L4 | L5)) =>
      compiletime.error("Label " + msg + " is not unique")
    case _ => ()

  inline def head7[L <: LabelVal, L1 <: LabelVal, L2 <: LabelVal, L3 <: LabelVal, L4 <: LabelVal, L5 <: LabelVal, L6 <: LabelVal](inline msg: String): Unit = summonFrom:
    case _: ((L | L1 | L2 | L3 | L4 | L5 | L6) =:= (L1 | L2 | L3 | L4 | L5 | L6)) =>
      compiletime.error("Label " + msg + " is not unique")
    case _ => ()

  inline def head8[L <: LabelVal, L1 <: LabelVal, L2 <: LabelVal, L3 <: LabelVal, L4 <: LabelVal, L5 <: LabelVal, L6 <: LabelVal, L7 <: LabelVal](inline msg: String): Unit = summonFrom:
    case _: ((L | L1 | L2 | L3 | L4 | L5 | L6 | L7) =:= (L1 | L2 | L3 | L4 | L5 | L6 | L7)) =>
      compiletime.error("Label " + msg + " is not unique")
    case _ => ()

  inline def head9[L <: LabelVal, L1 <: LabelVal, L2 <: LabelVal, L3 <: LabelVal, L4 <: LabelVal, L5 <: LabelVal, L6 <: LabelVal, L7 <: LabelVal, L8 <: LabelVal](inline msg: String): Unit = summonFrom:
    case _: ((L | L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8) =:= (L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8)) =>
      compiletime.error("Label " + msg + " is not unique")
    case _ => ()

  inline def uniq2[La <: LabelVal, Lb <: LabelVal](inline m1: String): Unit =
    head2[La, Lb](m1)
  inline def uniq3[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal](inline m1: String, inline m2: String): Unit =
    head3[La, Lb, Lc](m1)
    head2[    Lb, Lc](m2)
  inline def uniq4[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal](inline m1: String, inline m2: String, inline m3: String): Unit =
    head4[La, Lb, Lc, Ld](m1)
    head3[    Lb, Lc, Ld](m2)
    head2[        Lc, Ld](m3)
  inline def uniq5[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal](inline m1: String, inline m2: String, inline m3: String, inline m4: String): Unit =
    head5[La, Lb, Lc, Ld, Le](m1)
    head4[    Lb, Lc, Ld, Le](m2)
    head3[        Lc, Ld, Le](m3)
    head2[            Ld, Le](m4)
  inline def uniq6[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String): Unit =
    head6[La, Lb, Lc, Ld, Le, Lf](m1)
    head5[    Lb, Lc, Ld, Le, Lf](m2)
    head4[        Lc, Ld, Le, Lf](m3)
    head3[            Ld, Le, Lf](m4)
    head2[                Le, Lf](m5)
  inline def uniq7[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String): Unit =
    head7[La, Lb, Lc, Ld, Le, Lf, Lg](m1)
    head6[    Lb, Lc, Ld, Le, Lf, Lg](m2)
    head5[        Lc, Ld, Le, Lf, Lg](m3)
    head4[            Ld, Le, Lf, Lg](m4)
    head3[                Le, Lf, Lg](m5)
    head2[                    Lf, Lg](m6)
  inline def uniq8[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String, inline m7: String): Unit =
    head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](m1)
    head7[    Lb, Lc, Ld, Le, Lf, Lg, Lh](m2)
    head6[        Lc, Ld, Le, Lf, Lg, Lh](m3)
    head5[            Ld, Le, Lf, Lg, Lh](m4)
    head4[                Le, Lf, Lg, Lh](m5)
    head3[                    Lf, Lg, Lh](m6)
    head2[                        Lg, Lh](m7)
  inline def uniq9[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String, inline m7: String, inline m8: String): Unit =
    head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](m1)
    head8[    Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](m2)
    head7[        Lc, Ld, Le, Lf, Lg, Lh, Li](m3)
    head6[            Ld, Le, Lf, Lg, Lh, Li](m4)
    head5[                Le, Lf, Lg, Lh, Li](m5)
    head4[                    Lf, Lg, Lh, Li](m6)
    head3[                        Lg, Lh, Li](m7)
    head2[                            Lh, Li](m8)

  inline def miss1[La <: LabelVal, Tz](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => compiletime.error("Duplicate labels found" + msg)
      case _ =>
  inline def miss2[La <: LabelVal, Tz, Ty](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => compiletime.error("Duplicate labels found" + msg)
      case _ =>
  inline def miss3[La <: LabelVal, Tz, Ty, Tx](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => compiletime.error("Duplicate labels found" + msg)
      case _ =>
  inline def miss4[La <: LabelVal, Tz, Ty, Tx, Tw](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => compiletime.error("Duplicate labels found" + msg)
      case _ =>
  inline def miss5[La <: LabelVal, Tz, Ty, Tx, Tw, Tv](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => compiletime.error("Duplicate labels found" + msg)
      case _ =>
  inline def miss6[La <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => compiletime.error("Duplicate labels found" + msg)
      case _ =>
  inline def miss7[La <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => compiletime.error("Duplicate labels found" + msg)
      case _ =>
  inline def miss8[La <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => compiletime.error("Duplicate labels found" + msg)
      case _ =>
  inline def miss9[La <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](inline msg: String): Unit = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => compiletime.error("Duplicate labels found" + msg)
      case _ =>

  inline def has11[La <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _ => false
  inline def has12[La <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _ => false
  inline def has13[La <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has14[La <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has15[La <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has16[La <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has17[La <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has18[La <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has19[La <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
  inline def has21[La <: LabelVal, Lb <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _: ((Nothing \^ Lb) <:< Tz) => true
      case _ => false
  inline def has22[La <: LabelVal, Lb <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty)) => true
      case _ => false
  inline def has23[La <: LabelVal, Lb <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has24[La <: LabelVal, Lb <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has25[La <: LabelVal, Lb <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has26[La <: LabelVal, Lb <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has27[La <: LabelVal, Lb <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has28[La <: LabelVal, Lb <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has29[La <: LabelVal, Lb <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
  inline def has31[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _: ((Nothing \^ Lb) <:< Tz) => true
      case _: ((Nothing \^ Lc) <:< Tz) => true
      case _ => false
  inline def has32[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty)) => true
      case _ => false
  inline def has33[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has34[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has35[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has36[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has37[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has38[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has39[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
  inline def has41[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _: ((Nothing \^ Lb) <:< Tz) => true
      case _: ((Nothing \^ Lc) <:< Tz) => true
      case _: ((Nothing \^ Ld) <:< Tz) => true
      case _ => false
  inline def has42[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty)) => true
      case _ => false
  inline def has43[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has44[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has45[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has46[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has47[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has48[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has49[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
  inline def has51[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _: ((Nothing \^ Lb) <:< Tz) => true
      case _: ((Nothing \^ Lc) <:< Tz) => true
      case _: ((Nothing \^ Ld) <:< Tz) => true
      case _: ((Nothing \^ Le) <:< Tz) => true
      case _ => false
  inline def has52[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty)) => true
      case _ => false
  inline def has53[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has54[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has55[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has56[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has57[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has58[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has59[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
  inline def has61[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _: ((Nothing \^ Lb) <:< Tz) => true
      case _: ((Nothing \^ Lc) <:< Tz) => true
      case _: ((Nothing \^ Ld) <:< Tz) => true
      case _: ((Nothing \^ Le) <:< Tz) => true
      case _: ((Nothing \^ Lf) <:< Tz) => true
      case _ => false
  inline def has62[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty)) => true
      case _ => false
  inline def has63[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has64[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has65[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has66[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has67[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has68[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has69[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
  inline def has71[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _: ((Nothing \^ Lb) <:< Tz) => true
      case _: ((Nothing \^ Lc) <:< Tz) => true
      case _: ((Nothing \^ Ld) <:< Tz) => true
      case _: ((Nothing \^ Le) <:< Tz) => true
      case _: ((Nothing \^ Lf) <:< Tz) => true
      case _: ((Nothing \^ Lg) <:< Tz) => true
      case _ => false
  inline def has72[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty)) => true
      case _ => false
  inline def has73[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has74[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has75[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has76[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has77[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has78[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has79[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
  inline def has81[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _: ((Nothing \^ Lb) <:< Tz) => true
      case _: ((Nothing \^ Lc) <:< Tz) => true
      case _: ((Nothing \^ Ld) <:< Tz) => true
      case _: ((Nothing \^ Le) <:< Tz) => true
      case _: ((Nothing \^ Lf) <:< Tz) => true
      case _: ((Nothing \^ Lg) <:< Tz) => true
      case _: ((Nothing \^ Lh) <:< Tz) => true
      case _ => false
  inline def has82[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty)) => true
      case _ => false
  inline def has83[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has84[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has85[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has86[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has87[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has88[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has89[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
  inline def has91[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => true
      case _: ((Nothing \^ Lb) <:< Tz) => true
      case _: ((Nothing \^ Lc) <:< Tz) => true
      case _: ((Nothing \^ Ld) <:< Tz) => true
      case _: ((Nothing \^ Le) <:< Tz) => true
      case _: ((Nothing \^ Lf) <:< Tz) => true
      case _: ((Nothing \^ Lg) <:< Tz) => true
      case _: ((Nothing \^ Lh) <:< Tz) => true
      case _: ((Nothing \^ Li) <:< Tz) => true
      case _ => false
  inline def has92[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz, Ty]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty)) => true
      case _: ((Nothing \^ Li) <:< (Tz | Ty)) => true
      case _ => false
  inline def has93[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz, Ty, Tx]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx)) => true
      case _: ((Nothing \^ Li) <:< (Tz | Ty | Tx)) => true
      case _ => false
  inline def has94[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz, Ty, Tx, Tw]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw)) => true
      case _: ((Nothing \^ Li) <:< (Tz | Ty | Tx | Tw)) => true
      case _ => false
  inline def has95[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz, Ty, Tx, Tw, Tv]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _: ((Nothing \^ Li) <:< (Tz | Ty | Tx | Tw | Tv)) => true
      case _ => false
  inline def has96[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _: ((Nothing \^ Li) <:< (Tz | Ty | Tx | Tw | Tv | Tu)) => true
      case _ => false
  inline def has97[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _: ((Nothing \^ Li) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt)) => true
      case _ => false
  inline def has98[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _: ((Nothing \^ Li) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts)) => true
      case _ => false
  inline def has99[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr]: Boolean = summonFrom:
      case _: ((Nothing \^ La) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lb) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lc) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Ld) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Le) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lf) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lg) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Lh) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _: ((Nothing \^ Li) <:< (Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr)) => true
      case _ => false
}


extension [A](a: A) {
  /** Associate a compile-time name with this value */
  inline def labelled[L <: LabelVal]: (A \^ L) = \^(a)

  /** Associate a compile-time name with this value by giving the other (Singular) value */
  inline def \[L <: LabelVal](l: L): A \^ L = \^(a)

  /** Associate a compile-time name with this value where the labelled version is a subtype of the original */
  inline def sublabelled[L <: LabelVal]: (A \<^ L) = \<^(a)

  /** Associate a compile-time name with this value, where it is a subtype of its original type, by giving a name */
  inline def \<[L <: LabelVal](l: L): A \<^ L = \<^(a)

  /** Associate a compile-time name with this value where the labelled version is a supertype of the original */
  inline def superlabelled[L <: LabelVal]: (A \>^ L) = \>^(a)

  /** Associate a compile-time name with this value, where it is a supertype of its original type, by giving a name */
  inline def \>[L <: LabelVal](l: L): A \>^ L = \>^(a)
}

inline def conjure[A, L <: LabelVal](l: L)(using inline nm: ((A \^ L) | (A \<^ L) | (A \>^ L))): A = inline nm match
  case nt: (A \^ L)  => nt.unlabel
  case sb: (A \<^ L) => sb
  case sp: (A \>^ L) => sp.unlabel
  case _ => nm.asInstanceOf[A \<^ L]   // Cheating, but they're all the same type anyway, so....

extension [A, La <: LabelVal](q: A \^ La) {
  inline def updatedBy[Nz](source: Nz): A \^ La = summonFrom:
    case _: ((Nothing \^ La) <:< Nz) => summonInline[(Nz <:< (A \^ La))](source)
    case _ => compiletime.error("No matching label found")

  inline def updatedBy[Nz, Ny](source: (Nz, Ny)): A \^ La = summonFrom:
    case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.miss1[La, Ny](" in update of labeled value")
                                         summonInline[(Nz <:< (A \^ La))](source._1)
    case _ : ((Nothing \^ La) <:< Ny) => summonInline[(Ny <:< (A \^ La))](source._2)
    case _ => compiletime.error("No matching label found")

  inline def updatedBy[Nz, Ny, Nx](source: (Nz, Ny, Nx)): A \^ La = summonFrom:
    case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.miss2[La, Ny, Nx](" in update of labeled value")
                                         summonInline[(Nz <:< (A \^ La))](source._1)
    case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.miss1[La, Nx](" in update of labeled value")
                                         summonInline[(Ny <:< (A \^ La))](source._2)
    case _ : ((Nothing \^ La) <:< Nx) => summonInline[(Nx <:< (A \^ La))](source._3)
    case _ => compiletime.error("No matching label found")

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz, Ny, Nx, Nw)): A \^ La = summonFrom:
    case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.miss3[La, Ny, Nx, Nw](" in update of labeled value")
                                         summonInline[(Nz <:< (A \^ La))](source._1)
    case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.miss2[La, Nx, Nw](" in update of labeled value")
                                         summonInline[(Ny <:< (A \^ La))](source._2)
    case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.miss1[La, Nw](" in update of labeled value")
                                         summonInline[(Nx <:< (A \^ La))](source._3)
    case _ : ((Nothing \^ La) <:< Nw) => summonInline[(Nw <:< (A \^ La))](source._4)
    case _ => compiletime.error("No matching label found")

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz, Ny, Nx, Nw, Nv)): A \^ La = summonFrom:
    case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update of labeled value")
                                         summonInline[(Nz <:< (A \^ La))](source._1)
    case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.miss3[La, Nx, Nw, Nv](" in update of labeled value")
                                         summonInline[(Ny <:< (A \^ La))](source._2)
    case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.miss2[La, Nw, Nv](" in update of labeled value")
                                         summonInline[(Nx <:< (A \^ La))](source._3)
    case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.miss1[La, Nv](" in update of labeled value")
                                         summonInline[(Nw <:< (A \^ La))](source._4)
    case _ : ((Nothing \^ La) <:< Nv) => summonInline[(Nv <:< (A \^ La))](source._5)
    case _ => compiletime.error("No matching label found")

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz, Ny, Nx, Nw, Nv, Nu)): A \^ La = summonFrom:
    case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update of labeled value")
                                         summonInline[(Nz <:< (A \^ La))](source._1)
    case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update of labeled value")
                                         summonInline[(Ny <:< (A \^ La))](source._2)
    case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.miss3[La, Nw, Nv, Nu](" in update of labeled value")
                                         summonInline[(Nx <:< (A \^ La))](source._3)
    case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.miss2[La, Nv, Nu](" in update of labeled value")
                                         summonInline[(Nw <:< (A \^ La))](source._4)
    case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.miss1[La, Nu](" in update of labeled value")
                                         summonInline[(Nv <:< (A \^ La))](source._5)
    case _ : ((Nothing \^ La) <:< Nu) => summonInline[(Nu <:< (A \^ La))](source._6)
    case _ => compiletime.error("No matching label found")

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz, Ny, Nx, Nw, Nv, Nu, Nt)): A \^ La = summonFrom:
    case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update of labeled value")
                                         summonInline[(Nz <:< (A \^ La))](source._1)
    case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update of labeled value")
                                         summonInline[(Ny <:< (A \^ La))](source._2)
    case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update of labeled value")
                                         summonInline[(Nx <:< (A \^ La))](source._3)
    case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.miss3[La, Nv, Nu, Nt](" in update of labeled value")
                                         summonInline[(Nw <:< (A \^ La))](source._4)
    case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.miss2[La, Nu, Nt](" in update of labeled value")
                                         summonInline[(Nv <:< (A \^ La))](source._5)
    case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.miss1[La, Nt](" in update of labeled value")
                                         summonInline[(Nu <:< (A \^ La))](source._6)
    case _ : ((Nothing \^ La) <:< Nt) => summonInline[(Nt <:< (A \^ La))](source._7)
    case _ => compiletime.error("No matching label found")

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns)): A \^ La = summonFrom:
    case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update of labeled value")
                                         summonInline[(Nz <:< (A \^ La))](source._1)
    case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update of labeled value")
                                         summonInline[(Ny <:< (A \^ La))](source._2)
    case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update of labeled value")
                                         summonInline[(Nx <:< (A \^ La))](source._3)
    case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update of labeled value")
                                         summonInline[(Nw <:< (A \^ La))](source._4)
    case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.miss3[La, Nu, Nt, Ns](" in update of labeled value")
                                         summonInline[(Nv <:< (A \^ La))](source._5)
    case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.miss2[La, Nt, Ns](" in update of labeled value")
                                         summonInline[(Nu <:< (A \^ La))](source._6)
    case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.miss1[La, Ns](" in update of labeled value")
                                         summonInline[(Nt <:< (A \^ La))](source._7)
    case _ : ((Nothing \^ La) <:< Ns) => summonInline[(Ns <:< (A \^ La))](source._8)
    case _ => compiletime.error("No matching label found")

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr)): A \^ La = summonFrom:
    case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update of labeled value")
                                         summonInline[(Nz <:< (A \^ La))](source._1)
    case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update of labeled value")
                                         summonInline[(Ny <:< (A \^ La))](source._2)
    case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update of labeled value")
                                         summonInline[(Nx <:< (A \^ La))](source._3)
    case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update of labeled value")
                                         summonInline[(Nw <:< (A \^ La))](source._4)
    case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update of labeled value")
                                         summonInline[(Nv <:< (A \^ La))](source._5)
    case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.miss3[La, Nt, Ns, Nr](" in update of labeled value")
                                         summonInline[(Nu <:< (A \^ La))](source._6)
    case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.miss2[La, Ns, Nr](" in update of labeled value")
                                         summonInline[(Nt <:< (A \^ La))](source._7)
    case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.miss1[La, Nr](" in update of labeled value")
                                         summonInline[(Ns <:< (A \^ La))](source._8)
    case _ : ((Nothing \^ La) <:< Nr) => summonInline[(Nr <:< (A \^ La))](source._9)
    case _ => compiletime.error("No matching label found")
}


extension [A, La <: LabelVal, B, Lb <: LabelVal](q: (A \^ La, B \^ Lb)) {
  transparent inline def ~(inline name: La | Lb): A | B = inline name match
      case _: La =>
        LabelConflicts.head2[La, Lb](codeOf(name))
        q._1.unlabel
      case _: Lb =>
        q._2.unlabel

  inline def unlabel: (A, B) = q.asInstanceOf[(A, B)]

  transparent inline def label_1: La = compiletime.constValue[La]

  transparent inline def label_2: Lb = compiletime.constValue[Lb]

  transparent inline def labels: (La, Lb) = (compiletime.constValue[La], compiletime.constValue[Lb])

  inline def ~~(inline la: La, inline lb: Lb): (A, B) =
    LabelConflicts.uniq2[La, Lb](codeOf(la))
    unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B) \^ Lz =
    summonFrom:
      case _: (Lz =:= La) => LabelConflicts.head2[Lz, Lb](codeOf(lz))
                             q._1.asInstanceOf[A \^ Lz]
      case _: (Lz =:= Lb) => q._2.asInstanceOf[B \^ Lz]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): q.type | ((A | B) \^ Lz, (A | B) \^ Ly) =
    LabelConflicts.uniq2[Lz, Ly](codeOf(lz))
    summonFrom:
      case _: ((Lz, Ly) =:= (La, Lb)) => q
      case _ => (
        summonFrom:
          case _: (Lz =:= La) => LabelConflicts.head2[Lz, Lb](codeOf(lz))
                                 q._1.asInstanceOf[A \^ Lz]
          case _: (Lz =:= Lb) => q._2.asInstanceOf[B \^ Lz]
          case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
        summonFrom:
          case _: (Ly =:= La) => LabelConflicts.head2[Ly, Lb](codeOf(ly))
                                 q._1.asInstanceOf[A \^ Ly]
          case _: (Ly =:= Lb) => q._2.asInstanceOf[B \^ Ly]
          case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      )

  inline def updatedBy[Z, Lz <: LabelVal](source: Z \^ Lz): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has21[La, Lb, Z \^ Lz] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head2[Lz, Lb]("at _1")
                               summonInline[Z <:< A](source.unlabel).labelled[La]
        case _ => q._1,
      summonFrom:
        case _: (Lz =:= Lb) => summonInline[Z <:< B](source.unlabel).labelled[Lb]
        case _ => q._2
    )

  inline def updatedBy[Nz, Ny](source: (Nz,Ny)): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has22[La, Lb, Nz, Ny] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss1[La, Ny](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head2[La, Lb]("at _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.miss1[Lb, Ny](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ => q._2
     )

  inline def updatedBy[Nz, Ny, Nx](source: (Nz,Ny,Nx)): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has23[La, Lb, Nz, Ny, Nx] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss2[La, Ny, Nx](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss1[La, Nx](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head2[La, Lb]("at _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.miss2[Lb, Ny, Nx](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.miss1[Lb, Nx](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ => q._2
     )

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz,Ny,Nx,Nw)): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has24[La, Lb, Nz, Ny, Nx, Nw] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss3[La, Ny, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss2[La, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss1[La, Nw](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head2[La, Lb]("at _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.miss3[Lb, Ny, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.miss2[Lb, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.miss1[Lb, Nw](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ => q._2
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz,Ny,Nx,Nw,Nv)): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has25[La, Lb, Nz, Ny, Nx, Nw, Nv] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss3[La, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss2[La, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss1[La, Nv](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head2[La, Lb]("at _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.miss4[Lb, Ny, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.miss3[Lb, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.miss2[Lb, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.miss1[Lb, Nv](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ => q._2
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz,Ny,Nx,Nw,Nv,Nu)): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has26[La, Lb, Nz, Ny, Nx, Nw, Nv, Nu] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss3[La, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss2[La, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss1[La, Nu](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head2[La, Lb]("at _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.miss5[Lb, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.miss4[Lb, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.miss3[Lb, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.miss2[Lb, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.miss1[Lb, Nu](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ => q._2
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt)): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has27[La, Lb, Nz, Ny, Nx, Nw, Nv, Nu, Nt] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss3[La, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss2[La, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss1[La, Nt](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head2[La, Lb]("at _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.miss6[Lb, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.miss5[Lb, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.miss4[Lb, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.miss3[Lb, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.miss2[Lb, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.miss1[Lb, Nt](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ => q._2
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns)): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has28[La, Lb, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss3[La, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss2[La, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss1[La, Ns](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head2[La, Lb]("at _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.miss7[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.miss6[Lb, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.miss5[Lb, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.miss4[Lb, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.miss3[Lb, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.miss2[Lb, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.miss1[Lb, Ns](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ => q._2
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns,Nr)): (A \^ La, B \^ Lb) =
    inline if !LabelConflicts.has29[La, Lb, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss3[La, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss2[La, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head2[La, Lb]("at _1")
                                             LabelConflicts.miss1[La, Nr](" in update corresponding to _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ : ((Nothing \^ La) <:< Nr) => LabelConflicts.head2[La, Lb]("at _1")
                                             summonInline[(Nr <:< (A \^ La))](source._9)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.miss8[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.miss7[Lb, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.miss6[Lb, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.miss5[Lb, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.miss4[Lb, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.miss3[Lb, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.miss2[Lb, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.miss1[Lb, Nr](" in update corresponding to _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ : ((Nothing \^ Lb) <:< Nr) => summonInline[(Nr <:< (B \^ Lb))](source._9)
        case _ => q._2
     )

  transparent inline def revalue[Z](inline name: La | Lb)(to: Z):
    (Z \^ La, B \^ Lb) | 
    (A \^ La, Z \^ Lb)
    =
    inline name match
      case _: La =>
        LabelConflicts.head2[La, Lb](codeOf(name))
        q.copy(_1 = to.labelled[La])
      case _: Lb =>
        q.copy(_2 = to.labelled[Lb])

  transparent inline def relabel[Lz <: LabelVal](inline name: La | Lb)(inline lz: Lz):
    (A \^ Lz, B \^ Lb) | 
    (A \^ La, B \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head2[La, Lb](codeOf(name))
        LabelConflicts.head2[Lz, Lb](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ Lz, B \^ Lb)]
      case _: Lb =>
        LabelConflicts.head2[Lz, La](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lz)]

  transparent inline def redo[Z, Lz <: LabelVal](inline name: La | Lb)(inline to: Z \^ Lz):
    (Z \^ Lz, B \^ Lb) | 
    (A \^ La, Z \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head2[La, Lb](codeOf(name))
        LabelConflicts.head2[Lz, Lb](codeOf(name) + " changed, creating a labelling which")
        q.copy(_1 = to)
      case _: Lb =>
        LabelConflicts.head2[Lz, La](codeOf(name) + " changed, creating a labelling which")
        q.copy(_2 = to)
}

extension [A, B](q: (A, B)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal]: (A \^ La, B \^ Lb) =
    LabelConflicts.uniq2[La, Lb]("_1")
    q.asInstanceOf[(A \^ La, B \^ Lb)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal](inline la: La, inline lb: Lb): (A \^ La, B \^ Lb) =
    LabelConflicts.uniq2[La, Lb](codeOf(la))
    q.asInstanceOf[(A \^ La, B \^ Lb)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc)) {
  transparent inline def ~(inline name: La | Lb | Lc): A | B | C = inline name match
      case _: La =>
        LabelConflicts.head3[La, Lb, Lc](codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.head2[Lb, Lc](codeOf(name))
        q._2.unlabel
      case _: Lc =>
        q._3.unlabel

  inline def unlabel: (A, B, C) = q.asInstanceOf[(A, B, C)]

  transparent inline def label_1: La = compiletime.constValue[La]

  transparent inline def label_2: Lb = compiletime.constValue[Lb]

  transparent inline def label_3: Lc = compiletime.constValue[Lc]

  transparent inline def labels: (La, Lb, Lc) = (
    compiletime.constValue[La],
    compiletime.constValue[Lb],
    compiletime.constValue[Lc]
  )

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc): (A, B, C) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B | C) \^ Lz =
    summonFrom:
      case _: (Lz =:= La) => LabelConflicts.head3[Lz, Lb, Lc](codeOf(lz))
                             q._1.asInstanceOf[A \^ Lz]
      case _: (Lz =:= Lb) => LabelConflicts.head2[Lz, Lc](codeOf(lz))
                             q._2.asInstanceOf[B \^ Lz]
      case _: (Lz =:= Lc) => q._3.asInstanceOf[C \^ Lz]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C) \^ Lz, (A | B | C) \^ Ly) =
    LabelConflicts.uniq2[Lz, Ly](codeOf(lz))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head3[Lz, Lb, Lc](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head2[Lz, Lc](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => q._3.asInstanceOf[C \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head3[Ly, Lb, Lc](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head2[Ly, Lc](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => q._3.asInstanceOf[C \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): q.type | ((A | B | C) \^ Lz, (A | B | C) \^ Ly, (A | B | C) \^ Lx) =
    LabelConflicts.uniq3[Lz, Ly, Lx](codeOf(lz), codeOf(ly))
    summonFrom:
      case _: ((Lz, Ly, Lx) =:= (La, Lb, Lc)) => q
      case _ => (
        summonFrom:
          case _: (Lz =:= La) => LabelConflicts.head3[Lz, Lb, Lc](codeOf(lz))
                                 q._1.asInstanceOf[A \^ Lz]
          case _: (Lz =:= Lb) => LabelConflicts.head2[Lz, Lc](codeOf(lz))
                                 q._2.asInstanceOf[B \^ Lz]
          case _: (Lz =:= Lc) => q._3.asInstanceOf[C \^ Lz]
          case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
        summonFrom:
          case _: (Ly =:= La) => LabelConflicts.head3[Ly, Lb, Lc](codeOf(ly))
                                 q._1.asInstanceOf[A \^ Ly]
          case _: (Ly =:= Lb) => LabelConflicts.head2[Ly, Lc](codeOf(ly))
                                 q._2.asInstanceOf[B \^ Ly]
          case _: (Ly =:= Lc) => q._3.asInstanceOf[C \^ Ly]
          case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
        summonFrom:
          case _: (Lx =:= La) => LabelConflicts.head3[Lx, Lb, Lc](codeOf(lx))
                                 q._1.asInstanceOf[A \^ Lx]
          case _: (Lx =:= Lb) => LabelConflicts.head2[Lx, Lc](codeOf(lx))
                                 q._2.asInstanceOf[B \^ Lx]
          case _: (Lx =:= Lc) => q._3.asInstanceOf[C \^ Lx]
          case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      )

  inline def updatedBy[Z, Lz <: LabelVal](source: Z \^ Lz): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has31[La, Lb, Lc, Z \^ Lz] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head3[Lz, Lb, Lc]("at _1")
                               summonInline[Z <:< A](source.unlabel).labelled[La]
        case _ => q._1,
      summonFrom:
        case _: (Lz =:= Lb) => LabelConflicts.head2[Lz, Lc]("at _2")
                               summonInline[Z <:< B](source.unlabel).labelled[Lb]
        case _ => q._2,
      summonFrom:
        case _: (Lz =:= Lc) => summonInline[Z <:< C](source.unlabel).labelled[Lc]
        case _ => q._3
    )

  inline def updatedBy[Nz, Ny](source: (Nz,Ny)): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has32[La, Lb, Lc, Nz, Ny] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss1[La, Ny](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss1[Lb, Ny](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.miss1[Lc, Ny](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ => q._3
     )

  inline def updatedBy[Nz, Ny, Nx](source: (Nz,Ny,Nx)): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has33[La, Lb, Lc, Nz, Ny, Nx] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss2[La, Ny, Nx](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss1[La, Nx](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss2[Lb, Ny, Nx](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss1[Lb, Nx](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.miss2[Lc, Ny, Nx](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.miss1[Lc, Nx](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ => q._3
     )

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz,Ny,Nx,Nw)): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has34[La, Lb, Lc, Nz, Ny, Nx, Nw] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss3[La, Ny, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss2[La, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss1[La, Nw](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss3[Lb, Ny, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss2[Lb, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss1[Lb, Nw](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.miss3[Lc, Ny, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.miss2[Lc, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.miss1[Lc, Nw](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ => q._3
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz,Ny,Nx,Nw,Nv)): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has35[La, Lb, Lc, Nz, Ny, Nx, Nw, Nv] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss3[La, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss2[La, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss1[La, Nv](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss4[Lb, Ny, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss3[Lb, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss2[Lb, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss1[Lb, Nv](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.miss4[Lc, Ny, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.miss3[Lc, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.miss2[Lc, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.miss1[Lc, Nv](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ => q._3
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz,Ny,Nx,Nw,Nv,Nu)): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has36[La, Lb, Lc, Nz, Ny, Nx, Nw, Nv, Nu] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss3[La, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss2[La, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss1[La, Nu](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss5[Lb, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss4[Lb, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss3[Lb, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss2[Lb, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss1[Lb, Nu](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.miss5[Lc, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.miss4[Lc, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.miss3[Lc, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.miss2[Lc, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.miss1[Lc, Nu](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ => q._3
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt)): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has37[La, Lb, Lc, Nz, Ny, Nx, Nw, Nv, Nu, Nt] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss3[La, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss2[La, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss1[La, Nt](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss6[Lb, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss5[Lb, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss4[Lb, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss3[Lb, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss2[Lb, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss1[Lb, Nt](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.miss6[Lc, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.miss5[Lc, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.miss4[Lc, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.miss3[Lc, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.miss2[Lc, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.miss1[Lc, Nt](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ => q._3
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns)): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has38[La, Lb, Lc, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss3[La, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss2[La, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss1[La, Ns](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss7[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss6[Lb, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss5[Lb, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss4[Lb, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss3[Lb, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss2[Lb, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss1[Lb, Ns](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.miss7[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.miss6[Lc, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.miss5[Lc, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.miss4[Lc, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.miss3[Lc, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.miss2[Lc, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.miss1[Lc, Ns](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ => q._3
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns,Nr)): (A \^ La, B \^ Lb, C \^ Lc) =
    inline if !LabelConflicts.has39[La, Lb, Lc, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss3[La, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss2[La, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             LabelConflicts.miss1[La, Nr](" in update corresponding to _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ : ((Nothing \^ La) <:< Nr) => LabelConflicts.head3[La, Lb, Lc]("at _1")
                                             summonInline[(Nr <:< (A \^ La))](source._9)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss8[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss7[Lb, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss6[Lb, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss5[Lb, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss4[Lb, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss3[Lb, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss2[Lb, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             LabelConflicts.miss1[Lb, Nr](" in update corresponding to _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ : ((Nothing \^ Lb) <:< Nr) => LabelConflicts.head2[Lb, Lc]("at _2")
                                             summonInline[(Nr <:< (B \^ Lb))](source._9)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.miss8[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.miss7[Lc, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.miss6[Lc, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.miss5[Lc, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.miss4[Lc, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.miss3[Lc, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.miss2[Lc, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.miss1[Lc, Nr](" in update corresponding to _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ : ((Nothing \^ Lc) <:< Nr) => summonInline[(Nr <:< (C \^ Lc))](source._9)
        case _ => q._3
     )

  transparent inline def revalue[Z](inline name: La | Lb | Lc)(to: Z):
    (Z \^ La, B \^ Lb, C \^ Lc) | 
    (A \^ La, Z \^ Lb, C \^ Lc) | 
    (A \^ La, B \^ Lb, Z \^ Lc)
    =
    inline name match
      case _: La =>
        LabelConflicts.head3[La, Lb, Lc](codeOf(name))
        q.copy(_1 = to.labelled[La])
      case _: Lb =>
        LabelConflicts.head2[Lb, Lc](codeOf(name))
        q.copy(_2 = to.labelled[Lb])
      case _: Lc =>
        q.copy(_3 = to.labelled[Lc])

  transparent inline def relabel[Lz <: LabelVal](inline name: La | Lb | Lc)(inline lz: Lz):
    (A \^ Lz, B \^ Lb, C \^ Lc) | 
    (A \^ La, B \^ Lz, C \^ Lc) | 
    (A \^ La, B \^ Lb, C \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head3[La, Lb, Lc](codeOf(name))
        LabelConflicts.head3[Lz, Lb, Lc](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ Lz, B \^ Lb, C \^ Lc)]
      case _: Lb =>
        LabelConflicts.head2[Lb, Lc](codeOf(name))
        LabelConflicts.head3[Lz, La, Lc](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lz, C \^ Lc)]
      case _: Lc =>
        LabelConflicts.head3[Lz, La, Lb](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lz)]

  transparent inline def redo[Z, Lz <: LabelVal](inline name: La | Lb | Lc)(inline to: Z \^ Lz):
    (Z \^ Lz, B \^ Lb, C \^ Lc) | 
    (A \^ La, Z \^ Lz, C \^ Lc) | 
    (A \^ La, B \^ Lb, Z \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head3[La, Lb, Lc](codeOf(name))
        LabelConflicts.head3[Lz, Lb, Lc](codeOf(name) + " changed, creating a labelling which")
        q.copy(_1 = to)
      case _: Lb =>
        LabelConflicts.head2[Lb, Lc](codeOf(name))
        LabelConflicts.head3[Lz, La, Lc](codeOf(name) + " changed, creating a labelling which")
        q.copy(_2 = to)
      case _: Lc =>
        LabelConflicts.head3[Lz, La, Lb](codeOf(name) + " changed, creating a labelling which")
        q.copy(_3 = to)
}

extension [A, B, C](q: (A, B, C)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc) =
    LabelConflicts.uniq3[La, Lb, Lc]("_1", "_2")
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc): (A \^ La, B \^ Lb, C \^ Lc) =
    LabelConflicts.uniq3[La, Lb, Lc](codeOf(la), codeOf(lb))
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld): A | B | C | D = inline name match
      case _: La =>
        LabelConflicts.head4[La, Lb, Lc, Ld](codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.head3[Lb, Lc, Ld](codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.head2[Lc, Ld](codeOf(name))
        q._3.unlabel
      case _: Ld =>
        q._4.unlabel

  inline def unlabel: (A, B, C, D) = q.asInstanceOf[(A, B, C, D)]

  transparent inline def label_1: La = compiletime.constValue[La]

  transparent inline def label_2: Lb = compiletime.constValue[Lb]

  transparent inline def label_3: Lc = compiletime.constValue[Lc]

  transparent inline def label_4: Ld = compiletime.constValue[Ld]

  transparent inline def labels: (La, Lb, Lc, Ld) = (
    compiletime.constValue[La],
    compiletime.constValue[Lb],
    compiletime.constValue[Lc],
    compiletime.constValue[Ld]
  )

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld): (A, B, C, D) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B | C | D) \^ Lz =
    summonFrom:
      case _: (Lz =:= La) => LabelConflicts.head4[Lz, Lb, Lc, Ld](codeOf(lz))
                             q._1.asInstanceOf[A \^ Lz]
      case _: (Lz =:= Lb) => LabelConflicts.head3[Lz, Lc, Ld](codeOf(lz))
                             q._2.asInstanceOf[B \^ Lz]
      case _: (Lz =:= Lc) => LabelConflicts.head2[Lz, Ld](codeOf(lz))
                             q._3.asInstanceOf[C \^ Lz]
      case _: (Lz =:= Ld) => q._4.asInstanceOf[D \^ Lz]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D) \^ Lz, (A | B | C | D) \^ Ly) =
    LabelConflicts.uniq2[Lz, Ly](codeOf(lz))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head4[Lz, Lb, Lc, Ld](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head3[Lz, Lc, Ld](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head2[Lz, Ld](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => q._4.asInstanceOf[D \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head4[Ly, Lb, Lc, Ld](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head3[Ly, Lc, Ld](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head2[Ly, Ld](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => q._4.asInstanceOf[D \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D) \^ Lz, (A | B | C | D) \^ Ly, (A | B | C | D) \^ Lx) =
    LabelConflicts.uniq3[Lz, Ly, Lx](codeOf(lz), codeOf(ly))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head4[Lz, Lb, Lc, Ld](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head3[Lz, Lc, Ld](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head2[Lz, Ld](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => q._4.asInstanceOf[D \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head4[Ly, Lb, Lc, Ld](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head3[Ly, Lc, Ld](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head2[Ly, Ld](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => q._4.asInstanceOf[D \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head4[Lx, Lb, Lc, Ld](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head3[Lx, Lc, Ld](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head2[Lx, Ld](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => q._4.asInstanceOf[D \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): q.type | ((A | B | C | D) \^ Lz, (A | B | C | D) \^ Ly, (A | B | C | D) \^ Lx, (A | B | C | D) \^ Lw) =
    LabelConflicts.uniq4[Lz, Ly, Lx, Lw](codeOf(lz), codeOf(ly), codeOf(lx))
    summonFrom:
      case _: ((Lz, Ly, Lx, Lw) =:= (La, Lb, Lc, Ld)) => q
      case _ => (
        summonFrom:
          case _: (Lz =:= La) => LabelConflicts.head4[Lz, Lb, Lc, Ld](codeOf(lz))
                                 q._1.asInstanceOf[A \^ Lz]
          case _: (Lz =:= Lb) => LabelConflicts.head3[Lz, Lc, Ld](codeOf(lz))
                                 q._2.asInstanceOf[B \^ Lz]
          case _: (Lz =:= Lc) => LabelConflicts.head2[Lz, Ld](codeOf(lz))
                                 q._3.asInstanceOf[C \^ Lz]
          case _: (Lz =:= Ld) => q._4.asInstanceOf[D \^ Lz]
          case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
        summonFrom:
          case _: (Ly =:= La) => LabelConflicts.head4[Ly, Lb, Lc, Ld](codeOf(ly))
                                 q._1.asInstanceOf[A \^ Ly]
          case _: (Ly =:= Lb) => LabelConflicts.head3[Ly, Lc, Ld](codeOf(ly))
                                 q._2.asInstanceOf[B \^ Ly]
          case _: (Ly =:= Lc) => LabelConflicts.head2[Ly, Ld](codeOf(ly))
                                 q._3.asInstanceOf[C \^ Ly]
          case _: (Ly =:= Ld) => q._4.asInstanceOf[D \^ Ly]
          case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
        summonFrom:
          case _: (Lx =:= La) => LabelConflicts.head4[Lx, Lb, Lc, Ld](codeOf(lx))
                                 q._1.asInstanceOf[A \^ Lx]
          case _: (Lx =:= Lb) => LabelConflicts.head3[Lx, Lc, Ld](codeOf(lx))
                                 q._2.asInstanceOf[B \^ Lx]
          case _: (Lx =:= Lc) => LabelConflicts.head2[Lx, Ld](codeOf(lx))
                                 q._3.asInstanceOf[C \^ Lx]
          case _: (Lx =:= Ld) => q._4.asInstanceOf[D \^ Lx]
          case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
        summonFrom:
          case _: (Lw =:= La) => LabelConflicts.head4[Lw, Lb, Lc, Ld](codeOf(lw))
                                 q._1.asInstanceOf[A \^ Lw]
          case _: (Lw =:= Lb) => LabelConflicts.head3[Lw, Lc, Ld](codeOf(lw))
                                 q._2.asInstanceOf[B \^ Lw]
          case _: (Lw =:= Lc) => LabelConflicts.head2[Lw, Ld](codeOf(lw))
                                 q._3.asInstanceOf[C \^ Lw]
          case _: (Lw =:= Ld) => q._4.asInstanceOf[D \^ Lw]
          case _              => compiletime.error("Label " + codeOf(lw) + " not found")
      )

  inline def updatedBy[Z, Lz <: LabelVal](source: Z \^ Lz): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has41[La, Lb, Lc, Ld, Z \^ Lz] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head4[Lz, Lb, Lc, Ld]("at _1")
                               summonInline[Z <:< A](source.unlabel).labelled[La]
        case _ => q._1,
      summonFrom:
        case _: (Lz =:= Lb) => LabelConflicts.head3[Lz, Lc, Ld]("at _2")
                               summonInline[Z <:< B](source.unlabel).labelled[Lb]
        case _ => q._2,
      summonFrom:
        case _: (Lz =:= Lc) => LabelConflicts.head2[Lz, Ld]("at _3")
                               summonInline[Z <:< C](source.unlabel).labelled[Lc]
        case _ => q._3,
      summonFrom:
        case _: (Lz =:= Ld) => summonInline[Z <:< D](source.unlabel).labelled[Ld]
        case _ => q._4
    )

  inline def updatedBy[Nz, Ny](source: (Nz,Ny)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has42[La, Lb, Lc, Ld, Nz, Ny] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss1[La, Ny](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss1[Lb, Ny](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss1[Lc, Ny](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.miss1[Ld, Ny](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ => q._4
     )

  inline def updatedBy[Nz, Ny, Nx](source: (Nz,Ny,Nx)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has43[La, Lb, Lc, Ld, Nz, Ny, Nx] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss2[La, Ny, Nx](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss1[La, Nx](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss2[Lb, Ny, Nx](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss1[Lb, Nx](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss2[Lc, Ny, Nx](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss1[Lc, Nx](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.miss2[Ld, Ny, Nx](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.miss1[Ld, Nx](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ => q._4
     )

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz,Ny,Nx,Nw)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has44[La, Lb, Lc, Ld, Nz, Ny, Nx, Nw] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss3[La, Ny, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss2[La, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss1[La, Nw](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss3[Lb, Ny, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss2[Lb, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss1[Lb, Nw](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss3[Lc, Ny, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss2[Lc, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss1[Lc, Nw](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.miss3[Ld, Ny, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.miss2[Ld, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.miss1[Ld, Nw](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ => q._4
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz,Ny,Nx,Nw,Nv)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has45[La, Lb, Lc, Ld, Nz, Ny, Nx, Nw, Nv] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss3[La, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss2[La, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss1[La, Nv](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss4[Lb, Ny, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss3[Lb, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss2[Lb, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss1[Lb, Nv](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss4[Lc, Ny, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss3[Lc, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss2[Lc, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss1[Lc, Nv](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.miss4[Ld, Ny, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.miss3[Ld, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.miss2[Ld, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.miss1[Ld, Nv](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ => q._4
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz,Ny,Nx,Nw,Nv,Nu)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has46[La, Lb, Lc, Ld, Nz, Ny, Nx, Nw, Nv, Nu] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss3[La, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss2[La, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss1[La, Nu](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss5[Lb, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss4[Lb, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss3[Lb, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss2[Lb, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss1[Lb, Nu](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss5[Lc, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss4[Lc, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss3[Lc, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss2[Lc, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss1[Lc, Nu](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.miss5[Ld, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.miss4[Ld, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.miss3[Ld, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.miss2[Ld, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.miss1[Ld, Nu](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ => q._4
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has47[La, Lb, Lc, Ld, Nz, Ny, Nx, Nw, Nv, Nu, Nt] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss3[La, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss2[La, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss1[La, Nt](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss6[Lb, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss5[Lb, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss4[Lb, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss3[Lb, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss2[Lb, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss1[Lb, Nt](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss6[Lc, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss5[Lc, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss4[Lc, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss3[Lc, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss2[Lc, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss1[Lc, Nt](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.miss6[Ld, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.miss5[Ld, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.miss4[Ld, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.miss3[Ld, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.miss2[Ld, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.miss1[Ld, Nt](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ => q._4
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has48[La, Lb, Lc, Ld, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss3[La, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss2[La, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss1[La, Ns](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss7[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss6[Lb, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss5[Lb, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss4[Lb, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss3[Lb, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss2[Lb, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss1[Lb, Ns](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss7[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss6[Lc, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss5[Lc, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss4[Lc, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss3[Lc, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss2[Lc, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss1[Lc, Ns](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.miss7[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.miss6[Ld, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.miss5[Ld, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.miss4[Ld, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.miss3[Ld, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.miss2[Ld, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.miss1[Ld, Ns](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ => q._4
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns,Nr)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    inline if !LabelConflicts.has49[La, Lb, Lc, Ld, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss3[La, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss2[La, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             LabelConflicts.miss1[La, Nr](" in update corresponding to _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ : ((Nothing \^ La) <:< Nr) => LabelConflicts.head4[La, Lb, Lc, Ld]("at _1")
                                             summonInline[(Nr <:< (A \^ La))](source._9)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss8[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss7[Lb, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss6[Lb, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss5[Lb, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss4[Lb, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss3[Lb, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss2[Lb, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             LabelConflicts.miss1[Lb, Nr](" in update corresponding to _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ : ((Nothing \^ Lb) <:< Nr) => LabelConflicts.head3[Lb, Lc, Ld]("at _2")
                                             summonInline[(Nr <:< (B \^ Lb))](source._9)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss8[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss7[Lc, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss6[Lc, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss5[Lc, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss4[Lc, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss3[Lc, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss2[Lc, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             LabelConflicts.miss1[Lc, Nr](" in update corresponding to _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ : ((Nothing \^ Lc) <:< Nr) => LabelConflicts.head2[Lc, Ld]("at _3")
                                             summonInline[(Nr <:< (C \^ Lc))](source._9)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.miss8[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.miss7[Ld, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.miss6[Ld, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.miss5[Ld, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.miss4[Ld, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.miss3[Ld, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.miss2[Ld, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.miss1[Ld, Nr](" in update corresponding to _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ : ((Nothing \^ Ld) <:< Nr) => summonInline[(Nr <:< (D \^ Ld))](source._9)
        case _ => q._4
     )

  transparent inline def revalue[Z](inline name: La | Lb | Lc | Ld)(to: Z):
    (Z \^ La, B \^ Lb, C \^ Lc, D \^ Ld) | 
    (A \^ La, Z \^ Lb, C \^ Lc, D \^ Ld) | 
    (A \^ La, B \^ Lb, Z \^ Lc, D \^ Ld) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Ld)
    =
    inline name match
      case _: La =>
        LabelConflicts.head4[La, Lb, Lc, Ld](codeOf(name))
        q.copy(_1 = to.labelled[La])
      case _: Lb =>
        LabelConflicts.head3[Lb, Lc, Ld](codeOf(name))
        q.copy(_2 = to.labelled[Lb])
      case _: Lc =>
        LabelConflicts.head2[Lc, Ld](codeOf(name))
        q.copy(_3 = to.labelled[Lc])
      case _: Ld =>
        q.copy(_4 = to.labelled[Ld])

  transparent inline def relabel[Lz <: LabelVal](inline name: La | Lb | Lc | Ld)(inline lz: Lz):
    (A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld) | 
    (A \^ La, B \^ Lz, C \^ Lc, D \^ Ld) | 
    (A \^ La, B \^ Lb, C \^ Lz, D \^ Ld) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head4[La, Lb, Lc, Ld](codeOf(name))
        LabelConflicts.head4[Lz, Lb, Lc, Ld](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld)]
      case _: Lb =>
        LabelConflicts.head3[Lb, Lc, Ld](codeOf(name))
        LabelConflicts.head4[Lz, La, Lc, Ld](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lz, C \^ Lc, D \^ Ld)]
      case _: Lc =>
        LabelConflicts.head2[Lc, Ld](codeOf(name))
        LabelConflicts.head4[Lz, La, Lb, Ld](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lz, D \^ Ld)]
      case _: Ld =>
        LabelConflicts.head4[Lz, La, Lb, Lc](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lz)]

  transparent inline def redo[Z, Lz <: LabelVal](inline name: La | Lb | Lc | Ld)(inline to: Z \^ Lz):
    (Z \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld) | 
    (A \^ La, Z \^ Lz, C \^ Lc, D \^ Ld) | 
    (A \^ La, B \^ Lb, Z \^ Lz, D \^ Ld) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head4[La, Lb, Lc, Ld](codeOf(name))
        LabelConflicts.head4[Lz, Lb, Lc, Ld](codeOf(name) + " changed, creating a labelling which")
        q.copy(_1 = to)
      case _: Lb =>
        LabelConflicts.head3[Lb, Lc, Ld](codeOf(name))
        LabelConflicts.head4[Lz, La, Lc, Ld](codeOf(name) + " changed, creating a labelling which")
        q.copy(_2 = to)
      case _: Lc =>
        LabelConflicts.head2[Lc, Ld](codeOf(name))
        LabelConflicts.head4[Lz, La, Lb, Ld](codeOf(name) + " changed, creating a labelling which")
        q.copy(_3 = to)
      case _: Ld =>
        LabelConflicts.head4[Lz, La, Lb, Lc](codeOf(name) + " changed, creating a labelling which")
        q.copy(_4 = to)

}

extension [A, B, C, D](q: (A, B, C, D)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    LabelConflicts.uniq4[La, Lb, Lc, Ld]("_1", "_2", "_3")
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    LabelConflicts.uniq4[La, Lb, Lc, Ld](codeOf(la), codeOf(lb), codeOf(lc))
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le): A | B | C | D | E = inline name match
      case _: La =>
        LabelConflicts.head5[La, Lb, Lc, Ld, Le](codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.head4[Lb, Lc, Ld, Le](codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.head3[Lc, Ld, Le](codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.head2[Ld, Le](codeOf(name))
        q._4.unlabel
      case _: Le =>
        q._5.unlabel

  inline def unlabel: (A, B, C, D, E) = q.asInstanceOf[(A, B, C, D, E)]

  transparent inline def label_1: La = compiletime.constValue[La]

  transparent inline def label_2: Lb = compiletime.constValue[Lb]

  transparent inline def label_3: Lc = compiletime.constValue[Lc]

  transparent inline def label_4: Ld = compiletime.constValue[Ld]

  transparent inline def label_5: Le = compiletime.constValue[Le]

  transparent inline def labels: (La, Lb, Lc, Ld, Le) = (
    compiletime.constValue[La],
    compiletime.constValue[Lb],
    compiletime.constValue[Lc],
    compiletime.constValue[Ld],
    compiletime.constValue[Le]
  )

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le): (A, B, C, D, E) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B | C | D | E) \^ Lz =
    summonFrom:
      case _: (Lz =:= La) => LabelConflicts.head5[Lz, Lb, Lc, Ld, Le](codeOf(lz))
                             q._1.asInstanceOf[A \^ Lz]
      case _: (Lz =:= Lb) => LabelConflicts.head4[Lz, Lc, Ld, Le](codeOf(lz))
                             q._2.asInstanceOf[B \^ Lz]
      case _: (Lz =:= Lc) => LabelConflicts.head3[Lz, Ld, Le](codeOf(lz))
                             q._3.asInstanceOf[C \^ Lz]
      case _: (Lz =:= Ld) => LabelConflicts.head2[Lz, Le](codeOf(lz))
                             q._4.asInstanceOf[D \^ Lz]
      case _: (Lz =:= Le) => q._5.asInstanceOf[E \^ Lz]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E) \^ Lz, (A | B | C | D | E) \^ Ly) =
    LabelConflicts.uniq2[Lz, Ly](codeOf(lz))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head5[Lz, Lb, Lc, Ld, Le](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head4[Lz, Lc, Ld, Le](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head3[Lz, Ld, Le](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head2[Lz, Le](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => q._5.asInstanceOf[E \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head5[Ly, Lb, Lc, Ld, Le](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head4[Ly, Lc, Ld, Le](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head3[Ly, Ld, Le](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head2[Ly, Le](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => q._5.asInstanceOf[E \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E) \^ Lz, (A | B | C | D | E) \^ Ly, (A | B | C | D | E) \^ Lx) =
    LabelConflicts.uniq3[Lz, Ly, Lx](codeOf(lz), codeOf(ly))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head5[Lz, Lb, Lc, Ld, Le](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head4[Lz, Lc, Ld, Le](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head3[Lz, Ld, Le](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head2[Lz, Le](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => q._5.asInstanceOf[E \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head5[Ly, Lb, Lc, Ld, Le](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head4[Ly, Lc, Ld, Le](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head3[Ly, Ld, Le](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head2[Ly, Le](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => q._5.asInstanceOf[E \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head5[Lx, Lb, Lc, Ld, Le](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head4[Lx, Lc, Ld, Le](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head3[Lx, Ld, Le](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head2[Lx, Le](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => q._5.asInstanceOf[E \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E) \^ Lz, (A | B | C | D | E) \^ Ly, (A | B | C | D | E) \^ Lx, (A | B | C | D | E) \^ Lw) =
    LabelConflicts.uniq4[Lz, Ly, Lx, Lw](codeOf(lz), codeOf(ly), codeOf(lx))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head5[Lz, Lb, Lc, Ld, Le](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head4[Lz, Lc, Ld, Le](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head3[Lz, Ld, Le](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head2[Lz, Le](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => q._5.asInstanceOf[E \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head5[Ly, Lb, Lc, Ld, Le](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head4[Ly, Lc, Ld, Le](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head3[Ly, Ld, Le](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head2[Ly, Le](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => q._5.asInstanceOf[E \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head5[Lx, Lb, Lc, Ld, Le](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head4[Lx, Lc, Ld, Le](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head3[Lx, Ld, Le](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head2[Lx, Le](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => q._5.asInstanceOf[E \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head5[Lw, Lb, Lc, Ld, Le](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head4[Lw, Lc, Ld, Le](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head3[Lw, Ld, Le](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head2[Lw, Le](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => q._5.asInstanceOf[E \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): q.type | ((A | B | C | D | E) \^ Lz, (A | B | C | D | E) \^ Ly, (A | B | C | D | E) \^ Lx, (A | B | C | D | E) \^ Lw, (A | B | C | D | E) \^ Lv) =
    LabelConflicts.uniq5[Lz, Ly, Lx, Lw, Lv](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw))
    summonFrom:
      case _: ((Lz, Ly, Lx, Lw, Lv) =:= (La, Lb, Lc, Ld, Le)) => q
      case _ => (
        summonFrom:
          case _: (Lz =:= La) => LabelConflicts.head5[Lz, Lb, Lc, Ld, Le](codeOf(lz))
                                 q._1.asInstanceOf[A \^ Lz]
          case _: (Lz =:= Lb) => LabelConflicts.head4[Lz, Lc, Ld, Le](codeOf(lz))
                                 q._2.asInstanceOf[B \^ Lz]
          case _: (Lz =:= Lc) => LabelConflicts.head3[Lz, Ld, Le](codeOf(lz))
                                 q._3.asInstanceOf[C \^ Lz]
          case _: (Lz =:= Ld) => LabelConflicts.head2[Lz, Le](codeOf(lz))
                                 q._4.asInstanceOf[D \^ Lz]
          case _: (Lz =:= Le) => q._5.asInstanceOf[E \^ Lz]
          case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
        summonFrom:
          case _: (Ly =:= La) => LabelConflicts.head5[Ly, Lb, Lc, Ld, Le](codeOf(ly))
                                 q._1.asInstanceOf[A \^ Ly]
          case _: (Ly =:= Lb) => LabelConflicts.head4[Ly, Lc, Ld, Le](codeOf(ly))
                                 q._2.asInstanceOf[B \^ Ly]
          case _: (Ly =:= Lc) => LabelConflicts.head3[Ly, Ld, Le](codeOf(ly))
                                 q._3.asInstanceOf[C \^ Ly]
          case _: (Ly =:= Ld) => LabelConflicts.head2[Ly, Le](codeOf(ly))
                                 q._4.asInstanceOf[D \^ Ly]
          case _: (Ly =:= Le) => q._5.asInstanceOf[E \^ Ly]
          case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
        summonFrom:
          case _: (Lx =:= La) => LabelConflicts.head5[Lx, Lb, Lc, Ld, Le](codeOf(lx))
                                 q._1.asInstanceOf[A \^ Lx]
          case _: (Lx =:= Lb) => LabelConflicts.head4[Lx, Lc, Ld, Le](codeOf(lx))
                                 q._2.asInstanceOf[B \^ Lx]
          case _: (Lx =:= Lc) => LabelConflicts.head3[Lx, Ld, Le](codeOf(lx))
                                 q._3.asInstanceOf[C \^ Lx]
          case _: (Lx =:= Ld) => LabelConflicts.head2[Lx, Le](codeOf(lx))
                                 q._4.asInstanceOf[D \^ Lx]
          case _: (Lx =:= Le) => q._5.asInstanceOf[E \^ Lx]
          case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
        summonFrom:
          case _: (Lw =:= La) => LabelConflicts.head5[Lw, Lb, Lc, Ld, Le](codeOf(lw))
                                 q._1.asInstanceOf[A \^ Lw]
          case _: (Lw =:= Lb) => LabelConflicts.head4[Lw, Lc, Ld, Le](codeOf(lw))
                                 q._2.asInstanceOf[B \^ Lw]
          case _: (Lw =:= Lc) => LabelConflicts.head3[Lw, Ld, Le](codeOf(lw))
                                 q._3.asInstanceOf[C \^ Lw]
          case _: (Lw =:= Ld) => LabelConflicts.head2[Lw, Le](codeOf(lw))
                                 q._4.asInstanceOf[D \^ Lw]
          case _: (Lw =:= Le) => q._5.asInstanceOf[E \^ Lw]
          case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
        summonFrom:
          case _: (Lv =:= La) => LabelConflicts.head5[Lv, Lb, Lc, Ld, Le](codeOf(lv))
                                 q._1.asInstanceOf[A \^ Lv]
          case _: (Lv =:= Lb) => LabelConflicts.head4[Lv, Lc, Ld, Le](codeOf(lv))
                                 q._2.asInstanceOf[B \^ Lv]
          case _: (Lv =:= Lc) => LabelConflicts.head3[Lv, Ld, Le](codeOf(lv))
                                 q._3.asInstanceOf[C \^ Lv]
          case _: (Lv =:= Ld) => LabelConflicts.head2[Lv, Le](codeOf(lv))
                                 q._4.asInstanceOf[D \^ Lv]
          case _: (Lv =:= Le) => q._5.asInstanceOf[E \^ Lv]
          case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
      )

  inline def updatedBy[Z, Lz <: LabelVal](source: Z \^ Lz): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has51[La, Lb, Lc, Ld, Le, Z \^ Lz] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head5[Lz, Lb, Lc, Ld, Le]("at _1")
                               summonInline[Z <:< A](source.unlabel).labelled[La]
        case _ => q._1,
      summonFrom:
        case _: (Lz =:= Lb) => LabelConflicts.head4[Lz, Lc, Ld, Le]("at _2")
                               summonInline[Z <:< B](source.unlabel).labelled[Lb]
        case _ => q._2,
      summonFrom:
        case _: (Lz =:= Lc) => LabelConflicts.head3[Lz, Ld, Le]("at _3")
                               summonInline[Z <:< C](source.unlabel).labelled[Lc]
        case _ => q._3,
      summonFrom:
        case _: (Lz =:= Ld) => LabelConflicts.head2[Lz, Le]("at _4")
                               summonInline[Z <:< D](source.unlabel).labelled[Ld]
        case _ => q._4,
      summonFrom:
        case _: (Lz =:= Le) => summonInline[Z <:< E](source.unlabel).labelled[Le]
        case _ => q._5
    )

  inline def updatedBy[Nz, Ny](source: (Nz,Ny)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has52[La, Lb, Lc, Ld, Le, Nz, Ny] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss1[La, Ny](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss1[Lb, Ny](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss1[Lc, Ny](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss1[Ld, Ny](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head2[Ld, Le]("at _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.miss1[Le, Ny](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ => q._5
     )

  inline def updatedBy[Nz, Ny, Nx](source: (Nz,Ny,Nx)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has53[La, Lb, Lc, Ld, Le, Nz, Ny, Nx] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss2[La, Ny, Nx](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss1[La, Nx](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss2[Lb, Ny, Nx](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss1[Lb, Nx](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss2[Lc, Ny, Nx](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss1[Lc, Nx](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss2[Ld, Ny, Nx](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss1[Ld, Nx](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head2[Ld, Le]("at _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.miss2[Le, Ny, Nx](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.miss1[Le, Nx](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ => q._5
     )

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz,Ny,Nx,Nw)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has54[La, Lb, Lc, Ld, Le, Nz, Ny, Nx, Nw] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss3[La, Ny, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss2[La, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss1[La, Nw](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss3[Lb, Ny, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss2[Lb, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss1[Lb, Nw](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss3[Lc, Ny, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss2[Lc, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss1[Lc, Nw](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss3[Ld, Ny, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss2[Ld, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss1[Ld, Nw](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head2[Ld, Le]("at _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.miss3[Le, Ny, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.miss2[Le, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.miss1[Le, Nw](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ => q._5
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz,Ny,Nx,Nw,Nv)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has55[La, Lb, Lc, Ld, Le, Nz, Ny, Nx, Nw, Nv] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss3[La, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss2[La, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss1[La, Nv](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss4[Lb, Ny, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss3[Lb, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss2[Lb, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss1[Lb, Nv](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss4[Lc, Ny, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss3[Lc, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss2[Lc, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss1[Lc, Nv](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss4[Ld, Ny, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss3[Ld, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss2[Ld, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss1[Ld, Nv](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head2[Ld, Le]("at _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.miss4[Le, Ny, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.miss3[Le, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.miss2[Le, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.miss1[Le, Nv](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ => q._5
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz,Ny,Nx,Nw,Nv,Nu)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has56[La, Lb, Lc, Ld, Le, Nz, Ny, Nx, Nw, Nv, Nu] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss3[La, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss2[La, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss1[La, Nu](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss5[Lb, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss4[Lb, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss3[Lb, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss2[Lb, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss1[Lb, Nu](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss5[Lc, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss4[Lc, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss3[Lc, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss2[Lc, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss1[Lc, Nu](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss5[Ld, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss4[Ld, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss3[Ld, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss2[Ld, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss1[Ld, Nu](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head2[Ld, Le]("at _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.miss5[Le, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.miss4[Le, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.miss3[Le, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.miss2[Le, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.miss1[Le, Nu](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ => q._5
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has57[La, Lb, Lc, Ld, Le, Nz, Ny, Nx, Nw, Nv, Nu, Nt] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss3[La, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss2[La, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss1[La, Nt](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss6[Lb, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss5[Lb, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss4[Lb, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss3[Lb, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss2[Lb, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss1[Lb, Nt](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss6[Lc, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss5[Lc, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss4[Lc, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss3[Lc, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss2[Lc, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss1[Lc, Nt](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss6[Ld, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss5[Ld, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss4[Ld, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss3[Ld, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss2[Ld, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss1[Ld, Nt](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head2[Ld, Le]("at _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.miss6[Le, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.miss5[Le, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.miss4[Le, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.miss3[Le, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.miss2[Le, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.miss1[Le, Nt](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ => q._5
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has58[La, Lb, Lc, Ld, Le, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss3[La, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss2[La, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss1[La, Ns](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss7[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss6[Lb, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss5[Lb, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss4[Lb, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss3[Lb, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss2[Lb, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss1[Lb, Ns](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss7[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss6[Lc, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss5[Lc, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss4[Lc, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss3[Lc, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss2[Lc, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss1[Lc, Ns](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss7[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss6[Ld, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss5[Ld, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss4[Ld, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss3[Ld, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss2[Ld, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss1[Ld, Ns](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head2[Ld, Le]("at _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.miss7[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.miss6[Le, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.miss5[Le, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.miss4[Le, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.miss3[Le, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.miss2[Le, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.miss1[Le, Ns](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ => q._5
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns,Nr)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    inline if !LabelConflicts.has59[La, Lb, Lc, Ld, Le, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss3[La, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss2[La, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             LabelConflicts.miss1[La, Nr](" in update corresponding to _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ : ((Nothing \^ La) <:< Nr) => LabelConflicts.head5[La, Lb, Lc, Ld, Le]("at _1")
                                             summonInline[(Nr <:< (A \^ La))](source._9)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss8[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss7[Lb, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss6[Lb, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss5[Lb, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss4[Lb, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss3[Lb, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss2[Lb, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             LabelConflicts.miss1[Lb, Nr](" in update corresponding to _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ : ((Nothing \^ Lb) <:< Nr) => LabelConflicts.head4[Lb, Lc, Ld, Le]("at _2")
                                             summonInline[(Nr <:< (B \^ Lb))](source._9)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss8[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss7[Lc, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss6[Lc, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss5[Lc, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss4[Lc, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss3[Lc, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss2[Lc, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             LabelConflicts.miss1[Lc, Nr](" in update corresponding to _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ : ((Nothing \^ Lc) <:< Nr) => LabelConflicts.head3[Lc, Ld, Le]("at _3")
                                             summonInline[(Nr <:< (C \^ Lc))](source._9)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss8[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss7[Ld, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss6[Ld, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss5[Ld, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss4[Ld, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss3[Ld, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss2[Ld, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head2[Ld, Le]("at _4")
                                             LabelConflicts.miss1[Ld, Nr](" in update corresponding to _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ : ((Nothing \^ Ld) <:< Nr) => LabelConflicts.head2[Ld, Le]("at _4")
                                             summonInline[(Nr <:< (D \^ Ld))](source._9)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.miss8[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.miss7[Le, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.miss6[Le, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.miss5[Le, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.miss4[Le, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.miss3[Le, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.miss2[Le, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.miss1[Le, Nr](" in update corresponding to _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ : ((Nothing \^ Le) <:< Nr) => summonInline[(Nr <:< (E \^ Le))](source._9)
        case _ => q._5
     )

  transparent inline def revalue[Z](inline name: La | Lb | Lc | Ld | Le)(to: Z):
    (Z \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) | 
    (A \^ La, Z \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) | 
    (A \^ La, B \^ Lb, Z \^ Lc, D \^ Ld, E \^ Le) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Ld, E \^ Le) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Le)
    =
    inline name match
      case _: La =>
        LabelConflicts.head5[La, Lb, Lc, Ld, Le](codeOf(name))
        q.copy(_1 = to.labelled[La])
      case _: Lb =>
        LabelConflicts.head4[Lb, Lc, Ld, Le](codeOf(name))
        q.copy(_2 = to.labelled[Lb])
      case _: Lc =>
        LabelConflicts.head3[Lc, Ld, Le](codeOf(name))
        q.copy(_3 = to.labelled[Lc])
      case _: Ld =>
        LabelConflicts.head2[Ld, Le](codeOf(name))
        q.copy(_4 = to.labelled[Ld])
      case _: Le =>
        q.copy(_5 = to.labelled[Le])

  transparent inline def relabel[Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le)(inline lz: Lz):
    (A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) | 
    (A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le) | 
    (A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head5[La, Lb, Lc, Ld, Le](codeOf(name))
        LabelConflicts.head5[Lz, Lb, Lc, Ld, Le](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le)]
      case _: Lb =>
        LabelConflicts.head4[Lb, Lc, Ld, Le](codeOf(name))
        LabelConflicts.head5[Lz, La, Lc, Ld, Le](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le)]
      case _: Lc =>
        LabelConflicts.head3[Lc, Ld, Le](codeOf(name))
        LabelConflicts.head5[Lz, La, Lb, Ld, Le](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le)]
      case _: Ld =>
        LabelConflicts.head2[Ld, Le](codeOf(name))
        LabelConflicts.head5[Lz, La, Lb, Lc, Le](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le)]
      case _: Le =>
        LabelConflicts.head5[Lz, La, Lb, Lc, Ld](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz)]

  transparent inline def redo[Z, Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le)(inline to: Z \^ Lz):
    (Z \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) | 
    (A \^ La, Z \^ Lz, C \^ Lc, D \^ Ld, E \^ Le) | 
    (A \^ La, B \^ Lb, Z \^ Lz, D \^ Ld, E \^ Le) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Lz, E \^ Le) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head5[La, Lb, Lc, Ld, Le](codeOf(name))
        LabelConflicts.head5[Lz, Lb, Lc, Ld, Le](codeOf(name) + " changed, creating a labelling which")
        q.copy(_1 = to)
      case _: Lb =>
        LabelConflicts.head4[Lb, Lc, Ld, Le](codeOf(name))
        LabelConflicts.head5[Lz, La, Lc, Ld, Le](codeOf(name) + " changed, creating a labelling which")
        q.copy(_2 = to)
      case _: Lc =>
        LabelConflicts.head3[Lc, Ld, Le](codeOf(name))
        LabelConflicts.head5[Lz, La, Lb, Ld, Le](codeOf(name) + " changed, creating a labelling which")
        q.copy(_3 = to)
      case _: Ld =>
        LabelConflicts.head2[Ld, Le](codeOf(name))
        LabelConflicts.head5[Lz, La, Lb, Lc, Le](codeOf(name) + " changed, creating a labelling which")
        q.copy(_4 = to)
      case _: Le =>
        LabelConflicts.head5[Lz, La, Lb, Lc, Ld](codeOf(name) + " changed, creating a labelling which")
        q.copy(_5 = to)
}

extension [A, B, C, D, E](q: (A, B, C, D, E)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    LabelConflicts.uniq5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4")
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    LabelConflicts.uniq5[La, Lb, Lc, Ld, Le](codeOf(la), codeOf(lb), codeOf(lc), codeOf(ld))
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal, F, Lf <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le | Lf): A | B | C | D | E | F = inline name match
      case _: La =>
        LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf](codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.head5[Lb, Lc, Ld, Le, Lf](codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.head4[Lc, Ld, Le, Lf](codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.head3[Ld, Le, Lf](codeOf(name))
        q._4.unlabel
      case _: Le =>
        LabelConflicts.head2[Le, Lf](codeOf(name))
        q._5.unlabel
      case _: Lf =>
        q._6.unlabel

  inline def unlabel: (A, B, C, D, E, F) = q.asInstanceOf[(A, B, C, D, E, F)]

  transparent inline def label_1: La = compiletime.constValue[La]

  transparent inline def label_2: Lb = compiletime.constValue[Lb]

  transparent inline def label_3: Lc = compiletime.constValue[Lc]

  transparent inline def label_4: Ld = compiletime.constValue[Ld]

  transparent inline def label_5: Le = compiletime.constValue[Le]

  transparent inline def label_6: Lf = compiletime.constValue[Lf]

  transparent inline def labels: (La, Lb, Lc, Ld, Le, Lf) = (
    compiletime.constValue[La],
    compiletime.constValue[Lb],
    compiletime.constValue[Lc],
    compiletime.constValue[Ld],
    compiletime.constValue[Le],
    compiletime.constValue[Lf]
  )

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf): (A, B, C, D, E, F) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B | C | D | E | F) \^ Lz =
    summonFrom:
      case _: (Lz =:= La) => LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf](codeOf(lz))
                             q._1.asInstanceOf[A \^ Lz]
      case _: (Lz =:= Lb) => LabelConflicts.head5[Lz, Lc, Ld, Le, Lf](codeOf(lz))
                             q._2.asInstanceOf[B \^ Lz]
      case _: (Lz =:= Lc) => LabelConflicts.head4[Lz, Ld, Le, Lf](codeOf(lz))
                             q._3.asInstanceOf[C \^ Lz]
      case _: (Lz =:= Ld) => LabelConflicts.head3[Lz, Le, Lf](codeOf(lz))
                             q._4.asInstanceOf[D \^ Lz]
      case _: (Lz =:= Le) => LabelConflicts.head2[Lz, Lf](codeOf(lz))
                             q._5.asInstanceOf[E \^ Lz]
      case _: (Lz =:= Lf) => q._6.asInstanceOf[F \^ Lz]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly) =
    LabelConflicts.uniq2[Lz, Ly](codeOf(lz))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head5[Lz, Lc, Ld, Le, Lf](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head4[Lz, Ld, Le, Lf](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head3[Lz, Le, Lf](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head2[Lz, Lf](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => q._6.asInstanceOf[F \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head6[Ly, Lb, Lc, Ld, Le, Lf](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head5[Ly, Lc, Ld, Le, Lf](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head4[Ly, Ld, Le, Lf](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head3[Ly, Le, Lf](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head2[Ly, Lf](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => q._6.asInstanceOf[F \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly, (A | B | C | D | E | F) \^ Lx) =
    LabelConflicts.uniq3[Lz, Ly, Lx](codeOf(lz), codeOf(ly))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head5[Lz, Lc, Ld, Le, Lf](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head4[Lz, Ld, Le, Lf](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head3[Lz, Le, Lf](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head2[Lz, Lf](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => q._6.asInstanceOf[F \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head6[Ly, Lb, Lc, Ld, Le, Lf](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head5[Ly, Lc, Ld, Le, Lf](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head4[Ly, Ld, Le, Lf](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head3[Ly, Le, Lf](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head2[Ly, Lf](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => q._6.asInstanceOf[F \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head6[Lx, Lb, Lc, Ld, Le, Lf](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head5[Lx, Lc, Ld, Le, Lf](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head4[Lx, Ld, Le, Lf](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head3[Lx, Le, Lf](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head2[Lx, Lf](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => q._6.asInstanceOf[F \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly, (A | B | C | D | E | F) \^ Lx, (A | B | C | D | E | F) \^ Lw) =
    LabelConflicts.uniq4[Lz, Ly, Lx, Lw](codeOf(lz), codeOf(ly), codeOf(lx))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head5[Lz, Lc, Ld, Le, Lf](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head4[Lz, Ld, Le, Lf](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head3[Lz, Le, Lf](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head2[Lz, Lf](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => q._6.asInstanceOf[F \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head6[Ly, Lb, Lc, Ld, Le, Lf](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head5[Ly, Lc, Ld, Le, Lf](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head4[Ly, Ld, Le, Lf](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head3[Ly, Le, Lf](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head2[Ly, Lf](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => q._6.asInstanceOf[F \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head6[Lx, Lb, Lc, Ld, Le, Lf](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head5[Lx, Lc, Ld, Le, Lf](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head4[Lx, Ld, Le, Lf](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head3[Lx, Le, Lf](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head2[Lx, Lf](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => q._6.asInstanceOf[F \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head6[Lw, Lb, Lc, Ld, Le, Lf](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head5[Lw, Lc, Ld, Le, Lf](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head4[Lw, Ld, Le, Lf](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head3[Lw, Le, Lf](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head2[Lw, Lf](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => q._6.asInstanceOf[F \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly, (A | B | C | D | E | F) \^ Lx, (A | B | C | D | E | F) \^ Lw, (A | B | C | D | E | F) \^ Lv) =
    LabelConflicts.uniq5[Lz, Ly, Lx, Lw, Lv](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head5[Lz, Lc, Ld, Le, Lf](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head4[Lz, Ld, Le, Lf](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head3[Lz, Le, Lf](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head2[Lz, Lf](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => q._6.asInstanceOf[F \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head6[Ly, Lb, Lc, Ld, Le, Lf](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head5[Ly, Lc, Ld, Le, Lf](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head4[Ly, Ld, Le, Lf](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head3[Ly, Le, Lf](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head2[Ly, Lf](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => q._6.asInstanceOf[F \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head6[Lx, Lb, Lc, Ld, Le, Lf](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head5[Lx, Lc, Ld, Le, Lf](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head4[Lx, Ld, Le, Lf](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head3[Lx, Le, Lf](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head2[Lx, Lf](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => q._6.asInstanceOf[F \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head6[Lw, Lb, Lc, Ld, Le, Lf](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head5[Lw, Lc, Ld, Le, Lf](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head4[Lw, Ld, Le, Lf](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head3[Lw, Le, Lf](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head2[Lw, Lf](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => q._6.asInstanceOf[F \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head6[Lv, Lb, Lc, Ld, Le, Lf](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head5[Lv, Lc, Ld, Le, Lf](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head4[Lv, Ld, Le, Lf](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head3[Lv, Le, Lf](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head2[Lv, Lf](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => q._6.asInstanceOf[F \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu): q.type | ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly, (A | B | C | D | E | F) \^ Lx, (A | B | C | D | E | F) \^ Lw, (A | B | C | D | E | F) \^ Lv, (A | B | C | D | E | F) \^ Lu) =
    LabelConflicts.uniq6[Lz, Ly, Lx, Lw, Lv, Lu](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv))
    summonFrom:
      case _: ((Lz, Ly, Lx, Lw, Lv, Lu) =:= (La, Lb, Lc, Ld, Le, Lf)) => q
      case _ => (
        summonFrom:
          case _: (Lz =:= La) => LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf](codeOf(lz))
                                 q._1.asInstanceOf[A \^ Lz]
          case _: (Lz =:= Lb) => LabelConflicts.head5[Lz, Lc, Ld, Le, Lf](codeOf(lz))
                                 q._2.asInstanceOf[B \^ Lz]
          case _: (Lz =:= Lc) => LabelConflicts.head4[Lz, Ld, Le, Lf](codeOf(lz))
                                 q._3.asInstanceOf[C \^ Lz]
          case _: (Lz =:= Ld) => LabelConflicts.head3[Lz, Le, Lf](codeOf(lz))
                                 q._4.asInstanceOf[D \^ Lz]
          case _: (Lz =:= Le) => LabelConflicts.head2[Lz, Lf](codeOf(lz))
                                 q._5.asInstanceOf[E \^ Lz]
          case _: (Lz =:= Lf) => q._6.asInstanceOf[F \^ Lz]
          case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
        summonFrom:
          case _: (Ly =:= La) => LabelConflicts.head6[Ly, Lb, Lc, Ld, Le, Lf](codeOf(ly))
                                 q._1.asInstanceOf[A \^ Ly]
          case _: (Ly =:= Lb) => LabelConflicts.head5[Ly, Lc, Ld, Le, Lf](codeOf(ly))
                                 q._2.asInstanceOf[B \^ Ly]
          case _: (Ly =:= Lc) => LabelConflicts.head4[Ly, Ld, Le, Lf](codeOf(ly))
                                 q._3.asInstanceOf[C \^ Ly]
          case _: (Ly =:= Ld) => LabelConflicts.head3[Ly, Le, Lf](codeOf(ly))
                                 q._4.asInstanceOf[D \^ Ly]
          case _: (Ly =:= Le) => LabelConflicts.head2[Ly, Lf](codeOf(ly))
                                 q._5.asInstanceOf[E \^ Ly]
          case _: (Ly =:= Lf) => q._6.asInstanceOf[F \^ Ly]
          case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
        summonFrom:
          case _: (Lx =:= La) => LabelConflicts.head6[Lx, Lb, Lc, Ld, Le, Lf](codeOf(lx))
                                 q._1.asInstanceOf[A \^ Lx]
          case _: (Lx =:= Lb) => LabelConflicts.head5[Lx, Lc, Ld, Le, Lf](codeOf(lx))
                                 q._2.asInstanceOf[B \^ Lx]
          case _: (Lx =:= Lc) => LabelConflicts.head4[Lx, Ld, Le, Lf](codeOf(lx))
                                 q._3.asInstanceOf[C \^ Lx]
          case _: (Lx =:= Ld) => LabelConflicts.head3[Lx, Le, Lf](codeOf(lx))
                                 q._4.asInstanceOf[D \^ Lx]
          case _: (Lx =:= Le) => LabelConflicts.head2[Lx, Lf](codeOf(lx))
                                 q._5.asInstanceOf[E \^ Lx]
          case _: (Lx =:= Lf) => q._6.asInstanceOf[F \^ Lx]
          case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
        summonFrom:
          case _: (Lw =:= La) => LabelConflicts.head6[Lw, Lb, Lc, Ld, Le, Lf](codeOf(lw))
                                 q._1.asInstanceOf[A \^ Lw]
          case _: (Lw =:= Lb) => LabelConflicts.head5[Lw, Lc, Ld, Le, Lf](codeOf(lw))
                                 q._2.asInstanceOf[B \^ Lw]
          case _: (Lw =:= Lc) => LabelConflicts.head4[Lw, Ld, Le, Lf](codeOf(lw))
                                 q._3.asInstanceOf[C \^ Lw]
          case _: (Lw =:= Ld) => LabelConflicts.head3[Lw, Le, Lf](codeOf(lw))
                                 q._4.asInstanceOf[D \^ Lw]
          case _: (Lw =:= Le) => LabelConflicts.head2[Lw, Lf](codeOf(lw))
                                 q._5.asInstanceOf[E \^ Lw]
          case _: (Lw =:= Lf) => q._6.asInstanceOf[F \^ Lw]
          case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
        summonFrom:
          case _: (Lv =:= La) => LabelConflicts.head6[Lv, Lb, Lc, Ld, Le, Lf](codeOf(lv))
                                 q._1.asInstanceOf[A \^ Lv]
          case _: (Lv =:= Lb) => LabelConflicts.head5[Lv, Lc, Ld, Le, Lf](codeOf(lv))
                                 q._2.asInstanceOf[B \^ Lv]
          case _: (Lv =:= Lc) => LabelConflicts.head4[Lv, Ld, Le, Lf](codeOf(lv))
                                 q._3.asInstanceOf[C \^ Lv]
          case _: (Lv =:= Ld) => LabelConflicts.head3[Lv, Le, Lf](codeOf(lv))
                                 q._4.asInstanceOf[D \^ Lv]
          case _: (Lv =:= Le) => LabelConflicts.head2[Lv, Lf](codeOf(lv))
                                 q._5.asInstanceOf[E \^ Lv]
          case _: (Lv =:= Lf) => q._6.asInstanceOf[F \^ Lv]
          case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
        summonFrom:
          case _: (Lu =:= La) => LabelConflicts.head6[Lu, Lb, Lc, Ld, Le, Lf](codeOf(lu))
                                 q._1.asInstanceOf[A \^ Lu]
          case _: (Lu =:= Lb) => LabelConflicts.head5[Lu, Lc, Ld, Le, Lf](codeOf(lu))
                                 q._2.asInstanceOf[B \^ Lu]
          case _: (Lu =:= Lc) => LabelConflicts.head4[Lu, Ld, Le, Lf](codeOf(lu))
                                 q._3.asInstanceOf[C \^ Lu]
          case _: (Lu =:= Ld) => LabelConflicts.head3[Lu, Le, Lf](codeOf(lu))
                                 q._4.asInstanceOf[D \^ Lu]
          case _: (Lu =:= Le) => LabelConflicts.head2[Lu, Lf](codeOf(lu))
                                 q._5.asInstanceOf[E \^ Lu]
          case _: (Lu =:= Lf) => q._6.asInstanceOf[F \^ Lu]
          case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
      )

  inline def updatedBy[Z, Lz <: LabelVal](source: Z \^ Lz): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has61[La, Lb, Lc, Ld, Le, Lf, Z \^ Lz] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf]("at _1")
                               summonInline[Z <:< A](source.unlabel).labelled[La]
        case _ => q._1,
      summonFrom:
        case _: (Lz =:= Lb) => LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("at _2")
                               summonInline[Z <:< B](source.unlabel).labelled[Lb]
        case _ => q._2,
      summonFrom:
        case _: (Lz =:= Lc) => LabelConflicts.head4[Lz, Ld, Le, Lf]("at _3")
                               summonInline[Z <:< C](source.unlabel).labelled[Lc]
        case _ => q._3,
      summonFrom:
        case _: (Lz =:= Ld) => LabelConflicts.head3[Lz, Le, Lf]("at _4")
                               summonInline[Z <:< D](source.unlabel).labelled[Ld]
        case _ => q._4,
      summonFrom:
        case _: (Lz =:= Le) => LabelConflicts.head2[Lz, Lf]("at _5")
                               summonInline[Z <:< E](source.unlabel).labelled[Le]
        case _ => q._5,
      summonFrom:
        case _: (Lz =:= Lf) => summonInline[Z <:< F](source.unlabel).labelled[Lf]
        case _ => q._6
    )

  inline def updatedBy[Nz, Ny](source: (Nz,Ny)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has62[La, Lb, Lc, Ld, Le, Lf, Nz, Ny] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss1[La, Ny](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss1[Lb, Ny](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss1[Lc, Ny](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss1[Ld, Ny](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss1[Le, Ny](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head2[Le, Lf]("at _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.miss1[Lf, Ny](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ => q._6
     )

  inline def updatedBy[Nz, Ny, Nx](source: (Nz,Ny,Nx)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has63[La, Lb, Lc, Ld, Le, Lf, Nz, Ny, Nx] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss2[La, Ny, Nx](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss1[La, Nx](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss2[Lb, Ny, Nx](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss1[Lb, Nx](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss2[Lc, Ny, Nx](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss1[Lc, Nx](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss2[Ld, Ny, Nx](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss1[Ld, Nx](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss2[Le, Ny, Nx](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss1[Le, Nx](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head2[Le, Lf]("at _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.miss2[Lf, Ny, Nx](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.miss1[Lf, Nx](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ => q._6
     )

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz,Ny,Nx,Nw)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has64[La, Lb, Lc, Ld, Le, Lf, Nz, Ny, Nx, Nw] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss3[La, Ny, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss2[La, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss1[La, Nw](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss3[Lb, Ny, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss2[Lb, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss1[Lb, Nw](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss3[Lc, Ny, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss2[Lc, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss1[Lc, Nw](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss3[Ld, Ny, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss2[Ld, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss1[Ld, Nw](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss3[Le, Ny, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss2[Le, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss1[Le, Nw](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head2[Le, Lf]("at _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.miss3[Lf, Ny, Nx, Nw](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.miss2[Lf, Nx, Nw](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.miss1[Lf, Nw](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ => q._6
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz,Ny,Nx,Nw,Nv)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has65[La, Lb, Lc, Ld, Le, Lf, Nz, Ny, Nx, Nw, Nv] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss3[La, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss2[La, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss1[La, Nv](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss4[Lb, Ny, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss3[Lb, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss2[Lb, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss1[Lb, Nv](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss4[Lc, Ny, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss3[Lc, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss2[Lc, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss1[Lc, Nv](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss4[Ld, Ny, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss3[Ld, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss2[Ld, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss1[Ld, Nv](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss4[Le, Ny, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss3[Le, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss2[Le, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss1[Le, Nv](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head2[Le, Lf]("at _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.miss4[Lf, Ny, Nx, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.miss3[Lf, Nx, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.miss2[Lf, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.miss1[Lf, Nv](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ => q._6
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz,Ny,Nx,Nw,Nv,Nu)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has66[La, Lb, Lc, Ld, Le, Lf, Nz, Ny, Nx, Nw, Nv, Nu] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss3[La, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss2[La, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss1[La, Nu](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss5[Lb, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss4[Lb, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss3[Lb, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss2[Lb, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss1[Lb, Nu](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss5[Lc, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss4[Lc, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss3[Lc, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss2[Lc, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss1[Lc, Nu](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss5[Ld, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss4[Ld, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss3[Ld, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss2[Ld, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss1[Ld, Nu](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss5[Le, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss4[Le, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss3[Le, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss2[Le, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss1[Le, Nu](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head2[Le, Lf]("at _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.miss5[Lf, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.miss4[Lf, Nx, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.miss3[Lf, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.miss2[Lf, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.miss1[Lf, Nu](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ => q._6
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has67[La, Lb, Lc, Ld, Le, Lf, Nz, Ny, Nx, Nw, Nv, Nu, Nt] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss3[La, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss2[La, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss1[La, Nt](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss6[Lb, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss5[Lb, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss4[Lb, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss3[Lb, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss2[Lb, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss1[Lb, Nt](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss6[Lc, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss5[Lc, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss4[Lc, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss3[Lc, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss2[Lc, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss1[Lc, Nt](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss6[Ld, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss5[Ld, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss4[Ld, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss3[Ld, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss2[Ld, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss1[Ld, Nt](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss6[Le, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss5[Le, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss4[Le, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss3[Le, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss2[Le, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss1[Le, Nt](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head2[Le, Lf]("at _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.miss6[Lf, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.miss5[Lf, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.miss4[Lf, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.miss3[Lf, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.miss2[Lf, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.miss1[Lf, Nt](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ => q._6
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has68[La, Lb, Lc, Ld, Le, Lf, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss3[La, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss2[La, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss1[La, Ns](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss7[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss6[Lb, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss5[Lb, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss4[Lb, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss3[Lb, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss2[Lb, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss1[Lb, Ns](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss7[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss6[Lc, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss5[Lc, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss4[Lc, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss3[Lc, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss2[Lc, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss1[Lc, Ns](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss7[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss6[Ld, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss5[Ld, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss4[Ld, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss3[Ld, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss2[Ld, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss1[Ld, Ns](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss7[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss6[Le, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss5[Le, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss4[Le, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss3[Le, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss2[Le, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss1[Le, Ns](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.head2[Le, Lf]("at _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.miss7[Lf, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.miss6[Lf, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.miss5[Lf, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.miss4[Lf, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.miss3[Lf, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.miss2[Lf, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.miss1[Lf, Ns](" in update corresponding to _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ : ((Nothing \^ Lf) <:< Ns) => summonInline[(Ns <:< (F \^ Lf))](source._8)
        case _ => q._6
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns,Nr)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    inline if !LabelConflicts.has69[La, Lb, Lc, Ld, Le, Lf, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss3[La, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss2[La, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             LabelConflicts.miss1[La, Nr](" in update corresponding to _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ : ((Nothing \^ La) <:< Nr) => LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("at _1")
                                             summonInline[(Nr <:< (A \^ La))](source._9)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss8[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss7[Lb, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss6[Lb, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss5[Lb, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss4[Lb, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss3[Lb, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss2[Lb, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             LabelConflicts.miss1[Lb, Nr](" in update corresponding to _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ : ((Nothing \^ Lb) <:< Nr) => LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("at _2")
                                             summonInline[(Nr <:< (B \^ Lb))](source._9)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss8[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss7[Lc, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss6[Lc, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss5[Lc, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss4[Lc, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss3[Lc, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss2[Lc, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             LabelConflicts.miss1[Lc, Nr](" in update corresponding to _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ : ((Nothing \^ Lc) <:< Nr) => LabelConflicts.head4[Lc, Ld, Le, Lf]("at _3")
                                             summonInline[(Nr <:< (C \^ Lc))](source._9)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss8[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss7[Ld, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss6[Ld, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss5[Ld, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss4[Ld, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss3[Ld, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss2[Ld, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             LabelConflicts.miss1[Ld, Nr](" in update corresponding to _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ : ((Nothing \^ Ld) <:< Nr) => LabelConflicts.head3[Ld, Le, Lf]("at _4")
                                             summonInline[(Nr <:< (D \^ Ld))](source._9)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss8[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss7[Le, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss6[Le, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss5[Le, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss4[Le, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss3[Le, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss2[Le, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.head2[Le, Lf]("at _5")
                                             LabelConflicts.miss1[Le, Nr](" in update corresponding to _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ : ((Nothing \^ Le) <:< Nr) => LabelConflicts.head2[Le, Lf]("at _5")
                                             summonInline[(Nr <:< (E \^ Le))](source._9)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.miss8[Lf, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.miss7[Lf, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.miss6[Lf, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.miss5[Lf, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.miss4[Lf, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.miss3[Lf, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.miss2[Lf, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ : ((Nothing \^ Lf) <:< Ns) => LabelConflicts.miss1[Lf, Nr](" in update corresponding to _6")
                                             summonInline[(Ns <:< (F \^ Lf))](source._8)
        case _ : ((Nothing \^ Lf) <:< Nr) => summonInline[(Nr <:< (F \^ Lf))](source._9)
        case _ => q._6
     )

  transparent inline def revalue[Z](inline name: La | Lb | Lc | Ld | Le | Lf)(to: Z):
    (Z \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, Z \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, Z \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, Z \^ Lf)
    =
    inline name match
      case _: La =>
        LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf](codeOf(name))
        q.copy(_1 = to.labelled[La])
      case _: Lb =>
        LabelConflicts.head5[Lb, Lc, Ld, Le, Lf](codeOf(name))
        q.copy(_2 = to.labelled[Lb])
      case _: Lc =>
        LabelConflicts.head4[Lc, Ld, Le, Lf](codeOf(name))
        q.copy(_3 = to.labelled[Lc])
      case _: Ld =>
        LabelConflicts.head3[Ld, Le, Lf](codeOf(name))
        q.copy(_4 = to.labelled[Ld])
      case _: Le =>
        LabelConflicts.head2[Le, Lf](codeOf(name))
        q.copy(_5 = to.labelled[Le])
      case _: Lf =>
        q.copy(_6 = to.labelled[Lf])

  transparent inline def relabel[Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf)(inline lz: Lz):
    (A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)]
      case _: Lb =>
        LabelConflicts.head5[Lb, Lc, Ld, Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, La, Lc, Ld, Le, Lf](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)]
      case _: Lc =>
        LabelConflicts.head4[Lc, Ld, Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, La, Lb, Ld, Le, Lf](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le, F \^ Lf)]
      case _: Ld =>
        LabelConflicts.head3[Ld, Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, La, Lb, Lc, Le, Lf](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le, F \^ Lf)]
      case _: Le =>
        LabelConflicts.head2[Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, La, Lb, Lc, Ld, Lf](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz, F \^ Lf)]
      case _: Lf =>
        LabelConflicts.head6[Lz, La, Lb, Lc, Ld, Le](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lz)]

  transparent inline def redo[Z, Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf)(inline to: Z \^ Lz):
    (Z \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, Z \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, Z \^ Lz, D \^ Ld, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Lz, E \^ Le, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Lz, F \^ Lf) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, Z \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf](codeOf(name) + " changed, creating a labelling which")
        q.copy(_1 = to)
      case _: Lb =>
        LabelConflicts.head5[Lb, Lc, Ld, Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, La, Lc, Ld, Le, Lf](codeOf(name) + " changed, creating a labelling which")
        q.copy(_2 = to)
      case _: Lc =>
        LabelConflicts.head4[Lc, Ld, Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, La, Lb, Ld, Le, Lf](codeOf(name) + " changed, creating a labelling which")
        q.copy(_3 = to)
      case _: Ld =>
        LabelConflicts.head3[Ld, Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, La, Lb, Lc, Le, Lf](codeOf(name) + " changed, creating a labelling which")
        q.copy(_4 = to)
      case _: Le =>
        LabelConflicts.head2[Le, Lf](codeOf(name))
        LabelConflicts.head6[Lz, La, Lb, Lc, Ld, Lf](codeOf(name) + " changed, creating a labelling which")
        q.copy(_5 = to)
      case _: Lf =>
        LabelConflicts.head6[Lz, La, Lb, Lc, Ld, Le](codeOf(name) + " changed, creating a labelling which")
        q.copy(_6 = to)
}

extension [A, B, C, D, E, F](q: (A, B, C, D, E, F)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    LabelConflicts.uniq6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5")
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    LabelConflicts.uniq6[La, Lb, Lc, Ld, Le, Lf](codeOf(la), codeOf(lb), codeOf(lc), codeOf(ld), codeOf(le))
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal, F, Lf <: LabelVal, G, Lg <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le | Lf | Lg): A | B | C | D | E | F | G = inline name match
      case _: La =>
        LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg](codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg](codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.head5[Lc, Ld, Le, Lf, Lg](codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.head4[Ld, Le, Lf, Lg](codeOf(name))
        q._4.unlabel
      case _: Le =>
        LabelConflicts.head3[Le, Lf, Lg](codeOf(name))
        q._5.unlabel
      case _: Lf =>
        LabelConflicts.head2[Lf, Lg](codeOf(name))
        q._6.unlabel
      case _: Lg =>
        q._7.unlabel

  inline def unlabel: (A, B, C, D, E, F, G) = q.asInstanceOf[(A, B, C, D, E, F, G)]

  transparent inline def label_1: La = compiletime.constValue[La]

  transparent inline def label_2: Lb = compiletime.constValue[Lb]

  transparent inline def label_3: Lc = compiletime.constValue[Lc]

  transparent inline def label_4: Ld = compiletime.constValue[Ld]

  transparent inline def label_5: Le = compiletime.constValue[Le]

  transparent inline def label_6: Lf = compiletime.constValue[Lf]

  transparent inline def label_7: Lg = compiletime.constValue[Lg]

  transparent inline def labels: (La, Lb, Lc, Ld, Le, Lf, Lg) = (
    compiletime.constValue[La],
    compiletime.constValue[Lb],
    compiletime.constValue[Lc],
    compiletime.constValue[Ld],
    compiletime.constValue[Le],
    compiletime.constValue[Lf],
    compiletime.constValue[Lg]
  )

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg): (A, B, C, D, E, F, G) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B | C | D | E | F | G) \^ Lz =
    summonFrom:
      case _: (Lz =:= La) => LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                             q._1.asInstanceOf[A \^ Lz]
      case _: (Lz =:= Lb) => LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                             q._2.asInstanceOf[B \^ Lz]
      case _: (Lz =:= Lc) => LabelConflicts.head5[Lz, Ld, Le, Lf, Lg](codeOf(lz))
                             q._3.asInstanceOf[C \^ Lz]
      case _: (Lz =:= Ld) => LabelConflicts.head4[Lz, Le, Lf, Lg](codeOf(lz))
                             q._4.asInstanceOf[D \^ Lz]
      case _: (Lz =:= Le) => LabelConflicts.head3[Lz, Lf, Lg](codeOf(lz))
                             q._5.asInstanceOf[E \^ Lz]
      case _: (Lz =:= Lf) => LabelConflicts.head2[Lz, Lg](codeOf(lz))
                             q._6.asInstanceOf[F \^ Lz]
      case _: (Lz =:= Lg) => q._7.asInstanceOf[G \^ Lz]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly) =
    LabelConflicts.uniq2[Lz, Ly](codeOf(lz))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head5[Lz, Ld, Le, Lf, Lg](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head4[Lz, Le, Lf, Lg](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head3[Lz, Lf, Lg](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head2[Lz, Lg](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => q._7.asInstanceOf[G \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head5[Ly, Ld, Le, Lf, Lg](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head4[Ly, Le, Lf, Lg](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head3[Ly, Lf, Lg](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head2[Ly, Lg](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => q._7.asInstanceOf[G \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx) =
    LabelConflicts.uniq3[Lz, Ly, Lx](codeOf(lz), codeOf(ly))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head5[Lz, Ld, Le, Lf, Lg](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head4[Lz, Le, Lf, Lg](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head3[Lz, Lf, Lg](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head2[Lz, Lg](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => q._7.asInstanceOf[G \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head5[Ly, Ld, Le, Lf, Lg](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head4[Ly, Le, Lf, Lg](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head3[Ly, Lf, Lg](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head2[Ly, Lg](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => q._7.asInstanceOf[G \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head7[Lx, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head6[Lx, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head5[Lx, Ld, Le, Lf, Lg](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head4[Lx, Le, Lf, Lg](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head3[Lx, Lf, Lg](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head2[Lx, Lg](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => q._7.asInstanceOf[G \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx, (A | B | C | D | E | F | G) \^ Lw) =
    LabelConflicts.uniq4[Lz, Ly, Lx, Lw](codeOf(lz), codeOf(ly), codeOf(lx))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head5[Lz, Ld, Le, Lf, Lg](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head4[Lz, Le, Lf, Lg](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head3[Lz, Lf, Lg](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head2[Lz, Lg](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => q._7.asInstanceOf[G \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head5[Ly, Ld, Le, Lf, Lg](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head4[Ly, Le, Lf, Lg](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head3[Ly, Lf, Lg](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head2[Ly, Lg](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => q._7.asInstanceOf[G \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head7[Lx, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head6[Lx, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head5[Lx, Ld, Le, Lf, Lg](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head4[Lx, Le, Lf, Lg](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head3[Lx, Lf, Lg](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head2[Lx, Lg](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => q._7.asInstanceOf[G \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head7[Lw, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head6[Lw, Lc, Ld, Le, Lf, Lg](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head5[Lw, Ld, Le, Lf, Lg](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head4[Lw, Le, Lf, Lg](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head3[Lw, Lf, Lg](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head2[Lw, Lg](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => q._7.asInstanceOf[G \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx, (A | B | C | D | E | F | G) \^ Lw, (A | B | C | D | E | F | G) \^ Lv) =
    LabelConflicts.uniq5[Lz, Ly, Lx, Lw, Lv](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head5[Lz, Ld, Le, Lf, Lg](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head4[Lz, Le, Lf, Lg](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head3[Lz, Lf, Lg](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head2[Lz, Lg](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => q._7.asInstanceOf[G \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head5[Ly, Ld, Le, Lf, Lg](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head4[Ly, Le, Lf, Lg](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head3[Ly, Lf, Lg](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head2[Ly, Lg](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => q._7.asInstanceOf[G \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head7[Lx, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head6[Lx, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head5[Lx, Ld, Le, Lf, Lg](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head4[Lx, Le, Lf, Lg](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head3[Lx, Lf, Lg](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head2[Lx, Lg](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => q._7.asInstanceOf[G \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head7[Lw, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head6[Lw, Lc, Ld, Le, Lf, Lg](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head5[Lw, Ld, Le, Lf, Lg](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head4[Lw, Le, Lf, Lg](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head3[Lw, Lf, Lg](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head2[Lw, Lg](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => q._7.asInstanceOf[G \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head7[Lv, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head6[Lv, Lc, Ld, Le, Lf, Lg](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head5[Lv, Ld, Le, Lf, Lg](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head4[Lv, Le, Lf, Lg](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head3[Lv, Lf, Lg](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head2[Lv, Lg](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => q._7.asInstanceOf[G \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx, (A | B | C | D | E | F | G) \^ Lw, (A | B | C | D | E | F | G) \^ Lv, (A | B | C | D | E | F | G) \^ Lu) =
    LabelConflicts.uniq6[Lz, Ly, Lx, Lw, Lv, Lu](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head5[Lz, Ld, Le, Lf, Lg](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head4[Lz, Le, Lf, Lg](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head3[Lz, Lf, Lg](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head2[Lz, Lg](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => q._7.asInstanceOf[G \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head5[Ly, Ld, Le, Lf, Lg](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head4[Ly, Le, Lf, Lg](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head3[Ly, Lf, Lg](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head2[Ly, Lg](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => q._7.asInstanceOf[G \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head7[Lx, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head6[Lx, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head5[Lx, Ld, Le, Lf, Lg](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head4[Lx, Le, Lf, Lg](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head3[Lx, Lf, Lg](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head2[Lx, Lg](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => q._7.asInstanceOf[G \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head7[Lw, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head6[Lw, Lc, Ld, Le, Lf, Lg](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head5[Lw, Ld, Le, Lf, Lg](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head4[Lw, Le, Lf, Lg](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head3[Lw, Lf, Lg](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head2[Lw, Lg](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => q._7.asInstanceOf[G \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head7[Lv, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head6[Lv, Lc, Ld, Le, Lf, Lg](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head5[Lv, Ld, Le, Lf, Lg](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head4[Lv, Le, Lf, Lg](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head3[Lv, Lf, Lg](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head2[Lv, Lg](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => q._7.asInstanceOf[G \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
      summonFrom:
        case _: (Lu =:= La) => LabelConflicts.head7[Lu, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lu))
                               q._1.asInstanceOf[A \^ Lu]
        case _: (Lu =:= Lb) => LabelConflicts.head6[Lu, Lc, Ld, Le, Lf, Lg](codeOf(lu))
                               q._2.asInstanceOf[B \^ Lu]
        case _: (Lu =:= Lc) => LabelConflicts.head5[Lu, Ld, Le, Lf, Lg](codeOf(lu))
                               q._3.asInstanceOf[C \^ Lu]
        case _: (Lu =:= Ld) => LabelConflicts.head4[Lu, Le, Lf, Lg](codeOf(lu))
                               q._4.asInstanceOf[D \^ Lu]
        case _: (Lu =:= Le) => LabelConflicts.head3[Lu, Lf, Lg](codeOf(lu))
                               q._5.asInstanceOf[E \^ Lu]
        case _: (Lu =:= Lf) => LabelConflicts.head2[Lu, Lg](codeOf(lu))
                               q._6.asInstanceOf[F \^ Lu]
        case _: (Lu =:= Lg) => q._7.asInstanceOf[G \^ Lu]
        case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt): q.type | ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx, (A | B | C | D | E | F | G) \^ Lw, (A | B | C | D | E | F | G) \^ Lv, (A | B | C | D | E | F | G) \^ Lu, (A | B | C | D | E | F | G) \^ Lt) =
    LabelConflicts.uniq7[Lz, Ly, Lx, Lw, Lv, Lu, Lt](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv), codeOf(lu))
    summonFrom:
      case _: ((Lz, Ly, Lx, Lw, Lv, Lu, Lt) =:= (La, Lb, Lc, Ld, Le, Lf, Lg)) => q
      case _ => (
        summonFrom:
          case _: (Lz =:= La) => LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                                 q._1.asInstanceOf[A \^ Lz]
          case _: (Lz =:= Lb) => LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg](codeOf(lz))
                                 q._2.asInstanceOf[B \^ Lz]
          case _: (Lz =:= Lc) => LabelConflicts.head5[Lz, Ld, Le, Lf, Lg](codeOf(lz))
                                 q._3.asInstanceOf[C \^ Lz]
          case _: (Lz =:= Ld) => LabelConflicts.head4[Lz, Le, Lf, Lg](codeOf(lz))
                                 q._4.asInstanceOf[D \^ Lz]
          case _: (Lz =:= Le) => LabelConflicts.head3[Lz, Lf, Lg](codeOf(lz))
                                 q._5.asInstanceOf[E \^ Lz]
          case _: (Lz =:= Lf) => LabelConflicts.head2[Lz, Lg](codeOf(lz))
                                 q._6.asInstanceOf[F \^ Lz]
          case _: (Lz =:= Lg) => q._7.asInstanceOf[G \^ Lz]
          case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
        summonFrom:
          case _: (Ly =:= La) => LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                                 q._1.asInstanceOf[A \^ Ly]
          case _: (Ly =:= Lb) => LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg](codeOf(ly))
                                 q._2.asInstanceOf[B \^ Ly]
          case _: (Ly =:= Lc) => LabelConflicts.head5[Ly, Ld, Le, Lf, Lg](codeOf(ly))
                                 q._3.asInstanceOf[C \^ Ly]
          case _: (Ly =:= Ld) => LabelConflicts.head4[Ly, Le, Lf, Lg](codeOf(ly))
                                 q._4.asInstanceOf[D \^ Ly]
          case _: (Ly =:= Le) => LabelConflicts.head3[Ly, Lf, Lg](codeOf(ly))
                                 q._5.asInstanceOf[E \^ Ly]
          case _: (Ly =:= Lf) => LabelConflicts.head2[Ly, Lg](codeOf(ly))
                                 q._6.asInstanceOf[F \^ Ly]
          case _: (Ly =:= Lg) => q._7.asInstanceOf[G \^ Ly]
          case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
        summonFrom:
          case _: (Lx =:= La) => LabelConflicts.head7[Lx, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                                 q._1.asInstanceOf[A \^ Lx]
          case _: (Lx =:= Lb) => LabelConflicts.head6[Lx, Lc, Ld, Le, Lf, Lg](codeOf(lx))
                                 q._2.asInstanceOf[B \^ Lx]
          case _: (Lx =:= Lc) => LabelConflicts.head5[Lx, Ld, Le, Lf, Lg](codeOf(lx))
                                 q._3.asInstanceOf[C \^ Lx]
          case _: (Lx =:= Ld) => LabelConflicts.head4[Lx, Le, Lf, Lg](codeOf(lx))
                                 q._4.asInstanceOf[D \^ Lx]
          case _: (Lx =:= Le) => LabelConflicts.head3[Lx, Lf, Lg](codeOf(lx))
                                 q._5.asInstanceOf[E \^ Lx]
          case _: (Lx =:= Lf) => LabelConflicts.head2[Lx, Lg](codeOf(lx))
                                 q._6.asInstanceOf[F \^ Lx]
          case _: (Lx =:= Lg) => q._7.asInstanceOf[G \^ Lx]
          case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
        summonFrom:
          case _: (Lw =:= La) => LabelConflicts.head7[Lw, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lw))
                                 q._1.asInstanceOf[A \^ Lw]
          case _: (Lw =:= Lb) => LabelConflicts.head6[Lw, Lc, Ld, Le, Lf, Lg](codeOf(lw))
                                 q._2.asInstanceOf[B \^ Lw]
          case _: (Lw =:= Lc) => LabelConflicts.head5[Lw, Ld, Le, Lf, Lg](codeOf(lw))
                                 q._3.asInstanceOf[C \^ Lw]
          case _: (Lw =:= Ld) => LabelConflicts.head4[Lw, Le, Lf, Lg](codeOf(lw))
                                 q._4.asInstanceOf[D \^ Lw]
          case _: (Lw =:= Le) => LabelConflicts.head3[Lw, Lf, Lg](codeOf(lw))
                                 q._5.asInstanceOf[E \^ Lw]
          case _: (Lw =:= Lf) => LabelConflicts.head2[Lw, Lg](codeOf(lw))
                                 q._6.asInstanceOf[F \^ Lw]
          case _: (Lw =:= Lg) => q._7.asInstanceOf[G \^ Lw]
          case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
        summonFrom:
          case _: (Lv =:= La) => LabelConflicts.head7[Lv, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lv))
                                 q._1.asInstanceOf[A \^ Lv]
          case _: (Lv =:= Lb) => LabelConflicts.head6[Lv, Lc, Ld, Le, Lf, Lg](codeOf(lv))
                                 q._2.asInstanceOf[B \^ Lv]
          case _: (Lv =:= Lc) => LabelConflicts.head5[Lv, Ld, Le, Lf, Lg](codeOf(lv))
                                 q._3.asInstanceOf[C \^ Lv]
          case _: (Lv =:= Ld) => LabelConflicts.head4[Lv, Le, Lf, Lg](codeOf(lv))
                                 q._4.asInstanceOf[D \^ Lv]
          case _: (Lv =:= Le) => LabelConflicts.head3[Lv, Lf, Lg](codeOf(lv))
                                 q._5.asInstanceOf[E \^ Lv]
          case _: (Lv =:= Lf) => LabelConflicts.head2[Lv, Lg](codeOf(lv))
                                 q._6.asInstanceOf[F \^ Lv]
          case _: (Lv =:= Lg) => q._7.asInstanceOf[G \^ Lv]
          case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
        summonFrom:
          case _: (Lu =:= La) => LabelConflicts.head7[Lu, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lu))
                                 q._1.asInstanceOf[A \^ Lu]
          case _: (Lu =:= Lb) => LabelConflicts.head6[Lu, Lc, Ld, Le, Lf, Lg](codeOf(lu))
                                 q._2.asInstanceOf[B \^ Lu]
          case _: (Lu =:= Lc) => LabelConflicts.head5[Lu, Ld, Le, Lf, Lg](codeOf(lu))
                                 q._3.asInstanceOf[C \^ Lu]
          case _: (Lu =:= Ld) => LabelConflicts.head4[Lu, Le, Lf, Lg](codeOf(lu))
                                 q._4.asInstanceOf[D \^ Lu]
          case _: (Lu =:= Le) => LabelConflicts.head3[Lu, Lf, Lg](codeOf(lu))
                                 q._5.asInstanceOf[E \^ Lu]
          case _: (Lu =:= Lf) => LabelConflicts.head2[Lu, Lg](codeOf(lu))
                                 q._6.asInstanceOf[F \^ Lu]
          case _: (Lu =:= Lg) => q._7.asInstanceOf[G \^ Lu]
          case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
        summonFrom:
          case _: (Lt =:= La) => LabelConflicts.head7[Lt, Lb, Lc, Ld, Le, Lf, Lg](codeOf(lt))
                                 q._1.asInstanceOf[A \^ Lt]
          case _: (Lt =:= Lb) => LabelConflicts.head6[Lt, Lc, Ld, Le, Lf, Lg](codeOf(lt))
                                 q._2.asInstanceOf[B \^ Lt]
          case _: (Lt =:= Lc) => LabelConflicts.head5[Lt, Ld, Le, Lf, Lg](codeOf(lt))
                                 q._3.asInstanceOf[C \^ Lt]
          case _: (Lt =:= Ld) => LabelConflicts.head4[Lt, Le, Lf, Lg](codeOf(lt))
                                 q._4.asInstanceOf[D \^ Lt]
          case _: (Lt =:= Le) => LabelConflicts.head3[Lt, Lf, Lg](codeOf(lt))
                                 q._5.asInstanceOf[E \^ Lt]
          case _: (Lt =:= Lf) => LabelConflicts.head2[Lt, Lg](codeOf(lt))
                                 q._6.asInstanceOf[F \^ Lt]
          case _: (Lt =:= Lg) => q._7.asInstanceOf[G \^ Lt]
          case _              => compiletime.error("Label " + codeOf(lt) + " not found"),
      )

  inline def updatedBy[Z, Lz <: LabelVal](source: Z \^ Lz): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has71[La, Lb, Lc, Ld, Le, Lf, Lg, Z \^ Lz] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                               summonInline[Z <:< A](source.unlabel).labelled[La]
        case _ => q._1,
      summonFrom:
        case _: (Lz =:= Lb) => LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg]("at _2")
                               summonInline[Z <:< B](source.unlabel).labelled[Lb]
        case _ => q._2,
      summonFrom:
        case _: (Lz =:= Lc) => LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("at _3")
                               summonInline[Z <:< C](source.unlabel).labelled[Lc]
        case _ => q._3,
      summonFrom:
        case _: (Lz =:= Ld) => LabelConflicts.head4[Lz, Le, Lf, Lg]("at _4")
                               summonInline[Z <:< D](source.unlabel).labelled[Ld]
        case _ => q._4,
      summonFrom:
        case _: (Lz =:= Le) => LabelConflicts.head3[Lz, Lf, Lg]("at _5")
                               summonInline[Z <:< E](source.unlabel).labelled[Le]
        case _ => q._5,
      summonFrom:
        case _: (Lz =:= Lf) => LabelConflicts.head2[Lz, Lg]("at _6")
                               summonInline[Z <:< F](source.unlabel).labelled[Lf]
        case _ => q._6,
      summonFrom:
        case _: (Lz =:= Lg) => summonInline[Z <:< G](source.unlabel).labelled[Lg]
        case _ => q._7
    )

  inline def updatedBy[Nz, Ny](source: (Nz,Ny)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has72[La, Lb, Lc, Ld, Le, Lf, Lg, Nz, Ny] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss1[La, Ny](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss1[Lb, Ny](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss1[Lc, Ny](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss1[Ld, Ny](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss1[Le, Ny](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss1[Lf, Ny](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.miss1[Lg, Ny](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ => q._7
     )

  inline def updatedBy[Nz, Ny, Nx](source: (Nz,Ny,Nx)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has73[La, Lb, Lc, Ld, Le, Lf, Lg, Nz, Ny, Nx] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss2[La, Ny, Nx](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss1[La, Nx](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss2[Lb, Ny, Nx](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss1[Lb, Nx](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss2[Lc, Ny, Nx](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss1[Lc, Nx](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss2[Ld, Ny, Nx](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss1[Ld, Nx](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss2[Le, Ny, Nx](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss1[Le, Nx](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss2[Lf, Ny, Nx](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss1[Lf, Nx](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.miss2[Lg, Ny, Nx](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.miss1[Lg, Nx](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ => q._7
     )

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz,Ny,Nx,Nw)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has74[La, Lb, Lc, Ld, Le, Lf, Lg, Nz, Ny, Nx, Nw] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss3[La, Ny, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss2[La, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss1[La, Nw](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss3[Lb, Ny, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss2[Lb, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss1[Lb, Nw](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss3[Lc, Ny, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss2[Lc, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss1[Lc, Nw](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss3[Ld, Ny, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss2[Ld, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss1[Ld, Nw](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss3[Le, Ny, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss2[Le, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss1[Le, Nw](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss3[Lf, Ny, Nx, Nw](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss2[Lf, Nx, Nw](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss1[Lf, Nw](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.miss3[Lg, Ny, Nx, Nw](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.miss2[Lg, Nx, Nw](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.miss1[Lg, Nw](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ => q._7
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz,Ny,Nx,Nw,Nv)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has75[La, Lb, Lc, Ld, Le, Lf, Lg, Nz, Ny, Nx, Nw, Nv] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss3[La, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss2[La, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss1[La, Nv](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss4[Lb, Ny, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss3[Lb, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss2[Lb, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss1[Lb, Nv](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss4[Lc, Ny, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss3[Lc, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss2[Lc, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss1[Lc, Nv](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss4[Ld, Ny, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss3[Ld, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss2[Ld, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss1[Ld, Nv](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss4[Le, Ny, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss3[Le, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss2[Le, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss1[Le, Nv](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss4[Lf, Ny, Nx, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss3[Lf, Nx, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss2[Lf, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss1[Lf, Nv](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.miss4[Lg, Ny, Nx, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.miss3[Lg, Nx, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.miss2[Lg, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.miss1[Lg, Nv](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ => q._7
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz,Ny,Nx,Nw,Nv,Nu)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has76[La, Lb, Lc, Ld, Le, Lf, Lg, Nz, Ny, Nx, Nw, Nv, Nu] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss3[La, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss2[La, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss1[La, Nu](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss5[Lb, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss4[Lb, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss3[Lb, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss2[Lb, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss1[Lb, Nu](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss5[Lc, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss4[Lc, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss3[Lc, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss2[Lc, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss1[Lc, Nu](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss5[Ld, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss4[Ld, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss3[Ld, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss2[Ld, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss1[Ld, Nu](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss5[Le, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss4[Le, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss3[Le, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss2[Le, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss1[Le, Nu](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss5[Lf, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss4[Lf, Nx, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss3[Lf, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss2[Lf, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss1[Lf, Nu](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.miss5[Lg, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.miss4[Lg, Nx, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.miss3[Lg, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.miss2[Lg, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.miss1[Lg, Nu](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ => q._7
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has77[La, Lb, Lc, Ld, Le, Lf, Lg, Nz, Ny, Nx, Nw, Nv, Nu, Nt] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss3[La, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss2[La, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss1[La, Nt](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss6[Lb, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss5[Lb, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss4[Lb, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss3[Lb, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss2[Lb, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss1[Lb, Nt](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss6[Lc, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss5[Lc, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss4[Lc, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss3[Lc, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss2[Lc, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss1[Lc, Nt](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss6[Ld, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss5[Ld, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss4[Ld, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss3[Ld, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss2[Ld, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss1[Ld, Nt](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss6[Le, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss5[Le, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss4[Le, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss3[Le, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss2[Le, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss1[Le, Nt](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss6[Lf, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss5[Lf, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss4[Lf, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss3[Lf, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss2[Lf, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss1[Lf, Nt](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.miss6[Lg, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.miss5[Lg, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.miss4[Lg, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.miss3[Lg, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.miss2[Lg, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.miss1[Lg, Nt](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ => q._7
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has78[La, Lb, Lc, Ld, Le, Lf, Lg, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss3[La, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss2[La, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss1[La, Ns](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss7[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss6[Lb, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss5[Lb, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss4[Lb, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss3[Lb, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss2[Lb, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss1[Lb, Ns](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss7[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss6[Lc, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss5[Lc, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss4[Lc, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss3[Lc, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss2[Lc, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss1[Lc, Ns](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss7[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss6[Ld, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss5[Ld, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss4[Ld, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss3[Ld, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss2[Ld, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss1[Ld, Ns](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss7[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss6[Le, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss5[Le, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss4[Le, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss3[Le, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss2[Le, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss1[Le, Ns](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss7[Lf, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss6[Lf, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss5[Lf, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss4[Lf, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss3[Lf, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss2[Lf, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss1[Lf, Ns](" in update corresponding to _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ : ((Nothing \^ Lf) <:< Ns) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             summonInline[(Ns <:< (F \^ Lf))](source._8)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.miss7[Lg, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.miss6[Lg, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.miss5[Lg, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.miss4[Lg, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.miss3[Lg, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.miss2[Lg, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => LabelConflicts.miss1[Lg, Ns](" in update corresponding to _7")
                                             summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ : ((Nothing \^ Lg) <:< Ns) => summonInline[(Ns <:< (G \^ Lg))](source._8)
        case _ => q._7
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns,Nr)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    inline if !LabelConflicts.has79[La, Lb, Lc, Ld, Le, Lf, Lg, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss3[La, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss2[La, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             LabelConflicts.miss1[La, Nr](" in update corresponding to _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ : ((Nothing \^ La) <:< Nr) => LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("at _1")
                                             summonInline[(Nr <:< (A \^ La))](source._9)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss8[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss7[Lb, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss6[Lb, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss5[Lb, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss4[Lb, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss3[Lb, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss2[Lb, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             LabelConflicts.miss1[Lb, Nr](" in update corresponding to _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ : ((Nothing \^ Lb) <:< Nr) => LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("at _2")
                                             summonInline[(Nr <:< (B \^ Lb))](source._9)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss8[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss7[Lc, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss6[Lc, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss5[Lc, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss4[Lc, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss3[Lc, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss2[Lc, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             LabelConflicts.miss1[Lc, Nr](" in update corresponding to _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ : ((Nothing \^ Lc) <:< Nr) => LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("at _3")
                                             summonInline[(Nr <:< (C \^ Lc))](source._9)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss8[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss7[Ld, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss6[Ld, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss5[Ld, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss4[Ld, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss3[Ld, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss2[Ld, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             LabelConflicts.miss1[Ld, Nr](" in update corresponding to _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ : ((Nothing \^ Ld) <:< Nr) => LabelConflicts.head4[Ld, Le, Lf, Lg]("at _4")
                                             summonInline[(Nr <:< (D \^ Ld))](source._9)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss8[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss7[Le, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss6[Le, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss5[Le, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss4[Le, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss3[Le, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss2[Le, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             LabelConflicts.miss1[Le, Nr](" in update corresponding to _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ : ((Nothing \^ Le) <:< Nr) => LabelConflicts.head3[Le, Lf, Lg]("at _5")
                                             summonInline[(Nr <:< (E \^ Le))](source._9)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss8[Lf, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss7[Lf, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss6[Lf, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss5[Lf, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss4[Lf, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss3[Lf, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss2[Lf, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ : ((Nothing \^ Lf) <:< Ns) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             LabelConflicts.miss1[Lf, Nr](" in update corresponding to _6")
                                             summonInline[(Ns <:< (F \^ Lf))](source._8)
        case _ : ((Nothing \^ Lf) <:< Nr) => LabelConflicts.head2[Lf, Lg]("at _6")
                                             summonInline[(Nr <:< (F \^ Lf))](source._9)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.miss8[Lg, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.miss7[Lg, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.miss6[Lg, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.miss5[Lg, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.miss4[Lg, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.miss3[Lg, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => LabelConflicts.miss2[Lg, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ : ((Nothing \^ Lg) <:< Ns) => LabelConflicts.miss1[Lg, Nr](" in update corresponding to _7")
                                             summonInline[(Ns <:< (G \^ Lg))](source._8)
        case _ : ((Nothing \^ Lg) <:< Nr) => summonInline[(Nr <:< (G \^ Lg))](source._9)
        case _ => q._7
     )

  transparent inline def revalue[Z](inline name: La | Lb | Lc | Ld | Le | Lf | Lg)(to: Z):
    (Z \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, Z \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, Z \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, Z \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, Z \^ Lg)
    =
    inline name match
      case _: La =>
        LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg](codeOf(name))
        q.copy(_1 = to.labelled[La])
      case _: Lb =>
        LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg](codeOf(name))
        q.copy(_2 = to.labelled[Lb])
      case _: Lc =>
        LabelConflicts.head5[Lc, Ld, Le, Lf, Lg](codeOf(name))
        q.copy(_3 = to.labelled[Lc])
      case _: Ld =>
        LabelConflicts.head4[Ld, Le, Lf, Lg](codeOf(name))
        q.copy(_4 = to.labelled[Ld])
      case _: Le =>
        LabelConflicts.head3[Le, Lf, Lg](codeOf(name))
        q.copy(_5 = to.labelled[Le])
      case _: Lf =>
        LabelConflicts.head2[Lf, Lg](codeOf(name))
        q.copy(_6 = to.labelled[Lf])
      case _: Lg =>
        q.copy(_7 = to.labelled[Lg])

  transparent inline def relabel[Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg)(inline lz: Lz):
    (A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lz, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]
      case _: Lb =>
        LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lc, Ld, Le, Lf, Lg](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]
      case _: Lc =>
        LabelConflicts.head5[Lc, Ld, Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lb, Ld, Le, Lf, Lg](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]
      case _: Ld =>
        LabelConflicts.head4[Ld, Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lb, Lc, Le, Lf, Lg](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le, F \^ Lf, G \^ Lg)]
      case _: Le =>
        LabelConflicts.head3[Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lb, Lc, Ld, Lf, Lg](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz, F \^ Lf, G \^ Lg)]
      case _: Lf =>
        LabelConflicts.head2[Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lb, Lc, Ld, Le, Lg](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lz, G \^ Lg)]
      case _: Lg =>
        LabelConflicts.head7[Lz, La, Lb, Lc, Ld, Le, Lf](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lz)]

  transparent inline def redo[Z, Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg)(inline to: Z \^ Lz):
    (Z \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, Z \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, Z \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Lz, E \^ Le, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Lz, F \^ Lf, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, Z \^ Lz, G \^ Lg) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, Z \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg](codeOf(name) + " changed, creating a labelling which")
        q.copy(_1 = to)
      case _: Lb =>
        LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lc, Ld, Le, Lf, Lg](codeOf(name) + " changed, creating a labelling which")
        q.copy(_2 = to)
      case _: Lc =>
        LabelConflicts.head5[Lc, Ld, Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lb, Ld, Le, Lf, Lg](codeOf(name) + " changed, creating a labelling which")
        q.copy(_3 = to)
      case _: Ld =>
        LabelConflicts.head4[Ld, Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lb, Lc, Le, Lf, Lg](codeOf(name) + " changed, creating a labelling which")
        q.copy(_4 = to)
      case _: Le =>
        LabelConflicts.head3[Le, Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lb, Lc, Ld, Lf, Lg](codeOf(name) + " changed, creating a labelling which")
        q.copy(_5 = to)
      case _: Lf =>
        LabelConflicts.head2[Lf, Lg](codeOf(name))
        LabelConflicts.head7[Lz, La, Lb, Lc, Ld, Le, Lg](codeOf(name) + " changed, creating a labelling which")
        q.copy(_6 = to)
      case _: Lg =>
        LabelConflicts.head7[Lz, La, Lb, Lc, Ld, Le, Lf](codeOf(name) + " changed, creating a labelling which")
        q.copy(_7 = to)
}

extension [A, B, C, D, E, F, G](q: (A, B, C, D, E, F, G)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    LabelConflicts.uniq7[La, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6")
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    LabelConflicts.uniq7[La, Lb, Lc, Ld, Le, Lf, Lg](codeOf(la), codeOf(lb), codeOf(lc), codeOf(ld), codeOf(le), codeOf(lf))
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal, F, Lf <: LabelVal, G, Lg <: LabelVal, H, Lh <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh): A | B | C | D | E | F | G | H = inline name match
      case _: La =>
        LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh](codeOf(name))
        q._4.unlabel
      case _: Le =>
        LabelConflicts.head4[Le, Lf, Lg, Lh](codeOf(name))
        q._5.unlabel
      case _: Lf =>
        LabelConflicts.head3[Lf, Lg, Lh](codeOf(name))
        q._6.unlabel
      case _: Lg =>
        LabelConflicts.head2[Lg, Lh](codeOf(name))
        q._7.unlabel
      case _: Lh =>
        q._8.unlabel

  inline def unlabel: (A, B, C, D, E, F, G, H) = q.asInstanceOf[(A, B, C, D, E, F, G, H)]

  transparent inline def label_1: La = compiletime.constValue[La]

  transparent inline def label_2: Lb = compiletime.constValue[Lb]

  transparent inline def label_3: Lc = compiletime.constValue[Lc]

  transparent inline def label_4: Ld = compiletime.constValue[Ld]

  transparent inline def label_5: Le = compiletime.constValue[Le]

  transparent inline def label_6: Lf = compiletime.constValue[Lf]

  transparent inline def label_7: Lg = compiletime.constValue[Lg]

  transparent inline def label_8: Lh = compiletime.constValue[Lh]

  transparent inline def labels: (La, Lb, Lc, Ld, Le, Lf, Lg, Lh) = (
    compiletime.constValue[La],
    compiletime.constValue[Lb],
    compiletime.constValue[Lc],
    compiletime.constValue[Ld],
    compiletime.constValue[Le],
    compiletime.constValue[Lf],
    compiletime.constValue[Lg],
    compiletime.constValue[Lh]
  )

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh): (A, B, C, D, E, F, G, H) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B | C | D | E | F | G | H) \^ Lz =
    summonFrom:
      case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                             q._1.asInstanceOf[A \^ Lz]
      case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                             q._2.asInstanceOf[B \^ Lz]
      case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                             q._3.asInstanceOf[C \^ Lz]
      case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh](codeOf(lz))
                             q._4.asInstanceOf[D \^ Lz]
      case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh](codeOf(lz))
                             q._5.asInstanceOf[E \^ Lz]
      case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh](codeOf(lz))
                             q._6.asInstanceOf[F \^ Lz]
      case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh](codeOf(lz))
                             q._7.asInstanceOf[G \^ Lz]
      case _: (Lz =:= Lh) => q._8.asInstanceOf[H \^ Lz]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly) =
    LabelConflicts.uniq2[Lz, Ly](codeOf(lz))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => q._8.asInstanceOf[H \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head5[Ly, Le, Lf, Lg, Lh](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head4[Ly, Lf, Lg, Lh](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head3[Ly, Lg, Lh](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head2[Ly, Lh](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => q._8.asInstanceOf[H \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx) =
    LabelConflicts.uniq3[Lz, Ly, Lx](codeOf(lz), codeOf(ly))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => q._8.asInstanceOf[H \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head5[Ly, Le, Lf, Lg, Lh](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head4[Ly, Lf, Lg, Lh](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head3[Ly, Lg, Lh](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head2[Ly, Lh](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => q._8.asInstanceOf[H \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head8[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head7[Lx, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head6[Lx, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head5[Lx, Le, Lf, Lg, Lh](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head4[Lx, Lf, Lg, Lh](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head3[Lx, Lg, Lh](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head2[Lx, Lh](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => q._8.asInstanceOf[H \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw) =
    LabelConflicts.uniq4[Lz, Ly, Lx, Lw](codeOf(lz), codeOf(ly), codeOf(lx))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => q._8.asInstanceOf[H \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head5[Ly, Le, Lf, Lg, Lh](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head4[Ly, Lf, Lg, Lh](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head3[Ly, Lg, Lh](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head2[Ly, Lh](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => q._8.asInstanceOf[H \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head8[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head7[Lx, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head6[Lx, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head5[Lx, Le, Lf, Lg, Lh](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head4[Lx, Lf, Lg, Lh](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head3[Lx, Lg, Lh](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head2[Lx, Lh](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => q._8.asInstanceOf[H \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head8[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head7[Lw, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head6[Lw, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head5[Lw, Le, Lf, Lg, Lh](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head4[Lw, Lf, Lg, Lh](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head3[Lw, Lg, Lh](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head2[Lw, Lh](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => q._8.asInstanceOf[H \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw, (A | B | C | D | E | F | G | H) \^ Lv) =
    LabelConflicts.uniq5[Lz, Ly, Lx, Lw, Lv](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => q._8.asInstanceOf[H \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head5[Ly, Le, Lf, Lg, Lh](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head4[Ly, Lf, Lg, Lh](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head3[Ly, Lg, Lh](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head2[Ly, Lh](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => q._8.asInstanceOf[H \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head8[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head7[Lx, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head6[Lx, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head5[Lx, Le, Lf, Lg, Lh](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head4[Lx, Lf, Lg, Lh](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head3[Lx, Lg, Lh](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head2[Lx, Lh](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => q._8.asInstanceOf[H \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head8[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head7[Lw, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head6[Lw, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head5[Lw, Le, Lf, Lg, Lh](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head4[Lw, Lf, Lg, Lh](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head3[Lw, Lg, Lh](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head2[Lw, Lh](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => q._8.asInstanceOf[H \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head8[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head7[Lv, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head6[Lv, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head5[Lv, Le, Lf, Lg, Lh](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head4[Lv, Lf, Lg, Lh](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head3[Lv, Lg, Lh](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => LabelConflicts.head2[Lv, Lh](codeOf(lv))
                               q._7.asInstanceOf[G \^ Lv]
        case _: (Lv =:= Lh) => q._8.asInstanceOf[H \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw, (A | B | C | D | E | F | G | H) \^ Lv, (A | B | C | D | E | F | G | H) \^ Lu) =
    LabelConflicts.uniq6[Lz, Ly, Lx, Lw, Lv, Lu](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => q._8.asInstanceOf[H \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head5[Ly, Le, Lf, Lg, Lh](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head4[Ly, Lf, Lg, Lh](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head3[Ly, Lg, Lh](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head2[Ly, Lh](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => q._8.asInstanceOf[H \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head8[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head7[Lx, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head6[Lx, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head5[Lx, Le, Lf, Lg, Lh](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head4[Lx, Lf, Lg, Lh](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head3[Lx, Lg, Lh](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head2[Lx, Lh](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => q._8.asInstanceOf[H \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head8[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head7[Lw, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head6[Lw, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head5[Lw, Le, Lf, Lg, Lh](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head4[Lw, Lf, Lg, Lh](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head3[Lw, Lg, Lh](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head2[Lw, Lh](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => q._8.asInstanceOf[H \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head8[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head7[Lv, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head6[Lv, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head5[Lv, Le, Lf, Lg, Lh](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head4[Lv, Lf, Lg, Lh](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head3[Lv, Lg, Lh](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => LabelConflicts.head2[Lv, Lh](codeOf(lv))
                               q._7.asInstanceOf[G \^ Lv]
        case _: (Lv =:= Lh) => q._8.asInstanceOf[H \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
      summonFrom:
        case _: (Lu =:= La) => LabelConflicts.head8[Lu, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                               q._1.asInstanceOf[A \^ Lu]
        case _: (Lu =:= Lb) => LabelConflicts.head7[Lu, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                               q._2.asInstanceOf[B \^ Lu]
        case _: (Lu =:= Lc) => LabelConflicts.head6[Lu, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                               q._3.asInstanceOf[C \^ Lu]
        case _: (Lu =:= Ld) => LabelConflicts.head5[Lu, Le, Lf, Lg, Lh](codeOf(lu))
                               q._4.asInstanceOf[D \^ Lu]
        case _: (Lu =:= Le) => LabelConflicts.head4[Lu, Lf, Lg, Lh](codeOf(lu))
                               q._5.asInstanceOf[E \^ Lu]
        case _: (Lu =:= Lf) => LabelConflicts.head3[Lu, Lg, Lh](codeOf(lu))
                               q._6.asInstanceOf[F \^ Lu]
        case _: (Lu =:= Lg) => LabelConflicts.head2[Lu, Lh](codeOf(lu))
                               q._7.asInstanceOf[G \^ Lu]
        case _: (Lu =:= Lh) => q._8.asInstanceOf[H \^ Lu]
        case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw, (A | B | C | D | E | F | G | H) \^ Lv, (A | B | C | D | E | F | G | H) \^ Lu, (A | B | C | D | E | F | G | H) \^ Lt) =
    LabelConflicts.uniq7[Lz, Ly, Lx, Lw, Lv, Lu, Lt](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv), codeOf(lu))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => q._8.asInstanceOf[H \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head5[Ly, Le, Lf, Lg, Lh](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head4[Ly, Lf, Lg, Lh](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head3[Ly, Lg, Lh](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head2[Ly, Lh](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => q._8.asInstanceOf[H \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head8[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head7[Lx, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head6[Lx, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head5[Lx, Le, Lf, Lg, Lh](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head4[Lx, Lf, Lg, Lh](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head3[Lx, Lg, Lh](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head2[Lx, Lh](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => q._8.asInstanceOf[H \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head8[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head7[Lw, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head6[Lw, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head5[Lw, Le, Lf, Lg, Lh](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head4[Lw, Lf, Lg, Lh](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head3[Lw, Lg, Lh](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head2[Lw, Lh](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => q._8.asInstanceOf[H \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head8[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head7[Lv, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head6[Lv, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head5[Lv, Le, Lf, Lg, Lh](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head4[Lv, Lf, Lg, Lh](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head3[Lv, Lg, Lh](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => LabelConflicts.head2[Lv, Lh](codeOf(lv))
                               q._7.asInstanceOf[G \^ Lv]
        case _: (Lv =:= Lh) => q._8.asInstanceOf[H \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
      summonFrom:
        case _: (Lu =:= La) => LabelConflicts.head8[Lu, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                               q._1.asInstanceOf[A \^ Lu]
        case _: (Lu =:= Lb) => LabelConflicts.head7[Lu, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                               q._2.asInstanceOf[B \^ Lu]
        case _: (Lu =:= Lc) => LabelConflicts.head6[Lu, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                               q._3.asInstanceOf[C \^ Lu]
        case _: (Lu =:= Ld) => LabelConflicts.head5[Lu, Le, Lf, Lg, Lh](codeOf(lu))
                               q._4.asInstanceOf[D \^ Lu]
        case _: (Lu =:= Le) => LabelConflicts.head4[Lu, Lf, Lg, Lh](codeOf(lu))
                               q._5.asInstanceOf[E \^ Lu]
        case _: (Lu =:= Lf) => LabelConflicts.head3[Lu, Lg, Lh](codeOf(lu))
                               q._6.asInstanceOf[F \^ Lu]
        case _: (Lu =:= Lg) => LabelConflicts.head2[Lu, Lh](codeOf(lu))
                               q._7.asInstanceOf[G \^ Lu]
        case _: (Lu =:= Lh) => q._8.asInstanceOf[H \^ Lu]
        case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
      summonFrom:
        case _: (Lt =:= La) => LabelConflicts.head8[Lt, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lt))
                               q._1.asInstanceOf[A \^ Lt]
        case _: (Lt =:= Lb) => LabelConflicts.head7[Lt, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lt))
                               q._2.asInstanceOf[B \^ Lt]
        case _: (Lt =:= Lc) => LabelConflicts.head6[Lt, Ld, Le, Lf, Lg, Lh](codeOf(lt))
                               q._3.asInstanceOf[C \^ Lt]
        case _: (Lt =:= Ld) => LabelConflicts.head5[Lt, Le, Lf, Lg, Lh](codeOf(lt))
                               q._4.asInstanceOf[D \^ Lt]
        case _: (Lt =:= Le) => LabelConflicts.head4[Lt, Lf, Lg, Lh](codeOf(lt))
                               q._5.asInstanceOf[E \^ Lt]
        case _: (Lt =:= Lf) => LabelConflicts.head3[Lt, Lg, Lh](codeOf(lt))
                               q._6.asInstanceOf[F \^ Lt]
        case _: (Lt =:= Lg) => LabelConflicts.head2[Lt, Lh](codeOf(lt))
                               q._7.asInstanceOf[G \^ Lt]
        case _: (Lt =:= Lh) => q._8.asInstanceOf[H \^ Lt]
        case _              => compiletime.error("Label " + codeOf(lt) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal, Ls <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt, inline ls: Ls): q.type | ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw, (A | B | C | D | E | F | G | H) \^ Lv, (A | B | C | D | E | F | G | H) \^ Lu, (A | B | C | D | E | F | G | H) \^ Lt, (A | B | C | D | E | F | G | H) \^ Ls) =
    LabelConflicts.uniq8[Lz, Ly, Lx, Lw, Lv, Lu, Lt, Ls](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv), codeOf(lu), codeOf(lt))
    summonFrom:
      case _: ((Lz, Ly, Lx, Lw, Lv, Lu, Lt, Ls) =:= (La, Lb, Lc, Ld, Le, Lf, Lg, Lh)) => q
      case _ => (
        summonFrom:
          case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                                 q._1.asInstanceOf[A \^ Lz]
          case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                                 q._2.asInstanceOf[B \^ Lz]
          case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh](codeOf(lz))
                                 q._3.asInstanceOf[C \^ Lz]
          case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh](codeOf(lz))
                                 q._4.asInstanceOf[D \^ Lz]
          case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh](codeOf(lz))
                                 q._5.asInstanceOf[E \^ Lz]
          case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh](codeOf(lz))
                                 q._6.asInstanceOf[F \^ Lz]
          case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh](codeOf(lz))
                                 q._7.asInstanceOf[G \^ Lz]
          case _: (Lz =:= Lh) => q._8.asInstanceOf[H \^ Lz]
          case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
        summonFrom:
          case _: (Ly =:= La) => LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                                 q._1.asInstanceOf[A \^ Ly]
          case _: (Ly =:= Lb) => LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                                 q._2.asInstanceOf[B \^ Ly]
          case _: (Ly =:= Lc) => LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh](codeOf(ly))
                                 q._3.asInstanceOf[C \^ Ly]
          case _: (Ly =:= Ld) => LabelConflicts.head5[Ly, Le, Lf, Lg, Lh](codeOf(ly))
                                 q._4.asInstanceOf[D \^ Ly]
          case _: (Ly =:= Le) => LabelConflicts.head4[Ly, Lf, Lg, Lh](codeOf(ly))
                                 q._5.asInstanceOf[E \^ Ly]
          case _: (Ly =:= Lf) => LabelConflicts.head3[Ly, Lg, Lh](codeOf(ly))
                                 q._6.asInstanceOf[F \^ Ly]
          case _: (Ly =:= Lg) => LabelConflicts.head2[Ly, Lh](codeOf(ly))
                                 q._7.asInstanceOf[G \^ Ly]
          case _: (Ly =:= Lh) => q._8.asInstanceOf[H \^ Ly]
          case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
        summonFrom:
          case _: (Lx =:= La) => LabelConflicts.head8[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                                 q._1.asInstanceOf[A \^ Lx]
          case _: (Lx =:= Lb) => LabelConflicts.head7[Lx, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                                 q._2.asInstanceOf[B \^ Lx]
          case _: (Lx =:= Lc) => LabelConflicts.head6[Lx, Ld, Le, Lf, Lg, Lh](codeOf(lx))
                                 q._3.asInstanceOf[C \^ Lx]
          case _: (Lx =:= Ld) => LabelConflicts.head5[Lx, Le, Lf, Lg, Lh](codeOf(lx))
                                 q._4.asInstanceOf[D \^ Lx]
          case _: (Lx =:= Le) => LabelConflicts.head4[Lx, Lf, Lg, Lh](codeOf(lx))
                                 q._5.asInstanceOf[E \^ Lx]
          case _: (Lx =:= Lf) => LabelConflicts.head3[Lx, Lg, Lh](codeOf(lx))
                                 q._6.asInstanceOf[F \^ Lx]
          case _: (Lx =:= Lg) => LabelConflicts.head2[Lx, Lh](codeOf(lx))
                                 q._7.asInstanceOf[G \^ Lx]
          case _: (Lx =:= Lh) => q._8.asInstanceOf[H \^ Lx]
          case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
        summonFrom:
          case _: (Lw =:= La) => LabelConflicts.head8[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                                 q._1.asInstanceOf[A \^ Lw]
          case _: (Lw =:= Lb) => LabelConflicts.head7[Lw, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                                 q._2.asInstanceOf[B \^ Lw]
          case _: (Lw =:= Lc) => LabelConflicts.head6[Lw, Ld, Le, Lf, Lg, Lh](codeOf(lw))
                                 q._3.asInstanceOf[C \^ Lw]
          case _: (Lw =:= Ld) => LabelConflicts.head5[Lw, Le, Lf, Lg, Lh](codeOf(lw))
                                 q._4.asInstanceOf[D \^ Lw]
          case _: (Lw =:= Le) => LabelConflicts.head4[Lw, Lf, Lg, Lh](codeOf(lw))
                                 q._5.asInstanceOf[E \^ Lw]
          case _: (Lw =:= Lf) => LabelConflicts.head3[Lw, Lg, Lh](codeOf(lw))
                                 q._6.asInstanceOf[F \^ Lw]
          case _: (Lw =:= Lg) => LabelConflicts.head2[Lw, Lh](codeOf(lw))
                                 q._7.asInstanceOf[G \^ Lw]
          case _: (Lw =:= Lh) => q._8.asInstanceOf[H \^ Lw]
          case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
        summonFrom:
          case _: (Lv =:= La) => LabelConflicts.head8[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                                 q._1.asInstanceOf[A \^ Lv]
          case _: (Lv =:= Lb) => LabelConflicts.head7[Lv, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                                 q._2.asInstanceOf[B \^ Lv]
          case _: (Lv =:= Lc) => LabelConflicts.head6[Lv, Ld, Le, Lf, Lg, Lh](codeOf(lv))
                                 q._3.asInstanceOf[C \^ Lv]
          case _: (Lv =:= Ld) => LabelConflicts.head5[Lv, Le, Lf, Lg, Lh](codeOf(lv))
                                 q._4.asInstanceOf[D \^ Lv]
          case _: (Lv =:= Le) => LabelConflicts.head4[Lv, Lf, Lg, Lh](codeOf(lv))
                                 q._5.asInstanceOf[E \^ Lv]
          case _: (Lv =:= Lf) => LabelConflicts.head3[Lv, Lg, Lh](codeOf(lv))
                                 q._6.asInstanceOf[F \^ Lv]
          case _: (Lv =:= Lg) => LabelConflicts.head2[Lv, Lh](codeOf(lv))
                                 q._7.asInstanceOf[G \^ Lv]
          case _: (Lv =:= Lh) => q._8.asInstanceOf[H \^ Lv]
          case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
        summonFrom:
          case _: (Lu =:= La) => LabelConflicts.head8[Lu, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                                 q._1.asInstanceOf[A \^ Lu]
          case _: (Lu =:= Lb) => LabelConflicts.head7[Lu, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                                 q._2.asInstanceOf[B \^ Lu]
          case _: (Lu =:= Lc) => LabelConflicts.head6[Lu, Ld, Le, Lf, Lg, Lh](codeOf(lu))
                                 q._3.asInstanceOf[C \^ Lu]
          case _: (Lu =:= Ld) => LabelConflicts.head5[Lu, Le, Lf, Lg, Lh](codeOf(lu))
                                 q._4.asInstanceOf[D \^ Lu]
          case _: (Lu =:= Le) => LabelConflicts.head4[Lu, Lf, Lg, Lh](codeOf(lu))
                                 q._5.asInstanceOf[E \^ Lu]
          case _: (Lu =:= Lf) => LabelConflicts.head3[Lu, Lg, Lh](codeOf(lu))
                                 q._6.asInstanceOf[F \^ Lu]
          case _: (Lu =:= Lg) => LabelConflicts.head2[Lu, Lh](codeOf(lu))
                                 q._7.asInstanceOf[G \^ Lu]
          case _: (Lu =:= Lh) => q._8.asInstanceOf[H \^ Lu]
          case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
        summonFrom:
          case _: (Lt =:= La) => LabelConflicts.head8[Lt, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lt))
                                 q._1.asInstanceOf[A \^ Lt]
          case _: (Lt =:= Lb) => LabelConflicts.head7[Lt, Lc, Ld, Le, Lf, Lg, Lh](codeOf(lt))
                                 q._2.asInstanceOf[B \^ Lt]
          case _: (Lt =:= Lc) => LabelConflicts.head6[Lt, Ld, Le, Lf, Lg, Lh](codeOf(lt))
                                 q._3.asInstanceOf[C \^ Lt]
          case _: (Lt =:= Ld) => LabelConflicts.head5[Lt, Le, Lf, Lg, Lh](codeOf(lt))
                                 q._4.asInstanceOf[D \^ Lt]
          case _: (Lt =:= Le) => LabelConflicts.head4[Lt, Lf, Lg, Lh](codeOf(lt))
                                 q._5.asInstanceOf[E \^ Lt]
          case _: (Lt =:= Lf) => LabelConflicts.head3[Lt, Lg, Lh](codeOf(lt))
                                 q._6.asInstanceOf[F \^ Lt]
          case _: (Lt =:= Lg) => LabelConflicts.head2[Lt, Lh](codeOf(lt))
                                 q._7.asInstanceOf[G \^ Lt]
          case _: (Lt =:= Lh) => q._8.asInstanceOf[H \^ Lt]
          case _              => compiletime.error("Label " + codeOf(lt) + " not found"),
        summonFrom:
          case _: (Ls =:= La) => LabelConflicts.head8[Ls, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ls))
                                 q._1.asInstanceOf[A \^ Ls]
          case _: (Ls =:= Lb) => LabelConflicts.head7[Ls, Lc, Ld, Le, Lf, Lg, Lh](codeOf(ls))
                                 q._2.asInstanceOf[B \^ Ls]
          case _: (Ls =:= Lc) => LabelConflicts.head6[Ls, Ld, Le, Lf, Lg, Lh](codeOf(ls))
                                 q._3.asInstanceOf[C \^ Ls]
          case _: (Ls =:= Ld) => LabelConflicts.head5[Ls, Le, Lf, Lg, Lh](codeOf(ls))
                                 q._4.asInstanceOf[D \^ Ls]
          case _: (Ls =:= Le) => LabelConflicts.head4[Ls, Lf, Lg, Lh](codeOf(ls))
                                 q._5.asInstanceOf[E \^ Ls]
          case _: (Ls =:= Lf) => LabelConflicts.head3[Ls, Lg, Lh](codeOf(ls))
                                 q._6.asInstanceOf[F \^ Ls]
          case _: (Ls =:= Lg) => LabelConflicts.head2[Ls, Lh](codeOf(ls))
                                 q._7.asInstanceOf[G \^ Ls]
          case _: (Ls =:= Lh) => q._8.asInstanceOf[H \^ Ls]
          case _              => compiletime.error("Label " + codeOf(ls) + " not found"),
      )

  inline def updatedBy[Z, Lz <: LabelVal](source: Z \^ Lz): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has81[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Z \^ Lz] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                               summonInline[Z <:< A](source.unlabel).labelled[La]
        case _ => q._1,
      summonFrom:
        case _: (Lz =:= Lb) => LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                               summonInline[Z <:< B](source.unlabel).labelled[Lb]
        case _ => q._2,
      summonFrom:
        case _: (Lz =:= Lc) => LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh]("at _3")
                               summonInline[Z <:< C](source.unlabel).labelled[Lc]
        case _ => q._3,
      summonFrom:
        case _: (Lz =:= Ld) => LabelConflicts.head5[Lz, Le, Lf, Lg, Lh]("at _4")
                               summonInline[Z <:< D](source.unlabel).labelled[Ld]
        case _ => q._4,
      summonFrom:
        case _: (Lz =:= Le) => LabelConflicts.head4[Lz, Lf, Lg, Lh]("at _5")
                               summonInline[Z <:< E](source.unlabel).labelled[Le]
        case _ => q._5,
      summonFrom:
        case _: (Lz =:= Lf) => LabelConflicts.head3[Lz, Lg, Lh]("at _6")
                               summonInline[Z <:< F](source.unlabel).labelled[Lf]
        case _ => q._6,
      summonFrom:
        case _: (Lz =:= Lg) => LabelConflicts.head2[Lz, Lh]("at _7")
                               summonInline[Z <:< G](source.unlabel).labelled[Lg]
        case _ => q._7,
      summonFrom:
        case _: (Lz =:= Lh) => summonInline[Z <:< H](source.unlabel).labelled[Lh]
        case _ => q._8
    )

  inline def updatedBy[Nz, Ny](source: (Nz,Ny)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has82[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Nz, Ny] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss1[La, Ny](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss1[Lb, Ny](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss1[Lc, Ny](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss1[Ld, Ny](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss1[Le, Ny](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss1[Lf, Ny](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss1[Lg, Ny](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.miss1[Lh, Ny](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ => q._8
     )

  inline def updatedBy[Nz, Ny, Nx](source: (Nz,Ny,Nx)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has83[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Nz, Ny, Nx] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss2[La, Ny, Nx](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss1[La, Nx](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss2[Lb, Ny, Nx](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss1[Lb, Nx](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss2[Lc, Ny, Nx](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss1[Lc, Nx](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss2[Ld, Ny, Nx](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss1[Ld, Nx](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss2[Le, Ny, Nx](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss1[Le, Nx](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss2[Lf, Ny, Nx](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss1[Lf, Nx](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss2[Lg, Ny, Nx](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss1[Lg, Nx](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.miss2[Lh, Ny, Nx](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.miss1[Lh, Nx](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ => q._8
     )

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz,Ny,Nx,Nw)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has84[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Nz, Ny, Nx, Nw] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss3[La, Ny, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss2[La, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss1[La, Nw](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss3[Lb, Ny, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss2[Lb, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss1[Lb, Nw](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss3[Lc, Ny, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss2[Lc, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss1[Lc, Nw](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss3[Ld, Ny, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss2[Ld, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss1[Ld, Nw](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss3[Le, Ny, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss2[Le, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss1[Le, Nw](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss3[Lf, Ny, Nx, Nw](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss2[Lf, Nx, Nw](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss1[Lf, Nw](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss3[Lg, Ny, Nx, Nw](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss2[Lg, Nx, Nw](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss1[Lg, Nw](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.miss3[Lh, Ny, Nx, Nw](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.miss2[Lh, Nx, Nw](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.miss1[Lh, Nw](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ => q._8
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz,Ny,Nx,Nw,Nv)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has85[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Nz, Ny, Nx, Nw, Nv] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss3[La, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss2[La, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss1[La, Nv](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss4[Lb, Ny, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss3[Lb, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss2[Lb, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss1[Lb, Nv](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss4[Lc, Ny, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss3[Lc, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss2[Lc, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss1[Lc, Nv](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss4[Ld, Ny, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss3[Ld, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss2[Ld, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss1[Ld, Nv](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss4[Le, Ny, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss3[Le, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss2[Le, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss1[Le, Nv](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss4[Lf, Ny, Nx, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss3[Lf, Nx, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss2[Lf, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss1[Lf, Nv](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss4[Lg, Ny, Nx, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss3[Lg, Nx, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss2[Lg, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss1[Lg, Nv](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.miss4[Lh, Ny, Nx, Nw, Nv](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.miss3[Lh, Nx, Nw, Nv](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.miss2[Lh, Nw, Nv](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.miss1[Lh, Nv](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ => q._8
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz,Ny,Nx,Nw,Nv,Nu)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has86[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Nz, Ny, Nx, Nw, Nv, Nu] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss3[La, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss2[La, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss1[La, Nu](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss5[Lb, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss4[Lb, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss3[Lb, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss2[Lb, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss1[Lb, Nu](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss5[Lc, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss4[Lc, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss3[Lc, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss2[Lc, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss1[Lc, Nu](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss5[Ld, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss4[Ld, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss3[Ld, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss2[Ld, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss1[Ld, Nu](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss5[Le, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss4[Le, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss3[Le, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss2[Le, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss1[Le, Nu](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss5[Lf, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss4[Lf, Nx, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss3[Lf, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss2[Lf, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss1[Lf, Nu](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss5[Lg, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss4[Lg, Nx, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss3[Lg, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss2[Lg, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss1[Lg, Nu](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.miss5[Lh, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.miss4[Lh, Nx, Nw, Nv, Nu](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.miss3[Lh, Nw, Nv, Nu](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.miss2[Lh, Nv, Nu](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.miss1[Lh, Nu](" in update corresponding to _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ : ((Nothing \^ Lh) <:< Nu) => summonInline[(Nu <:< (H \^ Lh))](source._6)
        case _ => q._8
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has87[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Nz, Ny, Nx, Nw, Nv, Nu, Nt] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss3[La, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss2[La, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss1[La, Nt](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss6[Lb, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss5[Lb, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss4[Lb, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss3[Lb, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss2[Lb, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss1[Lb, Nt](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss6[Lc, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss5[Lc, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss4[Lc, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss3[Lc, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss2[Lc, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss1[Lc, Nt](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss6[Ld, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss5[Ld, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss4[Ld, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss3[Ld, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss2[Ld, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss1[Ld, Nt](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss6[Le, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss5[Le, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss4[Le, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss3[Le, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss2[Le, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss1[Le, Nt](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss6[Lf, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss5[Lf, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss4[Lf, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss3[Lf, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss2[Lf, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss1[Lf, Nt](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss6[Lg, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss5[Lg, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss4[Lg, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss3[Lg, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss2[Lg, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss1[Lg, Nt](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.miss6[Lh, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.miss5[Lh, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.miss4[Lh, Nw, Nv, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.miss3[Lh, Nv, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.miss2[Lh, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ : ((Nothing \^ Lh) <:< Nu) => LabelConflicts.miss1[Lh, Nt](" in update corresponding to _8")
                                             summonInline[(Nu <:< (H \^ Lh))](source._6)
        case _ : ((Nothing \^ Lh) <:< Nt) => summonInline[(Nt <:< (H \^ Lh))](source._7)
        case _ => q._8
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has88[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss3[La, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss2[La, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss1[La, Ns](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss7[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss6[Lb, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss5[Lb, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss4[Lb, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss3[Lb, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss2[Lb, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss1[Lb, Ns](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss7[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss6[Lc, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss5[Lc, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss4[Lc, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss3[Lc, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss2[Lc, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss1[Lc, Ns](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss7[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss6[Ld, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss5[Ld, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss4[Ld, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss3[Ld, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss2[Ld, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss1[Ld, Ns](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss7[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss6[Le, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss5[Le, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss4[Le, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss3[Le, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss2[Le, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss1[Le, Ns](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss7[Lf, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss6[Lf, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss5[Lf, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss4[Lf, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss3[Lf, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss2[Lf, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss1[Lf, Ns](" in update corresponding to _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ : ((Nothing \^ Lf) <:< Ns) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             summonInline[(Ns <:< (F \^ Lf))](source._8)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss7[Lg, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss6[Lg, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss5[Lg, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss4[Lg, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss3[Lg, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss2[Lg, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss1[Lg, Ns](" in update corresponding to _7")
                                             summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ : ((Nothing \^ Lg) <:< Ns) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             summonInline[(Ns <:< (G \^ Lg))](source._8)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.miss7[Lh, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.miss6[Lh, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.miss5[Lh, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.miss4[Lh, Nv, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.miss3[Lh, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ : ((Nothing \^ Lh) <:< Nu) => LabelConflicts.miss2[Lh, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nu <:< (H \^ Lh))](source._6)
        case _ : ((Nothing \^ Lh) <:< Nt) => LabelConflicts.miss1[Lh, Ns](" in update corresponding to _8")
                                             summonInline[(Nt <:< (H \^ Lh))](source._7)
        case _ : ((Nothing \^ Lh) <:< Ns) => summonInline[(Ns <:< (H \^ Lh))](source._8)
        case _ => q._8
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns,Nr)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    inline if !LabelConflicts.has89[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss3[La, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss2[La, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             LabelConflicts.miss1[La, Nr](" in update corresponding to _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ : ((Nothing \^ La) <:< Nr) => LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _1")
                                             summonInline[(Nr <:< (A \^ La))](source._9)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss8[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss7[Lb, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss6[Lb, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss5[Lb, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss4[Lb, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss3[Lb, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss2[Lb, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             LabelConflicts.miss1[Lb, Nr](" in update corresponding to _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ : ((Nothing \^ Lb) <:< Nr) => LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("at _2")
                                             summonInline[(Nr <:< (B \^ Lb))](source._9)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss8[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss7[Lc, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss6[Lc, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss5[Lc, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss4[Lc, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss3[Lc, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss2[Lc, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             LabelConflicts.miss1[Lc, Nr](" in update corresponding to _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ : ((Nothing \^ Lc) <:< Nr) => LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("at _3")
                                             summonInline[(Nr <:< (C \^ Lc))](source._9)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss8[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss7[Ld, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss6[Ld, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss5[Ld, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss4[Ld, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss3[Ld, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss2[Ld, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             LabelConflicts.miss1[Ld, Nr](" in update corresponding to _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ : ((Nothing \^ Ld) <:< Nr) => LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("at _4")
                                             summonInline[(Nr <:< (D \^ Ld))](source._9)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss8[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss7[Le, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss6[Le, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss5[Le, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss4[Le, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss3[Le, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss2[Le, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             LabelConflicts.miss1[Le, Nr](" in update corresponding to _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ : ((Nothing \^ Le) <:< Nr) => LabelConflicts.head4[Le, Lf, Lg, Lh]("at _5")
                                             summonInline[(Nr <:< (E \^ Le))](source._9)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss8[Lf, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss7[Lf, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss6[Lf, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss5[Lf, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss4[Lf, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss3[Lf, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss2[Lf, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ : ((Nothing \^ Lf) <:< Ns) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             LabelConflicts.miss1[Lf, Nr](" in update corresponding to _6")
                                             summonInline[(Ns <:< (F \^ Lf))](source._8)
        case _ : ((Nothing \^ Lf) <:< Nr) => LabelConflicts.head3[Lf, Lg, Lh]("at _6")
                                             summonInline[(Nr <:< (F \^ Lf))](source._9)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss8[Lg, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss7[Lg, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss6[Lg, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss5[Lg, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss4[Lg, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss3[Lg, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss2[Lg, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ : ((Nothing \^ Lg) <:< Ns) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             LabelConflicts.miss1[Lg, Nr](" in update corresponding to _7")
                                             summonInline[(Ns <:< (G \^ Lg))](source._8)
        case _ : ((Nothing \^ Lg) <:< Nr) => LabelConflicts.head2[Lg, Lh]("at _7")
                                             summonInline[(Nr <:< (G \^ Lg))](source._9)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.miss8[Lh, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.miss7[Lh, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.miss6[Lh, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.miss5[Lh, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.miss4[Lh, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ : ((Nothing \^ Lh) <:< Nu) => LabelConflicts.miss3[Lh, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nu <:< (H \^ Lh))](source._6)
        case _ : ((Nothing \^ Lh) <:< Nt) => LabelConflicts.miss2[Lh, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nt <:< (H \^ Lh))](source._7)
        case _ : ((Nothing \^ Lh) <:< Ns) => LabelConflicts.miss1[Lh, Nr](" in update corresponding to _8")
                                             summonInline[(Ns <:< (H \^ Lh))](source._8)
        case _ : ((Nothing \^ Lh) <:< Nr) => summonInline[(Nr <:< (H \^ Lh))](source._9)
        case _ => q._8
     )

  transparent inline def revalue[Z](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh)(to: Z):
    (Z \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, Z \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, Z \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, Z \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, Z \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, Z \^ Lh)
    =
    inline name match
      case _: La =>
        LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        q.copy(_1 = to.labelled[La])
      case _: Lb =>
        LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        q.copy(_2 = to.labelled[Lb])
      case _: Lc =>
        LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        q.copy(_3 = to.labelled[Lc])
      case _: Ld =>
        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh](codeOf(name))
        q.copy(_4 = to.labelled[Ld])
      case _: Le =>
        LabelConflicts.head4[Le, Lf, Lg, Lh](codeOf(name))
        q.copy(_5 = to.labelled[Le])
      case _: Lf =>
        LabelConflicts.head3[Lf, Lg, Lh](codeOf(name))
        q.copy(_6 = to.labelled[Lf])
      case _: Lg =>
        LabelConflicts.head2[Lg, Lh](codeOf(name))
        q.copy(_7 = to.labelled[Lg])
      case _: Lh =>
        q.copy(_8 = to.labelled[Lh])

  transparent inline def relabel[Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh)(inline lz: Lz):
    (A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lz, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lz, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Lb =>
        LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Lc =>
        LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Ld, Le, Lf, Lg, Lh](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Ld =>
        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Lc, Le, Lf, Lg, Lh](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Le =>
        LabelConflicts.head4[Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Lc, Ld, Lf, Lg, Lh](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Lf =>
        LabelConflicts.head3[Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Lc, Ld, Le, Lg, Lh](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lz, G \^ Lg, H \^ Lh)]
      case _: Lg =>
        LabelConflicts.head2[Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Lc, Ld, Le, Lf, Lh](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lz, H \^ Lh)]
      case _: Lh =>
        LabelConflicts.head8[Lz, La, Lb, Lc, Ld, Le, Lf, Lg](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lz)]

  transparent inline def redo[Z, Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh)(inline to: Z \^ Lz):
    (Z \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, Z \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, Z \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Lz, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Lz, F \^ Lf, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, Z \^ Lz, G \^ Lg, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, Z \^ Lz, H \^ Lh) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, Z \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name) + " changed, creating a labelling which")
        q.copy(_1 = to)
      case _: Lb =>
        LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name) + " changed, creating a labelling which")
        q.copy(_2 = to)
      case _: Lc =>
        LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Ld, Le, Lf, Lg, Lh](codeOf(name) + " changed, creating a labelling which")
        q.copy(_3 = to)
      case _: Ld =>
        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Lc, Le, Lf, Lg, Lh](codeOf(name) + " changed, creating a labelling which")
        q.copy(_4 = to)
      case _: Le =>
        LabelConflicts.head4[Le, Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Lc, Ld, Lf, Lg, Lh](codeOf(name) + " changed, creating a labelling which")
        q.copy(_5 = to)
      case _: Lf =>
        LabelConflicts.head3[Lf, Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Lc, Ld, Le, Lg, Lh](codeOf(name) + " changed, creating a labelling which")
        q.copy(_6 = to)
      case _: Lg =>
        LabelConflicts.head2[Lg, Lh](codeOf(name))
        LabelConflicts.head8[Lz, La, Lb, Lc, Ld, Le, Lf, Lh](codeOf(name) + " changed, creating a labelling which")
        q.copy(_7 = to)
      case _: Lh =>
        LabelConflicts.head8[Lz, La, Lb, Lc, Ld, Le, Lf, Lg](codeOf(name) + " changed, creating a labelling which")
        q.copy(_8 = to)
}

extension [A, B, C, D, E, F, G, H](q: (A, B, C, D, E, F, G, H)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    LabelConflicts.uniq8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7")
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    LabelConflicts.uniq8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(la), codeOf(lb), codeOf(lc), codeOf(ld), codeOf(le), codeOf(lf), codeOf(lg))
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal, F, Lf <: LabelVal, G, Lg <: LabelVal, H, Lh <: LabelVal, I, Li <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li): A | B | C | D | E | F | G | H | I = inline name match
      case _: La =>
        LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        q._4.unlabel
      case _: Le =>
        LabelConflicts.head5[Le, Lf, Lg, Lh, Li](codeOf(name))
        q._5.unlabel
      case _: Lf =>
        LabelConflicts.head4[Lf, Lg, Lh, Li](codeOf(name))
        q._6.unlabel
      case _: Lg =>
        LabelConflicts.head3[Lg, Lh, Li](codeOf(name))
        q._7.unlabel
      case _: Lh =>
        LabelConflicts.head2[Lh, Li](codeOf(name))
        q._8.unlabel
      case _: Li =>
        q._9.unlabel

  inline def unlabel: (A, B, C, D, E, F, G, H, I) = q.asInstanceOf[(A, B, C, D, E, F, G, H, I)]

  transparent inline def label_1: La = compiletime.constValue[La]

  transparent inline def label_2: Lb = compiletime.constValue[Lb]

  transparent inline def label_3: Lc = compiletime.constValue[Lc]

  transparent inline def label_4: Ld = compiletime.constValue[Ld]

  transparent inline def label_5: Le = compiletime.constValue[Le]

  transparent inline def label_6: Lf = compiletime.constValue[Lf]

  transparent inline def label_7: Lg = compiletime.constValue[Lg]

  transparent inline def label_8: Lh = compiletime.constValue[Lh]

  transparent inline def label_9: Li = compiletime.constValue[Li]

  transparent inline def labels: (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li) = (
    compiletime.constValue[La],
    compiletime.constValue[Lb],
    compiletime.constValue[Lc],
    compiletime.constValue[Ld],
    compiletime.constValue[Le],
    compiletime.constValue[Lf],
    compiletime.constValue[Lg],
    compiletime.constValue[Lh],
    compiletime.constValue[Li]
  )

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh, inline li: Li): (A, B, C, D, E, F, G, H, I) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B | C | D | E | F | G | H | I) \^ Lz =
    summonFrom:
      case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                             q._1.asInstanceOf[A \^ Lz]
      case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                             q._2.asInstanceOf[B \^ Lz]
      case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                             q._3.asInstanceOf[C \^ Lz]
      case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                             q._4.asInstanceOf[D \^ Lz]
      case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                             q._5.asInstanceOf[E \^ Lz]
      case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                             q._6.asInstanceOf[F \^ Lz]
      case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                             q._7.asInstanceOf[G \^ Lz]
      case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                             q._8.asInstanceOf[H \^ Lz]
      case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly) =
    LabelConflicts.uniq2[Lz, Ly](codeOf(lz))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                               q._8.asInstanceOf[H \^ Lz]
        case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head9[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head5[Ly, Lf, Lg, Lh, Li](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head4[Ly, Lg, Lh, Li](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head3[Ly, Lh, Li](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => LabelConflicts.head2[Ly, Li](codeOf(ly))
                               q._8.asInstanceOf[H \^ Ly]
        case _: (Ly =:= Li) => q._9.asInstanceOf[I \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx) =
    LabelConflicts.uniq3[Lz, Ly, Lx](codeOf(lz), codeOf(ly))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                               q._8.asInstanceOf[H \^ Lz]
        case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head9[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head5[Ly, Lf, Lg, Lh, Li](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head4[Ly, Lg, Lh, Li](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head3[Ly, Lh, Li](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => LabelConflicts.head2[Ly, Li](codeOf(ly))
                               q._8.asInstanceOf[H \^ Ly]
        case _: (Ly =:= Li) => q._9.asInstanceOf[I \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head8[Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head7[Lx, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head6[Lx, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head5[Lx, Lf, Lg, Lh, Li](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head4[Lx, Lg, Lh, Li](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head3[Lx, Lh, Li](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => LabelConflicts.head2[Lx, Li](codeOf(lx))
                               q._8.asInstanceOf[H \^ Lx]
        case _: (Lx =:= Li) => q._9.asInstanceOf[I \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw) =
    LabelConflicts.uniq4[Lz, Ly, Lx, Lw](codeOf(lz), codeOf(ly), codeOf(lx))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                               q._8.asInstanceOf[H \^ Lz]
        case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head9[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head5[Ly, Lf, Lg, Lh, Li](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head4[Ly, Lg, Lh, Li](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head3[Ly, Lh, Li](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => LabelConflicts.head2[Ly, Li](codeOf(ly))
                               q._8.asInstanceOf[H \^ Ly]
        case _: (Ly =:= Li) => q._9.asInstanceOf[I \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head8[Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head7[Lx, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head6[Lx, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head5[Lx, Lf, Lg, Lh, Li](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head4[Lx, Lg, Lh, Li](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head3[Lx, Lh, Li](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => LabelConflicts.head2[Lx, Li](codeOf(lx))
                               q._8.asInstanceOf[H \^ Lx]
        case _: (Lx =:= Li) => q._9.asInstanceOf[I \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head9[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head8[Lw, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head7[Lw, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head6[Lw, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head5[Lw, Lf, Lg, Lh, Li](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head4[Lw, Lg, Lh, Li](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head3[Lw, Lh, Li](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => LabelConflicts.head2[Lw, Li](codeOf(lw))
                               q._8.asInstanceOf[H \^ Lw]
        case _: (Lw =:= Li) => q._9.asInstanceOf[I \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv) =
    LabelConflicts.uniq5[Lz, Ly, Lx, Lw, Lv](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                               q._8.asInstanceOf[H \^ Lz]
        case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head9[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head5[Ly, Lf, Lg, Lh, Li](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head4[Ly, Lg, Lh, Li](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head3[Ly, Lh, Li](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => LabelConflicts.head2[Ly, Li](codeOf(ly))
                               q._8.asInstanceOf[H \^ Ly]
        case _: (Ly =:= Li) => q._9.asInstanceOf[I \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head8[Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head7[Lx, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head6[Lx, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head5[Lx, Lf, Lg, Lh, Li](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head4[Lx, Lg, Lh, Li](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head3[Lx, Lh, Li](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => LabelConflicts.head2[Lx, Li](codeOf(lx))
                               q._8.asInstanceOf[H \^ Lx]
        case _: (Lx =:= Li) => q._9.asInstanceOf[I \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head9[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head8[Lw, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head7[Lw, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head6[Lw, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head5[Lw, Lf, Lg, Lh, Li](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head4[Lw, Lg, Lh, Li](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head3[Lw, Lh, Li](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => LabelConflicts.head2[Lw, Li](codeOf(lw))
                               q._8.asInstanceOf[H \^ Lw]
        case _: (Lw =:= Li) => q._9.asInstanceOf[I \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head9[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head8[Lv, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head7[Lv, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head6[Lv, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head5[Lv, Lf, Lg, Lh, Li](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head4[Lv, Lg, Lh, Li](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => LabelConflicts.head3[Lv, Lh, Li](codeOf(lv))
                               q._7.asInstanceOf[G \^ Lv]
        case _: (Lv =:= Lh) => LabelConflicts.head2[Lv, Li](codeOf(lv))
                               q._8.asInstanceOf[H \^ Lv]
        case _: (Lv =:= Li) => q._9.asInstanceOf[I \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv, (A | B | C | D | E | F | G | H | I) \^ Lu) =
    LabelConflicts.uniq6[Lz, Ly, Lx, Lw, Lv, Lu](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                               q._8.asInstanceOf[H \^ Lz]
        case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head9[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head5[Ly, Lf, Lg, Lh, Li](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head4[Ly, Lg, Lh, Li](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head3[Ly, Lh, Li](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => LabelConflicts.head2[Ly, Li](codeOf(ly))
                               q._8.asInstanceOf[H \^ Ly]
        case _: (Ly =:= Li) => q._9.asInstanceOf[I \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head8[Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head7[Lx, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head6[Lx, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head5[Lx, Lf, Lg, Lh, Li](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head4[Lx, Lg, Lh, Li](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head3[Lx, Lh, Li](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => LabelConflicts.head2[Lx, Li](codeOf(lx))
                               q._8.asInstanceOf[H \^ Lx]
        case _: (Lx =:= Li) => q._9.asInstanceOf[I \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head9[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head8[Lw, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head7[Lw, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head6[Lw, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head5[Lw, Lf, Lg, Lh, Li](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head4[Lw, Lg, Lh, Li](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head3[Lw, Lh, Li](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => LabelConflicts.head2[Lw, Li](codeOf(lw))
                               q._8.asInstanceOf[H \^ Lw]
        case _: (Lw =:= Li) => q._9.asInstanceOf[I \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head9[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head8[Lv, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head7[Lv, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head6[Lv, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head5[Lv, Lf, Lg, Lh, Li](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head4[Lv, Lg, Lh, Li](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => LabelConflicts.head3[Lv, Lh, Li](codeOf(lv))
                               q._7.asInstanceOf[G \^ Lv]
        case _: (Lv =:= Lh) => LabelConflicts.head2[Lv, Li](codeOf(lv))
                               q._8.asInstanceOf[H \^ Lv]
        case _: (Lv =:= Li) => q._9.asInstanceOf[I \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
      summonFrom:
        case _: (Lu =:= La) => LabelConflicts.head9[Lu, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._1.asInstanceOf[A \^ Lu]
        case _: (Lu =:= Lb) => LabelConflicts.head8[Lu, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._2.asInstanceOf[B \^ Lu]
        case _: (Lu =:= Lc) => LabelConflicts.head7[Lu, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._3.asInstanceOf[C \^ Lu]
        case _: (Lu =:= Ld) => LabelConflicts.head6[Lu, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._4.asInstanceOf[D \^ Lu]
        case _: (Lu =:= Le) => LabelConflicts.head5[Lu, Lf, Lg, Lh, Li](codeOf(lu))
                               q._5.asInstanceOf[E \^ Lu]
        case _: (Lu =:= Lf) => LabelConflicts.head4[Lu, Lg, Lh, Li](codeOf(lu))
                               q._6.asInstanceOf[F \^ Lu]
        case _: (Lu =:= Lg) => LabelConflicts.head3[Lu, Lh, Li](codeOf(lu))
                               q._7.asInstanceOf[G \^ Lu]
        case _: (Lu =:= Lh) => LabelConflicts.head2[Lu, Li](codeOf(lu))
                               q._8.asInstanceOf[H \^ Lu]
        case _: (Lu =:= Li) => q._9.asInstanceOf[I \^ Lu]
        case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv, (A | B | C | D | E | F | G | H | I) \^ Lu, (A | B | C | D | E | F | G | H | I) \^ Lt) =
    LabelConflicts.uniq7[Lz, Ly, Lx, Lw, Lv, Lu, Lt](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv), codeOf(lu))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                               q._8.asInstanceOf[H \^ Lz]
        case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head9[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head5[Ly, Lf, Lg, Lh, Li](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head4[Ly, Lg, Lh, Li](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head3[Ly, Lh, Li](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => LabelConflicts.head2[Ly, Li](codeOf(ly))
                               q._8.asInstanceOf[H \^ Ly]
        case _: (Ly =:= Li) => q._9.asInstanceOf[I \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head8[Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head7[Lx, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head6[Lx, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head5[Lx, Lf, Lg, Lh, Li](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head4[Lx, Lg, Lh, Li](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head3[Lx, Lh, Li](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => LabelConflicts.head2[Lx, Li](codeOf(lx))
                               q._8.asInstanceOf[H \^ Lx]
        case _: (Lx =:= Li) => q._9.asInstanceOf[I \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head9[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head8[Lw, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head7[Lw, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head6[Lw, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head5[Lw, Lf, Lg, Lh, Li](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head4[Lw, Lg, Lh, Li](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head3[Lw, Lh, Li](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => LabelConflicts.head2[Lw, Li](codeOf(lw))
                               q._8.asInstanceOf[H \^ Lw]
        case _: (Lw =:= Li) => q._9.asInstanceOf[I \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head9[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head8[Lv, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head7[Lv, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head6[Lv, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head5[Lv, Lf, Lg, Lh, Li](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head4[Lv, Lg, Lh, Li](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => LabelConflicts.head3[Lv, Lh, Li](codeOf(lv))
                               q._7.asInstanceOf[G \^ Lv]
        case _: (Lv =:= Lh) => LabelConflicts.head2[Lv, Li](codeOf(lv))
                               q._8.asInstanceOf[H \^ Lv]
        case _: (Lv =:= Li) => q._9.asInstanceOf[I \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
      summonFrom:
        case _: (Lu =:= La) => LabelConflicts.head9[Lu, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._1.asInstanceOf[A \^ Lu]
        case _: (Lu =:= Lb) => LabelConflicts.head8[Lu, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._2.asInstanceOf[B \^ Lu]
        case _: (Lu =:= Lc) => LabelConflicts.head7[Lu, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._3.asInstanceOf[C \^ Lu]
        case _: (Lu =:= Ld) => LabelConflicts.head6[Lu, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._4.asInstanceOf[D \^ Lu]
        case _: (Lu =:= Le) => LabelConflicts.head5[Lu, Lf, Lg, Lh, Li](codeOf(lu))
                               q._5.asInstanceOf[E \^ Lu]
        case _: (Lu =:= Lf) => LabelConflicts.head4[Lu, Lg, Lh, Li](codeOf(lu))
                               q._6.asInstanceOf[F \^ Lu]
        case _: (Lu =:= Lg) => LabelConflicts.head3[Lu, Lh, Li](codeOf(lu))
                               q._7.asInstanceOf[G \^ Lu]
        case _: (Lu =:= Lh) => LabelConflicts.head2[Lu, Li](codeOf(lu))
                               q._8.asInstanceOf[H \^ Lu]
        case _: (Lu =:= Li) => q._9.asInstanceOf[I \^ Lu]
        case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
      summonFrom:
        case _: (Lt =:= La) => LabelConflicts.head9[Lt, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                               q._1.asInstanceOf[A \^ Lt]
        case _: (Lt =:= Lb) => LabelConflicts.head8[Lt, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                               q._2.asInstanceOf[B \^ Lt]
        case _: (Lt =:= Lc) => LabelConflicts.head7[Lt, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                               q._3.asInstanceOf[C \^ Lt]
        case _: (Lt =:= Ld) => LabelConflicts.head6[Lt, Le, Lf, Lg, Lh, Li](codeOf(lt))
                               q._4.asInstanceOf[D \^ Lt]
        case _: (Lt =:= Le) => LabelConflicts.head5[Lt, Lf, Lg, Lh, Li](codeOf(lt))
                               q._5.asInstanceOf[E \^ Lt]
        case _: (Lt =:= Lf) => LabelConflicts.head4[Lt, Lg, Lh, Li](codeOf(lt))
                               q._6.asInstanceOf[F \^ Lt]
        case _: (Lt =:= Lg) => LabelConflicts.head3[Lt, Lh, Li](codeOf(lt))
                               q._7.asInstanceOf[G \^ Lt]
        case _: (Lt =:= Lh) => LabelConflicts.head2[Lt, Li](codeOf(lt))
                               q._8.asInstanceOf[H \^ Lt]
        case _: (Lt =:= Li) => q._9.asInstanceOf[I \^ Lt]
        case _              => compiletime.error("Label " + codeOf(lt) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal, Ls <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt, inline ls: Ls): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv, (A | B | C | D | E | F | G | H | I) \^ Lu, (A | B | C | D | E | F | G | H | I) \^ Lt, (A | B | C | D | E | F | G | H | I) \^ Ls) =
    LabelConflicts.uniq8[Lz, Ly, Lx, Lw, Lv, Lu, Lt, Ls](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv), codeOf(lu), codeOf(lt))
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._1.asInstanceOf[A \^ Lz]
        case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._2.asInstanceOf[B \^ Lz]
        case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._3.asInstanceOf[C \^ Lz]
        case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                               q._4.asInstanceOf[D \^ Lz]
        case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                               q._5.asInstanceOf[E \^ Lz]
        case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                               q._6.asInstanceOf[F \^ Lz]
        case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                               q._7.asInstanceOf[G \^ Lz]
        case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                               q._8.asInstanceOf[H \^ Lz]
        case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
        case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
      summonFrom:
        case _: (Ly =:= La) => LabelConflicts.head9[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._1.asInstanceOf[A \^ Ly]
        case _: (Ly =:= Lb) => LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._2.asInstanceOf[B \^ Ly]
        case _: (Ly =:= Lc) => LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._3.asInstanceOf[C \^ Ly]
        case _: (Ly =:= Ld) => LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li](codeOf(ly))
                               q._4.asInstanceOf[D \^ Ly]
        case _: (Ly =:= Le) => LabelConflicts.head5[Ly, Lf, Lg, Lh, Li](codeOf(ly))
                               q._5.asInstanceOf[E \^ Ly]
        case _: (Ly =:= Lf) => LabelConflicts.head4[Ly, Lg, Lh, Li](codeOf(ly))
                               q._6.asInstanceOf[F \^ Ly]
        case _: (Ly =:= Lg) => LabelConflicts.head3[Ly, Lh, Li](codeOf(ly))
                               q._7.asInstanceOf[G \^ Ly]
        case _: (Ly =:= Lh) => LabelConflicts.head2[Ly, Li](codeOf(ly))
                               q._8.asInstanceOf[H \^ Ly]
        case _: (Ly =:= Li) => q._9.asInstanceOf[I \^ Ly]
        case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
      summonFrom:
        case _: (Lx =:= La) => LabelConflicts.head9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._1.asInstanceOf[A \^ Lx]
        case _: (Lx =:= Lb) => LabelConflicts.head8[Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._2.asInstanceOf[B \^ Lx]
        case _: (Lx =:= Lc) => LabelConflicts.head7[Lx, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._3.asInstanceOf[C \^ Lx]
        case _: (Lx =:= Ld) => LabelConflicts.head6[Lx, Le, Lf, Lg, Lh, Li](codeOf(lx))
                               q._4.asInstanceOf[D \^ Lx]
        case _: (Lx =:= Le) => LabelConflicts.head5[Lx, Lf, Lg, Lh, Li](codeOf(lx))
                               q._5.asInstanceOf[E \^ Lx]
        case _: (Lx =:= Lf) => LabelConflicts.head4[Lx, Lg, Lh, Li](codeOf(lx))
                               q._6.asInstanceOf[F \^ Lx]
        case _: (Lx =:= Lg) => LabelConflicts.head3[Lx, Lh, Li](codeOf(lx))
                               q._7.asInstanceOf[G \^ Lx]
        case _: (Lx =:= Lh) => LabelConflicts.head2[Lx, Li](codeOf(lx))
                               q._8.asInstanceOf[H \^ Lx]
        case _: (Lx =:= Li) => q._9.asInstanceOf[I \^ Lx]
        case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
      summonFrom:
        case _: (Lw =:= La) => LabelConflicts.head9[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._1.asInstanceOf[A \^ Lw]
        case _: (Lw =:= Lb) => LabelConflicts.head8[Lw, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._2.asInstanceOf[B \^ Lw]
        case _: (Lw =:= Lc) => LabelConflicts.head7[Lw, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._3.asInstanceOf[C \^ Lw]
        case _: (Lw =:= Ld) => LabelConflicts.head6[Lw, Le, Lf, Lg, Lh, Li](codeOf(lw))
                               q._4.asInstanceOf[D \^ Lw]
        case _: (Lw =:= Le) => LabelConflicts.head5[Lw, Lf, Lg, Lh, Li](codeOf(lw))
                               q._5.asInstanceOf[E \^ Lw]
        case _: (Lw =:= Lf) => LabelConflicts.head4[Lw, Lg, Lh, Li](codeOf(lw))
                               q._6.asInstanceOf[F \^ Lw]
        case _: (Lw =:= Lg) => LabelConflicts.head3[Lw, Lh, Li](codeOf(lw))
                               q._7.asInstanceOf[G \^ Lw]
        case _: (Lw =:= Lh) => LabelConflicts.head2[Lw, Li](codeOf(lw))
                               q._8.asInstanceOf[H \^ Lw]
        case _: (Lw =:= Li) => q._9.asInstanceOf[I \^ Lw]
        case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
      summonFrom:
        case _: (Lv =:= La) => LabelConflicts.head9[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._1.asInstanceOf[A \^ Lv]
        case _: (Lv =:= Lb) => LabelConflicts.head8[Lv, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._2.asInstanceOf[B \^ Lv]
        case _: (Lv =:= Lc) => LabelConflicts.head7[Lv, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._3.asInstanceOf[C \^ Lv]
        case _: (Lv =:= Ld) => LabelConflicts.head6[Lv, Le, Lf, Lg, Lh, Li](codeOf(lv))
                               q._4.asInstanceOf[D \^ Lv]
        case _: (Lv =:= Le) => LabelConflicts.head5[Lv, Lf, Lg, Lh, Li](codeOf(lv))
                               q._5.asInstanceOf[E \^ Lv]
        case _: (Lv =:= Lf) => LabelConflicts.head4[Lv, Lg, Lh, Li](codeOf(lv))
                               q._6.asInstanceOf[F \^ Lv]
        case _: (Lv =:= Lg) => LabelConflicts.head3[Lv, Lh, Li](codeOf(lv))
                               q._7.asInstanceOf[G \^ Lv]
        case _: (Lv =:= Lh) => LabelConflicts.head2[Lv, Li](codeOf(lv))
                               q._8.asInstanceOf[H \^ Lv]
        case _: (Lv =:= Li) => q._9.asInstanceOf[I \^ Lv]
        case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
      summonFrom:
        case _: (Lu =:= La) => LabelConflicts.head9[Lu, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._1.asInstanceOf[A \^ Lu]
        case _: (Lu =:= Lb) => LabelConflicts.head8[Lu, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._2.asInstanceOf[B \^ Lu]
        case _: (Lu =:= Lc) => LabelConflicts.head7[Lu, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._3.asInstanceOf[C \^ Lu]
        case _: (Lu =:= Ld) => LabelConflicts.head6[Lu, Le, Lf, Lg, Lh, Li](codeOf(lu))
                               q._4.asInstanceOf[D \^ Lu]
        case _: (Lu =:= Le) => LabelConflicts.head5[Lu, Lf, Lg, Lh, Li](codeOf(lu))
                               q._5.asInstanceOf[E \^ Lu]
        case _: (Lu =:= Lf) => LabelConflicts.head4[Lu, Lg, Lh, Li](codeOf(lu))
                               q._6.asInstanceOf[F \^ Lu]
        case _: (Lu =:= Lg) => LabelConflicts.head3[Lu, Lh, Li](codeOf(lu))
                               q._7.asInstanceOf[G \^ Lu]
        case _: (Lu =:= Lh) => LabelConflicts.head2[Lu, Li](codeOf(lu))
                               q._8.asInstanceOf[H \^ Lu]
        case _: (Lu =:= Li) => q._9.asInstanceOf[I \^ Lu]
        case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
      summonFrom:
        case _: (Lt =:= La) => LabelConflicts.head9[Lt, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                               q._1.asInstanceOf[A \^ Lt]
        case _: (Lt =:= Lb) => LabelConflicts.head8[Lt, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                               q._2.asInstanceOf[B \^ Lt]
        case _: (Lt =:= Lc) => LabelConflicts.head7[Lt, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                               q._3.asInstanceOf[C \^ Lt]
        case _: (Lt =:= Ld) => LabelConflicts.head6[Lt, Le, Lf, Lg, Lh, Li](codeOf(lt))
                               q._4.asInstanceOf[D \^ Lt]
        case _: (Lt =:= Le) => LabelConflicts.head5[Lt, Lf, Lg, Lh, Li](codeOf(lt))
                               q._5.asInstanceOf[E \^ Lt]
        case _: (Lt =:= Lf) => LabelConflicts.head4[Lt, Lg, Lh, Li](codeOf(lt))
                               q._6.asInstanceOf[F \^ Lt]
        case _: (Lt =:= Lg) => LabelConflicts.head3[Lt, Lh, Li](codeOf(lt))
                               q._7.asInstanceOf[G \^ Lt]
        case _: (Lt =:= Lh) => LabelConflicts.head2[Lt, Li](codeOf(lt))
                               q._8.asInstanceOf[H \^ Lt]
        case _: (Lt =:= Li) => q._9.asInstanceOf[I \^ Lt]
        case _              => compiletime.error("Label " + codeOf(lt) + " not found"),
      summonFrom:
        case _: (Ls =:= La) => LabelConflicts.head9[Ls, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ls))
                               q._1.asInstanceOf[A \^ Ls]
        case _: (Ls =:= Lb) => LabelConflicts.head8[Ls, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ls))
                               q._2.asInstanceOf[B \^ Ls]
        case _: (Ls =:= Lc) => LabelConflicts.head7[Ls, Ld, Le, Lf, Lg, Lh, Li](codeOf(ls))
                               q._3.asInstanceOf[C \^ Ls]
        case _: (Ls =:= Ld) => LabelConflicts.head6[Ls, Le, Lf, Lg, Lh, Li](codeOf(ls))
                               q._4.asInstanceOf[D \^ Ls]
        case _: (Ls =:= Le) => LabelConflicts.head5[Ls, Lf, Lg, Lh, Li](codeOf(ls))
                               q._5.asInstanceOf[E \^ Ls]
        case _: (Ls =:= Lf) => LabelConflicts.head4[Ls, Lg, Lh, Li](codeOf(ls))
                               q._6.asInstanceOf[F \^ Ls]
        case _: (Ls =:= Lg) => LabelConflicts.head3[Ls, Lh, Li](codeOf(ls))
                               q._7.asInstanceOf[G \^ Ls]
        case _: (Ls =:= Lh) => LabelConflicts.head2[Ls, Li](codeOf(ls))
                               q._8.asInstanceOf[H \^ Ls]
        case _: (Ls =:= Li) => q._9.asInstanceOf[I \^ Ls]
        case _              => compiletime.error("Label " + codeOf(ls) + " not found"),
    )

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal, Ls <: LabelVal, Lr <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt, inline ls: Ls, inline lr: Lr): q.type | ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv, (A | B | C | D | E | F | G | H | I) \^ Lu, (A | B | C | D | E | F | G | H | I) \^ Lt, (A | B | C | D | E | F | G | H | I) \^ Ls, (A | B | C | D | E | F | G | H | I) \^ Lr) =
    LabelConflicts.uniq9[Lz, Ly, Lx, Lw, Lv, Lu, Lt, Ls, Lr](codeOf(lz), codeOf(ly), codeOf(lx), codeOf(lw), codeOf(lv), codeOf(lu), codeOf(lt), codeOf(ls))
    summonFrom:
      case _: ((Lz, Ly, Lx, Lw, Lv, Lu, Lt, Ls, Lr) =:= (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li)) => q
      case _ => (
        summonFrom:
          case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                                 q._1.asInstanceOf[A \^ Lz]
          case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                                 q._2.asInstanceOf[B \^ Lz]
          case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li](codeOf(lz))
                                 q._3.asInstanceOf[C \^ Lz]
          case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li](codeOf(lz))
                                 q._4.asInstanceOf[D \^ Lz]
          case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li](codeOf(lz))
                                 q._5.asInstanceOf[E \^ Lz]
          case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li](codeOf(lz))
                                 q._6.asInstanceOf[F \^ Lz]
          case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li](codeOf(lz))
                                 q._7.asInstanceOf[G \^ Lz]
          case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li](codeOf(lz))
                                 q._8.asInstanceOf[H \^ Lz]
          case _: (Lz =:= Li) => q._9.asInstanceOf[I \^ Lz]
          case _              => compiletime.error("Label " + codeOf(lz) + " not found"),
        summonFrom:
          case _: (Ly =:= La) => LabelConflicts.head9[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                                 q._1.asInstanceOf[A \^ Ly]
          case _: (Ly =:= Lb) => LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                                 q._2.asInstanceOf[B \^ Ly]
          case _: (Ly =:= Lc) => LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li](codeOf(ly))
                                 q._3.asInstanceOf[C \^ Ly]
          case _: (Ly =:= Ld) => LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li](codeOf(ly))
                                 q._4.asInstanceOf[D \^ Ly]
          case _: (Ly =:= Le) => LabelConflicts.head5[Ly, Lf, Lg, Lh, Li](codeOf(ly))
                                 q._5.asInstanceOf[E \^ Ly]
          case _: (Ly =:= Lf) => LabelConflicts.head4[Ly, Lg, Lh, Li](codeOf(ly))
                                 q._6.asInstanceOf[F \^ Ly]
          case _: (Ly =:= Lg) => LabelConflicts.head3[Ly, Lh, Li](codeOf(ly))
                                 q._7.asInstanceOf[G \^ Ly]
          case _: (Ly =:= Lh) => LabelConflicts.head2[Ly, Li](codeOf(ly))
                                 q._8.asInstanceOf[H \^ Ly]
          case _: (Ly =:= Li) => q._9.asInstanceOf[I \^ Ly]
          case _              => compiletime.error("Label " + codeOf(ly) + " not found"),
        summonFrom:
          case _: (Lx =:= La) => LabelConflicts.head9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                                 q._1.asInstanceOf[A \^ Lx]
          case _: (Lx =:= Lb) => LabelConflicts.head8[Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                                 q._2.asInstanceOf[B \^ Lx]
          case _: (Lx =:= Lc) => LabelConflicts.head7[Lx, Ld, Le, Lf, Lg, Lh, Li](codeOf(lx))
                                 q._3.asInstanceOf[C \^ Lx]
          case _: (Lx =:= Ld) => LabelConflicts.head6[Lx, Le, Lf, Lg, Lh, Li](codeOf(lx))
                                 q._4.asInstanceOf[D \^ Lx]
          case _: (Lx =:= Le) => LabelConflicts.head5[Lx, Lf, Lg, Lh, Li](codeOf(lx))
                                 q._5.asInstanceOf[E \^ Lx]
          case _: (Lx =:= Lf) => LabelConflicts.head4[Lx, Lg, Lh, Li](codeOf(lx))
                                 q._6.asInstanceOf[F \^ Lx]
          case _: (Lx =:= Lg) => LabelConflicts.head3[Lx, Lh, Li](codeOf(lx))
                                 q._7.asInstanceOf[G \^ Lx]
          case _: (Lx =:= Lh) => LabelConflicts.head2[Lx, Li](codeOf(lx))
                                 q._8.asInstanceOf[H \^ Lx]
          case _: (Lx =:= Li) => q._9.asInstanceOf[I \^ Lx]
          case _              => compiletime.error("Label " + codeOf(lx) + " not found"),
        summonFrom:
          case _: (Lw =:= La) => LabelConflicts.head9[Lw, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                                 q._1.asInstanceOf[A \^ Lw]
          case _: (Lw =:= Lb) => LabelConflicts.head8[Lw, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                                 q._2.asInstanceOf[B \^ Lw]
          case _: (Lw =:= Lc) => LabelConflicts.head7[Lw, Ld, Le, Lf, Lg, Lh, Li](codeOf(lw))
                                 q._3.asInstanceOf[C \^ Lw]
          case _: (Lw =:= Ld) => LabelConflicts.head6[Lw, Le, Lf, Lg, Lh, Li](codeOf(lw))
                                 q._4.asInstanceOf[D \^ Lw]
          case _: (Lw =:= Le) => LabelConflicts.head5[Lw, Lf, Lg, Lh, Li](codeOf(lw))
                                 q._5.asInstanceOf[E \^ Lw]
          case _: (Lw =:= Lf) => LabelConflicts.head4[Lw, Lg, Lh, Li](codeOf(lw))
                                 q._6.asInstanceOf[F \^ Lw]
          case _: (Lw =:= Lg) => LabelConflicts.head3[Lw, Lh, Li](codeOf(lw))
                                 q._7.asInstanceOf[G \^ Lw]
          case _: (Lw =:= Lh) => LabelConflicts.head2[Lw, Li](codeOf(lw))
                                 q._8.asInstanceOf[H \^ Lw]
          case _: (Lw =:= Li) => q._9.asInstanceOf[I \^ Lw]
          case _              => compiletime.error("Label " + codeOf(lw) + " not found"),
        summonFrom:
          case _: (Lv =:= La) => LabelConflicts.head9[Lv, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                                 q._1.asInstanceOf[A \^ Lv]
          case _: (Lv =:= Lb) => LabelConflicts.head8[Lv, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                                 q._2.asInstanceOf[B \^ Lv]
          case _: (Lv =:= Lc) => LabelConflicts.head7[Lv, Ld, Le, Lf, Lg, Lh, Li](codeOf(lv))
                                 q._3.asInstanceOf[C \^ Lv]
          case _: (Lv =:= Ld) => LabelConflicts.head6[Lv, Le, Lf, Lg, Lh, Li](codeOf(lv))
                                 q._4.asInstanceOf[D \^ Lv]
          case _: (Lv =:= Le) => LabelConflicts.head5[Lv, Lf, Lg, Lh, Li](codeOf(lv))
                                 q._5.asInstanceOf[E \^ Lv]
          case _: (Lv =:= Lf) => LabelConflicts.head4[Lv, Lg, Lh, Li](codeOf(lv))
                                 q._6.asInstanceOf[F \^ Lv]
          case _: (Lv =:= Lg) => LabelConflicts.head3[Lv, Lh, Li](codeOf(lv))
                                 q._7.asInstanceOf[G \^ Lv]
          case _: (Lv =:= Lh) => LabelConflicts.head2[Lv, Li](codeOf(lv))
                                 q._8.asInstanceOf[H \^ Lv]
          case _: (Lv =:= Li) => q._9.asInstanceOf[I \^ Lv]
          case _              => compiletime.error("Label " + codeOf(lv) + " not found"),
        summonFrom:
          case _: (Lu =:= La) => LabelConflicts.head9[Lu, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                                 q._1.asInstanceOf[A \^ Lu]
          case _: (Lu =:= Lb) => LabelConflicts.head8[Lu, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                                 q._2.asInstanceOf[B \^ Lu]
          case _: (Lu =:= Lc) => LabelConflicts.head7[Lu, Ld, Le, Lf, Lg, Lh, Li](codeOf(lu))
                                 q._3.asInstanceOf[C \^ Lu]
          case _: (Lu =:= Ld) => LabelConflicts.head6[Lu, Le, Lf, Lg, Lh, Li](codeOf(lu))
                                 q._4.asInstanceOf[D \^ Lu]
          case _: (Lu =:= Le) => LabelConflicts.head5[Lu, Lf, Lg, Lh, Li](codeOf(lu))
                                 q._5.asInstanceOf[E \^ Lu]
          case _: (Lu =:= Lf) => LabelConflicts.head4[Lu, Lg, Lh, Li](codeOf(lu))
                                 q._6.asInstanceOf[F \^ Lu]
          case _: (Lu =:= Lg) => LabelConflicts.head3[Lu, Lh, Li](codeOf(lu))
                                 q._7.asInstanceOf[G \^ Lu]
          case _: (Lu =:= Lh) => LabelConflicts.head2[Lu, Li](codeOf(lu))
                                 q._8.asInstanceOf[H \^ Lu]
          case _: (Lu =:= Li) => q._9.asInstanceOf[I \^ Lu]
          case _              => compiletime.error("Label " + codeOf(lu) + " not found"),
        summonFrom:
          case _: (Lt =:= La) => LabelConflicts.head9[Lt, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                                 q._1.asInstanceOf[A \^ Lt]
          case _: (Lt =:= Lb) => LabelConflicts.head8[Lt, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                                 q._2.asInstanceOf[B \^ Lt]
          case _: (Lt =:= Lc) => LabelConflicts.head7[Lt, Ld, Le, Lf, Lg, Lh, Li](codeOf(lt))
                                 q._3.asInstanceOf[C \^ Lt]
          case _: (Lt =:= Ld) => LabelConflicts.head6[Lt, Le, Lf, Lg, Lh, Li](codeOf(lt))
                                 q._4.asInstanceOf[D \^ Lt]
          case _: (Lt =:= Le) => LabelConflicts.head5[Lt, Lf, Lg, Lh, Li](codeOf(lt))
                                 q._5.asInstanceOf[E \^ Lt]
          case _: (Lt =:= Lf) => LabelConflicts.head4[Lt, Lg, Lh, Li](codeOf(lt))
                                 q._6.asInstanceOf[F \^ Lt]
          case _: (Lt =:= Lg) => LabelConflicts.head3[Lt, Lh, Li](codeOf(lt))
                                 q._7.asInstanceOf[G \^ Lt]
          case _: (Lt =:= Lh) => LabelConflicts.head2[Lt, Li](codeOf(lt))
                                 q._8.asInstanceOf[H \^ Lt]
          case _: (Lt =:= Li) => q._9.asInstanceOf[I \^ Lt]
          case _              => compiletime.error("Label " + codeOf(lt) + " not found"),
        summonFrom:
          case _: (Ls =:= La) => LabelConflicts.head9[Ls, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ls))
                                 q._1.asInstanceOf[A \^ Ls]
          case _: (Ls =:= Lb) => LabelConflicts.head8[Ls, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(ls))
                                 q._2.asInstanceOf[B \^ Ls]
          case _: (Ls =:= Lc) => LabelConflicts.head7[Ls, Ld, Le, Lf, Lg, Lh, Li](codeOf(ls))
                                 q._3.asInstanceOf[C \^ Ls]
          case _: (Ls =:= Ld) => LabelConflicts.head6[Ls, Le, Lf, Lg, Lh, Li](codeOf(ls))
                                 q._4.asInstanceOf[D \^ Ls]
          case _: (Ls =:= Le) => LabelConflicts.head5[Ls, Lf, Lg, Lh, Li](codeOf(ls))
                                 q._5.asInstanceOf[E \^ Ls]
          case _: (Ls =:= Lf) => LabelConflicts.head4[Ls, Lg, Lh, Li](codeOf(ls))
                                 q._6.asInstanceOf[F \^ Ls]
          case _: (Ls =:= Lg) => LabelConflicts.head3[Ls, Lh, Li](codeOf(ls))
                                 q._7.asInstanceOf[G \^ Ls]
          case _: (Ls =:= Lh) => LabelConflicts.head2[Ls, Li](codeOf(ls))
                                 q._8.asInstanceOf[H \^ Ls]
          case _: (Ls =:= Li) => q._9.asInstanceOf[I \^ Ls]
          case _              => compiletime.error("Label " + codeOf(ls) + " not found"),
        summonFrom:
          case _: (Lr =:= La) => LabelConflicts.head9[Lr, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lr))
                                 q._1.asInstanceOf[A \^ Lr]
          case _: (Lr =:= Lb) => LabelConflicts.head8[Lr, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(lr))
                                 q._2.asInstanceOf[B \^ Lr]
          case _: (Lr =:= Lc) => LabelConflicts.head7[Lr, Ld, Le, Lf, Lg, Lh, Li](codeOf(lr))
                                 q._3.asInstanceOf[C \^ Lr]
          case _: (Lr =:= Ld) => LabelConflicts.head6[Lr, Le, Lf, Lg, Lh, Li](codeOf(lr))
                                 q._4.asInstanceOf[D \^ Lr]
          case _: (Lr =:= Le) => LabelConflicts.head5[Lr, Lf, Lg, Lh, Li](codeOf(lr))
                                 q._5.asInstanceOf[E \^ Lr]
          case _: (Lr =:= Lf) => LabelConflicts.head4[Lr, Lg, Lh, Li](codeOf(lr))
                                 q._6.asInstanceOf[F \^ Lr]
          case _: (Lr =:= Lg) => LabelConflicts.head3[Lr, Lh, Li](codeOf(lr))
                                 q._7.asInstanceOf[G \^ Lr]
          case _: (Lr =:= Lh) => LabelConflicts.head2[Lr, Li](codeOf(lr))
                                 q._8.asInstanceOf[H \^ Lr]
          case _: (Lr =:= Li) => q._9.asInstanceOf[I \^ Lr]
          case _              => compiletime.error("Label " + codeOf(lr) + " not found"),
      )

  inline def updatedBy[Z, Lz <: LabelVal](source: Z \^ Lz): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has91[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Z \^ Lz] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _: (Lz =:= La) => LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                               summonInline[Z <:< A](source.unlabel).labelled[La]
        case _ => q._1,
      summonFrom:
        case _: (Lz =:= Lb) => LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                               summonInline[Z <:< B](source.unlabel).labelled[Lb]
        case _ => q._2,
      summonFrom:
        case _: (Lz =:= Lc) => LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                               summonInline[Z <:< C](source.unlabel).labelled[Lc]
        case _ => q._3,
      summonFrom:
        case _: (Lz =:= Ld) => LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li]("at _4")
                               summonInline[Z <:< D](source.unlabel).labelled[Ld]
        case _ => q._4,
      summonFrom:
        case _: (Lz =:= Le) => LabelConflicts.head5[Lz, Lf, Lg, Lh, Li]("at _5")
                               summonInline[Z <:< E](source.unlabel).labelled[Le]
        case _ => q._5,
      summonFrom:
        case _: (Lz =:= Lf) => LabelConflicts.head4[Lz, Lg, Lh, Li]("at _6")
                               summonInline[Z <:< F](source.unlabel).labelled[Lf]
        case _ => q._6,
      summonFrom:
        case _: (Lz =:= Lg) => LabelConflicts.head3[Lz, Lh, Li]("at _7")
                               summonInline[Z <:< G](source.unlabel).labelled[Lg]
        case _ => q._7,
      summonFrom:
        case _: (Lz =:= Lh) => LabelConflicts.head2[Lz, Li]("at _8")
                               summonInline[Z <:< H](source.unlabel).labelled[Lh]
        case _ => q._8,
      summonFrom:
        case _: (Lz =:= Li) => summonInline[Z <:< I](source.unlabel).labelled[Li]
        case _ => q._9
    )

  inline def updatedBy[Nz, Ny](source: (Nz,Ny)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has92[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Nz, Ny] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss1[La, Ny](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss1[Lb, Ny](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss1[Lc, Ny](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss1[Ld, Ny](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss1[Le, Ny](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss1[Lf, Ny](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss1[Lg, Ny](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss1[Lh, Ny](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.head2[Lh, Li]("at _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ => q._8,
      summonFrom:
        case _ : ((Nothing \^ Li) <:< Nz) => LabelConflicts.miss1[Li, Ny](" in update corresponding to _9")
                                             summonInline[(Nz <:< (I \^ Li))](source._1)
        case _ : ((Nothing \^ Li) <:< Ny) => summonInline[(Ny <:< (I \^ Li))](source._2)
        case _ => q._9
     )

  inline def updatedBy[Nz, Ny, Nx](source: (Nz,Ny,Nx)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has93[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Nz, Ny, Nx] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss2[La, Ny, Nx](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss1[La, Nx](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss2[Lb, Ny, Nx](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss1[Lb, Nx](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss2[Lc, Ny, Nx](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss1[Lc, Nx](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss2[Ld, Ny, Nx](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss1[Ld, Nx](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss2[Le, Ny, Nx](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss1[Le, Nx](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss2[Lf, Ny, Nx](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss1[Lf, Nx](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss2[Lg, Ny, Nx](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss1[Lg, Nx](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss2[Lh, Ny, Nx](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss1[Lh, Nx](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.head2[Lh, Li]("at _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ => q._8,
      summonFrom:
        case _ : ((Nothing \^ Li) <:< Nz) => LabelConflicts.miss2[Li, Ny, Nx](" in update corresponding to _9")
                                             summonInline[(Nz <:< (I \^ Li))](source._1)
        case _ : ((Nothing \^ Li) <:< Ny) => LabelConflicts.miss1[Li, Nx](" in update corresponding to _9")
                                             summonInline[(Ny <:< (I \^ Li))](source._2)
        case _ : ((Nothing \^ Li) <:< Nx) => summonInline[(Nx <:< (I \^ Li))](source._3)
        case _ => q._9
     )

  inline def updatedBy[Nz, Ny, Nx, Nw](source: (Nz,Ny,Nx,Nw)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has94[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Nz, Ny, Nx, Nw] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss3[La, Ny, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss2[La, Nx, Nw](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss1[La, Nw](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss3[Lb, Ny, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss2[Lb, Nx, Nw](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss1[Lb, Nw](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss3[Lc, Ny, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss2[Lc, Nx, Nw](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss1[Lc, Nw](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss3[Ld, Ny, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss2[Ld, Nx, Nw](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss1[Ld, Nw](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss3[Le, Ny, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss2[Le, Nx, Nw](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss1[Le, Nw](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss3[Lf, Ny, Nx, Nw](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss2[Lf, Nx, Nw](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss1[Lf, Nw](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss3[Lg, Ny, Nx, Nw](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss2[Lg, Nx, Nw](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss1[Lg, Nw](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss3[Lh, Ny, Nx, Nw](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss2[Lh, Nx, Nw](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss1[Lh, Nw](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.head2[Lh, Li]("at _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ => q._8,
      summonFrom:
        case _ : ((Nothing \^ Li) <:< Nz) => LabelConflicts.miss3[Li, Ny, Nx, Nw](" in update corresponding to _9")
                                             summonInline[(Nz <:< (I \^ Li))](source._1)
        case _ : ((Nothing \^ Li) <:< Ny) => LabelConflicts.miss2[Li, Nx, Nw](" in update corresponding to _9")
                                             summonInline[(Ny <:< (I \^ Li))](source._2)
        case _ : ((Nothing \^ Li) <:< Nx) => LabelConflicts.miss1[Li, Nw](" in update corresponding to _9")
                                             summonInline[(Nx <:< (I \^ Li))](source._3)
        case _ : ((Nothing \^ Li) <:< Nw) => summonInline[(Nw <:< (I \^ Li))](source._4)
        case _ => q._9
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv](source: (Nz,Ny,Nx,Nw,Nv)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has95[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Nz, Ny, Nx, Nw, Nv] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss4[La, Ny, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss3[La, Nx, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss2[La, Nw, Nv](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss1[La, Nv](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss4[Lb, Ny, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss3[Lb, Nx, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss2[Lb, Nw, Nv](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss1[Lb, Nv](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss4[Lc, Ny, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss3[Lc, Nx, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss2[Lc, Nw, Nv](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss1[Lc, Nv](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss4[Ld, Ny, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss3[Ld, Nx, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss2[Ld, Nw, Nv](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss1[Ld, Nv](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss4[Le, Ny, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss3[Le, Nx, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss2[Le, Nw, Nv](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss1[Le, Nv](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss4[Lf, Ny, Nx, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss3[Lf, Nx, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss2[Lf, Nw, Nv](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss1[Lf, Nv](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss4[Lg, Ny, Nx, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss3[Lg, Nx, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss2[Lg, Nw, Nv](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss1[Lg, Nv](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss4[Lh, Ny, Nx, Nw, Nv](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss3[Lh, Nx, Nw, Nv](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss2[Lh, Nw, Nv](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss1[Lh, Nv](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.head2[Lh, Li]("at _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ => q._8,
      summonFrom:
        case _ : ((Nothing \^ Li) <:< Nz) => LabelConflicts.miss4[Li, Ny, Nx, Nw, Nv](" in update corresponding to _9")
                                             summonInline[(Nz <:< (I \^ Li))](source._1)
        case _ : ((Nothing \^ Li) <:< Ny) => LabelConflicts.miss3[Li, Nx, Nw, Nv](" in update corresponding to _9")
                                             summonInline[(Ny <:< (I \^ Li))](source._2)
        case _ : ((Nothing \^ Li) <:< Nx) => LabelConflicts.miss2[Li, Nw, Nv](" in update corresponding to _9")
                                             summonInline[(Nx <:< (I \^ Li))](source._3)
        case _ : ((Nothing \^ Li) <:< Nw) => LabelConflicts.miss1[Li, Nv](" in update corresponding to _9")
                                             summonInline[(Nw <:< (I \^ Li))](source._4)
        case _ : ((Nothing \^ Li) <:< Nv) => summonInline[(Nv <:< (I \^ Li))](source._5)
        case _ => q._9
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu](source: (Nz,Ny,Nx,Nw,Nv,Nu)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has96[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Nz, Ny, Nx, Nw, Nv, Nu] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss5[La, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss4[La, Nx, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss3[La, Nw, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss2[La, Nv, Nu](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss1[La, Nu](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss5[Lb, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss4[Lb, Nx, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss3[Lb, Nw, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss2[Lb, Nv, Nu](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss1[Lb, Nu](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss5[Lc, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss4[Lc, Nx, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss3[Lc, Nw, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss2[Lc, Nv, Nu](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss1[Lc, Nu](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss5[Ld, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss4[Ld, Nx, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss3[Ld, Nw, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss2[Ld, Nv, Nu](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss1[Ld, Nu](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss5[Le, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss4[Le, Nx, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss3[Le, Nw, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss2[Le, Nv, Nu](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss1[Le, Nu](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss5[Lf, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss4[Lf, Nx, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss3[Lf, Nw, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss2[Lf, Nv, Nu](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss1[Lf, Nu](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss5[Lg, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss4[Lg, Nx, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss3[Lg, Nw, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss2[Lg, Nv, Nu](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss1[Lg, Nu](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss5[Lh, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss4[Lh, Nx, Nw, Nv, Nu](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss3[Lh, Nw, Nv, Nu](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss2[Lh, Nv, Nu](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss1[Lh, Nu](" in update corresponding to _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ : ((Nothing \^ Lh) <:< Nu) => LabelConflicts.head2[Lh, Li]("at _8")
                                             summonInline[(Nu <:< (H \^ Lh))](source._6)
        case _ => q._8,
      summonFrom:
        case _ : ((Nothing \^ Li) <:< Nz) => LabelConflicts.miss5[Li, Ny, Nx, Nw, Nv, Nu](" in update corresponding to _9")
                                             summonInline[(Nz <:< (I \^ Li))](source._1)
        case _ : ((Nothing \^ Li) <:< Ny) => LabelConflicts.miss4[Li, Nx, Nw, Nv, Nu](" in update corresponding to _9")
                                             summonInline[(Ny <:< (I \^ Li))](source._2)
        case _ : ((Nothing \^ Li) <:< Nx) => LabelConflicts.miss3[Li, Nw, Nv, Nu](" in update corresponding to _9")
                                             summonInline[(Nx <:< (I \^ Li))](source._3)
        case _ : ((Nothing \^ Li) <:< Nw) => LabelConflicts.miss2[Li, Nv, Nu](" in update corresponding to _9")
                                             summonInline[(Nw <:< (I \^ Li))](source._4)
        case _ : ((Nothing \^ Li) <:< Nv) => LabelConflicts.miss1[Li, Nu](" in update corresponding to _9")
                                             summonInline[(Nv <:< (I \^ Li))](source._5)
        case _ : ((Nothing \^ Li) <:< Nu) => summonInline[(Nu <:< (I \^ Li))](source._6)
        case _ => q._9
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has97[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Nz, Ny, Nx, Nw, Nv, Nu, Nt] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss6[La, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss5[La, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss4[La, Nw, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss3[La, Nv, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss2[La, Nu, Nt](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss1[La, Nt](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss6[Lb, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss5[Lb, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss4[Lb, Nw, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss3[Lb, Nv, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss2[Lb, Nu, Nt](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss1[Lb, Nt](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss6[Lc, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss5[Lc, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss4[Lc, Nw, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss3[Lc, Nv, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss2[Lc, Nu, Nt](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss1[Lc, Nt](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss6[Ld, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss5[Ld, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss4[Ld, Nw, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss3[Ld, Nv, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss2[Ld, Nu, Nt](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss1[Ld, Nt](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss6[Le, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss5[Le, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss4[Le, Nw, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss3[Le, Nv, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss2[Le, Nu, Nt](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss1[Le, Nt](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss6[Lf, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss5[Lf, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss4[Lf, Nw, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss3[Lf, Nv, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss2[Lf, Nu, Nt](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss1[Lf, Nt](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss6[Lg, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss5[Lg, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss4[Lg, Nw, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss3[Lg, Nv, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss2[Lg, Nu, Nt](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss1[Lg, Nt](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss6[Lh, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss5[Lh, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss4[Lh, Nw, Nv, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss3[Lh, Nv, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss2[Lh, Nu, Nt](" in update corresponding to _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ : ((Nothing \^ Lh) <:< Nu) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss1[Lh, Nt](" in update corresponding to _8")
                                             summonInline[(Nu <:< (H \^ Lh))](source._6)
        case _ : ((Nothing \^ Lh) <:< Nt) => LabelConflicts.head2[Lh, Li]("at _8")
                                             summonInline[(Nt <:< (H \^ Lh))](source._7)
        case _ => q._8,
      summonFrom:
        case _ : ((Nothing \^ Li) <:< Nz) => LabelConflicts.miss6[Li, Ny, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _9")
                                             summonInline[(Nz <:< (I \^ Li))](source._1)
        case _ : ((Nothing \^ Li) <:< Ny) => LabelConflicts.miss5[Li, Nx, Nw, Nv, Nu, Nt](" in update corresponding to _9")
                                             summonInline[(Ny <:< (I \^ Li))](source._2)
        case _ : ((Nothing \^ Li) <:< Nx) => LabelConflicts.miss4[Li, Nw, Nv, Nu, Nt](" in update corresponding to _9")
                                             summonInline[(Nx <:< (I \^ Li))](source._3)
        case _ : ((Nothing \^ Li) <:< Nw) => LabelConflicts.miss3[Li, Nv, Nu, Nt](" in update corresponding to _9")
                                             summonInline[(Nw <:< (I \^ Li))](source._4)
        case _ : ((Nothing \^ Li) <:< Nv) => LabelConflicts.miss2[Li, Nu, Nt](" in update corresponding to _9")
                                             summonInline[(Nv <:< (I \^ Li))](source._5)
        case _ : ((Nothing \^ Li) <:< Nu) => LabelConflicts.miss1[Li, Nt](" in update corresponding to _9")
                                             summonInline[(Nu <:< (I \^ Li))](source._6)
        case _ : ((Nothing \^ Li) <:< Nt) => summonInline[(Nt <:< (I \^ Li))](source._7)
        case _ => q._9
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has98[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss7[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss6[La, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss5[La, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss4[La, Nv, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss3[La, Nu, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss2[La, Nt, Ns](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss1[La, Ns](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss7[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss6[Lb, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss5[Lb, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss4[Lb, Nv, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss3[Lb, Nu, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss2[Lb, Nt, Ns](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss1[Lb, Ns](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss7[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss6[Lc, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss5[Lc, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss4[Lc, Nv, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss3[Lc, Nu, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss2[Lc, Nt, Ns](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss1[Lc, Ns](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss7[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss6[Ld, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss5[Ld, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss4[Ld, Nv, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss3[Ld, Nu, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss2[Ld, Nt, Ns](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss1[Ld, Ns](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss7[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss6[Le, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss5[Le, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss4[Le, Nv, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss3[Le, Nu, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss2[Le, Nt, Ns](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss1[Le, Ns](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss7[Lf, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss6[Lf, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss5[Lf, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss4[Lf, Nv, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss3[Lf, Nu, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss2[Lf, Nt, Ns](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss1[Lf, Ns](" in update corresponding to _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ : ((Nothing \^ Lf) <:< Ns) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             summonInline[(Ns <:< (F \^ Lf))](source._8)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss7[Lg, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss6[Lg, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss5[Lg, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss4[Lg, Nv, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss3[Lg, Nu, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss2[Lg, Nt, Ns](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss1[Lg, Ns](" in update corresponding to _7")
                                             summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ : ((Nothing \^ Lg) <:< Ns) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             summonInline[(Ns <:< (G \^ Lg))](source._8)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss7[Lh, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss6[Lh, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss5[Lh, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss4[Lh, Nv, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss3[Lh, Nu, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ : ((Nothing \^ Lh) <:< Nu) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss2[Lh, Nt, Ns](" in update corresponding to _8")
                                             summonInline[(Nu <:< (H \^ Lh))](source._6)
        case _ : ((Nothing \^ Lh) <:< Nt) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss1[Lh, Ns](" in update corresponding to _8")
                                             summonInline[(Nt <:< (H \^ Lh))](source._7)
        case _ : ((Nothing \^ Lh) <:< Ns) => LabelConflicts.head2[Lh, Li]("at _8")
                                             summonInline[(Ns <:< (H \^ Lh))](source._8)
        case _ => q._8,
      summonFrom:
        case _ : ((Nothing \^ Li) <:< Nz) => LabelConflicts.miss7[Li, Ny, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _9")
                                             summonInline[(Nz <:< (I \^ Li))](source._1)
        case _ : ((Nothing \^ Li) <:< Ny) => LabelConflicts.miss6[Li, Nx, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _9")
                                             summonInline[(Ny <:< (I \^ Li))](source._2)
        case _ : ((Nothing \^ Li) <:< Nx) => LabelConflicts.miss5[Li, Nw, Nv, Nu, Nt, Ns](" in update corresponding to _9")
                                             summonInline[(Nx <:< (I \^ Li))](source._3)
        case _ : ((Nothing \^ Li) <:< Nw) => LabelConflicts.miss4[Li, Nv, Nu, Nt, Ns](" in update corresponding to _9")
                                             summonInline[(Nw <:< (I \^ Li))](source._4)
        case _ : ((Nothing \^ Li) <:< Nv) => LabelConflicts.miss3[Li, Nu, Nt, Ns](" in update corresponding to _9")
                                             summonInline[(Nv <:< (I \^ Li))](source._5)
        case _ : ((Nothing \^ Li) <:< Nu) => LabelConflicts.miss2[Li, Nt, Ns](" in update corresponding to _9")
                                             summonInline[(Nu <:< (I \^ Li))](source._6)
        case _ : ((Nothing \^ Li) <:< Nt) => LabelConflicts.miss1[Li, Ns](" in update corresponding to _9")
                                             summonInline[(Nt <:< (I \^ Li))](source._7)
        case _ : ((Nothing \^ Li) <:< Ns) => summonInline[(Ns <:< (I \^ Li))](source._8)
        case _ => q._9
     )

  inline def updatedBy[Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](source: (Nz,Ny,Nx,Nw,Nv,Nu,Nt,Ns,Nr)): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) =
    inline if !LabelConflicts.has99[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Nz, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr] then compiletime.error("No matching labels found")
    (
      summonFrom:
        case _ : ((Nothing \^ La) <:< Nz) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss8[La, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nz <:< (A \^ La))](source._1)
        case _ : ((Nothing \^ La) <:< Ny) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss7[La, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Ny <:< (A \^ La))](source._2)
        case _ : ((Nothing \^ La) <:< Nx) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss6[La, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nx <:< (A \^ La))](source._3)
        case _ : ((Nothing \^ La) <:< Nw) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss5[La, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nw <:< (A \^ La))](source._4)
        case _ : ((Nothing \^ La) <:< Nv) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss4[La, Nu, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nv <:< (A \^ La))](source._5)
        case _ : ((Nothing \^ La) <:< Nu) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss3[La, Nt, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nu <:< (A \^ La))](source._6)
        case _ : ((Nothing \^ La) <:< Nt) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss2[La, Ns, Nr](" in update corresponding to _1")
                                             summonInline[(Nt <:< (A \^ La))](source._7)
        case _ : ((Nothing \^ La) <:< Ns) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             LabelConflicts.miss1[La, Nr](" in update corresponding to _1")
                                             summonInline[(Ns <:< (A \^ La))](source._8)
        case _ : ((Nothing \^ La) <:< Nr) => LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _1")
                                             summonInline[(Nr <:< (A \^ La))](source._9)
        case _ => q._1,
      summonFrom:
        case _ : ((Nothing \^ Lb) <:< Nz) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss8[Lb, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nz <:< (B \^ Lb))](source._1)
        case _ : ((Nothing \^ Lb) <:< Ny) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss7[Lb, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Ny <:< (B \^ Lb))](source._2)
        case _ : ((Nothing \^ Lb) <:< Nx) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss6[Lb, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nx <:< (B \^ Lb))](source._3)
        case _ : ((Nothing \^ Lb) <:< Nw) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss5[Lb, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nw <:< (B \^ Lb))](source._4)
        case _ : ((Nothing \^ Lb) <:< Nv) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss4[Lb, Nu, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nv <:< (B \^ Lb))](source._5)
        case _ : ((Nothing \^ Lb) <:< Nu) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss3[Lb, Nt, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nu <:< (B \^ Lb))](source._6)
        case _ : ((Nothing \^ Lb) <:< Nt) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss2[Lb, Ns, Nr](" in update corresponding to _2")
                                             summonInline[(Nt <:< (B \^ Lb))](source._7)
        case _ : ((Nothing \^ Lb) <:< Ns) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             LabelConflicts.miss1[Lb, Nr](" in update corresponding to _2")
                                             summonInline[(Ns <:< (B \^ Lb))](source._8)
        case _ : ((Nothing \^ Lb) <:< Nr) => LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("at _2")
                                             summonInline[(Nr <:< (B \^ Lb))](source._9)
        case _ => q._2,
      summonFrom:
        case _ : ((Nothing \^ Lc) <:< Nz) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss8[Lc, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nz <:< (C \^ Lc))](source._1)
        case _ : ((Nothing \^ Lc) <:< Ny) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss7[Lc, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Ny <:< (C \^ Lc))](source._2)
        case _ : ((Nothing \^ Lc) <:< Nx) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss6[Lc, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nx <:< (C \^ Lc))](source._3)
        case _ : ((Nothing \^ Lc) <:< Nw) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss5[Lc, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nw <:< (C \^ Lc))](source._4)
        case _ : ((Nothing \^ Lc) <:< Nv) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss4[Lc, Nu, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nv <:< (C \^ Lc))](source._5)
        case _ : ((Nothing \^ Lc) <:< Nu) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss3[Lc, Nt, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nu <:< (C \^ Lc))](source._6)
        case _ : ((Nothing \^ Lc) <:< Nt) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss2[Lc, Ns, Nr](" in update corresponding to _3")
                                             summonInline[(Nt <:< (C \^ Lc))](source._7)
        case _ : ((Nothing \^ Lc) <:< Ns) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             LabelConflicts.miss1[Lc, Nr](" in update corresponding to _3")
                                             summonInline[(Ns <:< (C \^ Lc))](source._8)
        case _ : ((Nothing \^ Lc) <:< Nr) => LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("at _3")
                                             summonInline[(Nr <:< (C \^ Lc))](source._9)
        case _ => q._3,
      summonFrom:
        case _ : ((Nothing \^ Ld) <:< Nz) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss8[Ld, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nz <:< (D \^ Ld))](source._1)
        case _ : ((Nothing \^ Ld) <:< Ny) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss7[Ld, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Ny <:< (D \^ Ld))](source._2)
        case _ : ((Nothing \^ Ld) <:< Nx) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss6[Ld, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nx <:< (D \^ Ld))](source._3)
        case _ : ((Nothing \^ Ld) <:< Nw) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss5[Ld, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nw <:< (D \^ Ld))](source._4)
        case _ : ((Nothing \^ Ld) <:< Nv) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss4[Ld, Nu, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nv <:< (D \^ Ld))](source._5)
        case _ : ((Nothing \^ Ld) <:< Nu) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss3[Ld, Nt, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nu <:< (D \^ Ld))](source._6)
        case _ : ((Nothing \^ Ld) <:< Nt) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss2[Ld, Ns, Nr](" in update corresponding to _4")
                                             summonInline[(Nt <:< (D \^ Ld))](source._7)
        case _ : ((Nothing \^ Ld) <:< Ns) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             LabelConflicts.miss1[Ld, Nr](" in update corresponding to _4")
                                             summonInline[(Ns <:< (D \^ Ld))](source._8)
        case _ : ((Nothing \^ Ld) <:< Nr) => LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("at _4")
                                             summonInline[(Nr <:< (D \^ Ld))](source._9)
        case _ => q._4,
      summonFrom:
        case _ : ((Nothing \^ Le) <:< Nz) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss8[Le, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nz <:< (E \^ Le))](source._1)
        case _ : ((Nothing \^ Le) <:< Ny) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss7[Le, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Ny <:< (E \^ Le))](source._2)
        case _ : ((Nothing \^ Le) <:< Nx) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss6[Le, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nx <:< (E \^ Le))](source._3)
        case _ : ((Nothing \^ Le) <:< Nw) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss5[Le, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nw <:< (E \^ Le))](source._4)
        case _ : ((Nothing \^ Le) <:< Nv) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss4[Le, Nu, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nv <:< (E \^ Le))](source._5)
        case _ : ((Nothing \^ Le) <:< Nu) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss3[Le, Nt, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nu <:< (E \^ Le))](source._6)
        case _ : ((Nothing \^ Le) <:< Nt) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss2[Le, Ns, Nr](" in update corresponding to _5")
                                             summonInline[(Nt <:< (E \^ Le))](source._7)
        case _ : ((Nothing \^ Le) <:< Ns) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             LabelConflicts.miss1[Le, Nr](" in update corresponding to _5")
                                             summonInline[(Ns <:< (E \^ Le))](source._8)
        case _ : ((Nothing \^ Le) <:< Nr) => LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("at _5")
                                             summonInline[(Nr <:< (E \^ Le))](source._9)
        case _ => q._5,
      summonFrom:
        case _ : ((Nothing \^ Lf) <:< Nz) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss8[Lf, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nz <:< (F \^ Lf))](source._1)
        case _ : ((Nothing \^ Lf) <:< Ny) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss7[Lf, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Ny <:< (F \^ Lf))](source._2)
        case _ : ((Nothing \^ Lf) <:< Nx) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss6[Lf, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nx <:< (F \^ Lf))](source._3)
        case _ : ((Nothing \^ Lf) <:< Nw) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss5[Lf, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nw <:< (F \^ Lf))](source._4)
        case _ : ((Nothing \^ Lf) <:< Nv) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss4[Lf, Nu, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nv <:< (F \^ Lf))](source._5)
        case _ : ((Nothing \^ Lf) <:< Nu) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss3[Lf, Nt, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nu <:< (F \^ Lf))](source._6)
        case _ : ((Nothing \^ Lf) <:< Nt) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss2[Lf, Ns, Nr](" in update corresponding to _6")
                                             summonInline[(Nt <:< (F \^ Lf))](source._7)
        case _ : ((Nothing \^ Lf) <:< Ns) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             LabelConflicts.miss1[Lf, Nr](" in update corresponding to _6")
                                             summonInline[(Ns <:< (F \^ Lf))](source._8)
        case _ : ((Nothing \^ Lf) <:< Nr) => LabelConflicts.head4[Lf, Lg, Lh, Li]("at _6")
                                             summonInline[(Nr <:< (F \^ Lf))](source._9)
        case _ => q._6,
      summonFrom:
        case _ : ((Nothing \^ Lg) <:< Nz) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss8[Lg, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nz <:< (G \^ Lg))](source._1)
        case _ : ((Nothing \^ Lg) <:< Ny) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss7[Lg, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Ny <:< (G \^ Lg))](source._2)
        case _ : ((Nothing \^ Lg) <:< Nx) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss6[Lg, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nx <:< (G \^ Lg))](source._3)
        case _ : ((Nothing \^ Lg) <:< Nw) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss5[Lg, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nw <:< (G \^ Lg))](source._4)
        case _ : ((Nothing \^ Lg) <:< Nv) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss4[Lg, Nu, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nv <:< (G \^ Lg))](source._5)
        case _ : ((Nothing \^ Lg) <:< Nu) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss3[Lg, Nt, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nu <:< (G \^ Lg))](source._6)
        case _ : ((Nothing \^ Lg) <:< Nt) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss2[Lg, Ns, Nr](" in update corresponding to _7")
                                             summonInline[(Nt <:< (G \^ Lg))](source._7)
        case _ : ((Nothing \^ Lg) <:< Ns) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             LabelConflicts.miss1[Lg, Nr](" in update corresponding to _7")
                                             summonInline[(Ns <:< (G \^ Lg))](source._8)
        case _ : ((Nothing \^ Lg) <:< Nr) => LabelConflicts.head3[Lg, Lh, Li]("at _7")
                                             summonInline[(Nr <:< (G \^ Lg))](source._9)
        case _ => q._7,
      summonFrom:
        case _ : ((Nothing \^ Lh) <:< Nz) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss8[Lh, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nz <:< (H \^ Lh))](source._1)
        case _ : ((Nothing \^ Lh) <:< Ny) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss7[Lh, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Ny <:< (H \^ Lh))](source._2)
        case _ : ((Nothing \^ Lh) <:< Nx) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss6[Lh, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nx <:< (H \^ Lh))](source._3)
        case _ : ((Nothing \^ Lh) <:< Nw) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss5[Lh, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nw <:< (H \^ Lh))](source._4)
        case _ : ((Nothing \^ Lh) <:< Nv) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss4[Lh, Nu, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nv <:< (H \^ Lh))](source._5)
        case _ : ((Nothing \^ Lh) <:< Nu) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss3[Lh, Nt, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nu <:< (H \^ Lh))](source._6)
        case _ : ((Nothing \^ Lh) <:< Nt) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss2[Lh, Ns, Nr](" in update corresponding to _8")
                                             summonInline[(Nt <:< (H \^ Lh))](source._7)
        case _ : ((Nothing \^ Lh) <:< Ns) => LabelConflicts.head2[Lh, Li]("at _8")
                                             LabelConflicts.miss1[Lh, Nr](" in update corresponding to _8")
                                             summonInline[(Ns <:< (H \^ Lh))](source._8)
        case _ : ((Nothing \^ Lh) <:< Nr) => LabelConflicts.head2[Lh, Li]("at _8")
                                             summonInline[(Nr <:< (H \^ Lh))](source._9)
        case _ => q._8,
      summonFrom:
        case _ : ((Nothing \^ Li) <:< Nz) => LabelConflicts.miss8[Li, Ny, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _9")
                                             summonInline[(Nz <:< (I \^ Li))](source._1)
        case _ : ((Nothing \^ Li) <:< Ny) => LabelConflicts.miss7[Li, Nx, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _9")
                                             summonInline[(Ny <:< (I \^ Li))](source._2)
        case _ : ((Nothing \^ Li) <:< Nx) => LabelConflicts.miss6[Li, Nw, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _9")
                                             summonInline[(Nx <:< (I \^ Li))](source._3)
        case _ : ((Nothing \^ Li) <:< Nw) => LabelConflicts.miss5[Li, Nv, Nu, Nt, Ns, Nr](" in update corresponding to _9")
                                             summonInline[(Nw <:< (I \^ Li))](source._4)
        case _ : ((Nothing \^ Li) <:< Nv) => LabelConflicts.miss4[Li, Nu, Nt, Ns, Nr](" in update corresponding to _9")
                                             summonInline[(Nv <:< (I \^ Li))](source._5)
        case _ : ((Nothing \^ Li) <:< Nu) => LabelConflicts.miss3[Li, Nt, Ns, Nr](" in update corresponding to _9")
                                             summonInline[(Nu <:< (I \^ Li))](source._6)
        case _ : ((Nothing \^ Li) <:< Nt) => LabelConflicts.miss2[Li, Ns, Nr](" in update corresponding to _9")
                                             summonInline[(Nt <:< (I \^ Li))](source._7)
        case _ : ((Nothing \^ Li) <:< Ns) => LabelConflicts.miss1[Li, Nr](" in update corresponding to _9")
                                             summonInline[(Ns <:< (I \^ Li))](source._8)
        case _ : ((Nothing \^ Li) <:< Nr) => summonInline[(Nr <:< (I \^ Li))](source._9)
        case _ => q._9
     )

  transparent inline def revalue[Z](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li)(to: Z):
    (Z \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, Z \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, Z \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, Z \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, Z \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, Z \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, Z \^ Li)
    =
    inline name match
      case _: La =>
        LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        q.copy(_1 = to.labelled[La])
      case _: Lb =>
        LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        q.copy(_2 = to.labelled[Lb])
      case _: Lc =>
        LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        q.copy(_3 = to.labelled[Lc])
      case _: Ld =>
        LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        q.copy(_4 = to.labelled[Ld])
      case _: Le =>
        LabelConflicts.head5[Le, Lf, Lg, Lh, Li](codeOf(name))
        q.copy(_5 = to.labelled[Le])
      case _: Lf =>
        LabelConflicts.head4[Lf, Lg, Lh, Li](codeOf(name))
        q.copy(_6 = to.labelled[Lf])
      case _: Lg =>
        LabelConflicts.head3[Lg, Lh, Li](codeOf(name))
        q.copy(_7 = to.labelled[Lg])
      case _: Lh =>
        LabelConflicts.head2[Lh, Li](codeOf(name))
        q.copy(_8 = to.labelled[Lh])
      case _: Li =>
        q.copy(_9 = to.labelled[Li])

  transparent inline def relabel[Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li)(inline lz: Lz):
    (A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lz, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lz, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lz, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Lb =>
        LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Lc =>
        LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Ld, Le, Lf, Lg, Lh, Li](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Ld =>
        LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Le, Lf, Lg, Lh, Li](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lz, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Le =>
        LabelConflicts.head5[Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Lf, Lg, Lh, Li](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lz, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Lf =>
        LabelConflicts.head4[Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Le, Lg, Lh, Li](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lz, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Lg =>
        LabelConflicts.head3[Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Le, Lf, Lh, Li](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lz, H \^ Lh, I \^ Li)]
      case _: Lh =>
        LabelConflicts.head2[Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Le, Lf, Lg, Li](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lz, I \^ Li)]
      case _: Li =>
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name) + " to " + codeOf(lz) + " creates a labelling which")
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Lz)]

  transparent inline def redo[Z, Lz <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li)(inline to: Z \^ Lz):
    (Z \^ Lz, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, Z \^ Lz, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, Z \^ Lz, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, Z \^ Lz, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, Z \^ Lz, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, Z \^ Lz, G \^ Lg, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, Z \^ Lz, H \^ Lh, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, Z \^ Lz, I \^ Li) | 
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, Z \^ Lz)
    =
    inline name match
      case _: La =>
        LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name) + " changed, creating a labelling which")
        q.copy(_1 = to)
      case _: Lb =>
        LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name) + " changed, creating a labelling which")
        q.copy(_2 = to)
      case _: Lc =>
        LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Ld, Le, Lf, Lg, Lh, Li](codeOf(name) + " changed, creating a labelling which")
        q.copy(_3 = to)
      case _: Ld =>
        LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Le, Lf, Lg, Lh, Li](codeOf(name) + " changed, creating a labelling which")
        q.copy(_4 = to)
      case _: Le =>
        LabelConflicts.head5[Le, Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Lf, Lg, Lh, Li](codeOf(name) + " changed, creating a labelling which")
        q.copy(_5 = to)
      case _: Lf =>
        LabelConflicts.head4[Lf, Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Le, Lg, Lh, Li](codeOf(name) + " changed, creating a labelling which")
        q.copy(_6 = to)
      case _: Lg =>
        LabelConflicts.head3[Lg, Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Le, Lf, Lh, Li](codeOf(name) + " changed, creating a labelling which")
        q.copy(_7 = to)
      case _: Lh =>
        LabelConflicts.head2[Lh, Li](codeOf(name))
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Le, Lf, Lg, Li](codeOf(name) + " changed, creating a labelling which")
        q.copy(_8 = to)
      case _: Li =>
        LabelConflicts.head9[Lz, La, Lb, Lc, Ld, Le, Lf, Lg, Lh](codeOf(name) + " changed, creating a labelling which")
        q.copy(_9 = to)
}

extension [A, B, C, D, E, F, G, H, I](q: (A, B, C, D, E, F, G, H, I)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li ) =
    LabelConflicts.uniq9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh, inline li: Li): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li ) =
    LabelConflicts.uniq9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](codeOf(la), codeOf(lb), codeOf(lc), codeOf(ld), codeOf(le), codeOf(lf), codeOf(lg), codeOf(lh))
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
}

