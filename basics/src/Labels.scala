// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.labels

import scala.compiletime.{codeOf, summonFrom}

import scala.annotation.targetName

import kse.basics.{LabelVal, \ => \^ }


object LabelConflicts {
  inline def diff[La, Lb](inline m1: String, inline m2: String)(inline msg: String): Unit = summonFrom:
    case _: (La =:= Lb) => compiletime.error("Label types match at " + m1 + " and " + m2 + msg)
    case _ => ()

  inline def head3[La, Lb, Lc](inline m1: String, inline m2: String, inline m3: String)(inline msg: String): Unit =
    diff[La, Lb](m1, m2)(msg)
    diff[La, Lc](m1, m3)(msg)
  inline def head4[La, Lb, Lc, Ld](inline m1: String, inline m2: String, inline m3: String, inline m4: String)(inline msg: String): Unit =
    head3[La, Lb, Lc](m1, m2, m3)(msg)
    diff[La, Ld](m1, m4)(msg)
  inline def head5[La, Lb, Lc, Ld, Le](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String)(inline msg: String): Unit =
    head4[La, Lb, Lc, Ld](m1, m2, m3, m4)(msg)
    diff[La, Le](m1, m5)(msg)
  inline def head6[La, Lb, Lc, Ld, Le, Lf](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String)(inline msg: String): Unit =
    head5[La, Lb, Lc, Ld, Le](m1, m2, m3, m4, m5)(msg)
    diff[La, Lf](m1, m6)(msg)
  inline def head7[La, Lb, Lc, Ld, Le, Lf, Lg](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String, inline m7: String)(inline msg: String): Unit =
    head6[La, Lb, Lc, Ld, Le, Lf](m1, m2, m3, m4, m5, m6)(msg)
    diff[La, Lg](m1, m7)(msg)
  inline def head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String, inline m7: String, inline m8: String)(inline msg: String): Unit =
    head7[La, Lb, Lc, Ld, Le, Lf, Lg](m1, m2, m3, m4, m5, m6, m7)(msg)
    diff[La, Lh](m1, m8)(msg)
  inline def head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String, inline m7: String, inline m8: String, inline m9: String)(inline msg: String): Unit =
    head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](m1, m2, m3, m4, m5, m6, m7, m8)(msg)
    diff[La, Li](m1, m9)(msg)

  inline def types2[La, Lb](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => diff[La, Lb]("_1", "_2")(msg)
    case _ => compiletime.error("Do not need to check 2-tuple for conflict at index " + codeOf(i))
  inline def types3[La, Lb, Lc](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head3[La, Lb, Lc]("_1", "_2", "_3")(msg)
    case 1 => diff[Lb, Lc]("_2", "_3")(msg)
    case _ => compiletime.error("Do not need to check 3-tuple for conflict at index " + codeOf(i))
  inline def types4[La, Lb, Lc, Ld](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(msg)
    case 1 => head3[Lb, Lc, Ld]("_2", "_3", "_4")(msg)
    case 2 => diff[Lc, Ld]("_3", "_4")(msg)
    case _ => compiletime.error("Do not need to check 4-tuple for conflict at index " + codeOf(i))
  inline def types5[La, Lb, Lc, Ld, Le](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(msg)
    case 1 => head4    [Lb, Lc, Ld, Le]      ("_2", "_3", "_4", "_5")(msg)
    case 2 => head3        [Lc, Ld, Le]            ("_3", "_4", "_5")(msg)
    case 3 => diff             [Ld, Le]                  ("_4", "_5")(msg)
    case _ => compiletime.error("Do not need to check 5-tuple for conflict at index " + codeOf(i))
  inline def types6[La, Lb, Lc, Ld, Le, Lf](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")(msg)
    case 1 => head5    [Lb, Lc, Ld, Le, Lf]      ("_2", "_3", "_4", "_5", "_6")(msg)
    case 2 => head4        [Lc, Ld, Le, Lf]            ("_3", "_4", "_5", "_6")(msg)
    case 3 => head3            [Ld, Le, Lf]                  ("_4", "_5", "_6")(msg)
    case 4 => diff                 [Le, Lf]                        ("_5", "_6")(msg)
    case _ => compiletime.error("Do not need to check 6-tuple for conflict at index " + codeOf(i))
  inline def types7[La, Lb, Lc, Ld, Le, Lf, Lg](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head7[La, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")(msg)
    case 1 => head6    [Lb, Lc, Ld, Le, Lf, Lg]      ("_2", "_3", "_4", "_5", "_6", "_7")(msg)
    case 2 => head5        [Lc, Ld, Le, Lf, Lg]            ("_3", "_4", "_5", "_6", "_7")(msg)
    case 3 => head4            [Ld, Le, Lf, Lg]                  ("_4", "_5", "_6", "_7")(msg)
    case 4 => head3                [Le, Lf, Lg]                        ("_5", "_6", "_7")(msg)
    case 5 => diff                     [Lf, Lg]                              ("_6", "_7")(msg)
    case _ => compiletime.error("Do not need to check 7-tuple for conflict at index " + codeOf(i))
  inline def types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")(msg)
    case 1 => head7    [Lb, Lc, Ld, Le, Lf, Lg, Lh]      ("_2", "_3", "_4", "_5", "_6", "_7", "_8")(msg)
    case 2 => head6        [Lc, Ld, Le, Lf, Lg, Lh]            ("_3", "_4", "_5", "_6", "_7", "_8")(msg)
    case 3 => head5            [Ld, Le, Lf, Lg, Lh]                  ("_4", "_5", "_6", "_7", "_8")(msg)
    case 4 => head4                [Le, Lf, Lg, Lh]                        ("_5", "_6", "_7", "_8")(msg)
    case 5 => head3                    [Lf, Lg, Lh]                              ("_6", "_7", "_8")(msg)
    case 6 => diff                         [Lg, Lh]                                    ("_7", "_8")(msg)
    case _ => compiletime.error("Do not need to check 8-tuple for conflict at index " + codeOf(i))
  inline def types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(msg)
    case 1 => head8    [Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]      ("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(msg)
    case 2 => head7        [Lc, Ld, Le, Lf, Lg, Lh, Li]            ("_3", "_4", "_5", "_6", "_7", "_8", "_9")(msg)
    case 3 => head6            [Ld, Le, Lf, Lg, Lh, Li]                  ("_4", "_5", "_6", "_7", "_8", "_9")(msg)
    case 4 => head5                [Le, Lf, Lg, Lh, Li]                        ("_5", "_6", "_7", "_8", "_9")(msg)
    case 5 => head4                    [Lf, Lg, Lh, Li]                              ("_6", "_7", "_8", "_9")(msg)
    case 6 => head3                        [Lg, Lh, Li]                                    ("_7", "_8", "_9")(msg)
    case 7 => diff                             [Lh, Li]                                          ("_8", "_9")(msg)
    case _ => compiletime.error("Do not need to check 9-tuple for conflict at index " + codeOf(i))

  inline def alter2[La, Lb](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => diff[La, Lb]("_1", "_2")(msg)
    case 1 => diff[Lb, La]("_2", "_1")(msg)
    case _ => compiletime.error("Do not need to check 2-tuple for modification at index " + codeOf(i))
  inline def alter3[La, Lb, Lc](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head3[La, Lb, Lc]("_1", "_2", "_3")(msg)
    case 1 => head3[Lb, La, Lc]("_2", "_1", "_3")(msg)
    case 2 => head3[Lc, La, Lb]("_3", "_1", "_2")(msg)
    case _ => compiletime.error("Do not need to check 3-tuple for modification at index " + codeOf(i))
  inline def alter4[La, Lb, Lc, Ld](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(msg)
    case 1 => head4[Lb, La, Lc, Ld]("_2", "_1", "_3", "_4")(msg)
    case 2 => head4[Lc, La, Lb, Ld]("_3", "_1", "_2", "_4")(msg)
    case 3 => head4[Ld, La, Lb, Lc]("_4", "_1", "_2", "_3")(msg)
    case _ => compiletime.error("Do not need to check 4-tuple for modification at index " + codeOf(i))
  inline def alter5[La, Lb, Lc, Ld, Le](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(msg)
    case 1 => head5[Lb, La, Lc, Ld, Le]("_2", "_1", "_3", "_4", "_5")(msg)
    case 2 => head5[Lc, La, Lb, Ld, Le]("_3", "_1", "_2", "_4", "_5")(msg)
    case 3 => head5[Ld, La, Lb, Lc, Le]("_4", "_1", "_2", "_3", "_5")(msg)
    case 4 => head5[Le, La, Lb, Lc, Ld]("_5", "_1", "_2", "_3", "_4")(msg)
    case _ => compiletime.error("Do not need to check 5-tuple for modification at index " + codeOf(i))
  inline def alter6[La, Lb, Lc, Ld, Le, Lf](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")(msg)
    case 1 => head6[Lb, La, Lc, Ld, Le, Lf]("_2", "_1", "_3", "_4", "_5", "_6")(msg)
    case 2 => head6[Lc, La, Lb, Ld, Le, Lf]("_3", "_1", "_2", "_4", "_5", "_6")(msg)
    case 3 => head6[Ld, La, Lb, Lc, Le, Lf]("_4", "_1", "_2", "_3", "_5", "_6")(msg)
    case 4 => head6[Le, La, Lb, Lc, Ld, Lf]("_5", "_1", "_2", "_3", "_4", "_6")(msg)
    case 5 => head6[Lf, La, Lb, Lc, Ld, Le]("_6", "_1", "_2", "_3", "_4", "_5")(msg)
    case _ => compiletime.error("Do not need to check 6-tuple for modification at index " + codeOf(i))
  inline def alter7[La, Lb, Lc, Ld, Le, Lf, Lg](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head7[La, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")(msg)
    case 1 => head7[Lb, La, Lc, Ld, Le, Lf, Lg]("_2", "_1", "_3", "_4", "_5", "_6", "_7")(msg)
    case 2 => head7[Lc, La, Lb, Ld, Le, Lf, Lg]("_3", "_1", "_2", "_4", "_5", "_6", "_7")(msg)
    case 3 => head7[Ld, La, Lb, Lc, Le, Lf, Lg]("_4", "_1", "_2", "_3", "_5", "_6", "_7")(msg)
    case 4 => head7[Le, La, Lb, Lc, Ld, Lf, Lg]("_5", "_1", "_2", "_3", "_4", "_6", "_7")(msg)
    case 5 => head7[Lf, La, Lb, Lc, Ld, Le, Lg]("_6", "_1", "_2", "_3", "_4", "_5", "_7")(msg)
    case 6 => head7[Lg, La, Lb, Lc, Ld, Le, Lf]("_7", "_1", "_2", "_3", "_4", "_5", "_6")(msg)
    case _ => compiletime.error("Do not need to check 7-tuple for modification at index " + codeOf(i))
  inline def alter8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")(msg)
    case 1 => head8[Lb, La, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_1", "_3", "_4", "_5", "_6", "_7", "_8")(msg)
    case 2 => head8[Lc, La, Lb, Ld, Le, Lf, Lg, Lh]("_3", "_1", "_2", "_4", "_5", "_6", "_7", "_8")(msg)
    case 3 => head8[Ld, La, Lb, Lc, Le, Lf, Lg, Lh]("_4", "_1", "_2", "_3", "_5", "_6", "_7", "_8")(msg)
    case 4 => head8[Le, La, Lb, Lc, Ld, Lf, Lg, Lh]("_5", "_1", "_2", "_3", "_4", "_6", "_7", "_8")(msg)
    case 5 => head8[Lf, La, Lb, Lc, Ld, Le, Lg, Lh]("_6", "_1", "_2", "_3", "_4", "_5", "_7", "_8")(msg)
    case 6 => head8[Lg, La, Lb, Lc, Ld, Le, Lf, Lh]("_7", "_1", "_2", "_3", "_4", "_5", "_6", "_8")(msg)
    case 7 => head8[Lh, La, Lb, Lc, Ld, Le, Lf, Lg]("_8", "_1", "_2", "_3", "_4", "_5", "_6", "_7")(msg)
    case _ => compiletime.error("Do not need to check 8-tuple for modification at index " + codeOf(i))
  inline def alter9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](inline i: Int)(inline msg: String = ""): Unit = inline i match
    case 0 => head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(msg)
    case 1 => head9[Lb, La, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_1", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(msg)
    case 2 => head9[Lc, La, Lb, Ld, Le, Lf, Lg, Lh, Li]("_3", "_1", "_2", "_4", "_5", "_6", "_7", "_8", "_9")(msg)
    case 3 => head9[Ld, La, Lb, Lc, Le, Lf, Lg, Lh, Li]("_4", "_1", "_2", "_3", "_5", "_6", "_7", "_8", "_9")(msg)
    case 4 => head9[Le, La, Lb, Lc, Ld, Lf, Lg, Lh, Li]("_5", "_1", "_2", "_3", "_4", "_6", "_7", "_8", "_9")(msg)
    case 5 => head9[Lf, La, Lb, Lc, Ld, Le, Lg, Lh, Li]("_6", "_1", "_2", "_3", "_4", "_5", "_7", "_8", "_9")(msg)
    case 6 => head9[Lg, La, Lb, Lc, Ld, Le, Lf, Lh, Li]("_7", "_1", "_2", "_3", "_4", "_5", "_6", "_8", "_9")(msg)
    case 7 => head9[Lh, La, Lb, Lc, Ld, Le, Lf, Lg, Li]("_8", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_9")(msg)
    case 8 => head9[Li, La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_9", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")(msg)
    case _ => compiletime.error("Do not need to check 9-tuple for modification at index " + codeOf(i))

  inline def uniq2[La, Lb](inline msg: String = ""): Unit =
    diff[La, Lb]("_1", "_2")(msg)
  inline def uniq3[La, Lb, Lc](inline msg: String = ""): Unit =
    uniq2[La, Lb](msg)
    head3[Lc, La, Lb]("_3", "_1", "_2")(msg)
  inline def uniq4[La, Lb, Lc, Ld](inline msg: String = ""): Unit =
    uniq3[La, Lb, Lc](msg)
    head4[Ld, La, Lb, Lc]("_4", "_1", "_2", "_3")(msg)
  inline def uniq5[La, Lb, Lc, Ld, Le](inline msg: String = ""): Unit =
    uniq4[La, Lb, Lc, Ld](msg)
    head5[Le, La, Lb, Lc, Ld]("_5", "_1", "_2", "_3", "_4")(msg)
  inline def uniq6[La, Lb, Lc, Ld, Le, Lf](inline msg: String = ""): Unit =
    uniq5[La, Lb, Lc, Ld, Le](msg)
    head6[Lf, La, Lb, Lc, Ld, Le]("_6", "_1", "_2", "_3", "_4", "_5")(msg)
  inline def uniq7[La, Lb, Lc, Ld, Le, Lf, Lg](inline msg: String = ""): Unit =
    uniq6[La, Lb, Lc, Ld, Le, Lf](msg)
    head7[Lg, La, Lb, Lc, Ld, Le, Lf]("_7", "_1", "_2", "_3", "_4", "_5", "_6")(msg)
  inline def uniq8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](inline msg: String = ""): Unit =
    uniq7[La, Lb, Lc, Ld, Le, Lf, Lg](msg)
    head8[Lh, La, Lb, Lc, Ld, Le, Lf, Lg]("_8", "_1", "_2", "_3", "_4", "_5", "_6", "_7")(msg)
  inline def uniq9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](inline msg: String = ""): Unit =
    uniq8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](msg)
    head9[Li, La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_9", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")(msg)


  inline def unik2[La, Lb](inline la: La, inline lb: Lb): Unit =
    diff[La, Lb]("_1", "_2")(" with " + codeOf(lb))
  inline def unik3[La, Lb, Lc](inline la: La, inline lb: Lb, inline lc: Lc): Unit =
    unik2[La, Lb](la, lb)
    head3[Lc, La, Lb]("_3", "_1", "_2")(" with " + codeOf(lc))
  inline def unik4[La, Lb, Lc, Ld](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld): Unit =
    unik3[La, Lb, Lc](la, lb, lc)
    head4[Ld, La, Lb, Lc]("_4", "_1", "_2", "_3")(" with " + codeOf(ld))
  inline def unik5[La, Lb, Lc, Ld, Le](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le): Unit =
    unik4[La, Lb, Lc, Ld](la, lb, lc, ld)
    head5[Le, La, Lb, Lc, Ld]("_5", "_1", "_2", "_3", "_4")(" with " + codeOf(le))
  inline def unik6[La, Lb, Lc, Ld, Le, Lf](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf): Unit =
    unik5[La, Lb, Lc, Ld, Le](la, lb, lc, ld, le)
    head6[Lf, La, Lb, Lc, Ld, Le]("_6", "_1", "_2", "_3", "_4", "_5")(" with " + codeOf(lf))
  inline def unik7[La, Lb, Lc, Ld, Le, Lf, Lg](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg): Unit =
    unik6[La, Lb, Lc, Ld, Le, Lf](la, lb, lc, ld, le, lf)
    head7[Lg, La, Lb, Lc, Ld, Le, Lf]("_7", "_1", "_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lg))
  inline def unik8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh): Unit =
    unik7[La, Lb, Lc, Ld, Le, Lf, Lg](la, lb, lc, ld, le, lf, lg)
    head8[Lh, La, Lb, Lc, Ld, Le, Lf, Lg]("_8", "_1", "_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lh))
  inline def unik9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh, inline li: Li): Unit =
    unik8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](la, lb, lc, ld, le, lf, lg, lh)
    head9[Li, La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_9", "_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(li))
}


extension [A](a: A) {
  /** Associate a compile-time name with this value */
  inline def labelled[L <: LabelVal]: (A \^ L) = \^(a)

  /** Associate a compile-time name with this value by giving the other (Singular) value */
  inline def \[L <: LabelVal](l: L): A \^ L = \^(a)

  /** Associate a compile-time name with this value by taking from another labeled value */
  inline def labelLike[B, L <: LabelVal](bl: B \^ L): A \^ L = \^(a)
}


extension [A, La <: LabelVal, B, Lb <: LabelVal](q: (A \^ La, B \^ Lb)) {
  transparent inline def ~(inline name: La | Lb): A | B = inline name match
      case _: La =>
        LabelConflicts.types2[La, Lb](0)(" with " + codeOf(name))
        q._1.unlabel
      case _: Lb =>
        q._2.unlabel

  inline def unlabel: (A, B) = q.asInstanceOf[(A, B)]

  transparent inline def revalue[X](inline name: La | Lb)(to: X): (X \^ La, B \^ Lb) | (A \^ La, X \^ Lb) = inline name match
    case _: La =>
      LabelConflicts.types2[La, Lb](0)(" with " + codeOf(name))
      (to.labelled[La], q._2)
    case _: Lb =>
      (q._1, to.labelled[Lb])

  transparent inline def relabel[Lx <: LabelVal](inline name: La | Lb)(inline to: Lx): (A \^ Lx, B \^ Lb) | (A \^ La, B \^ Lx) = inline name match
    case _: La =>
      LabelConflicts.types2[La, Lb](0)(" with " + codeOf(name))
      LabelConflicts.alter2[Lx, Lb](0)(" for new label " + codeOf(to))
      q.asInstanceOf[(A \^ Lx, B \^ Lb)]
    case _: Lb =>
      LabelConflicts.alter2[La, Lx](1)(" for new label " + codeOf(to))
      q.asInstanceOf[(A \^ La, B \^ Lx)]

  transparent inline def redo[X, Lx <: LabelVal](inline name: La | Lb)(to: X \^ Lx): (X \^ Lx, B \^ Lb) | (A \^ La, X \^ Lx) = inline name match
    case _: La =>
      LabelConflicts.types2[La, Lb](0)(" with " + codeOf(name))
      LabelConflicts.alter2[Lx, Lb](0)(" for new label")
      (to, q._2)
    case _: Lb =>
      LabelConflicts.alter2[La, Lx](1)(" for new label")
      (q._1, to)
}

extension [A, B](q: (A, B)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal]: (A \^ La, B \^ Lb) =
    LabelConflicts.uniq2[La, Lb]()
    q.asInstanceOf[(A \^ La, B \^ Lb)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal](inline la: La, inline lb: Lb): (A \^ La, B \^ Lb) =
    LabelConflicts.unik2(la, lb)
    q.asInstanceOf[(A \^ La, B \^ Lb)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc)) {
  transparent inline def ~(inline name: La | Lb | Lc): A | B | C = inline name match
      case _: La =>
        LabelConflicts.types3[La, Lb, Lc](0)(" with " + codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.types3[La, Lb, Lc](1)(" with " + codeOf(name))
        q._2.unlabel
      case _: Lc =>
        q._3.unlabel

  inline def unlabel: (A, B, C) = q.asInstanceOf[(A, B, C)]

  transparent inline def revalue[X](inline name: La | Lb | Lc)(to: X): (X \^ La, B \^ Lb, C \^ Lc) | (A \^ La, X \^ Lb, C \^ Lc) | (A \^ La, B \^ Lb, X \^ Lc) = inline name match
      case _: La =>
        LabelConflicts.types3[La, Lb, Lc](0)(" with " + codeOf(name))
        (to.labelled[La], q._2, q._3)
      case _: Lb =>
        LabelConflicts.types3[La, Lb, Lc](1)(" with " + codeOf(name))
        (q._1, to.labelled[Lb], q._3)
      case _: Lc =>
        (q._1, q._2, to.labelled[Lc])

  transparent inline def relabel[Lx <: LabelVal](inline name: La | Lb | Lc)(inline to: Lx): (A \^ Lx, B \^ Lb, C \^ Lc) | (A \^ La, B \^ Lx, C \^ Lc) | (A \^ La, B \^ Lb, C \^ Lx) = inline name match
    case _: La =>
      LabelConflicts.types3[La, Lb, Lc](0)(" with " + codeOf(name))
      LabelConflicts.alter3[Lx, Lb, Lc](0)(" for new label " + codeOf(to))
      q.asInstanceOf[(A \^ Lx, B \^ Lb, C \^ Lc)]
    case _: Lb =>
      LabelConflicts.types3[La, Lb, Lc](1)(" with " + codeOf(name))
      LabelConflicts.alter3[La, Lx, Lc](1)(" for new label " + codeOf(to))
      q.asInstanceOf[(A \^ La, B \^ Lx, C \^ Lc)]
    case _: Lc =>
      LabelConflicts.alter3[La, Lb, Lx](2)(" for new label " + codeOf(to))
      q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lx)]

  transparent inline def redo[X, Lx <: LabelVal](inline name: La | Lb | Lc)(to: X \^ Lx): (X \^ Lx, B \^ Lb, C \^ Lc) | (A \^ La, X \^ Lx, C \^ Lc) | (A \^ La, B \^ Lb, X \^ Lx) = inline name match
    case _: La =>
      LabelConflicts.types3[La, Lb, Lc](0)(" with " + codeOf(name))
      LabelConflicts.alter3[Lx, Lb, Lc](0)(" for new label")
      (to, q._2, q._3)
    case _: Lb =>
      LabelConflicts.types3[La, Lb, Lc](1)(" with " + codeOf(name))
      LabelConflicts.alter3[La, Lx, Lc](1)(" for new label")
      (q._1, to, q._3)
    case _: Lc =>
      LabelConflicts.alter3[La, Lb, Lx](2)(" for new label")
      (q._1, q._2, to)
}

extension [A, B, C](q: (A, B, C)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc) =
    LabelConflicts.uniq3[La, Lb, Lc]()
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc): (A \^ La, B \^ Lb, C \^ Lc) =
    LabelConflicts.unik3(la, lb, lc)
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld): A | B | C | D = inline name match
      case _: La =>
        LabelConflicts.types4[La, Lb, Lc, Ld](0)(" with " + codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.types4[La, Lb, Lc, Ld](1)(" with " + codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.types4[La, Lb, Lc, Ld](2)(" with " + codeOf(name))
        q._3.unlabel
      case _: Ld =>
        q._4.unlabel

  inline def unlabel: (A, B, C, D) = q.asInstanceOf[(A, B, C, D)]

  transparent inline def revalue[X](inline name: La | Lb | Lc | Ld)(to: X):
    (X \^ La, B \^ Lb, C \^ Lc, D \^ Ld) |
    (A \^ La, X \^ Lb, C \^ Lc, D \^ Ld) |
    (A \^ La, B \^ Lb, X \^ Lc, D \^ Ld) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Ld)
    =
    inline name match
      case _: La =>
        LabelConflicts.types4[La, Lb, Lc, Ld](0)(" with " + codeOf(name))
        (to.labelled[La], q._2, q._3, q._4)
      case _: Lb =>
        LabelConflicts.types4[La, Lb, Lc, Ld](1)(" with " + codeOf(name))
        (q._1, to.labelled[Lb], q._3, q._4)
      case _: Lc =>
        LabelConflicts.types4[La, Lb, Lc, Ld](2)(" with " + codeOf(name))
        (q._1, q._2, to.labelled[Lc], q._4)
      case _: Ld =>
        (q._1, q._2, q._3, to.labelled[Ld])

  transparent inline def relabel[Lx <: LabelVal](inline name: La | Lb | Lc | Ld)(inline to: Lx):
    (A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld) |
    (A \^ La, B \^ Lx, C \^ Lc, D \^ Ld) |
    (A \^ La, B \^ Lb, C \^ Lx, D \^ Ld) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types4[La, Lb, Lc, Ld](0)(" with " + codeOf(name))
        LabelConflicts.alter4[Lx, Lb, Lc, Ld](0)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld)]
      case _: Lb =>
        LabelConflicts.types4[La, Lb, Lc, Ld](1)(" with " + codeOf(name))
        LabelConflicts.alter4[La, Lx, Lc, Ld](1)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lx, C \^ Lc, D \^ Ld)]
      case _: Lc =>
        LabelConflicts.types4[La, Lb, Lc, Ld](2)(" with " + codeOf(name))
        LabelConflicts.alter4[La, Lb, Lx, Ld](2)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lx, D \^ Ld)]
      case _: Ld =>
        LabelConflicts.alter4[La, Lb, Lc, Lx](3)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lx)]

  transparent inline def redo[X, Lx <: LabelVal](inline name: La | Lb | Lc | Ld)(to: X \^ Lx):
    (X \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld) |
    (A \^ La, X \^ Lx, C \^ Lc, D \^ Ld) |
    (A \^ La, B \^ Lb, X \^ Lx, D \^ Ld) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types4[La, Lb, Lc, Ld](0)(" with " + codeOf(name))
        LabelConflicts.alter4[Lx, Lb, Lc, Ld](0)(" for new label")
        (to, q._2, q._3, q._4)
      case _: Lb =>
        LabelConflicts.types4[La, Lb, Lc, Ld](1)(" with " + codeOf(name))
        LabelConflicts.alter4[La, Lx, Lc, Ld](1)(" for new label")
        (q._1, to, q._3, q._4)
      case _: Lc =>
        LabelConflicts.types4[La, Lb, Lc, Ld](2)(" with " + codeOf(name))
        LabelConflicts.alter4[La, Lb, Lx, Ld](2)(" for new label")
        (q._1, q._2, to, q._4)
      case _: Ld =>
        LabelConflicts.alter4[La, Lb, Lc, Lx](3)(" for new label")
        (q._1, q._2, q._3, to)
}

extension [A, B, C, D](q: (A, B, C, D)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    LabelConflicts.uniq4[La, Lb, Lc, Ld]()
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld) =
    LabelConflicts.unik4(la, lb, lc, ld)
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld)]
}



extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le): A | B | C | D | E = inline name match
      case _: La =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](0)(" with " + codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](1)(" with " + codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](2)(" with " + codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](3)(" with " + codeOf(name))
        q._4.unlabel
      case _: Le =>
        q._5.unlabel

  inline def unlabel: (A, B, C, D, E) = q.asInstanceOf[(A, B, C, D, E)]

  transparent inline def revalue[X](inline name: La | Lb | Lc | Ld | Le)(to: X):
    (X \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) |
    (A \^ La, X \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) |
    (A \^ La, B \^ Lb, X \^ Lc, D \^ Ld, E \^ Le) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Ld, E \^ Le) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Le)
    =
    inline name match
      case _: La =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](0)(" with " + codeOf(name))
        (to.labelled[La], q._2, q._3, q._4, q._5)
      case _: Lb =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](1)(" with " + codeOf(name))
        (q._1, to.labelled[Lb], q._3, q._4, q._5)
      case _: Lc =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](2)(" with " + codeOf(name))
        (q._1, q._2, to.labelled[Lc], q._4, q._5)
      case _: Ld =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](3)(" with " + codeOf(name))
        (q._1, q._2, q._3, to.labelled[Ld], q._5)
      case _: Le =>
        (q._1, q._2, q._3, q._4, to.labelled[Le])

  transparent inline def relabel[Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le)(inline to: Lx):
    (A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) |
    (A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le) |
    (A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](0)(" with " + codeOf(name))
        LabelConflicts.alter5[Lx, Lb, Lc, Ld, Le](0)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le)]
      case _: Lb =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](1)(" with " + codeOf(name))
        LabelConflicts.alter5[La, Lx, Lc, Ld, Le](1)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le)]
      case _: Lc =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](2)(" with " + codeOf(name))
        LabelConflicts.alter5[La, Lb, Lx, Ld, Le](2)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le)]
      case _: Ld =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](3)(" with " + codeOf(name))
        LabelConflicts.alter5[La, Lb, Lc, Lx, Le](3)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le)]
      case _: Le =>
        LabelConflicts.alter5[La, Lb, Lc, Ld, Lx](4)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx)]

  transparent inline def redo[X, Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le)(to: X \^ Lx):
    (X \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) |
    (A \^ La, X \^ Lx, C \^ Lc, D \^ Ld, E \^ Le) |
    (A \^ La, B \^ Lb, X \^ Lx, D \^ Ld, E \^ Le) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Lx, E \^ Le) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](0)(" with " + codeOf(name))
        LabelConflicts.alter5[Lx, Lb, Lc, Ld, Le](0)(" for new label")
        (to, q._2, q._3, q._4, q._5)
      case _: Lb =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](1)(" with " + codeOf(name))
        LabelConflicts.alter5[La, Lx, Lc, Ld, Le](1)(" for new label")
        (q._1, to, q._3, q._4, q._5)
      case _: Lc =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](2)(" with " + codeOf(name))
        LabelConflicts.alter5[La, Lb, Lx, Ld, Le](2)(" for new label")
        (q._1, q._2, to, q._4, q._5)
      case _: Ld =>
        LabelConflicts.types5[La, Lb, Lc, Ld, Le](3)(" with " + codeOf(name))
        LabelConflicts.alter5[La, Lb, Lc, Lx, Le](3)(" for new label")
        (q._1, q._2, q._3, to, q._5)
      case _: Le =>
        LabelConflicts.alter5[La, Lb, Lc, Ld, Lx](4)(" for new label")
        (q._1, q._2, q._3, q._4, to)
}

extension [A, B, C, D, E](q: (A, B, C, D, E)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    LabelConflicts.uniq5[La, Lb, Lc, Ld, Le]()
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le) =
    LabelConflicts.unik5(la, lb, lc, ld, le)
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal, F, Lf <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le | Lf): A | B | C | D | E | F = inline name match
      case _: La =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](0)(" with " + codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](1)(" with " + codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](2)(" with " + codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](3)(" with " + codeOf(name))
        q._4.unlabel
      case _: Le =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](4)(" with " + codeOf(name))
        q._5.unlabel
      case _: Lf =>
        q._6.unlabel

  inline def unlabel: (A, B, C, D, E, F) = q.asInstanceOf[(A, B, C, D, E, F)]

  transparent inline def revalue[X](inline name: La | Lb | Lc | Ld | Le | Lf)(to: X):
    (X \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, X \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, X \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, X \^ Lf)
    =
    inline name match
      case _: La =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](0)(" with " + codeOf(name))
        (to.labelled[La], q._2, q._3, q._4, q._5, q._6)
      case _: Lb =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](1)(" with " + codeOf(name))
        (q._1, to.labelled[Lb], q._3, q._4, q._5, q._6)
      case _: Lc =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](2)(" with " + codeOf(name))
        (q._1, q._2, to.labelled[Lc], q._4, q._5, q._6)
      case _: Ld =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](3)(" with " + codeOf(name))
        (q._1, q._2, q._3, to.labelled[Ld], q._5, q._6)
      case _: Le =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](4)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, to.labelled[Le], q._6)
      case _: Lf =>
        (q._1, q._2, q._3, q._4, q._5, to.labelled[Lf])

  transparent inline def relabel[Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf)(inline to: Lx):
    (A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](0)(" with " + codeOf(name))
        LabelConflicts.alter6[Lx, Lb, Lc, Ld, Le, Lf](0)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)]
      case _: Lb =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](1)(" with " + codeOf(name))
        LabelConflicts.alter6[La, Lx, Lc, Ld, Le, Lf](1)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)]
      case _: Lc =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](2)(" with " + codeOf(name))
        LabelConflicts.alter6[La, Lb, Lx, Ld, Le, Lf](2)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le, F \^ Lf)]
      case _: Ld =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](3)(" with " + codeOf(name))
        LabelConflicts.alter6[La, Lb, Lc, Lx, Le, Lf](3)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le, F \^ Lf)]
      case _: Le =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](4)(" with " + codeOf(name))
        LabelConflicts.alter6[La, Lb, Lc, Ld, Lx, Lf](4)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx, F \^ Lf)]
      case _: Lf =>
        LabelConflicts.alter6[La, Lb, Lc, Ld, Le, Lx](5)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lx)]

  transparent inline def redo[X, Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf)(to: X \^ Lx):
    (X \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, X \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, X \^ Lx, D \^ Ld, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Lx, E \^ Le, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Lx, F \^ Lf) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, X \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](0)(" with " + codeOf(name))
        LabelConflicts.alter6[Lx, Lb, Lc, Ld, Le, Lf](0)(" for new label")
        (to, q._2, q._3, q._4, q._5, q._6)
      case _: Lb =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](1)(" with " + codeOf(name))
        LabelConflicts.alter6[La, Lx, Lc, Ld, Le, Lf](1)(" for new label")
        (q._1, to, q._3, q._4, q._5, q._6)
      case _: Lc =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](2)(" with " + codeOf(name))
        LabelConflicts.alter6[La, Lb, Lx, Ld, Le, Lf](2)(" for new label")
        (q._1, q._2, to, q._4, q._5, q._6)
      case _: Ld =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](3)(" with " + codeOf(name))
        LabelConflicts.alter6[La, Lb, Lc, Lx, Le, Lf](3)(" for new label")
        (q._1, q._2, q._3, to, q._5, q._6)
      case _: Le =>
        LabelConflicts.types6[La, Lb, Lc, Ld, Le, Lf](4)(" with " + codeOf(name))
        LabelConflicts.alter6[La, Lb, Lc, Ld, Lx, Lf](4)(" for new label")
        (q._1, q._2, q._3, q._4, to, q._6)
      case _: Lf =>
        LabelConflicts.alter6[La, Lb, Lc, Ld, Le, Lx](5)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, to)
}

extension [A, B, C, D, E, F](q: (A, B, C, D, E, F)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    LabelConflicts.uniq6[La, Lb, Lc, Ld, Le, Lf]()
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf) =
    LabelConflicts.unik6(la, lb, lc, ld, le, lf)
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal, F, Lf <: LabelVal, G, Lg <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le | Lf | Lg): A | B | C | D | E | F | G = inline name match
      case _: La =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](0)(" with " + codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](1)(" with " + codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](2)(" with " + codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](3)(" with " + codeOf(name))
        q._4.unlabel
      case _: Le =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](4)(" with " + codeOf(name))
        q._5.unlabel
      case _: Lf =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](5)(" with " + codeOf(name))
        q._6.unlabel
      case _: Lg =>
        q._7.unlabel

  inline def unlabel: (A, B, C, D, E, F, G) = q.asInstanceOf[(A, B, C, D, E, F, G)]

  transparent inline def revalue[X](inline name: La | Lb | Lc | Ld | Le | Lf | Lg)(to: X):
    (X \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, X \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, X \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, X \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, X \^ Lg)
    =
    inline name match
      case _: La =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](0)(" with " + codeOf(name))
        (to.labelled[La], q._2, q._3, q._4, q._5, q._6, q._7)
      case _: Lb =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](1)(" with " + codeOf(name))
        (q._1, to.labelled[Lb], q._3, q._4, q._5, q._6, q._7)
      case _: Lc =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](2)(" with " + codeOf(name))
        (q._1, q._2, to.labelled[Lc], q._4, q._5, q._6, q._7)
      case _: Ld =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](3)(" with " + codeOf(name))
        (q._1, q._2, q._3, to.labelled[Ld], q._5, q._6, q._7)
      case _: Le =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](4)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, to.labelled[Le], q._6, q._7)
      case _: Lf =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](5)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, q._5, to.labelled[Lf], q._7)
      case _: Lg =>
        (q._1, q._2, q._3, q._4, q._5, q._6, to.labelled[Lg])

  transparent inline def relabel[Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg)(inline to: Lx):
    (A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lx, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](0)(" with " + codeOf(name))
        LabelConflicts.alter7[Lx, Lb, Lc, Ld, Le, Lf, Lg](0)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]
      case _: Lb =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](1)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lx, Lc, Ld, Le, Lf, Lg](1)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]
      case _: Lc =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](2)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lb, Lx, Ld, Le, Lf, Lg](2)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]
      case _: Ld =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](3)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lb, Lc, Lx, Le, Lf, Lg](3)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le, F \^ Lf, G \^ Lg)]
      case _: Le =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](4)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lb, Lc, Ld, Lx, Lf, Lg](4)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx, F \^ Lf, G \^ Lg)]
      case _: Lf =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](5)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lb, Lc, Ld, Le, Lx, Lg](5)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lx, G \^ Lg)]
      case _: Lg =>
        LabelConflicts.alter7[La, Lb, Lc, Ld, Le, Lf, Lx](6)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lx)]

  transparent inline def redo[X, Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg)(to: X \^ Lx):
    (X \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, X \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, X \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Lx, E \^ Le, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Lx, F \^ Lf, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, X \^ Lx, G \^ Lg) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, X \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](0)(" with " + codeOf(name))
        LabelConflicts.alter7[Lx, Lb, Lc, Ld, Le, Lf, Lg](0)(" for new label")
        (to, q._2, q._3, q._4, q._5, q._6, q._7)
      case _: Lb =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](1)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lx, Lc, Ld, Le, Lf, Lg](1)(" for new label")
        (q._1, to, q._3, q._4, q._5, q._6, q._7)
      case _: Lc =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](2)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lb, Lx, Ld, Le, Lf, Lg](2)(" for new label")
        (q._1, q._2, to, q._4, q._5, q._6, q._7)
      case _: Ld =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](3)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lb, Lc, Lx, Le, Lf, Lg](3)(" for new label")
        (q._1, q._2, q._3, to, q._5, q._6, q._7)
      case _: Le =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](4)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lb, Lc, Ld, Lx, Lf, Lg](4)(" for new label")
        (q._1, q._2, q._3, q._4, to, q._6, q._7)
      case _: Lf =>
        LabelConflicts.types7[La, Lb, Lc, Ld, Le, Lf, Lg](5)(" with " + codeOf(name))
        LabelConflicts.alter7[La, Lb, Lc, Ld, Le, Lx, Lg](5)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, to, q._7)
      case _: Lg =>
        LabelConflicts.alter7[La, Lb, Lc, Ld, Le, Lf, Lx](6)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, q._6, to)
}

extension [A, B, C, D, E, F, G](q: (A, B, C, D, E, F, G)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    LabelConflicts.uniq7[La, Lb, Lc, Ld, Le, Lf, Lg]()
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg) =
    LabelConflicts.unik7(la, lb, lc, ld, le, lf, lg)
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal, F, Lf <: LabelVal, G, Lg <: LabelVal, H, Lh <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh): A | B | C | D | E | F | G | H = inline name match
      case _: La =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](0)(" with " + codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](1)(" with " + codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](2)(" with " + codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](3)(" with " + codeOf(name))
        q._4.unlabel
      case _: Le =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](4)(" with " + codeOf(name))
        q._5.unlabel
      case _: Lf =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](5)(" with " + codeOf(name))
        q._6.unlabel
      case _: Lg =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](6)(" with " + codeOf(name))
        q._7.unlabel
      case _: Lh =>
        q._8.unlabel

  inline def unlabel: (A, B, C, D, E, F, G, H) = q.asInstanceOf[(A, B, C, D, E, F, G, H)]

  transparent inline def revalue[X](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh)(to: X):
    (X \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, X \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, X \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, X \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, X \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, X \^ Lh)
    =
    inline name match
      case _: La =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](0)(" with " + codeOf(name))
        (to.labelled[La], q._2, q._3, q._4, q._5, q._6, q._7, q._8)
      case _: Lb =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](1)(" with " + codeOf(name))
        (q._1, to.labelled[Lb], q._3, q._4, q._5, q._6, q._7, q._8)
      case _: Lc =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](2)(" with " + codeOf(name))
        (q._1, q._2, to.labelled[Lc], q._4, q._5, q._6, q._7, q._8)
      case _: Ld =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](3)(" with " + codeOf(name))
        (q._1, q._2, q._3, to.labelled[Ld], q._5, q._6, q._7, q._8)
      case _: Le =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](4)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, to.labelled[Le], q._6, q._7, q._8)
      case _: Lf =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](5)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, q._5, to.labelled[Lf], q._7, q._8)
      case _: Lg =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](6)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, q._5, q._6, to.labelled[Lg], q._8)
      case _: Lh =>
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, to.labelled[Lh])

  transparent inline def relabel[Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh)(inline to: Lx):
    (A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lx, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lx, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](0)(" with " + codeOf(name))
        LabelConflicts.alter8[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh](0)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Lb =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](1)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lx, Lc, Ld, Le, Lf, Lg, Lh](1)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Lc =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](2)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lx, Ld, Le, Lf, Lg, Lh](2)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Ld =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](3)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lc, Lx, Le, Lf, Lg, Lh](3)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Le =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](4)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lc, Ld, Lx, Lf, Lg, Lh](4)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx, F \^ Lf, G \^ Lg, H \^ Lh)]
      case _: Lf =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](5)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lc, Ld, Le, Lx, Lg, Lh](5)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lx, G \^ Lg, H \^ Lh)]
      case _: Lg =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](6)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lc, Ld, Le, Lf, Lx, Lh](6)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lx, H \^ Lh)]
      case _: Lh =>
        LabelConflicts.alter8[La, Lb, Lc, Ld, Le, Lf, Lg, Lx](7)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lx)]

  transparent inline def redo[X, Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh)(to: X \^ Lx):
    (X \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, X \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, X \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Lx, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Lx, F \^ Lf, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, X \^ Lx, G \^ Lg, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, X \^ Lx, H \^ Lh) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, X \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](0)(" with " + codeOf(name))
        LabelConflicts.alter8[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh](0)(" for new label")
        (to, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
      case _: Lb =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](1)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lx, Lc, Ld, Le, Lf, Lg, Lh](1)(" for new label")
        (q._1, to, q._3, q._4, q._5, q._6, q._7, q._8)
      case _: Lc =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](2)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lx, Ld, Le, Lf, Lg, Lh](2)(" for new label")
        (q._1, q._2, to, q._4, q._5, q._6, q._7, q._8)
      case _: Ld =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](3)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lc, Lx, Le, Lf, Lg, Lh](3)(" for new label")
        (q._1, q._2, q._3, to, q._5, q._6, q._7, q._8)
      case _: Le =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](4)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lc, Ld, Lx, Lf, Lg, Lh](4)(" for new label")
        (q._1, q._2, q._3, q._4, to, q._6, q._7, q._8)
      case _: Lf =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](5)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lc, Ld, Le, Lx, Lg, Lh](5)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, to, q._7, q._8)
      case _: Lg =>
        LabelConflicts.types8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh](6)(" with " + codeOf(name))
        LabelConflicts.alter8[La, Lb, Lc, Ld, Le, Lf, Lx, Lh](6)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, q._6, to, q._8)
      case _: Lh =>
        LabelConflicts.alter8[La, Lb, Lc, Ld, Le, Lf, Lg, Lx](7)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, to)
}

extension [A, B, C, D, E, F, G, H](q: (A, B, C, D, E, F, G, H)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    LabelConflicts.uniq8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]()
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh) =
    LabelConflicts.unik8(la, lb, lc, ld, le, lf, lg, lh)
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh)]
}


extension [A, La <: LabelVal, B, Lb <: LabelVal, C, Lc <: LabelVal, D, Ld <: LabelVal, E, Le <: LabelVal, F, Lf <: LabelVal, G, Lg <: LabelVal, H, Lh <: LabelVal, I, Li <: LabelVal](q: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)) {
  transparent inline def ~(inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li): A | B | C | D | E | F | G | H | I = inline name match
      case _: La =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](0)(" with " + codeOf(name))
        q._1.unlabel
      case _: Lb =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](1)(" with " + codeOf(name))
        q._2.unlabel
      case _: Lc =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](2)(" with " + codeOf(name))
        q._3.unlabel
      case _: Ld =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](3)(" with " + codeOf(name))
        q._4.unlabel
      case _: Le =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](4)(" with " + codeOf(name))
        q._5.unlabel
      case _: Lf =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](5)(" with " + codeOf(name))
        q._6.unlabel
      case _: Lg =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](6)(" with " + codeOf(name))
        q._7.unlabel
      case _: Lh =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](7)(" with " + codeOf(name))
        q._8.unlabel
      case _: Li =>
        q._9.unlabel

  inline def unlabel: (A, B, C, D, E, F, G, H, I) = q.asInstanceOf[(A, B, C, D, E, F, G, H, I)]

  transparent inline def revalue[X](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li)(to: X):
    (X \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, X \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, X \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, X \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, X \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, X \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, X \^ Li)
    =
    inline name match
      case _: La =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](0)(" with " + codeOf(name))
        (to.labelled[La], q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: Lb =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](1)(" with " + codeOf(name))
        (q._1, to.labelled[Lb], q._3, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: Lc =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](2)(" with " + codeOf(name))
        (q._1, q._2, to.labelled[Lc], q._4, q._5, q._6, q._7, q._8, q._9)
      case _: Ld =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](3)(" with " + codeOf(name))
        (q._1, q._2, q._3, to.labelled[Ld], q._5, q._6, q._7, q._8, q._9)
      case _: Le =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](4)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, to.labelled[Le], q._6, q._7, q._8, q._9)
      case _: Lf =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](5)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, q._5, to.labelled[Lf], q._7, q._8, q._9)
      case _: Lg =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](6)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, q._5, q._6, to.labelled[Lg], q._8, q._9)
      case _: Lh =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](7)(" with " + codeOf(name))
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, to.labelled[Lh], q._9)
      case _: Li =>
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, to.labelled[Li])

  transparent inline def relabel[Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li)(inline to: Lx):
    (A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lx, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lx, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lx, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](0)(" with " + codeOf(name))
        LabelConflicts.alter9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](0)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Lb =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](1)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](1)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Lc =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](2)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lx, Ld, Le, Lf, Lg, Lh, Li](2)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Ld =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](3)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Lx, Le, Lf, Lg, Lh, Li](3)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Lx, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Le =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](4)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Ld, Lx, Lf, Lg, Lh, Li](4)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Lx, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Lf =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](5)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Ld, Le, Lx, Lg, Lh, Li](5)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lx, G \^ Lg, H \^ Lh, I \^ Li)]
      case _: Lg =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](6)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Ld, Le, Lf, Lx, Lh, Li](6)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lx, H \^ Lh, I \^ Li)]
      case _: Lh =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](7)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Ld, Le, Lf, Lg, Lx, Li](7)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lx, I \^ Li)]
      case _: Li =>
        LabelConflicts.alter9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Lx](8)(" for new label " + codeOf(to))
        q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Lx)]

  transparent inline def redo[X, Lx <: LabelVal](inline name: La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li)(to: X \^ Lx):
    (X \^ Lx, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, X \^ Lx, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, X \^ Lx, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, X \^ Lx, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, X \^ Lx, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, X \^ Lx, G \^ Lg, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, X \^ Lx, H \^ Lh, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, X \^ Lx, I \^ Li) |
    (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, X \^ Lx)
    =
    inline name match
      case _: La =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](0)(" with " + codeOf(name))
        LabelConflicts.alter9[Lx, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](0)(" for new label")
        (to, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: Lb =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](1)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lx, Lc, Ld, Le, Lf, Lg, Lh, Li](1)(" for new label")
        (q._1, to, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: Lc =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](2)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lx, Ld, Le, Lf, Lg, Lh, Li](2)(" for new label")
        (q._1, q._2, to, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: Ld =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](3)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Lx, Le, Lf, Lg, Lh, Li](3)(" for new label")
        (q._1, q._2, q._3, to, q._5, q._6, q._7, q._8, q._9)
      case _: Le =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](4)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Ld, Lx, Lf, Lg, Lh, Li](4)(" for new label")
        (q._1, q._2, q._3, q._4, to, q._6, q._7, q._8, q._9)
      case _: Lf =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](5)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Ld, Le, Lx, Lg, Lh, Li](5)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, to, q._7, q._8, q._9)
      case _: Lg =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](6)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Ld, Le, Lf, Lx, Lh, Li](6)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, q._6, to, q._8, q._9)
      case _: Lh =>
        LabelConflicts.types9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li](7)(" with " + codeOf(name))
        LabelConflicts.alter9[La, Lb, Lc, Ld, Le, Lf, Lg, Lx, Li](7)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, to, q._9)
      case _: Li =>
        LabelConflicts.alter9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Lx](8)(" for new label")
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, to)
}

extension [A, B, C, D, E, F, G, H, I](q: (A, B, C, D, E, F, G, H, I)) {
  /** Infer or explicitly add labels to tuple */
  inline def label[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal]: (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li ) =
    LabelConflicts.uniq9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]()
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]

  /** Add labels by value */
  inline def \\[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal](inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh, inline li: Li): (A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li ) =
    LabelConflicts.unik9(la, lb, lc, ld, le, lf, lg, lh, li)
    q.asInstanceOf[(A \^ La, B \^ Lb, C \^ Lc, D \^ Ld, E \^ Le, F \^ Lf, G \^ Lg, H \^ Lh, I \^ Li)]
}
