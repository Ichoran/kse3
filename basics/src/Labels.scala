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

  inline def unyq2[L1, L2](inline m1: String, inline m2: String): Unit =
    diff[L1, L2](m1, m2)("")
  inline def unyq3[L1, L2, L3](inline m1: String, inline m2: String, inline m3: String): Unit =
    unyq2[L1, L2](m1, m2)
    head3[L3, L1, L2](m3, m1, m2)("")
  inline def unyq4[L1, L2, L3, L4](inline m1: String, inline m2: String, inline m3: String, inline m4: String): Unit =
    unyq3[L1, L2, L3](m1, m2, m3)
    head4[L4, L1, L2, L3](m4, m1, m2, m3)("")
  inline def unyq5[L1, L2, L3, L4, L5](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String): Unit =
    unyq4[L1, L2, L3, L4](m1, m2, m3, m4)
    head5[L4, L1, L2, L3, L4](m5, m1, m2, m3, m4)("")
  inline def unyq6[L1, L2, L3, L4, L5, L6](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String): Unit =
    unyq5[L1, L2, L3, L4, L5](m1, m2, m3, m4, m5)
    head6[L4, L1, L2, L3, L4, L5](m6, m1, m2, m3, m4, m5)("")
  inline def unyq7[L1, L2, L3, L4, L5, L6, L7](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String, inline m7: String): Unit =
    unyq6[L1, L2, L3, L4, L5, L6](m1, m2, m3, m4, m5, m6)
    head7[L4, L1, L2, L3, L4, L5, L6](m7, m1, m2, m3, m4, m5, m6)("")
  inline def unyq8[L1, L2, L3, L4, L5, L6, L7, L8](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String, inline m7: String, inline m8: String): Unit =
    unyq7[L1, L2, L3, L4, L5, L6, L7](m1, m2, m3, m4, m5, m6, m7)
    head8[L4, L1, L2, L3, L4, L5, L6, L7](m8, m1, m2, m3, m4, m5, m6, m7)("")
  inline def unyq9[L1, L2, L3, L4, L5, L6, L7, L8, L9](inline m1: String, inline m2: String, inline m3: String, inline m4: String, inline m5: String, inline m6: String, inline m7: String, inline m8: String, inline m9: String): Unit =
    unyq8[L1, L2, L3, L4, L5, L6, L7, L8](m1, m2, m3, m4, m5, m6, m7, m8)
    head9[L4, L1, L2, L3, L4, L5, L6, L7, L8](m9, m1, m2, m3, m4, m5, m6, m7, m8)("")
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

  inline def ~~(inline la: La, inline lb: Lb): (A, B) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): (A | B) \^ Lz =
    summonFrom:
      case _: (La =:= Lz) =>
          LabelConflicts.types2[La, Lb](0)(" with " + codeOf(lz))
          q._1.asInstanceOf[(A \^ Lz)]
      case _: (Lb =:= Lz) => q._2.asInstanceOf[(B \^ Lz)]
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): (q.type | (B \^ Lb, A \^ La)) =
    LabelConflicts.unik2(lz, ly)
    summonFrom:
      case _: (La =:= Lz) => summonFrom:
        case _: (Lb =:= Ly) => q
        case _              => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lb =:= Lz) => summonFrom:
        case _: (La =:= Ly) => (q._2, q._1)
        case _              => compiletime.error("No label found matching " + codeOf(lz))
      case _              => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def updatedBy[Tz](source: Tz): (A \^ La | Tz, B \^ Lb | Tz) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => 
        LabelConflicts.diff[La, Lb]("_1", "_2")("")
        (source, q._2)
      case _: ((Nothing \^ Lb) <:< Tz) => 
        (q._1, source)
      case _ =>
        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty](source: (Tz, Ty)): (A \^ La | Tz | Ty, B \^ Lb | Tz | Ty) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._2)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._1)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            (q._1, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            (q._1, source._2)
          case _ =>
            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx](source: (Tz, Ty, Tx)): (A \^ La | Tz | Ty | Tx, B \^ Lb | Tz | Ty | Tx) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._3)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._1)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._3)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._2)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._3, q._2)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            (q._1, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            (q._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            (q._1, source._3)
          case _ =>
            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw](source: (Tz, Ty, Tx, Tw)): (A \^ La | Tz | Ty | Tx | Tw, B \^ Lb | Tz | Ty | Tx | Tw) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._4)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._1)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._4)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._2)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._4)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._3, q._2)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._3)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._4, q._2)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            (q._1, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            (q._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            (q._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            (q._1, source._4)
          case _ =>
            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv](source: (Tz, Ty, Tx, Tw, Tv)): (A \^ La | Tz | Ty | Tx | Tw | Tv, B \^ Lb | Tz | Ty | Tx | Tw | Tv) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._5)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._1)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._5)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._2)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._5)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._3, q._2)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._5)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._4, q._2)
      case _: ((Nothing \^ La) <:< Tv) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._4)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._5, q._2)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            (q._1, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            (q._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            (q._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            (q._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            (q._1, source._5)
          case _ =>
            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu](source: (Tz, Ty, Tx, Tw, Tv, Tu)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._6)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._1)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._6)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._2)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._6)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._3, q._2)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._6)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._4, q._2)
      case _: ((Nothing \^ La) <:< Tv) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._4)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._6)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._5, q._2)
      case _: ((Nothing \^ La) <:< Tu) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._5)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._6, q._2)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            (q._1, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            (q._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            (q._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            (q._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            (q._1, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            (q._1, source._6)
          case _ =>
            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._7)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._1)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._7)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._2)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._7)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._3, q._2)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._7)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._4, q._2)
      case _: ((Nothing \^ La) <:< Tv) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._4)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._7)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._5, q._2)
      case _: ((Nothing \^ La) <:< Tu) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._5)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._7)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._6, q._2)
      case _: ((Nothing \^ La) <:< Tt) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._6)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._7, q._2)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            (q._1, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            (q._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            (q._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            (q._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            (q._1, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            (q._1, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            (q._1, source._7)
          case _ =>
            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._8)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._1)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._8)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._2)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._8)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._3, q._2)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._8)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._4, q._2)
      case _: ((Nothing \^ La) <:< Tv) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._4)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._8)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._5, q._2)
      case _: ((Nothing \^ La) <:< Tu) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._5)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._8)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._6, q._2)
      case _: ((Nothing \^ La) <:< Tt) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._6)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._8)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._7, q._2)
      case _: ((Nothing \^ La) <:< Ts) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._7)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._8, q._2)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            (q._1, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            (q._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            (q._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            (q._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            (q._1, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            (q._1, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            (q._1, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            (q._1, source._8)
          case _ =>
            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._8)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._1, source._9)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._1)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._8)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._2, source._9)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._2)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._8)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._3, source._9)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._3, q._2)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._8)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._4, source._9)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._4, q._2)
      case _: ((Nothing \^ La) <:< Tv) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._4)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._8)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._5, source._9)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._5, q._2)
      case _: ((Nothing \^ La) <:< Tu) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._5)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._8)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._6, source._9)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._6, q._2)
      case _: ((Nothing \^ La) <:< Tt) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._6)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._8)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._7, source._9)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._7, q._2)
      case _: ((Nothing \^ La) <:< Ts) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._7)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._8, source._9)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._8, q._2)
      case _: ((Nothing \^ La) <:< Tr) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._9, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._9, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._9, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._9, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._9, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._9, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._9, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            (source._9, source._8)
          case _ =>
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._9, q._2)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            (q._1, source._1)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            (q._1, source._2)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            (q._1, source._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            (q._1, source._4)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            (q._1, source._5)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            (q._1, source._6)
          case _: ((Nothing \^ Lb) <:< Tt) =>
            (q._1, source._7)
          case _: ((Nothing \^ Lb) <:< Ts) =>
            (q._1, source._8)
          case _: ((Nothing \^ Lb) <:< Tr) =>
            (q._1, source._9)
          case _ =>
            compiletime.error("No matching labels so no update possible")

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

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc): (A, B, C) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): ((A | B | C) \^ Lz) =
    summonFrom:
      case _: (Lz =:= La) =>
        LabelConflicts.head3[Lz, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(lz))
        q._1.asInstanceOf[(A \^ Lz)]
      case _: (Lz =:= Lb) =>
        LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
        q._2.asInstanceOf[(B \^ Lz)]
      case _: (Lz =:= Lc) =>
        q._3.asInstanceOf[(C \^ Lz)]
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C) \^ Lz, (A | B | C) \^ Ly) =
    LabelConflicts.unik2(lz, ly)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
            (q._1.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
            (q._1.unlabel, q._3.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
            (q._2.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            (q._2.unlabel, q._3.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
            (q._3.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            (q._3.unlabel, q._2.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C) \^ Lz, (A | B | C) \^ Ly, (A | B | C) \^ Lx) =
    LabelConflicts.unik3(lz, ly, lx)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                q.asInstanceOf[(A \^ Lz, B \^ Ly, C \^ Lx)]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                (q._1.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                (q._2.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                (q._2.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                (q._3.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                (q._3.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def updatedBy[Tz](source: Tz): (A \^ La | Tz, B \^ Lb | Tz, C \^ Lc | Tz) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => 
        LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
        (source, q._2, q._3)
      case _: ((Nothing \^ Lb) <:< Tz) => 
        LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
        (q._1, source, q._3)
      case _: ((Nothing \^ Lc) <:< Tz) => 
        (q._1, q._2, source)
      case _ =>
        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty](source: (Tz, Ty)): (A \^ La | Tz | Ty, B \^ Lb | Tz | Ty, C \^ Lc | Tz | Ty) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.diff[La, Lc]("_1", "_3")("")
            LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
            (source._1, source._2, q._3)
          case _: ((Nothing \^ Lc) <:< Ty) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._1, q._2, source._2)
          case _ =>
            LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
            (source._1, q._2, q._3)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.diff[La, Lc]("_1", "_3")("")
            LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
            (source._2, source._1, q._3)
          case _: ((Nothing \^ Lc) <:< Tz) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.diff[La, Lb]("_1", "_2")("")
            (source._2, q._2, source._1)
          case _ =>
            LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
            (source._2, q._2, q._3)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._2)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._1)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._2, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                (q._1, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                (q._1, q._2, source._2)
              case _ =>
                compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx](source: (Tz, Ty, Tx)): (A \^ La | Tz | Ty | Tx, B \^ Lb | Tz | Ty | Tx, C \^ Lc | Tz | Ty | Tx) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._3, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._3)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._1, q._2, q._3)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._1)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._3, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._3)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._2, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._1)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._2, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._2)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._3, q._2, q._3)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._3)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._3)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._2)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._3, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                (q._1, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                (q._1, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                (q._1, q._2, source._3)
              case _ =>
                compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw](source: (Tz, Ty, Tx, Tw)): (A \^ La | Tz | Ty | Tx | Tw, B \^ Lb | Tz | Ty | Tx | Tw, C \^ Lc | Tz | Ty | Tx | Tw) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._4, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._4)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._1, q._2, q._3)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._4, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._4)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._2, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._4, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._4)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._3, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._3, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._3)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._4, q._2, q._3)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._4)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._4)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._4)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._3)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._4, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                (q._1, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                (q._1, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                (q._1, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                (q._1, q._2, source._4)
              case _ =>
                compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv](source: (Tz, Ty, Tx, Tw, Tv)): (A \^ La | Tz | Ty | Tx | Tw | Tv, B \^ Lb | Tz | Ty | Tx | Tw | Tv, C \^ Lc | Tz | Ty | Tx | Tw | Tv) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._5, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._5, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._5, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._5)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._1, q._2, q._3)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._5, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._5, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._5, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._5)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._2, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._5, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._5, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._5)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._3, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._5, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._5, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._5)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._4, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tv) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._1, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._2, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._3, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._4, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._4, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._4)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._5, q._2, q._3)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._5)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._5)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._5)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._5)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._4)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._5, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                (q._1, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                (q._1, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                (q._1, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                (q._1, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                (q._1, q._2, source._5)
              case _ =>
                compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu](source: (Tz, Ty, Tx, Tw, Tv, Tu)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._2, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._3, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._4, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._5, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._5, source._4)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._5, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._5, q._3)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._6, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._6, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._6, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._1, source._6, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._6, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._1, q._2, source._6)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._1, q._2, q._3)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._1, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._3, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._4, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._5, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._5, source._4)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._5, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._5, q._3)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._6, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._6, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._6, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._2, source._6, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._6, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._2, q._2, source._6)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._2, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._1, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._2, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._4, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._5, source._4)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._5, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._5, q._3)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._6, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._6, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._6, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._3, source._6, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._6, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._3, q._2, source._6)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._3, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._1, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._2, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._3, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._5, source._3)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._5, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._5, q._3)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._6, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._6, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._6, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._4, source._6, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._4, source._6, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._4, q._2, source._6)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._4, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tv) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._1, source._4)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._1, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._2, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._3, source._4)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._3, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._4, source._3)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._4, source._6)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._6, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._6, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._6, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._5, source._6, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._5, source._6, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._5, q._2, source._6)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._5, q._2, q._3)
      case _: ((Nothing \^ La) <:< Tu) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._1, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._1, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._6, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._2, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._6, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._3, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._3, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._6, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._4, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._4, source._5)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._6, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._5, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                (source._6, source._5, source._4)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._6, source._5, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._6, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._6, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._6, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._6, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[La, Lc]("_1", "_3")
                LabelConflicts.diff[La, Lb]("_1", "_2")("")
                (source._6, q._2, source._5)
              case _ =>
                LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                (source._6, q._2, q._3)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._1, source._6)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._1, q._3)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._1)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._2, source._6)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._2, q._3)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._2)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._3, source._6)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._3, q._3)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._3)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._4, source._6)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._4, q._3)
          case _: ((Nothing \^ Lb) <:< Tv) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._4)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._5, source._6)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._5, q._3)
          case _: ((Nothing \^ Lb) <:< Tu) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._6, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._6, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._6, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._6, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                (q._1, source._6, source._5)
              case _ =>
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._6, q._3)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                (q._1, q._2, source._1)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                (q._1, q._2, source._2)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                (q._1, q._2, source._3)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                (q._1, q._2, source._4)
              case _: ((Nothing \^ Lc) <:< Tv) =>
                (q._1, q._2, source._5)
              case _: ((Nothing \^ Lc) <:< Tu) =>
                (q._1, q._2, source._6)
              case _ =>
                compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt) =
  compiletime.error("Implementation restrictions prevent name-update of 3-tuple by 7-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts) =
  compiletime.error("Implementation restrictions prevent name-update of 3-tuple by 8-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr) =
  compiletime.error("Implementation restrictions prevent name-update of 3-tuple by 9-tuple")

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

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld): (A, B, C, D) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): ((A | B | C | D) \^ Lz) =
    summonFrom:
      case _: (Lz =:= La) =>
        LabelConflicts.head4[Lz, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(lz))
        q._1.asInstanceOf[(A \^ Lz)]
      case _: (Lz =:= Lb) =>
        LabelConflicts.head3[Lz, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lz))
        q._2.asInstanceOf[(B \^ Lz)]
      case _: (Lz =:= Lc) =>
        LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
        q._3.asInstanceOf[(C \^ Lz)]
      case _: (Lz =:= Ld) =>
        q._4.asInstanceOf[(D \^ Lz)]
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D) \^ Lz, (A | B | C | D) \^ Ly) =
    LabelConflicts.unik2(lz, ly)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            LabelConflicts.head3[Lz, Lc, Ld]("_1", "_3", "_4")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(ly))
            (q._1.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head3[Lz, Lb, Ld]("_1", "_2", "_4")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
            (q._1.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head3[Lz, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(lz))
            (q._1.unlabel, q._4.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head3[Lz, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lc, Ld]("_1", "_3", "_4")(" with " + codeOf(ly))
            (q._2.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
            (q._2.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
            (q._2.unlabel, q._4.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lb, Ld]("_1", "_2", "_4")(" with " + codeOf(ly))
            (q._3.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
            (q._3.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            (q._3.unlabel, q._4.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head3[Ly, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(ly))
            (q._4.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
            (q._4.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            (q._4.unlabel, q._3.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D) \^ Lz, (A | B | C | D) \^ Ly, (A | B | C | D) \^ Lx) =
    LabelConflicts.unik3(lz, ly, lx)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Ld]("_1", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                (q._1.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Ld]("_1", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
                (q._1.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
                (q._1.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_1", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
                (q._2.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_1", "_4")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                (q._2.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Lc]("_1", "_3")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                (q._2.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_1", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
                (q._3.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_1", "_4")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                (q._3.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lx, Lb]("_1", "_2")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                (q._3.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
                (q._4.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lc]("_1", "_3")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                (q._4.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lx, Lb]("_1", "_2")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                (q._4.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D) \^ Lz, (A | B | C | D) \^ Ly, (A | B | C | D) \^ Lx, (A | B | C | D) \^ Lw) =
    LabelConflicts.unik4(lz, ly, lx, lw)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    q.asInstanceOf[(A \^ Lz, B \^ Ly, C \^ Lx, D \^ Lw)]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    (q._1.unlabel, q._2.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    (q._1.unlabel, q._3.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    (q._1.unlabel, q._3.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    (q._1.unlabel, q._4.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    (q._1.unlabel, q._4.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    (q._2.unlabel, q._1.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    (q._2.unlabel, q._1.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    (q._2.unlabel, q._3.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    (q._2.unlabel, q._3.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    (q._2.unlabel, q._4.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    (q._2.unlabel, q._4.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    (q._3.unlabel, q._1.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    (q._3.unlabel, q._1.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    (q._3.unlabel, q._2.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    (q._3.unlabel, q._2.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    (q._3.unlabel, q._4.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    (q._3.unlabel, q._4.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    (q._4.unlabel, q._1.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    (q._4.unlabel, q._1.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    (q._4.unlabel, q._2.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    (q._4.unlabel, q._2.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    (q._4.unlabel, q._3.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    (q._4.unlabel, q._3.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def updatedBy[Tz](source: Tz): (A \^ La | Tz, B \^ Lb | Tz, C \^ Lc | Tz, D \^ Ld | Tz) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => 
        LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
        (source, q._2, q._3, q._4)
      case _: ((Nothing \^ Lb) <:< Tz) => 
        LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
        (q._1, source, q._3, q._4)
      case _: ((Nothing \^ Lc) <:< Tz) => 
        LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
        (q._1, q._2, source, q._4)
      case _: ((Nothing \^ Ld) <:< Tz) => 
        (q._1, q._2, q._3, source)
      case _ =>
        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty](source: (Tz, Ty)): (A \^ La | Tz | Ty, B \^ Lb | Tz | Ty, C \^ Lc | Tz | Ty, D \^ Ld | Tz | Ty) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
            LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
            (source._1, source._2, q._3, q._4)
          case _: ((Nothing \^ Lc) <:< Ty) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
            LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
            (source._1, q._2, source._2, q._4)
          case _: ((Nothing \^ Ld) <:< Ty) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
            (source._1, q._2, q._3, source._2)
          case _ =>
            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
            (source._1, q._2, q._3, q._4)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
            LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
            (source._2, source._1, q._3, q._4)
          case _: ((Nothing \^ Lc) <:< Tz) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
            LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
            (source._2, q._2, source._1, q._4)
          case _: ((Nothing \^ Ld) <:< Tz) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
            (source._2, q._2, q._3, source._1)
          case _ =>
            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
            (source._2, q._2, q._3, q._4)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                (q._1, source._1, source._2, q._4)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._1, q._3, source._2)
              case _ =>
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (q._1, source._1, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                (q._1, source._2, source._1, q._4)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (q._1, source._2, q._3, source._1)
              case _ =>
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (q._1, source._2, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._1, source._2)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._2, source._1)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._2, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    (q._1, q._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    (q._1, q._2, q._3, source._2)
                  case _ =>
                    compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx](source: (Tz, Ty, Tx)): (A \^ La | Tz | Ty | Tx, B \^ Lb | Tz | Ty | Tx, C \^ Lc | Tz | Ty | Tx, D \^ Ld | Tz | Ty | Tx) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.diff[La, Ld]("_1", "_4")("")
                LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                (source._1, source._2, source._3, q._4)
              case _: ((Nothing \^ Ld) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._2, q._3, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._1, source._2, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.diff[La, Ld]("_1", "_4")("")
                LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                (source._1, source._3, source._2, q._4)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._1, source._3, q._3, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._1, source._3, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._1, q._2, source._2, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, q._2, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._1, q._2, source._3, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, q._2, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._1, q._2, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._1, q._2, q._3, source._3)
                  case _ =>
                    LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                    (source._1, q._2, q._3, q._4)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.diff[La, Ld]("_1", "_4")("")
                LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                (source._2, source._1, source._3, q._4)
              case _: ((Nothing \^ Ld) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._1, q._3, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._2, source._1, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.diff[La, Ld]("_1", "_4")("")
                LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                (source._2, source._3, source._1, q._4)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._2, source._3, q._3, source._1)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._2, source._3, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._2, q._2, source._1, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, q._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._2, q._2, source._3, source._1)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, q._2, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._2, q._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._2, q._2, q._3, source._3)
                  case _ =>
                    LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                    (source._2, q._2, q._3, q._4)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.diff[La, Ld]("_1", "_4")("")
                LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                (source._3, source._1, source._2, q._4)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._1, q._3, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._3, source._1, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.diff[La, Ld]("_1", "_4")("")
                LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                (source._3, source._2, source._1, q._4)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.diff[La, Lc]("_1", "_3")("")
                LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                (source._3, source._2, q._3, source._1)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._3, source._2, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._3, q._2, source._1, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, q._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._3, q._2, source._2, source._1)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, q._2, source._2, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._3, q._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._3, q._2, q._3, source._2)
                  case _ =>
                    LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                    (source._3, q._2, q._3, q._4)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._1, source._2, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._1, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._1, source._3, source._2)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._1, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._1, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._1, q._3, source._3)
                  case _ =>
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (q._1, source._1, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._2, source._1, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._2, source._3, source._1)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._2, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._2, q._3, source._3)
                  case _ =>
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (q._1, source._2, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._3, source._1, source._2)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._3, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._3, source._2, source._1)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._3, source._2, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._3, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._3, q._3, source._2)
                  case _ =>
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (q._1, source._3, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._1, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._1, source._3)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._2, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._2, source._3)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._3, source._2)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    (q._1, q._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    (q._1, q._2, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    (q._1, q._2, q._3, source._3)
                  case _ =>
                    compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw](source: (Tz, Ty, Tx, Tw)): (A \^ La | Tz | Ty | Tx | Tw, B \^ Lb | Tz | Ty | Tx | Tw, C \^ Lc | Tz | Ty | Tx | Tw, D \^ Ld | Tz | Ty | Tx | Tw) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._1, source._2, source._3, source._4)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, source._2, source._3, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._1, source._2, source._4, source._3)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, source._2, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._1, source._2, q._3, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._1, source._2, q._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._1, source._2, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._1, source._3, source._2, source._4)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, source._3, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._1, source._3, source._4, source._2)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, source._3, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._1, source._3, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._1, source._3, q._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._1, source._3, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._1, source._4, source._2, source._3)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, source._4, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._1, source._4, source._3, source._2)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, source._4, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._1, source._4, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._1, source._4, q._3, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._1, source._4, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._1, q._2, source._2, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._1, q._2, source._2, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, q._2, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._1, q._2, source._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._1, q._2, source._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, q._2, source._3, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._1, q._2, source._4, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._1, q._2, source._4, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, q._2, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._1, q._2, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._1, q._2, q._3, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._1, q._2, q._3, source._4)
                  case _ =>
                    LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                    (source._1, q._2, q._3, q._4)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._2, source._1, source._3, source._4)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, source._1, source._3, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._2, source._1, source._4, source._3)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, source._1, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._2, source._1, q._3, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._2, source._1, q._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._2, source._1, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._2, source._3, source._1, source._4)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, source._3, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._2, source._3, source._4, source._1)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, source._3, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._2, source._3, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._2, source._3, q._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._2, source._3, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._2, source._4, source._1, source._3)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, source._4, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._2, source._4, source._3, source._1)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, source._4, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._2, source._4, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._2, source._4, q._3, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._2, source._4, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._2, q._2, source._1, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._2, q._2, source._1, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, q._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._2, q._2, source._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._2, q._2, source._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, q._2, source._3, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._2, q._2, source._4, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._2, q._2, source._4, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, q._2, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._2, q._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._2, q._2, q._3, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._2, q._2, q._3, source._4)
                  case _ =>
                    LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                    (source._2, q._2, q._3, q._4)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._3, source._1, source._2, source._4)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, source._1, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._3, source._1, source._4, source._2)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, source._1, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._3, source._1, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._3, source._1, q._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._3, source._1, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._3, source._2, source._1, source._4)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, source._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._3, source._2, source._4, source._1)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, source._2, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._3, source._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._3, source._2, q._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._3, source._2, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._3, source._4, source._1, source._2)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, source._4, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._3, source._4, source._2, source._1)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, source._4, source._2, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._3, source._4, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._3, source._4, q._3, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._3, source._4, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._3, q._2, source._1, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._3, q._2, source._1, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, q._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._3, q._2, source._2, source._1)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._3, q._2, source._2, source._4)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, q._2, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._3, q._2, source._4, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._3, q._2, source._4, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, q._2, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._3, q._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._3, q._2, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._3, q._2, q._3, source._4)
                  case _ =>
                    LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                    (source._3, q._2, q._3, q._4)
      case _: ((Nothing \^ La) <:< Tw) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._4, source._1, source._2, source._3)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, source._1, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._4, source._1, source._3, source._2)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, source._1, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._4, source._1, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._4, source._1, q._3, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._4, source._1, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._4, source._2, source._1, source._3)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, source._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._4, source._2, source._3, source._1)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, source._2, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._4, source._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._4, source._2, q._3, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._4, source._2, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._4, source._3, source._1, source._2)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, source._3, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")
                    (source._4, source._3, source._2, source._1)
                  case _ =>
                    LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                    LabelConflicts.diff[La, Ld]("_1", "_4")("")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, source._3, source._2, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._4, source._3, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                    LabelConflicts.diff[La, Lc]("_1", "_3")("")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (source._4, source._3, q._3, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lb]("_1", "_2")
                    LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (source._4, source._3, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._4, q._2, source._1, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._4, q._2, source._1, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, q._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._4, q._2, source._2, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._4, q._2, source._2, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, q._2, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._4, q._2, source._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.diff[La, Lb]("_1", "_2")("")
                    (source._4, q._2, source._3, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._4, q._2, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._4, q._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._4, q._2, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[La, Ld]("_1", "_4")
                    LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                    (source._4, q._2, q._3, source._3)
                  case _ =>
                    LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                    (source._4, q._2, q._3, q._4)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._1, source._2, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._1, source._2, source._4)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._1, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._1, source._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._1, source._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._1, source._3, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._1, source._4, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._1, source._4, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._1, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._1, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._1, q._3, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._1, q._3, source._4)
                  case _ =>
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (q._1, source._1, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._2, source._1, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._2, source._1, source._4)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._2, source._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._2, source._3, source._4)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._2, source._3, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._2, source._4, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._2, source._4, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._2, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._2, q._3, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._2, q._3, source._4)
                  case _ =>
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (q._1, source._2, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._3, source._1, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._3, source._1, source._4)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._3, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._3, source._2, source._1)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._3, source._2, source._4)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._3, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._3, source._4, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._3, source._4, source._2)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._3, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._3, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._3, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._3, q._3, source._4)
                  case _ =>
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (q._1, source._3, q._3, q._4)
          case _: ((Nothing \^ Lb) <:< Tw) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._4, source._1, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._4, source._1, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._4, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._4, source._2, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._4, source._2, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._4, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._4, source._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    (q._1, source._4, source._3, source._2)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._4, source._3, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._4, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._4, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                    LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                    (q._1, source._4, q._3, source._3)
                  case _ =>
                    LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                    (q._1, source._4, q._3, q._4)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._1, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._1, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._1, source._4)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._1, q._4)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._2, source._1)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._2, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._2, source._4)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._2, q._4)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._3, source._4)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._3, q._4)
              case _: ((Nothing \^ Lc) <:< Tw) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._4, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._4, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    (q._1, q._2, source._4, source._3)
                  case _ =>
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._4, q._4)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    (q._1, q._2, q._3, source._1)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    (q._1, q._2, q._3, source._2)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    (q._1, q._2, q._3, source._3)
                  case _: ((Nothing \^ Ld) <:< Tw) =>
                    (q._1, q._2, q._3, source._4)
                  case _ =>
                    compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv](source: (Tz, Ty, Tx, Tw, Tv)): (A \^ La | Tz | Ty | Tx | Tw | Tv, B \^ Lb | Tz | Ty | Tx | Tw | Tv, C \^ Lc | Tz | Ty | Tx | Tw | Tv, D \^ Ld | Tz | Ty | Tx | Tw | Tv) =
  compiletime.error("Implementation restrictions prevent name-update of 4-tuple by 5-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu](source: (Tz, Ty, Tx, Tw, Tv, Tu)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu) =
  compiletime.error("Implementation restrictions prevent name-update of 4-tuple by 6-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt) =
  compiletime.error("Implementation restrictions prevent name-update of 4-tuple by 7-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts) =
  compiletime.error("Implementation restrictions prevent name-update of 4-tuple by 8-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr) =
  compiletime.error("Implementation restrictions prevent name-update of 4-tuple by 9-tuple")

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

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le): (A, B, C, D, E) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): ((A | B | C | D | E) \^ Lz) =
    summonFrom:
      case _: (Lz =:= La) =>
        LabelConflicts.head5[Lz, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(lz))
        q._1.asInstanceOf[(A \^ Lz)]
      case _: (Lz =:= Lb) =>
        LabelConflicts.head4[Lz, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lz))
        q._2.asInstanceOf[(B \^ Lz)]
      case _: (Lz =:= Lc) =>
        LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
        q._3.asInstanceOf[(C \^ Lz)]
      case _: (Lz =:= Ld) =>
        LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
        q._4.asInstanceOf[(D \^ Lz)]
      case _: (Lz =:= Le) =>
        q._5.asInstanceOf[(E \^ Lz)]
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E) \^ Lz, (A | B | C | D | E) \^ Ly) =
    LabelConflicts.unik2(lz, ly)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            LabelConflicts.head4[Lz, Lc, Ld, Le]("_1", "_3", "_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(ly))
            (q._1.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head4[Lz, Lb, Ld, Le]("_1", "_2", "_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
            (q._1.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head4[Lz, Lb, Lc, Le]("_1", "_2", "_3", "_5")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
            (q._1.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head4[Lz, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(lz))
            (q._1.unlabel, q._5.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head4[Lz, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lc, Ld, Le]("_1", "_3", "_4", "_5")(" with " + codeOf(ly))
            (q._2.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head3[Lz, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
            (q._2.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head3[Lz, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
            (q._2.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head3[Lz, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lz))
            (q._2.unlabel, q._5.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lb, Ld, Le]("_1", "_2", "_4", "_5")(" with " + codeOf(ly))
            (q._3.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(ly))
            (q._3.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
            (q._3.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
            (q._3.unlabel, q._5.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lb, Lc, Le]("_1", "_2", "_3", "_5")(" with " + codeOf(ly))
            (q._4.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(ly))
            (q._4.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
            (q._4.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            (q._4.unlabel, q._5.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head4[Ly, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(ly))
            (q._5.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head3[Ly, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(ly))
            (q._5.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
            (q._5.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            (q._5.unlabel, q._4.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E) \^ Lz, (A | B | C | D | E) \^ Ly, (A | B | C | D | E) \^ Lx) =
    LabelConflicts.unik3(lz, ly, lx)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Ld, Le]("_1", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lc, Le]("_1", "_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lc, Ld]("_1", "_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(ly))
                (q._1.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Ld, Le]("_1", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lb, Le]("_1", "_2", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lb, Ld]("_1", "_2", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                (q._1.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Lc, Le]("_1", "_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Lb, Le]("_1", "_2", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(lz))
                (q._1.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Lc, Ld]("_1", "_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Lb, Ld]("_1", "_2", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(lz))
                (q._1.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_1", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Le]("_1", "_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Ld]("_1", "_3", "_4")(" with " + codeOf(ly))
                (q._2.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_1", "_4", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Le]("_2", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                (q._2.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Le]("_1", "_3", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Le]("_2", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                (q._2.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Lc, Ld]("_1", "_3", "_4")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                (q._2.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_1", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lb, Le]("_1", "_2", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lb, Ld]("_1", "_2", "_4")(" with " + codeOf(ly))
                (q._3.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_1", "_4", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_2", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                (q._3.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lb, Le]("_1", "_2", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_2", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                (q._3.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Lb, Ld]("_1", "_2", "_4")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                (q._3.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Le]("_1", "_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lb, Le]("_1", "_2", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Ly, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(ly))
                (q._4.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Le]("_1", "_3", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_2", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                (q._4.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lb, Le]("_1", "_2", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_2", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                (q._4.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lx, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                (q._4.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Ly, Lc, Ld]("_1", "_3", "_4")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Ly, Lb, Ld]("_1", "_2", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Ly, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(ly))
                (q._5.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Ly, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Ld]("_1", "_3", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                (q._5.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lb, Ld]("_1", "_2", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                (q._5.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lx, Lb, Lc]("_1", "_2", "_3")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                (q._5.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E) \^ Lz, (A | B | C | D | E) \^ Ly, (A | B | C | D | E) \^ Lx, (A | B | C | D | E) \^ Lw) =
    LabelConflicts.unik4(lz, ly, lx, lw)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Le]("_1", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_2", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_4", "_5")(" with " + codeOf(lw))
                    (q._1.unlabel, q._2.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Ld]("_1", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                    (q._1.unlabel, q._2.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Le]("_1", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_2", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_3", "_5")(" with " + codeOf(lw))
                    (q._1.unlabel, q._2.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                    (q._1.unlabel, q._2.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Ld]("_1", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Ld]("_3", "_4")(" with " + codeOf(lw))
                    (q._1.unlabel, q._2.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                    (q._1.unlabel, q._2.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Le]("_1", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_2", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_4", "_5")(" with " + codeOf(lw))
                    (q._1.unlabel, q._3.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Ld]("_1", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                    (q._1.unlabel, q._3.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Le]("_1", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_2", "_5")(" with " + codeOf(lw))
                    (q._1.unlabel, q._3.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
                    (q._1.unlabel, q._3.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Ld]("_1", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Ld]("_2", "_4")(" with " + codeOf(lw))
                    (q._1.unlabel, q._3.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
                    (q._1.unlabel, q._3.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Le]("_1", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_2", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_3", "_5")(" with " + codeOf(lw))
                    (q._1.unlabel, q._4.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                    (q._1.unlabel, q._4.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Le]("_1", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_2", "_5")(" with " + codeOf(lw))
                    (q._1.unlabel, q._4.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
                    (q._1.unlabel, q._4.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lw, Lc]("_2", "_3")(" with " + codeOf(lw))
                    (q._1.unlabel, q._4.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
                    (q._1.unlabel, q._4.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Ld]("_1", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_3", "_4")(" with " + codeOf(lw))
                    (q._1.unlabel, q._5.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                    (q._1.unlabel, q._5.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Ld]("_1", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_2", "_4")(" with " + codeOf(lw))
                    (q._1.unlabel, q._5.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
                    (q._1.unlabel, q._5.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Lc]("_1", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lw, Lc]("_2", "_3")(" with " + codeOf(lw))
                    (q._1.unlabel, q._5.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Lb]("_1", "_2")(" with " + codeOf(lz))
                    (q._1.unlabel, q._5.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Le]("_2", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_1", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_4", "_5")(" with " + codeOf(lw))
                    (q._2.unlabel, q._1.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_1", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                    (q._2.unlabel, q._1.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Le]("_2", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_1", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_3", "_5")(" with " + codeOf(lw))
                    (q._2.unlabel, q._1.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
                    (q._2.unlabel, q._1.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_1", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Ld]("_3", "_4")(" with " + codeOf(lw))
                    (q._2.unlabel, q._1.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
                    (q._2.unlabel, q._1.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Le]("_2", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_1", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_4", "_5")(" with " + codeOf(lw))
                    (q._2.unlabel, q._3.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_1", "_4")(" with " + codeOf(lx))
                    (q._2.unlabel, q._3.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Le]("_2", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_1", "_5")(" with " + codeOf(lw))
                    (q._2.unlabel, q._3.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    (q._2.unlabel, q._3.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Ld]("_1", "_4")(" with " + codeOf(lw))
                    (q._2.unlabel, q._3.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    (q._2.unlabel, q._3.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Le]("_2", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_1", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_3", "_5")(" with " + codeOf(lw))
                    (q._2.unlabel, q._4.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Lc]("_1", "_3")(" with " + codeOf(lx))
                    (q._2.unlabel, q._4.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Le]("_2", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_1", "_5")(" with " + codeOf(lw))
                    (q._2.unlabel, q._4.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    (q._2.unlabel, q._4.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lw, Lc]("_1", "_3")(" with " + codeOf(lw))
                    (q._2.unlabel, q._4.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    (q._2.unlabel, q._4.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Ld]("_1", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_3", "_4")(" with " + codeOf(lw))
                    (q._2.unlabel, q._5.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Lc]("_1", "_3")(" with " + codeOf(lx))
                    (q._2.unlabel, q._5.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Ld]("_2", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_1", "_4")(" with " + codeOf(lw))
                    (q._2.unlabel, q._5.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    (q._2.unlabel, q._5.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Lc]("_2", "_3")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lw, Lc]("_1", "_3")(" with " + codeOf(lw))
                    (q._2.unlabel, q._5.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    (q._2.unlabel, q._5.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_1", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_2", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_4", "_5")(" with " + codeOf(lw))
                    (q._3.unlabel, q._1.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_1", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                    (q._3.unlabel, q._1.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_1", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_2", "_5")(" with " + codeOf(lw))
                    (q._3.unlabel, q._1.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
                    (q._3.unlabel, q._1.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_1", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Ld]("_2", "_4")(" with " + codeOf(lw))
                    (q._3.unlabel, q._1.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
                    (q._3.unlabel, q._1.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_2", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_1", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_4", "_5")(" with " + codeOf(lw))
                    (q._3.unlabel, q._2.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_1", "_4")(" with " + codeOf(lx))
                    (q._3.unlabel, q._2.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_2", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_1", "_5")(" with " + codeOf(lw))
                    (q._3.unlabel, q._2.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    (q._3.unlabel, q._2.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Ld]("_1", "_4")(" with " + codeOf(lw))
                    (q._3.unlabel, q._2.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    (q._3.unlabel, q._2.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_1", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_2", "_5")(" with " + codeOf(lw))
                    (q._3.unlabel, q._4.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lx, Lb]("_1", "_2")(" with " + codeOf(lx))
                    (q._3.unlabel, q._4.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_2", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_1", "_5")(" with " + codeOf(lw))
                    (q._3.unlabel, q._4.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    (q._3.unlabel, q._4.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lw, Lb]("_1", "_2")(" with " + codeOf(lw))
                    (q._3.unlabel, q._4.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lb) =>
                    (q._3.unlabel, q._4.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Ld]("_1", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_2", "_4")(" with " + codeOf(lw))
                    (q._3.unlabel, q._5.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lx, Lb]("_1", "_2")(" with " + codeOf(lx))
                    (q._3.unlabel, q._5.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                    LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_1", "_4")(" with " + codeOf(lw))
                    (q._3.unlabel, q._5.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    (q._3.unlabel, q._5.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lw, Lb]("_1", "_2")(" with " + codeOf(lw))
                    (q._3.unlabel, q._5.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lb) =>
                    (q._3.unlabel, q._5.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_1", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_2", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_3", "_5")(" with " + codeOf(lw))
                    (q._4.unlabel, q._1.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                    (q._4.unlabel, q._1.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_1", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_2", "_5")(" with " + codeOf(lw))
                    (q._4.unlabel, q._1.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
                    (q._4.unlabel, q._1.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Lc]("_2", "_3")(" with " + codeOf(lw))
                    (q._4.unlabel, q._1.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
                    (q._4.unlabel, q._1.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_2", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_1", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_3", "_5")(" with " + codeOf(lw))
                    (q._4.unlabel, q._2.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Lc]("_1", "_3")(" with " + codeOf(lx))
                    (q._4.unlabel, q._2.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_2", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_1", "_5")(" with " + codeOf(lw))
                    (q._4.unlabel, q._2.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    (q._4.unlabel, q._2.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Lc]("_1", "_3")(" with " + codeOf(lw))
                    (q._4.unlabel, q._2.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    (q._4.unlabel, q._2.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_1", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_2", "_5")(" with " + codeOf(lw))
                    (q._4.unlabel, q._3.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    LabelConflicts.diff[Lx, Lb]("_1", "_2")(" with " + codeOf(lx))
                    (q._4.unlabel, q._3.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                    LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Le]("_2", "_5")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Le]("_1", "_5")(" with " + codeOf(lw))
                    (q._4.unlabel, q._3.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Le) =>
                    (q._4.unlabel, q._3.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Le) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lw, Lb]("_1", "_2")(" with " + codeOf(lw))
                    (q._4.unlabel, q._3.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lb) =>
                    (q._4.unlabel, q._3.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lx, Lc]("_1", "_3")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Lc]("_2", "_3")(" with " + codeOf(lw))
                    (q._4.unlabel, q._5.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lx, Lb]("_1", "_2")(" with " + codeOf(lx))
                    (q._4.unlabel, q._5.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Lc]("_1", "_3")(" with " + codeOf(lw))
                    (q._4.unlabel, q._5.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    (q._4.unlabel, q._5.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lw, Lb]("_1", "_2")(" with " + codeOf(lw))
                    (q._4.unlabel, q._5.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lb) =>
                    (q._4.unlabel, q._5.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Ly, Ld]("_1", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_3", "_4")(" with " + codeOf(lw))
                    (q._5.unlabel, q._1.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                    (q._5.unlabel, q._1.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Ly, Ld]("_1", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_2", "_4")(" with " + codeOf(lw))
                    (q._5.unlabel, q._1.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
                    (q._5.unlabel, q._1.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Ly, Lc]("_1", "_3")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Lc]("_2", "_3")(" with " + codeOf(lw))
                    (q._5.unlabel, q._1.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Ly, Lb]("_1", "_2")(" with " + codeOf(ly))
                    (q._5.unlabel, q._1.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_1", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_3", "_4")(" with " + codeOf(lw))
                    (q._5.unlabel, q._2.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Lc]("_1", "_3")(" with " + codeOf(lx))
                    (q._5.unlabel, q._2.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Ly, Ld]("_2", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_1", "_4")(" with " + codeOf(lw))
                    (q._5.unlabel, q._2.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    (q._5.unlabel, q._2.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Ly, Lc]("_2", "_3")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lw, Lc]("_1", "_3")(" with " + codeOf(lw))
                    (q._5.unlabel, q._2.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    (q._5.unlabel, q._2.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_1", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_2", "_4")(" with " + codeOf(lw))
                    (q._5.unlabel, q._3.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    LabelConflicts.diff[Lx, Lb]("_1", "_2")(" with " + codeOf(lx))
                    (q._5.unlabel, q._3.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                    LabelConflicts.diff[Lx, Ld]("_2", "_4")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Ld]("_1", "_4")(" with " + codeOf(lw))
                    (q._5.unlabel, q._3.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Ld) =>
                    (q._5.unlabel, q._3.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Ld) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lw, Lb]("_1", "_2")(" with " + codeOf(lw))
                    (q._5.unlabel, q._3.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lb) =>
                    (q._5.unlabel, q._3.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                summonFrom:
                  case _: (Lw =:= Lb) =>
                    LabelConflicts.diff[Lx, Lc]("_1", "_3")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Lc]("_2", "_3")(" with " + codeOf(lw))
                    (q._5.unlabel, q._4.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    LabelConflicts.diff[Lx, Lb]("_1", "_2")(" with " + codeOf(lx))
                    (q._5.unlabel, q._4.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lb) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lx, Lc]("_2", "_3")(" with " + codeOf(lx))
                    LabelConflicts.diff[Lw, Lc]("_1", "_3")(" with " + codeOf(lw))
                    (q._5.unlabel, q._4.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lc) =>
                    (q._5.unlabel, q._4.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _: (Lx =:= Lc) =>
                summonFrom:
                  case _: (Lw =:= La) =>
                    LabelConflicts.diff[Lw, Lb]("_1", "_2")(" with " + codeOf(lw))
                    (q._5.unlabel, q._4.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx, Lw]
                  case _: (Lw =:= Lb) =>
                    (q._5.unlabel, q._4.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx, Lw]
                  case _ => compiletime.error("No label found matching " + codeOf(lw))
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E) \^ Lz, (A | B | C | D | E) \^ Ly, (A | B | C | D | E) \^ Lx, (A | B | C | D | E) \^ Lw, (A | B | C | D | E) \^ Lv) =
    compiletime.error("Implementation restrictions prevent 5-way picking by name")

  transparent inline def updatedBy[Tz](source: Tz): (A \^ La | Tz, B \^ Lb | Tz, C \^ Lc | Tz, D \^ Ld | Tz, E \^ Le | Tz) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => 
        LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
        (source, q._2, q._3, q._4, q._5)
      case _: ((Nothing \^ Lb) <:< Tz) => 
        LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
        (q._1, source, q._3, q._4, q._5)
      case _: ((Nothing \^ Lc) <:< Tz) => 
        LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
        (q._1, q._2, source, q._4, q._5)
      case _: ((Nothing \^ Ld) <:< Tz) => 
        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
        (q._1, q._2, q._3, source, q._5)
      case _: ((Nothing \^ Le) <:< Tz) => 
        (q._1, q._2, q._3, q._4, source)
      case _ =>
        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty](source: (Tz, Ty)): (A \^ La | Tz | Ty, B \^ Lb | Tz | Ty, C \^ Lc | Tz | Ty, D \^ Ld | Tz | Ty, E \^ Le | Tz | Ty) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
            LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
            (source._1, source._2, q._3, q._4, q._5)
          case _: ((Nothing \^ Lc) <:< Ty) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
            LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
            (source._1, q._2, source._2, q._4, q._5)
          case _: ((Nothing \^ Ld) <:< Ty) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
            LabelConflicts.diff[Ld, Le]("_4", "_5")("")
            (source._1, q._2, q._3, source._2, q._5)
          case _: ((Nothing \^ Le) <:< Ty) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
            (source._1, q._2, q._3, q._4, source._2)
          case _ =>
            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
            (source._1, q._2, q._3, q._4, q._5)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
            LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
            (source._2, source._1, q._3, q._4, q._5)
          case _: ((Nothing \^ Lc) <:< Tz) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
            LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
            (source._2, q._2, source._1, q._4, q._5)
          case _: ((Nothing \^ Ld) <:< Tz) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
            LabelConflicts.diff[Ld, Le]("_4", "_5")("")
            (source._2, q._2, q._3, source._1, q._5)
          case _: ((Nothing \^ Le) <:< Tz) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
            (source._2, q._2, q._3, q._4, source._1)
          case _ =>
            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
            (source._2, q._2, q._3, q._4, q._5)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                (q._1, source._1, source._2, q._4, q._5)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                (q._1, source._1, q._3, source._2, q._5)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (q._1, source._1, q._3, q._4, source._2)
              case _ =>
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (q._1, source._1, q._3, q._4, q._5)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                (q._1, source._2, source._1, q._4, q._5)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                (q._1, source._2, q._3, source._1, q._5)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (q._1, source._2, q._3, q._4, source._1)
              case _ =>
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (q._1, source._2, q._3, q._4, q._5)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (q._1, q._2, source._1, source._2, q._5)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._1, q._4, source._2)
                  case _ =>
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, q._2, source._1, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (q._1, q._2, source._2, source._1, q._5)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, q._2, source._2, q._4, source._1)
                  case _ =>
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, q._2, source._2, q._4, q._5)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        (q._1, q._2, q._3, source._1, source._2)
                      case _ =>
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, q._3, source._1, q._5)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        (q._1, q._2, q._3, source._2, source._1)
                      case _ =>
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, q._3, source._2, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        (q._1, q._2, q._3, q._4, source._1)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        (q._1, q._2, q._3, q._4, source._2)
                      case _ =>
                        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx](source: (Tz, Ty, Tx)): (A \^ La | Tz | Ty | Tx, B \^ Lb | Tz | Ty | Tx, C \^ Lc | Tz | Ty | Tx, D \^ Ld | Tz | Ty | Tx, E \^ Le | Tz | Ty | Tx) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head3[La, Ld, Le]("_1", "_4", "_5")("")
                LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                (source._1, source._2, source._3, q._4, q._5)
              case _: ((Nothing \^ Ld) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head3[La, Lc, Le]("_1", "_3", "_5")("")
                LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                (source._1, source._2, q._3, source._3, q._5)
              case _: ((Nothing \^ Le) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._1, source._2, q._3, q._4, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._1, source._2, q._3, q._4, q._5)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head3[La, Ld, Le]("_1", "_4", "_5")("")
                LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                (source._1, source._3, source._2, q._4, q._5)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head3[La, Lc, Le]("_1", "_3", "_5")("")
                LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                (source._1, source._3, q._3, source._2, q._5)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._1, source._3, q._3, q._4, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._1, source._3, q._3, q._4, q._5)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head3[La, Lb, Le]("_1", "_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (source._1, q._2, source._2, source._3, q._5)
                  case _: ((Nothing \^ Le) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, q._2, source._2, q._4, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._1, q._2, source._2, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head3[La, Lb, Le]("_1", "_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (source._1, q._2, source._3, source._2, q._5)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._1, q._2, source._3, q._4, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._1, q._2, source._3, q._4, q._5)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                        (source._1, q._2, q._3, source._2, source._3)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._1, q._2, q._3, source._2, q._5)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                        (source._1, q._2, q._3, source._3, source._2)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._1, q._2, q._3, source._3, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[La, Le]("_1", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                        (source._1, q._2, q._3, q._4, source._2)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq2[La, Le]("_1", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                        (source._1, q._2, q._3, q._4, source._3)
                      case _ =>
                        LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                        (source._1, q._2, q._3, q._4, q._5)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head3[La, Ld, Le]("_1", "_4", "_5")("")
                LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                (source._2, source._1, source._3, q._4, q._5)
              case _: ((Nothing \^ Ld) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head3[La, Lc, Le]("_1", "_3", "_5")("")
                LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                (source._2, source._1, q._3, source._3, q._5)
              case _: ((Nothing \^ Le) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._2, source._1, q._3, q._4, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._2, source._1, q._3, q._4, q._5)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head3[La, Ld, Le]("_1", "_4", "_5")("")
                LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                (source._2, source._3, source._1, q._4, q._5)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head3[La, Lc, Le]("_1", "_3", "_5")("")
                LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                (source._2, source._3, q._3, source._1, q._5)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._2, source._3, q._3, q._4, source._1)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._2, source._3, q._3, q._4, q._5)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head3[La, Lb, Le]("_1", "_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (source._2, q._2, source._1, source._3, q._5)
                  case _: ((Nothing \^ Le) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, q._2, source._1, q._4, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._2, q._2, source._1, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head3[La, Lb, Le]("_1", "_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (source._2, q._2, source._3, source._1, q._5)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._2, q._2, source._3, q._4, source._1)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._2, q._2, source._3, q._4, q._5)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                        (source._2, q._2, q._3, source._1, source._3)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._2, q._2, q._3, source._1, q._5)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                        (source._2, q._2, q._3, source._3, source._1)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._2, q._2, q._3, source._3, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[La, Le]("_1", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                        (source._2, q._2, q._3, q._4, source._1)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq2[La, Le]("_1", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                        (source._2, q._2, q._3, q._4, source._3)
                      case _ =>
                        LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                        (source._2, q._2, q._3, q._4, q._5)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head3[La, Ld, Le]("_1", "_4", "_5")("")
                LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                (source._3, source._1, source._2, q._4, q._5)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head3[La, Lc, Le]("_1", "_3", "_5")("")
                LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                (source._3, source._1, q._3, source._2, q._5)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._3, source._1, q._3, q._4, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._3, source._1, q._3, q._4, q._5)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head3[La, Ld, Le]("_1", "_4", "_5")("")
                LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                (source._3, source._2, source._1, q._4, q._5)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head3[La, Lc, Le]("_1", "_3", "_5")("")
                LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                (source._3, source._2, q._3, source._1, q._5)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head3[La, Lc, Ld]("_1", "_3", "_4")("")
                LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                (source._3, source._2, q._3, q._4, source._1)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._3, source._2, q._3, q._4, q._5)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head3[La, Lb, Le]("_1", "_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (source._3, q._2, source._1, source._2, q._5)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, q._2, source._1, q._4, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._3, q._2, source._1, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head3[La, Lb, Le]("_1", "_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (source._3, q._2, source._2, source._1, q._5)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head3[La, Lb, Ld]("_1", "_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (source._3, q._2, source._2, q._4, source._1)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._3, q._2, source._2, q._4, q._5)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                        (source._3, q._2, q._3, source._1, source._2)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._3, q._2, q._3, source._1, q._5)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head3[La, Lb, Lc]("_1", "_2", "_3")("")
                        (source._3, q._2, q._3, source._2, source._1)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._3, q._2, q._3, source._2, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[La, Le]("_1", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                        (source._3, q._2, q._3, q._4, source._1)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[La, Le]("_1", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                        (source._3, q._2, q._3, q._4, source._2)
                      case _ =>
                        LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                        (source._3, q._2, q._3, q._4, q._5)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.diff[Lb, Le]("_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (q._1, source._1, source._2, source._3, q._5)
                  case _: ((Nothing \^ Le) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._1, source._2, q._4, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._1, source._2, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.diff[Lb, Le]("_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (q._1, source._1, source._3, source._2, q._5)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._1, source._3, q._4, source._2)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._1, source._3, q._4, q._5)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                        (q._1, source._1, q._3, source._2, source._3)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._1, q._3, source._2, q._5)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                        (q._1, source._1, q._3, source._3, source._2)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._1, q._3, source._3, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                        LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                        (q._1, source._1, q._3, q._4, source._2)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                        LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                        (q._1, source._1, q._3, q._4, source._3)
                      case _ =>
                        LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                        (q._1, source._1, q._3, q._4, q._5)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.diff[Lb, Le]("_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (q._1, source._2, source._1, source._3, q._5)
                  case _: ((Nothing \^ Le) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._2, source._1, q._4, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._2, source._1, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.diff[Lb, Le]("_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (q._1, source._2, source._3, source._1, q._5)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._2, source._3, q._4, source._1)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._2, source._3, q._4, q._5)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                        (q._1, source._2, q._3, source._1, source._3)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._2, q._3, source._1, q._5)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                        (q._1, source._2, q._3, source._3, source._1)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._2, q._3, source._3, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                        LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                        (q._1, source._2, q._3, q._4, source._1)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                        LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                        (q._1, source._2, q._3, q._4, source._3)
                      case _ =>
                        LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                        (q._1, source._2, q._3, q._4, q._5)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.diff[Lb, Le]("_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (q._1, source._3, source._1, source._2, q._5)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._3, source._1, q._4, source._2)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._3, source._1, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.diff[Lb, Le]("_2", "_5")("")
                    LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                    LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                    (q._1, source._3, source._2, source._1, q._5)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.diff[Lb, Ld]("_2", "_4")("")
                    LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                    (q._1, source._3, source._2, q._4, source._1)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._3, source._2, q._4, q._5)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                        (q._1, source._3, q._3, source._1, source._2)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._3, q._3, source._1, q._5)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.diff[Lb, Lc]("_2", "_3")("")
                        (q._1, source._3, q._3, source._2, source._1)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._3, q._3, source._2, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                        LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                        (q._1, source._3, q._3, q._4, source._1)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                        LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                        (q._1, source._3, q._3, q._4, source._2)
                      case _ =>
                        LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                        (q._1, source._3, q._3, q._4, q._5)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        (q._1, q._2, source._1, source._2, source._3)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._1, source._2, q._5)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        (q._1, q._2, source._1, source._3, source._2)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._1, source._3, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                        LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                        (q._1, q._2, source._1, q._4, source._2)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                        LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                        (q._1, q._2, source._1, q._4, source._3)
                      case _ =>
                        LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                        (q._1, q._2, source._1, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        (q._1, q._2, source._2, source._1, source._3)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._2, source._1, q._5)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        (q._1, q._2, source._2, source._3, source._1)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._2, source._3, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                        LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                        (q._1, q._2, source._2, q._4, source._1)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                        LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                        (q._1, q._2, source._2, q._4, source._3)
                      case _ =>
                        LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                        (q._1, q._2, source._2, q._4, q._5)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        (q._1, q._2, source._3, source._1, source._2)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._3, source._1, q._5)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        (q._1, q._2, source._3, source._2, source._1)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._3, source._2, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                        LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                        (q._1, q._2, source._3, q._4, source._1)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                        LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                        (q._1, q._2, source._3, q._4, source._2)
                      case _ =>
                        LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                        (q._1, q._2, source._3, q._4, q._5)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        (q._1, q._2, q._3, source._1, source._2)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        (q._1, q._2, q._3, source._1, source._3)
                      case _ =>
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, q._3, source._1, q._5)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        (q._1, q._2, q._3, source._2, source._1)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        (q._1, q._2, q._3, source._2, source._3)
                      case _ =>
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, q._3, source._2, q._5)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        (q._1, q._2, q._3, source._3, source._1)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        (q._1, q._2, q._3, source._3, source._2)
                      case _ =>
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, q._3, source._3, q._5)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        (q._1, q._2, q._3, q._4, source._1)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        (q._1, q._2, q._3, q._4, source._2)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        (q._1, q._2, q._3, q._4, source._3)
                      case _ =>
                        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw](source: (Tz, Ty, Tx, Tw)): (A \^ La | Tz | Ty | Tx | Tw, B \^ Lb | Tz | Ty | Tx | Tw, C \^ Lc | Tz | Ty | Tx | Tw, D \^ Ld | Tz | Ty | Tx | Tw, E \^ Le | Tz | Ty | Tx | Tw) =
  compiletime.error("Implementation restrictions prevent name-update of 5-tuple by 4-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv](source: (Tz, Ty, Tx, Tw, Tv)): (A \^ La | Tz | Ty | Tx | Tw | Tv, B \^ Lb | Tz | Ty | Tx | Tw | Tv, C \^ Lc | Tz | Ty | Tx | Tw | Tv, D \^ Ld | Tz | Ty | Tx | Tw | Tv, E \^ Le | Tz | Ty | Tx | Tw | Tv) =
  compiletime.error("Implementation restrictions prevent name-update of 5-tuple by 5-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu](source: (Tz, Ty, Tx, Tw, Tv, Tu)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu) =
  compiletime.error("Implementation restrictions prevent name-update of 5-tuple by 6-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt) =
  compiletime.error("Implementation restrictions prevent name-update of 5-tuple by 7-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts) =
  compiletime.error("Implementation restrictions prevent name-update of 5-tuple by 8-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr) =
  compiletime.error("Implementation restrictions prevent name-update of 5-tuple by 9-tuple")

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

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf): (A, B, C, D, E, F) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): ((A | B | C | D | E | F) \^ Lz) =
    summonFrom:
      case _: (Lz =:= La) =>
        LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
        q._1.asInstanceOf[(A \^ Lz)]
      case _: (Lz =:= Lb) =>
        LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
        q._2.asInstanceOf[(B \^ Lz)]
      case _: (Lz =:= Lc) =>
        LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
        q._3.asInstanceOf[(C \^ Lz)]
      case _: (Lz =:= Ld) =>
        LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
        q._4.asInstanceOf[(D \^ Lz)]
      case _: (Lz =:= Le) =>
        LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
        q._5.asInstanceOf[(E \^ Lz)]
      case _: (Lz =:= Lf) =>
        q._6.asInstanceOf[(F \^ Lz)]
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly) =
    LabelConflicts.unik2(lz, ly)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._1.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head5[Lz, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._1.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head5[Lz, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
            (q._1.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head5[Lz, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
            (q._1.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head5[Lz, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(lz))
            (q._1.unlabel, q._6.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._2.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head4[Lz, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._2.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head4[Lz, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
            (q._2.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head4[Lz, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
            (q._2.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head4[Lz, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lz))
            (q._2.unlabel, q._6.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._3.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._3.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head3[Lz, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
            (q._3.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head3[Lz, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
            (q._3.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
            (q._3.unlabel, q._6.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")(" with " + codeOf(ly))
            (q._4.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(ly))
            (q._4.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(ly))
            (q._4.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
            (q._4.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
            (q._4.unlabel, q._6.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")(" with " + codeOf(ly))
            (q._5.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(ly))
            (q._5.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(ly))
            (q._5.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
            (q._5.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            (q._5.unlabel, q._6.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lf) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head5[Ly, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(ly))
            (q._6.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head4[Ly, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(ly))
            (q._6.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
            (q._6.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
            (q._6.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            (q._6.unlabel, q._5.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly, (A | B | C | D | E | F) \^ Lx) =
    LabelConflicts.unik3(lz, ly, lx)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_1", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Lc, Le, Lf]("_1", "_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Lc, Ld, Lf]("_1", "_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Lc, Ld, Le]("_1", "_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(ly))
                (q._1.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_1", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Lb, Le, Lf]("_1", "_2", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Lb, Ld, Lf]("_1", "_2", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Lb, Ld, Le]("_1", "_2", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
                (q._1.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Lc, Le, Lf]("_1", "_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Lb, Le, Lf]("_1", "_2", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Lb, Lc, Lf]("_1", "_2", "_3", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Lb, Lc, Le]("_1", "_2", "_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                (q._1.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Lc, Ld, Lf]("_1", "_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Lb, Ld, Lf]("_1", "_2", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Lb, Lc, Lf]("_1", "_2", "_3", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_4", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(lz))
                (q._1.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Lc, Ld, Le]("_1", "_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Lb, Ld, Le]("_1", "_2", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Lb, Lc, Le]("_1", "_2", "_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._1.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(lz))
                (q._1.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_1", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Le, Lf]("_1", "_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Lf]("_1", "_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Le]("_1", "_3", "_4", "_5")(" with " + codeOf(ly))
                (q._2.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_1", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Le, Lf]("_2", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Ld, Lf]("_2", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
                (q._2.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Le, Lf]("_1", "_3", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Le, Lf]("_2", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lc, Lf]("_2", "_3", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                (q._2.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Lf]("_1", "_3", "_4", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Ld, Lf]("_2", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lc, Lf]("_2", "_3", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_4", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lz))
                (q._2.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Lc, Ld, Le]("_1", "_3", "_4", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._2.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lz))
                (q._2.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_1", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lb, Le, Lf]("_1", "_2", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lb, Ld, Lf]("_1", "_2", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lb, Ld, Le]("_1", "_2", "_4", "_5")(" with " + codeOf(ly))
                (q._3.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_1", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_2", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lf]("_2", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(ly))
                (q._3.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lb, Le, Lf]("_1", "_2", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_2", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lf]("_3", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                (q._3.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lb, Ld, Lf]("_1", "_2", "_4", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lf]("_2", "_4", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lf]("_3", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_4", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                (q._3.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Lb, Ld, Le]("_1", "_2", "_4", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Le]("_3", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._3.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Ld]("_3", "_4")(" with " + codeOf(lz))
                (q._3.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Le, Lf]("_1", "_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lb, Le, Lf]("_1", "_2", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lb, Lc, Lf]("_1", "_2", "_3", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lb, Lc, Le]("_1", "_2", "_3", "_5")(" with " + codeOf(ly))
                (q._4.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Le, Lf]("_1", "_3", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_2", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Lf]("_2", "_3", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(ly))
                (q._4.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lb, Le, Lf]("_1", "_2", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_2", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_3", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                (q._4.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lb, Lc, Lf]("_1", "_2", "_3", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Lf]("_2", "_3", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_3", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                (q._4.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Lb, Lc, Le]("_1", "_2", "_3", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                (q._4.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                (q._4.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Lf]("_1", "_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lb, Ld, Lf]("_1", "_2", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lb, Lc, Lf]("_1", "_2", "_3", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Ly, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(ly))
                (q._5.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Lf]("_1", "_3", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lf]("_2", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lc, Lf]("_2", "_3", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Ly, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(ly))
                (q._5.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lb, Ld, Lf]("_1", "_2", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lf]("_2", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_3", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                (q._5.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lb, Lc, Lf]("_1", "_2", "_3", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Lf]("_2", "_3", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_3", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                (q._5.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lx, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lx, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                (q._5.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                (q._5.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lf) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Ly, Lc, Ld, Le]("_1", "_3", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Ly, Lb, Ld, Le]("_1", "_2", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Ly, Lb, Lc, Le]("_1", "_2", "_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Ly, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(ly))
                (q._6.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Ly, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Le]("_1", "_3", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Ly, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Ly, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Ly, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(ly))
                (q._6.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lb, Ld, Le]("_1", "_2", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Le]("_2", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Ly, Le]("_3", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Ly, Ld]("_3", "_4")(" with " + codeOf(ly))
                (q._6.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lb, Lc, Le]("_1", "_2", "_3", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lc, Le]("_2", "_3", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Le]("_3", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                (q._6.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lx, Lb, Lc, Ld]("_1", "_2", "_3", "_4")(" with " + codeOf(lx))
                (q._6.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lx, Lc, Ld]("_2", "_3", "_4")(" with " + codeOf(lx))
                (q._6.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lx, Ld]("_3", "_4")(" with " + codeOf(lx))
                (q._6.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                (q._6.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly, (A | B | C | D | E | F) \^ Lx, (A | B | C | D | E | F) \^ Lw) =
    compiletime.error("Implementation restrictions prevent 4-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly, (A | B | C | D | E | F) \^ Lx, (A | B | C | D | E | F) \^ Lw, (A | B | C | D | E | F) \^ Lv) =
    compiletime.error("Implementation restrictions prevent 5-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu): ((A | B | C | D | E | F) \^ Lz, (A | B | C | D | E | F) \^ Ly, (A | B | C | D | E | F) \^ Lx, (A | B | C | D | E | F) \^ Lw, (A | B | C | D | E | F) \^ Lv, (A | B | C | D | E | F) \^ Lu) =
    compiletime.error("Implementation restrictions prevent 6-way picking by name")

  transparent inline def updatedBy[Tz](source: Tz): (A \^ La | Tz, B \^ Lb | Tz, C \^ Lc | Tz, D \^ Ld | Tz, E \^ Le | Tz, F \^ Lf | Tz) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => 
        LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")("")
        (source, q._2, q._3, q._4, q._5, q._6)
      case _: ((Nothing \^ Lb) <:< Tz) => 
        LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
        (q._1, source, q._3, q._4, q._5, q._6)
      case _: ((Nothing \^ Lc) <:< Tz) => 
        LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
        (q._1, q._2, source, q._4, q._5, q._6)
      case _: ((Nothing \^ Ld) <:< Tz) => 
        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
        (q._1, q._2, q._3, source, q._5, q._6)
      case _: ((Nothing \^ Le) <:< Tz) => 
        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
        (q._1, q._2, q._3, q._4, source, q._6)
      case _: ((Nothing \^ Lf) <:< Tz) => 
        (q._1, q._2, q._3, q._4, q._5, source)
      case _ =>
        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty](source: (Tz, Ty)): (A \^ La | Tz | Ty, B \^ Lb | Tz | Ty, C \^ Lc | Tz | Ty, D \^ Ld | Tz | Ty, E \^ Le | Tz | Ty, F \^ Lf | Tz | Ty) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head5[La, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")("")
            LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
            (source._1, source._2, q._3, q._4, q._5, q._6)
          case _: ((Nothing \^ Lc) <:< Ty) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head5[La, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")("")
            LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
            (source._1, q._2, source._2, q._4, q._5, q._6)
          case _: ((Nothing \^ Ld) <:< Ty) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head5[La, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")("")
            LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
            (source._1, q._2, q._3, source._2, q._5, q._6)
          case _: ((Nothing \^ Le) <:< Ty) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head5[La, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")("")
            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
            (source._1, q._2, q._3, q._4, source._2, q._6)
          case _: ((Nothing \^ Lf) <:< Ty) => 
            LabelConflicts.unyq2[La, Lf]("_1", "_6")
            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
            (source._1, q._2, q._3, q._4, q._5, source._2)
          case _ =>
            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")("")
            (source._1, q._2, q._3, q._4, q._5, q._6)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head5[La, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")("")
            LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
            (source._2, source._1, q._3, q._4, q._5, q._6)
          case _: ((Nothing \^ Lc) <:< Tz) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head5[La, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")("")
            LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
            (source._2, q._2, source._1, q._4, q._5, q._6)
          case _: ((Nothing \^ Ld) <:< Tz) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head5[La, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")("")
            LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
            (source._2, q._2, q._3, source._1, q._5, q._6)
          case _: ((Nothing \^ Le) <:< Tz) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head5[La, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")("")
            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
            (source._2, q._2, q._3, q._4, source._1, q._6)
          case _: ((Nothing \^ Lf) <:< Tz) => 
            LabelConflicts.unyq2[La, Lf]("_1", "_6")
            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
            (source._2, q._2, q._3, q._4, q._5, source._1)
          case _ =>
            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")("")
            (source._2, q._2, q._3, q._4, q._5, q._6)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                (q._1, source._1, source._2, q._4, q._5, q._6)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                (q._1, source._1, q._3, source._2, q._5, q._6)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                (q._1, source._1, q._3, q._4, source._2, q._6)
              case _: ((Nothing \^ Lf) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (q._1, source._1, q._3, q._4, q._5, source._2)
              case _ =>
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (q._1, source._1, q._3, q._4, q._5, q._6)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                (q._1, source._2, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                (q._1, source._2, q._3, source._1, q._5, q._6)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                (q._1, source._2, q._3, q._4, source._1, q._6)
              case _: ((Nothing \^ Lf) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (q._1, source._2, q._3, q._4, q._5, source._1)
              case _ =>
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (q._1, source._2, q._3, q._4, q._5, q._6)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (q._1, q._2, source._1, source._2, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (q._1, q._2, source._1, q._4, source._2, q._6)
                  case _: ((Nothing \^ Lf) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, q._2, source._1, q._4, q._5, source._2)
                  case _ =>
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (q._1, q._2, source._2, source._1, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (q._1, q._2, source._2, q._4, source._1, q._6)
                  case _: ((Nothing \^ Lf) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, q._2, source._2, q._4, q._5, source._1)
                  case _ =>
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, q._2, q._3, source._1, source._2, q._6)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, q._3, source._1, q._5, source._2)
                      case _ =>
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, q._2, q._3, source._2, source._1, q._6)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, q._3, source._2, q._5, source._1)
                      case _ =>
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            (q._1, q._2, q._3, q._4, source._1, source._2)
                          case _ =>
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            (q._1, q._2, q._3, q._4, source._2, source._1)
                          case _ =>
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            (q._1, q._2, q._3, q._4, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            (q._1, q._2, q._3, q._4, q._5, source._2)
                          case _ =>
                            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx](source: (Tz, Ty, Tx)): (A \^ La | Tz | Ty | Tx, B \^ Lb | Tz | Ty | Tx, C \^ Lc | Tz | Ty | Tx, D \^ Ld | Tz | Ty | Tx, E \^ Le | Tz | Ty | Tx, F \^ Lf | Tz | Ty | Tx) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head4[La, Ld, Le, Lf]("_1", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                (source._1, source._2, source._3, q._4, q._5, q._6)
              case _: ((Nothing \^ Ld) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head4[La, Lc, Le, Lf]("_1", "_3", "_5", "_6")("")
                LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                (source._1, source._2, q._3, source._3, q._5, q._6)
              case _: ((Nothing \^ Le) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head4[La, Lc, Ld, Lf]("_1", "_3", "_4", "_6")("")
                LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                (source._1, source._2, q._3, q._4, source._3, q._6)
              case _: ((Nothing \^ Lf) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Lf]("_1", "_2", "_6")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._1, source._2, q._3, q._4, q._5, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head5[La, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")("")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (source._1, source._2, q._3, q._4, q._5, q._6)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head4[La, Ld, Le, Lf]("_1", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                (source._1, source._3, source._2, q._4, q._5, q._6)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head4[La, Lc, Le, Lf]("_1", "_3", "_5", "_6")("")
                LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                (source._1, source._3, q._3, source._2, q._5, q._6)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head4[La, Lc, Ld, Lf]("_1", "_3", "_4", "_6")("")
                LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                (source._1, source._3, q._3, q._4, source._2, q._6)
              case _: ((Nothing \^ Lf) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Lf]("_1", "_2", "_6")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._1, source._3, q._3, q._4, q._5, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head5[La, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")("")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (source._1, source._3, q._3, q._4, q._5, q._6)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head4[La, Lb, Le, Lf]("_1", "_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (source._1, q._2, source._2, source._3, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head4[La, Lb, Ld, Lf]("_1", "_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (source._1, q._2, source._2, q._4, source._3, q._6)
                  case _: ((Nothing \^ Lf) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Lf]("_1", "_3", "_6")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._1, q._2, source._2, q._4, q._5, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head5[La, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (source._1, q._2, source._2, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head4[La, Lb, Le, Lf]("_1", "_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (source._1, q._2, source._3, source._2, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head4[La, Lb, Ld, Lf]("_1", "_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (source._1, q._2, source._3, q._4, source._2, q._6)
                  case _: ((Nothing \^ Lf) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Lf]("_1", "_3", "_6")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._1, q._2, source._3, q._4, q._5, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head5[La, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (source._1, q._2, source._3, q._4, q._5, q._6)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) => 
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Lf]("_1", "_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (source._1, q._2, q._3, source._2, source._3, q._6)
                      case _: ((Nothing \^ Lf) <:< Tx) => 
                        LabelConflicts.unyq3[La, Ld, Lf]("_1", "_4", "_6")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._1, q._2, q._3, source._2, q._5, source._3)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head5[La, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (source._1, q._2, q._3, source._2, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Lf]("_1", "_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (source._1, q._2, q._3, source._3, source._2, q._6)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq3[La, Ld, Lf]("_1", "_4", "_6")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._1, q._2, q._3, source._3, q._5, source._2)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head5[La, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (source._1, q._2, q._3, source._3, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq3[La, Le, Lf]("_1", "_5", "_6")
                            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                            (source._1, q._2, q._3, q._4, source._2, source._3)
                          case _ =>
                            LabelConflicts.unyq2[La, Le]("_1", "_5")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (source._1, q._2, q._3, q._4, source._2, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq3[La, Le, Lf]("_1", "_5", "_6")
                            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                            (source._1, q._2, q._3, q._4, source._3, source._2)
                          case _ =>
                            LabelConflicts.unyq2[La, Le]("_1", "_5")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (source._1, q._2, q._3, q._4, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[La, Lf]("_1", "_6")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                            (source._1, q._2, q._3, q._4, q._5, source._2)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[La, Lf]("_1", "_6")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                            (source._1, q._2, q._3, q._4, q._5, source._3)
                          case _ =>
                            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")("")
                            (source._1, q._2, q._3, q._4, q._5, q._6)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head4[La, Ld, Le, Lf]("_1", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                (source._2, source._1, source._3, q._4, q._5, q._6)
              case _: ((Nothing \^ Ld) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head4[La, Lc, Le, Lf]("_1", "_3", "_5", "_6")("")
                LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                (source._2, source._1, q._3, source._3, q._5, q._6)
              case _: ((Nothing \^ Le) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head4[La, Lc, Ld, Lf]("_1", "_3", "_4", "_6")("")
                LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                (source._2, source._1, q._3, q._4, source._3, q._6)
              case _: ((Nothing \^ Lf) <:< Tx) => 
                LabelConflicts.unyq3[La, Lb, Lf]("_1", "_2", "_6")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._2, source._1, q._3, q._4, q._5, source._3)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head5[La, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")("")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (source._2, source._1, q._3, q._4, q._5, q._6)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head4[La, Ld, Le, Lf]("_1", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                (source._2, source._3, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head4[La, Lc, Le, Lf]("_1", "_3", "_5", "_6")("")
                LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                (source._2, source._3, q._3, source._1, q._5, q._6)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head4[La, Lc, Ld, Lf]("_1", "_3", "_4", "_6")("")
                LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                (source._2, source._3, q._3, q._4, source._1, q._6)
              case _: ((Nothing \^ Lf) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Lf]("_1", "_2", "_6")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._2, source._3, q._3, q._4, q._5, source._1)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head5[La, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")("")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (source._2, source._3, q._3, q._4, q._5, q._6)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head4[La, Lb, Le, Lf]("_1", "_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (source._2, q._2, source._1, source._3, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head4[La, Lb, Ld, Lf]("_1", "_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (source._2, q._2, source._1, q._4, source._3, q._6)
                  case _: ((Nothing \^ Lf) <:< Tx) => 
                    LabelConflicts.unyq3[La, Lc, Lf]("_1", "_3", "_6")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._2, q._2, source._1, q._4, q._5, source._3)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head5[La, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (source._2, q._2, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head4[La, Lb, Le, Lf]("_1", "_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (source._2, q._2, source._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head4[La, Lb, Ld, Lf]("_1", "_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (source._2, q._2, source._3, q._4, source._1, q._6)
                  case _: ((Nothing \^ Lf) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Lf]("_1", "_3", "_6")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._2, q._2, source._3, q._4, q._5, source._1)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head5[La, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (source._2, q._2, source._3, q._4, q._5, q._6)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) => 
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Lf]("_1", "_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (source._2, q._2, q._3, source._1, source._3, q._6)
                      case _: ((Nothing \^ Lf) <:< Tx) => 
                        LabelConflicts.unyq3[La, Ld, Lf]("_1", "_4", "_6")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._2, q._2, q._3, source._1, q._5, source._3)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head5[La, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (source._2, q._2, q._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Lf]("_1", "_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (source._2, q._2, q._3, source._3, source._1, q._6)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq3[La, Ld, Lf]("_1", "_4", "_6")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._2, q._2, q._3, source._3, q._5, source._1)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head5[La, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (source._2, q._2, q._3, source._3, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq3[La, Le, Lf]("_1", "_5", "_6")
                            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                            (source._2, q._2, q._3, q._4, source._1, source._3)
                          case _ =>
                            LabelConflicts.unyq2[La, Le]("_1", "_5")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (source._2, q._2, q._3, q._4, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq3[La, Le, Lf]("_1", "_5", "_6")
                            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                            (source._2, q._2, q._3, q._4, source._3, source._1)
                          case _ =>
                            LabelConflicts.unyq2[La, Le]("_1", "_5")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (source._2, q._2, q._3, q._4, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[La, Lf]("_1", "_6")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                            (source._2, q._2, q._3, q._4, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[La, Lf]("_1", "_6")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                            (source._2, q._2, q._3, q._4, q._5, source._3)
                          case _ =>
                            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")("")
                            (source._2, q._2, q._3, q._4, q._5, q._6)
      case _: ((Nothing \^ La) <:< Tx) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head4[La, Ld, Le, Lf]("_1", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                (source._3, source._1, source._2, q._4, q._5, q._6)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head4[La, Lc, Le, Lf]("_1", "_3", "_5", "_6")("")
                LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                (source._3, source._1, q._3, source._2, q._5, q._6)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head4[La, Lc, Ld, Lf]("_1", "_3", "_4", "_6")("")
                LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                (source._3, source._1, q._3, q._4, source._2, q._6)
              case _: ((Nothing \^ Lf) <:< Ty) => 
                LabelConflicts.unyq3[La, Lb, Lf]("_1", "_2", "_6")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._3, source._1, q._3, q._4, q._5, source._2)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head5[La, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")("")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (source._3, source._1, q._3, q._4, q._5, q._6)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Lc]("_1", "_2", "_3")
                LabelConflicts.head4[La, Ld, Le, Lf]("_1", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                (source._3, source._2, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Ld]("_1", "_2", "_4")
                LabelConflicts.head4[La, Lc, Le, Lf]("_1", "_3", "_5", "_6")("")
                LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                (source._3, source._2, q._3, source._1, q._5, q._6)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Le]("_1", "_2", "_5")
                LabelConflicts.head4[La, Lc, Ld, Lf]("_1", "_3", "_4", "_6")("")
                LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                (source._3, source._2, q._3, q._4, source._1, q._6)
              case _: ((Nothing \^ Lf) <:< Tz) => 
                LabelConflicts.unyq3[La, Lb, Lf]("_1", "_2", "_6")
                LabelConflicts.head4[La, Lc, Ld, Le]("_1", "_3", "_4", "_5")("")
                LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                (source._3, source._2, q._3, q._4, q._5, source._1)
              case _ =>
                LabelConflicts.unyq2[La, Lb]("_1", "_2")
                LabelConflicts.head5[La, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")("")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (source._3, source._2, q._3, q._4, q._5, q._6)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head4[La, Lb, Le, Lf]("_1", "_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (source._3, q._2, source._1, source._2, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head4[La, Lb, Ld, Lf]("_1", "_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (source._3, q._2, source._1, q._4, source._2, q._6)
                  case _: ((Nothing \^ Lf) <:< Ty) => 
                    LabelConflicts.unyq3[La, Lc, Lf]("_1", "_3", "_6")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._3, q._2, source._1, q._4, q._5, source._2)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head5[La, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (source._3, q._2, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Ld]("_1", "_3", "_4")
                    LabelConflicts.head4[La, Lb, Le, Lf]("_1", "_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (source._3, q._2, source._2, source._1, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Le]("_1", "_3", "_5")
                    LabelConflicts.head4[La, Lb, Ld, Lf]("_1", "_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (source._3, q._2, source._2, q._4, source._1, q._6)
                  case _: ((Nothing \^ Lf) <:< Tz) => 
                    LabelConflicts.unyq3[La, Lc, Lf]("_1", "_3", "_6")
                    LabelConflicts.head4[La, Lb, Ld, Le]("_1", "_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (source._3, q._2, source._2, q._4, q._5, source._1)
                  case _ =>
                    LabelConflicts.unyq2[La, Lc]("_1", "_3")
                    LabelConflicts.head5[La, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (source._3, q._2, source._2, q._4, q._5, q._6)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Lf]("_1", "_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (source._3, q._2, q._3, source._1, source._2, q._6)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq3[La, Ld, Lf]("_1", "_4", "_6")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._3, q._2, q._3, source._1, q._5, source._2)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head5[La, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (source._3, q._2, q._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq3[La, Ld, Le]("_1", "_4", "_5")
                        LabelConflicts.head4[La, Lb, Lc, Lf]("_1", "_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (source._3, q._2, q._3, source._2, source._1, q._6)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq3[La, Ld, Lf]("_1", "_4", "_6")
                        LabelConflicts.head4[La, Lb, Lc, Le]("_1", "_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (source._3, q._2, q._3, source._2, q._5, source._1)
                      case _ =>
                        LabelConflicts.unyq2[La, Ld]("_1", "_4")
                        LabelConflicts.head5[La, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (source._3, q._2, q._3, source._2, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq3[La, Le, Lf]("_1", "_5", "_6")
                            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                            (source._3, q._2, q._3, q._4, source._1, source._2)
                          case _ =>
                            LabelConflicts.unyq2[La, Le]("_1", "_5")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (source._3, q._2, q._3, q._4, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq3[La, Le, Lf]("_1", "_5", "_6")
                            LabelConflicts.head4[La, Lb, Lc, Ld]("_1", "_2", "_3", "_4")("")
                            (source._3, q._2, q._3, q._4, source._2, source._1)
                          case _ =>
                            LabelConflicts.unyq2[La, Le]("_1", "_5")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (source._3, q._2, q._3, q._4, source._2, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[La, Lf]("_1", "_6")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                            (source._3, q._2, q._3, q._4, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[La, Lf]("_1", "_6")
                            LabelConflicts.head5[La, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")("")
                            (source._3, q._2, q._3, q._4, q._5, source._2)
                          case _ =>
                            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")("")
                            (source._3, q._2, q._3, q._4, q._5, q._6)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.head3[Lb, Le, Lf]("_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (q._1, source._1, source._2, source._3, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.head3[Lb, Ld, Lf]("_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (q._1, source._1, source._2, q._4, source._3, q._6)
                  case _: ((Nothing \^ Lf) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Lf]("_2", "_3", "_6")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._1, source._2, q._4, q._5, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, source._1, source._2, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.head3[Lb, Le, Lf]("_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (q._1, source._1, source._3, source._2, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.head3[Lb, Ld, Lf]("_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (q._1, source._1, source._3, q._4, source._2, q._6)
                  case _: ((Nothing \^ Lf) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Lf]("_2", "_3", "_6")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._1, source._3, q._4, q._5, source._2)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, source._1, source._3, q._4, q._5, q._6)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) => 
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.head3[Lb, Lc, Lf]("_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, source._1, q._3, source._2, source._3, q._6)
                      case _: ((Nothing \^ Lf) <:< Tx) => 
                        LabelConflicts.unyq3[Lb, Ld, Lf]("_2", "_4", "_6")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._1, q._3, source._2, q._5, source._3)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, source._1, q._3, source._2, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.head3[Lb, Lc, Lf]("_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, source._1, q._3, source._3, source._2, q._6)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq3[Lb, Ld, Lf]("_2", "_4", "_6")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._1, q._3, source._3, q._5, source._2)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, source._1, q._3, source._3, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq3[Lb, Le, Lf]("_2", "_5", "_6")
                            LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                            (q._1, source._1, q._3, q._4, source._2, source._3)
                          case _ =>
                            LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                            LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, source._1, q._3, q._4, source._2, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq3[Lb, Le, Lf]("_2", "_5", "_6")
                            LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                            (q._1, source._1, q._3, q._4, source._3, source._2)
                          case _ =>
                            LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                            LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, source._1, q._3, q._4, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                            LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                            (q._1, source._1, q._3, q._4, q._5, source._2)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                            LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                            (q._1, source._1, q._3, q._4, q._5, source._3)
                          case _ =>
                            LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                            (q._1, source._1, q._3, q._4, q._5, q._6)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.head3[Lb, Le, Lf]("_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (q._1, source._2, source._1, source._3, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.head3[Lb, Ld, Lf]("_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (q._1, source._2, source._1, q._4, source._3, q._6)
                  case _: ((Nothing \^ Lf) <:< Tx) => 
                    LabelConflicts.unyq3[Lb, Lc, Lf]("_2", "_3", "_6")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._2, source._1, q._4, q._5, source._3)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, source._2, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.head3[Lb, Le, Lf]("_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (q._1, source._2, source._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.head3[Lb, Ld, Lf]("_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (q._1, source._2, source._3, q._4, source._1, q._6)
                  case _: ((Nothing \^ Lf) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Lf]("_2", "_3", "_6")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._2, source._3, q._4, q._5, source._1)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, source._2, source._3, q._4, q._5, q._6)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) => 
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.head3[Lb, Lc, Lf]("_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, source._2, q._3, source._1, source._3, q._6)
                      case _: ((Nothing \^ Lf) <:< Tx) => 
                        LabelConflicts.unyq3[Lb, Ld, Lf]("_2", "_4", "_6")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._2, q._3, source._1, q._5, source._3)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, source._2, q._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.head3[Lb, Lc, Lf]("_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, source._2, q._3, source._3, source._1, q._6)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq3[Lb, Ld, Lf]("_2", "_4", "_6")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._2, q._3, source._3, q._5, source._1)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, source._2, q._3, source._3, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq3[Lb, Le, Lf]("_2", "_5", "_6")
                            LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                            (q._1, source._2, q._3, q._4, source._1, source._3)
                          case _ =>
                            LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                            LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, source._2, q._3, q._4, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq3[Lb, Le, Lf]("_2", "_5", "_6")
                            LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                            (q._1, source._2, q._3, q._4, source._3, source._1)
                          case _ =>
                            LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                            LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, source._2, q._3, q._4, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                            LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                            (q._1, source._2, q._3, q._4, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                            LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                            (q._1, source._2, q._3, q._4, q._5, source._3)
                          case _ =>
                            LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                            (q._1, source._2, q._3, q._4, q._5, q._6)
          case _: ((Nothing \^ Lb) <:< Tx) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.head3[Lb, Le, Lf]("_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (q._1, source._3, source._1, source._2, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.head3[Lb, Ld, Lf]("_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (q._1, source._3, source._1, q._4, source._2, q._6)
                  case _: ((Nothing \^ Lf) <:< Ty) => 
                    LabelConflicts.unyq3[Lb, Lc, Lf]("_2", "_3", "_6")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._3, source._1, q._4, q._5, source._2)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, source._3, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Ld]("_2", "_3", "_4")
                    LabelConflicts.head3[Lb, Le, Lf]("_2", "_5", "_6")("")
                    LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                    LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                    (q._1, source._3, source._2, source._1, q._5, q._6)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Le]("_2", "_3", "_5")
                    LabelConflicts.head3[Lb, Ld, Lf]("_2", "_4", "_6")("")
                    LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                    LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                    (q._1, source._3, source._2, q._4, source._1, q._6)
                  case _: ((Nothing \^ Lf) <:< Tz) => 
                    LabelConflicts.unyq3[Lb, Lc, Lf]("_2", "_3", "_6")
                    LabelConflicts.head3[Lb, Ld, Le]("_2", "_4", "_5")("")
                    LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                    (q._1, source._3, source._2, q._4, q._5, source._1)
                  case _ =>
                    LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                    LabelConflicts.head4[Lb, Ld, Le, Lf]("_2", "_4", "_5", "_6")("")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, source._3, source._2, q._4, q._5, q._6)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.head3[Lb, Lc, Lf]("_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, source._3, q._3, source._1, source._2, q._6)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq3[Lb, Ld, Lf]("_2", "_4", "_6")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._3, q._3, source._1, q._5, source._2)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, source._3, q._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq3[Lb, Ld, Le]("_2", "_4", "_5")
                        LabelConflicts.head3[Lb, Lc, Lf]("_2", "_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, source._3, q._3, source._2, source._1, q._6)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq3[Lb, Ld, Lf]("_2", "_4", "_6")
                        LabelConflicts.head3[Lb, Lc, Le]("_2", "_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, source._3, q._3, source._2, q._5, source._1)
                      case _ =>
                        LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                        LabelConflicts.head4[Lb, Lc, Le, Lf]("_2", "_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, source._3, q._3, source._2, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq3[Lb, Le, Lf]("_2", "_5", "_6")
                            LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                            (q._1, source._3, q._3, q._4, source._1, source._2)
                          case _ =>
                            LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                            LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, source._3, q._3, q._4, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq3[Lb, Le, Lf]("_2", "_5", "_6")
                            LabelConflicts.head3[Lb, Lc, Ld]("_2", "_3", "_4")("")
                            (q._1, source._3, q._3, q._4, source._2, source._1)
                          case _ =>
                            LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                            LabelConflicts.head4[Lb, Lc, Ld, Lf]("_2", "_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, source._3, q._3, q._4, source._2, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                            LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                            (q._1, source._3, q._3, q._4, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                            LabelConflicts.head4[Lb, Lc, Ld, Le]("_2", "_3", "_4", "_5")("")
                            (q._1, source._3, q._3, q._4, q._5, source._2)
                          case _ =>
                            LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                            (q._1, source._3, q._3, q._4, q._5, q._6)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) => 
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        LabelConflicts.diff[Lc, Lf]("_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, q._2, source._1, source._2, source._3, q._6)
                      case _: ((Nothing \^ Lf) <:< Tx) => 
                        LabelConflicts.unyq3[Lc, Ld, Lf]("_3", "_4", "_6")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._1, source._2, q._5, source._3)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, source._1, source._2, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        LabelConflicts.diff[Lc, Lf]("_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, q._2, source._1, source._3, source._2, q._6)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq3[Lc, Ld, Lf]("_3", "_4", "_6")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._1, source._3, q._5, source._2)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, source._1, source._3, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq3[Lc, Le, Lf]("_3", "_5", "_6")
                            LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                            (q._1, q._2, source._1, q._4, source._2, source._3)
                          case _ =>
                            LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                            LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, source._1, q._4, source._2, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq3[Lc, Le, Lf]("_3", "_5", "_6")
                            LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                            (q._1, q._2, source._1, q._4, source._3, source._2)
                          case _ =>
                            LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                            LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, source._1, q._4, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                            LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                            (q._1, q._2, source._1, q._4, q._5, source._2)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                            LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                            (q._1, q._2, source._1, q._4, q._5, source._3)
                          case _ =>
                            LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                            (q._1, q._2, source._1, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tx) => 
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        LabelConflicts.diff[Lc, Lf]("_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, q._2, source._2, source._1, source._3, q._6)
                      case _: ((Nothing \^ Lf) <:< Tx) => 
                        LabelConflicts.unyq3[Lc, Ld, Lf]("_3", "_4", "_6")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._2, source._1, q._5, source._3)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, source._2, source._1, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        LabelConflicts.diff[Lc, Lf]("_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, q._2, source._2, source._3, source._1, q._6)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq3[Lc, Ld, Lf]("_3", "_4", "_6")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._2, source._3, q._5, source._1)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, source._2, source._3, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq3[Lc, Le, Lf]("_3", "_5", "_6")
                            LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                            (q._1, q._2, source._2, q._4, source._1, source._3)
                          case _ =>
                            LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                            LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, source._2, q._4, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq3[Lc, Le, Lf]("_3", "_5", "_6")
                            LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                            (q._1, q._2, source._2, q._4, source._3, source._1)
                          case _ =>
                            LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                            LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, source._2, q._4, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                            LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                            (q._1, q._2, source._2, q._4, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                            LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                            (q._1, q._2, source._2, q._4, q._5, source._3)
                          case _ =>
                            LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                            (q._1, q._2, source._2, q._4, q._5, q._6)
              case _: ((Nothing \^ Lc) <:< Tx) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        LabelConflicts.diff[Lc, Lf]("_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, q._2, source._3, source._1, source._2, q._6)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq3[Lc, Ld, Lf]("_3", "_4", "_6")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._3, source._1, q._5, source._2)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, source._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq3[Lc, Ld, Le]("_3", "_4", "_5")
                        LabelConflicts.diff[Lc, Lf]("_3", "_6")("")
                        LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                        LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                        (q._1, q._2, source._3, source._2, source._1, q._6)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq3[Lc, Ld, Lf]("_3", "_4", "_6")
                        LabelConflicts.diff[Lc, Le]("_3", "_5")("")
                        LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                        (q._1, q._2, source._3, source._2, q._5, source._1)
                      case _ =>
                        LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                        LabelConflicts.head3[Lc, Le, Lf]("_3", "_5", "_6")("")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, source._3, source._2, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq3[Lc, Le, Lf]("_3", "_5", "_6")
                            LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                            (q._1, q._2, source._3, q._4, source._1, source._2)
                          case _ =>
                            LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                            LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, source._3, q._4, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq3[Lc, Le, Lf]("_3", "_5", "_6")
                            LabelConflicts.diff[Lc, Ld]("_3", "_4")("")
                            (q._1, q._2, source._3, q._4, source._2, source._1)
                          case _ =>
                            LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                            LabelConflicts.head3[Lc, Ld, Lf]("_3", "_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, source._3, q._4, source._2, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                            LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                            (q._1, q._2, source._3, q._4, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                            LabelConflicts.head3[Lc, Ld, Le]("_3", "_4", "_5")("")
                            (q._1, q._2, source._3, q._4, q._5, source._2)
                          case _ =>
                            LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                            (q._1, q._2, source._3, q._4, q._5, q._6)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq3[Ld, Le, Lf]("_4", "_5", "_6")
                            (q._1, q._2, q._3, source._1, source._2, source._3)
                          case _ =>
                            LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                            LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, source._1, source._2, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq3[Ld, Le, Lf]("_4", "_5", "_6")
                            (q._1, q._2, q._3, source._1, source._3, source._2)
                          case _ =>
                            LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                            LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, source._1, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                            LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                            (q._1, q._2, q._3, source._1, q._5, source._2)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                            LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                            (q._1, q._2, q._3, source._1, q._5, source._3)
                          case _ =>
                            LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                            (q._1, q._2, q._3, source._1, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq3[Ld, Le, Lf]("_4", "_5", "_6")
                            (q._1, q._2, q._3, source._2, source._1, source._3)
                          case _ =>
                            LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                            LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, source._2, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq3[Ld, Le, Lf]("_4", "_5", "_6")
                            (q._1, q._2, q._3, source._2, source._3, source._1)
                          case _ =>
                            LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                            LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, source._2, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                            LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                            (q._1, q._2, q._3, source._2, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                            LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                            (q._1, q._2, q._3, source._2, q._5, source._3)
                          case _ =>
                            LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                            (q._1, q._2, q._3, source._2, q._5, q._6)
                  case _: ((Nothing \^ Ld) <:< Tx) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq3[Ld, Le, Lf]("_4", "_5", "_6")
                            (q._1, q._2, q._3, source._3, source._1, source._2)
                          case _ =>
                            LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                            LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, source._3, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq3[Ld, Le, Lf]("_4", "_5", "_6")
                            (q._1, q._2, q._3, source._3, source._2, source._1)
                          case _ =>
                            LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                            LabelConflicts.diff[Ld, Lf]("_4", "_6")("")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, source._3, source._2, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                            LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                            (q._1, q._2, q._3, source._3, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                            LabelConflicts.diff[Ld, Le]("_4", "_5")("")
                            (q._1, q._2, q._3, source._3, q._5, source._2)
                          case _ =>
                            LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                            (q._1, q._2, q._3, source._3, q._5, q._6)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            (q._1, q._2, q._3, q._4, source._1, source._2)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            (q._1, q._2, q._3, q._4, source._1, source._3)
                          case _ =>
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            (q._1, q._2, q._3, q._4, source._2, source._1)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            (q._1, q._2, q._3, q._4, source._2, source._3)
                          case _ =>
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6)
                      case _: ((Nothing \^ Le) <:< Tx) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            (q._1, q._2, q._3, q._4, source._3, source._1)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            (q._1, q._2, q._3, q._4, source._3, source._2)
                          case _ =>
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, q._4, source._3, q._6)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            (q._1, q._2, q._3, q._4, q._5, source._1)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            (q._1, q._2, q._3, q._4, q._5, source._2)
                          case _: ((Nothing \^ Lf) <:< Tx) =>
                            (q._1, q._2, q._3, q._4, q._5, source._3)
                          case _ =>
                            compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw](source: (Tz, Ty, Tx, Tw)): (A \^ La | Tz | Ty | Tx | Tw, B \^ Lb | Tz | Ty | Tx | Tw, C \^ Lc | Tz | Ty | Tx | Tw, D \^ Ld | Tz | Ty | Tx | Tw, E \^ Le | Tz | Ty | Tx | Tw, F \^ Lf | Tz | Ty | Tx | Tw) =
  compiletime.error("Implementation restrictions prevent name-update of 6-tuple by 4-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv](source: (Tz, Ty, Tx, Tw, Tv)): (A \^ La | Tz | Ty | Tx | Tw | Tv, B \^ Lb | Tz | Ty | Tx | Tw | Tv, C \^ Lc | Tz | Ty | Tx | Tw | Tv, D \^ Ld | Tz | Ty | Tx | Tw | Tv, E \^ Le | Tz | Ty | Tx | Tw | Tv, F \^ Lf | Tz | Ty | Tx | Tw | Tv) =
  compiletime.error("Implementation restrictions prevent name-update of 6-tuple by 5-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu](source: (Tz, Ty, Tx, Tw, Tv, Tu)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu) =
  compiletime.error("Implementation restrictions prevent name-update of 6-tuple by 6-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt) =
  compiletime.error("Implementation restrictions prevent name-update of 6-tuple by 7-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts) =
  compiletime.error("Implementation restrictions prevent name-update of 6-tuple by 8-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr) =
  compiletime.error("Implementation restrictions prevent name-update of 6-tuple by 9-tuple")

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

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg): (A, B, C, D, E, F, G) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): ((A | B | C | D | E | F | G) \^ Lz) =
    summonFrom:
      case _: (Lz =:= La) =>
        LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
        q._1.asInstanceOf[(A \^ Lz)]
      case _: (Lz =:= Lb) =>
        LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
        q._2.asInstanceOf[(B \^ Lz)]
      case _: (Lz =:= Lc) =>
        LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
        q._3.asInstanceOf[(C \^ Lz)]
      case _: (Lz =:= Ld) =>
        LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
        q._4.asInstanceOf[(D \^ Lz)]
      case _: (Lz =:= Le) =>
        LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
        q._5.asInstanceOf[(E \^ Lz)]
      case _: (Lz =:= Lf) =>
        LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
        q._6.asInstanceOf[(F \^ Lz)]
      case _: (Lz =:= Lg) =>
        q._7.asInstanceOf[(G \^ Lz)]
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly) =
    LabelConflicts.unik2(lz, ly)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg]("_1", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._1.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head6[Lz, Lb, Ld, Le, Lf, Lg]("_1", "_2", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._1.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head6[Lz, Lb, Lc, Le, Lf, Lg]("_1", "_2", "_3", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._1.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head6[Lz, Lb, Lc, Ld, Lf, Lg]("_1", "_2", "_3", "_4", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
            (q._1.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lg]("_1", "_2", "_3", "_4", "_5", "_7")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
            (q._1.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head6[Lz, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
            (q._1.unlabel, q._7.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg]("_1", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._2.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._2.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head5[Lz, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._2.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head5[Lz, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
            (q._2.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head5[Lz, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
            (q._2.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
            (q._2.unlabel, q._7.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lb, Ld, Le, Lf, Lg]("_1", "_2", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._3.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._3.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head4[Lz, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._3.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head4[Lz, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
            (q._3.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head4[Lz, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
            (q._3.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
            (q._3.unlabel, q._7.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lb, Lc, Le, Lf, Lg]("_1", "_2", "_3", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._4.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._4.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._4.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head3[Lz, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
            (q._4.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head3[Lz, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
            (q._4.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
            (q._4.unlabel, q._7.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lb, Lc, Ld, Lf, Lg]("_1", "_2", "_3", "_4", "_6", "_7")(" with " + codeOf(ly))
            (q._5.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")(" with " + codeOf(ly))
            (q._5.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(ly))
            (q._5.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(ly))
            (q._5.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
            (q._5.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
            (q._5.unlabel, q._7.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lf) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lb, Lc, Ld, Le, Lg]("_1", "_2", "_3", "_4", "_5", "_7")(" with " + codeOf(ly))
            (q._6.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")(" with " + codeOf(ly))
            (q._6.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(ly))
            (q._6.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(ly))
            (q._6.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
            (q._6.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            (q._6.unlabel, q._7.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lg) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head6[Ly, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._7.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head5[Ly, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._7.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
            (q._7.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
            (q._7.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
            (q._7.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            (q._7.unlabel, q._6.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx) =
    LabelConflicts.unik3(lz, ly, lx)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_1", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head5[Lz, Lc, Le, Lf, Lg]("_1", "_3", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head5[Lz, Lc, Ld, Lf, Lg]("_1", "_3", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head5[Lz, Lc, Ld, Le, Lg]("_1", "_3", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                (q._1.unlabel, q._2.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_1", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head5[Lz, Lb, Le, Lf, Lg]("_1", "_2", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head5[Lz, Lb, Ld, Lf, Lg]("_1", "_2", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head5[Lz, Lb, Ld, Le, Lg]("_1", "_2", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head5[Lz, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                (q._1.unlabel, q._3.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head5[Lz, Lc, Le, Lf, Lg]("_1", "_3", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head5[Lz, Lb, Le, Lf, Lg]("_1", "_2", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head5[Lz, Lb, Lc, Lf, Lg]("_1", "_2", "_3", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head5[Lz, Lb, Lc, Le, Lg]("_1", "_2", "_3", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head5[Lz, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                (q._1.unlabel, q._4.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head5[Lz, Lc, Ld, Lf, Lg]("_1", "_3", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head5[Lz, Lb, Ld, Lf, Lg]("_1", "_2", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head5[Lz, Lb, Lc, Lf, Lg]("_1", "_2", "_3", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head5[Lz, Lb, Lc, Ld, Lg]("_1", "_2", "_3", "_4", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head5[Lz, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                (q._1.unlabel, q._5.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head5[Lz, Lc, Ld, Le, Lg]("_1", "_3", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head5[Lz, Lb, Ld, Le, Lg]("_1", "_2", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head5[Lz, Lb, Lc, Le, Lg]("_1", "_2", "_3", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head5[Lz, Lb, Lc, Ld, Lg]("_1", "_2", "_3", "_4", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_5", "_7")(" with " + codeOf(lx))
                (q._1.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head5[Lz, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(lz))
                (q._1.unlabel, q._6.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lg) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Lx, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._7.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head5[Lz, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._7.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head5[Lz, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._7.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head5[Lz, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._1.unlabel, q._7.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head5[Lz, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(lz))
                (q._1.unlabel, q._7.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lc) =>
                LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_1", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head5[Lz, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Le, Lf, Lg]("_1", "_3", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head5[Lz, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Lf, Lg]("_1", "_3", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head5[Lz, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Le, Lg]("_1", "_3", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                (q._2.unlabel, q._1.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Ld, Le, Lf, Lg]("_1", "_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_2", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Ld, Lf, Lg]("_2", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Ld, Le, Lg]("_2", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                (q._2.unlabel, q._3.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Lz, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Le, Lf, Lg]("_1", "_3", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_2", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Lc, Lf, Lg]("_2", "_3", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Lc, Le, Lg]("_2", "_3", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head4[Lz, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                (q._2.unlabel, q._4.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Lz, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Lf, Lg]("_1", "_3", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Ld, Lf, Lg]("_2", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Lc, Lf, Lg]("_2", "_3", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Lc, Ld, Lg]("_2", "_3", "_4", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head4[Lz, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                (q._2.unlabel, q._5.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Lz, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Le, Lg]("_1", "_3", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Ld, Le, Lg]("_2", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Lc, Le, Lg]("_2", "_3", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Lc, Ld, Lg]("_2", "_3", "_4", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_5", "_7")(" with " + codeOf(lx))
                (q._2.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head4[Lz, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lz))
                (q._2.unlabel, q._6.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lg) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Lz, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Lx, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._7.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._7.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._7.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._2.unlabel, q._7.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lz))
                (q._2.unlabel, q._7.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_1", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Le, Lf, Lg]("_1", "_2", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Ld, Lf, Lg]("_1", "_2", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Ld, Le, Lg]("_1", "_2", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")(" with " + codeOf(ly))
                (q._3.unlabel, q._1.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Ld, Le, Lf, Lg]("_1", "_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_2", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Lz, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Lf, Lg]("_2", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Lz, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lg]("_2", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(ly))
                (q._3.unlabel, q._2.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Le, Lf, Lg]("_1", "_2", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_2", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_3", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Le, Lg]("_3", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head3[Lz, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                (q._3.unlabel, q._4.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Ld, Lf, Lg]("_1", "_2", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Lf, Lg]("_2", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_3", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Ld, Lg]("_3", "_4", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head3[Lz, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                (q._3.unlabel, q._5.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Ld, Le, Lg]("_1", "_2", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lg]("_2", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Le, Lg]("_3", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Ld, Lg]("_3", "_4", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_5", "_7")(" with " + codeOf(lx))
                (q._3.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
                (q._3.unlabel, q._6.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lg) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Lx, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._7.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._7.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._7.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._3.unlabel, q._7.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lz))
                (q._3.unlabel, q._7.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Le, Lf, Lg]("_1", "_3", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Le, Lf, Lg]("_1", "_2", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Lc, Lf, Lg]("_1", "_2", "_3", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Lc, Le, Lg]("_1", "_2", "_3", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")(" with " + codeOf(ly))
                (q._4.unlabel, q._1.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Le, Lf, Lg]("_1", "_3", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_2", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Lf, Lg]("_2", "_3", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Le, Lg]("_2", "_3", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(ly))
                (q._4.unlabel, q._2.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Le, Lf, Lg]("_1", "_2", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Le, Lf, Lg]("_3", "_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Le, Lf, Lg]("_2", "_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_3", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Lz, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lg]("_3", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(ly))
                (q._4.unlabel, q._3.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Lc, Lf, Lg]("_1", "_2", "_3", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Lf, Lg]("_2", "_3", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_3", "_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Lg]("_4", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                (q._4.unlabel, q._5.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Lc, Le, Lg]("_1", "_2", "_3", "_5", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Le, Lg]("_2", "_3", "_5", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lg]("_3", "_5", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lg]("_4", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_5", "_7")(" with " + codeOf(lx))
                (q._4.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                (q._4.unlabel, q._6.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lg) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Lx, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._7.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._7.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._7.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lf]("_4", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._4.unlabel, q._7.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Le]("_4", "_5")(" with " + codeOf(lz))
                (q._4.unlabel, q._7.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Lf, Lg]("_1", "_3", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Ld, Lf, Lg]("_1", "_2", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Lc, Lf, Lg]("_1", "_2", "_3", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Lc, Ld, Lg]("_1", "_2", "_3", "_4", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")(" with " + codeOf(ly))
                (q._5.unlabel, q._1.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Lf, Lg]("_1", "_3", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Lf, Lg]("_2", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Lf, Lg]("_2", "_3", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Lg]("_2", "_3", "_4", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(ly))
                (q._5.unlabel, q._2.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Ld, Lf, Lg]("_1", "_2", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Lf, Lg]("_3", "_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Lf, Lg]("_2", "_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_3", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lg]("_3", "_4", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(ly))
                (q._5.unlabel, q._3.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Lc, Lf, Lg]("_1", "_2", "_3", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Lf, Lg]("_2", "_3", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Lf, Lg]("_4", "_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Lf, Lg]("_3", "_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_4", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_6", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
                (q._5.unlabel, q._4.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Lc, Ld, Lg]("_1", "_2", "_3", "_4", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Lg]("_2", "_3", "_4", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lg]("_3", "_4", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lg]("_5", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_4", "_7")(" with " + codeOf(lx))
                (q._5.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                (q._5.unlabel, q._6.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lg) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head5[Lx, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._7.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head4[Lx, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._7.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.head3[Lx, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._7.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lf]("_5", "_6")(" with " + codeOf(lz))
                LabelConflicts.diff[Lx, Lf]("_4", "_6")(" with " + codeOf(lx))
                (q._5.unlabel, q._7.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                (q._5.unlabel, q._7.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lf) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Le, Lg]("_1", "_3", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Ld, Le, Lg]("_1", "_2", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Lc, Le, Lg]("_1", "_2", "_3", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lb, Lc, Ld, Lg]("_1", "_2", "_3", "_4", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head5[Ly, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(ly))
                (q._6.unlabel, q._1.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head5[Ly, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Le, Lg]("_1", "_3", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lg]("_2", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Le, Lg]("_2", "_3", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Lc, Ld, Lg]("_2", "_3", "_4", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head4[Ly, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(ly))
                (q._6.unlabel, q._2.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Ld, Le, Lg]("_1", "_2", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head4[Ly, Ld, Le, Lg]("_3", "_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lg]("_2", "_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lg]("_3", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Ld, Lg]("_3", "_4", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
                (q._6.unlabel, q._3.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Lc, Le, Lg]("_1", "_2", "_3", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Le, Lg]("_2", "_3", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.head3[Ly, Le, Lg]("_4", "_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lg]("_3", "_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_4", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_5", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                (q._6.unlabel, q._4.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Lc, Ld, Lg]("_1", "_2", "_3", "_4", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Lg]("_2", "_3", "_4", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lg]("_3", "_4", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
                LabelConflicts.diff[Ly, Lg]("_5", "_7")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lg]("_4", "_7")(" with " + codeOf(lx))
                (q._6.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lg) =>
                (q._6.unlabel, q._5.unlabel, q._7.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lg) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Lx, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._7.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lx, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._7.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lx, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._7.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._6.unlabel, q._7.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                (q._6.unlabel, q._7.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lg) =>
        summonFrom:
          case _: (Ly =:= La) =>
            summonFrom:
              case _: (Lx =:= Lb) =>
                LabelConflicts.head5[Ly, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._1.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head5[Ly, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._1.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head5[Ly, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._1.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head5[Ly, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._1.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head5[Ly, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(ly))
                (q._7.unlabel, q._1.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lb) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Ly, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lc, Ld, Le, Lf]("_1", "_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._2.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._2.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head4[Ly, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._2.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head4[Ly, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._2.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head4[Ly, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(ly))
                (q._7.unlabel, q._2.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lc) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Ld, Le, Lf]("_1", "_2", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._3.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Ly, Ld, Le, Lf]("_3", "_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Ld, Le, Lf]("_2", "_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._3.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.head3[Ly, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._3.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.head3[Ly, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._3.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.head3[Ly, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(ly))
                (q._7.unlabel, q._3.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Ld) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Lc, Le, Lf]("_1", "_2", "_3", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._4.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Le, Lf]("_2", "_3", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._4.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Ly, Le, Lf]("_4", "_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Le, Lf]("_3", "_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._4.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                LabelConflicts.diff[Ly, Lf]("_4", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_5", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._4.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                LabelConflicts.diff[Ly, Le]("_4", "_5")(" with " + codeOf(ly))
                (q._7.unlabel, q._4.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Le) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head5[Lx, Lb, Lc, Ld, Lf]("_1", "_2", "_3", "_4", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._5.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head4[Lx, Lc, Ld, Lf]("_2", "_3", "_4", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._5.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.head3[Lx, Ld, Lf]("_3", "_4", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._5.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Ly, Lf]("_5", "_6")(" with " + codeOf(ly))
                LabelConflicts.diff[Lx, Lf]("_4", "_6")(" with " + codeOf(lx))
                (q._7.unlabel, q._5.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lf) =>
                (q._7.unlabel, q._5.unlabel, q._6.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _: (Ly =:= Lf) =>
            summonFrom:
              case _: (Lx =:= La) =>
                LabelConflicts.head5[Lx, Lb, Lc, Ld, Le]("_1", "_2", "_3", "_4", "_5")(" with " + codeOf(lx))
                (q._7.unlabel, q._6.unlabel, q._1.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lb) =>
                LabelConflicts.head4[Lx, Lc, Ld, Le]("_2", "_3", "_4", "_5")(" with " + codeOf(lx))
                (q._7.unlabel, q._6.unlabel, q._2.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Lc) =>
                LabelConflicts.head3[Lx, Ld, Le]("_3", "_4", "_5")(" with " + codeOf(lx))
                (q._7.unlabel, q._6.unlabel, q._3.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Ld) =>
                LabelConflicts.diff[Lx, Le]("_4", "_5")(" with " + codeOf(lx))
                (q._7.unlabel, q._6.unlabel, q._4.unlabel).label[Lz, Ly, Lx]
              case _: (Lx =:= Le) =>
                (q._7.unlabel, q._6.unlabel, q._5.unlabel).label[Lz, Ly, Lx]
              case _ => compiletime.error("No label found matching " + codeOf(lx))
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx, (A | B | C | D | E | F | G) \^ Lw) =
    compiletime.error("Implementation restrictions prevent 4-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx, (A | B | C | D | E | F | G) \^ Lw, (A | B | C | D | E | F | G) \^ Lv) =
    compiletime.error("Implementation restrictions prevent 5-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx, (A | B | C | D | E | F | G) \^ Lw, (A | B | C | D | E | F | G) \^ Lv, (A | B | C | D | E | F | G) \^ Lu) =
    compiletime.error("Implementation restrictions prevent 6-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt): ((A | B | C | D | E | F | G) \^ Lz, (A | B | C | D | E | F | G) \^ Ly, (A | B | C | D | E | F | G) \^ Lx, (A | B | C | D | E | F | G) \^ Lw, (A | B | C | D | E | F | G) \^ Lv, (A | B | C | D | E | F | G) \^ Lu, (A | B | C | D | E | F | G) \^ Lt) =
    compiletime.error("Implementation restrictions prevent 7-way picking by name")

  transparent inline def updatedBy[Tz](source: Tz): (A \^ La | Tz, B \^ Lb | Tz, C \^ Lc | Tz, D \^ Ld | Tz, E \^ Le | Tz, F \^ Lf | Tz, G \^ Lg | Tz) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => 
        LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")("")
        (source, q._2, q._3, q._4, q._5, q._6, q._7)
      case _: ((Nothing \^ Lb) <:< Tz) => 
        LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")("")
        (q._1, source, q._3, q._4, q._5, q._6, q._7)
      case _: ((Nothing \^ Lc) <:< Tz) => 
        LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
        (q._1, q._2, source, q._4, q._5, q._6, q._7)
      case _: ((Nothing \^ Ld) <:< Tz) => 
        LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
        (q._1, q._2, q._3, source, q._5, q._6, q._7)
      case _: ((Nothing \^ Le) <:< Tz) => 
        LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
        (q._1, q._2, q._3, q._4, source, q._6, q._7)
      case _: ((Nothing \^ Lf) <:< Tz) => 
        LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
        (q._1, q._2, q._3, q._4, q._5, source, q._7)
      case _: ((Nothing \^ Lg) <:< Tz) => 
        (q._1, q._2, q._3, q._4, q._5, q._6, source)
      case _ =>
        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty](source: (Tz, Ty)): (A \^ La | Tz | Ty, B \^ Lb | Tz | Ty, C \^ Lc | Tz | Ty, D \^ Ld | Tz | Ty, E \^ Le | Tz | Ty, F \^ Lf | Tz | Ty, G \^ Lg | Tz | Ty) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head6[La, Lc, Ld, Le, Lf, Lg]("_1", "_3", "_4", "_5", "_6", "_7")("")
            LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")("")
            (source._1, source._2, q._3, q._4, q._5, q._6, q._7)
          case _: ((Nothing \^ Lc) <:< Ty) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head6[La, Lb, Ld, Le, Lf, Lg]("_1", "_2", "_4", "_5", "_6", "_7")("")
            LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
            (source._1, q._2, source._2, q._4, q._5, q._6, q._7)
          case _: ((Nothing \^ Ld) <:< Ty) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head6[La, Lb, Lc, Le, Lf, Lg]("_1", "_2", "_3", "_5", "_6", "_7")("")
            LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
            (source._1, q._2, q._3, source._2, q._5, q._6, q._7)
          case _: ((Nothing \^ Le) <:< Ty) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head6[La, Lb, Lc, Ld, Lf, Lg]("_1", "_2", "_3", "_4", "_6", "_7")("")
            LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
            (source._1, q._2, q._3, q._4, source._2, q._6, q._7)
          case _: ((Nothing \^ Lf) <:< Ty) => 
            LabelConflicts.unyq2[La, Lf]("_1", "_6")
            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lg]("_1", "_2", "_3", "_4", "_5", "_7")("")
            LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
            (source._1, q._2, q._3, q._4, q._5, source._2, q._7)
          case _: ((Nothing \^ Lg) <:< Ty) => 
            LabelConflicts.unyq2[La, Lg]("_1", "_7")
            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, source._2)
          case _ =>
            LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, q._7)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head6[La, Lc, Ld, Le, Lf, Lg]("_1", "_3", "_4", "_5", "_6", "_7")("")
            LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")("")
            (source._2, source._1, q._3, q._4, q._5, q._6, q._7)
          case _: ((Nothing \^ Lc) <:< Tz) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head6[La, Lb, Ld, Le, Lf, Lg]("_1", "_2", "_4", "_5", "_6", "_7")("")
            LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
            (source._2, q._2, source._1, q._4, q._5, q._6, q._7)
          case _: ((Nothing \^ Ld) <:< Tz) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head6[La, Lb, Lc, Le, Lf, Lg]("_1", "_2", "_3", "_5", "_6", "_7")("")
            LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
            (source._2, q._2, q._3, source._1, q._5, q._6, q._7)
          case _: ((Nothing \^ Le) <:< Tz) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head6[La, Lb, Lc, Ld, Lf, Lg]("_1", "_2", "_3", "_4", "_6", "_7")("")
            LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
            (source._2, q._2, q._3, q._4, source._1, q._6, q._7)
          case _: ((Nothing \^ Lf) <:< Tz) => 
            LabelConflicts.unyq2[La, Lf]("_1", "_6")
            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lg]("_1", "_2", "_3", "_4", "_5", "_7")("")
            LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
            (source._2, q._2, q._3, q._4, q._5, source._1, q._7)
          case _: ((Nothing \^ Lg) <:< Tz) => 
            LabelConflicts.unyq2[La, Lg]("_1", "_7")
            LabelConflicts.head6[La, Lb, Lc, Ld, Le, Lf]("_1", "_2", "_3", "_4", "_5", "_6")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, source._1)
          case _ =>
            LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, q._7)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head5[Lb, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")("")
                LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
                (q._1, source._1, source._2, q._4, q._5, q._6, q._7)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head5[Lb, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")("")
                LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
                (q._1, source._1, q._3, source._2, q._5, q._6, q._7)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head5[Lb, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")("")
                LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                (q._1, source._1, q._3, q._4, source._2, q._6, q._7)
              case _: ((Nothing \^ Lf) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")("")
                LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                (q._1, source._1, q._3, q._4, q._5, source._2, q._7)
              case _: ((Nothing \^ Lg) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lg]("_2", "_7")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, source._2)
              case _ =>
                LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, q._7)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head5[Lb, Ld, Le, Lf, Lg]("_2", "_4", "_5", "_6", "_7")("")
                LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
                (q._1, source._2, source._1, q._4, q._5, q._6, q._7)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head5[Lb, Lc, Le, Lf, Lg]("_2", "_3", "_5", "_6", "_7")("")
                LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
                (q._1, source._2, q._3, source._1, q._5, q._6, q._7)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head5[Lb, Lc, Ld, Lf, Lg]("_2", "_3", "_4", "_6", "_7")("")
                LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                (q._1, source._2, q._3, q._4, source._1, q._6, q._7)
              case _: ((Nothing \^ Lf) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lg]("_2", "_3", "_4", "_5", "_7")("")
                LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                (q._1, source._2, q._3, q._4, q._5, source._1, q._7)
              case _: ((Nothing \^ Lg) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lg]("_2", "_7")
                LabelConflicts.head5[Lb, Lc, Ld, Le, Lf]("_2", "_3", "_4", "_5", "_6")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, source._1)
              case _ =>
                LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, q._7)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.head4[Lc, Le, Lf, Lg]("_3", "_5", "_6", "_7")("")
                    LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
                    (q._1, q._2, source._1, source._2, q._5, q._6, q._7)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.head4[Lc, Ld, Lf, Lg]("_3", "_4", "_6", "_7")("")
                    LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                    (q._1, q._2, source._1, q._4, source._2, q._6, q._7)
                  case _: ((Nothing \^ Lf) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                    LabelConflicts.head4[Lc, Ld, Le, Lg]("_3", "_4", "_5", "_7")("")
                    LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                    (q._1, q._2, source._1, q._4, q._5, source._2, q._7)
                  case _: ((Nothing \^ Lg) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lg]("_3", "_7")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, source._2)
                  case _ =>
                    LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, q._7)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.head4[Lc, Le, Lf, Lg]("_3", "_5", "_6", "_7")("")
                    LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
                    (q._1, q._2, source._2, source._1, q._5, q._6, q._7)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.head4[Lc, Ld, Lf, Lg]("_3", "_4", "_6", "_7")("")
                    LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                    (q._1, q._2, source._2, q._4, source._1, q._6, q._7)
                  case _: ((Nothing \^ Lf) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                    LabelConflicts.head4[Lc, Ld, Le, Lg]("_3", "_4", "_5", "_7")("")
                    LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                    (q._1, q._2, source._2, q._4, q._5, source._1, q._7)
                  case _: ((Nothing \^ Lg) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lg]("_3", "_7")
                    LabelConflicts.head4[Lc, Ld, Le, Lf]("_3", "_4", "_5", "_6")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, source._1)
                  case _ =>
                    LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, q._7)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        LabelConflicts.head3[Ld, Lf, Lg]("_4", "_6", "_7")("")
                        LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                        (q._1, q._2, q._3, source._1, source._2, q._6, q._7)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                        LabelConflicts.head3[Ld, Le, Lg]("_4", "_5", "_7")("")
                        LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                        (q._1, q._2, q._3, source._1, q._5, source._2, q._7)
                      case _: ((Nothing \^ Lg) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lg]("_4", "_7")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, source._2)
                      case _ =>
                        LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, q._7)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        LabelConflicts.head3[Ld, Lf, Lg]("_4", "_6", "_7")("")
                        LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                        (q._1, q._2, q._3, source._2, source._1, q._6, q._7)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                        LabelConflicts.head3[Ld, Le, Lg]("_4", "_5", "_7")("")
                        LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                        (q._1, q._2, q._3, source._2, q._5, source._1, q._7)
                      case _: ((Nothing \^ Lg) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lg]("_4", "_7")
                        LabelConflicts.head3[Ld, Le, Lf]("_4", "_5", "_6")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, source._1)
                      case _ =>
                        LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, q._7)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            LabelConflicts.diff[Le, Lg]("_5", "_7")("")
                            LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                            (q._1, q._2, q._3, q._4, source._1, source._2, q._7)
                          case _: ((Nothing \^ Lg) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Lg]("_5", "_7")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, source._2)
                          case _ =>
                            LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, q._7)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            LabelConflicts.diff[Le, Lg]("_5", "_7")("")
                            LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                            (q._1, q._2, q._3, q._4, source._2, source._1, q._7)
                          case _: ((Nothing \^ Lg) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Lg]("_5", "_7")
                            LabelConflicts.diff[Le, Lf]("_5", "_6")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, source._1)
                          case _ =>
                            LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, q._7)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Ty) =>
                                LabelConflicts.unyq2[Lf, Lg]("_6", "_7")
                                (q._1, q._2, q._3, q._4, q._5, source._1, source._2)
                              case _ =>
                                LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                                (q._1, q._2, q._3, q._4, q._5, source._1, q._7)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Tz) =>
                                LabelConflicts.unyq2[Lf, Lg]("_6", "_7")
                                (q._1, q._2, q._3, q._4, q._5, source._2, source._1)
                              case _ =>
                                LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                                (q._1, q._2, q._3, q._4, q._5, source._2, q._7)
                          case _ =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Tz) =>
                                (q._1, q._2, q._3, q._4, q._5, q._6, source._1)
                              case _: ((Nothing \^ Lg) <:< Ty) =>
                                (q._1, q._2, q._3, q._4, q._5, q._6, source._2)
                              case _ =>
                                compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx](source: (Tz, Ty, Tx)): (A \^ La | Tz | Ty | Tx, B \^ Lb | Tz | Ty | Tx, C \^ Lc | Tz | Ty | Tx, D \^ Ld | Tz | Ty | Tx, E \^ Le | Tz | Ty | Tx, F \^ Lf | Tz | Ty | Tx, G \^ Lg | Tz | Ty | Tx) =
  compiletime.error("Implementation restrictions prevent name-update of 7-tuple by 3-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw](source: (Tz, Ty, Tx, Tw)): (A \^ La | Tz | Ty | Tx | Tw, B \^ Lb | Tz | Ty | Tx | Tw, C \^ Lc | Tz | Ty | Tx | Tw, D \^ Ld | Tz | Ty | Tx | Tw, E \^ Le | Tz | Ty | Tx | Tw, F \^ Lf | Tz | Ty | Tx | Tw, G \^ Lg | Tz | Ty | Tx | Tw) =
  compiletime.error("Implementation restrictions prevent name-update of 7-tuple by 4-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv](source: (Tz, Ty, Tx, Tw, Tv)): (A \^ La | Tz | Ty | Tx | Tw | Tv, B \^ Lb | Tz | Ty | Tx | Tw | Tv, C \^ Lc | Tz | Ty | Tx | Tw | Tv, D \^ Ld | Tz | Ty | Tx | Tw | Tv, E \^ Le | Tz | Ty | Tx | Tw | Tv, F \^ Lf | Tz | Ty | Tx | Tw | Tv, G \^ Lg | Tz | Ty | Tx | Tw | Tv) =
  compiletime.error("Implementation restrictions prevent name-update of 7-tuple by 5-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu](source: (Tz, Ty, Tx, Tw, Tv, Tu)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu) =
  compiletime.error("Implementation restrictions prevent name-update of 7-tuple by 6-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt) =
  compiletime.error("Implementation restrictions prevent name-update of 7-tuple by 7-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts) =
  compiletime.error("Implementation restrictions prevent name-update of 7-tuple by 8-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr) =
  compiletime.error("Implementation restrictions prevent name-update of 7-tuple by 9-tuple")

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

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh): (A, B, C, D, E, F, G, H) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): ((A | B | C | D | E | F | G | H) \^ Lz) =
    summonFrom:
      case _: (Lz =:= La) =>
        LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
        q._1.asInstanceOf[(A \^ Lz)]
      case _: (Lz =:= Lb) =>
        LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
        q._2.asInstanceOf[(B \^ Lz)]
      case _: (Lz =:= Lc) =>
        LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
        q._3.asInstanceOf[(C \^ Lz)]
      case _: (Lz =:= Ld) =>
        LabelConflicts.head5[Lz, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
        q._4.asInstanceOf[(D \^ Lz)]
      case _: (Lz =:= Le) =>
        LabelConflicts.head4[Lz, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(lz))
        q._5.asInstanceOf[(E \^ Lz)]
      case _: (Lz =:= Lf) =>
        LabelConflicts.head3[Lz, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(lz))
        q._6.asInstanceOf[(F \^ Lz)]
      case _: (Lz =:= Lg) =>
        LabelConflicts.diff[Lz, Lh]("_7", "_8")(" with " + codeOf(lz))
        q._7.asInstanceOf[(G \^ Lz)]
      case _: (Lz =:= Lh) =>
        q._8.asInstanceOf[(H \^ Lz)]
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly) =
    LabelConflicts.unik2(lz, ly)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._1.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head7[Lz, Lb, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._1.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head7[Lz, Lb, Lc, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._1.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head7[Lz, Lb, Lc, Ld, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._1.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(ly))
            (q._1.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_8")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lh]("_7", "_8")(" with " + codeOf(ly))
            (q._1.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head7[Lz, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            (q._1.unlabel, q._8.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._2.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh]("_2", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._2.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head6[Lz, Lc, Le, Lf, Lg, Lh]("_2", "_3", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._2.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head6[Lz, Lc, Ld, Lf, Lg, Lh]("_2", "_3", "_4", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._2.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head6[Lz, Lc, Ld, Le, Lg, Lh]("_2", "_3", "_4", "_5", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(ly))
            (q._2.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lh]("_2", "_3", "_4", "_5", "_6", "_8")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lh]("_7", "_8")(" with " + codeOf(ly))
            (q._2.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head6[Lz, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            (q._2.unlabel, q._8.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lb, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._3.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh]("_2", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._3.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head5[Lz, Le, Lf, Lg, Lh]("_3", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._3.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head5[Lz, Ld, Lf, Lg, Lh]("_3", "_4", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._3.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head5[Lz, Ld, Le, Lg, Lh]("_3", "_4", "_5", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(ly))
            (q._3.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head5[Lz, Ld, Le, Lf, Lh]("_3", "_4", "_5", "_6", "_8")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lh]("_7", "_8")(" with " + codeOf(ly))
            (q._3.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head5[Lz, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            (q._3.unlabel, q._8.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head5[Lz, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lb, Lc, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._4.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head5[Lz, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lc, Le, Lf, Lg, Lh]("_2", "_3", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._4.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head5[Lz, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Le, Lf, Lg, Lh]("_3", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._4.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head4[Lz, Lf, Lg, Lh]("_4", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._4.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head4[Lz, Le, Lg, Lh]("_4", "_5", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(ly))
            (q._4.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head4[Lz, Le, Lf, Lh]("_4", "_5", "_6", "_8")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lh]("_7", "_8")(" with " + codeOf(ly))
            (q._4.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head4[Lz, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(lz))
            (q._4.unlabel, q._8.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head4[Lz, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lb, Lc, Ld, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._5.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head4[Lz, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lc, Ld, Lf, Lg, Lh]("_2", "_3", "_4", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._5.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head4[Lz, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Ld, Lf, Lg, Lh]("_3", "_4", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._5.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head4[Lz, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lf, Lg, Lh]("_4", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._5.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head3[Lz, Lg, Lh]("_5", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(ly))
            (q._5.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head3[Lz, Lf, Lh]("_5", "_6", "_8")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lh]("_7", "_8")(" with " + codeOf(ly))
            (q._5.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head3[Lz, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(lz))
            (q._5.unlabel, q._8.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lf) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head3[Lz, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_7", "_8")(" with " + codeOf(ly))
            (q._6.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head3[Lz, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lc, Ld, Le, Lg, Lh]("_2", "_3", "_4", "_5", "_7", "_8")(" with " + codeOf(ly))
            (q._6.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head3[Lz, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Ld, Le, Lg, Lh]("_3", "_4", "_5", "_7", "_8")(" with " + codeOf(ly))
            (q._6.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head3[Lz, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Le, Lg, Lh]("_4", "_5", "_7", "_8")(" with " + codeOf(ly))
            (q._6.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head3[Lz, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lg, Lh]("_5", "_7", "_8")(" with " + codeOf(ly))
            (q._6.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.diff[Lz, Lh]("_6", "_8")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lh]("_7", "_8")(" with " + codeOf(ly))
            (q._6.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.diff[Lz, Lg]("_6", "_7")(" with " + codeOf(lz))
            (q._6.unlabel, q._8.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lg) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.diff[Lz, Lh]("_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lf, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_8")(" with " + codeOf(ly))
            (q._7.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.diff[Lz, Lh]("_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lh]("_2", "_3", "_4", "_5", "_6", "_8")(" with " + codeOf(ly))
            (q._7.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.diff[Lz, Lh]("_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Ld, Le, Lf, Lh]("_3", "_4", "_5", "_6", "_8")(" with " + codeOf(ly))
            (q._7.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.diff[Lz, Lh]("_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Le, Lf, Lh]("_4", "_5", "_6", "_8")(" with " + codeOf(ly))
            (q._7.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.diff[Lz, Lh]("_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lf, Lh]("_5", "_6", "_8")(" with " + codeOf(ly))
            (q._7.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.diff[Lz, Lh]("_7", "_8")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Lh]("_6", "_8")(" with " + codeOf(ly))
            (q._7.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            (q._7.unlabel, q._8.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lh) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head7[Ly, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._8.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head6[Ly, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._8.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head5[Ly, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._8.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head4[Ly, Le, Lf, Lg]("_4", "_5", "_6", "_7")(" with " + codeOf(ly))
            (q._8.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head3[Ly, Lf, Lg]("_5", "_6", "_7")(" with " + codeOf(ly))
            (q._8.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.diff[Ly, Lg]("_6", "_7")(" with " + codeOf(ly))
            (q._8.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            (q._8.unlabel, q._7.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx) =
    compiletime.error("Implementation restrictions prevent 3-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw) =
    compiletime.error("Implementation restrictions prevent 4-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw, (A | B | C | D | E | F | G | H) \^ Lv) =
    compiletime.error("Implementation restrictions prevent 5-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw, (A | B | C | D | E | F | G | H) \^ Lv, (A | B | C | D | E | F | G | H) \^ Lu) =
    compiletime.error("Implementation restrictions prevent 6-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw, (A | B | C | D | E | F | G | H) \^ Lv, (A | B | C | D | E | F | G | H) \^ Lu, (A | B | C | D | E | F | G | H) \^ Lt) =
    compiletime.error("Implementation restrictions prevent 7-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal, Ls <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt, inline ls: Ls): ((A | B | C | D | E | F | G | H) \^ Lz, (A | B | C | D | E | F | G | H) \^ Ly, (A | B | C | D | E | F | G | H) \^ Lx, (A | B | C | D | E | F | G | H) \^ Lw, (A | B | C | D | E | F | G | H) \^ Lv, (A | B | C | D | E | F | G | H) \^ Lu, (A | B | C | D | E | F | G | H) \^ Lt, (A | B | C | D | E | F | G | H) \^ Ls) =
    compiletime.error("Implementation restrictions prevent 8-way picking by name")

  transparent inline def updatedBy[Tz](source: Tz): (A \^ La | Tz, B \^ Lb | Tz, C \^ Lc | Tz, D \^ Ld | Tz, E \^ Le | Tz, F \^ Lf | Tz, G \^ Lg | Tz, H \^ Lh | Tz) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => 
        LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
        (source, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
      case _: ((Nothing \^ Lb) <:< Tz) => 
        LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
        (q._1, source, q._3, q._4, q._5, q._6, q._7, q._8)
      case _: ((Nothing \^ Lc) <:< Tz) => 
        LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
        (q._1, q._2, source, q._4, q._5, q._6, q._7, q._8)
      case _: ((Nothing \^ Ld) <:< Tz) => 
        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
        (q._1, q._2, q._3, source, q._5, q._6, q._7, q._8)
      case _: ((Nothing \^ Le) <:< Tz) => 
        LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
        (q._1, q._2, q._3, q._4, source, q._6, q._7, q._8)
      case _: ((Nothing \^ Lf) <:< Tz) => 
        LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
        (q._1, q._2, q._3, q._4, q._5, source, q._7, q._8)
      case _: ((Nothing \^ Lg) <:< Tz) => 
        LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
        (q._1, q._2, q._3, q._4, q._5, q._6, source, q._8)
      case _: ((Nothing \^ Lh) <:< Tz) => 
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, source)
      case _ =>
        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty](source: (Tz, Ty)): (A \^ La | Tz | Ty, B \^ Lb | Tz | Ty, C \^ Lc | Tz | Ty, D \^ Ld | Tz | Ty, E \^ Le | Tz | Ty, F \^ Lf | Tz | Ty, G \^ Lg | Tz | Ty, H \^ Lh | Tz | Ty) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head7[La, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_3", "_4", "_5", "_6", "_7", "_8")("")
            LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
            (source._1, source._2, q._3, q._4, q._5, q._6, q._7, q._8)
          case _: ((Nothing \^ Lc) <:< Ty) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head7[La, Lb, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_4", "_5", "_6", "_7", "_8")("")
            LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
            (source._1, q._2, source._2, q._4, q._5, q._6, q._7, q._8)
          case _: ((Nothing \^ Ld) <:< Ty) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head7[La, Lb, Lc, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_5", "_6", "_7", "_8")("")
            LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
            (source._1, q._2, q._3, source._2, q._5, q._6, q._7, q._8)
          case _: ((Nothing \^ Le) <:< Ty) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head7[La, Lb, Lc, Ld, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_6", "_7", "_8")("")
            LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
            (source._1, q._2, q._3, q._4, source._2, q._6, q._7, q._8)
          case _: ((Nothing \^ Lf) <:< Ty) => 
            LabelConflicts.unyq2[La, Lf]("_1", "_6")
            LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_7", "_8")("")
            LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
            (source._1, q._2, q._3, q._4, q._5, source._2, q._7, q._8)
          case _: ((Nothing \^ Lg) <:< Ty) => 
            LabelConflicts.unyq2[La, Lg]("_1", "_7")
            LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_8")("")
            LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, source._2, q._8)
          case _: ((Nothing \^ Lh) <:< Ty) => 
            LabelConflicts.unyq2[La, Lh]("_1", "_8")
            LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, q._7, source._2)
          case _ =>
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head7[La, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_3", "_4", "_5", "_6", "_7", "_8")("")
            LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
            (source._2, source._1, q._3, q._4, q._5, q._6, q._7, q._8)
          case _: ((Nothing \^ Lc) <:< Tz) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head7[La, Lb, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_4", "_5", "_6", "_7", "_8")("")
            LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
            (source._2, q._2, source._1, q._4, q._5, q._6, q._7, q._8)
          case _: ((Nothing \^ Ld) <:< Tz) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head7[La, Lb, Lc, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_5", "_6", "_7", "_8")("")
            LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
            (source._2, q._2, q._3, source._1, q._5, q._6, q._7, q._8)
          case _: ((Nothing \^ Le) <:< Tz) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head7[La, Lb, Lc, Ld, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_6", "_7", "_8")("")
            LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
            (source._2, q._2, q._3, q._4, source._1, q._6, q._7, q._8)
          case _: ((Nothing \^ Lf) <:< Tz) => 
            LabelConflicts.unyq2[La, Lf]("_1", "_6")
            LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_7", "_8")("")
            LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
            (source._2, q._2, q._3, q._4, q._5, source._1, q._7, q._8)
          case _: ((Nothing \^ Lg) <:< Tz) => 
            LabelConflicts.unyq2[La, Lg]("_1", "_7")
            LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_8")("")
            LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, source._1, q._8)
          case _: ((Nothing \^ Lh) <:< Tz) => 
            LabelConflicts.unyq2[La, Lh]("_1", "_8")
            LabelConflicts.head7[La, Lb, Lc, Ld, Le, Lf, Lg]("_1", "_2", "_3", "_4", "_5", "_6", "_7")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, q._7, source._1)
          case _ =>
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head6[Lb, Ld, Le, Lf, Lg, Lh]("_2", "_4", "_5", "_6", "_7", "_8")("")
                LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
                (q._1, source._1, source._2, q._4, q._5, q._6, q._7, q._8)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head6[Lb, Lc, Le, Lf, Lg, Lh]("_2", "_3", "_5", "_6", "_7", "_8")("")
                LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
                (q._1, source._1, q._3, source._2, q._5, q._6, q._7, q._8)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head6[Lb, Lc, Ld, Lf, Lg, Lh]("_2", "_3", "_4", "_6", "_7", "_8")("")
                LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                (q._1, source._1, q._3, q._4, source._2, q._6, q._7, q._8)
              case _: ((Nothing \^ Lf) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                LabelConflicts.head6[Lb, Lc, Ld, Le, Lg, Lh]("_2", "_3", "_4", "_5", "_7", "_8")("")
                LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                (q._1, source._1, q._3, q._4, q._5, source._2, q._7, q._8)
              case _: ((Nothing \^ Lg) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lg]("_2", "_7")
                LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lh]("_2", "_3", "_4", "_5", "_6", "_8")("")
                LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, source._2, q._8)
              case _: ((Nothing \^ Lh) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lh]("_2", "_8")
                LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, q._7, source._2)
              case _ =>
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, q._7, q._8)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head6[Lb, Ld, Le, Lf, Lg, Lh]("_2", "_4", "_5", "_6", "_7", "_8")("")
                LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
                (q._1, source._2, source._1, q._4, q._5, q._6, q._7, q._8)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head6[Lb, Lc, Le, Lf, Lg, Lh]("_2", "_3", "_5", "_6", "_7", "_8")("")
                LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
                (q._1, source._2, q._3, source._1, q._5, q._6, q._7, q._8)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head6[Lb, Lc, Ld, Lf, Lg, Lh]("_2", "_3", "_4", "_6", "_7", "_8")("")
                LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                (q._1, source._2, q._3, q._4, source._1, q._6, q._7, q._8)
              case _: ((Nothing \^ Lf) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                LabelConflicts.head6[Lb, Lc, Ld, Le, Lg, Lh]("_2", "_3", "_4", "_5", "_7", "_8")("")
                LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                (q._1, source._2, q._3, q._4, q._5, source._1, q._7, q._8)
              case _: ((Nothing \^ Lg) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lg]("_2", "_7")
                LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lh]("_2", "_3", "_4", "_5", "_6", "_8")("")
                LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, source._1, q._8)
              case _: ((Nothing \^ Lh) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lh]("_2", "_8")
                LabelConflicts.head6[Lb, Lc, Ld, Le, Lf, Lg]("_2", "_3", "_4", "_5", "_6", "_7")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, q._7, source._1)
              case _ =>
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, q._7, q._8)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.head5[Lc, Le, Lf, Lg, Lh]("_3", "_5", "_6", "_7", "_8")("")
                    LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
                    (q._1, q._2, source._1, source._2, q._5, q._6, q._7, q._8)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.head5[Lc, Ld, Lf, Lg, Lh]("_3", "_4", "_6", "_7", "_8")("")
                    LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                    (q._1, q._2, source._1, q._4, source._2, q._6, q._7, q._8)
                  case _: ((Nothing \^ Lf) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                    LabelConflicts.head5[Lc, Ld, Le, Lg, Lh]("_3", "_4", "_5", "_7", "_8")("")
                    LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                    (q._1, q._2, source._1, q._4, q._5, source._2, q._7, q._8)
                  case _: ((Nothing \^ Lg) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lg]("_3", "_7")
                    LabelConflicts.head5[Lc, Ld, Le, Lf, Lh]("_3", "_4", "_5", "_6", "_8")("")
                    LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, source._2, q._8)
                  case _: ((Nothing \^ Lh) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lh]("_3", "_8")
                    LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, q._7, source._2)
                  case _ =>
                    LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, q._7, q._8)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.head5[Lc, Le, Lf, Lg, Lh]("_3", "_5", "_6", "_7", "_8")("")
                    LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
                    (q._1, q._2, source._2, source._1, q._5, q._6, q._7, q._8)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.head5[Lc, Ld, Lf, Lg, Lh]("_3", "_4", "_6", "_7", "_8")("")
                    LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                    (q._1, q._2, source._2, q._4, source._1, q._6, q._7, q._8)
                  case _: ((Nothing \^ Lf) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                    LabelConflicts.head5[Lc, Ld, Le, Lg, Lh]("_3", "_4", "_5", "_7", "_8")("")
                    LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                    (q._1, q._2, source._2, q._4, q._5, source._1, q._7, q._8)
                  case _: ((Nothing \^ Lg) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lg]("_3", "_7")
                    LabelConflicts.head5[Lc, Ld, Le, Lf, Lh]("_3", "_4", "_5", "_6", "_8")("")
                    LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, source._1, q._8)
                  case _: ((Nothing \^ Lh) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lh]("_3", "_8")
                    LabelConflicts.head5[Lc, Ld, Le, Lf, Lg]("_3", "_4", "_5", "_6", "_7")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, q._7, source._1)
                  case _ =>
                    LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, q._7, q._8)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        LabelConflicts.head4[Ld, Lf, Lg, Lh]("_4", "_6", "_7", "_8")("")
                        LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                        (q._1, q._2, q._3, source._1, source._2, q._6, q._7, q._8)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                        LabelConflicts.head4[Ld, Le, Lg, Lh]("_4", "_5", "_7", "_8")("")
                        LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                        (q._1, q._2, q._3, source._1, q._5, source._2, q._7, q._8)
                      case _: ((Nothing \^ Lg) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lg]("_4", "_7")
                        LabelConflicts.head4[Ld, Le, Lf, Lh]("_4", "_5", "_6", "_8")("")
                        LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, source._2, q._8)
                      case _: ((Nothing \^ Lh) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lh]("_4", "_8")
                        LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, q._7, source._2)
                      case _ =>
                        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, q._7, q._8)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        LabelConflicts.head4[Ld, Lf, Lg, Lh]("_4", "_6", "_7", "_8")("")
                        LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                        (q._1, q._2, q._3, source._2, source._1, q._6, q._7, q._8)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                        LabelConflicts.head4[Ld, Le, Lg, Lh]("_4", "_5", "_7", "_8")("")
                        LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                        (q._1, q._2, q._3, source._2, q._5, source._1, q._7, q._8)
                      case _: ((Nothing \^ Lg) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lg]("_4", "_7")
                        LabelConflicts.head4[Ld, Le, Lf, Lh]("_4", "_5", "_6", "_8")("")
                        LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, source._1, q._8)
                      case _: ((Nothing \^ Lh) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lh]("_4", "_8")
                        LabelConflicts.head4[Ld, Le, Lf, Lg]("_4", "_5", "_6", "_7")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, q._7, source._1)
                      case _ =>
                        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, q._7, q._8)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            LabelConflicts.head3[Le, Lg, Lh]("_5", "_7", "_8")("")
                            LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                            (q._1, q._2, q._3, q._4, source._1, source._2, q._7, q._8)
                          case _: ((Nothing \^ Lg) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Lg]("_5", "_7")
                            LabelConflicts.head3[Le, Lf, Lh]("_5", "_6", "_8")("")
                            LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, source._2, q._8)
                          case _: ((Nothing \^ Lh) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Lh]("_5", "_8")
                            LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, q._7, source._2)
                          case _ =>
                            LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, q._7, q._8)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            LabelConflicts.head3[Le, Lg, Lh]("_5", "_7", "_8")("")
                            LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                            (q._1, q._2, q._3, q._4, source._2, source._1, q._7, q._8)
                          case _: ((Nothing \^ Lg) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Lg]("_5", "_7")
                            LabelConflicts.head3[Le, Lf, Lh]("_5", "_6", "_8")("")
                            LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, source._1, q._8)
                          case _: ((Nothing \^ Lh) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Lh]("_5", "_8")
                            LabelConflicts.head3[Le, Lf, Lg]("_5", "_6", "_7")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, q._7, source._1)
                          case _ =>
                            LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, q._7, q._8)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Ty) => 
                                LabelConflicts.unyq2[Lf, Lg]("_6", "_7")
                                LabelConflicts.diff[Lf, Lh]("_6", "_8")("")
                                LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                                (q._1, q._2, q._3, q._4, q._5, source._1, source._2, q._8)
                              case _: ((Nothing \^ Lh) <:< Ty) => 
                                LabelConflicts.unyq2[Lf, Lh]("_6", "_8")
                                LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                                (q._1, q._2, q._3, q._4, q._5, source._1, q._7, source._2)
                              case _ =>
                                LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                                (q._1, q._2, q._3, q._4, q._5, source._1, q._7, q._8)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Tz) => 
                                LabelConflicts.unyq2[Lf, Lg]("_6", "_7")
                                LabelConflicts.diff[Lf, Lh]("_6", "_8")("")
                                LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                                (q._1, q._2, q._3, q._4, q._5, source._2, source._1, q._8)
                              case _: ((Nothing \^ Lh) <:< Tz) => 
                                LabelConflicts.unyq2[Lf, Lh]("_6", "_8")
                                LabelConflicts.diff[Lf, Lg]("_6", "_7")("")
                                (q._1, q._2, q._3, q._4, q._5, source._2, q._7, source._1)
                              case _ =>
                                LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                                (q._1, q._2, q._3, q._4, q._5, source._2, q._7, q._8)
                          case _ =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Tz) =>
                                summonFrom:
                                  case _: ((Nothing \^ Lh) <:< Ty) =>
                                    LabelConflicts.unyq2[Lg, Lh]("_7", "_8")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._1, source._2)
                                  case _ =>
                                    LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._1, q._8)
                              case _: ((Nothing \^ Lg) <:< Ty) =>
                                summonFrom:
                                  case _: ((Nothing \^ Lh) <:< Tz) =>
                                    LabelConflicts.unyq2[Lg, Lh]("_7", "_8")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._2, source._1)
                                  case _ =>
                                    LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._2, q._8)
                              case _ =>
                                summonFrom:
                                  case _: ((Nothing \^ Lh) <:< Tz) =>
                                    (q._1, q._2, q._3, q._4, q._5, q._6, q._7, source._1)
                                  case _: ((Nothing \^ Lh) <:< Ty) =>
                                    (q._1, q._2, q._3, q._4, q._5, q._6, q._7, source._2)
                                  case _ =>
                                    compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx](source: (Tz, Ty, Tx)): (A \^ La | Tz | Ty | Tx, B \^ Lb | Tz | Ty | Tx, C \^ Lc | Tz | Ty | Tx, D \^ Ld | Tz | Ty | Tx, E \^ Le | Tz | Ty | Tx, F \^ Lf | Tz | Ty | Tx, G \^ Lg | Tz | Ty | Tx, H \^ Lh | Tz | Ty | Tx) =
  compiletime.error("Implementation restrictions prevent name-update of 8-tuple by 3-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw](source: (Tz, Ty, Tx, Tw)): (A \^ La | Tz | Ty | Tx | Tw, B \^ Lb | Tz | Ty | Tx | Tw, C \^ Lc | Tz | Ty | Tx | Tw, D \^ Ld | Tz | Ty | Tx | Tw, E \^ Le | Tz | Ty | Tx | Tw, F \^ Lf | Tz | Ty | Tx | Tw, G \^ Lg | Tz | Ty | Tx | Tw, H \^ Lh | Tz | Ty | Tx | Tw) =
  compiletime.error("Implementation restrictions prevent name-update of 8-tuple by 4-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv](source: (Tz, Ty, Tx, Tw, Tv)): (A \^ La | Tz | Ty | Tx | Tw | Tv, B \^ Lb | Tz | Ty | Tx | Tw | Tv, C \^ Lc | Tz | Ty | Tx | Tw | Tv, D \^ Ld | Tz | Ty | Tx | Tw | Tv, E \^ Le | Tz | Ty | Tx | Tw | Tv, F \^ Lf | Tz | Ty | Tx | Tw | Tv, G \^ Lg | Tz | Ty | Tx | Tw | Tv, H \^ Lh | Tz | Ty | Tx | Tw | Tv) =
  compiletime.error("Implementation restrictions prevent name-update of 8-tuple by 5-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu](source: (Tz, Ty, Tx, Tw, Tv, Tu)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu, H \^ Lh | Tz | Ty | Tx | Tw | Tv | Tu) =
  compiletime.error("Implementation restrictions prevent name-update of 8-tuple by 6-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt, H \^ Lh | Tz | Ty | Tx | Tw | Tv | Tu | Tt) =
  compiletime.error("Implementation restrictions prevent name-update of 8-tuple by 7-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, H \^ Lh | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts) =
  compiletime.error("Implementation restrictions prevent name-update of 8-tuple by 8-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, H \^ Lh | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr) =
  compiletime.error("Implementation restrictions prevent name-update of 8-tuple by 9-tuple")

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

  inline def ~~(inline la: La, inline lb: Lb, inline lc: Lc, inline ld: Ld, inline le: Le, inline lf: Lf, inline lg: Lg, inline lh: Lh, inline li: Li): (A, B, C, D, E, F, G, H, I) = unlabel

  transparent inline def pick[Lz <: LabelVal](inline lz: Lz): ((A | B | C | D | E | F | G | H | I) \^ Lz) =
    summonFrom:
      case _: (Lz =:= La) =>
        LabelConflicts.head9[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
        q._1.asInstanceOf[(A \^ Lz)]
      case _: (Lz =:= Lb) =>
        LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
        q._2.asInstanceOf[(B \^ Lz)]
      case _: (Lz =:= Lc) =>
        LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
        q._3.asInstanceOf[(C \^ Lz)]
      case _: (Lz =:= Ld) =>
        LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
        q._4.asInstanceOf[(D \^ Lz)]
      case _: (Lz =:= Le) =>
        LabelConflicts.head5[Lz, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
        q._5.asInstanceOf[(E \^ Lz)]
      case _: (Lz =:= Lf) =>
        LabelConflicts.head4[Lz, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(lz))
        q._6.asInstanceOf[(F \^ Lz)]
      case _: (Lz =:= Lg) =>
        LabelConflicts.head3[Lz, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(lz))
        q._7.asInstanceOf[(G \^ Lz)]
      case _: (Lz =:= Lh) =>
        LabelConflicts.diff[Lz, Li]("_8", "_9")(" with " + codeOf(lz))
        q._8.asInstanceOf[(H \^ Lz)]
      case _: (Lz =:= Li) =>
        q._9.asInstanceOf[(I \^ Lz)]
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal](inline lz: Lz, inline ly: Ly): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly) =
    LabelConflicts.unik2(lz, ly)
    summonFrom:
      case _: (Lz =:= La) =>
        summonFrom:
          case _: (Ly =:= Lb) =>
            LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._1.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head8[Lz, Lb, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._1.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head8[Lz, Lb, Lc, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._1.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head8[Lz, Lb, Lc, Ld, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._1.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._1.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(ly))
            (q._1.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_9")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Li]("_8", "_9")(" with " + codeOf(ly))
            (q._1.unlabel, q._8.unlabel).label[Lz, Ly]
          case _: (Ly =:= Li) =>
            LabelConflicts.head8[Lz, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            (q._1.unlabel, q._9.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lb) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head8[Lz, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head8[Ly, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._2.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li]("_2", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._2.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head7[Lz, Lc, Le, Lf, Lg, Lh, Li]("_2", "_3", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._2.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head7[Lz, Lc, Ld, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._2.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head7[Lz, Lc, Ld, Le, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._2.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(ly))
            (q._2.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_9")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Li]("_8", "_9")(" with " + codeOf(ly))
            (q._2.unlabel, q._8.unlabel).label[Lz, Ly]
          case _: (Ly =:= Li) =>
            LabelConflicts.head7[Lz, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            (q._2.unlabel, q._9.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lc) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head8[Ly, Lb, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._3.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head7[Lz, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Ld, Le, Lf, Lg, Lh, Li]("_2", "_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._3.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li]("_3", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._3.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head6[Lz, Ld, Lf, Lg, Lh, Li]("_3", "_4", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._3.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head6[Lz, Ld, Le, Lg, Lh, Li]("_3", "_4", "_5", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._3.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head6[Lz, Ld, Le, Lf, Lh, Li]("_3", "_4", "_5", "_6", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(ly))
            (q._3.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Li]("_3", "_4", "_5", "_6", "_7", "_9")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Li]("_8", "_9")(" with " + codeOf(ly))
            (q._3.unlabel, q._8.unlabel).label[Lz, Ly]
          case _: (Ly =:= Li) =>
            LabelConflicts.head6[Lz, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            (q._3.unlabel, q._9.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Ld) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head8[Ly, Lb, Lc, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._4.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lc, Le, Lf, Lg, Lh, Li]("_2", "_3", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._4.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head6[Lz, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Le, Lf, Lg, Lh, Li]("_3", "_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._4.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head5[Lz, Lf, Lg, Lh, Li]("_4", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._4.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head5[Lz, Le, Lg, Lh, Li]("_4", "_5", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._4.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head5[Lz, Le, Lf, Lh, Li]("_4", "_5", "_6", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(ly))
            (q._4.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head5[Lz, Le, Lf, Lg, Li]("_4", "_5", "_6", "_7", "_9")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Li]("_8", "_9")(" with " + codeOf(ly))
            (q._4.unlabel, q._8.unlabel).label[Lz, Ly]
          case _: (Ly =:= Li) =>
            LabelConflicts.head5[Lz, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            (q._4.unlabel, q._9.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Le) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head5[Lz, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head8[Ly, Lb, Lc, Ld, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._5.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head5[Lz, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lc, Ld, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._5.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head5[Lz, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Ld, Lf, Lg, Lh, Li]("_3", "_4", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._5.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head5[Lz, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Lf, Lg, Lh, Li]("_4", "_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._5.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head4[Lz, Lg, Lh, Li]("_5", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._5.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head4[Lz, Lf, Lh, Li]("_5", "_6", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(ly))
            (q._5.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head4[Lz, Lf, Lg, Li]("_5", "_6", "_7", "_9")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Li]("_8", "_9")(" with " + codeOf(ly))
            (q._5.unlabel, q._8.unlabel).label[Lz, Ly]
          case _: (Ly =:= Li) =>
            LabelConflicts.head4[Lz, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(lz))
            (q._5.unlabel, q._9.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lf) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head4[Lz, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._6.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head4[Lz, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lc, Ld, Le, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._6.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head4[Lz, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Ld, Le, Lg, Lh, Li]("_3", "_4", "_5", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._6.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head4[Lz, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Le, Lg, Lh, Li]("_4", "_5", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._6.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head4[Lz, Lg, Lh, Li]("_6", "_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lg, Lh, Li]("_5", "_7", "_8", "_9")(" with " + codeOf(ly))
            (q._6.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.head3[Lz, Lh, Li]("_6", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(ly))
            (q._6.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.head3[Lz, Lg, Li]("_6", "_7", "_9")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Li]("_8", "_9")(" with " + codeOf(ly))
            (q._6.unlabel, q._8.unlabel).label[Lz, Ly]
          case _: (Ly =:= Li) =>
            LabelConflicts.head3[Lz, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(lz))
            (q._6.unlabel, q._9.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lg) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head3[Lz, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_8", "_9")(" with " + codeOf(ly))
            (q._7.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head3[Lz, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_8", "_9")(" with " + codeOf(ly))
            (q._7.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head3[Lz, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Ld, Le, Lf, Lh, Li]("_3", "_4", "_5", "_6", "_8", "_9")(" with " + codeOf(ly))
            (q._7.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head3[Lz, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Le, Lf, Lh, Li]("_4", "_5", "_6", "_8", "_9")(" with " + codeOf(ly))
            (q._7.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head3[Lz, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lf, Lh, Li]("_5", "_6", "_8", "_9")(" with " + codeOf(ly))
            (q._7.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head3[Lz, Lh, Li]("_7", "_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lh, Li]("_6", "_8", "_9")(" with " + codeOf(ly))
            (q._7.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            LabelConflicts.diff[Lz, Li]("_7", "_9")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Li]("_8", "_9")(" with " + codeOf(ly))
            (q._7.unlabel, q._8.unlabel).label[Lz, Ly]
          case _: (Ly =:= Li) =>
            LabelConflicts.diff[Lz, Lh]("_7", "_8")(" with " + codeOf(lz))
            (q._7.unlabel, q._9.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Lh) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.diff[Lz, Li]("_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_9")(" with " + codeOf(ly))
            (q._8.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.diff[Lz, Li]("_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_9")(" with " + codeOf(ly))
            (q._8.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.diff[Lz, Li]("_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Li]("_3", "_4", "_5", "_6", "_7", "_9")(" with " + codeOf(ly))
            (q._8.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.diff[Lz, Li]("_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head5[Ly, Le, Lf, Lg, Li]("_4", "_5", "_6", "_7", "_9")(" with " + codeOf(ly))
            (q._8.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.diff[Lz, Li]("_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head4[Ly, Lf, Lg, Li]("_5", "_6", "_7", "_9")(" with " + codeOf(ly))
            (q._8.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.diff[Lz, Li]("_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.head3[Ly, Lg, Li]("_6", "_7", "_9")(" with " + codeOf(ly))
            (q._8.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.diff[Lz, Li]("_8", "_9")(" with " + codeOf(lz))
            LabelConflicts.diff[Ly, Li]("_7", "_9")(" with " + codeOf(ly))
            (q._8.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Li) =>
            (q._8.unlabel, q._9.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _: (Lz =:= Li) =>
        summonFrom:
          case _: (Ly =:= La) =>
            LabelConflicts.head8[Ly, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._9.unlabel, q._1.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lb) =>
            LabelConflicts.head7[Ly, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._9.unlabel, q._2.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lc) =>
            LabelConflicts.head6[Ly, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._9.unlabel, q._3.unlabel).label[Lz, Ly]
          case _: (Ly =:= Ld) =>
            LabelConflicts.head5[Ly, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._9.unlabel, q._4.unlabel).label[Lz, Ly]
          case _: (Ly =:= Le) =>
            LabelConflicts.head4[Ly, Lf, Lg, Lh]("_5", "_6", "_7", "_8")(" with " + codeOf(ly))
            (q._9.unlabel, q._5.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lf) =>
            LabelConflicts.head3[Ly, Lg, Lh]("_6", "_7", "_8")(" with " + codeOf(ly))
            (q._9.unlabel, q._6.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lg) =>
            LabelConflicts.diff[Ly, Lh]("_7", "_8")(" with " + codeOf(ly))
            (q._9.unlabel, q._7.unlabel).label[Lz, Ly]
          case _: (Ly =:= Lh) =>
            (q._9.unlabel, q._8.unlabel).label[Lz, Ly]
          case _ => compiletime.error("No label found matching " + codeOf(ly))
      case _ => compiletime.error("No label found matching " + codeOf(lz))

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx) =
    compiletime.error("Implementation restrictions prevent 3-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw) =
    compiletime.error("Implementation restrictions prevent 4-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv) =
    compiletime.error("Implementation restrictions prevent 5-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv, (A | B | C | D | E | F | G | H | I) \^ Lu) =
    compiletime.error("Implementation restrictions prevent 6-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv, (A | B | C | D | E | F | G | H | I) \^ Lu, (A | B | C | D | E | F | G | H | I) \^ Lt) =
    compiletime.error("Implementation restrictions prevent 7-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal, Ls <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt, inline ls: Ls): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv, (A | B | C | D | E | F | G | H | I) \^ Lu, (A | B | C | D | E | F | G | H | I) \^ Lt, (A | B | C | D | E | F | G | H | I) \^ Ls) =
    compiletime.error("Implementation restrictions prevent 8-way picking by name")

  transparent inline def pick[Lz <: LabelVal, Ly <: LabelVal, Lx <: LabelVal, Lw <: LabelVal, Lv <: LabelVal, Lu <: LabelVal, Lt <: LabelVal, Ls <: LabelVal, Lr <: LabelVal](inline lz: Lz, inline ly: Ly, inline lx: Lx, inline lw: Lw, inline lv: Lv, inline lu: Lu, inline lt: Lt, inline ls: Ls, inline lr: Lr): ((A | B | C | D | E | F | G | H | I) \^ Lz, (A | B | C | D | E | F | G | H | I) \^ Ly, (A | B | C | D | E | F | G | H | I) \^ Lx, (A | B | C | D | E | F | G | H | I) \^ Lw, (A | B | C | D | E | F | G | H | I) \^ Lv, (A | B | C | D | E | F | G | H | I) \^ Lu, (A | B | C | D | E | F | G | H | I) \^ Lt, (A | B | C | D | E | F | G | H | I) \^ Ls, (A | B | C | D | E | F | G | H | I) \^ Lr) =
    compiletime.error("Implementation restrictions prevent 9-way picking by name")

  transparent inline def updatedBy[Tz](source: Tz): (A \^ La | Tz, B \^ Lb | Tz, C \^ Lc | Tz, D \^ Ld | Tz, E \^ Le | Tz, F \^ Lf | Tz, G \^ Lg | Tz, H \^ Lh | Tz, I \^ Li | Tz) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) => 
        LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
        (source, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: ((Nothing \^ Lb) <:< Tz) => 
        LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
        (q._1, source, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: ((Nothing \^ Lc) <:< Tz) => 
        LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
        (q._1, q._2, source, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: ((Nothing \^ Ld) <:< Tz) => 
        LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
        (q._1, q._2, q._3, source, q._5, q._6, q._7, q._8, q._9)
      case _: ((Nothing \^ Le) <:< Tz) => 
        LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
        (q._1, q._2, q._3, q._4, source, q._6, q._7, q._8, q._9)
      case _: ((Nothing \^ Lf) <:< Tz) => 
        LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
        (q._1, q._2, q._3, q._4, q._5, source, q._7, q._8, q._9)
      case _: ((Nothing \^ Lg) <:< Tz) => 
        LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
        (q._1, q._2, q._3, q._4, q._5, q._6, source, q._8, q._9)
      case _: ((Nothing \^ Lh) <:< Tz) => 
        LabelConflicts.diff[Lh, Li]("_8", "_9")("")
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, source, q._9)
      case _: ((Nothing \^ Li) <:< Tz) => 
        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, source)
      case _ =>
        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty](source: (Tz, Ty)): (A \^ La | Tz | Ty, B \^ Lb | Tz | Ty, C \^ Lc | Tz | Ty, D \^ Ld | Tz | Ty, E \^ Le | Tz | Ty, F \^ Lf | Tz | Ty, G \^ Lg | Tz | Ty, H \^ Lh | Tz | Ty, I \^ Li | Tz | Ty) =
    summonFrom:
      case _: ((Nothing \^ La) <:< Tz) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Ty) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head8[La, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
            LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
            (source._1, source._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Lc) <:< Ty) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head8[La, Lb, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_4", "_5", "_6", "_7", "_8", "_9")("")
            LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
            (source._1, q._2, source._2, q._4, q._5, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Ld) <:< Ty) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head8[La, Lb, Lc, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_5", "_6", "_7", "_8", "_9")("")
            LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
            (source._1, q._2, q._3, source._2, q._5, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Le) <:< Ty) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head8[La, Lb, Lc, Ld, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_6", "_7", "_8", "_9")("")
            LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
            (source._1, q._2, q._3, q._4, source._2, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Lf) <:< Ty) => 
            LabelConflicts.unyq2[La, Lf]("_1", "_6")
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_7", "_8", "_9")("")
            LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
            (source._1, q._2, q._3, q._4, q._5, source._2, q._7, q._8, q._9)
          case _: ((Nothing \^ Lg) <:< Ty) => 
            LabelConflicts.unyq2[La, Lg]("_1", "_7")
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_8", "_9")("")
            LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, source._2, q._8, q._9)
          case _: ((Nothing \^ Lh) <:< Ty) => 
            LabelConflicts.unyq2[La, Lh]("_1", "_8")
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_9")("")
            LabelConflicts.diff[Lh, Li]("_8", "_9")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, q._7, source._2, q._9)
          case _: ((Nothing \^ Li) <:< Ty) => 
            LabelConflicts.unyq2[La, Li]("_1", "_9")
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, source._2)
          case _ =>
            LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
            (source._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
      case _: ((Nothing \^ La) <:< Ty) =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) => 
            LabelConflicts.unyq2[La, Lb]("_1", "_2")
            LabelConflicts.head8[La, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
            LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
            (source._2, source._1, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Lc) <:< Tz) => 
            LabelConflicts.unyq2[La, Lc]("_1", "_3")
            LabelConflicts.head8[La, Lb, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_4", "_5", "_6", "_7", "_8", "_9")("")
            LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
            (source._2, q._2, source._1, q._4, q._5, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Ld) <:< Tz) => 
            LabelConflicts.unyq2[La, Ld]("_1", "_4")
            LabelConflicts.head8[La, Lb, Lc, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_5", "_6", "_7", "_8", "_9")("")
            LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
            (source._2, q._2, q._3, source._1, q._5, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Le) <:< Tz) => 
            LabelConflicts.unyq2[La, Le]("_1", "_5")
            LabelConflicts.head8[La, Lb, Lc, Ld, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_6", "_7", "_8", "_9")("")
            LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
            (source._2, q._2, q._3, q._4, source._1, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Lf) <:< Tz) => 
            LabelConflicts.unyq2[La, Lf]("_1", "_6")
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_7", "_8", "_9")("")
            LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
            (source._2, q._2, q._3, q._4, q._5, source._1, q._7, q._8, q._9)
          case _: ((Nothing \^ Lg) <:< Tz) => 
            LabelConflicts.unyq2[La, Lg]("_1", "_7")
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_8", "_9")("")
            LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, source._1, q._8, q._9)
          case _: ((Nothing \^ Lh) <:< Tz) => 
            LabelConflicts.unyq2[La, Lh]("_1", "_8")
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_9")("")
            LabelConflicts.diff[Lh, Li]("_8", "_9")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, q._7, source._1, q._9)
          case _: ((Nothing \^ Li) <:< Tz) => 
            LabelConflicts.unyq2[La, Li]("_1", "_9")
            LabelConflicts.head8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, q._7, q._8, source._1)
          case _ =>
            LabelConflicts.head9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_1", "_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
            (source._2, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
      case _ =>
        summonFrom:
          case _: ((Nothing \^ Lb) <:< Tz) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head7[Lb, Ld, Le, Lf, Lg, Lh, Li]("_2", "_4", "_5", "_6", "_7", "_8", "_9")("")
                LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
                (q._1, source._1, source._2, q._4, q._5, q._6, q._7, q._8, q._9)
              case _: ((Nothing \^ Ld) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head7[Lb, Lc, Le, Lf, Lg, Lh, Li]("_2", "_3", "_5", "_6", "_7", "_8", "_9")("")
                LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
                (q._1, source._1, q._3, source._2, q._5, q._6, q._7, q._8, q._9)
              case _: ((Nothing \^ Le) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head7[Lb, Lc, Ld, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_6", "_7", "_8", "_9")("")
                LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
                (q._1, source._1, q._3, q._4, source._2, q._6, q._7, q._8, q._9)
              case _: ((Nothing \^ Lf) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_7", "_8", "_9")("")
                LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                (q._1, source._1, q._3, q._4, q._5, source._2, q._7, q._8, q._9)
              case _: ((Nothing \^ Lg) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lg]("_2", "_7")
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_8", "_9")("")
                LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, source._2, q._8, q._9)
              case _: ((Nothing \^ Lh) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Lh]("_2", "_8")
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_9")("")
                LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, q._7, source._2, q._9)
              case _: ((Nothing \^ Li) <:< Ty) => 
                LabelConflicts.unyq2[Lb, Li]("_2", "_9")
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, q._7, q._8, source._2)
              case _ =>
                LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
                (q._1, source._1, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
          case _: ((Nothing \^ Lb) <:< Ty) =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lc]("_2", "_3")
                LabelConflicts.head7[Lb, Ld, Le, Lf, Lg, Lh, Li]("_2", "_4", "_5", "_6", "_7", "_8", "_9")("")
                LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
                (q._1, source._2, source._1, q._4, q._5, q._6, q._7, q._8, q._9)
              case _: ((Nothing \^ Ld) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Ld]("_2", "_4")
                LabelConflicts.head7[Lb, Lc, Le, Lf, Lg, Lh, Li]("_2", "_3", "_5", "_6", "_7", "_8", "_9")("")
                LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
                (q._1, source._2, q._3, source._1, q._5, q._6, q._7, q._8, q._9)
              case _: ((Nothing \^ Le) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Le]("_2", "_5")
                LabelConflicts.head7[Lb, Lc, Ld, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_6", "_7", "_8", "_9")("")
                LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
                (q._1, source._2, q._3, q._4, source._1, q._6, q._7, q._8, q._9)
              case _: ((Nothing \^ Lf) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lf]("_2", "_6")
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_7", "_8", "_9")("")
                LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                (q._1, source._2, q._3, q._4, q._5, source._1, q._7, q._8, q._9)
              case _: ((Nothing \^ Lg) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lg]("_2", "_7")
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_8", "_9")("")
                LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, source._1, q._8, q._9)
              case _: ((Nothing \^ Lh) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Lh]("_2", "_8")
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_9")("")
                LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, q._7, source._1, q._9)
              case _: ((Nothing \^ Li) <:< Tz) => 
                LabelConflicts.unyq2[Lb, Li]("_2", "_9")
                LabelConflicts.head7[Lb, Lc, Ld, Le, Lf, Lg, Lh]("_2", "_3", "_4", "_5", "_6", "_7", "_8")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, q._7, q._8, source._1)
              case _ =>
                LabelConflicts.head8[Lb, Lc, Ld, Le, Lf, Lg, Lh, Li]("_2", "_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
                (q._1, source._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
          case _ =>
            summonFrom:
              case _: ((Nothing \^ Lc) <:< Tz) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.head6[Lc, Le, Lf, Lg, Lh, Li]("_3", "_5", "_6", "_7", "_8", "_9")("")
                    LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
                    (q._1, q._2, source._1, source._2, q._5, q._6, q._7, q._8, q._9)
                  case _: ((Nothing \^ Le) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.head6[Lc, Ld, Lf, Lg, Lh, Li]("_3", "_4", "_6", "_7", "_8", "_9")("")
                    LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
                    (q._1, q._2, source._1, q._4, source._2, q._6, q._7, q._8, q._9)
                  case _: ((Nothing \^ Lf) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                    LabelConflicts.head6[Lc, Ld, Le, Lg, Lh, Li]("_3", "_4", "_5", "_7", "_8", "_9")("")
                    LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                    (q._1, q._2, source._1, q._4, q._5, source._2, q._7, q._8, q._9)
                  case _: ((Nothing \^ Lg) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lg]("_3", "_7")
                    LabelConflicts.head6[Lc, Ld, Le, Lf, Lh, Li]("_3", "_4", "_5", "_6", "_8", "_9")("")
                    LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, source._2, q._8, q._9)
                  case _: ((Nothing \^ Lh) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Lh]("_3", "_8")
                    LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Li]("_3", "_4", "_5", "_6", "_7", "_9")("")
                    LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, q._7, source._2, q._9)
                  case _: ((Nothing \^ Li) <:< Ty) => 
                    LabelConflicts.unyq2[Lc, Li]("_3", "_9")
                    LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, q._7, q._8, source._2)
                  case _ =>
                    LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
                    (q._1, q._2, source._1, q._4, q._5, q._6, q._7, q._8, q._9)
              case _: ((Nothing \^ Lc) <:< Ty) =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Ld]("_3", "_4")
                    LabelConflicts.head6[Lc, Le, Lf, Lg, Lh, Li]("_3", "_5", "_6", "_7", "_8", "_9")("")
                    LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
                    (q._1, q._2, source._2, source._1, q._5, q._6, q._7, q._8, q._9)
                  case _: ((Nothing \^ Le) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Le]("_3", "_5")
                    LabelConflicts.head6[Lc, Ld, Lf, Lg, Lh, Li]("_3", "_4", "_6", "_7", "_8", "_9")("")
                    LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
                    (q._1, q._2, source._2, q._4, source._1, q._6, q._7, q._8, q._9)
                  case _: ((Nothing \^ Lf) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lf]("_3", "_6")
                    LabelConflicts.head6[Lc, Ld, Le, Lg, Lh, Li]("_3", "_4", "_5", "_7", "_8", "_9")("")
                    LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                    (q._1, q._2, source._2, q._4, q._5, source._1, q._7, q._8, q._9)
                  case _: ((Nothing \^ Lg) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lg]("_3", "_7")
                    LabelConflicts.head6[Lc, Ld, Le, Lf, Lh, Li]("_3", "_4", "_5", "_6", "_8", "_9")("")
                    LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, source._1, q._8, q._9)
                  case _: ((Nothing \^ Lh) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Lh]("_3", "_8")
                    LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Li]("_3", "_4", "_5", "_6", "_7", "_9")("")
                    LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, q._7, source._1, q._9)
                  case _: ((Nothing \^ Li) <:< Tz) => 
                    LabelConflicts.unyq2[Lc, Li]("_3", "_9")
                    LabelConflicts.head6[Lc, Ld, Le, Lf, Lg, Lh]("_3", "_4", "_5", "_6", "_7", "_8")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, q._7, q._8, source._1)
                  case _ =>
                    LabelConflicts.head7[Lc, Ld, Le, Lf, Lg, Lh, Li]("_3", "_4", "_5", "_6", "_7", "_8", "_9")("")
                    (q._1, q._2, source._2, q._4, q._5, q._6, q._7, q._8, q._9)
              case _ =>
                summonFrom:
                  case _: ((Nothing \^ Ld) <:< Tz) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        LabelConflicts.head5[Ld, Lf, Lg, Lh, Li]("_4", "_6", "_7", "_8", "_9")("")
                        LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
                        (q._1, q._2, q._3, source._1, source._2, q._6, q._7, q._8, q._9)
                      case _: ((Nothing \^ Lf) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                        LabelConflicts.head5[Ld, Le, Lg, Lh, Li]("_4", "_5", "_7", "_8", "_9")("")
                        LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                        (q._1, q._2, q._3, source._1, q._5, source._2, q._7, q._8, q._9)
                      case _: ((Nothing \^ Lg) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lg]("_4", "_7")
                        LabelConflicts.head5[Ld, Le, Lf, Lh, Li]("_4", "_5", "_6", "_8", "_9")("")
                        LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, source._2, q._8, q._9)
                      case _: ((Nothing \^ Lh) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Lh]("_4", "_8")
                        LabelConflicts.head5[Ld, Le, Lf, Lg, Li]("_4", "_5", "_6", "_7", "_9")("")
                        LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, q._7, source._2, q._9)
                      case _: ((Nothing \^ Li) <:< Ty) => 
                        LabelConflicts.unyq2[Ld, Li]("_4", "_9")
                        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, q._7, q._8, source._2)
                      case _ =>
                        LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
                        (q._1, q._2, q._3, source._1, q._5, q._6, q._7, q._8, q._9)
                  case _: ((Nothing \^ Ld) <:< Ty) =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Le]("_4", "_5")
                        LabelConflicts.head5[Ld, Lf, Lg, Lh, Li]("_4", "_6", "_7", "_8", "_9")("")
                        LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
                        (q._1, q._2, q._3, source._2, source._1, q._6, q._7, q._8, q._9)
                      case _: ((Nothing \^ Lf) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lf]("_4", "_6")
                        LabelConflicts.head5[Ld, Le, Lg, Lh, Li]("_4", "_5", "_7", "_8", "_9")("")
                        LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                        (q._1, q._2, q._3, source._2, q._5, source._1, q._7, q._8, q._9)
                      case _: ((Nothing \^ Lg) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lg]("_4", "_7")
                        LabelConflicts.head5[Ld, Le, Lf, Lh, Li]("_4", "_5", "_6", "_8", "_9")("")
                        LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, source._1, q._8, q._9)
                      case _: ((Nothing \^ Lh) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Lh]("_4", "_8")
                        LabelConflicts.head5[Ld, Le, Lf, Lg, Li]("_4", "_5", "_6", "_7", "_9")("")
                        LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, q._7, source._1, q._9)
                      case _: ((Nothing \^ Li) <:< Tz) => 
                        LabelConflicts.unyq2[Ld, Li]("_4", "_9")
                        LabelConflicts.head5[Ld, Le, Lf, Lg, Lh]("_4", "_5", "_6", "_7", "_8")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, q._7, q._8, source._1)
                      case _ =>
                        LabelConflicts.head6[Ld, Le, Lf, Lg, Lh, Li]("_4", "_5", "_6", "_7", "_8", "_9")("")
                        (q._1, q._2, q._3, source._2, q._5, q._6, q._7, q._8, q._9)
                  case _ =>
                    summonFrom:
                      case _: ((Nothing \^ Le) <:< Tz) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            LabelConflicts.head4[Le, Lg, Lh, Li]("_5", "_7", "_8", "_9")("")
                            LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                            (q._1, q._2, q._3, q._4, source._1, source._2, q._7, q._8, q._9)
                          case _: ((Nothing \^ Lg) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Lg]("_5", "_7")
                            LabelConflicts.head4[Le, Lf, Lh, Li]("_5", "_6", "_8", "_9")("")
                            LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, source._2, q._8, q._9)
                          case _: ((Nothing \^ Lh) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Lh]("_5", "_8")
                            LabelConflicts.head4[Le, Lf, Lg, Li]("_5", "_6", "_7", "_9")("")
                            LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, q._7, source._2, q._9)
                          case _: ((Nothing \^ Li) <:< Ty) => 
                            LabelConflicts.unyq2[Le, Li]("_5", "_9")
                            LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, q._7, q._8, source._2)
                          case _ =>
                            LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
                            (q._1, q._2, q._3, q._4, source._1, q._6, q._7, q._8, q._9)
                      case _: ((Nothing \^ Le) <:< Ty) =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Lf]("_5", "_6")
                            LabelConflicts.head4[Le, Lg, Lh, Li]("_5", "_7", "_8", "_9")("")
                            LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                            (q._1, q._2, q._3, q._4, source._2, source._1, q._7, q._8, q._9)
                          case _: ((Nothing \^ Lg) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Lg]("_5", "_7")
                            LabelConflicts.head4[Le, Lf, Lh, Li]("_5", "_6", "_8", "_9")("")
                            LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, source._1, q._8, q._9)
                          case _: ((Nothing \^ Lh) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Lh]("_5", "_8")
                            LabelConflicts.head4[Le, Lf, Lg, Li]("_5", "_6", "_7", "_9")("")
                            LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, q._7, source._1, q._9)
                          case _: ((Nothing \^ Li) <:< Tz) => 
                            LabelConflicts.unyq2[Le, Li]("_5", "_9")
                            LabelConflicts.head4[Le, Lf, Lg, Lh]("_5", "_6", "_7", "_8")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, q._7, q._8, source._1)
                          case _ =>
                            LabelConflicts.head5[Le, Lf, Lg, Lh, Li]("_5", "_6", "_7", "_8", "_9")("")
                            (q._1, q._2, q._3, q._4, source._2, q._6, q._7, q._8, q._9)
                      case _ =>
                        summonFrom:
                          case _: ((Nothing \^ Lf) <:< Tz) =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Ty) => 
                                LabelConflicts.unyq2[Lf, Lg]("_6", "_7")
                                LabelConflicts.head3[Lf, Lh, Li]("_6", "_8", "_9")("")
                                LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                                (q._1, q._2, q._3, q._4, q._5, source._1, source._2, q._8, q._9)
                              case _: ((Nothing \^ Lh) <:< Ty) => 
                                LabelConflicts.unyq2[Lf, Lh]("_6", "_8")
                                LabelConflicts.head3[Lf, Lg, Li]("_6", "_7", "_9")("")
                                LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                                (q._1, q._2, q._3, q._4, q._5, source._1, q._7, source._2, q._9)
                              case _: ((Nothing \^ Li) <:< Ty) => 
                                LabelConflicts.unyq2[Lf, Li]("_6", "_9")
                                LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                                (q._1, q._2, q._3, q._4, q._5, source._1, q._7, q._8, source._2)
                              case _ =>
                                LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                                (q._1, q._2, q._3, q._4, q._5, source._1, q._7, q._8, q._9)
                          case _: ((Nothing \^ Lf) <:< Ty) =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Tz) => 
                                LabelConflicts.unyq2[Lf, Lg]("_6", "_7")
                                LabelConflicts.head3[Lf, Lh, Li]("_6", "_8", "_9")("")
                                LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                                (q._1, q._2, q._3, q._4, q._5, source._2, source._1, q._8, q._9)
                              case _: ((Nothing \^ Lh) <:< Tz) => 
                                LabelConflicts.unyq2[Lf, Lh]("_6", "_8")
                                LabelConflicts.head3[Lf, Lg, Li]("_6", "_7", "_9")("")
                                LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                                (q._1, q._2, q._3, q._4, q._5, source._2, q._7, source._1, q._9)
                              case _: ((Nothing \^ Li) <:< Tz) => 
                                LabelConflicts.unyq2[Lf, Li]("_6", "_9")
                                LabelConflicts.head3[Lf, Lg, Lh]("_6", "_7", "_8")("")
                                (q._1, q._2, q._3, q._4, q._5, source._2, q._7, q._8, source._1)
                              case _ =>
                                LabelConflicts.head4[Lf, Lg, Lh, Li]("_6", "_7", "_8", "_9")("")
                                (q._1, q._2, q._3, q._4, q._5, source._2, q._7, q._8, q._9)
                          case _ =>
                            summonFrom:
                              case _: ((Nothing \^ Lg) <:< Tz) =>
                                summonFrom:
                                  case _: ((Nothing \^ Lh) <:< Ty) => 
                                    LabelConflicts.unyq2[Lg, Lh]("_7", "_8")
                                    LabelConflicts.diff[Lg, Li]("_7", "_9")("")
                                    LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._1, source._2, q._9)
                                  case _: ((Nothing \^ Li) <:< Ty) => 
                                    LabelConflicts.unyq2[Lg, Li]("_7", "_9")
                                    LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._1, q._8, source._2)
                                  case _ =>
                                    LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._1, q._8, q._9)
                              case _: ((Nothing \^ Lg) <:< Ty) =>
                                summonFrom:
                                  case _: ((Nothing \^ Lh) <:< Tz) => 
                                    LabelConflicts.unyq2[Lg, Lh]("_7", "_8")
                                    LabelConflicts.diff[Lg, Li]("_7", "_9")("")
                                    LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._2, source._1, q._9)
                                  case _: ((Nothing \^ Li) <:< Tz) => 
                                    LabelConflicts.unyq2[Lg, Li]("_7", "_9")
                                    LabelConflicts.diff[Lg, Lh]("_7", "_8")("")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._2, q._8, source._1)
                                  case _ =>
                                    LabelConflicts.head3[Lg, Lh, Li]("_7", "_8", "_9")("")
                                    (q._1, q._2, q._3, q._4, q._5, q._6, source._2, q._8, q._9)
                              case _ =>
                                summonFrom:
                                  case _: ((Nothing \^ Lh) <:< Tz) =>
                                    summonFrom:
                                      case _: ((Nothing \^ Li) <:< Ty) =>
                                        LabelConflicts.unyq2[Lh, Li]("_8", "_9")
                                        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, source._1, source._2)
                                      case _ =>
                                        LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                                        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, source._1, q._9)
                                  case _: ((Nothing \^ Lh) <:< Ty) =>
                                    summonFrom:
                                      case _: ((Nothing \^ Li) <:< Tz) =>
                                        LabelConflicts.unyq2[Lh, Li]("_8", "_9")
                                        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, source._2, source._1)
                                      case _ =>
                                        LabelConflicts.diff[Lh, Li]("_8", "_9")("")
                                        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, source._2, q._9)
                                  case _ =>
                                    summonFrom:
                                      case _: ((Nothing \^ Li) <:< Tz) =>
                                        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, source._1)
                                      case _: ((Nothing \^ Li) <:< Ty) =>
                                        (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, source._2)
                                      case _ =>
                                        compiletime.error("No matching labels so no update possible")

  transparent inline def updatedBy[Tz, Ty, Tx](source: (Tz, Ty, Tx)): (A \^ La | Tz | Ty | Tx, B \^ Lb | Tz | Ty | Tx, C \^ Lc | Tz | Ty | Tx, D \^ Ld | Tz | Ty | Tx, E \^ Le | Tz | Ty | Tx, F \^ Lf | Tz | Ty | Tx, G \^ Lg | Tz | Ty | Tx, H \^ Lh | Tz | Ty | Tx, I \^ Li | Tz | Ty | Tx) =
  compiletime.error("Implementation restrictions prevent name-update of 9-tuple by 3-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw](source: (Tz, Ty, Tx, Tw)): (A \^ La | Tz | Ty | Tx | Tw, B \^ Lb | Tz | Ty | Tx | Tw, C \^ Lc | Tz | Ty | Tx | Tw, D \^ Ld | Tz | Ty | Tx | Tw, E \^ Le | Tz | Ty | Tx | Tw, F \^ Lf | Tz | Ty | Tx | Tw, G \^ Lg | Tz | Ty | Tx | Tw, H \^ Lh | Tz | Ty | Tx | Tw, I \^ Li | Tz | Ty | Tx | Tw) =
  compiletime.error("Implementation restrictions prevent name-update of 9-tuple by 4-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv](source: (Tz, Ty, Tx, Tw, Tv)): (A \^ La | Tz | Ty | Tx | Tw | Tv, B \^ Lb | Tz | Ty | Tx | Tw | Tv, C \^ Lc | Tz | Ty | Tx | Tw | Tv, D \^ Ld | Tz | Ty | Tx | Tw | Tv, E \^ Le | Tz | Ty | Tx | Tw | Tv, F \^ Lf | Tz | Ty | Tx | Tw | Tv, G \^ Lg | Tz | Ty | Tx | Tw | Tv, H \^ Lh | Tz | Ty | Tx | Tw | Tv, I \^ Li | Tz | Ty | Tx | Tw | Tv) =
  compiletime.error("Implementation restrictions prevent name-update of 9-tuple by 5-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu](source: (Tz, Ty, Tx, Tw, Tv, Tu)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu, H \^ Lh | Tz | Ty | Tx | Tw | Tv | Tu, I \^ Li | Tz | Ty | Tx | Tw | Tv | Tu) =
  compiletime.error("Implementation restrictions prevent name-update of 9-tuple by 6-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt, H \^ Lh | Tz | Ty | Tx | Tw | Tv | Tu | Tt, I \^ Li | Tz | Ty | Tx | Tw | Tv | Tu | Tt) =
  compiletime.error("Implementation restrictions prevent name-update of 9-tuple by 7-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, H \^ Lh | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts, I \^ Li | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts) =
  compiletime.error("Implementation restrictions prevent name-update of 9-tuple by 8-tuple")

  transparent inline def updatedBy[Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr](source: (Tz, Ty, Tx, Tw, Tv, Tu, Tt, Ts, Tr)): (A \^ La | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, B \^ Lb | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, C \^ Lc | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, D \^ Ld | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, E \^ Le | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, F \^ Lf | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, G \^ Lg | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, H \^ Lh | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr, I \^ Li | Tz | Ty | Tx | Tw | Tv | Tu | Tt | Ts | Tr) =
  compiletime.error("Implementation restrictions prevent name-update of 9-tuple by 9-tuple")

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
