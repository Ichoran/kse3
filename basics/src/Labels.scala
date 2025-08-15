// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023-25 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.labels


//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.util.NotGiven
import scala.compiletime.{codeOf, summonInline, summonFrom, erasedValue, constValue, constValueOpt, constValueTuple}
import scala.annotation.targetName

import kse.basics.{LabelStr, \ => \^, \< => \<^, \> => \>^ }



object ExplicitTypeIndices {
  type UpTo1[A, Z] <: 0 | 1 = Z match
    case A => 0
    case _ => 1

  type UpTo2[A, B, Z] = Z match
    case A => 0
    case B => 1
    case _ => 2

  type UpTo3[A, B, C, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case _ => 3

  type UpTo4[A, B, C, D, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case _ => 4

  type UpTo5[A, B, C, D, E, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case _ => 5

  type UpTo6[A, B, C, D, E, F, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case _ => 6

  type UpTo7[A, B, C, D, E, F, G, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case _ => 7

  type UpTo8[A, B, C, D, E, F, G, H, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case _ => 8

  type UpTo9[A, B, C, D, E, F, G, H, I, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case _ => 9

  type UpTo10[A, B, C, D, E, F, G, H, I, J, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case _ => 10

  type UpTo11[A, B, C, D, E, F, G, H, I, J, K, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case _ => 11

  type UpTo12[A, B, C, D, E, F, G, H, I, J, K, L, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case _ => 12

  type UpTo13[A, B, C, D, E, F, G, H, I, J, K, L, M, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case _ => 13

  type UpTo14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case _ => 14

  type UpTo15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case O => 14
    case _ => 15

  type UpTo16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case O => 14
    case P => 15
    case _ => 16

  type UpTo17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case O => 14
    case P => 15
    case Q => 16
    case _ => 17

  type UpTo18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case O => 14
    case P => 15
    case Q => 16
    case R => 17
    case _ => 18

  type UpTo19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case O => 14
    case P => 15
    case Q => 16
    case R => 17
    case S => 18
    case _ => 19

  type UpTo20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case O => 14
    case P => 15
    case Q => 16
    case R => 17
    case S => 18
    case T => 19
    case _ => 20

  type UpTo21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case O => 14
    case P => 15
    case Q => 16
    case R => 17
    case S => 18
    case T => 19
    case U => 20
    case _ => 21

  type UpTo22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z] = Z match
    case A => 0
    case B => 1
    case C => 2
    case D => 3
    case E => 4
    case F => 5
    case G => 6
    case H => 7
    case I => 8
    case J => 9
    case K => 10
    case L => 11
    case M => 12
    case N => 13
    case O => 14
    case P => 15
    case Q => 16
    case R => 17
    case S => 18
    case T => 19
    case U => 20
    case V => 21
    case _ => 22
}



object NamesAndLabels {
  import compiletime.ops.any.*
  import compiletime.ops.boolean.*
  import compiletime.ops.int.*
  import NamedTuple.{NamedTuple => NTup}

  type LabeledValue[Ns <: Tuple, Vs <: Tuple, L <: LabelStr] <: (Any \^ L) = Ns match
    case EmptyTuple => Nothing
    case L *: _     => (Tuple.Head[Vs] \^ L)
    case _ *: tail  => LabeledValue[tail, Tuple.Tail[Vs], L]

  type IndexOfType[Ns <: Tuple, L, N <: Int] <: Int = Ns match
    case EmptyTuple => -1
    case L *: _     => N
    case _ *: tail  => IndexOfType[tail, L, N+1]

  transparent inline def byLabel[Ns <: Tuple, Vs <: Tuple, L <: LabelStr](tup: NTup[Ns, Vs], inline lb: L): LabeledValue[Ns, Vs, L] =
    inline constValue[IndexOfType[Ns, L, 0]] match
      case -1 => compiletime.error("No tuple fields by the name of " + constValue[L])
      case i => tup(i).asInstanceOf[LabeledValue[Ns, Vs, L]]

  transparent inline def indexByLabel[Ts <: Tuple](tup: Ts)[L <: LabelStr, N <: Int]: Int =
    inline if constValue[Tuple.Size[Ts]] <= constValue[N] then -1
    else summonFrom:
      case _: (Tuple.Elem[Ts, N] <:< (Any \^ L)) => constValue[N]
      case _ => indexByLabel[Ts](tup)[L, N+1]

  type NamesToLabels[Ns <: Tuple, Vs <: Tuple] <: Tuple = Ns match
    case EmptyTuple => EmptyTuple
    case l *: rest  => (Tuple.Head[Vs] \^ l) *: NamesToLabels[rest, Tuple.Tail[Vs]]

  transparent inline def toLabeled[Ns <: Tuple, Vs <: Tuple](tup: NTup[Ns, Vs]): NamesToLabels[Ns, Vs] =
    inline constValueOpt[Tuple.Size[Ns]] match
      case None => compiletime.error("Use named tuples only")
      case _ => inline constValue[Tuple.Size[Ns]] match
        case 0 => compiletime.error("Use nonempty named tuples only")
        case _ => tup.asInstanceOf[NamesToLabels[Ns, Vs]]

  type LabeledAsNames[Ts <: Tuple] <: Tuple = Ts match
    case EmptyTuple => EmptyTuple
    case (_ \^ lb) *: rest => lb *: LabeledAsNames[rest]

  type LabeledAsValues[Ts <: Tuple] <: Tuple = Ts match
    case EmptyTuple => EmptyTuple
    case (v \^ _) *: rest => v *: LabeledAsValues[rest]

  type NameIfPresent[T, Ts <: Tuple] <: String = Ts match
    case EmptyTuple => T match
      case "" => "\"\""
      case _  => ""
    case "" *: rest => "\"\""
    case T *: rest => compiletime.ops.any.ToString[T]
    case _ *: rest => NameIfPresent[T, rest]

  type NameIfAbsent[T, Ts <: Tuple] <: String = Ts match
    case EmptyTuple => compiletime.ops.any.ToString[T]
    case T *: rest => ""
    case _ *: rest => NameIfAbsent[T, rest]

  type NameOfDuplicate[Ts <: Tuple] <: String = Ts match
    case EmptyTuple => ""
    case t *: rest => NameIfPresent[t, rest] match
      case "" => NameOfDuplicate[rest]
      case _  => NameIfPresent[t, rest]

  transparent inline def toNamed[Ts <: Tuple](tup: Ts): NTup[LabeledAsNames[Ts], LabeledAsValues[Ts]] =
    inline constValue[NameOfDuplicate[LabeledAsNames[Ts]]] match
      case "" => NamedTuple[LabeledAsNames[Ts], LabeledAsValues[Ts]](tup.asInstanceOf[LabeledAsValues[Ts]])
      case s => compiletime.error("Duplicate or missing names: " + s)

  type AllStringEntries[Ts <: Tuple] <: Boolean = Ts match
    case EmptyTuple => true
    case String *: rest => AllStringEntries[rest]
    case _ => false

  inline def checkNames[Ns <: Tuple]: Unit =
    inline constValue[NameOfDuplicate[Ns]] match
      case "" =>
        inline if constValue[AllStringEntries[Ns]] then ()
        else compiletime.error("Names must be strings")
      case s => compiletime.error("Duplicate or missing names: " + s)

  type NamedOrDefault[Ns <: Tuple, Ts <: Tuple, L, V] = Ns match
    case EmptyTuple => V
    case L *: _ => Ts match
      case EmptyTuple => Nothing
      case t *: _ => t
    case _ *: ns => Ts match
      case EmptyTuple => Nothing
      case _ *: ts => NamedOrDefault[ns, ts, L, V]

  type CopiedFrom[Ns <: Tuple, Ts <: Tuple, Nz <: Tuple, Tz <: Tuple] = Ts match
    case EmptyTuple => EmptyTuple
    case t *: ts => Ns match
      case EmptyTuple => Nothing
      case n *: ns => NamedOrDefault[Nz, Tz, n, t] *: CopiedFrom[ns, ts, Nz, Tz]

  type CopiedIndices[Ns <: Tuple, Nz <: Tuple, N <: Int] <: Tuple = Ns match
    case EmptyTuple => EmptyTuple
    case n *: rest => IndexOfType[Nz, n, 0] match
      case -1 => N *: CopiedIndices[rest, Nz, N-1]
      case _  => IndexOfType[Nz, n, 0] *: CopiedIndices[rest, Nz, N-1]

  transparent inline def byNameOrBackup[Ns <: Tuple, Ts <: Tuple, V](nts: NTup[Ns, Ts], inline v: V)[L, N <: Int] =
    inline constValue[IndexOfType[Ns, L, 0]] match
      case -1 => v
      case i  => nts(i)

  type NameOfMissing[Ts <: Tuple, Xs <: Tuple] <: String = Xs match
    case EmptyTuple => ""
    case x *: rest => NameIfAbsent[x, Ts] match
      case ""  => NameOfMissing[Ts, rest]
      case _ => NameIfAbsent[x, Ts]

  transparent inline def copyWithUpdateByName[Ns <: Tuple, Ts <: Tuple, Nz <: Tuple, Tz <: Tuple](nts: NTup[Ns, Ts], ntz: NTup[Nz, Tz]) =
    inline constValue[NameOfMissing[Ns, Nz]] match
      case "" => inline nts.toTuple match
        case EmptyTuple => nts
        case tp @ Tuple1(_) => Tuple1(byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0])
        case tp @ (_, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
          byNameOrBackup(ntz,tp._15)[Tuple.Elem[Ns,14],14],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
          byNameOrBackup(ntz,tp._15)[Tuple.Elem[Ns,14],14],
          byNameOrBackup(ntz,tp._16)[Tuple.Elem[Ns,15],15],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
          byNameOrBackup(ntz,tp._15)[Tuple.Elem[Ns,14],14],
          byNameOrBackup(ntz,tp._16)[Tuple.Elem[Ns,15],15],
          byNameOrBackup(ntz,tp._17)[Tuple.Elem[Ns,16],16],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
          byNameOrBackup(ntz,tp._15)[Tuple.Elem[Ns,14],14],
          byNameOrBackup(ntz,tp._16)[Tuple.Elem[Ns,15],15],
          byNameOrBackup(ntz,tp._17)[Tuple.Elem[Ns,16],16],
          byNameOrBackup(ntz,tp._18)[Tuple.Elem[Ns,17],17],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
          byNameOrBackup(ntz,tp._15)[Tuple.Elem[Ns,14],14],
          byNameOrBackup(ntz,tp._16)[Tuple.Elem[Ns,15],15],
          byNameOrBackup(ntz,tp._17)[Tuple.Elem[Ns,16],16],
          byNameOrBackup(ntz,tp._18)[Tuple.Elem[Ns,17],17],
          byNameOrBackup(ntz,tp._19)[Tuple.Elem[Ns,18],18],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
          byNameOrBackup(ntz,tp._15)[Tuple.Elem[Ns,14],14],
          byNameOrBackup(ntz,tp._16)[Tuple.Elem[Ns,15],15],
          byNameOrBackup(ntz,tp._17)[Tuple.Elem[Ns,16],16],
          byNameOrBackup(ntz,tp._18)[Tuple.Elem[Ns,17],17],
          byNameOrBackup(ntz,tp._19)[Tuple.Elem[Ns,18],18],
          byNameOrBackup(ntz,tp._20)[Tuple.Elem[Ns,19],19],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
          byNameOrBackup(ntz,tp._15)[Tuple.Elem[Ns,14],14],
          byNameOrBackup(ntz,tp._16)[Tuple.Elem[Ns,15],15],
          byNameOrBackup(ntz,tp._17)[Tuple.Elem[Ns,16],16],
          byNameOrBackup(ntz,tp._18)[Tuple.Elem[Ns,17],17],
          byNameOrBackup(ntz,tp._19)[Tuple.Elem[Ns,18],18],
          byNameOrBackup(ntz,tp._20)[Tuple.Elem[Ns,19],19],
          byNameOrBackup(ntz,tp._21)[Tuple.Elem[Ns,20],20],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case tp @ (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => (
          byNameOrBackup(ntz, tp._1)[Tuple.Elem[Ns, 0], 0],
          byNameOrBackup(ntz, tp._2)[Tuple.Elem[Ns, 1], 1],
          byNameOrBackup(ntz, tp._3)[Tuple.Elem[Ns, 2], 2],
          byNameOrBackup(ntz, tp._4)[Tuple.Elem[Ns, 3], 3],
          byNameOrBackup(ntz, tp._5)[Tuple.Elem[Ns, 4], 4],
          byNameOrBackup(ntz, tp._6)[Tuple.Elem[Ns, 5], 5],
          byNameOrBackup(ntz, tp._7)[Tuple.Elem[Ns, 6], 6],
          byNameOrBackup(ntz, tp._8)[Tuple.Elem[Ns, 7], 7],
          byNameOrBackup(ntz, tp._9)[Tuple.Elem[Ns, 8], 8],
          byNameOrBackup(ntz,tp._10)[Tuple.Elem[Ns, 9], 9],
          byNameOrBackup(ntz,tp._11)[Tuple.Elem[Ns,10],10],
          byNameOrBackup(ntz,tp._12)[Tuple.Elem[Ns,11],11],
          byNameOrBackup(ntz,tp._13)[Tuple.Elem[Ns,12],12],
          byNameOrBackup(ntz,tp._14)[Tuple.Elem[Ns,13],13],
          byNameOrBackup(ntz,tp._15)[Tuple.Elem[Ns,14],14],
          byNameOrBackup(ntz,tp._16)[Tuple.Elem[Ns,15],15],
          byNameOrBackup(ntz,tp._17)[Tuple.Elem[Ns,16],16],
          byNameOrBackup(ntz,tp._18)[Tuple.Elem[Ns,17],17],
          byNameOrBackup(ntz,tp._19)[Tuple.Elem[Ns,18],18],
          byNameOrBackup(ntz,tp._20)[Tuple.Elem[Ns,19],19],
          byNameOrBackup(ntz,tp._21)[Tuple.Elem[Ns,20],20],
          byNameOrBackup(ntz,tp._22)[Tuple.Elem[Ns,21],21],
        ).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
        case _ =>
          val ts = nts.asInstanceOf[Ts]
          val tz = ntz.asInstanceOf[Tz]
          val indices = constValueTuple[CopiedIndices[Ns, Nz, -1]].productIterator.asInstanceOf[Iterator[Int]]
          Tuple.fromIArray(IArray.from(
            indices.map(i => if i < 0 then ts.productElement(-1-i) else tz.productElement(i))
          )).asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]
      case s => compiletime.error("Field name missing in source tuple: " + s)

  type ElemOrSwap[Ts <: Tuple, V, K <: Int, N <: Int] = K match
    case N => V
    case _ => Tuple.Elem[Ts, K]

  type ElemOrSkip[Ts <: Tuple, K <: Int, N <: Int] = K < N match
    case true => Tuple.Elem[Ts, K]
    case _    => Tuple.Elem[Ts, K+1]

  type ElemOrExtra[Ts <: Tuple, V, K <: Int, N <: Int] = K < N match
    case true => Tuple.Elem[Ts, K]
    case _    => K match
      case N => V
      case _ => Tuple.Elem[Ts, K-1]

  type TupleWithSwap[Ts <: Tuple, V, K <: Int, N <: Int] <: Tuple = Ts match
    case EmptyTuple => EmptyTuple
    case t *: rest => K < N match
      case true => t *: TupleWithSwap[rest, V, K+1, N]
      case _ => V *: rest

  type TupleWithSkip[Ts <: Tuple, K <: Int, N <: Int] <: Tuple = Ts match
    case EmptyTuple => EmptyTuple
    case t *: rest => K < N match
      case true => t *: TupleWithSkip[rest, K+1, N]
      case _ => rest

  type TupleWithExtra[Ts <: Tuple, V, K <: Int, N <: Int] <: Tuple = Ts match
    case EmptyTuple => K > N match
      case true => EmptyTuple
      case _ => Tuple1[V]
    case t *: rest => K < N match
      case true => t *: TupleWithExtra[rest, V, K+1, N]
      case _ => V *: t *: rest

  inline def byNumberOrBackup[Ts <: Tuple, V](tup: Ts, inline v: V)[K <: Int, N <: Int]: ElemOrSwap[Ts, V, K, N] =
    inline if constValue[K] == constValue[N] then v.asInstanceOf[ElemOrSwap[Ts, V, K, N]] else tup(constValue[K]).asInstanceOf[ElemOrSwap[Ts, V, K, N]]

  inline def byNumberOrMap[Ts <: Tuple, V, N <: Int](tup: Ts, inline vf: Tuple.Elem[Ts, N] => V)[K <: Int]: ElemOrSwap[Ts, V, K, N] =
    inline if constValue[K] == constValue[N] then vf(tup(constValue[N])).asInstanceOf[ElemOrSwap[Ts, V, K, N]] else tup(constValue[K]).asInstanceOf[ElemOrSwap[Ts, V, K, N]]

  inline def byNumberOrSkip[Ts <: Tuple](tup: Ts)[K <: Int, N <: Int]: ElemOrSkip[Ts, K, N] =
    tup(constValue[K] + (inline if constValue[K] >= constValue[N] then 1 else 0)).asInstanceOf[ElemOrSkip[Ts, K, N]]

  inline def byNumberOrExtra[Ts <: Tuple, V](tup: Ts, inline v: V)[K <: Int, N <: Int]: ElemOrExtra[Ts, V, K, N] =
    inline if constValue[K] < constValue[N] then tup(constValue[K]).asInstanceOf[ElemOrExtra[Ts, V, K, N]]
    else
      inline if constValue[K] > constValue[N] then tup(constValue[K]-1).asInstanceOf[ElemOrExtra[Ts, V, K, N]]
      else v.asInstanceOf[ElemOrExtra[Ts, V, K, N]]
}


extension [A](a: A) {
  /** Associate a compile-time name with this value by giving the other (Singular) value */
  inline def \[L <: LabelStr](l: L): A \^ L = \^.wrap(a)

  /** Associate a compile-time name with this value, where it is a subtype of its original type, by giving a name */
  inline def \<[L <: LabelStr](l: L): A \<^ L = \<^.wrap(a)

  /** Associate a compile-time name with this value, where it is a supertype of its original type, by giving a name */
  inline def \>[L <: LabelStr](l: L): A \>^ L = \>^.wrap(a)
}


extension [Ts <: Tuple](tup: Ts) {
  transparent inline def asNamed: NamedTuple.NamedTuple[NamesAndLabels.LabeledAsNames[Ts], NamesAndLabels.LabeledAsValues[Ts]] =
    NamesAndLabels.toNamed[Ts](tup)
}

extension [Ns <: Tuple, Ts <: Tuple](tup: NamedTuple.NamedTuple[Ns, Ts]) {
  transparent inline def asLabeled: NamesAndLabels.NamesToLabels[Ns,Ts] =
    NamesAndLabels.toLabeled[Ns, Ts](tup)

  transparent inline infix def pluck[L <: LabelStr](lb: L): NamesAndLabels.LabeledValue[Ns, Ts, L] =
    NamesAndLabels.byLabel(tup, lb)
}


extension [L <: LabelStr, A](la: A \^ L) {
  inline def nt =
    inline if compiletime.constValue[L] == "" then compiletime.error("Invalid tuple field name")
    else NamedTuple.withNames(Tuple1(la.unlabel))[Tuple1[L]]
  inline def kv: (L, A) = (compiletime.constValue[L], la.unlabel)
}


extension [L <: LabelStr, A](inline tup: NamedTuple.NamedTuple[Tuple1[L], Tuple1[A]]) {
  transparent inline def lb: A \^ L = \^.wrap(kse.basics.basicsMacroImpl.extractNamedTuple1Literal(tup))
}

extension [L <: LabelStr, A](tup: NamedTuple.NamedTuple[Tuple1[L], Tuple1[A]]) {
  inline def kv: (L, A) = (compiletime.constValue[L], tup.asInstanceOf[Tuple1[A]]._1)
}


extension [L <: LabelStr](l: L) {
  inline def labeled[A](f: \^.Assumed[L] ?=> A): A \^ L = \^.wrap[A](f(using \^.Assumed.label[L]))[L]
  inline def assumed[A](f: \^.Assumed[L] ?=> A): A      =            f(using \^.Assumed.label[L])
}


inline def conjure[L <: LabelStr](l: L)[A](using inline nm: ((A \^ L) | (A \>^ L))): A = inline nm match
  case nt: (A \^ L)  => nt.unlabel
  case sp: (A \>^ L) => sp
  case _ => nm.asInstanceOf[A]   // Cheating, but they're all the same type anyway, so....





/*

################
## GENERATORS ##
################

def mkTypeIdx(n: Int) =
  assert(n > 0 && n < 26)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWXY".take(n).map(_.toString)
  val targ = args.mkString(", ")
  val norg = (0 to n).mkString(" | ")
  println(s"type UpTo$n[$targ, Z] = Z match")
  for (a, i) <- args.zipAll(0 to n, "_", -1) do
    println(s"  case $a => $i")

for n <- 1 to 22 do
  mkTypeIdx(n)
  println()

def mkCopyFrom(n: Int) =
  val tupn = ("_"*n).mkString(", ")
  println(s"case tp @ ($tupn) => (")
  for k <- 0 until n do
    val elm = s"tp._${k+1}"
    println(f"  byNameOrBackup(ntz,$elm%6s)[Tuple.Elem[Ns,$k%2d],$k%2d],")
  println(s").asInstanceOf[CopiedFrom[Ns, Ts, Nz, Tz]]")

for n <- 2 to 22 do
  mkCopyFrom(n)
*/
