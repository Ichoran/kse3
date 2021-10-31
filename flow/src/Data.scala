package kse.flow


//////////////////////////////////////////////////////////////////////////////////
/// Generally helpful evaluation/execution utilities for singletons and tuples ///
//////////////////////////////////////////////////////////////////////////////////


extension [A](a: A) {
  inline def fn[B](inline f: A => B): B = f(a)
  inline def tap(inline f: A => Unit): A = { f(a); a }

  inline def tup[Z](inline z: Z): (A, Z) = (a, z)
  inline def tup_1[Z](inline z: Z): (Z, A) = (z, a)
}

extension [A, B](q: (A, B)) {
  inline def _1To[Z](inline z: Z): (Z, B) = (z, q._2)
  inline def _1Fn[Z](inline zfn: A => Z): (Z, B) = (zfn(q._1), q._2)

  inline def _2To[Z](inline z: Z): (A, Z) = (q._1, z)
  inline def _2Fn[Z](inline zfn: B => Z): (A, Z) = (q._1, zfn(q._2))

  inline def fns[Z, Y](inline az: A => Z, inline by: B => Y): (Z, Y) = (az(q._1), by(q._2))
  inline def sameFn[Z](zfn: (A | B) => Z): (Z, Z) = (zfn(q._1), zfn(q._2))
  inline def fold[Z](inline zfn: (A, B) => Z): Z = zfn(q._1, q._2)
  inline def reduce[Z >: A | B](zop: (Z, Z) => Z) = zop(q._1, q._2)

  inline def tup[Z](inline z: Z): (A, B, Z) = (q._1, q._2, z)
  inline def tup_1[Z](inline z: Z): (Z, A, B) = (z, q._1, q._2)
  inline def tup_2[Z](inline z: Z): (A, Z, B) = (q._1, z, q._2)
  inline def also[Z](inline zfn: (A, B) => Z): (A, B, Z) = (q._1, q._2, zfn(q._1, q._2))

  inline def drop_1: B = q._2
  inline def drop: A = q._1

  inline def join[Y, Z](p: (Y, Z)): (A, B, Y, Z) = (q._1, q._2, p._1, p._2)
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3)
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4)
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5)
  inline def join[U, V, W, X, Y, Z](p: (U, V, W, X, Y, Z)): (A, B, U, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5, p._6)
  inline def join[T, U, V, W, X, Y, Z](p: (T, U, V, W, X, Y, Z)): (A, B, T, U, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5, p._6, p._7)
}

extension [A, B, C](q: (A, B, C)) {
  inline def _1To[Z](inline z: Z): (Z, B, C) = (z, q._2, q._3)
  inline def _1Fn[Z](inline zfn: A => Z): (Z, B, C) = (zfn(q._1), q._2, q._3)

  inline def _2To[Z](inline z: Z): (A, Z, C) = (q._1, z, q._3)
  inline def _2Fn[Z](inline zfn: B => Z): (A, Z, C) = (q._1, zfn(q._2), q._3)

  inline def _3To[Z](inline z: Z): (A, B, Z) = (q._1, q._2, z)
  inline def _3Fn[Z](inline zfn: C => Z): (A, B, Z) = (q._1, q._2, zfn(q._3))

  inline def fns[Z, Y, X](inline az: A => Z, inline by: B => Y, inline cx: C => X): (Z, Y, X) = (az(q._1), by(q._2), cx(q._3))
  inline def sameFn[Z](zfn: (A | B | C) => Z): (Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3))
  inline def fold[Z](inline zfn: (A, B, C) => Z): Z = zfn(q._1, q._2, q._3)
  inline def reduce[Z >: A | B | C](zop: (Z, Z) => Z) = zop(zop(q._1, q._2), q._3)

  inline def tup[Z](inline z: Z): (A, B, C, Z) = (q._1, q._2, q._3, z)
  inline def tup_1[Z](inline z: Z): (Z, A, B, C) = (z, q._1, q._2, q._3)
  inline def tup_2[Z](inline z: Z): (A, Z, B, C) = (q._1, z, q._2, q._3)
  inline def tup_3[Z](inline z: Z): (A, B, Z, C) = (q._1, q._2, z, q._3)
  inline def also[Z](inline zfn: (A, B, C) => Z): (A, B, C, Z) = (q._1, q._2, q._3, zfn(q._1, q._2, q._3))

  inline def drop_1: (B, C) = (q._2, q._3)
  inline def drop_2: (A, C) = (q._1, q._3)
  inline def drop: (A, B) = (q._1, q._2)

  inline def join[Y, Z](p: (Y, Z)): (A, B, C, Y, Z) = (q._1, q._2, q._3, p._1, p._2)
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3)
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4)
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, C, V, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4, p._5)
  inline def join[U, V, W, X, Y, Z](p: (U, V, W, X, Y, Z)): (A, B, C, U, V, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4, p._5, p._6)

  inline def cut1: (A, (B, C)) = (q._1, (q._2, q._3))
  inline def cut2: ((A, B), C) = ((q._1, q._2), q._3)
}

extension [A, B, C, D](q: (A, B, C, D)) {
  inline def _1To[Z](inline z: Z): (Z, B, C, D) = (z, q._2, q._3, q._4)
  inline def _1Fn[Z](inline zfn: A => Z): (Z, B, C, D) = (zfn(q._1), q._2, q._3, q._4)

  inline def _2To[Z](inline z: Z): (A, Z, C, D) = (q._1, z, q._3, q._4)
  inline def _2Fn[Z](inline zfn: B => Z): (A, Z, C, D) = (q._1, zfn(q._2), q._3, q._4)

  inline def _3To[Z](inline z: Z): (A, B, Z, D) = (q._1, q._2, z, q._4)
  inline def _3Fn[Z](inline zfn: C => Z): (A, B, Z, D) = (q._1, q._2, zfn(q._3), q._4)

  inline def _4To[Z](inline z: Z): (A, B, C, Z) = (q._1, q._2, q._3, z)
  inline def _4Fn[Z](inline zfn: D => Z): (A, B, C, Z) = (q._1, q._2, q._3, zfn(q._4))

  inline def fns[Z, Y, X, W](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W): (Z, Y, X, W) = (az(q._1), by(q._2), cx(q._3), dw(q._4))
  inline def sameFn[Z](zfn: (A | B | C | D) => Z): (Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4))
  inline def fold[Z](inline zfn: (A, B, C, D) => Z): Z = zfn(q._1, q._2, q._3, q._4)
  inline def reduce[Z >: A | B | C | D](zop: (Z, Z) => Z) = zop(zop(zop(q._1, q._2), q._3), q._4)

  inline def tup[Z](inline z: Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, z)
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D) = (z, q._1, q._2, q._3, q._4)
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D) = (q._1, z, q._2, q._3, q._4)
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D) = (q._1, q._2, z, q._3, q._4)
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D) = (q._1, q._2, q._3, z, q._4)
  inline def also[Z](inline zfn: (A, B, C, D) => Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, zfn(q._1, q._2, q._3, q._4))

  inline def drop_1: (B, C, D) = (q._2, q._3, q._4)
  inline def drop_2: (A, C, D) = (q._1, q._3, q._4)
  inline def drop_3: (A, B, D) = (q._1, q._2, q._4)
  inline def drop: (A, B, C) = (q._1, q._2, q._3)

  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2)
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3)
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, D, W, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3, p._4)
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, C, D, V, W, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3, p._4, p._5)

  inline def cut1: (A, (B, C, D)) = (q._1, (q._2, q._3, q._4))
  inline def cut2: ((A, B), (C, D)) = ((q._1, q._2), (q._3, q._4))
  inline def cut3: ((A, B, C), D) = ((q._1, q._2, q._3), q._4)
}

extension [A, B, C, D, E](q: (A, B, C, D, E)) {
  inline def _1To[Z](inline z: Z): (Z, B, C, D, E) = (z, q._2, q._3, q._4, q._5)
  inline def _1Fn[Z](inline zfn: A => Z): (Z, B, C, D, E) = (zfn(q._1), q._2, q._3, q._4, q._5)

  inline def _2To[Z](inline z: Z): (A, Z, C, D, E) = (q._1, z, q._3, q._4, q._5)
  inline def _2Fn[Z](inline zfn: B => Z): (A, Z, C, D, E) = (q._1, zfn(q._2), q._3, q._4, q._5)

  inline def _3To[Z](inline z: Z): (A, B, Z, D, E) = (q._1, q._2, z, q._4, q._5)
  inline def _3Fn[Z](inline zfn: C => Z): (A, B, Z, D, E) = (q._1, q._2, zfn(q._3), q._4, q._5)

  inline def _4To[Z](inline z: Z): (A, B, C, Z, E) = (q._1, q._2, q._3, z, q._5)
  inline def _4Fn[Z](inline zfn: D => Z): (A, B, C, Z, E) = (q._1, q._2, q._3, zfn(q._4), q._5)

  inline def _5To[Z](inline z: Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, z)
  inline def _5Fn[Z](inline zfn: E => Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, zfn(q._5))

  inline def fns[Z, Y, X, W, V](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V): (Z, Y, X, W, V) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5))
  inline def sameFn[Z](zfn: (A | B | C | D | E) => Z): (Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5))
  inline def fold[Z](inline zfn: (A, B, C, D, E) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5)
  inline def reduce[Z >: A | B | C | D | E](zop: (Z, Z) => Z) = zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5)

  inline def tup[Z](inline z: Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, z)
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E) = (z, q._1, q._2, q._3, q._4, q._5)
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E) = (q._1, z, q._2, q._3, q._4, q._5)
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E) = (q._1, q._2, z, q._3, q._4, q._5)
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E) = (q._1, q._2, q._3, z, q._4, q._5)
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E) = (q._1, q._2, q._3, q._4, z, q._5)
  inline def also[Z](inline zfn: (A, B, C, D, E) => Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, zfn(q._1, q._2, q._3, q._4, q._5))

  inline def drop_1: (B, C, D, E) = (q._2, q._3, q._4, q._5)
  inline def drop_2: (A, C, D, E) = (q._1, q._3, q._4, q._5)
  inline def drop_3: (A, B, D, E) = (q._1, q._2, q._4, q._5)
  inline def drop_4: (A, B, C, E) = (q._1, q._2, q._3, q._5)
  inline def drop: (A, B, C, D) = (q._1, q._2, q._3, q._4)

  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2)
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, E, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2, p._3)
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, D, E, W, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2, p._3, p._4)

  inline def cut1: (A, (B, C, D, E)) = (q._1, (q._2, q._3, q._4, q._5))
  inline def cut2: ((A, B), (C, D, E)) = ((q._1, q._2), (q._3, q._4, q._5))
  inline def cut3: ((A, B, C), (D, E)) = ((q._1, q._2, q._3), (q._4, q._5))
  inline def cut4: ((A, B, C, D), E) = ((q._1, q._2, q._3, q._4), q._5)
}

extension [A, B, C, D, E, F](q: (A, B, C, D, E, F)) {
  inline def _1To[Z](inline z: Z): (Z, B, C, D, E, F) = (z, q._2, q._3, q._4, q._5, q._6)
  inline def _1Fn[Z](inline zfn: A => Z): (Z, B, C, D, E, F) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6)

  inline def _2To[Z](inline z: Z): (A, Z, C, D, E, F) = (q._1, z, q._3, q._4, q._5, q._6)
  inline def _2Fn[Z](inline zfn: B => Z): (A, Z, C, D, E, F) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6)

  inline def _3To[Z](inline z: Z): (A, B, Z, D, E, F) = (q._1, q._2, z, q._4, q._5, q._6)
  inline def _3Fn[Z](inline zfn: C => Z): (A, B, Z, D, E, F) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6)

  inline def _4To[Z](inline z: Z): (A, B, C, Z, E, F) = (q._1, q._2, q._3, z, q._5, q._6)
  inline def _4Fn[Z](inline zfn: D => Z): (A, B, C, Z, E, F) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6)

  inline def _5To[Z](inline z: Z): (A, B, C, D, Z, F) = (q._1, q._2, q._3, q._4, z, q._6)
  inline def _5Fn[Z](inline zfn: E => Z): (A, B, C, D, Z, F) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6)

  inline def _6To[Z](inline z: Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, z)
  inline def _6Fn[Z](inline zfn: F => Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6))

  inline def fns[Z, Y, X, W, V, U](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U): (Z, Y, X, W, V, U) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6))
  inline def sameFn[Z](zfn: (A | B | C | D | E | F) => Z): (Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6))
  inline def fold[Z](inline zfn: (A, B, C, D, E, F) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6)
  inline def reduce[Z >: A | B | C | D | E | F](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6)

  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, z)
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F) = (z, q._1, q._2, q._3, q._4, q._5, q._6)
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F) = (q._1, z, q._2, q._3, q._4, q._5, q._6)
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F) = (q._1, q._2, z, q._3, q._4, q._5, q._6)
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F) = (q._1, q._2, q._3, z, q._4, q._5, q._6)
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F) = (q._1, q._2, q._3, q._4, z, q._5, q._6)
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F) = (q._1, q._2, q._3, q._4, q._5, z, q._6)
  inline def also[Z](inline zfn: (A, B, C, D, E, F) => Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._1, q._2, q._3, q._4, q._5, q._6))

  inline def drop_1: (B, C, D, E, F) = (q._2, q._3, q._4, q._5, q._6)
  inline def drop_2: (A, C, D, E, F) = (q._1, q._3, q._4, q._5, q._6)
  inline def drop_3: (A, B, D, E, F) = (q._1, q._2, q._4, q._5, q._6)
  inline def drop_4: (A, B, C, E, F) = (q._1, q._2, q._3, q._5, q._6)
  inline def drop_5: (A, B, C, D, F) = (q._1, q._2, q._3, q._4, q._6)
  inline def drop: (A, B, C, D, E) = (q._1, q._2, q._3, q._4, q._5)

  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, F, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, p._1, p._2)
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, E, F, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, p._1, p._2, p._3)

  inline def cut1: (A, (B, C, D, E, F)) = (q._1, (q._2, q._3, q._4, q._5, q._6))
  inline def cut2: ((A, B), (C, D, E, F)) = ((q._1, q._2), (q._3, q._4, q._5, q._6))
  inline def cut3: ((A, B, C), (D, E, F)) = ((q._1, q._2, q._3), (q._4, q._5, q._6))
  inline def cut4: ((A, B, C, D), (E, F)) = ((q._1, q._2, q._3, q._4), (q._5, q._6))
  inline def cut5: ((A, B, C, D, E), F) = ((q._1, q._2, q._3, q._4, q._5), q._6)
}


extension [A, B, C, D, E, F, G](q: (A, B, C, D, E, F, G)) {
  inline def _1To[Z](inline z: Z): (Z, B, C, D, E, F, G) = (z, q._2, q._3, q._4, q._5, q._6, q._7)
  inline def _1Fn[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7)

  inline def _2To[Z](inline z: Z): (A, Z, C, D, E, F, G) = (q._1, z, q._3, q._4, q._5, q._6, q._7)
  inline def _2Fn[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7)

  inline def _3To[Z](inline z: Z): (A, B, Z, D, E, F, G) = (q._1, q._2, z, q._4, q._5, q._6, q._7)
  inline def _3Fn[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7)

  inline def _4To[Z](inline z: Z): (A, B, C, Z, E, F, G) = (q._1, q._2, q._3, z, q._5, q._6, q._7)
  inline def _4Fn[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7)

  inline def _5To[Z](inline z: Z): (A, B, C, D, Z, F, G) = (q._1, q._2, q._3, q._4, z, q._6, q._7)
  inline def _5Fn[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7)

  inline def _6To[Z](inline z: Z): (A, B, C, D, E, Z, G) = (q._1, q._2, q._3, q._4, q._5, z, q._7)
  inline def _6Fn[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7)

  inline def _7To[Z](inline z: Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, z)
  inline def _7Fn[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7))

  inline def fns[Z, Y, X, W, V, U, T](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T): (Z, Y, X, W, V, U, T) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7))
  inline def sameFn[Z](zfn: (A | B | C | D | E | F | G) => Z): (Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7))
  inline def fold[Z](inline zfn: (A, B, C, D, E, F, G) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7)
  inline def reduce[Z >: A | B | C | D | E | F | G](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7)

  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z)
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F, G) = (z, q._1, q._2, q._3, q._4, q._5, q._6, q._7)
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F, G) = (q._1, z, q._2, q._3, q._4, q._5, q._6, q._7)
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F, G) = (q._1, q._2, z, q._3, q._4, q._5, q._6, q._7)
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F, G) = (q._1, q._2, q._3, z, q._4, q._5, q._6, q._7)
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F, G) = (q._1, q._2, q._3, q._4, z, q._5, q._6, q._7)
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F, G) = (q._1, q._2, q._3, q._4, q._5, z, q._6, q._7)
  inline def tup_7[Z](inline z: Z): (A, B, C, D, E, F, Z, G) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._7)
  inline def also[Z](inline zfn: (A, B, C, D, E, F, G) => Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7))

  inline def drop_1: (B, C, D, E, F, G) = (q._2, q._3, q._4, q._5, q._6, q._7)
  inline def drop_2: (A, C, D, E, F, G) = (q._1, q._3, q._4, q._5, q._6, q._7)
  inline def drop_3: (A, B, D, E, F, G) = (q._1, q._2, q._4, q._5, q._6, q._7)
  inline def drop_4: (A, B, C, E, F, G) = (q._1, q._2, q._3, q._5, q._6, q._7)
  inline def drop_5: (A, B, C, D, F, G) = (q._1, q._2, q._3, q._4, q._6, q._7)
  inline def drop_6: (A, B, C, D, E, G) = (q._1, q._2, q._3, q._4, q._5, q._7)
  inline def drop: (A, B, C, D, E, F) = (q._1, q._2, q._3, q._4, q._5, q._6)

  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, F, G, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, p._1, p._2)

  inline def cut1: (A, (B, C, D, E, F, G)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7))
  inline def cut2: ((A, B), (C, D, E, F, G)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7))
  inline def cut3: ((A, B, C), (D, E, F, G)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7))
  inline def cut4: ((A, B, C, D), (E, F, G)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7))
  inline def cut5: ((A, B, C, D, E), (F, G)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7))
  inline def cut6: ((A, B, C, D, E, F), G) = ((q._1, q._2, q._3, q._4, q._5, q._6), q._7)
}

extension [A, B, C, D, E, F, G, H](q: (A, B, C, D, E, F, G, H)) {
  inline def _1To[Z](inline z: Z): (Z, B, C, D, E, F, G, H) = (z, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  inline def _1Fn[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G, H) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7, q._8)

  inline def _2To[Z](inline z: Z): (A, Z, C, D, E, F, G, H) = (q._1, z, q._3, q._4, q._5, q._6, q._7, q._8)
  inline def _2Fn[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G, H) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7, q._8)

  inline def _3To[Z](inline z: Z): (A, B, Z, D, E, F, G, H) = (q._1, q._2, z, q._4, q._5, q._6, q._7, q._8)
  inline def _3Fn[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G, H) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7, q._8)

  inline def _4To[Z](inline z: Z): (A, B, C, Z, E, F, G, H) = (q._1, q._2, q._3, z, q._5, q._6, q._7, q._8)
  inline def _4Fn[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G, H) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7, q._8)

  inline def _5To[Z](inline z: Z): (A, B, C, D, Z, F, G, H) = (q._1, q._2, q._3, q._4, z, q._6, q._7, q._8)
  inline def _5Fn[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G, H) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7, q._8)

  inline def _6To[Z](inline z: Z): (A, B, C, D, E, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, z, q._7, q._8)
  inline def _6Fn[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7, q._8)

  inline def _7To[Z](inline z: Z): (A, B, C, D, E, F, Z, H) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._8)
  inline def _7Fn[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z, H) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7), q._8)

  inline def _8To[Z](inline z: Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z)
  inline def _8Fn[Z](inline zfn: H => Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._8))

  inline def fns[Z, Y, X, W, V, U, T, S](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T, inline hs: H => S): (Z, Y, X, W, V, U, T, S) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7), hs(q._8))
  inline def sameFn[Z](zfn: (A | B | C | D | E | F | G | H) => Z): (Z, Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7), zfn(q._8))
  inline def fold[Z](inline zfn: (A, B, C, D, E, F, G, H) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  inline def reduce[Z >: A | B | C | D | E | F | G | H](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7), q._8)

  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, z)
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F, G, H) = (z, q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F, G, H) = (q._1, z, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F, G, H) = (q._1, q._2, z, q._3, q._4, q._5, q._6, q._7, q._8)
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F, G, H) = (q._1, q._2, q._3, z, q._4, q._5, q._6, q._7, q._8)
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F, G, H) = (q._1, q._2, q._3, q._4, z, q._5, q._6, q._7, q._8)
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F, G, H) = (q._1, q._2, q._3, q._4, q._5, z, q._6, q._7, q._8)
  inline def tup_7[Z](inline z: Z): (A, B, C, D, E, F, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._7, q._8)
  inline def also[Z](inline zfn: (A, B, C, D, E, F, G, H) => Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8))

  inline def drop_1: (B, C, D, E, F, G, H) = (q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  inline def drop_2: (A, C, D, E, F, G, H) = (q._1, q._3, q._4, q._5, q._6, q._7, q._8)
  inline def drop_3: (A, B, D, E, F, G, H) = (q._1, q._2, q._4, q._5, q._6, q._7, q._8)
  inline def drop_4: (A, B, C, E, F, G, H) = (q._1, q._2, q._3, q._5, q._6, q._7, q._8)
  inline def drop_5: (A, B, C, D, F, G, H) = (q._1, q._2, q._3, q._4, q._6, q._7, q._8)
  inline def drop_6: (A, B, C, D, E, G, H) = (q._1, q._2, q._3, q._4, q._5, q._7, q._8)
  inline def drop_7: (A, B, C, D, E, F, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._8)
  inline def drop: (A, B, C, D, E, F, G) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7)

  inline def cut1: (A, (B, C, D, E, F, G, H)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7, q._8))
  inline def cut2: ((A, B), (C, D, E, F, G, H)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7, q._8))
  inline def cut3: ((A, B, C), (D, E, F, G, H)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7, q._8))
  inline def cut4: ((A, B, C, D), (E, F, G, H)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7, q._8))
  inline def cut5: ((A, B, C, D, E), (F, G, H)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7, q._8))
  inline def cut6: ((A, B, C, D, E, F), (G, H)) = ((q._1, q._2, q._3, q._4, q._5, q._6), (q._7, q._8))
  inline def cut7: ((A, B, C, D, E, F, G), H) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7), q._8)
}

extension [A, B, C, D, E, F, G, H, I](q: (A, B, C, D, E, F, G, H, I)) {
  inline def _1To[Z](inline z: Z): (Z, B, C, D, E, F, G, H, I) = (z, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  inline def _1Fn[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G, H, I) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)

  inline def _2To[Z](inline z: Z): (A, Z, C, D, E, F, G, H, I) = (q._1, z, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  inline def _2Fn[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G, H, I) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7, q._8, q._9)

  inline def _3To[Z](inline z: Z): (A, B, Z, D, E, F, G, H, I) = (q._1, q._2, z, q._4, q._5, q._6, q._7, q._8, q._9)
  inline def _3Fn[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G, H, I) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7, q._8, q._9)

  inline def _4To[Z](inline z: Z): (A, B, C, Z, E, F, G, H, I) = (q._1, q._2, q._3, z, q._5, q._6, q._7, q._8, q._9)
  inline def _4Fn[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G, H, I) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7, q._8, q._9)

  inline def _5To[Z](inline z: Z): (A, B, C, D, Z, F, G, H, I) = (q._1, q._2, q._3, q._4, z, q._6, q._7, q._8, q._9)
  inline def _5Fn[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G, H, I) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7, q._8, q._9)

  inline def _6To[Z](inline z: Z): (A, B, C, D, E, Z, G, H, I) = (q._1, q._2, q._3, q._4, q._5, z, q._7, q._8, q._9)
  inline def _6Fn[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G, H, I) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7, q._8, q._9)

  inline def _7To[Z](inline z: Z): (A, B, C, D, E, F, Z, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._8, q._9)
  inline def _7Fn[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7), q._8, q._9)

  inline def _8To[Z](inline z: Z): (A, B, C, D, E, F, G, Z, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z, q._9)
  inline def _8Fn[Z](inline zfn: H => Z): (A, B, C, D, E, F, G, Z, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._8), q._9)

  inline def _9To[Z](inline z: Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, z)
  inline def _9Fn[Z](inline zfn: I => Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, zfn(q._9))

  inline def fns[Z, Y, X, W, V, U, T, S, R](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T, inline hs: H => S, inline ir: I => R): (Z, Y, X, W, V, U, T, S, R) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7), hs(q._8), ir(q._9))
  inline def sameFn[Z](zfn: (A | B | C | D | E | F | G | H | I) => Z): (Z, Z, Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7), zfn(q._8), zfn(q._9))
  inline def fold[Z](inline zfn: (A, B, C, D, E, F, G, H, I) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  inline def reduce[Z >: A | B | C | D | E | F | G | H | I](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7), q._8), q._9)

  inline def drop_1: (B, C, D, E, F, G, H, I) = (q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  inline def drop_2: (A, C, D, E, F, G, H, I) = (q._1, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  inline def drop_3: (A, B, D, E, F, G, H, I) = (q._1, q._2, q._4, q._5, q._6, q._7, q._8, q._9)
  inline def drop_4: (A, B, C, E, F, G, H, I) = (q._1, q._2, q._3, q._5, q._6, q._7, q._8, q._9)
  inline def drop_5: (A, B, C, D, F, G, H, I) = (q._1, q._2, q._3, q._4, q._6, q._7, q._8, q._9)
  inline def drop_6: (A, B, C, D, E, G, H, I) = (q._1, q._2, q._3, q._4, q._5, q._7, q._8, q._9)
  inline def drop_7: (A, B, C, D, E, F, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._8, q._9)
  inline def drop_8: (A, B, C, D, E, F, G, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._9)
  inline def drop: (A, B, C, D, E, F, G, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)

  inline def cut1: (A, (B, C, D, E, F, G, H, I)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9))
  inline def cut2: ((A, B), (C, D, E, F, G, H, I)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7, q._8, q._9))
  inline def cut3: ((A, B, C), (D, E, F, G, H, I)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7, q._8, q._9))
  inline def cut4: ((A, B, C, D), (E, F, G, H, I)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7, q._8, q._9))
  inline def cut5: ((A, B, C, D, E), (F, G, H, I)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7, q._8, q._9))
  inline def cut6: ((A, B, C, D, E, F), (G, H, I)) = ((q._1, q._2, q._3, q._4, q._5, q._6), (q._7, q._8, q._9))
  inline def cut7: ((A, B, C, D, E, F, G), (H, I)) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7), (q._8, q._9))
  inline def cut8: ((A, B, C, D, E, F, G, H), I) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8), q._9)
}
