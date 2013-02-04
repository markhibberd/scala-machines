package com.clarifi.machines

object Wye {
  import Machine._
  import Process._

  /** Connects the output of two processes into a `Wye`. */
  def wye[A, AA, B, BB, O](pa: Process[A, AA],
                           pb: Process[B, BB],
                           y: Wye[AA, BB, O]): Wye[A, B, O] =
    y match {
      case Return(a)         => a /* _|_ this is not possible, as a is of type Nothing */
      case Stop              => Stop
      case Emit(o, next)     => Emit(o, () => wye(pa, pb, next()))
      case Await(k, This(kl), f) => pa match {
        case Return(a)      => a /* _|_ this is not possible, as a is of type Nothing */
        case Stop           => wye(stopped, pb, f())
        case Emit(a, next)  => wye(next(), pb, k(kl(a)))
        case Await(l, t, g) =>
          Await(
            (paa: Any) => wye(paa.asInstanceOf[Process[A, AA]], pb, y),
            This(a => l(t(a))),
            () => wye(g(), pb, y)
          )
      }
      case Await(k, That(kr), f) => pb match {
        case Return(a)      => a /* _|_ this is not possible, as a is of type Nothing */
        case Stop           => wye(pa, stopped, f())
        case Emit(b, next)  => wye(pa, next(), k(kr(b)))
        case Await(l, t, g) =>
          Await(
            (pbb: Any) => wye(pa, pbb.asInstanceOf[Process[B, BB]], y),
            That(b => l(t(b))),
            () => wye(pa, g(), y)
          )
      }
      case Await(k, Both(kl, kr), f) => pa match {
        case Return(a)      => a /* _|_ this is not possible, as a is of type Nothing */
        case Emit(a, next)  => wye(next(), pb, k(kl(a)))
        case Stop           => pb match {
          case Return(a)      => a /* _|_ this is not possible, as a is of type Nothing */
          case Emit(b, next)  => wye(stopped, next(), k(kr(b)))
          case Stop           => wye(stopped, stopped, f())
          case Await(l, t, g) =>
            Await(
              (pbb: Any) => wye(stopped, pbb.asInstanceOf[Process[B, BB]], y),
              That(b => l(t(b))),
              () => wye(stopped, g(), y)
            )
        }
        case Await(la, ta, ga) => pb match {
          case Return(a)     => a /* _|_ this is not possible, as a: Nothing */
          case Emit(b, next) => wye(pa, next(), k(kr(b)))
          case Stop          =>
            Await(
              (paa: Any) => wye(paa.asInstanceOf[Process[A,AA]], stopped, y),
              This(a => la(ta(a))),
              () => wye(ga(), stopped, y)
            )
          case Await(lb, tb, gb) =>
            Await(
              (x: Any) => x.asInstanceOf[Wye[A, B, O]],
              Both(a  => wye(la(ta(a)), pb, y),
                   b => wye(pa, lb(tb(b)), y)),
              () => wye(ga(), gb(), y)
            )
        }
      }
    }
}
