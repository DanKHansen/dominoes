import scala.annotation.tailrec

object Dominoes:
   def chain(input: List[(Int, Int)]): Option[List[(Int, Int)]] =
      if input.isEmpty then Some(Nil)
      else if input.size == 1 then if input.head.swap == input.head then Some(input) else None
      else
         @tailrec
         def loop(dominoes: List[(Int, Int)], acc: List[(Int, Int)] = Nil): List[(Int, Int)] =
            (acc.lastOption, dominoes.headOption) match
               case (Some((_, a)), Some((b, _))) if a == b => loop(dominoes.tail, acc :+ dominoes.head)
               case (Some((_, a)), Some((_, b))) if a == b => loop(dominoes.tail, acc :+ dominoes.head.swap)
               case (_, None)                              =>
                  (acc.headOption, acc.lastOption) match
                     case (Some((a, _)), Some((_, b))) if a == b => acc
                     case _                                      => Nil
               case (None, _)                              => loop(dominoes.tail, acc :+ dominoes.head)
               case _                                      => Nil

         input.permutations.map(loop(_)).find(_.nonEmpty)
