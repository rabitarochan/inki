package com.rabitarochan.inki

import scala.util.parsing.input.{ Position, NoPosition }

/** Inki root element. */
trait Element

/** Inki inline elements. */
trait Span extends Element
case class Text( content: String ) extends Span
case class Strong( content: String ) extends Span
case class Italic( content: String ) extends Span

/** Inki block elements. */
trait Block extends Element {
  def indent: Int
}
case class Paragraph( indent: Int, children: Seq[ Span ] ) extends Block
case class Header( indent: Int, level: Int, children: Seq[ Span ])
