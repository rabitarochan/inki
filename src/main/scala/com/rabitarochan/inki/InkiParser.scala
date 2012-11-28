package com.rabitarochan.inki

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{ Position, NoPosition }


object InkiParser extends RegexParsers with ParserHelper {
  override def skipWhitespace = false
  def sizeOfTab: Int = 2


  case class Indent( size: Int )

  def indent: Parser[ Indent ] = """[ \t]*""".r ^^ { s =>
    val size = s.toList.map {
      case ' ' => 1
      case '\t' => sizeOfTab
    }.sum
    Indent( size )
  }
  def eol = """\r?\n""".r

  def chunk: Parser[ Chunk ] =
    orderedList | unorderedList | textLine | emptyLines


  def emptyLines: Parser[ SpanChunk ] =
    rep1( emptyLine | emptySpace ) ^^ { s => EmptyChunk( foldContent( s ) ) }
  def emptyLine: Parser[ SpanChunk ] = 
    """[\t ]*\r?\n""".r ^^ { s => EmptyChunk( s ) }
  def emptySpace: Parser[ SpanChunk ] = 
    """[\t ]+""".r ^^ { s => EmptyChunk( s ) }


  def span: Parser[ SpanChunk ] =
    strongItalicSpan | strongSpan | italicSpan | textSpan

  def textSpan: Parser[ SpanChunk ] =
    """\S[^\r\n]*""".r ^^ { s => TextSpanChunk( s ) }

  def strongSpan: Parser[ SpanChunk ] = 
    " *" ~> """\S[^\r\n]*(?=\* )""".r <~ "* " ^^ { s => StrongSpanChunk( s ) }

  def italicSpan: Parser[ SpanChunk ] =
    " /" ~> """\S[^\r\n]*(?=/ )""".r <~ "/ " ^^ { s => ItalicSpanChunk( s ) }

  def strongItalicSpan: Parser[ SpanChunk ] =
    (( " /*" ~> """\S[^\r\n]*(?=\*/ )""".r <~ "*/ " ) |
     ( " */" ~> """\S[^\r\n]*(?=/\* )""".r <~ "/* " )) ^^ {
      s => StrongItalicSpanChunk( s )
    }


  def textLine: Parser[ Chunk ] =
    indent ~ rep1( span ) <~ opt( eol ) ^^ {
      case indent ~ spans => TextChunk( indent.size, spans )
    }


  def orderedList: Parser[ Chunk ] =
    indent ~ ( "+ " ~> rep1( span ) <~ opt( eol ) ) ^^ {
      case indent ~ spans => OrderedListChunk( indent.size, spans )
    }
  def unorderedList: Parser[ Chunk ] =
    indent ~ ( "- " ~> rep1( span ) <~ opt( eol ) ) ^^ {
      case indent ~ spans => UnorderedListChunk( indent.size, spans )
    }

}

trait ParserHelper {
  def foldContent( chunks: Seq[ SpanChunk ] ): String =
    chunks.foldLeft( "" )( ( str, chunk ) => str + chunk.content )
}


trait Chunk

trait SpanChunk extends Chunk {
  def content: String
}
case class EmptyChunk( content: String ) extends SpanChunk
case class TextSpanChunk( content: String ) extends SpanChunk
case class StrongSpanChunk( content: String ) extends SpanChunk
case class ItalicSpanChunk( content: String ) extends SpanChunk
case class StrongItalicSpanChunk( content: String ) extends SpanChunk

trait BlockChunk extends Chunk
case class TextChunk( indent: Int, spans: Seq[ SpanChunk ] ) extends Chunk 
case class OrderedListChunk( indent: Int, spans: Seq[ SpanChunk ] ) extends Chunk
case class UnorderedListChunk( indent: Int, spans: Seq[ SpanChunk ] ) extends Chunk
