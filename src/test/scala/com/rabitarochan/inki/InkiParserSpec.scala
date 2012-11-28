package com.rabitarochan.inki

import org.specs2.mutable._

class InkiParserSpec extends Specification {

  import com.rabitarochan.inki.InkiParser._

  "InkiParser - SpanChunk" should {
    "TextSpanChunk" in {
      val src = "This is TextSpanChunk."
      parse( textSpan, src ).get === {
        TextSpanChunk( src )
      }
    }

    "StrongSpanChunk" in {
      parse( strongSpan, " *Strong Span Chunk* " ).get === {
        StrongSpanChunk( "Strong Span Chunk" )
      }
    }

    "ItalicSpanChunk" in {
      parse( italicSpan, " /Italic Span Chunk/ " ).get === {
        ItalicSpanChunk( "Italic Span Chunk" )
      }
    }

    "String and Italic span like /*content*/" in {
      parse( strongItalicSpan, " /*Strong and Italic Chunk*/ " ).get === {
        StrongItalicSpanChunk( "Strong and Italic Chunk" )
      }
    }

    "String and Italic span like */content/*" in {
      parse( strongItalicSpan, " */Italic and Strong Chunk/* " ).get === {
        StrongItalicSpanChunk( "Italic and Strong Chunk" )
      }
    }
  }

  "InkiParser - Empty" should {
    "EmptySpace" in {
      parse( emptySpace, "  " ).get === {
        EmptyChunk( "  " )
      }
    }

    "EmptyLine with space." in {
      parse( emptyLine, "  \r\n" ).get === {
        EmptyChunk( "  \r\n" )
      }
    }

    "EmptyLine without space." in {
      parse( emptyLine, "\n" ).get === {
        EmptyChunk( "\n" )
      }
    }

    "EmptyLines one line." in {
      parse( emptyLines, "    \n" ).get === {
        EmptyChunk( "    \n" )
      }
    }

    "EmptyLines multi lines." in {
      parse( emptyLines, "  \r\n  \r\n" ).get === {
        EmptyChunk( "  \r\n  \r\n" )
      }
    }
  }

  "InkiParser - TextLine" should {
    "TextLine" in {
      parse( textLine, "  this is test." ).get === {
        TextChunk( 2, Seq[ SpanChunk ](
          TextSpanChunk( "this is test." )
        ))
      }
    }

    "TextLine with eol." in {
      parse( textLine, "this is test with eol.\r\n" ).get === {
        TextChunk( 0, Seq[ SpanChunk ](
          TextSpanChunk( "this is test with eol." )
        ))
      }
    }

    "TextLine include TextSpan and StrongSpan" in {
      parse( textLine, "  this is *Strong* test." ).get === {
        TextChunk( 2, Seq[ SpanChunk ](
          TextSpanChunk( "this is" ),
          StrongSpanChunk( "Strong" ),
          TextSpanChunk( "test." )
        ))
      }
    }
  }

  "InkiParser - OrderedList" should {
    "OrderedList" in {
      parse( orderedList, "  + List 1" ).get === {
        OrderedListChunk( 2, Seq[ SpanChunk ](
          TextSpanChunk( "List 1" )
        ))
      }
    }

    "OrderedList with eol" in {
      parse( orderedList, "  + List 2\n" ).get === {
        OrderedListChunk( 2, Seq[ SpanChunk ](
          TextSpanChunk( "List 2" )
        ))
      }
    }
  }

  "InkiParser - UnorderedList" should {
    "UnorderedList" in {
      parse( unorderedList, "- List 1" ).get === {
        UnorderedListChunk( 0, Seq[ SpanChunk ](
          TextSpanChunk( "List 1" )
        ))
      }
    }

    "UnorderedList with eol" in {
      parse( unorderedList, "- List 2\n" ).get === {
        UnorderedListChunk( 0, Seq[ SpanChunk ](
          TextSpanChunk( "List 2" )
        ))
      }
    }
  }
}