function DrawContent()
{
  _gel('content_div').innerHTML = '<a href="www.google.de">blubb</a>';
}

_IG_RegisterOnloadHandler(DrawContent);