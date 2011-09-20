{
 add the following define to compile without viewer/editor
}

{$DEFINE SLIMRESED}

{$ifdef VER200}
  {$define HavePNGImage}
	{$define HaveGIFImage}
{$endif}

{$ifdef VER210}
  {$define HavePNGImage}
	{$define HaveGIFImage}
{$endif}

{$ifdef VER220}
  {$define HavePNGImage}
	{$define HaveGIFImage}
{$endif}

{$ifdef VER230}
  {$define HavePNGImage}
	{$define HaveGIFImage}
{$endif}
