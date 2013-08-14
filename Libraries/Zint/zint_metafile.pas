unit zint_metafile;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: DWYWBDBU (do what you want, but dont blame us)

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b work in progress

  Notes:
    - this is an adaption of the png-backend from zint
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, Types, zint;

function RenderSymbol(symbol : zint_symbol; ATarget : TZintMetafile) : Integer;

implementation

uses
  zint_common;

const SSET : AnsiString = '0123456789ABCDEF';


procedure draw_bar(symbol : zint_symbol; Canvas : TZintMetafileCanvas; xpos, xlen, ypos, ylen, image_width, image_height : Integer);
{ Draw a rectangle }
var
  png_ypos : Integer;
begin
  png_ypos := image_height - ypos - ylen;
  { This fudge is needed because EPS measures height from the bottom up but
  PNG measures y position from the top down }

  Canvas.Brush.Color := symbol.fgcolor;
  Canvas.Pen.Color := symbol.fgcolor;

  Canvas.Rectangle(xpos, png_ypos, xpos + xlen, png_ypos + ylen);
end;

{ Central bullseye in Maxicode symbols }
procedure draw_bullseye(symbol : zint_symbol; Canvas : TZintMetafileCanvas; image_width, xoffset, yoffset : Integer);
var
  i, j : Integer;
begin
  Canvas.Pen.Color := symbol.fgcolor;
  Canvas.Brush.Style:=bsSolid;

  for I := 0 to 5 do
  begin
    if i mod 2 = 0 then
     Canvas.Brush.Color := symbol.fgcolor
    else
     Canvas.Brush.Color := symbol.bgcolor;


    Canvas.Ellipse(xoffset + trunc(image_width / 3)  + trunc(i*(image_width / 40)),
                    yoffset + trunc((image_width / 3) + trunc(image_width / 100)) + trunc(i * (image_width / 40)),
                    xoffset + trunc(image_width / 3)  + trunc(image_width / (10 / 3)) - trunc(i * (image_width / 40)),
                    yoffset + trunc((image_width / 3) + trunc(image_width / 100)) + trunc(image_width / (10 / 3)) - trunc(i * (image_width / 40)));

     {
    Canvas.Ellipse(xoffset + 1000  + (i*75),
                    yoffset + 1030 + (i*75),
                    xoffset + 1000  + 900 - (i*75),
                    yoffset + 1030 + 900 - (i*75));
 }

  end;
end;

{ Put a hexagon into the pixel buffer }
procedure draw_hexagon(symbol : zint_symbol; Canvas : TZintMetafileCanvas; image_width, xposn, yposn : Integer);
var
  i, j : Integer;
  Points : array[0..5] of TPoint;
begin
  Canvas.Brush.Color := symbol.fgcolor;
  Canvas.Pen.Color := symbol.fgcolor;

  Points[0] := Point(xposn + 0, yposn + trunc(image_width / 120));
  Points[1] := Point(xposn + 0, yposn + trunc(image_width / 40));
  Points[2] := Point(xposn + Trunc((image_width / 120) * sqrt(3)), yposn + trunc(image_width / 30));
  Points[3] := Point(xposn + Trunc((image_width / 60) * sqrt(3)), yposn + trunc(image_width / 40));
  Points[4] := Point(xposn + Trunc((image_width / 60) * sqrt(3)), yposn + trunc(image_width / 120));
  Points[5] := Point(xposn + Trunc((image_width / 120) * sqrt(3)), yposn + 0);

  {$IFDEF FPC}
  Canvas.Polygon(@Points[0], Length(Points), true);
  {$ELSE}
  Canvas.Polygon(Points);
  {$ENDIF}
                                              {
  Canvas.Polygon([Point(xposn + 0, yposn + 25),
                   Point(xposn + 0, yposn + 75),
                   Point(xposn + Trunc(25 * sqrt(3)), yposn + 100),
                   Point(xposn + Trunc(50 * sqrt(3)), yposn + 75),
                   Point(xposn + Trunc(50 * sqrt(3)), yposn + 25),
                   Point(xposn + Trunc(25 * sqrt(3)), yposn + 0)]);}
 (*Canvas.Polygon([point(xposn + 10, yposn + 40),
                  point(xposn + 40, yposn + 10),
                  point(xposn + 60, yposn + 10),
                  point(xposn + 90, yposn + 40),
                  point(xposn + 90, yposn + 70),
                  point(xposn + 60, yposn + 100),
                  point(xposn + 40, yposn + 100),
                  point(xposn + 10, yposn + 70),
                  point(xposn + 10, yposn + 40)]);          *)
end;


procedure draw_string(symbol : zint_symbol; Canvas : TZintMetafileCanvas; input_string : AnsiString; xposn, yposn, image_width, image_height : Integer);
{ Plot a string into the pixel buffer }
var
  i, string_length, string_left_hand : Integer;
begin
	string_length := strlen(input_string);
  string_left_hand := xposn - Trunc( Canvas.TextExtent(input_string).cx / 2);

  Canvas.Pen.Color := symbol.fgcolor;
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(string_left_hand, yposn, input_string);
  Canvas.Brush.Style := bsSolid;
end;

function render_maxi(symbol : zint_symbol; ATarget : TZintMetafile) : Integer;
var
  i, row, column, xposn, yposn : Integer;
  image_height, image_width : Integer;
  error_number : Integer;
  xoffset, yoffset : Integer;
  Canvas : TZintMetafileCanvas;
begin
  error_number := 0;
  xoffset := symbol.border_width + symbol.whitespace_width;
  yoffset := symbol.border_width + symbol.whitespace_width;

  image_width := 3000 + (2 * xoffset );
	image_height := 3000 + (2 * yoffset );

  ATarget.Width := image_width;
  ATarget.Height := image_height;

  Canvas := TZintMetafileCanvas.Create(ATarget{$IFNDEF FPC}, 0{$ENDIF});
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := symbol.bgcolor;
    Canvas.FillRect(Rect(0, 0, image_width, image_height));
    Canvas.Brush.Color := symbol.fgcolor;
    Canvas.Pen.Color := symbol.fgcolor;
    Canvas.Pen.Style := psSolid;

    draw_bullseye(symbol, Canvas, image_width -2 * xoffset, (xoffset), (yoffset));

    for row := 0 to symbol.rows - 1 do
    begin
      yposn := trunc(row * ((image_height - 2 * yoffset) / (100 / 3))); //row * 90
      for column := 0 to symbol.width - 1 do
      begin
        xposn := trunc(column * ((image_width -2 * xoffset) / 30));  //column * 100;
        if (module_is_set(symbol, row, column) <> 0) then
        begin
          if (row and 1) <> 0  then
          begin
            { Odd (reduced) row }
            Inc(xposn, trunc(image_width / 60));  //Inc(xposn, 50);
            draw_hexagon(symbol, Canvas, image_width - 2 * xoffset, xposn + (xoffset), yposn + (yoffset));
          end
          else
          begin
            { Even (full) row }
            draw_hexagon(symbol, Canvas, image_width - 2 * xoffset, xposn + ( xoffset), yposn + (yoffset));
          end;
        end;
      end;
    end;

    if (((symbol.output_options and BARCODE_BOX) <> 0) or ((symbol.output_options and BARCODE_BIND) <> 0)) then
    begin
      { boundary bars }
      draw_bar(symbol, Canvas, 0, image_width , 0, symbol.border_width , image_width, image_height);
      draw_bar(symbol, Canvas, 0, image_width , image_height - (symbol.border_width ), symbol.border_width , image_width, image_height);
    end;

    if ((symbol.output_options and BARCODE_BOX) <> 0) then
    begin
      { side bars }
      draw_bar(symbol, Canvas, 0, symbol.border_width , 0, image_height, image_width, image_height);
      draw_bar(symbol, Canvas, image_width, symbol.border_width, 0, image_height, image_width, image_height);
    end;
  finally
    Canvas.Free;
  end;

  result := error_number; exit;
end;

function render(symbol : zint_symbol; ATarget : TZintMetafile) : Integer;
var
	textdone, main_width, comp_offset, large_bar_count : Integer;
	textpart, addon : AnsiString;
	addon_text_posn, preset_height, large_bar_height : Single;
	i, r, textoffset, yoffset, xoffset, latch, image_width, image_height : Integer;
	addon_latch : Integer;
	this_row, block_width, plot_height, plot_yposn, textpos : Integer;
	row_height, row_posn : Single;
	error_number : Integer;
	default_text_posn : Integer;
	next_yposn : Integer;
	local_text : AnsiString;
  modulePixelWidth : Integer;
  Canvas : TZintMetafileCanvas;
begin
  error_number := 0;
  addon_latch := 0;

	if (symbol.show_hrt <> 0) then
		local_text := symbol.text
  else
		local_text := '';

	textdone := 0;
	main_width := symbol.width;
	addon := '';
	comp_offset := 0;
	addon_text_posn := 0.0;
	row_height := 0;

	if (symbol.height = 0) then
		symbol.height := 50;

	large_bar_count := 0;
	preset_height := 0.0;
	for i := 0 to symbol.rows - 1 do
  begin
		preset_height := preset_height + symbol.row_height[i];
		if (symbol.row_height[i] = 0) then
			Inc(large_bar_count);
	end;

	if (large_bar_count = 0) then
  begin
		symbol.height := Trunc(preset_height);
		large_bar_height := 10;
	end
  else
		large_bar_height := (symbol.height - preset_height) / large_bar_count;

	while (module_is_set(symbol, symbol.rows - 1, comp_offset) = 0) do
		Inc(comp_offset);

	{ Certain symbols need whitespace otherwise characters get chopped off the sides }
	if ((((symbol.symbology = BARCODE_EANX) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_EANX_CC))
		or (symbol.symbology = BARCODE_ISBNX)) then
  begin
		case Length(local_text) of
			13, { EAN 13 }
			16,
			19:
      begin
				if (symbol.whitespace_width = 0) then
					symbol.whitespace_width := 10;
				main_width := 96 + comp_offset;
      end
      else
				main_width := 68 + comp_offset;
		end;
	end;

	if (((symbol.symbology = BARCODE_UPCA) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_UPCA_CC)) then
  begin
		if (symbol.whitespace_width = 0) then
    begin
			symbol.whitespace_width := 10;
			main_width := 96 + comp_offset;
		end;
	end;

	if (((symbol.symbology = BARCODE_UPCE) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_UPCE_CC)) then
  begin
		if (symbol.whitespace_width = 0) then
    begin
			symbol.whitespace_width := 10;
			main_width := 51 + comp_offset;
		end;
	end;

	latch := 0;
	{ Isolate add-on text }
	if (is_extendable(symbol.symbology) <> 0) then
  begin
		for i := 1 to Length(local_text)do
    begin
			if (latch = 1) then
				addon := addon + local_text[i];
			if (symbol.text[i] = '+') then
				latch := 1;
		end;
	end;

	if (Length(local_text) <> 0) then
		textoffset := 9
  else
		textoffset := 0;

	xoffset := symbol.border_width + symbol.whitespace_width;
	yoffset := symbol.border_width;
	image_width := 2 * (symbol.width + xoffset + xoffset);
	image_height := 2 * (symbol.height + textoffset + yoffset + yoffset);

  ATarget.Width := image_width;
  ATarget.Height := image_height;

  Canvas := TZintMetafileCanvas.Create(ATarget{$IFNDEF FPC}, 0{$ENDIF});
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := symbol.bgcolor;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := symbol.bgcolor;
    Canvas.Rectangle(0, 0, image_width, image_height);
    Canvas.Brush.Color := symbol.fgcolor;
    Canvas.Pen.Color := symbol.fgcolor;
    Canvas.Font.Assign(symbol.hrt_font);

    if (((symbol.output_options and BARCODE_BOX) <> 0) or ((symbol.output_options and BARCODE_BIND) <> 0)) then
      default_text_posn := image_height - 17
    else
      default_text_posn := image_height - 17 - symbol.border_width - symbol.border_width;

    row_posn := textoffset + yoffset;
    next_yposn := textoffset + yoffset;
    row_height := 0;

    { Plot the body of the symbol to the pixel buffer }
    for r := 0 to symbol.rows - 1 do
    begin
      this_row := symbol.rows - r - 1; { invert r otherwise plots upside down }
      row_posn := row_posn + row_height;
      plot_yposn := next_yposn;
      if (symbol.row_height[this_row] = 0) then
        row_height := large_bar_height
      else
        row_height := symbol.row_height[this_row];

      next_yposn := Trunc(row_posn + row_height);
      plot_height := next_yposn - plot_yposn;

      i := 0;
      if (module_is_set(symbol, this_row, 0) <> 0) then
        latch := 1
      else
        latch := 0;

      repeat
        block_width := 0;
        repeat
          Inc(block_width);
        until not (module_is_set(symbol, this_row, i + block_width) = module_is_set(symbol, this_row, i));
        if ((addon_latch = 0) and (r = 0) and (i > main_width)) then
        begin
          plot_height := Trunc(row_height - 5.0);
          plot_yposn := Trunc(row_posn - 5.0);
          addon_text_posn := row_posn + row_height - 8.0;
          addon_latch := 1;
        end;
        if (latch = 1) then
        begin
          { a bar }
          draw_bar(symbol, Canvas, (i + xoffset) * 2, block_width * 2, plot_yposn * 2, plot_height * 2, image_width, image_height);
          latch := 0;
        end
        else
        begin
          { a space }
          latch := 1;
        end;
        Inc(i, block_width);

      until not (i < symbol.width);
    end;

    Inc(xoffset, comp_offset);

    if ((((symbol.symbology = BARCODE_EANX) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_EANX_CC)) or (symbol.symbology = BARCODE_ISBNX)) then
    begin
      { guard bar extensions and text formatting for EAN8 and EAN13 }
      case Length(local_text) of
        8, { EAN-8 }
        11,
        14:
        begin
          draw_bar(symbol, Canvas, (0 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (2 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (32 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (34 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (64 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (66 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          textpart := '';
          for i := 1 to 4 do
            textpart := textpart + symbol.text[i];

          textpos := 2 * (17 + xoffset);

          draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
          textpart := '';
          for i := 1 to 4 do
            textpart := textpart +symbol.text[i + 4];
          textpos := 2 * (50 + xoffset);
          draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
          textdone := 1;
          case Length(addon) of
            2:
            begin
              textpos := 2 * (xoffset + 86);
              draw_string(symbol, Canvas, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, image_width, image_height);
            end;
            5:
            begin
              textpos := 2 * (xoffset + 100);
              draw_string(symbol, Canvas, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, image_width, image_height);
            end;
          end;
        end;
        13, { EAN 13 }
        16,
        19:
        begin
          draw_bar(symbol, Canvas, (0 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (2 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (46 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (48 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (92 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          draw_bar(symbol, Canvas, (94 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);

          textpart := symbol.text[1];
          textpos := 2 * (-7 + xoffset);
          draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
          textpart := '';
          for i := 1 to 5 do
            textpart := textpart + symbol.text[i + 1];
          textpos := 2 * (24 + xoffset);
          draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
          textpart := '';
          for i := 1 to 5  do
            textpart := textpart + symbol.text[i + 7];
          textpos := 2 * (71 + xoffset);
          draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
          textdone := 1;
          case Length(addon) of
            2:
            begin
              textpos := 2 * (xoffset + 114);
              draw_string(symbol, Canvas, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, image_width, image_height);
            end;
            5:
            begin
              textpos := 2 * (xoffset + 128);
              draw_string(symbol, Canvas, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, image_width, image_height);
            end;
          end;
        end;
      end;
    end;

    if (((symbol.symbology = BARCODE_UPCA) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_UPCA_CC)) then
    begin
      { guard bar extensions and text formatting for UPCA }
      latch := 1;

      i := 0 + comp_offset;
      repeat
        block_width := 0;
        repeat
          Inc(block_width);
        until not (module_is_set(symbol, symbol.rows - 1, i + block_width) = module_is_set(symbol, symbol.rows - 1, i));
        if (latch = 1) then
        begin
          { a bar }
          draw_bar(symbol, Canvas, (i + xoffset - comp_offset) * 2, block_width * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          latch := 0;
        end
        else
        begin
          { a space }
          latch := 1;
        end;
        Inc(i, block_width);
      until not (i < 11 + comp_offset);
      draw_bar(symbol, Canvas, (46 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
      draw_bar(symbol, Canvas, (48 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
      latch := 1;
      i := 85 + comp_offset;
      repeat
        block_width := 0;
        repeat
          Inc(block_width);
        until not (module_is_set(symbol, symbol.rows - 1, i + block_width) = module_is_set(symbol, symbol.rows - 1, i));
        if (latch = 1) then
        begin
          { a bar }
          draw_bar(symbol, Canvas, (i + xoffset - comp_offset) * 2, block_width * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
          latch := 0;
        end
        else
        begin
          { a space }
          latch := 1;
        end;
        Inc(i, block_width);
      until not (i < 96 + comp_offset);
      textpart := symbol.text[1];
      textpos := 2 * (-5 + xoffset);
      draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
      textpart := '';
      for i := 1 to 5 do
        textpart := textpart + symbol.text[i + 1];
      textpos := 2 * (27 + xoffset);
      draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
      textpart := '';
      for i := 1 to 5 do
        textpart[i] := symbol.text[i + 6];
      textpos := 2 * (68 + xoffset);
      draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
      textpart := symbol.text[11];
      textpos := 2 * (100 + xoffset);
      draw_string(symbol, Canvas, textpart, textpos, default_text_posn, image_width, image_height);
      textdone := 1;
      case Length(addon) of
        2:
        begin
          textpos := 2 * (xoffset + 116);
          draw_string(symbol, Canvas, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, image_width, image_height);
        end;
        5:
        begin
          textpos := 2 * (xoffset + 130);
          draw_string(symbol, Canvas, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, image_width, image_height);
        end;
      end;
    end;

    if (((symbol.symbology = BARCODE_UPCE) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_UPCE_CC)) then
    begin
      { guard bar extensions and text formatting for UPCE }
      draw_bar(symbol, Canvas, (0 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
      draw_bar(symbol, Canvas, (2 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
      draw_bar(symbol, Canvas, (46 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
      draw_bar(symbol, Canvas, (48 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
      draw_bar(symbol, Canvas, (50 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);

      textpart := symbol.text[1];
      textpos := 2 * (-5 + xoffset);
      draw_string(symbol, Canvas, textpart, textpos, default_text_posn,  image_width, image_height);
      for i := 1 to 6 do
        textpart[i] := symbol.text[i + 1];
      textpos := 2 * (24 + xoffset);
      draw_string(symbol, Canvas, textpart, textpos, default_text_posn,  image_width, image_height);
      textpart := symbol.text[8];
      textpos := 2 * (55 + xoffset);
      draw_string(symbol, Canvas, textpart, textpos, default_text_posn,  image_width, image_height);
      textdone := 1;
      case Length(addon) of
        2:
        begin
          textpos := 2 * (xoffset + 70);
          draw_string(symbol, Canvas, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13,  image_width, image_height);
        end;
        5:
        begin
          textpos := 2 * (xoffset + 84);
          draw_string(symbol, Canvas, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13,  image_width, image_height);
        end;
      end;
    end;

    Dec(xoffset, comp_offset);

    { Put boundary bars or box around symbol }
    if (((symbol.output_options and BARCODE_BOX) <> 0) or ((symbol.output_options and BARCODE_BIND) <> 0)) then
    begin
      { boundary bars }
      draw_bar(symbol, Canvas, 0, (symbol.width + xoffset + xoffset) * 2, textoffset * 2, symbol.border_width * 2, image_width, image_height);
      draw_bar(symbol, Canvas, 0, (symbol.width + xoffset + xoffset) * 2, (textoffset + symbol.height + symbol.border_width) * 2, symbol.border_width * 2, image_width, image_height);
      if ((symbol.output_options and BARCODE_BIND) <> 0) then
      begin
        if ((symbol.rows > 1) and (is_stackable(symbol.symbology) = 1)) then
        begin
          { row binding }
          for r := 1 to symbol.rows - 1 do
            draw_bar(symbol, Canvas, xoffset * 2, symbol.width * 2, Trunc((r * row_height) + textoffset + yoffset - 1) * 2, 2 * 2, image_width, image_height);
        end;
      end;
    end;

    if ((symbol.output_options and BARCODE_BOX) <> 0) then
    begin
      { side bars }
      draw_bar(symbol, Canvas, 0, symbol.border_width * 2, textoffset * 2, (symbol.height + (2 * symbol.border_width)) * 2, image_width, image_height);
      draw_bar(symbol, Canvas, (symbol.width + xoffset + xoffset - symbol.border_width) * 2, symbol.border_width * 2, textoffset * 2, (symbol.height + (2 * symbol.border_width)) * 2, image_width, image_height);
    end;

    { Put the human readable text at the bottom }
    if ((textdone = 0) and (Length(local_text) <> 0)) then
    begin
      textpos := Trunc(image_width / 2);
      draw_string(symbol, Canvas, local_text, textpos, default_text_posn,  image_width, image_height);
    end;
  finally
    Canvas.Free;
  end;

  result := error_number; exit;
end;

function RenderSymbol(symbol : zint_symbol; ATarget : TZintMetafile) : Integer;
var
  error : Integer;
begin
  if (symbol.symbology = BARCODE_MAXICODE) then
    error := render_maxi(symbol, ATarget)
  else
    error := render(symbol, ATarget);

  result := error; exit;
end;

end.

