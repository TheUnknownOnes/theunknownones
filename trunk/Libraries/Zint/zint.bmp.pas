unit zint.bmp;

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
    - maxicode support is missing
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Graphics, zint.zint;

function bmp_handle(var symbol : zint_symbol; const ABitmap : TBitmap) : Integer;

implementation

uses
  zint.common, zint.font;

const SSET : AnsiString = '0123456789ABCDEF';

const hexagon : array[0..119] of Integer = (
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 1, 1, 1, 0, 0, 0,
	0, 0, 0, 1, 1, 1, 1, 1, 0, 0,
	0, 0, 1, 1, 1, 1, 1, 1, 1, 0,
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 0, 1, 1, 1, 1, 1, 1, 1, 0,
	0, 0, 0, 1, 1, 1, 1, 1, 0, 0,
	0, 0, 0, 0, 1, 1, 1, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

const bullseye_compressed : array[0..1115] of Cardinal = (
	0,0,0,0,0,255,248,0,0,0,0,0,
	0,0,0,0,31,255,255,192,0,0,0,0,
	0,0,0,1,255,255,255,252,0,0,0,0,
	0,0,0,7,255,255,255,255,0,0,0,0,
	0,0,0,31,255,255,255,255,192,0,0,0,
	0,0,0,127,255,255,255,255,240,0,0,0,
	0,0,1,255,255,255,255,255,252,0,0,0,
	0,0,7,255,255,255,255,255,255,0,0,0,
	0,0,15,255,255,0,7,255,255,128,0,0,
	0,0,63,255,240,0,0,127,255,224,0,0,
	0,0,127,255,128,0,0,15,255,240,0,0,
	0,0,255,252,0,0,0,1,255,248,0,0,
	0,1,255,240,0,0,0,0,127,252,0,0,
	0,3,255,224,0,0,0,0,63,254,0,0,
	0,7,255,128,0,0,0,0,15,255,0,0,
	0,15,255,0,0,0,0,0,7,255,128,0,
	0,31,252,0,0,127,240,0,1,255,192,0,
	0,63,248,0,7,255,255,0,0,255,224,0,
	0,127,240,0,63,255,255,224,0,127,240,0,
	0,127,224,0,255,255,255,248,0,63,240,0,
	0,255,192,1,255,255,255,252,0,31,248,0,
	1,255,128,7,255,255,255,255,0,15,252,0,
	1,255,0,15,255,255,255,255,128,7,252,0,
	3,255,0,63,255,255,255,255,224,7,254,0,
	3,254,0,127,255,192,31,255,240,3,254,0,
	7,252,0,255,252,0,1,255,248,1,255,0,
	7,252,1,255,240,0,0,127,252,1,255,0,
	15,248,1,255,192,0,0,31,252,0,255,128,
	15,240,3,255,128,0,0,15,254,0,127,128,
	31,240,7,255,0,0,0,7,255,0,127,192,
	31,224,7,254,0,0,0,3,255,0,63,192,
	63,224,15,252,0,0,0,1,255,128,63,224,
	63,224,31,248,0,63,192,0,255,192,63,224,
	63,192,31,240,0,255,240,0,127,192,31,224,
	63,192,63,224,3,255,252,0,63,224,31,224,
	127,192,63,224,7,255,254,0,63,224,31,240,
	127,128,63,192,15,255,255,0,31,224,15,240,
	127,128,127,192,31,255,255,128,31,240,15,240,
	127,128,127,128,63,255,255,192,15,240,15,240,
	127,128,127,128,63,255,255,192,15,240,15,240,
	255,0,127,128,127,240,255,224,15,240,7,240,
	255,0,255,128,127,192,63,224,15,248,7,240,
	255,0,255,0,255,128,31,240,7,248,7,240,
	255,0,255,0,255,128,31,240,7,248,7,240,
	255,0,255,0,255,0,15,240,7,248,7,240,
	255,0,255,0,255,0,15,240,7,248,7,240,
	255,0,255,0,255,0,15,240,7,248,7,240,
	255,0,255,0,255,0,15,240,7,248,7,240,
	255,0,255,0,255,128,31,240,7,248,7,240,
	255,0,255,0,255,128,31,240,7,248,7,240,
	255,0,255,0,127,192,63,224,7,248,7,240,
	255,0,255,128,127,240,255,224,15,248,7,240,
	255,0,127,128,63,255,255,192,15,240,7,240,
	127,128,127,128,63,255,255,192,15,240,15,240,
	127,128,127,128,31,255,255,128,15,240,15,240,
	127,128,127,192,15,255,255,0,31,240,15,240,
	127,128,63,192,7,255,254,0,31,224,15,240,
	127,192,63,224,3,255,252,0,63,224,31,240,
	63,192,63,224,0,255,240,0,63,224,31,224,
	63,192,31,240,0,63,192,0,127,192,31,224,
	63,224,31,248,0,0,0,0,255,192,63,224,
	63,224,15,252,0,0,0,1,255,128,63,224,
	31,224,7,254,0,0,0,3,255,0,63,192,
	31,240,7,255,0,0,0,7,255,0,127,192,
	15,240,3,255,128,0,0,15,254,0,127,128,
	15,248,1,255,192,0,0,31,252,0,255,128,
	7,252,1,255,240,0,0,127,252,1,255,0,
	7,252,0,255,252,0,1,255,248,1,255,0,
	3,254,0,127,255,192,31,255,240,3,254,0,
	3,255,0,63,255,255,255,255,224,7,254,0,
	1,255,0,15,255,255,255,255,128,7,252,0,
	1,255,128,7,255,255,255,255,0,15,252,0,
	0,255,192,1,255,255,255,252,0,31,248,0,
	0,127,224,0,255,255,255,248,0,63,240,0,
	0,127,240,0,63,255,255,224,0,127,240,0,
	0,63,248,0,7,255,255,0,0,255,224,0,
	0,31,252,0,0,127,240,0,1,255,192,0,
	0,15,255,0,0,0,0,0,7,255,128,0,
	0,7,255,128,0,0,0,0,15,255,0,0,
	0,3,255,224,0,0,0,0,63,254,0,0,
	0,1,255,240,0,0,0,0,127,252,0,0,
	0,0,255,252,0,0,0,1,255,248,0,0,
	0,0,127,255,128,0,0,15,255,240,0,0,
	0,0,63,255,240,0,0,127,255,224,0,0,
	0,0,15,255,255,0,7,255,255,128,0,0,
	0,0,7,255,255,255,255,255,255,0,0,0,
	0,0,1,255,255,255,255,255,252,0,0,0,
	0,0,0,127,255,255,255,255,240,0,0,0,
	0,0,0,31,255,255,255,255,192,0,0,0,
	0,0,0,7,255,255,255,255,0,0,0,0,
	0,0,0,1,255,255,255,252,0,0,0,0,
	0,0,0,0,31,255,255,192,0,0,0,0,
	0,0,0,0,0,255,248,0,0,0,0,0
);

procedure draw_bar(const ABitmap : TBitmap; xpos, xlen, ypos, ylen, image_width, image_height : Integer);
{ Draw a rectangle }
var
  png_ypos : Integer;
begin
  png_ypos := image_height - ypos - ylen;
  { This fudge is needed because EPS measures height from the bottom up but
  PNG measures y position from the top down }

  ABitmap.Canvas.Rectangle(xpos, png_ypos, xpos + xlen, png_ypos + ylen);
end;

function bullseye_pixel(row, col : Integer) : Integer;
var
  block_val, block_pos, return_val : Integer;
begin
  block_val := bullseye_compressed[(row * 12) + (col div 8)];
  return_val := 0;
  block_pos := col mod 8;

  case block_pos of
    0: if ((block_val and $80) <> 0) then return_val := 1;
    1: if ((block_val and $40) <> 0) then return_val := 1;
    2: if ((block_val and $20) <> 0) then return_val := 1;
    3: if ((block_val and $10) <> 0) then return_val := 1;
    4: if ((block_val and $08) <> 0) then return_val := 1;
    5: if ((block_val and $04) <> 0) then return_val := 1;
    6: if ((block_val and $02) <> 0) then return_val := 1;
    7: if ((block_val and $01) <> 0) then return_val := 1;
  end;

  result := return_val; exit;
end;

{ Central bullseye in Maxicode symbols }
procedure draw_bullseye(const ABitmap : TBitmap; image_width, xoffset, yoffset : Integer);
var
  i, j : Integer;
begin
  for j := 103 to 195 do
  begin
    for i := 0 to 94 do
    begin
      if (bullseye_pixel(j - 103, i) <> 0) then
      begin
        ABitmap.Canvas.Pixels[i + 99 + xoffset, j + yoffset] := ABitmap.Canvas.pen.Color;
      end;
    end;
  end;
end;

{ Put a hexagon into the pixel buffer }
procedure draw_hexagon(const ABitmap : TBitmap; image_width, xposn, yposn : Integer);
var
  i, j : Integer;
begin
  for i := 0 to 11 do
  begin
    for j := 0 to 9 do
    begin
      if (hexagon[(i * 10) + j] = 1) then
      begin
        ABitmap.Canvas.Pixels[xposn + j, i + yposn] := ABitmap.Canvas.pen.Color;
      end;
    end;
  end;
end;

procedure draw_letter(const ABitmap : TBitmap; letter : AnsiChar; xposn, yposn, smalltext, image_width, image_height : Integer);
{ Put a letter into a position }
var
  _letter : Byte absolute letter;
  skip, i, j, glyph_no, alphabet : Integer;
begin
  skip := 0;
  alphabet := 0;

  if (_letter < 33) then skip := 1;
  if ((_letter > 127) and (_letter < 161)) then skip := 1;

  if (skip = 0) then
  begin
    if (_letter > 128) then
    begin
      alphabet := 1;
      glyph_no := _letter - 161;
    end
    else
      glyph_no := _letter - 33;

    if (smalltext <> 0) then
    begin
      for i := 0 to 8 do
      begin
        for j := 0 to 4 do
        begin
          if (alphabet = 0) then
          begin
            if (small_font[(glyph_no * 5) + (i * 475) + j - 1] = 1) then
              ABitmap.Canvas.Pixels[xposn + j, yposn + i] := ABitmap.Canvas.pen.Color;
          end
          else
          begin
            if (small_font_extended[(glyph_no * 5) + (i * 475) + j - 1] = 1) then
              ABitmap.Canvas.Pixels[xposn + j, yposn + i] := ABitmap.Canvas.pen.Color;
          end;
        end;
      end;
    end
    else
    begin
      for i := 0 to 13 do
      begin
        for j := 0 to 6  do
        begin
          if (alphabet = 0) then
          begin
            if (ascii_font[(glyph_no * 7) + (i * 665) + j - 1] = 1) then
              ABitmap.Canvas.Pixels[xposn + j, yposn + i] := ABitmap.Canvas.pen.Color;
          end
          else
          begin
            if (ascii_ext_font[(glyph_no * 7) + (i * 665) + j - 1] = 1) then
              ABitmap.Canvas.Pixels[xposn + j, yposn + i] := ABitmap.Canvas.pen.Color;
          end;
        end;
      end;
    end;
  end;
end;

procedure draw_string(const ABitmap : TBitmap; input_string : AnsiString; xposn, yposn, smalltext, image_width, image_height : Integer);
{ Plot a string into the pixel buffer }
var
  i, string_length, string_left_hand : Integer;
begin
	string_length := strlen(input_string);
  string_left_hand := xposn - Trunc((7 * string_length) / 2);

  for i := 1 to string_length do
    draw_letter(ABitmap, input_string[i], string_left_hand + (i * 7), yposn, smalltext, image_width, image_height);
end;

function maxi_bmp_plot(var symbol : zint_symbol; const ABitmap : TBitmap) : Integer;
var
  i, row, column, xposn, yposn : Integer;
  image_height, image_width : Integer;
  error_number : Integer;
  xoffset, yoffset : Integer;
  BGColor, FGColor : TColor;
begin
  xoffset := symbol.border_width + symbol.whitespace_width;
  yoffset := symbol.border_width;
  image_width := 300 + (2 * xoffset * 2);
  image_height := 300 + (2 * yoffset * 2);

  BGColor := StringToColor(symbol.bgcolour);
  FGColor := StringToColor(symbol.fgcolour);
  ABitmap.SetSize(image_width, image_height);
  ABitmap.PixelFormat := pf24bit;
  ABitmap.Canvas.Brush.Color := BGColor;
  ABitmap.Canvas.Brush.Style := bsSolid;
  ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
  ABitmap.Canvas.Brush.Color := FGColor;
  ABitmap.Canvas.Pen.Color := FGColor;
  ABitmap.Canvas.Pen.Style := psSolid;

  draw_bullseye(ABitmap, image_width, (2 * xoffset), (2 * yoffset));

  for row := 0 to symbol.rows - 1 do
  begin
    yposn := row * 9;
    for column := 0 to symbol.width - 1 do
    begin
      xposn := column * 10;
      if (module_is_set(symbol, row, column) <> 0) then
      begin
        if (row and 1) <> 0  then
        begin
          { Odd (reduced) row }
          Inc(xposn, 5);
          draw_hexagon(ABitmap, image_width, xposn + (2 * xoffset), yposn + (2 * yoffset));
        end
        else
        begin
          { Even (full) row }
          draw_hexagon(ABitmap, image_width, xposn + (2 * xoffset), yposn + (2 * yoffset));
        end;
      end;
    end;
  end;

  if (((symbol.output_options and BARCODE_BOX) <> 0) or ((symbol.output_options and BARCODE_BIND) <> 0)) then
  begin
    { boundary bars }
    draw_bar(ABitmap, 0, image_width, 0, symbol.border_width * 2, image_width, image_height);
    draw_bar(ABitmap, 0, image_width, 300 + (symbol.border_width * 2), symbol.border_width * 2, image_width, image_height);
  end;

  if ((symbol.output_options and BARCODE_BOX) <> 0) then
  begin
    { side bars }
    draw_bar(ABitmap, 0, symbol.border_width * 2, 0, image_height, image_width, image_height);
    draw_bar(ABitmap, 300 + ((symbol.border_width + symbol.whitespace_width + symbol.whitespace_width) * 2), symbol.border_width * 2, 0, image_height, image_width, image_height);
  end;

  result := error_number; exit;
end;

function bmp_plot(var symbol : zint_symbol; const ABitmap : TBitmap) : Integer;
var
	textdone, main_width, comp_offset, large_bar_count : Integer;
	textpart, addon : AnsiString;
	addon_text_posn, preset_height, large_bar_height : Single;
	i, r, textoffset, yoffset, xoffset, latch, image_width, image_height : Integer;
	addon_latch, smalltext : Integer;
	this_row, block_width, plot_height, plot_yposn, textpos : Integer;
	row_height, row_posn : Single;
	error_number : Integer;
	default_text_posn : Integer;
	next_yposn : Integer;
	local_text : AnsiString;
  BGColor, FGColor : TColor;
begin
  addon_latch := 0;
  smalltext := 0;

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
	if ((symbol.output_options and SMALL_TEXT) <> 0) then
		smalltext := 1;

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

  BGColor := StringToColor(symbol.bgcolour);
  FGColor := StringToColor(symbol.fgcolour);
  ABitmap.SetSize(image_width, image_height);
  ABitmap.PixelFormat := pf24bit;
  ABitmap.Canvas.Brush.Color := BGColor;
  ABitmap.Canvas.Brush.Style := bsSolid;
  ABitmap.Canvas.FillRect(ABitmap.Canvas.ClipRect);
  ABitmap.Canvas.Brush.Color := FGColor;
  ABitmap.Canvas.Pen.Color := FGColor;
  ABitmap.Canvas.Pen.Style := psSolid;

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
				draw_bar(ABitmap, (i + xoffset) * 2, block_width * 2, plot_yposn * 2, plot_height * 2, image_width, image_height);
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
				draw_bar(ABitmap, (0 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (2 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (32 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (34 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (64 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (66 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
        textpart := '';
				for i := 1 to 4 do
					textpart := textpart + symbol.text[i];

				textpos := 2 * (17 + xoffset);

				draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
        textpart := '';
				for i := 1 to 4 do
					textpart := textpart +symbol.text[i + 4];
				textpos := 2 * (50 + xoffset);
				draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
				textdone := 1;
				case Length(addon) of
					2:
          begin
						textpos := 2 * (xoffset + 86);
						draw_string(ABitmap, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, smalltext, image_width, image_height);
					end;
					5:
          begin
						textpos := 2 * (xoffset + 100);
						draw_string(ABitmap, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, smalltext, image_width, image_height);
					end;
				end;
			end;
			13, { EAN 13 }
			16,
			19:
      begin
				draw_bar(ABitmap, (0 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (2 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (46 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (48 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (92 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				draw_bar(ABitmap, (94 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);

				textpart := symbol.text[1];
				textpos := 2 * (-7 + xoffset);
				draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
        textpart := '';
				for i := 1 to 5 do
					textpart := textpart + symbol.text[i + 1];
				textpos := 2 * (24 + xoffset);
				draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
        textpart := '';
				for i := 1 to 5  do
					textpart := textpart + symbol.text[i + 7];
				textpos := 2 * (71 + xoffset);
				draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
				textdone := 1;
				case Length(addon) of
					2:
          begin
						textpos := 2 * (xoffset + 114);
						draw_string(ABitmap, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, smalltext, image_width, image_height);
					end;
					5:
          begin
            textpos := 2 * (xoffset + 128);
						draw_string(ABitmap, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, smalltext, image_width, image_height);
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
				draw_bar(ABitmap, (i + xoffset - comp_offset) * 2, block_width * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
				latch := 0;
			end
      else
      begin
				{ a space }
				latch := 1;
			end;
			Inc(i, block_width);
		until not (i < 11 + comp_offset);
		draw_bar(ABitmap, (46 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
		draw_bar(ABitmap, (48 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
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
				draw_bar(ABitmap, (i + xoffset - comp_offset) * 2, block_width * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
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
		draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
    textpart := '';
		for i := 1 to 5 do
			textpart := textpart + symbol.text[i + 1];
		textpos := 2 * (27 + xoffset);
		draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
    textpart := '';
		for i := 1 to 5 do
			textpart[i] := symbol.text[i + 6];
		textpos := 2 * (68 + xoffset);
		draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
		textpart := symbol.text[11];
		textpos := 2 * (100 + xoffset);
		draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
		textdone := 1;
		case Length(addon) of
			2:
      begin
				textpos := 2 * (xoffset + 116);
				draw_string(ABitmap, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, smalltext, image_width, image_height);
      end;
			5:
      begin
				textpos := 2 * (xoffset + 130);
				draw_string(ABitmap, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, smalltext, image_width, image_height);
      end;
		end;
	end;

	if (((symbol.symbology = BARCODE_UPCE) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_UPCE_CC)) then
  begin
		{ guard bar extensions and text formatting for UPCE }
		draw_bar(ABitmap, (0 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
		draw_bar(ABitmap, (2 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
		draw_bar(ABitmap, (46 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
		draw_bar(ABitmap, (48 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);
		draw_bar(ABitmap, (50 + xoffset) * 2, 1 * 2, (4 + yoffset) * 2, 5 * 2, image_width, image_height);

		textpart := symbol.text[1];
		textpos := 2 * (-5 + xoffset);
		draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
		for i := 1 to 6 do
			textpart[i] := symbol.text[i + 1];
		textpos := 2 * (24 + xoffset);
		draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
		textpart := symbol.text[8];
		textpos := 2 * (55 + xoffset);
		draw_string(ABitmap, textpart, textpos, default_text_posn, smalltext, image_width, image_height);
		textdone := 1;
		case Length(addon) of
			2:
      begin
				textpos := 2 * (xoffset + 70);
				draw_string(ABitmap, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, smalltext, image_width, image_height);
			end;
			5:
      begin
				textpos := 2 * (xoffset + 84);
				draw_string(ABitmap, addon, textpos, image_height - Trunc(addon_text_posn * 2) - 13, smalltext, image_width, image_height);
      end;
		end;
	end;

	Dec(xoffset, comp_offset);

	{ Put boundary bars or box around symbol }
	if (((symbol.output_options and BARCODE_BOX) <> 0) or ((symbol.output_options and BARCODE_BIND) <> 0)) then
  begin
		{ boundary bars }
		draw_bar(ABitmap, 0, (symbol.width + xoffset + xoffset) * 2, textoffset * 2, symbol.border_width * 2, image_width, image_height);
		draw_bar(ABitmap, 0, (symbol.width + xoffset + xoffset) * 2, (textoffset + symbol.height + symbol.border_width) * 2, symbol.border_width * 2, image_width, image_height);
		if ((symbol.output_options and BARCODE_BIND) <> 0) then
    begin
			if ((symbol.rows > 1) and (is_stackable(symbol.symbology) = 1)) then
      begin
				{ row binding }
				for r := 1 to symbol.rows - 1 do
					draw_bar(ABitmap, xoffset * 2, symbol.width * 2, Trunc((r * row_height) + textoffset + yoffset - 1) * 2, 2 * 2, image_width, image_height);
			end;
		end;
	end;

	if ((symbol.output_options and BARCODE_BOX) <> 0) then
  begin
		{ side bars }
		draw_bar(ABitmap, 0, symbol.border_width * 2, textoffset * 2, (symbol.height + (2 * symbol.border_width)) * 2, image_width, image_height);
		draw_bar(ABitmap, (symbol.width + xoffset + xoffset - symbol.border_width) * 2, symbol.border_width * 2, textoffset * 2, (symbol.height + (2 * symbol.border_width)) * 2, image_width, image_height);
	end;

	{ Put the human readable text at the bottom }
	if ((textdone = 0) and (Length(local_text) <> 0)) then
  begin
		textpos := Trunc(image_width / 2);
		draw_string(ABitmap, local_text, textpos, default_text_posn, smalltext, image_width, image_height);
	end;

	result := error_number; exit;
end;

function bmp_handle(var symbol : zint_symbol; const ABitmap : TBitmap) : Integer;
var
  error : Integer;
begin
  if (symbol.symbology = BARCODE_MAXICODE) then
    error := maxi_bmp_plot(symbol, ABitmap)
  else
    error := bmp_plot(symbol, ABitmap);

  result := error; exit;
end;

end.

