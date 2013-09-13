unit zint_render_;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
 SysUtils, zint;

 function render_plot(symbol : zint_symbol; width : Single; height : Single) : Integer;

implementation

uses
  zint_common, zint_helper;

const GL_CONST = 2.8346;

{
 * Create a new line with its memory allocated ready for adding to the
 * rendered structure.
 *
 * This is much quicker than writing out each line manually (in some cases!)
 }
function render_plot_create_line(x, y, width, _length : Single) : Pzint_render_line;
begin
  New(Result);
  Result^.next := nil;
  Result^.x := x;
  Result^.y := y;
  Result^.width := width;
  Result^.length := _length;
end;

{
 * Add the line to the current rendering and update the last line's
 * next value.
 }
function render_plot_add_line(symbol : zint_symbol; line : Pzint_render_line; var last_line : Pzint_render_line) : Integer;
begin
  if Assigned(last_line) then
    last_line^.next := line
  else
    symbol.rendered^.lines := line; // first line

  last_line := line;
  result := 1; exit;
end;

function render_plot_create_ring(x, y, radius, line_width : Single) : Pzint_render_ring;
begin
  New(Result);
  Result^.next := nil;
  Result^.x := x;
  Result^.y := y;
  Result^.radius := radius;
  Result^.line_width := line_width;
end;

function render_plot_add_ring(symbol : zint_symbol; ring : Pzint_render_ring; var last_ring : Pzint_render_ring) : Integer;
begin
	if Assigned(last_ring) then
		last_ring^.next := ring
	else
		symbol.rendered^.rings := ring; // first ring

	last_ring := ring;
	result := 1; exit;
end;

function render_plot_create_hexagon(x, y, width, height : Single) : Pzint_render_hexagon;
begin
  New(Result);
  Result^.next := nil;
  Result^.x := x;
  Result^.y := y;
  Result^.width := width;
  Result^.height := height;
end;

function render_plot_add_hexagon(symbol : zint_symbol; hexagon : Pzint_render_hexagon; var last_hexagon : Pzint_render_hexagon) : Integer;
begin
  if Assigned(last_hexagon) then
    last_hexagon^.next := hexagon
  else
    symbol.rendered^.hexagons := hexagon; // first hexagon

  last_hexagon := hexagon;
  result := 1; exit;
end;

{
 * Add a string structure to the symbol.
 * Coordinates assumed to be from top-center.
 }
function render_plot_add_string(symbol : zint_symbol;
    const text : TArrayOfChar; x, y, fsize, width : Single;
    var last_string : Pzint_render_string) : Integer;
var
  _string : Pzint_render_string;
begin
  New(_string);
  _string^.next := nil;
  _string^.x := x;
  _string^.y := y;
  _string^.width := width;
  _string^.fsize := fsize;
  _string^.length := strlen(text);
  _string^.text := ArrayOfCharToString(text);

  if Assigned(last_string) then
    last_string^.next := _string
  else
    symbol.rendered^.strings := _string; // First character
  last_string := _string;

  result := 1; exit;
end;

function render_plot(symbol : zint_symbol; width : Single; height : Single) : Integer;
var
  render : Pzint_render;
  line, last_line : Pzint_render_line;
  last_string : Pzint_render_string;
  ring, last_ring : Pzint_render_ring;
  hexagon, last_hexagon : Pzint_render_hexagon;

  i, r, block_width, latch, this_row : Integer;
  textpos, textwidth, large_bar_height, preset_height, row_height, row_posn : Single;
  text_offset, text_height, xoffset, yoffset, textdone, main_symbol_width_x, addon_width_x : Integer;
  addon, textpart : TArrayOfChar;
  large_bar_count, symbol_lead_in, total_symbol_width_x, total_area_width_x : Integer;
  addon_text_posn : Single;
  default_text_posn : Single;
  scaler : Single;
  hide_text : Integer;
  required_aspect : Single;
  symbol_aspect : Single;
  x_dimension : Single;
  upceanflag : Integer;
  addon_latch : Integer;
begin
  SetLength(addon, 6);
  SetLength(textpart, 10);
  line := nil;
  last_line := nil;
  last_string := nil;
  ring := nil;
  last_ring := nil;
  hexagon := nil;
  last_hexagon := nil;
  textpos := 0;
  textwidth := 0;
  large_bar_height := 0;
  preset_height := 0;
  row_height := 0;
  row_posn := 0;
  hide_text := 0;
  symbol_aspect := 1;
  upceanflag := 0;

  // Allocate memory for the rendered version
  New(render);
  symbol.rendered := render;
  render^.lines := nil;
  render^.strings := nil;
  render^.rings := nil;
  render^.hexagons := nil;

  row_height := 0;
  textdone := 0;
  textpos := 0.0;
  main_symbol_width_x := symbol.width;
  strcpy(addon, '');
  symbol_lead_in := 0;
  addon_text_posn := 0.0;
  addon_width_x := 0;

  {
   * Determine if there will be any addon texts and text height
   }
  latch := 0;
  r := 0;
  { Isolate add-on text }
  if (is_extendable(symbol.symbology) <> 0) then
  begin
    for i := 0 to ustrlen(symbol.text) - 1 do
    begin
      if (latch = 1) then
      begin
        addon[r] := Chr(symbol.text[i]);
        Inc(r);
      end;
      if (symbol.text[i] = Ord('+')) then
        latch := 1;
    end;
  end;
  addon[r] := #0;

  if ((symbol.show_hrt = 0) or (ustrlen(symbol.text) = 0)) then
  begin
    hide_text := 1;
    text_offset := 0;
    text_height := 0;
  end
  else
  begin
    text_height := 9;
    text_offset := 2;
  end;


  {
   * Calculate the width of the barcode, especially if there are any extra
   * borders or white space to add.
   }

  while((module_is_set(symbol, symbol.rows - 1, symbol_lead_in)) = 0) do
    Inc(symbol_lead_in);

  { Certain symbols need whitespace otherwise characters get chopped off the sides }
  if ((((symbol.symbology = BARCODE_EANX) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_EANX_CC))
    or (symbol.symbology = BARCODE_ISBNX)) then
  begin
    case ustrlen(symbol.text) of
      13, { EAN 13 }
      16,
      19:
      begin
        if (symbol.whitespace_width = 0) then
          symbol.whitespace_width := 10;
        main_symbol_width_x := 96 + symbol_lead_in;
        upceanflag := 13;
      end;
      2:
      begin
        main_symbol_width_x := 22 + symbol_lead_in;
        upceanflag := 2;
      end;
      5:
      begin
        main_symbol_width_x := 49 + symbol_lead_in;
        upceanflag := 5;
      end;
      else
        main_symbol_width_x := 68 + symbol_lead_in;
        upceanflag := 8;
    end;
    case ustrlen(symbol.text) of
      11,
      16:
        { EAN-2 add-on }
        addon_width_x := 31;
      14,
      19:
        { EAN-5 add-on }
        addon_width_x := 58;
    end;
  end;

  if (((symbol.symbology = BARCODE_UPCA) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_UPCA_CC)) then
  begin
    upceanflag := 12;
    if (symbol.whitespace_width < 10) then
    begin
      symbol.whitespace_width := 10;
      main_symbol_width_x := 96 + symbol_lead_in;
    end;
    case ustrlen(symbol.text) of
      15:
        { EAN-2 add-on }
        addon_width_x := 31;
      18:
        { EAN-5 add-on }
        addon_width_x := 58;
    end;
  end;

  if (((symbol.symbology = BARCODE_UPCE) and (symbol.rows = 1)) or (symbol.symbology = BARCODE_UPCE_CC)) then
  begin
    upceanflag := 6;
    if (symbol.whitespace_width = 0) then
    begin
      symbol.whitespace_width := 10;
      main_symbol_width_x := 51 + symbol_lead_in;
    end;
    case ustrlen(symbol.text) of
      11:
        { EAN-2 add-on }
        addon_width_x := 31;
      14:
        { EAN-5 add-on }
        addon_width_x := 58;
    end;
  end;

  total_symbol_width_x := main_symbol_width_x + addon_width_x;
  total_area_width_x := total_symbol_width_x + (2 * (symbol.border_width + symbol.whitespace_width));

  xoffset := symbol.border_width + symbol.whitespace_width;
  yoffset := symbol.border_width;

  // Determine if height should be overridden
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
    required_aspect := width / height;
    symbol_aspect := (total_symbol_width_x + (2 * xoffset)) / (preset_height + (2 * yoffset) + text_offset + text_height);
    symbol.height := Trunc(preset_height);
    if (required_aspect > symbol_aspect) then
    begin
      { the area is too wide }
      scaler := height / (preset_height + (2 * yoffset) + text_offset + text_height);
      render^.width := symbol_aspect * height;
      render^.height := height;
    end
    else
    begin
      { the area is too high }
      scaler := width / (total_symbol_width_x + (2 * xoffset));
      render^.width := width;
      render^.height := width / symbol_aspect;
    end;
  end
  else
  begin
    scaler := width / (total_symbol_width_x + (2 * xoffset));
    symbol.height := Trunc((height / scaler) - ((2 * yoffset) + text_offset + text_height));

    render^.width := width;
    render^.height := height;
  end;

  if (large_bar_count <> 0) then
    large_bar_height := (symbol.height - preset_height) / large_bar_count;

  if (((symbol.output_options and BARCODE_BOX) <> 0) or ((symbol.output_options and BARCODE_BIND) <> 0)) then
    default_text_posn := (symbol.height + text_offset + symbol.border_width + symbol.border_width) * scaler
  else
    default_text_posn := (symbol.height + text_offset + symbol.border_width) * scaler;

  x_dimension := render^.width / total_area_width_x;
  x_dimension := x_dimension / GL_CONST;

  //{ Set minimum size of symbol }
  //{ Barcode must be at least 2mm high by 2mm across }
  //if (render^.height < ((x_dimension * ((2 * symbol.border_width) + text_offset + text_height)) + 2.0) * GL_CONST) then
  //  render^.height := ((x_dimension * ((2 * symbol.border_width) + text_offset + text_height)) + 2.0) * GL_CONST;
  //
  //if (render^.width < (2.0 * GL_CONST)) then
  //  render^.width := (2.0 * GL_CONST);
  //
  //if (symbol.symbology = BARCODE_CODABAR) then
  //begin
  //  { The minimum X-dimension of Codabar is 0.191mm. The minimum bar height is 5mm }
  //  if (x_dimension < 0.191) then
  //    render^.width := 0.191 * GL_CONST * total_area_width_x;
  //  if (render^.height < ((x_dimension * ((2 * symbol.border_width) + text_offset + text_height)) + 5.0) * GL_CONST) then
  //    render^.height := ((x_dimension * ((2 * symbol.border_width) + text_offset + text_height)) + 5.0) * GL_CONST;
  //end;
  //
  //if (symbol.symbology = BARCODE_CODE49) then
  //begin
  //  { The minimum X-dimension of Code 49 is 0.191mm }
  //  if (x_dimension < 0.191) then
  //  begin
  //    render^.width := 0.191 * GL_CONST * total_area_width_x;
  //    render^.height := render^.width / symbol_aspect;
  //  end;
  //end;
  //
  //if (upceanflag <> 0) then
  //begin
  //  { The X-dimension of UPC/EAN symbols is fixed at 0.330mm }
  //  { NOTE: This code will need adjustment before it correctly deals with composite symbols }
  //  render^.width := 0.330 * GL_CONST * total_area_width_x;
  //  { The height is also fixed }
  //  case upceanflag of
  //    6,
  //    12,
  //    13:
  //      { UPC-A, UPC-E and EAN-13 }
  //      { Height of bars should be 22.85mm }
  //      render^.height := ((0.330 * ((2 * symbol.border_width) + text_offset + text_height)) + 22.85) * GL_CONST;
  //    8:
  //      { EAN-8 }
  //      { Height of bars should be 18.23mm }
  //      render^.height := ((0.330 * ((2 * symbol.border_width) + text_offset + text_height)) + 18.23) * GL_CONST;
  //    else
  //      { EAN-2 and EAN-5 }
  //      { Height of bars should be 21.10mm }
  //      render^.height := ((0.330 * ((2 * symbol.border_width) + text_offset + text_height)) + 21.10) * GL_CONST;
  //  end;
  //end;
  //
  //if (symbol.symbology = BARCODE_ONECODE) then
  //begin
  //  { The size of USPS Intelligent Mail barcode is fixed }
  //  render^.width := 0.508 * GL_CONST * total_area_width_x;
  //  render^.height := 4.064 * GL_CONST;
  //end;
  //
  //if ((symbol.symbology = BARCODE_POSTNET) or (symbol.symbology = BARCODE_PLANET)) then
  //begin
  //  { The size of PostNet and PLANET are fized }
  //  render^.width := 0.508 * GL_CONST * total_area_width_x;
  //  render^.height := 2.921 * GL_CONST;
  //end;
  //
  //if (((symbol.symbology = BARCODE_AUSPOST) or (symbol.symbology = BARCODE_AUSREPLY)) or
  //  ((symbol.symbology = BARCODE_AUSROUTE) or (symbol.symbology = BARCODE_AUSREDIRECT))) then
  //begin
  //  { Australia Post use the same sizes as USPS }
  //  render^.width := 0.508 * GL_CONST * total_area_width_x;
  //  render^.height := 4.064 * GL_CONST;
  //end;
  //
  //if ((symbol.symbology = BARCODE_RM4SCC) or (symbol.symbology = BARCODE_KIX)) then
  //begin
  //  { Royal Mail and KIX Code uses 22 bars per inch }
  //  render^.width := 0.577 * GL_CONST * total_area_width_x;
  //  render^.height := 5.22 * GL_CONST;
  //end;

  if (symbol.symbology = BARCODE_MAXICODE) then
  begin
    { Maxicode is a fixed size }
    scaler := GL_CONST; { Converts from millimeters to the scale used by glabels }
    render^.width := 28.16 * scaler;
    render^.height := 26.86 * scaler;

    { Central bullseye pattern }

    ring := render_plot_create_ring(13.64 * scaler, 13.43 * scaler, 3.54 * scaler, 0.67 * scaler);
    render_plot_add_ring(symbol, ring, last_ring);
    ring := render_plot_create_ring(13.64 * scaler, 13.43 * scaler, 2.20 * scaler, 0.67 * scaler);
    render_plot_add_ring(symbol, ring, last_ring);
    ring := render_plot_create_ring(13.64 * scaler, 13.43 * scaler, 0.85 * scaler, 0.67 * scaler);
    render_plot_add_ring(symbol, ring, last_ring);

    { Hexagons }
    for r := 0 to symbol.rows -1 do
    begin
      for i := 0 to symbol.width - 1 do
      begin
        if (module_is_set(symbol, r, i) <> 0) then
        begin
          if (r and 1) <> 0 then
            hexagon := render_plot_create_hexagon(((i * 0.88) + 1.76) * scaler, ((r * 0.76) + 0.76) * scaler, 0.88 * scaler, 1.02 * scaler)
          else
            hexagon := render_plot_create_hexagon(((i * 0.88) + 1.32) * scaler, ((r * 0.76) + 0.76) * scaler, 0.88 * scaler, 1.02 * scaler);
          render_plot_add_hexagon(symbol, hexagon, last_hexagon);
        end;
      end;
    end;
  end
  else
  begin
    { everything else uses rectangles (or squares) }
    { Works from the bottom of the symbol up }
    addon_latch := 0;

    for r := 0 to symbol.rows - 1 do
    begin
      this_row := r;
      if (symbol.row_height[this_row] = 0) then
        row_height := large_bar_height
      else
        row_height := symbol.row_height[this_row];
      row_posn := 0;
      for i := 0 to r - 1 do
      begin
        if (symbol.row_height[i] = 0) then
          row_posn := row_posn + large_bar_height
        else
          row_posn := row_posn + symbol.row_height[i];
      end;
      row_posn := row_posn + yoffset;

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
        if ((addon_latch = 0) and (r = (symbol.rows - 1)) and (i > main_symbol_width_x)) then
        begin
          addon_text_posn := row_posn * scaler;
          addon_latch := 1;
        end;
        if (latch = 1) then
        begin
          { a bar }
          if (addon_latch = 0) then
            line := render_plot_create_line((i + xoffset) * scaler, (row_posn) * scaler, block_width * scaler, row_height * scaler)
          else
            line := render_plot_create_line((i + xoffset) * scaler, (row_posn + 10.0) * scaler, block_width * scaler, (row_height - 5.0) * scaler);

          latch := 0;

          render_plot_add_line(symbol, line, last_line);
        end
        else
        begin
          { a space }
          latch := 1;
        end;
        Inc(i, block_width);

      until not (i < symbol.width);
    end;
  end;
  { That's done the actual data area, everything else is human-friendly }


  { Add the text }
  Dec(xoffset, symbol_lead_in);
  row_posn := (row_posn + large_bar_height) * scaler;

  if (hide_text = 0) then
  begin
    if (upceanflag = 8) then
    begin
      { guard bar extensions and text formatting for EAN-8 }
      i := 0;
      line := symbol.rendered^.lines;
      while Assigned(line) do
      begin
        case i of
          0,
          1,
          10,
          11,
          20,
          21:
            line^.length := line^.length + (5.0 * scaler);
        end;
        Inc(i);
        line := line^.next
      end;


      for i := 0 to 3 do
        textpart[i] := Chr(symbol.text[i]);
      textpart[4] := #0;
      textpos := 17;
      textwidth := 4.0 * 8.5;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, last_string);

      for i := 0 to 3 do
        textpart[i] := Chr(symbol.text[i + 4]);
      textpart[4] := #0;
      textpos := 50;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, last_string);
      textdone := 1;
      case strlen(addon) of
        2:
        begin
          textpos := xoffset + 86;
          textwidth := 2.0 * 8.5;
          render_plot_add_string(symbol, addon, textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, last_string);
        end;
        5:
        begin
          textpos := xoffset + 100;
          textwidth := 5.0 * 8.5;
          render_plot_add_string(symbol, addon, textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, last_string);
        end;
      end;

    end;

    if (upceanflag = 13) then
    begin
      { guard bar extensions and text formatting for EAN-13 }
      i := 0;
      line := symbol.rendered^.lines;
      while Assigned(line) do
      begin
        case i of
          0,
          1,
          14,
          15,
          28,
          29:
            line^.length := line^.length + (5.0 * scaler);
        end;
        Inc(i);
        line := line^.next
      end;

      textpart[0] := Chr(symbol.text[0]);
      textpart[1] := #0;
      textpos := -5; // 7
      textwidth := 8.5;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, last_string);

      for i := 0 to 5 do
        textpart[i] := Chr(symbol.text[i + 1]);
      textpart[6] := #0;
      textpos := 25;
      textwidth := 6.0 * 8.5;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, last_string);

      for i := 0 to 5 do
        textpart[i] := Chr(symbol.text[i + 7]);
      textpart[6] := #0;
      textpos := 72;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, last_string);

      textdone := 1;
      case strlen(addon) of
        2:
        begin
          textpos := xoffset + 114;
          textwidth := 2.0 * 8.5;
          render_plot_add_string(symbol, addon, textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, last_string);
        end;
        5:
        begin
          textpos := xoffset + 128;
          textwidth := 5.0 * 8.5;
          render_plot_add_string(symbol, addon, textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, last_string);
        end;
      end;
    end;

    if (upceanflag = 12) then
    begin
      { guard bar extensions and text formatting for UPCA }
      i := 0;
      line := symbol.rendered^.lines;
      while Assigned(line) do
      begin
        case i of
          0,
          1,
          2,
          3,
          14,
          15,
          26,
          27,
          28,
          29:
            line^.length := line^.length + (5.0 * scaler);
        end;
        Inc(i);
        line := line^.next
      end;

      textpart[0] := Chr(symbol.text[0]);
      textpart[1] := #0;
      textpos := -5;
      textwidth := 6.2;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn + (2.0 * scaler), 8.0 * scaler, textwidth * scaler, last_string);

      for i := 0 to 4 do
        textpart[i] := Chr(symbol.text[i + 1]);
      textpart[5] := #0;
      textpos := 27;
      textwidth := 5.0 * 8.5;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, last_string);

      for i := 0 to 4 do
        textpart[i] := Chr(symbol.text[i + 6]);
      textpos := 68;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, last_string);

      textpart[0] := Chr(symbol.text[11]);
      textpart[1] := #0;
      textpos := 100;
      textwidth := 6.2;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn + (2.0 * scaler), 8.0 * scaler, textwidth * scaler, last_string);
      textdone := 1;
      case strlen(addon) of
        2:
        begin
          textpos := xoffset + 116;
          textwidth := 2.0 * 8.5;
          render_plot_add_string(symbol, addon, textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, last_string);
        end;
        5:
        begin
          textpos := xoffset + 130;
          textwidth := 5.0 * 8.5;
          render_plot_add_string(symbol, addon, textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, last_string);
        end;
      end;
    end;

    if (upceanflag = 6) then
    begin
      { guard bar extensions and text formatting for UPCE }
      i := 0;
      line := symbol.rendered^.lines;
      while Assigned(line) do
      begin
        case i of
          0,
          1,
          14,
          15,
          16:
            line^.length := line^.length + (5.0 * scaler);
        end;
        Inc(i);
        line := line^.next
      end;

      textpart[0] := Chr(symbol.text[0]);
      textpart[1] := #0;
      textpos := -5;
      textwidth := 6.2;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn + (2.0 * scaler), 8.0 * scaler, textwidth * scaler, last_string);

      for i := 0 to 5 do
        textpart[i] := Chr(symbol.text[i + 1]);
      textpart[6] := #0;
      textpos := 24;
      textwidth := 6.0 * 8.5;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, last_string);

      textpart[0] := Chr(symbol.text[7]);
      textpart[1] := #0;
      textpos := 55;
      textwidth := 6.2;
      render_plot_add_string(symbol, textpart, (textpos + xoffset) * scaler, default_text_posn + (2.0 * scaler), 8.0 * scaler, textwidth * scaler, last_string);
      textdone := 1;
      case strlen(addon) of
        2:
        begin
          textpos := xoffset + 70;
          textwidth := 2.0 * 8.5;
          render_plot_add_string(symbol, addon, textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, last_string);
        end;
        5:
        begin
          textpos := xoffset + 84;
          textwidth := 5.0 * 8.5;
          render_plot_add_string(symbol, addon, textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, last_string);
        end;
      end;
    end;

    { Put normal human readable text at the bottom (and centered) }
    if (textdone = 0) then
    begin
      // caculate start xoffset to center text
      if symbol.input_mode = UNICODE_MODE then
        {$IFDEF FPC}
        render_plot_add_string(symbol, StrToArrayOfChar(TEncoding.ANSI.GetString(symbol.text)), ((symbol.width / 2.0) + xoffset) * scaler, default_text_posn, 9.0 * scaler, 0.0, last_string)
        {$ELSE}
        {$IFDEF UseTEncoding}
        render_plot_add_string(symbol, StrToArrayOfChar(TEncoding.UTF8.GetString(symbol.text)), ((symbol.width / 2.0) + xoffset) * scaler, default_text_posn, 9.0 * scaler, 0.0, last_string)
        {$ELSE}
        render_plot_add_string(symbol, StrToArrayOfChar(UTF8Decode(ArrayOfByteToString(symbol.text))), ((symbol.width / 2.0) + xoffset) * scaler, default_text_posn, 9.0 * scaler, 0.0, last_string)
        {$ENDIF}
        {$ENDIF}
      else
        render_plot_add_string(symbol, ArrayOfByteToArrayOfChar(symbol.text), ((symbol.width / 2.0) + xoffset) * scaler, default_text_posn, 9.0 * scaler, 0.0, last_string);
    end;
  end;

  case symbol.symbology of
    BARCODE_MAXICODE:
    else
      if ((symbol.output_options and BARCODE_BIND) <> 0) then
      begin
        if ((symbol.rows > 1) and (is_stackable(symbol.symbology) = 1)) then
        begin
          { row binding }
          for r := 1 to symbol.rows - 1 do
          begin
            line := render_plot_create_line(xoffset * scaler, ((r * row_height) + yoffset - 1) * scaler, symbol.width * scaler, 2.0 * scaler);
            render_plot_add_line(symbol, line, last_line);
          end;
        end;
      end;
      if (((symbol.output_options and BARCODE_BOX) <> 0) or ((symbol.output_options and BARCODE_BIND) <> 0)) then
      begin
        line := render_plot_create_line(0, 0, (symbol.width + xoffset + xoffset) * scaler, symbol.border_width * scaler);
        render_plot_add_line(symbol, line, last_line);
        line := render_plot_create_line(0, (symbol.height + symbol.border_width) * scaler, (symbol.width + xoffset + xoffset) * scaler, symbol.border_width * scaler);
        render_plot_add_line(symbol, line, last_line);
      end;
      if ((symbol.output_options and BARCODE_BOX) <> 0) then
      begin
        { side bars }
        line := render_plot_create_line(0, 0, symbol.border_width * scaler, (symbol.height + (2 * symbol.border_width)) * scaler);
        render_plot_add_line(symbol, line, last_line);
        line := render_plot_create_line((symbol.width + xoffset + xoffset - symbol.border_width) * scaler, 0, symbol.border_width * scaler, (symbol.height + (2 * symbol.border_width)) * scaler);
        render_plot_add_line(symbol, line, last_line);
      end;
  end;

  result := 1; exit;
end;

end.

