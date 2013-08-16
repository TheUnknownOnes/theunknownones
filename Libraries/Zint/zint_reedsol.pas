unit zint_reedsol;

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
  zint_common;

procedure rs_init_gf(poly : Integer);
procedure rs_init_code(nsym : Integer; index : Integer);
procedure rs_encode(len : Integer; const data : TArrayOfByte; var res : TArrayOfByte);
procedure rs_encode_long(len : Integer; data : TArrayOfCardinal; var res : TArrayOfCardinal);
procedure rs_free;

implementation

// It is not written with high efficiency in mind, so is probably
// not suitable for real-time encoding.  The aim was to keep it
// simple, general and clear.
//
// <Some notes on the theory and implementation need to be added here>

// Usage:
// First call rs_init_gf(poly) to set up the Galois Field parameters.
// Then  call rs_init_code(size, index) to set the encoding size
// Then  call rs_encode(datasize, data, out) to encode the data.
//
// These can be called repeatedly as required - but note that
// rs_init_code must be called following any rs_init_gf call.
//
// If the parameters are fixed, some of the statics below can be
// replaced with constants in the obvious way, and additionally
// malloc/free can be avoided by using static arrays of a suitable
// size.

var
  gfpoly : Integer;
  symsize : Integer;    // in bits
  logmod : Integer;    // 2**symsize - 1
  rlen : Integer;

  logt, alog, rspoly : TArrayOfInteger;

// rs_init_gf(poly) initialises the parameters for the Galois Field.
// The symbol size is determined from the highest bit set in poly
// This implementation will support sizes up to 30 bits (though that
// will result in very large log/antilog tables) - bit sizes of
// 8 or 4 are typical
//
// The poly is the bit pattern representing the GF characteristic
// polynomial.  e.g. for ECC200 (8-bit symbols) the polynomial is
// a**8 + a**5 + a**3 + a**2 + 1, which translates to 0x12d.

procedure rs_init_gf(poly : Integer);
var
  m, b, p, v : Integer;
begin
  // Find the top bit, and hence the symbol size
  b := 1;
  m := 0;
  while (b <= poly) do
  begin
    Inc(m);
    b := b shl 1;
  end;

  b := b shr 1;
  Dec(m);
  gfpoly := poly;
  symsize := m;

  // Calculate the log/alog tables
  logmod := (1 shl m) - 1;
  SetLength(logt, logmod + 1);
  SetLength(alog, logmod);

  p := 1;
  v := 0;
  while (v < logmod) do
  begin
    alog[v] := p;
    logt[p] := v;
    p := p shl 1;
    if (p and b) <> 0 then
      p := p xor poly;
    Inc(v);
  end;
end;

// rs_init_code(nsym, index) initialises the Reed-Solomon encoder
// nsym is the number of symbols to be generated (to be appended
// to the input data).  index is usually 1 - it is the index of
// the constant in the first term (i) of the RS generator polynomial:
// (x + 2**i)*(x + 2**(i+1))*...   [nsym terms]
// For ECC200, index is 1.

procedure rs_init_code(nsym : Integer; index : Integer);
var
  i, k : Integer;
begin
  SetLength(rspoly, nsym + 1);

  rlen := nsym;

  rspoly[0] := 1;
  for i := 1 to nsym do
  begin
    rspoly[i] := 1;
    for k := i - 1 downto 1 do
    begin
      if (rspoly[k] <> 0) then
        rspoly[k] := alog[(logt[rspoly[k]] + index) mod logmod];
      rspoly[k] := rspoly[k] xor rspoly[k - 1];
    end;
    rspoly[0] := alog[(logt[rspoly[0]] + index) mod logmod];
    Inc(index);
  end;
end;

procedure rs_encode(len : Integer; const data : TArrayOfByte; var res : TArrayOfByte);
var
  i, k, m : Integer;
begin
  for i := 0 to rlen - 1 do
    res[i] := 0;
  for i := 0 to len - 1 do
  begin
    m := res[rlen - 1] xor data[i];
    for k := rlen - 1 downto 1 do
    begin
      if (m <> 0) and (rspoly[k] <> 0) then
        res[k] := res[k - 1] xor alog[(logt[m] + logt[rspoly[k]]) mod logmod]
      else
        res[k] := res[k - 1];
    end;
    if (m <> 0) and (rspoly[0] <> 0) then
      res[0] := alog[(logt[m] + logt[rspoly[0]]) mod logmod]
    else
      res[0] := 0;
  end;
end;

procedure rs_encode_long(len : Integer; data : TArrayOfCardinal; var res : TArrayOfCardinal);
{ The same as above but for larger bitlengths - Aztec code compatible }
var
  i, k, m : Integer;
begin
  for i := 0 to rlen - 1 do
    res[i] := 0;
  for i := 0 to len - 1 do
  begin
    m := res[rlen - 1] xor data[i];
    for k := rlen - 1 downto 1 do
    begin
      if (m <> 0) and (rspoly[k] <> 0) then
        res[k] := res[k - 1] xor alog[(logt[m] + logt[rspoly[k]]) mod logmod]
      else
        res[k] := res[k - 1];
    end;
    if (m <> 0) and (rspoly[0] <> 0) then
      res[0] := alog[(logt[m] + logt[rspoly[0]]) mod logmod]
    else
      res[0] := 0;
  end;
end;

procedure rs_free;
begin { Free memory }
  SetLength(logt, 0);
  SetLength(alog, 0);
  SetLength(rspoly, 0);
end;

end.

