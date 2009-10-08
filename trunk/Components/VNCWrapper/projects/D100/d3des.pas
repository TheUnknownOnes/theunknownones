{
  Translated into Pascal by www.TheUnknownOnes.net

 This is D3DES (V5.09) by Richard Outerbridge with the double and
 triple-length support removed for use in VNC.  Also the bytebit[] array
 has been reversed so that the most significant bit in each byte of the
 key is ignored, not the least significant.

 These changes are
 Copyright (C) 1999 AT&T Laboratories Cambridge. All Rights Reserved.

 This software is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


  D3DES (V5.09) -

 A portable, public domain, version of the Data Encryption Standard.

 Written with Symantec's THINK (Lightspeed) C by Richard Outerbridge.
 Thanks to: Dan Hoey for his excellent Initial and Inverse permutation
 code;  Jim Gillogly & Phil Karn for the DES key schedule code; Dennis
 Ferguson, Eric Young and Dana How for comparing notes; and Ray Lau,
 for humouring me on.

 Copyright (c) 1988,1989,1990,1991,1992 by Richard Outerbridge.
 (GEnie : OUTER; CIS : [71755,204]) Graven Imagery, 1992.
 }

unit d3des;

interface

function DES_Encrypt(APassword : String; Key : array of Byte) : String;
function DES_Decrypt(APassword : String; Key : array of Byte) : String;

var
  KnL : array[0..31] of LongWord = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  KnR : array[0..31] of LongWord = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  Kn3 : array[0..31] of LongWord = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  Df_Key : array[0..23] of Byte =
    ( $01, $23, $45, $67, $89, $ab, $cd, $ef,
   	  $fe, $dc, $ba, $98, $76, $54, $32, $10,
	    $89, $ab, $cd, $ef, $01, $23, $45, $67 );

  ByteBit : array[0..7] of Word = (1, 2, 4, 8, 16, 32, 64, 128);

  BigByte : array[0..23] of LongWord = (
    $800000,	$400000,	$200000,	$100000,
    $80000,	  $40000,	  $20000,	  $10000,
    $8000,	  $4000,	  $2000,	  $1000,
    $800, 	  $400, 	  $200, 	  $100,
    $80,		  $40,		  $20,		  $10,
    $8,		    $4,		    $2,		    $1);

  // Use the key schedule specified in the Standard (ANSI X3.92-1981).

  pc1 : array[0..55] of Integer =
    ( 56, 48, 40, 32, 24, 16,  8,	 0, 57, 49, 41, 33, 25, 17,
      9,  1, 58, 50, 42, 34, 26, 18, 10,  2, 59, 51, 43, 35,
      62, 54, 46, 38, 30, 22, 14,	6, 61, 53, 45, 37, 29, 21,
      13,  5, 60, 52, 44, 36, 28,	20, 12,  4, 27, 19, 11,  3 );

  totrot : array[0..15] of Byte =
	  (1, 2, 4, 6, 8, 10, 12, 14, 15, 17, 19, 21, 23, 25, 27, 28 );

  pc2 : array[0..47] of Byte =
    ( 13, 16, 10, 23,  0,  4,  2, 27, 14,  5, 20,  9,
      22, 18, 11,  3, 25,  7, 15,  6, 26, 19, 12,  1,
      40, 51, 30, 36, 46, 54, 29, 39, 50, 44, 32, 47,
      43, 48, 38, 55, 33, 52, 45, 41, 49, 35, 28, 31 );

  SP1 : array[0..63] of LongWord= (
	$01010400, $00000000, $00010000, $01010404,
	$01010004, $00010404, $00000004, $00010000,
	$00000400, $01010400, $01010404, $00000400,
	$01000404, $01010004, $01000000, $00000004,
	$00000404, $01000400, $01000400, $00010400,
	$00010400, $01010000, $01010000, $01000404,
	$00010004, $01000004, $01000004, $00010004,
	$00000000, $00000404, $00010404, $01000000,
	$00010000, $01010404, $00000004, $01010000,
	$01010400, $01000000, $01000000, $00000400,
	$01010004, $00010000, $00010400, $01000004,
	$00000400, $00000004, $01000404, $00010404,
	$01010404, $00010004, $01010000, $01000404,
	$01000004, $00000404, $00010404, $01010400,
	$00000404, $01000400, $01000400, $00000000,
	$00010004, $00010400, $00000000, $01010004 );

  SP2 : array[0..63] of LongWord= (
	$80108020, $80008000, $00008000, $00108020,
	$00100000, $00000020, $80100020, $80008020,
	$80000020, $80108020, $80108000, $80000000,
	$80008000, $00100000, $00000020, $80100020,
	$00108000, $00100020, $80008020, $00000000,
	$80000000, $00008000, $00108020, $80100000,
	$00100020, $80000020, $00000000, $00108000,
	$00008020, $80108000, $80100000, $00008020,
	$00000000, $00108020, $80100020, $00100000,
	$80008020, $80100000, $80108000, $00008000,
	$80100000, $80008000, $00000020, $80108020,
	$00108020, $00000020, $00008000, $80000000,
	$00008020, $80108000, $00100000, $80000020,
	$00100020, $80008020, $80000020, $00100020,
	$00108000, $00000000, $80008000, $00008020,
	$80000000, $80100020, $80108020, $00108000 );

  SP3 : array[0..63] of LongWord= (
	$00000208, $08020200, $00000000, $08020008,
	$08000200, $00000000, $00020208, $08000200,
	$00020008, $08000008, $08000008, $00020000,
	$08020208, $00020008, $08020000, $00000208,
	$08000000, $00000008, $08020200, $00000200,
	$00020200, $08020000, $08020008, $00020208,
	$08000208, $00020200, $00020000, $08000208,
	$00000008, $08020208, $00000200, $08000000,
	$08020200, $08000000, $00020008, $00000208,
	$00020000, $08020200, $08000200, $00000000,
	$00000200, $00020008, $08020208, $08000200,
	$08000008, $00000200, $00000000, $08020008,
	$08000208, $00020000, $08000000, $08020208,
	$00000008, $00020208, $00020200, $08000008,
	$08020000, $08000208, $00000208, $08020000,
	$00020208, $00000008, $08020008, $00020200 );

  SP4 : array[0..63] of LongWord= (
	$00802001, $00002081, $00002081, $00000080,
	$00802080, $00800081, $00800001, $00002001,
	$00000000, $00802000, $00802000, $00802081,
	$00000081, $00000000, $00800080, $00800001,
	$00000001, $00002000, $00800000, $00802001,
	$00000080, $00800000, $00002001, $00002080,
	$00800081, $00000001, $00002080, $00800080,
	$00002000, $00802080, $00802081, $00000081,
	$00800080, $00800001, $00802000, $00802081,
	$00000081, $00000000, $00000000, $00802000,
	$00002080, $00800080, $00800081, $00000001,
	$00802001, $00002081, $00002081, $00000080,
	$00802081, $00000081, $00000001, $00002000,
	$00800001, $00002001, $00802080, $00800081,
	$00002001, $00002080, $00800000, $00802001,
	$00000080, $00800000, $00002000, $00802080 );

  SP5 : array[0..63] of LongWord= (
	$00000100, $02080100, $02080000, $42000100,
	$00080000, $00000100, $40000000, $02080000,
	$40080100, $00080000, $02000100, $40080100,
	$42000100, $42080000, $00080100, $40000000,
	$02000000, $40080000, $40080000, $00000000,
	$40000100, $42080100, $42080100, $02000100,
	$42080000, $40000100, $00000000, $42000000,
	$02080100, $02000000, $42000000, $00080100,
	$00080000, $42000100, $00000100, $02000000,
	$40000000, $02080000, $42000100, $40080100,
	$02000100, $40000000, $42080000, $02080100,
	$40080100, $00000100, $02000000, $42080000,
	$42080100, $00080100, $42000000, $42080100,
	$02080000, $00000000, $40080000, $42000000,
	$00080100, $02000100, $40000100, $00080000,
	$00000000, $40080000, $02080100, $40000100 );

  SP6 : array[0..63] of LongWord= (
	$20000010, $20400000, $00004000, $20404010,
	$20400000, $00000010, $20404010, $00400000,
	$20004000, $00404010, $00400000, $20000010,
	$00400010, $20004000, $20000000, $00004010,
	$00000000, $00400010, $20004010, $00004000,
	$00404000, $20004010, $00000010, $20400010,
	$20400010, $00000000, $00404010, $20404000,
	$00004010, $00404000, $20404000, $20000000,
	$20004000, $00000010, $20400010, $00404000,
	$20404010, $00400000, $00004010, $20000010,
	$00400000, $20004000, $20000000, $00004010,
	$20000010, $20404010, $00404000, $20400000,
	$00404010, $20404000, $00000000, $20400010,
	$00000010, $00004000, $20400000, $00404010,
	$00004000, $00400010, $20004010, $00000000,
	$20404000, $20000000, $00400010, $20004010 );

  SP7 : array[0..63] of LongWord= (
	$00200000, $04200002, $04000802, $00000000,
	$00000800, $04000802, $00200802, $04200800,
	$04200802, $00200000, $00000000, $04000002,
	$00000002, $04000000, $04200002, $00000802,
	$04000800, $00200802, $00200002, $04000800,
	$04000002, $04200000, $04200800, $00200002,
	$04200000, $00000800, $00000802, $04200802,
	$00200800, $00000002, $04000000, $00200800,
	$04000000, $00200800, $00200000, $04000802,
	$04000802, $04200002, $04200002, $00000002,
	$00200002, $04000000, $04000800, $00200000,
	$04200800, $00000802, $00200802, $04200800,
	$00000802, $04000002, $04200802, $04200000,
	$00200800, $00000000, $00000002, $04200802,
	$00000000, $00200802, $04200000, $00000800,
	$04000002, $04000800, $00000800, $00200002 );

  SP8 : array[0..63] of LongWord= (
	$10001040, $00001000, $00040000, $10041040,
	$10000000, $10001040, $00000040, $10000000,
	$00040040, $10040000, $10041040, $00041000,
	$10041000, $00041040, $00001000, $00000040,
	$10040000, $10000040, $10001000, $00001040,
	$00041000, $00040040, $10040040, $10041000,
	$00001040, $00000000, $00000000, $10040040,
	$10000040, $10001000, $00041040, $00040000,
	$00041040, $00040000, $10041000, $00001000,
	$00000040, $10040040, $00001000, $00041040,
	$10001000, $00000040, $10000040, $10040000,
	$10040040, $10000000, $00040000, $10001040,
	$00000000, $10041040, $00040040, $10000040,
	$10040000, $10001000, $10001040, $00000000,
	$10041040, $00041000, $00041000, $00001040,
	$00001040, $00040040, $10000000, $10041000 );
  VNC_DES_Key : array[0..7] of Byte = (23,82,107,6,35,78,88,7);

implementation
uses StrUtils;

const
  EN0=0;  // MODE == encrypt
  DE1=1;  // MODE == decrypt

procedure cpkey(into : PLongWord);
var
  from, endp : PLongWord;
begin
	from:=@KnL;
  endp:=@KnL[31];
	while (Integer(from)<=Integer(endp)) do
  begin
    into^:=from^;
    Inc(into);
    Inc(from);
  end;
end;

procedure usekey(from : PLongWord);
var
  to_, endp : PLongWord;
begin
	to_:=@KnL;
  endp:=@KnL[31];
	while (Integer(to_)<=Integer(endp)) do
  begin
    to_^:=from^;
    Inc(to_);
    Inc(from);
  end;
end;

procedure cookey(raw1 : PLongWord);
var
  cook, raw0 : PLongWord;
	dough : array[0..31] of Longword;
  i : Integer;
begin
	cook:=@dough;
	for i:=0 to 15 do
  begin
		raw0:=raw1;
    Inc(Raw1);
		cook^:=(raw0^ and $00fc0000) shl 6;
		cook^:=cook^ or ((raw0^ and $00000fc0) shl 10);
		cook^:=cook^ or ((raw1^ and $00fc0000) shr 10);
		cook^:=cook^ or ((raw1^ and $00000fc0) shr 6);
    Inc(cook);
		cook^:=(raw0^ and $0003f000) shl 12;
		cook^:=cook^ or ((raw0^ and $0000003f) shl 16);
		cook^:=cook^ or ((raw1^ and $0003f000) shr 4);
		cook^:=cook^ or (raw1^ and $0000003f);
    Inc(cook);
    Inc(Raw1);
  end;
	usekey(@dough);
end;

procedure deskey(key : array of byte; edf : Integer);	// Thanks to James Gillogly & Phil Karn!
var
  i, j, l, m, n : Integer;
	pc1m, pcr : array[0..55] of Byte;
	kn : array[0..31] of LongWord;
  blubb : Integer;
begin
	for j:=0 to 55 do
  begin
 		l:=pc1[j];
		m:=l and 7;
    blubb:=(key[l shr 3] and ByteBit[m]);
    if blubb<>0 then
		  pc1m[j]:=1
    else
      pc1m[j]:=0;
  end;
  
	for i:=0 to 15 do
  begin
		if (edf=DE1) then
      m:=(15-i) shl 1
		else
      m:=i shl 1;
		n:=m+1;
    kn[n]:=0;
		kn[m]:=kn[n];
		for j:=0 to 27 do
    begin
			l:=j+totrot[i];
			if (l<28) then
        pcr[j]:=pc1m[l]
			else
        pcr[j]:=pc1m[l - 28];
    end;
		for j:=28 to 55 do
    begin
      l:=j+totrot[i];
      if (l<56) then
        pcr[j]:=pc1m[l]
      else
        pcr[j]:=pc1m[l-28];
    end;
		for j:=0 to 23 do
    begin
			if (pcr[pc2[j]])<>0 then
        kn[m]:= kn[m] or bigbyte[j];
			if (pcr[pc2[j+24]])<>0 then
        kn[n]:= kn[n] or BigByte[j];
    end
  end;
	cookey(@kn);
end;

procedure scrunch(outof : PByte; into : PLongWord);
begin
	into^:= (outof^ and $ff) shl 24;
  Inc(outof);
	into^:= into^ or ((outof^ and $ff) shl 16);
  Inc(outof);
	into^:= into^ or ((outof^ and $ff) shl 8);
  Inc(outof);
	into^:=into^ or (outof^ and $ff);
  Inc(outof);
  Inc(into);
	into^:= (outof^ and $ff) shl 24;
  Inc(outof);
	into^:= into^ or ((outof^ and $ff) shl 16);
  Inc(outof);
	into^:= into^ or ((outof^ and $ff) shl 8);
  Inc(outof);
	into^:=into^ or (outof^ and $ff);
end;

procedure unscrun(outof : PLongWord; into : PByte);
begin
	into^:=(outof^ shr 24) and $ff;
  Inc(into);
	into^:=(outof^ shr 16) and $ff;
  Inc(into);
	into^:=(outof^ shr  8) and $ff;
  Inc(into);
	into^:= outof^ and $ff;
  Inc(into);
  Inc(outof);
	into^:=(outof^ shr 24) and $ff;
  Inc(into);
	into^:=(outof^ shr 16) and $ff;
  Inc(into);
	into^:=(outof^ shr  8) and $ff;
  Inc(into);
	into^:= outof^ and $ff;
end;

procedure desfunc(block : PLongWord; keys : PLongWord);
var
  fval, work, right, leftt : LongWord;
	round : Integer;
begin
	leftt:= block^;
  Inc(block);
	right:= block^;
  Dec(block);
	work:=((leftt shr 4) xor right) and $0f0f0f0f;
	right:= right xor work;
	leftt:= leftt xor (work shl 4);
	work:= ((leftt shr 16) xor right) and $0000ffff;
	right:= right xor work;
	leftt:= leftt xor (work shl 16);
	work:= ((right shr 2) xor leftt) and $33333333;
	leftt:= leftt xor work;
	right:= right xor (work shl 2);
	work:= ((right shr 8) xor leftt) and $00ff00ff;
	leftt:= leftt xor work;
	right:= right xor (work shl 8);
	right:= ((right shl 1) or ((right shr 31) and 1)) and $ffffffff;
	work:= (leftt xor right) and $aaaaaaaa;
	leftt:= leftt xor work;
	right:= right xor work;
	leftt:= ((leftt shl 1) or ((leftt shr 31) and 1)) and $ffffffff;

  round:=0;
  while round<8 do
	//for round:= 0 to 7 do
  begin
		work:= (right shl 28) or (right shr 4);
		work:= work xor keys^;
    Inc(Keys);
		fval:= SP7[ work		 and $3f];
		fval:= fval or SP5[(work shr  8) and $3f];
		fval:= fval or SP3[(work shr 16) and $3f];
		fval:= fval or SP1[(work shr 24) and $3f];
		work:= right xor keys^;
    Inc(Keys);
    fval:= fval or SP8[ work		 and $3f];
		fval:= fval or SP6[(work shr  8) and $3f];
		fval:= fval or SP4[(work shr 16) and $3f];
		fval:= fval or SP2[(work shr 24) and $3f];
		leftt:= leftt xor fval;
		work:= (leftt shl 28) or (leftt shr 4);
		work:= work xor keys^;
    Inc(Keys);
		fval:= SP7[ work		 and $3f];
		fval:= fval or SP5[(work shr  8) and $3f];
		fval:= fval or SP3[(work shr 16) and $3f];
		fval:= fval or SP1[(work shr 24) and $3f];
		work:= leftt xor keys^;
    Inc(Keys);
		fval:= fval or SP8[ work		 and $3f];
		fval:= fval or SP6[(work shr  8) and $3f];
		fval:= fval or SP4[(work shr 16) and $3f];
		fval:= fval or SP2[(work shr 24) and $3f];
		right:= right xor fval;
    Inc(round);
	end;

	right:= (right shl 31) or (right shr 1);
	work:= (leftt xor right) and $aaaaaaaa;
	leftt:= leftt xor  work;
	right:= right xor work;
	leftt:= (leftt shl 31) or (leftt shr 1);
	work:= ((leftt shr 8) xor right) and $00ff00ff;
	right:= right xor work;
	leftt:= leftt xor  (work shl 8);
	work:= ((leftt shr 2) xor right) and $33333333;
	right:= right xor work;
	leftt:= leftt xor  (work shl 2);
	work:= ((right shr 16) xor leftt) and $0000ffff;
	leftt:= leftt xor  work;
	right:= right xor (work shl 16);
	work:= ((right shr 4) xor leftt) and $0f0f0f0f;
	leftt:= leftt xor  work;
	right:= right xor (work shl 4);
	block^:= right;
  Inc(block);
	block^:= leftt;
end;

procedure des(inblock : PByte; var outblock : array of char);
var
  work : array[0..1] of LongWord;
begin
	scrunch(inblock, @work);
	desfunc(@work, @KnL);
	unscrun(@work, @outblock);
end;

function DES_Encrypt(APassword : String; Key : array of Byte) : String;
var
  Input : array[0..7] of char;
  Output : array[0..7] of Char;
  idx : Integer;
begin
  for idx:=Low(Input) to High(Input) do
    Input[idx]:=APassword[idx+1];
  FillChar(Output,8,#0);
  deskey(Key,EN0);
  des(@Input,Output);
  Result:=Output;
end;

function DES_Decrypt(APassword : String; Key : array of Byte) : String;
var
  Input : array[0..7] of char;
  Output : array[0..7] of Char;
  idx : Integer;
begin
  for idx:=Low(Input) to High(Input) do
    Input[idx]:=APassword[idx+1];
  FillChar(Output,8,#0);
  deskey(Key,DE1);
  des(@Input,Output);
  Result:=Output;
end;


end.
