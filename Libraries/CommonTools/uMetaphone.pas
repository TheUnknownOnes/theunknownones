unit uMetaphone;

interface

function MetaphoneKey(const AStr: String): String;
function SoundsSimilar(const AStr1, AStr2: String): Boolean;

implementation

{Metaphone algorithm translated by Tom White <wcs@intellex.com>

v1.1 fixes a few bugs

    Checks length of string before removing trailing S (>1)
    PH used to translate to H, now translates to F

Original C version by Michael Kuhn <rhlab!mkuhn@uunet.uu.net>
}

//TUO converted it to work without pointers

uses sysutils;


function SoundsSimilar(const AStr1, AStr2: String): Boolean;
begin
  Result:=MetaphoneKey(AStr1)=MetaphoneKey(AStr2);
end;

{a=pointer to input string
 lg=max length of return string
 res=pointer to buffer to place result string in - must be lg+1 bytes long}

function MetaphoneKey(const AStr : String): String;
var
  b, c, d, e : string;
  inp, outp : string;
  vowels, frontv, varson, dbl : string;
  excppair, nxtltr : string;
  t, ii, jj, lng, lastchr : integer;
  curltr, prevltr, nextltr, nextltr2, nextltr3 : char;
  vowelafter, vowelbefore, frontvafter, silent, hard : boolean;
  alphachr : string;
  lg : Integer;
begin
  inp:=UpperCase(AStr);
  lg:=Length(inp);
{  showmessage(inp);}
  vowels:='AEIOU';
  frontv:='EIY';
  varson:='CSPTG';
  dbl:='.';  {Lets us allow certain letters to be doubled}
  excppair:='AGKPW';
  nxtltr:='ENNNR';
  alphachr:='ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  {Remove non-alpha characters}
  outp:='';
  for t:=1 to length(inp) do
    if pos(inp[t],alphachr)>0 then outp:=outp+inp[t];

  inp:=outp;
  outp:='';

  if length(inp)=0 then begin
    Result:=EmptyStr;
    exit;
  end;

  {Check rules at beginning of word}
  if length(inp)>1 then begin
    b:=inp[1];
    c:=inp[2];
    ii:=pos(excppair,b);
    jj:=pos(nxtltr,c);
    if (ii=jj) and (ii>0) then
      inp:=copy(inp,2,length(inp)-1);
  end;

  if inp[1]='X' then inp[1]:='S';

  if (length(inp)>2) and (copy(inp,1,2)='WH') then
    inp:='W'+copy(inp,3,length(inp)-2);

  if (length(inp)>1) and (inp[length(inp)]='S') then
    inp:=copy(inp,1,length(inp)-1);

  ii:=0;
  repeat
    ii:=ii+1;
    {Main Loop!}

    silent:=false;
    hard:=false;
    curltr:=inp[ii];
    vowelbefore:=false;
    prevltr:=' ';
    if ii>1 then begin
      prevltr:=inp[ii-1];
      if pos(prevltr,vowels)>0 then
        vowelbefore:=true;
    end;

    if ((ii=1) and (pos(curltr,vowels)>0)) then begin
      outp:=outp+curltr;
      continue;
    end;

    vowelafter:=false;
    frontvafter:=false;
    nextltr:=' ';
    if ii<length(inp) then begin
      nextltr:=inp[ii+1];
      if pos(nextltr,vowels)>0 then vowelafter:=true;
      if pos(nextltr,frontv)>0 then frontvafter:=true;
    end;

    {Skip double letters EXCEPT ones in variable double}
    if pos(curltr,dbl)=0 then
      if curltr=nextltr then
        continue;

    nextltr2:=' ';
    if (length(inp)-ii)>1 then
      nextltr2:=inp[ii+2];

    nextltr3:=' ';
    if (length(inp)-ii)>2 then
      nextltr3:=inp[ii+3];

    case curltr of

      'B' : begin
              silent:=false;
              if (ii=length(inp)) and (prevltr='M') then
                silent:=true;
              if not silent then outp:=outp+curltr;
            end;

      'C' : begin
              if not ((ii>2) and (prevltr='S') and frontvafter) then
                if ((ii>1) and (nextltr='I') and (nextltr2='A')) then
                  outp:=outp+'X'
                else
                  if frontvafter then begin
                    outp:=outp+'S';
                  end else begin
                    if ((ii>2) and (prevltr='S') and (nextltr='H')) then begin
                      outp:=outp+'K';
                    end else begin
                      if nextltr='H' then begin
                        if ((ii=1) and (pos(nextltr2,vowels)=0)) then
                          outp:=outp+'K'
                        else
                          outp:=outp+'X';
                      end else begin
                        if prevltr='C' then
                          outp:=outp+'C'
                        else
                          outp:=outp+'K';
                      end;
                    end;
                  end;
              end;

      'D' : begin
              if ((nextltr='G') and (pos(nextltr2,frontv)>0)) then
                outp:=outp+'J'
              else
                outp:=outp+'T';
            end;

      'G' : begin
              silent:=false;
              if ((ii<length(inp)) and (nextltr='H') and (pos(nextltr2,vowels)=0)) then
                silent:=true;
              if ((ii=length(inp)-4) and (nextltr='N') and (nextltr2='E') and
                (nextltr3='D')) then silent:=true
              else if ((ii=length(inp)-2) and (nextltr='N')) then silent:=true;
              if (prevltr='D') and frontvafter then silent:=true;
              if prevltr='G' then
                hard:=true;

              if not silent then begin
                if frontvafter and (not hard) then
                  outp:=outp+'J'
                else
                  outp:=outp+'K';
              end;

            end;

      'H' : begin
              silent:=false;
              if pos(prevltr,varson)>0 then silent:=true;
              if vowelbefore and (not vowelafter) then silent:=true;

              if not silent then outp:=outp+curltr;
            end;


      'F', 'J', 'L', 'M', 'N', 'R' : outp:=outp+curltr;

      'K' : if prevltr<>'C' then outp:=outp+curltr;

      'P' : if nextltr='H' then outp:=outp+'F' else outp:=outp+'P';

      'Q' : outp:=outp+'K';

      'S' : begin
              if ((ii>2) and (nextltr='I') and ((nextltr2='O') or (nextltr2='A'))) then
                outp:=outp+'X';
              if (nextltr='H') then
                outp:=outp+'X'
              else
                outp:=outp+'S';
            end;

      'T' : begin
              if ((ii>0) and (nextltr='I') and ((nextltr2='O') or (nextltr2='A'))) then
                outp:=outp+'X';
              if nextltr='H' then begin
                if ((ii>1) or (pos(nextltr2,vowels)>0)) then
                  outp:=outp+'0'
                else
                  outp:=outp+'T';
              end else
                if not ((ii<length(inp)-3) and (nextltr='C') and (nextltr2='H')) then
                  outp:=outp+'T';
            end;

      'V' : outp:=outp+'F';

      'W', 'Y' : if (ii<length(inp)-1) and vowelafter then outp:=outp+curltr;

      'X' : outp:=outp+'KS';

      'Z' : outp:=outp+'S';

    end;

  until (length(outp)>lg-1) or (ii>length(inp));

  Result:=outp;
end;

end.
