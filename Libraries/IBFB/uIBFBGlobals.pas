//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uIBFBGlobals;

interface

type
  Int = Integer;
  Unsigned_Int = Cardinal;

  Long = Integer;
  Unsigned_Long = Cardinal;

  Short = Smallint;
  PShort = ^Short;
  Signed_Short = Short;
  Unsigned_Short = Word;

  __int64 = Int64;
  Unsigned___int64 = Int64;

  Unsigned_Char = AnsiChar;
  PUnsigned_Char = ^Unsigned_Char;

  tm = packed record
    tm_sec : int;   // Seconds
    tm_min : int;   // Minutes
    tm_hour : int;  // Hour (0--23)
    tm_mday : int;  // Day of month (1--31)
    tm_mon : int;   // Month (0--11)
    tm_year : int;  // Year (calendar year minus 1900)
    tm_wday : int;  // Weekday (0--6) Sunday = 0)
    tm_yday : int;  // Day of year (0--365)
    tm_isdst : int; // 0 if daylight savings time is not in effect
  end;

implementation

end.
