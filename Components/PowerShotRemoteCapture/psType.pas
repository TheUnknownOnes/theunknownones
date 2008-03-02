unit psType;

interface

uses
  prType;

type
  psUInt8 =   type prUInt8;   PpsUInt8 = ^psUint8;
  psInt8 =    type prInt8;    PpsInt8 = ^psInt8;
  psUInt16 =  type prUInt16;  PpsUInt16 = ^psUInt16;
  psInt16 =   type prInt16;   PpsInt16 = ^psInt16;
  psUInt32 =  type prUInt32;  PpsUInt32 = ^psUInt32;
  psInt32 =   type prInt32;   PpsInt32 = ^psInt32;
  psUInt64 =  type prUInt64;  PpsUInt64 = ^psUInt64;
  psInt64 =   type prUInt64;  PpsInt64 = ^psInt64;
  psChar =    type prChar;    PpsChar = ^psChar;
  psWChar =   type prWChar;   PpsWChar = ^psWChar;
  psFloat32 = type prFloat32; PpsFloat32 = ^psFloat32;
  psInt128 = packed record
    Value : array[0..15] of Byte;
  end;                        PpsInt128 = ^psInt128;
  psUInt128 = type psInt128;   PpsUint128 = ^psUInt128;

  psDllsVerInfo = type prDllsVerInfo; PpsDllsVerInfo = ^psDllsVerInfo; 

implementation

end.
